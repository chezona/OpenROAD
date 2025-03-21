#include "ram/ram.h"
#include "sta/Liberty.hh"
#include "sta/PortDirection.hh"
#include "utl/Logger.h"
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include "db_sta/dbNetwork.hh"
#include "sta/FuncExpr.hh" // Add this for proper FuncExpr definition
#include "defrw/defwWriter.hpp" // Add this for DEF writing

namespace ram {

//----------------------------------------------
// HierarchyBuilder implementation
//----------------------------------------------

HierarchyBuilder::HierarchyBuilder(odb::dbBlock* block, const MemoryConfig& config, Logger* logger)
  : block_(block),
    config_(config),
    logger_(logger)
{
  // Find and cache commonly used cell masters
  findMasters();
}

std::unique_ptr<Element> HierarchyBuilder::buildHierarchy()
{
  logger_->info(utl::RAM, 1200, "Building memory hierarchy for {} words x {} bytes, type: {}",
               config_.getWordCount(), config_.getBytesPerWord(),
               config_.getType() == RAM_1RW ? "1RW" : 
               config_.getType() == RAM_1RW1R ? "1RW1R" : "2R1W");
  
  // Create clock, enable and other global nets
  odb::dbNet* clk = createBTermNet("CLK");
  
  // Create data input nets
  int data_width = config_.getBytesPerWord() * 8;
  std::array<odb::dbNet*, 32> data_in{};
  for (int i = 0; i < data_width && i < 32; ++i) {
    data_in[i] = createBTermNet(fmt::format("Di{}", i));
  }
  
  // Create address nets
  std::vector<odb::dbNet*> address;
  int addr_bits = config_.getAddressBits();
  for (int i = 0; i < addr_bits; ++i) {
    address.push_back(createBTermNet(fmt::format("A{}", i)));
  }
  
  // Create data output nets
  int read_ports = config_.getReadPorts();
  std::vector<std::array<odb::dbNet*, 32>> data_out(read_ports);
  for (int rp = 0; rp < read_ports; ++rp) {
    for (int i = 0; i < data_width && i < 32; ++i) {
      data_out[rp][i] = createBTermNet(fmt::format("Do{}_{}", rp, i));
    }
  }
  
  // Create write enable nets
  std::vector<odb::dbNet*> we_nets(config_.getWordCount());
  for (int w = 0; w < config_.getWordCount(); ++w) {
    we_nets[w] = createBTermNet(fmt::format("WE{}", w));
  }
  
  // If 1RW1R, create additional read enable net
  odb::dbNet* read_enable = nullptr;
  if (config_.getType() == RAM_1RW1R) {
    read_enable = createBTermNet("RE");
  }
  
  // Build the appropriate memory hierarchy based on size
  std::unique_ptr<Element> top_element;
  
  if (config_.getWordCount() <= 8) {
    // For small memories, use RAM8 directly
    top_element = createRAM8("ram8", 
                           config_.getReadPorts(), 
                           clk, 
                           we_nets, 
                           address, 
                           data_in, 
                           data_out);
  }
  else if (config_.getWordCount() <= 32) {
    // For medium memories, use RAM32
    top_element = createRAM32("ram32", 
                            config_.getReadPorts(), 
                            clk, 
                            we_nets, 
                            address, 
                            data_in, 
                            data_out);
  }
  else if (config_.getWordCount() <= 128) {
    // For larger memories, use RAM128
    top_element = createRAM128("ram128", 
                             config_.getReadPorts(), 
                             clk, 
                             we_nets, 
                             address, 
                             data_in, 
                             data_out);
  }
  else {
    // For very large memories, use RAM512
    top_element = createRAM512("ram512", 
                             config_.getReadPorts(), 
                             clk, 
                             we_nets, 
                             address, 
                             data_in, 
                             data_out);
  }
  
  // If 1RW1R, add read port
  if (config_.getType() == RAM_1RW1R && read_enable) {
    auto read_port = createReadPort("read_port", clk, read_enable, address);
    // Add read port to top-level hierarchy
    if (top_element && read_port) {
      top_element->addChild(std::move(read_port));
    }
  }
  
  logger_->info(utl::RAM, 1210, "Memory hierarchy built successfully");
  return top_element;
}

void HierarchyBuilder::findMasters()
{
  logger_->info(utl::RAM, 1220, "Finding cell masters");
  
  // Modified function to avoid std::function parameter issue
  auto findCellMatch = [&](auto matchFunc, const char* name, odb::dbMaster** target) {
    if (*target) return; // Already set
    
    odb::dbMaster* best = nullptr;
    float best_area = std::numeric_limits<float>::max();

    for (auto lib : block_->getDb()->getLibs()) {
      for (auto master : lib->getMasters()) {
        // Use network_, not dbDatabase directly
        if (!network_) {
          logger_->warn(utl::RAM, 1225, "No network available for cell lookup");
          continue;
        }
        
        auto cell = network_->dbToSta(master);
        if (!cell) continue;

        auto liberty = network_->libertyCell(cell);
        if (!liberty) continue;

        sta::LibertyCellPortIterator port_iter(liberty);
        sta::LibertyPort* out_port = nullptr;
        bool reject = false;

        while (port_iter.hasNext()) {
          auto port = port_iter.next();
          if (port->direction()->isAnyOutput()) {
            if (!out_port) {
              out_port = port;
            } else {
              reject = true;
              break;
            }
          }
        }

        if (!reject && out_port && matchFunc(out_port)) {
          if (liberty->area() < best_area) {
            best_area = liberty->area();
            best = master;
          }
        }
      }
    }

    if (best) {
      *target = best;
      masters_cache_[name] = best;
      logger_->info(utl::RAM, 1230, "Found {} cell: {}", name, best->getName());
    } else {
      logger_->warn(utl::RAM, 1240, "Could not find {} cell", name);
    }
  };
  
  // Find storage cell (DFF)
  findCellMatch([](sta::LibertyPort* port) {
    return port->libertyCell()->hasSequentials()
        && std::strcmp(port->name(), "Q") == 0;
  }, "storage", &storage_cell_);
  
  // Find tristate buffer
  findCellMatch([](sta::LibertyPort* port) {
    return port->direction()->isTristate()
        && std::strcmp(port->name(), "Z") == 0;
  }, "tristate", &tristate_cell_);
  
  // Find inverter
  findCellMatch([](sta::LibertyPort* port) {
    return port->libertyCell()->isInverter()
        && port->direction()->isOutput()
        && std::strcmp(port->name(), "Y") == 0;
  }, "inverter", &inv_cell_);
  
  // Find AND2 gate - modified to use FuncExpr safely
  findCellMatch([](sta::LibertyPort* port) {
    auto func = port->function();
    if (!func) return false;
    
    // Check if it's an AND gate with two inputs
    if (func->op() != sta::FuncExpr::op_and) return false;
    
    auto left = func->left();
    auto right = func->right();
    if (!left || !right) return false;
    
    return left->op() == sta::FuncExpr::op_port && 
           right->op() == sta::FuncExpr::op_port;
  }, "and2", &and2_cell_);
  
  // Find clock gate
  findCellMatch([](sta::LibertyPort* port) {
    return port->libertyCell()->isClockGate();
  }, "clock_gate", &clock_gate_cell_);
  
  // Find 2:1 mux - modified to use FuncExpr safely
  findCellMatch([](sta::LibertyPort* port) {
    auto func = port->function();
    if (!func) return false;
    
    // Check if it's an OR of two ANDs (basic 2:1 mux structure)
    if (func->op() != sta::FuncExpr::op_or) return false;
    
    auto left = func->left();
    auto right = func->right();
    if (!left || !right) return false;
    
    return left->op() == sta::FuncExpr::op_and && 
           right->op() == sta::FuncExpr::op_and;
  }, "mux2", &mux2_cell_);
  
  // Validate essential cells are found
  if (!storage_cell_ || !tristate_cell_ || !inv_cell_ || !and2_cell_) {
    logger_->error(utl::RAM, 1250, "Missing essential technology cells");
  }
}

odb::dbMaster* HierarchyBuilder::findMaster(const std::function<bool(sta::LibertyPort*)>& match, const char* name)
{
  // This is similar to the function in findMasters but returns the found master
  odb::dbMaster* best = nullptr;
  float best_area = std::numeric_limits<float>::max();

  for (auto lib : block_->getDb()->getLibs()) {
    for (auto master : lib->getMasters()) {
      if (!network_) {
        logger_->warn(utl::RAM, 1226, "No network available for cell lookup");
        continue;
      }
      
      auto cell = network_->dbToSta(master);
      if (!cell) continue;

      auto liberty = network_->libertyCell(cell);
      if (!liberty) continue;

      sta::LibertyCellPortIterator port_iter(liberty);
      sta::LibertyPort* out_port = nullptr;
      bool reject = false;

      while (port_iter.hasNext()) {
        auto port = port_iter.next();
        if (port->direction()->isAnyOutput()) {
          if (!out_port) {
            out_port = port;
          } else {
            reject = true;
            break;
          }
        }
      }

      if (!reject && out_port && match(out_port)) {
        if (liberty->area() < best_area) {
          best_area = liberty->area();
          best = master;
        }
      }
    }
  }

  return best;
}

odb::dbMaster* HierarchyBuilder::getMaster(const std::string& name)
{
  // Check if we already found this master
  auto it = masters_cache_.find(name);
  if (it != masters_cache_.end()) {
    return it->second;
  }
  
  // Otherwise, look it up in the libraries
  for (auto lib : block_->getDb()->getLibs()) {
    odb::dbMaster* master = lib->findMaster(name.c_str());
    if (master) {
      masters_cache_[name] = master;
      return master;
    }
  }
  
  return nullptr;
}

odb::dbNet* HierarchyBuilder::createNet(const std::string& name)
{
  // Check if net already exists in cache
  auto it = nets_cache_.find(name);
  if (it != nets_cache_.end()) {
    return it->second;
  }
  
  // Create a new net
  auto net = odb::dbNet::create(block_, name.c_str());
  nets_cache_[name] = net;
  return net;
}

odb::dbNet* HierarchyBuilder::createBTermNet(const std::string& name)
{
  // Check if net already exists in cache
  auto it = nets_cache_.find(name);
  if (it != nets_cache_.end()) {
    return it->second;
  }
  
  // Create a new net with a block terminal
  auto net = odb::dbNet::create(block_, name.c_str());
  odb::dbBTerm::create(net, name.c_str());
  nets_cache_[name] = net;
  return net;
}

odb::dbInst* HierarchyBuilder::createInst(const std::string& name, odb::dbMaster* master,
                                         const std::vector<std::pair<std::string, odb::dbNet*>>& connections)
{
  // Create a new instance
  auto inst = odb::dbInst::create(block_, master, name.c_str());
  
  // Connect pins to nets
  for (const auto& [term_name, net] : connections) {
    auto mterm = master->findMTerm(term_name.c_str());
    if (!mterm) {
      logger_->error(utl::RAM, 1260, "Term {} not found on master {}", 
                    term_name, master->getName());
      continue;
    }
    inst->getITerm(mterm)->connect(net);
  }
  
  return inst;
}

std::unique_ptr<Element> HierarchyBuilder::createBit(const std::string& prefix, 
                                                    int read_ports,
                                                    odb::dbNet* clock,
                                                    odb::dbNet* data_in,
                                                    const std::vector<odb::dbNet*>& select,
                                                    std::vector<odb::dbNet*>& data_out)
{
  logger_->info(utl::RAM, 1300, "Creating bit cell {}", prefix);
  
  if ((int)select.size() < read_ports) {
    logger_->error(utl::RAM, 1310, "Bit {}: Select vector too small ({} < {})",
                  prefix, select.size(), read_ports);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto storage_net = createNet(fmt::format("{}.storage", prefix));

  // Create storage element (DFF)
  auto dff_inst = createInst(fmt::format("{}.dff", prefix), storage_cell_,
                           {{"GATE", clock}, {"D", data_in}, {"Q", storage_net}});
  
  // Create Element with BIT type
  std::unique_ptr<Element> bit_elem = std::make_unique<Element>(dff_inst, Element::BIT);

  // Create tri-state buffers for each read port
  for (int rp = 0; rp < read_ports; ++rp) {
    auto tbuf_inst = createInst(fmt::format("{}.tbuf{}", prefix, rp), tristate_cell_,
                              {{"A", storage_net}, {"TE_B", select[rp]}, {"Z", data_out[rp]}});
    bit_elem->addChild(std::make_unique<Element>(tbuf_inst));
  }

  return bit_elem;
}

//----------------------------------------------
// Building Block Methods Implementation
//----------------------------------------------

std::unique_ptr<Element> HierarchyBuilder::createBit(const std::string& prefix, 
                                                    int read_ports,
                                                    odb::dbNet* clock,
                                                    odb::dbNet* data_in,
                                                    const std::vector<odb::dbNet*>& select,
                                                    std::vector<odb::dbNet*>& data_out)
{
  logger_->info(utl::RAM, 1300, "Creating bit cell {}", prefix);
  
  if ((int)select.size() < read_ports) {
    logger_->error(utl::RAM, 1310, "Bit {}: Select vector too small ({} < {})",
                  prefix, select.size(), read_ports);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto storage_net = createNet(fmt::format("{}.storage", prefix));

  // Create storage element (DFF)
  auto dff_inst = createInst(fmt::format("{}.dff", prefix), storage_cell_,
                           {{"GATE", clock}, {"D", data_in}, {"Q", storage_net}});
  
  // Create Element with BIT type
  std::unique_ptr<Element> bit_elem = std::make_unique<Element>(dff_inst, Element::BIT);

  // Create tri-state buffers for each read port
  for (int rp = 0; rp < read_ports; ++rp) {
    auto tbuf_inst = createInst(fmt::format("{}.tbuf{}", prefix, rp), tristate_cell_,
                              {{"A", storage_net}, {"TE_B", select[rp]}, {"Z", data_out[rp]}});
    bit_elem->addChild(std::make_unique<Element>(tbuf_inst));
  }

  return bit_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createByte(const std::string& prefix,
                                                     int read_ports,
                                                     odb::dbNet* clock,
                                                     odb::dbNet* write_enable,
                                                     const std::vector<odb::dbNet*>& selects,
                                                     const std::array<odb::dbNet*, 8>& data_input,
                                                     const std::vector<std::array<odb::dbNet*, 8>>& data_output)
{
  logger_->info(utl::RAM, 1320, "Creating byte {}", prefix);
  
  if (selects.empty()) {
    logger_->error(utl::RAM, 1330, "Byte {}: No select signals", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto byte_elem = std::make_unique<Element>(std::move(layout), Element::BYTE);

  // Invert the select signals for tristate control
  std::vector<odb::dbNet*> select_b_nets;
  select_b_nets.reserve(read_ports);
  for (int i = 0; i < read_ports; ++i) {
    auto select_b = createNet(fmt::format("{}.select{}_b", prefix, i));
    auto inv_inst = createInst(fmt::format("{}.inv{}", prefix, i), inv_cell_,
                             {{"A", selects[0]}, {"Y", select_b}});
    byte_elem->addChild(std::make_unique<Element>(inv_inst));
    select_b_nets.push_back(select_b);
  }

  // Clock gating
  auto clock_b = createNet(fmt::format("{}.clock_b", prefix));
  auto gclk = createNet(fmt::format("{}.gclk", prefix));
  auto we_gated = createNet(fmt::format("{}.we_gated", prefix));

  // Create clock inverter
  auto clk_inv_inst = createInst(fmt::format("{}.clock_inv", prefix), inv_cell_,
                               {{"A", clock}, {"Y", clock_b}});
  byte_elem->addChild(std::make_unique<Element>(clk_inv_inst));

  // Create write enable AND gate
  auto cg_and_inst = createInst(fmt::format("{}.cgand", prefix), and2_cell_,
                              {{"A", selects[0]}, {"B", write_enable}, {"X", we_gated}});
  byte_elem->addChild(std::make_unique<Element>(cg_and_inst));

  // Create clock gate
  auto clk_gate_inst = createInst(fmt::format("{}.cg", prefix), clock_gate_cell_,
                                {{"CLK", clock_b}, {"GATE", we_gated}, {"GCLK", gclk}});
  byte_elem->addChild(std::make_unique<Element>(clk_gate_inst));

  // Create 8 bits
  for (int bit = 0; bit < 8; ++bit) {
    std::vector<odb::dbNet*> bit_outputs;
    bit_outputs.reserve(read_ports);
    for (int rp = 0; rp < read_ports; ++rp) {
      bit_outputs.push_back(data_output[rp][bit]);
    }
    
    // Create bit cell
    auto bit_elem = createBit(fmt::format("{}.bit{}", prefix, bit),
                             read_ports, gclk,
                             data_input[bit], select_b_nets, bit_outputs);
    
    if (bit_elem) {
      byte_elem->addChild(std::move(bit_elem));
    }
  }

  return byte_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createWord(const std::string& prefix,
                                                     int read_ports,
                                                     odb::dbNet* clock,
                                                     const std::vector<odb::dbNet*>& we_per_byte,
                                                     odb::dbNet* sel,
                                                     const std::array<odb::dbNet*, 32>& data_input,
                                                     const std::vector<std::array<odb::dbNet*, 32>>& data_output)
{
  logger_->info(utl::RAM, 1340, "Creating word {}", prefix);
  
  if (we_per_byte.size() < 4) {
    logger_->error(utl::RAM, 1350, "Word {}: not enough WE signals for 4 bytes", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto word_elem = std::make_unique<Element>(std::move(layout), Element::WORD);

  // Create clock buffer
  auto clkbuf_net = createNet(fmt::format("{}.clkbuf_out", prefix));
  auto clkbuf_inst = createInst(fmt::format("{}.clkbuf", prefix), inv_cell_,
                              {{"A", clock}, {"Y", clkbuf_net}});
  word_elem->addChild(std::make_unique<Element>(clkbuf_inst));

  // Create select buffers for each read port
  std::vector<odb::dbNet*> selbuf_nets;
  selbuf_nets.reserve(read_ports);
  for (int rp = 0; rp < read_ports; ++rp) {
    auto selbuf_net = createNet(fmt::format("{}.selbuf{}_out", prefix, rp));
    auto selbuf_inst = createInst(fmt::format("{}.selbuf{}", prefix, rp), inv_cell_,
                                {{"A", sel}, {"Y", selbuf_net}});
    word_elem->addChild(std::make_unique<Element>(selbuf_inst));
    selbuf_nets.push_back(selbuf_net);
  }

  // Create 4 bytes
  for (int b = 0; b < 4; ++b) {
    // Extract 8 data inputs for this byte
    std::array<odb::dbNet*, 8> di;
    for (int i = 0; i < 8; ++i) {
      di[i] = data_input[b * 8 + i];
    }
    
    // Extract data outputs for this byte
    std::vector<std::array<odb::dbNet*, 8>> do_arrays(read_ports);
    for (int rp = 0; rp < read_ports; ++rp) {
      for (int i = 0; i < 8; ++i) {
        do_arrays[rp][i] = data_output[rp][b * 8 + i];
      }
    }
    
    // Create byte
    auto byte_elem = createByte(fmt::format("{}.byte{}", prefix, b),
                               read_ports, clkbuf_net,
                               we_per_byte[b], selbuf_nets, di, do_arrays);
    
    if (byte_elem) {
      word_elem->addChild(std::move(byte_elem));
    }
  }

  return word_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createDecoder(const std::string& prefix,
                                                        int address_bits,
                                                        const std::vector<odb::dbNet*>& inputs)
{
  logger_->info(utl::RAM, 1360, "Creating decoder {}", prefix);
  
  if ((int)inputs.size() != address_bits) {
    logger_->error(utl::RAM, 1370, "Decoder {}: need {} bits, got {}",
                  prefix, address_bits, inputs.size());
    return nullptr;
  }
  
  if (!inv_cell_ || !and2_cell_) {
    logger_->error(utl::RAM, 1380, "Missing cells for decoder {}", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto decoder_elem = std::make_unique<Element>(std::move(layout), Element::DECODER);
  
  // Create inverted address signals
  std::vector<odb::dbNet*> inverted(address_bits);
  for (int i = 0; i < address_bits; ++i) {
    inverted[i] = createNet(fmt::format("{}.A{}_bar", prefix, i));
    auto inv_inst = createInst(fmt::format("{}.inv{}", prefix, i),
                             inv_cell_,
                             {{"A", inputs[i]}, {"Y", inverted[i]}});
    decoder_elem->addChild(std::make_unique<Element>(inv_inst));
  }

  // Create decoder outputs (2^address_bits)
  int outputs = 1 << address_bits;
  for (int o = 0; o < outputs; ++o) {
    // For each output, determine which inputs (or inverted inputs) to use
    std::vector<odb::dbNet*> terms;
    for (int bit = 0; bit < address_bits; ++bit) {
      bool is_one = (o & (1 << bit)) != 0;
      terms.push_back(is_one ? inputs[bit] : inverted[bit]);
    }

    // Create AND gate tree
    odb::dbNet* current_net = terms[0];
    for (size_t t = 1; t < terms.size(); ++t) {
      auto new_net = createNet(fmt::format("{}.and_{}_{}", prefix, o, t));
      auto and_inst = createInst(fmt::format("{}.and_{}_{}", prefix, o, t),
                               and2_cell_,
                               {{"A", current_net}, {"B", terms[t]}, {"X", new_net}});
      decoder_elem->addChild(std::make_unique<Element>(and_inst));
      current_net = new_net;
    }

    // Create output buffer
    auto out_net = createNet(fmt::format("{}.dec_out{}", prefix, o));
    auto buf_inst = createInst(fmt::format("{}.buf{}", prefix, o),
                             inv_cell_,
                             {{"A", current_net}, {"Y", out_net}});
    decoder_elem->addChild(std::make_unique<Element>(buf_inst));
  }

  return decoder_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createRAM8(const std::string& prefix,
                                                     int read_ports,
                                                     odb::dbNet* clock,
                                                     const std::vector<odb::dbNet*>& we_per_word,
                                                     const std::vector<odb::dbNet*>& addr3bit,
                                                     const std::array<odb::dbNet*, 32>& data_input,
                                                     const std::vector<std::array<odb::dbNet*, 32>>& data_output)
{
  logger_->info(utl::RAM, 1390, "Creating RAM8 {}", prefix);
  
  if (we_per_word.size() < 8) {
    logger_->error(utl::RAM, 1400, "RAM8 {}: need 8 WE signals", prefix);
    return nullptr;
  }
  
  if (addr3bit.size() != 3) {
    logger_->error(utl::RAM, 1410, "RAM8 {}: need exactly 3 address bits", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto ram8_elem = std::make_unique<Element>(std::move(layout), Element::RAM8);

  // Create 3-to-8 decoder
  auto decoder = createDecoder(fmt::format("{}.dec", prefix), 3, addr3bit);
  if (decoder) {
    ram8_elem->addChild(std::move(decoder));
  } else {
    logger_->error(utl::RAM, 1420, "RAM8 {}: Decoder creation failed", prefix);
    return nullptr;
  }

  // Create 8 words
  for (int w = 0; w < 8; ++w) {
    // Get decoder output for this word
    auto sel_w = block_->findNet(fmt::format("{}.dec.dec_out{}", prefix, w).c_str());
    if (!sel_w) {
      logger_->warn(utl::RAM, 1430, "RAM8 {}: No net for dec_out{}", prefix, w);
      continue;
    }
    
    // Create write enable signals for the 4 bytes in this word
    std::vector<odb::dbNet*> we_4(4, we_per_word[w]);
    
    // Create word
    auto word_elem = createWord(fmt::format("{}.word{}", prefix, w),
                              read_ports, clock,
                              we_4, sel_w, data_input, data_output);
    
    if (word_elem) {
      ram8_elem->addChild(std::move(word_elem));
    }
  }

  return ram8_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createRAM32(const std::string& prefix,
                                                      int read_ports,
                                                      odb::dbNet* clock,
                                                      const std::vector<odb::dbNet*>& we_32,
                                                      const std::vector<odb::dbNet*>& addr5bit,
                                                      const std::array<odb::dbNet*, 32>& data_input,
                                                      const std::vector<std::array<odb::dbNet*, 32>>& data_output)
{
  logger_->info(utl::RAM, 1440, "Creating RAM32 {}", prefix);
  
  if (we_32.size() < 32) {
    logger_->error(utl::RAM, 1450, "RAM32 {}: need 32 WEs total", prefix);
    return nullptr;
  }
  
  if (addr5bit.size() < 5) {
    logger_->error(utl::RAM, 1460, "RAM32 {}: need 5 address bits", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto ram32_elem = std::make_unique<Element>(std::move(layout), Element::RAM32);

  // Use top 2 bits for selecting between 4 RAM8 blocks
  std::vector<odb::dbNet*> top_addr2(addr5bit.begin(), addr5bit.begin() + 2);
  
  // Create 2-to-4 decoder
  auto decoder = createDecoder(fmt::format("{}.dec2x4", prefix), 2, top_addr2);
  if (decoder) {
    ram32_elem->addChild(std::move(decoder));
  } else {
    logger_->error(utl::RAM, 1470, "RAM32 {}: 2x4 decoder creation failed", prefix);
    return nullptr;
  }

  // Use lower 3 bits for addressing within each RAM8 block
  std::vector<odb::dbNet*> sub_addr3(addr5bit.begin() + 2, addr5bit.end());

  // Create 4 RAM8 blocks
  for (int sub = 0; sub < 4; ++sub) {
    // Get decoder output for this RAM8 block
    auto sub_sel = block_->findNet(fmt::format("{}.dec2x4.dec_out{}", prefix, sub).c_str());
    if (!sub_sel) {
      logger_->warn(utl::RAM, 1480, "RAM32 {}: No net for dec2x4.dec_out{}", prefix, sub);
      continue;
    }
    
    // Extract 8 WE signals for this RAM8 block
    std::vector<odb::dbNet*> we_subblock;
    for (int w = sub * 8; w < (sub * 8) + 8; w++) {
      we_subblock.push_back(we_32[w]);
    }
    
    // Create RAM8 block
    auto ram8_elem = createRAM8(fmt::format("{}.ram8_{}", prefix, sub),
                               read_ports, clock,
                               we_subblock, sub_addr3,
                               data_input, data_output);
    
    if (ram8_elem) {
      ram32_elem->addChild(std::move(ram8_elem));
    }
  }

  return ram32_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createRAM128(const std::string& prefix,
                                                       int read_ports,
                                                       odb::dbNet* clock,
                                                       const std::vector<odb::dbNet*>& we_128,
                                                       const std::vector<odb::dbNet*>& addr7bit,
                                                       const std::array<odb::dbNet*, 32>& data_input,
                                                       const std::vector<std::array<odb::dbNet*, 32>>& data_output)
{
  logger_->info(utl::RAM, 1490, "Creating RAM128 {}", prefix);
  
  if (we_128.size() < 128) {
    logger_->error(utl::RAM, 1500, "RAM128 {}: need 128 WEs total", prefix);
    return nullptr;
  }
  
  if (addr7bit.size() < 7) {
    logger_->error(utl::RAM, 1510, "RAM128 {}: need 7 address bits", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto ram128_elem = std::make_unique<Element>(std::move(layout), Element::RAM128);

  // Use top 2 bits for selecting between 4 RAM32 blocks
  std::vector<odb::dbNet*> top_addr2(addr7bit.begin(), addr7bit.begin() + 2);
  
  // Create 2-to-4 decoder
  auto decoder = createDecoder(fmt::format("{}.dec2x4", prefix), 2, top_addr2);
  if (decoder) {
    ram128_elem->addChild(std::move(decoder));
  } else {
    logger_->error(utl::RAM, 1520, "RAM128 {}: 2x4 decoder creation failed", prefix);
    return nullptr;
  }

  // Use lower 5 bits for addressing within each RAM32 block
  std::vector<odb::dbNet*> sub_addr5(addr7bit.begin() + 2, addr7bit.end());

  // Create 4 RAM32 blocks
  for (int sub = 0; sub < 4; ++sub) {
    // Get decoder output for this RAM32 block
    auto sub_sel = block_->findNet(fmt::format("{}.dec2x4.dec_out{}", prefix, sub).c_str());
    if (!sub_sel) {
      logger_->warn(utl::RAM, 1530, "RAM128 {}: No net for dec2x4.dec_out{}", prefix, sub);
      continue;
    }
    
    // Extract 32 WE signals for this RAM32 block
    std::vector<odb::dbNet*> we_subblock;
    for (int w = sub * 32; w < (sub * 32) + 32; w++) {
      we_subblock.push_back(we_128[w]);
    }
    
    // Create RAM32 block
    auto ram32_elem = createRAM32(fmt::format("{}.ram32_{}", prefix, sub),
                                read_ports, clock,
                                we_subblock, sub_addr5,
                                data_input, data_output);
    
    if (ram32_elem) {
      ram128_elem->addChild(std::move(ram32_elem));
    }
  }

  return ram128_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createRAM512(const std::string& prefix,
                                                       int read_ports,
                                                       odb::dbNet* clock,
                                                       const std::vector<odb::dbNet*>& we_512,
                                                       const std::vector<odb::dbNet*>& addr9bit,
                                                       const std::array<odb::dbNet*, 32>& data_input,
                                                       const std::vector<std::array<odb::dbNet*, 32>>& data_output)
{
  logger_->info(utl::RAM, 1540, "Creating RAM512 {}", prefix);
  
  if (we_512.size() < 512) {
    logger_->error(utl::RAM, 1550, "RAM512 {}: need 512 WEs total", prefix);
    return nullptr;
  }
  
  if (addr9bit.size() < 9) {
    logger_->error(utl::RAM, 1560, "RAM512 {}: need 9 address bits", prefix);
    return nullptr;
  }

  auto layout = std::make_unique<Layout>(odb::horizontal);
  auto ram512_elem = std::make_unique<Element>(std::move(layout), Element::RAM512);

  // Use top 2 bits for selecting between 4 RAM128 blocks
  std::vector<odb::dbNet*> top_addr2(addr9bit.begin(), addr9bit.begin() + 2);
  
  // Create 2-to-4 decoder
  auto decoder = createDecoder(fmt::format("{}.dec2x4", prefix), 2, top_addr2);
  if (decoder) {
    ram512_elem->addChild(std::move(decoder));
  } else {
    logger_->error(utl::RAM, 1570, "RAM512 {}: 2x4 decoder creation failed", prefix);
    return nullptr;
  }

  // Use lower 7 bits for addressing within each RAM128 block
  std::vector<odb::dbNet*> sub_addr7(addr9bit.begin() + 2, addr9bit.end());

  // Create 4 RAM128 blocks
  for (int sub = 0; sub < 4; ++sub) {
    // Get decoder output for this RAM128 block
    auto sub_sel = block_->findNet(fmt::format("{}.dec2x4.dec_out{}", prefix, sub).c_str());
    if (!sub_sel) {
      logger_->warn(utl::RAM, 1580, "RAM512 {}: No net for dec2x4.dec_out{}", prefix, sub);
      continue;
    }
    
    // Extract 128 WE signals for this RAM128 block
    std::vector<odb::dbNet*> we_subblock;
    for (int w = sub * 128; w < (sub * 128) + 128; w++) {
      we_subblock.push_back(we_512[w]);
    }
    
    // Create RAM128 block
    auto ram128_elem = createRAM128(fmt::format("{}.ram128_{}", prefix, sub),
                                  read_ports, clock,
                                  we_subblock, sub_addr7,
                                  data_input, data_output);
    
    if (ram128_elem) {
      ram512_elem->addChild(std::move(ram128_elem));
    }
  }

  return ram512_elem;
}

std::unique_ptr<Element> HierarchyBuilder::createReadPort(const std::string& prefix,
                                                         odb::dbNet* clock,
                                                         odb::dbNet* read_enable,
                                                         const std::vector<odb::dbNet*>& addr)
{
  logger_->info(utl::RAM, 1590, "Creating read port {}", prefix);
  
  // For 1RW1R memories, this creates an additional read port
  auto layout = std::make_unique<Layout>(odb::horizontal);
  
  // Create read port structure
  // This would involve:
  // 1. Address decoders
  // 2. Read enable logic
  // 3. Read sense amplifiers or output staging
  
  // For now, we'll create a simple placeholder structure
  auto read_en_buf = createNet(fmt::format("{}.read_en_buf", prefix));
  auto read_en_inst = createInst(fmt::format("{}.read_en_buf", prefix), inv_cell_,
                               {{"A", read_enable}, {"Y", read_en_buf}});
  
  auto read_port_elem = std::make_unique<Element>(read_en_inst);
  
  // In a real implementation, we'd create the full read port circuitry
  
  return read_port_elem;
}

} // namespace ram