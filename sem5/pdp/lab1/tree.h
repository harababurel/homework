#pragma once
#include <map>
#include <memory>
#include <queue>
#include <set>
#include <stdexcept>
#include "composite_node.h"
#include "literal_node.h"
#include "node.h"

namespace pdp {

using Symbol = std::string;

class Tree {
 public:
  Tree() : symbol_to_nodes_mtx_(std::make_unique<std::mutex>()) {}
  const std::vector<Symbol> symbols();

  void AddSymbol(const Symbol& symbol, const int value);

  void AddSymbol(const Symbol& symbol, const std::vector<Symbol>& ancestors);

  void UpdateSymbol(const Symbol& symbol, const int value);

  int GetValue(const Symbol& symbol);

  bool SymbolExists(const Symbol& symbol);

 private:
  std::map<Symbol, std::unique_ptr<Node>> symbols_to_nodes_;
  std::unique_ptr<std::mutex> symbol_to_nodes_mtx_;

  Node& GetNode(const Symbol& symbol);
};

}  // namespace pdp
