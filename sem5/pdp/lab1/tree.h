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

  /* Rules:
   * 0. the update source node is initially locked.
   * 1. a breadth-first exploration is started from the source.
   * 2. the exploration updates each visited node's value.
   * 3. at each point in time, the current node can only be unlocked after ALL
   * descendants are locked, in order to prevent data races.
   */
  void UpdateSymbol(const Symbol& symbol, const int value);
  int GetValue(const Symbol& symbol);
  bool SymbolExists(const Symbol& symbol);

  /* The consistency check locks the entire tree level by level, starting with
   * literal nodes (leaves), so that no new updates can start. It potentially
   * waits for pending updates to finish before locking some nodes.
   */
  void ConsistencyCheck();

 private:
  Node& GetNode(const Symbol& symbol);
  void ComputeLevel(const Symbol& symbol, std::map<Symbol, int>& level);

  std::map<Symbol, std::unique_ptr<Node>> symbols_to_nodes_;
  std::unique_ptr<std::mutex> symbol_to_nodes_mtx_;
};

}  // namespace pdp
