#include "tree.h"
#include <sstream>

namespace pdp {

const std::vector<Symbol> Tree::symbols() {
  std::vector<Symbol> ret;
  ret.reserve(symbols_to_nodes_.size());
  for (const auto& x : symbols_to_nodes_) {
    ret.push_back(x.first);
  }
  return ret;
}

void Tree::AddSymbol(const Symbol& symbol, const int value) {
  if (SymbolExists(symbol)) {
    return;
  }
  std::lock_guard<std::mutex> guard(*symbol_to_nodes_mtx_);
  symbols_to_nodes_[symbol] = std::make_unique<Node>(value);
}

void Tree::AddSymbol(const Symbol& symbol,
                     const std::vector<Symbol>& ancestors) {
  if (SymbolExists(symbol)) {
    return;
  }

  symbol_to_nodes_mtx_->lock();
  symbols_to_nodes_[symbol] = std::make_unique<CompositeNode>(ancestors);
  symbol_to_nodes_mtx_->unlock();

  Node& node = GetNode(symbol);
  std::lock_guard<std::mutex> guard(node.mutex());

  for (const auto& ancestor_symbol : ancestors) {
    Node& ancestor = GetNode(ancestor_symbol);

    node.increase_value(ancestor.value());

    std::lock_guard<std::mutex> guard(ancestor.mutex());
    ancestor.add_descendant(symbol);
  }
}

void Tree::UpdateSymbol(const Symbol& symbol, const int value) {
  if (!SymbolExists(symbol)) {
    AddSymbol(symbol, value);
  }

  int delta;

  GetNode(symbol).mutex().lock();
  std::deque<Symbol> q{symbol};
  while (!q.empty()) {
    auto current_symbol = q.front();
    q.pop_front();

    Node& node = GetNode(current_symbol);

    // Rules:
    // 0. node should already be locked
    // 1. node's value can be safely updated
    // 2. node can only be unlocked after ALL descendants are locked, in order
    //    to prevent data races.

    if (current_symbol == symbol) {
      delta = value - node.value();
      node.set_value(value);
    } else {
      node.increase_value(delta);
    }

    for (const auto& descendant : node.descendants()) {
      GetNode(descendant).mutex().lock();
      q.push_back(descendant);
    }

    node.mutex().unlock();
  }
}

int Tree::GetValue(const Symbol& symbol) { return GetNode(symbol).value(); }

bool Tree::SymbolExists(const Symbol& symbol) {
  return symbols_to_nodes_.find(symbol) != symbols_to_nodes_.end();
}

Node& Tree::GetNode(const Symbol& symbol) {
  if (!SymbolExists(symbol)) {
    std::stringstream buffer;
    buffer << "Symbol [" << symbol << "] does not exist.";
    throw std::invalid_argument(buffer.str());
  }
  return *(symbols_to_nodes_.find(symbol)->second);
}

void Tree::ConsistencyCheck() {
  std::lock_guard<std::mutex> guard(*symbol_to_nodes_mtx_);

  std::map<Symbol, int> level;
  for (const auto& symbol : symbols()) {
    ComputeLevel(symbol, level);
  }

  std::map<int, std::vector<Symbol>> on_level;

  for (const auto& entry : level) {
    /* printf("Symbol %s is on level %d\n", entry.first.c_str(), entry.second);
     */
    on_level[entry.second].push_back(entry.first);
  }

  for (auto entry = on_level.rbegin(); entry != on_level.rend(); entry++) {
    /* printf("level %d: ", entry->first); */

    for (const auto& symbol : entry->second) {
      /* printf("locking node %s\n", symbol.c_str()); */
      GetNode(symbol).mutex().lock();
    }
    /* printf("\n"); */
  }

  for (const auto& symbol : symbols()) {
    Node& node = GetNode(symbol);

    try {
      CompositeNode& composite_node = dynamic_cast<CompositeNode&>(node);

      int expected_value = 0;

      for (const auto& ancestor : composite_node.ancestors()) {
        expected_value += GetNode(ancestor).value();
      }

      if (expected_value != composite_node.value()) {
        printf("BAD SUM FOR NODE %s! EXPECTED %d, GOT %d.\n", symbol.c_str(),
               expected_value, node.value());
      }
    } catch (...) {
      // LiteralNode, not CompositeNode
    }
  }

  for (const auto& symbol : symbols()) {
    GetNode(symbol).mutex().unlock();
  }
}

void Tree::ComputeLevel(const Symbol& symbol, std::map<Symbol, int>& level) {
  if (level.find(symbol) != level.end()) {
    return;
  }

  level[symbol] = 1;

  Node& node = GetNode(symbol);
  for (const auto& descendant : node.descendants()) {
    ComputeLevel(descendant, level);
    level[symbol] = std::max(level[symbol], 1 + level[descendant]);
  }
}

}  // namespace pdp
