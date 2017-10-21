#include "tree.h"

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
  {
    Node& node = GetNode(symbol);
    std::lock_guard<std::mutex> guard(node.mutex());
    delta = value - node.value();
    node.set_value(value);
  }

  std::deque<Symbol> q{symbol};
  while (!q.empty()) {
    auto current_symbol = q.front();
    q.pop_front();

    Node& node = GetNode(current_symbol);

    if (current_symbol != symbol) {
      std::lock_guard<std::mutex> guard(node.mutex());
      node.increase_value(delta);
    }

    std::copy(node.descendants().begin(), node.descendants().end(),
              std::back_inserter(q));
  }
}

int Tree::GetValue(const Symbol& symbol) { return GetNode(symbol).value(); }

bool Tree::SymbolExists(const Symbol& symbol) {
  return symbols_to_nodes_.find(symbol) != symbols_to_nodes_.end();
}

Node& Tree::GetNode(const Symbol& symbol) {
  if (!SymbolExists(symbol)) {
    throw std::invalid_argument("Symbol does not exist!");
  }
  return *(symbols_to_nodes_.find(symbol)->second);
}

}  // namespace pdp
