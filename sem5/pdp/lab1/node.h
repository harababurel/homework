#pragma once
#include <mutex>

namespace pdp {

using Symbol = std::string;

class Node {
 public:
  Node() : value_(0) {}
  Node(int value) : value_(value) {}

  std::mutex& mutex() { return mutex_; }
  int value() { return value_; }
  void set_value(int value) { value_ = value; }
  void increase_value(int delta) { value_ += delta; }
  void add_descendant(const Symbol& descendant) {
    descendants_.push_back(descendant);
  }
  const std::vector<Symbol>& descendants() { return descendants_; }

 private:
  Symbol symbol_;
  int value_;
  std::vector<Symbol> descendants_;
  std::mutex mutex_;
};

}  // namespace pdp
