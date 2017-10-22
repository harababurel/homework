#pragma once
#include <vector>
#include "node.h"

namespace pdp {

using Symbol = std::string;

class CompositeNode : public Node {
 public:
  CompositeNode(const std::vector<Symbol>& ancestors) : ancestors_(ancestors) {}
  const std::vector<Symbol>& ancestors() { return ancestors_; }

 private:
  std::vector<Symbol> ancestors_;
};

}  // namespace pdp
