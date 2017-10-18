#pragma once
#include <iostream>
#include <string>
#include <vector>
#include "util/status.h"

namespace lftc {

using Symbol = std::string;
using SymbolVector = std::vector<Symbol>;
using Token = std::string;
using TokenVector = std::vector<Token>;

class Lexer {
 public:
  Lexer(SymbolVector symbols) : symbols_(symbols) {}

  util::Status Tokenize(const std::string& code, TokenVector* tokens);

 private:
  SymbolVector symbols_;
};

}  // namespace lftc
