#include "lexer.h"
#include <iostream>
#include "util/status.h"

namespace lftc {

using Symbol = std::string;
using SymbolVector = std::vector<Symbol>;
using Token = std::string;
using TokenVector = std::vector<Token>;

util::Status Lexer::Tokenize(const std::string& code, TokenVector* tokens) {
  tokens->clear();

  Token current_token;

  bool open_string = false;
  for (int i = 0; i < int(code.size());) {
    if (code[i] == '"') {
      if (open_string) {
        tokens->push_back(current_token);
        current_token = "";
      }
      open_string = !open_string;

      tokens->push_back(std::string(1, code[i++]));
      continue;
    }

    if (open_string) {
      current_token += code[i++];
      continue;
    }

    if (std::isspace(code[i])) {
      i++;
      continue;
    }

    bool found_symbol = false;
    Symbol current_symbol;
    for (const auto& symbol : symbols_) {
      if (code.substr(i, symbol.size()) == symbol) {
        current_symbol = symbol;
        found_symbol = true;
        break;
      }
    }

    if (found_symbol) {
      if (!current_token.empty()) {
        tokens->push_back(current_token);
        current_token = "";
      }
      tokens->push_back(current_symbol);
      i += current_symbol.size();
      continue;
    }

    current_token += code[i];
    i++;
  }

  if (!current_token.empty()) {
    tokens->push_back(current_token);
  }

  return util::OkStatus();
}

}  // namespace lftc
