#pragma once
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include "util/status.h"

namespace lftc {

using Symbol = std::string;
using SymbolID = int;
using SymbolTable = std::unordered_map<Symbol, SymbolID>;
using ProgramInternalForm = std::vector<std::pair<int, int>>;
using Token = std::string;
using TokenVector = std::vector<Token>;

class Lexer {
 public:
  Lexer(std::unordered_map<std::string, int> reserved_words)
      : reserved_words_(reserved_words) {}

  util::Status Tokenize(const std::string& code);

  util::Status CreateSymbolTable();

  util::Status CreatePIF();

  const ProgramInternalForm& pif() const { return pif_; }

  const TokenVector& tokens() const { return tokens_; }

  const std::string TokensStr() const {
    std::stringstream buff;

    buff << "Tokens:\n";
    for (const auto& token : tokens_) {
      buff << token << "\n";
    }

    return buff.str();
  }

  const std::string IdentifierTableStr() {
    return SymbolTableStr("Identifier Symbol Table", identifier_table_);
  }

  const std::string ConstantTableStr() {
    return SymbolTableStr("Constant Symbol Table", constant_table_);
  }

  const std::string ProgramInternalFormStr();

 private:
  std::unordered_map<std::string, int> reserved_words_;
  TokenVector tokens_;

  SymbolTable identifier_table_;
  SymbolTable constant_table_;
  ProgramInternalForm pif_;

  static bool isIdentifier(const Symbol& symbol);
  static bool isConstant(const Symbol& symbol);
  const std::string SymbolTableStr(const std::string& table_name,
                                   const SymbolTable& table_);
};

}  // namespace lftc
