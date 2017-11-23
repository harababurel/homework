#pragma once
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include "dfa/dfa.h"
#include "json.hpp"
#include "util/status.h"

namespace lftc {

using json = nlohmann::json;

using Symbol = std::string;
using SymbolID = int;
using SymbolTable = std::unordered_map<Symbol, SymbolID>;
using ProgramInternalForm = std::vector<std::pair<int, int>>;
using Token = std::string;
using TokenVector = std::vector<Token>;

class Lexer {
 public:
  Lexer(std::unordered_map<std::string, int> reserved_words)
      : reserved_words_(reserved_words) {
    json j;
    std::ifstream f;

    f.open("data/identifier.json");
    f >> j;
    identifier_dfa_ = std::make_unique<DeterministicFiniteAutomaton>(j);
    f.close();

    f.open("data/constant.json");
    f >> j;
    constant_dfa_ = std::make_unique<DeterministicFiniteAutomaton>(j);
    f.close();
  }

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

  std::unique_ptr<DeterministicFiniteAutomaton> identifier_dfa_;
  std::unique_ptr<DeterministicFiniteAutomaton> constant_dfa_;

  bool IsIdentifier(const Symbol& symbol);
  bool IsConstant(const Symbol& symbol);
  const std::string SymbolTableStr(const std::string& table_name,
                                   const SymbolTable& table_);
};

}  // namespace lftc
