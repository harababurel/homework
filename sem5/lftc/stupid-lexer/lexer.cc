#include "lexer.h"
#include <iostream>
#include "util/status.h"

namespace lftc {

using Symbol = std::string;
using SymbolID = int;
using SymbolVector = std::vector<std::pair<Symbol, SymbolID>>;
using Token = std::string;
using TokenVector = std::vector<Token>;

util::Status Lexer::Tokenize(const std::string& code) {
  tokens_.clear();

  Token current_token;
  for (int i = 0; i < int(code.size());) {
    if (std::isspace(code[i])) {
      if (!current_token.empty()) {
        tokens_.push_back(current_token);
        current_token.clear();
      }
      i++;
      continue;
    }

    bool found_reserved_word = false;
    std::string current_reserved_word;
    for (const auto& entry : reserved_words_) {
      const std::string& reserved_word = entry.first;
      /* const int reserved_word_id = entry.second; */

      if (code.substr(i, reserved_word.size()) == reserved_word) {
        current_reserved_word = reserved_word;
        found_reserved_word = true;
        break;
      }
    }

    if (found_reserved_word) {
      if (!current_token.empty()) {
        tokens_.push_back(current_token);
        current_token = "";
      }
      tokens_.push_back(current_reserved_word);
      i += current_reserved_word.size();
      continue;
    }

    current_token += code[i];
    i++;
  }

  if (!current_token.empty()) {
    tokens_.push_back(current_token);
  }

  return util::OkStatus();
}

util::Status Lexer::CreateSymbolTable() {
  identifier_table_.clear();
  constant_table_.clear();

  for (const auto& token : tokens_) {
    if (reserved_words_.find(token) != reserved_words_.end()) {
      // is reserved word
      continue;
    }

    if (isConstant(token)) {
      if (constant_table_.find(token) == constant_table_.end()) {
        constant_table_[token] = constant_table_.size();
      }
    } else if (isIdentifier(token)) {
      if (identifier_table_.find(token) == identifier_table_.end()) {
        identifier_table_[token] = identifier_table_.size();
      }
    } else {
      std::stringstream buff;
      buff << "Lexical error at token <" << token << ">";

      return util::Status(util::error::INVALID_ARGUMENT, buff.str());
    }
  }

  return util::OkStatus();
}

util::Status Lexer::CreatePIF() {
  pif_.clear();
  pif_.reserve(tokens_.size());

  for (const auto& token : tokens_) {
    if (identifier_table_.find(token) != identifier_table_.end()) {
      pif_.emplace_back(identifier_table_[token], 0);
    } else if (constant_table_.find(token) != constant_table_.end()) {
      pif_.emplace_back(constant_table_[token], 1);
    } else {
      pif_.emplace_back(-1, reserved_words_[token]);
    }
  }

  return util::OkStatus();
}

bool Lexer::isConstant(const Symbol& symbol) {
  if (symbol == "true" || symbol == "false") {
    return true;
  }

  int dots = 0;
  for (const char& c : symbol) {
    if (c == '.') {
      dots++;
    } else if (!('0' <= c && c <= '9')) {
      return false;
    }
  }

  if (dots > 1) {
    return false;
  }

  return true;
}

bool Lexer::isIdentifier(const Symbol& symbol) {
  if (symbol.size() > 8) {
    return false;
  }

  for (const char& c : symbol) {
    if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) {
      continue;
    } else {
      return false;
    }
  }

  return true;
}

const std::string Lexer::SymbolTableStr(const std::string& table_name,
                                        const SymbolTable& table) {
  std::stringstream buff;

  buff << table_name << ":\n";
  buff.width(10);
  buff << "SYMBOL";
  buff << " | ID\n";
  for (const auto& entry : table) {
    buff.width(10);
    buff << entry.first << " | " << entry.second << "\n";
  }

  return buff.str();
}

const std::string Lexer::ProgramInternalFormStr() {
  std::stringstream buff;

  buff << "Program Internal Form:\n";
  buff.width(16);
  buff << "SYMBOL TABLE ID";
  buff << " | TOKEN TYPE\n";
  for (const auto& entry : pif_) {
    buff.width(16);
    buff << entry.first << " | " << entry.second << "\n";
  }

  return buff.str();
}

}  // namespace lftc
