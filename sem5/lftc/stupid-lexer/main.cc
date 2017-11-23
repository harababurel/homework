/* aba */
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "dfa/dfa.h"
#include "lexer.h"

void load_reserved_words(const std::string& filename,
                         std::unordered_map<std::string, int>* reserved_words) {
  std::ifstream file(filename);

  if (!file.is_open()) {
    std::cout << "Could not open symbol file!\n";
  }

  lftc::Symbol symbol;
  int symbol_id;
  while (file >> symbol >> symbol_id) {
    (*reserved_words)[symbol] = symbol_id;
  }

  file.close();
}

std::string load_code(const std::string& filename) {
  std::ifstream file(filename);
  if (!file.is_open()) {
    std::cout << "Could not open code file!\n";
  }

  std::stringstream buffer;
  buffer << file.rdbuf();

  return buffer.str();
}

int main(int argc, char* argv[]) {
  std::unordered_map<std::string, int> reserved_words;
  load_reserved_words("reserved_words.csv", &reserved_words);

  lftc::Lexer lexer(reserved_words);
  std::string code = load_code("data/gcd.cc");

  lexer.Tokenize(code);
  auto status = lexer.CreateSymbolTable();

  if (!status.ok()) {
    std::cout << status.error_message() << "\n";
    return 1;
  }

  lexer.CreatePIF();

  std::cout << lexer.IdentifierTableStr() << "\n";
  std::cout << lexer.ConstantTableStr() << "\n";
  std::cout << lexer.ProgramInternalFormStr() << "\n";

  return 0;
}
