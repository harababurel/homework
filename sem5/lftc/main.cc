/* aba */
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include "lexer.h"

void load_symbols(const std::string& filename,
                  std::vector<std::string>* symbols) {
  std::ifstream file(filename);

  if (!file.is_open()) {
    std::cout << "Could not open symbol file!\n";
  }

  lftc::Symbol symbol;
  while (file >> symbol) {
    /* std::cout << "<" << symbol << ">\n"; */
    symbols->push_back(symbol);
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
  std::vector<std::string> symbols;
  load_symbols("symbols.csv", &symbols);

  lftc::Lexer lexer(symbols);
  std::string code = load_code("ex1.cc");

  lftc::TokenVector tokens;
  lexer.Tokenize(code, &tokens);

  std::cout << "Tokens:\n";
  for (const auto& token : tokens) {
    std::cout << token << "\n";
  }

  return 0;
}
