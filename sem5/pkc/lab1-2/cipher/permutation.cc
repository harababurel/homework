#include "permutation.h"
#include <cmath>
#include <numeric>
#include <sstream>
#include <vector>

namespace cipher {
namespace permutation {

using Key = std::vector<int>;  // a permutation
using Block = std::string;

util::Status PermutationCipher::Encode(const std::string& message,
                                       const Key& key, std::string* code) {
  for (const auto& c : message) {
    if (alphabet_.find(c) == std::string::npos) {
      std::stringstream msg;
      msg << "Character '" << c << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }
  }

  std::vector<Block> blocks;
  blocks.reserve(std::ceil(int(message.size()) / int(key.size())));
  for (int i = 0; i < int(message.size()); i += key.size()) {
    auto block = message.substr(i, key.size());
    ApplyPermutation(key, &block);
    blocks.push_back(std::move(block));
  }

  *code = std::accumulate(blocks.begin(), blocks.end(), std::string(""));
  return util::OkStatus();
}

util::Status PermutationCipher::Decode(const std::string& code, const Key& key,
                                       std::string* message) {
  Key inverse;
  InvertPermutation(key, &inverse);

  return Encode(code, inverse, message);
}

void PermutationCipher::ApplyPermutation(const Key& permutation, Block* block) {
  if (block->size() < permutation.size()) {
    block->resize(permutation.size(), alphabet_[0]);
  }

  Block new_block;
  new_block.resize(block->size());
  for (int i = 0; i < int(block->size()); i++) {
    new_block[i] = (*block)[permutation[i]];
  }
  *block = new_block;
}

void PermutationCipher::InvertPermutation(const Key& permutation,
                                          Key* inverse) {
  inverse->clear();
  inverse->resize(permutation.size());

  for (int i = 0; i < int(permutation.size()); i++) {
    (*inverse)[permutation[i]] = i;
  }
}

}  // namespace permutation
}  // namespace cipher
