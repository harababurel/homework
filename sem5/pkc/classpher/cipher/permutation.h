#pragma once
#include <string>
#include <vector>
#include "classical_cipher.h"
#include "util/status.h"

namespace cipher {
namespace permutation {

using Key = std::vector<int>;  // a permutation
using Block = std::string;

class PermutationCipher final : public ClassicalCipher<Key> {
 public:
  PermutationCipher() = default;
  PermutationCipher(const std::string& alphabet) : ClassicalCipher(alphabet) {}

  util::Status Encode(const std::string& message, const Key& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const Key& key,
                      std::string* message) override;

 private:
  void ApplyPermutation(const Key& permutation, Block* block);
  void InvertPermutation(const Key& permutation, Key* inverse);
};

}  // namespace permutation
}  // namespace cipher
