#pragma once
#include <NTL/ZZ.h>
#include <string>
#include "cipher.h"
#include "util/status.h"

namespace cipher {
namespace rsa {

using PublicKey = std::pair<NTL::ZZ, NTL::ZZ>;
using PrivateKey = NTL::ZZ;
using Block = NTL::ZZ;

class RSACipher final : public Cipher<PublicKey> {
 public:
  RSACipher();
  util::Status Encode(const std::string& message, const PublicKey& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const PublicKey& key,
                      std::string* message) override;

  PublicKey public_key_;

 private:
  util::Status GenerateKeys();
  util::Status ComputeBlockSizes();
  Block StringToBlock(const std::string& s, const size_t block_size);
  std::string BlockToString(const Block& block, const size_t block_size);
  util::Status SplitMessageIntoBlocks(const std::string& message,
                                      const size_t block_size,
                                      std::vector<Block>* blocks);

  PrivateKey private_key_;
  size_t k_plaintext_block_size_;
  size_t k_ciphertext_block_size_;
  const size_t k_factor_size_ = 10;
};

}  // namespace rsa
}  // namespace cipher
