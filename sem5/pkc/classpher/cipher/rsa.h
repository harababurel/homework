#pragma once
#include <NTL/ZZ.h>
#include <string>
#include "cipher.h"
#include "util/status.h"

namespace cipher {
namespace rsa {

using PublicKey = std::pair<NTL::ZZ, NTL::ZZ>;
using PrivateKey = NTL::ZZ;

class RSACipher final : public Cipher<PublicKey> {
 public:
  RSACipher();
  util::Status Encode(const std::string& message, const PublicKey& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const PublicKey& key,
                      std::string* message) override;

 private:
  util::Status GenerateKeys();
  util::Status ComputeBlockSizes();

  PublicKey public_key_;
  PrivateKey private_key_;
  unsigned int k_plaintext_block_size_;
  unsigned int k_ciphertext_block_size_;
  const unsigned int k_factor_size_ = 10;
};

}  // namespace rsa
}  // namespace cipher
