#pragma once
#include <NTL/ZZ.h>
#include <optional>
#include <string>
#include "public_key_cipher.h"
#include "util/status.h"

namespace cipher {
namespace rsa {

using Block = NTL::ZZ;

class RSACipher final : public PublicKeyCipher<PublicKey, PrivateKey> {
 public:
  RSACipher();
  util::Status Encode(const std::string& message, const PublicKey& public_key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const PrivateKey& private_key,
                      std::string* message) override;

 private:
  util::Status GenerateKeys();
  util::Status ComputeBlockSizes();
  Block StringToBlock(const std::string& s, const size_t block_size);

  /* If block_size is provided, the resulting string will have exactly
   * block_size characters. Otherwise, excess characters will be trimmed. */
  std::string BlockToString(const Block& block,
                            const std::optional<size_t> block_size);

  util::Status SplitMessageIntoBlocks(const std::string& message,
                                      const size_t block_size,
                                      std::vector<Block>* blocks);

  size_t k_plaintext_block_size_;
  size_t k_ciphertext_block_size_;
  const size_t k_factor_size_ = 14;
};

}  // namespace rsa
}  // namespace cipher
