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
  util::Status Encode(const std::string& message, const PublicKey& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const PublicKey& key,
                      std::string* message) override;

  util::Status GenerateKeys();

 private:
  PublicKey public_key;
  PrivateKey private_key;
};

}  // namespace rsa
}  // namespace cipher
