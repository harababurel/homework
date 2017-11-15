#pragma once
#include <string>
#include "cipher.h"
#include "util/status.h"

namespace cipher {
namespace vigenere {

using Key = std::string;

class VigenereCipher final : public Cipher<Key> {
 public:
  util::Status Encode(const std::string& message, const Key& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const Key& key,
                      std::string* message) override;
};

}  // namespace vigenere
}  // namespace cipher
