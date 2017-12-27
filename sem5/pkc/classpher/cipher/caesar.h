#pragma once
#include <string>
#include "classical_cipher.h"
#include "util/status.h"

namespace cipher {
namespace caesar {

using Key = int;

class CaesarCipher final : public ClassicalCipher<Key> {
 public:
  util::Status Encode(const std::string& message, const Key& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const Key& key,
                      std::string* message) override;
};

}  // namespace caesar
}  // namespace cipher
