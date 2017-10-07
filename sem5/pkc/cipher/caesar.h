#pragma once
#include <string>
#include "cipher.h"
#include "util/status.h"

namespace cipher {

class CaesarCipher final : public Cipher<int> {
 public:
  util::Status Encode(const std::string& message, const int& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const int& key,
                      std::string* message) override;
};

}  // namespace cipher
