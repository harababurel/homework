#pragma once
#include <string>
#include "classical_cipher.h"
#include "util/status.h"

namespace cipher {
namespace affine {

using Key = std::pair<int, int>;

class AffineCipher final : public ClassicalCipher<Key> {
 public:
  util::Status Encode(const std::string& message, const Key& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const Key& key,
                      std::string* message) override;

 private:
  util::Status CheckValidKey(const Key& key);
};

}  // namespace affine
}  // namespace cipher
