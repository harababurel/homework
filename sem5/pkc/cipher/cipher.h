#pragma once
#include <boost/type_index.hpp>
#include <set>
#include <string>
#include "cipher/icipher.h"
#include "util/status.h"

namespace cipher {

template <class KeyT>
class Cipher : public ICipher {
 public:
  Cipher() : ICipher() {}
  Cipher(const std::string& alphabet) : ICipher(alphabet) {}

  virtual util::Status Encode(const std::string& message, const KeyT& key,
                              std::string* code) = 0;

  virtual util::Status Decode(const std::string& code, const KeyT& key,
                              std::string* message) = 0;

  const std::string TypeName() override {
    return std::string(boost::typeindex::type_id<KeyT>().pretty_name());
  }
};

}  // namespace cipher
