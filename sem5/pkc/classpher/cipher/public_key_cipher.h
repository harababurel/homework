#pragma once
#include <boost/type_index.hpp>
#include <set>
#include <string>
#include "cipher/icipher.h"
#include "util/status.h"

namespace cipher {

template <class PublicKeyT, class PrivateKeyT>
class PublicKeyCipher : public ICipher {
 public:
  PublicKeyCipher() : ICipher() {}
  PublicKeyCipher(const std::string& alphabet) : ICipher(alphabet) {}

  virtual util::Status Encode(const std::string& message, const PublicKeyT& key,
                              std::string* code) = 0;

  virtual util::Status Decode(const std::string& code, const PrivateKeyT& key,
                              std::string* message) = 0;

  const std::string TypeName() override {
    return std::string(boost::typeindex::type_id<PublicKeyT>().pretty_name()) +
           ", " +
           std::string(boost::typeindex::type_id<PublicKeyT>().pretty_name());
  }
};

}  // namespace cipher
