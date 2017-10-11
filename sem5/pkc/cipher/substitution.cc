#include "substitution.h"
#include <algorithm>
#include <sstream>

namespace cipher {
namespace substitution {

util::Status SubstitutionCipher::Encode(const std::string& message,
                                        const Key& key, std::string* code) {
  auto status = CheckValidKey(key);
  if (!status.ok()) return status;

  code->resize(message.size(), '.');
  for (int i = 0; i < int(message.size()); i++) {
    size_t index = alphabet_.find(message[i]);

    if (index == std::string::npos) {
      std::stringstream msg;
      msg << "Message character '" << message[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    (*code)[i] = key[index];
  }

  return util::OkStatus();
}

util::Status SubstitutionCipher::Decode(const std::string& code, const Key& key,
                                        std::string* message) {
  auto status = CheckValidKey(key);
  if (!status.ok()) return status;

  message->resize(code.size(), '.');
  for (int i = 0; i < int(code.size()); i++) {
    size_t index = key.find(code[i]);

    if (index == std::string::npos) {
      std::stringstream msg;
      msg << "Code character '" << code[i] << "' not in key.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    (*message)[i] = alphabet_[index];
  }

  return util::OkStatus();
}

util::Status SubstitutionCipher::CheckValidKey(const Key& key) {
  std::string expected = alphabet();
  std::string actual = key;

  std::sort(expected.begin(), expected.end());
  std::sort(actual.begin(), actual.end());

  if (expected == actual) {
    return util::OkStatus();
  } else {
    return util::Status(util::error::INVALID_ARGUMENT,
                        "The key must be a permutation of the alphabet.");
  }
}

}  // namespace substitution
}  // namespace cipher
