#include "affine.h"
#include <NTL/ZZ.h>
#include <numeric>
#include <sstream>

namespace cipher {
namespace affine {

util::Status AffineCipher::Encode(const std::string& message, const Key& key,
                                  std::string* code) {
  auto status = CheckValidKey(key);
  if (!status.ok()) return status;

  code->resize(message.size(), '.');
  for (int i = 0; i < int(message.size()); i++) {
    size_t old_index = alphabet_.find(message[i]);

    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Message character '" << message[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    size_t new_index = normalize(key.first * old_index + key.second);
    (*code)[i] = alphabet_[new_index];
  }

  return util::OkStatus();
}

util::Status AffineCipher::Decode(const std::string& code, const Key& key,
                                  std::string* message) {
  auto status = CheckValidKey(key);
  if (!status.ok()) return status;

  message->resize(code.size(), '.');
  for (int i = 0; i < int(code.size()); i++) {
    size_t old_index = alphabet_.find(code[i]);

    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Code character '" << code[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    int a_inv = NTL::InvMod(key.first, base());
    size_t new_index = normalize(a_inv * (old_index - key.second));
    (*message)[i] = alphabet_[new_index];
  }

  return util::OkStatus();
}

util::Status AffineCipher::CheckValidKey(const Key& key) {
  if (std::gcd(key.first, base()) == 1) {
    return util::OkStatus();
  } else {
    return util::Status(
        util::error::INVALID_ARGUMENT,
        "The value a must be chosen such that a and m are coprime.");
  }
}

}  // namespace affine
}  // namespace cipher
