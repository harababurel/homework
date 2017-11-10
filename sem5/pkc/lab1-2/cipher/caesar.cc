#include "caesar.h"
#include <sstream>

namespace cipher {
namespace caesar {

util::Status CaesarCipher::Encode(const std::string& message, const Key& key,
                                  std::string* code) {
  int delta = (key % base() + base()) % base();

  code->resize(message.size(), '.');
  for (int i = 0; i < int(message.size()); i++) {
    size_t old_index = alphabet_.find(message[i]);

    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Message character '" << message[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    size_t new_index = (old_index + delta) % base();
    (*code)[i] = alphabet_[new_index];
  }

  return util::OkStatus();
}

util::Status CaesarCipher::Decode(const std::string& code, const Key& key,
                                  std::string* message) {
  return Encode(code, -key, message);
}

}  // namespace caesar
}  // namespace cipher
