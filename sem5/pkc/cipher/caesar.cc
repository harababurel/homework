#include "caesar.h"
#include <sstream>

namespace cipher {

util::Status CaesarCipher::Encode(const std::string& message, const int& key,
                                  std::string* code) {
  int key_prepared = (key % AlphabetSize() + AlphabetSize()) % AlphabetSize();

  *code = message;
  for (int i = 0; i < int(code->size()); i++) {
    size_t old_index = alphabet_.find((*code)[i]);

    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Character '" << (*code)[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    size_t new_index = (old_index + key_prepared) % AlphabetSize();

    (*code)[i] = alphabet_[new_index];
  }

  return util::OkStatus();
}

util::Status CaesarCipher::Decode(const std::string& code, const int& key,
                                  std::string* message) {
  return Encode(code, -key, message);
}

}  // namespace cipher
