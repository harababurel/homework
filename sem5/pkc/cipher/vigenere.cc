#include "vigenere.h"
#include <sstream>

namespace cipher {

using Key = std::string;

util::Status VigenereCipher::Encode(const std::string& message, const Key& key,
                                    std::string* code) {
  for (char c : key) {
    if (alphabet_.find(c) == std::string::npos) {
      std::stringstream msg;
      msg << "Key character '" << c << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }
  }

  *code = message;
  for (int i = 0; i < int(message.size()); i++) {
    size_t key_index = alphabet_.find(key[i % int(key.size())]);
    int delta = (key_index % AlphabetSize() + AlphabetSize()) % AlphabetSize();

    size_t old_index = alphabet_.find(message[i]);
    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Message character '" << key[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    size_t new_index = (old_index + delta) % AlphabetSize();
    (*code)[i] = alphabet_[new_index];
  }

  return util::OkStatus();
}

util::Status VigenereCipher::Decode(const std::string& code, const Key& key,
                                    std::string* message) {
  std::string reverse_key = key;

  for (int i = 0; i < int(key.size()); i++) {
    size_t old_index = alphabet_.find(key[i]);

    if (old_index == std::string::npos) {
      std::stringstream msg;
      msg << "Character '" << key[i] << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }

    size_t new_index =
        ((AlphabetSize() - old_index) % AlphabetSize() + AlphabetSize()) %
        AlphabetSize();

    reverse_key[i] = alphabet_[new_index];
  }

  return Encode(code, reverse_key, message);
}

}  // namespace cipher
