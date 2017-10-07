#pragma once
#include <set>
#include <string>
#include "util/status.h"

namespace cipher {

template <class KeyT>
class Cipher {
 public:
  Cipher() : alphabet_(" abcdefghijklmnopqrstuvwxyz"){};
  Cipher(const std::string& alphabet) { SetAlphabet(alphabet); }

  virtual util::Status Encode(const std::string& message, const KeyT& key,
                              std::string* code) = 0;

  virtual util::Status Decode(const std::string& code, const KeyT& key,
                              std::string* message) = 0;

  util::Status SetAlphabet(const std::string& alphabet) {
    if (alphabet.empty()) {
      return util::Status(util::error::INVALID_ARGUMENT,
                          "Alphabet must not be empty.");
    } else if (std::set<char>(alphabet.begin(), alphabet.end()).size() !=
               alphabet.size()) {
      return util::Status(util::error::INVALID_ARGUMENT,
                          "Alphabet must not contain duplicates.");
    }

    alphabet_ = alphabet;
    return util::OkStatus();
  }
  const std::string& alphabet() { return alphabet_; }

 protected:
  std::string alphabet_;
  int AlphabetSize() { return int(alphabet_.size()); }
};

}  // namespace cipher
