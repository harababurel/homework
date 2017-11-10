#pragma once
#include <vector>
#include "util/status.h"

namespace cipher {

class ICipher {
 public:
  ICipher() : alphabet_(" abcdefghijklmnopqrstuvwxyz"){};
  ICipher(const std::string& alphabet) { SetAlphabet(alphabet); }

  virtual util::Status Encode(const std::string& message,
                              const std::string& key, std::string* code) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Decode(const std::string& code, const std::string& key,
                              std::string* message) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Encode(const std::string& message, const int& key,
                              std::string* code) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Decode(const std::string& code, const int& key,
                              std::string* message) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Encode(const std::string& message,
                              const std::pair<int, int>& key,
                              std::string* code) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Decode(const std::string& code,
                              const std::pair<int, int>& key,
                              std::string* message) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Encode(const std::string& message,
                              const std::vector<int>& key, std::string* code) {
    return util::UnimplementedStatus();
  }

  virtual util::Status Decode(const std::string& code,
                              const std::vector<int>& key,
                              std::string* message) {
    return util::UnimplementedStatus();
  }

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
  virtual const std::string TypeName() = 0;

 protected:
  std::string alphabet_;
  int base() { return int(alphabet_.size()); }
  int normalize(const int index) {
    return ((index % base()) + base()) % base();
  }
};

}  // namespace cipher
