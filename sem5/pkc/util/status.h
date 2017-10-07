#pragma once

namespace util {
namespace error {

enum Code {
  OK = 0,
  CANCELLED = 1,
  UNKNOWN = 2,
  INVALID_ARGUMENT = 3,
  DEADLINE_EXCEEDED = 4,
  NOT_FOUND = 5,
  ALREADY_EXISTS = 6,
  PERMISSION_DENIED = 7,
  RESOURCE_EXHAUSTED = 8,
  FAILED_PRECONDITION = 9,
  ABORTED = 10,
  OUT_OF_RANGE = 11,
  UNIMPLEMENTED = 12,
  INTERNAL = 13,
  UNAVAILABLE = 14,
  DATA_LOSS = 15,
};

const Code Code_MIN = OK;
const Code Code_MAX = DATA_LOSS;

}  // namespace error

class Status {
 public:
  Status() : code_(util::error::OK) {}
  Status(util::error::Code code, const std::string& msg)
      : code_(code), msg_(msg) {}

  // Copy constructor
  Status(const Status& status) : code_(status.code_), msg_(status.msg_) {}

  // Assignment constructor
  Status& operator=(const Status& status) {
    code_ = status.code_;
    msg_ = status.msg_;
    return *this;
  }

  // Equality operator
  bool operator==(const Status& status) {
    return code_ == status.code_ && msg_ == status.msg_;
  }

  bool ok() const { return code_ == util::error::OK; }

  const std::string& error_message() const { return msg_; }

  const util::error::Code& error_code() const { return code_; }

 private:
  util::error::Code code_;
  std::string msg_;
};

// Returns an OK status, equivalent to a default constructed instance.
inline Status OkStatus() { return Status(); }

}  // namespace util
