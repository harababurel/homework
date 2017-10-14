#pragma once
#include <NTL/ZZ.h>
#include <sstream>
#include <unordered_map>
#include "factorization.h"
#include "util/status.h"

namespace f11n {

class Factorizer {
 public:
  Factorizer() : num_trials_(10) {}
  Factorizer(int num_trials) : num_trials_(num_trials) {}

  using Base = NTL::ZZ;
  using Exponent = int;

  util::Status factorize(const NTL::ZZ& n, Factorization* factorization);

 private:
  int num_trials_;
};

}  // namespace f11n
