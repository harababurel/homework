#pragma once
#include <NTL/ZZ.h>
#include <unordered_map>
#include "ntl_hash.h"

namespace f11n {

using Base = NTL::ZZ;
using Exponent = int;
using FactorMap = std::unordered_map<Base, Exponent>;

class Factorization {
 public:
  Factorization operator&(const Factorization& other) const;

  bool Contains(const Base& base) const {
    return factors_.find(base) != factors_.end();
  }

  void AddOrUpdate(const Base& base, const Exponent& exp) {
    factors_[base] = exp;
  }

  void AddIfNotPresent(const Base& base, const Exponent& exp) {
    if (!Contains(base)) {
      factors_[base] = exp;
    }
  }

  Exponent GetExponent(const Base& base) const {
    if (!Contains(base)) {
      return 0;
    }

    return factors_.find(base)->second;
  }

  NTL::ZZ Product() {
    NTL::ZZ product(1);
    for (auto factor : factors_) {
      product *= NTL::power(factor.first, factor.second);
    }

    return product;
  }

  const FactorMap& factors() const { return factors_; }

 private:
  FactorMap factors_;
};
}  // namespace f11n
