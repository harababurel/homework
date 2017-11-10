#pragma once
#include <NTL/ZZ.h>

class GCD {
 public:
  static NTL::ZZ subtraction_gcd(const NTL::ZZ& a, const NTL::ZZ& b);
  static NTL::ZZ division_gcd(const NTL::ZZ& a, const NTL::ZZ& b);
  static NTL::ZZ factorization_gcd(const NTL::ZZ& a, const NTL::ZZ& b);
};
