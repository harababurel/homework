#include "gcd.h"
#include "factorizer.h"

NTL::ZZ GCD::subtraction_gcd(const NTL::ZZ& a, const NTL::ZZ& b) {
  NTL::ZZ x = a, y = b;
  while (x != y) {
    if (x > y) {
      x -= y;
    } else {
      y -= x;
    }
  }

  return x;
}

NTL::ZZ GCD::division_gcd(const NTL::ZZ& a, const NTL::ZZ& b) {
  NTL::ZZ x = a, y = b, aux;
  while (y != 0) {
    aux = y;
    y = x % y;
    x = aux;
  }

  return x;
}

NTL::ZZ GCD::factorization_gcd(const NTL::ZZ& a, const NTL::ZZ& b) {
  f11n::Factorization a_factors, b_factors;
  f11n::Factorizer factorizer;
  factorizer.factorize(a, &a_factors);
  factorizer.factorize(b, &b_factors);

  return (a_factors & b_factors).Product();
}
