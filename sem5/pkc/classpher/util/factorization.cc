#include "factorization.h"
#include "ntl_hash.h"

namespace f11n {

Factorization Factorization::operator&(const Factorization& other) const {
  Factorization common;

  for (const auto& factor : factors()) {
    const auto& base = factor.first;
    const auto& exponent = factor.second;

    if (other.Contains(base)) {
      const auto& other_exponent = other.GetExponent(base);
      common.AddOrUpdate(base, std::min(exponent, other_exponent));
    }
  }

  return common;
}

}  // namespace f11n
