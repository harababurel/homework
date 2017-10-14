#include "factorizer.h"

namespace f11n {

util::Status Factorizer::factorize(const NTL::ZZ& n,
                                   Factorization* factorization) {
  NTL::ZZ x = n;
  NTL::ZZ current_prime(2);

  while (current_prime <= x) {
    while (x % current_prime == 0) {
      factorization->AddIfNotPresent(current_prime, 0);
      factorization->AddOrUpdate(current_prime,
                                 factorization->GetExponent(current_prime) + 1);

      x /= current_prime;
    }

    current_prime = NTL::NextPrime(current_prime + 1, num_trials_);
  }
  return util::OkStatus();
}

}  // namespace f11n
