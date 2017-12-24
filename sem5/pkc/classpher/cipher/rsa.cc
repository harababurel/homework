#include "rsa.h"
#include <NTL/ZZ.h>
#include <numeric>
#include <sstream>

namespace cipher {
namespace rsa {

util::Status RSACipher::Encode(const std::string& message, const PublicKey& key,
                               std::string* code) {
  return util::UnimplementedStatus();
}

util::Status RSACipher::Decode(const std::string& code, const PublicKey& key,
                               std::string* message) {
  return util::UnimplementedStatus();
}

util::Status RSACipher::GenerateKeys() {
  NTL::ZZ p, q, e, d;
  const int factor_size = 5;

  NTL::GenPrime(p, factor_size);
  NTL::GenPrime(q, factor_size);
  auto n = p * q;
  auto phi_n = (p - 1) * (q - 1);

  do {
    e = 1 + NTL::RandomBnd(phi_n - 1);
  } while (NTL::GCD(e, phi_n) != 1);

  d = NTL::InvMod(e, phi_n);

  this->public_key = {n, e};
  this->private_key = d;

  std::cout << "public key: {" << this->public_key.first << ", "
            << this->public_key.second << "}\n";
  std::cout << "private key: " << this->private_key << "\n";

  return util::OkStatus();
}

}  // namespace rsa
}  // namespace cipher
