#include "rsa.h"
#include <NTL/ZZ.h>
#include <numeric>
#include <sstream>

namespace cipher {
namespace rsa {

RSACipher::RSACipher() {
  GenerateKeys();
  ComputeBlockSizes();
}

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

  NTL::GenPrime(p, k_factor_size_);
  NTL::GenPrime(q, k_factor_size_);
  auto n = p * q;
  auto phi_n = (p - 1) * (q - 1);

  do {
    e = 1 + NTL::RandomBnd(phi_n - 1);
  } while (NTL::GCD(e, phi_n) != 1);

  d = NTL::InvMod(e, phi_n);

  public_key_ = {n, e};
  private_key_ = d;

  std::cerr << "public key: {" << public_key_.first << ", "
            << public_key_.second << "}\n";
  std::cerr << "private key: " << private_key_ << "\n";

  return util::OkStatus();
}

util::Status RSACipher::ComputeBlockSizes() {
  auto n = public_key_.first;
  k_plaintext_block_size_ = floor(log(n) / log(alphabet_.size()));
  k_ciphertext_block_size_ = ceil(log(n) / log(alphabet_.size()));

  std::cerr << "Plaintext will be split into blocks of "
            << k_plaintext_block_size_ << " characters.\n";

  std::cerr << "Ciphertext will be split into blocks of "
            << k_ciphertext_block_size_ << " characters.\n";

  if (std::min(k_plaintext_block_size_, k_ciphertext_block_size_) == 0) {
    return util::Status(
        util::error::FAILED_PRECONDITION,
        "Plain/Cipher text must be split into blocks of size >= 1");
  }

  return util::OkStatus();
}

}  // namespace rsa
}  // namespace cipher
