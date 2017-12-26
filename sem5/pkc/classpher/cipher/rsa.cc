#include "rsa.h"
#include <NTL/ZZ.h>
#include <algorithm>
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
  std::vector<Block> blocks;
  SplitMessageIntoBlocks(message, k_plaintext_block_size_, &blocks);

  code->clear();
  for (auto block : blocks) {
    Block cipher_block = NTL::PowerMod(block, key.second, key.first);
    (*code) += BlockToString(cipher_block, k_plaintext_block_size_);
  }

  return util::OkStatus();
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

util::Status RSACipher::SplitMessageIntoBlocks(const std::string& message,
                                               const size_t block_size,
                                               std::vector<Block>* blocks) {
  size_t block_count = message.size() / block_size;
  if (message.size() % block_size) {
    block_count++;
  }

  blocks->clear();
  blocks->reserve(block_count);

  for (size_t i = 0; i < block_count; i++) {
    const std::string s = message.substr(i * block_size, block_size);
    blocks->push_back(StringToBlock(s, block_size));
  }

  return util::OkStatus();
}

Block RSACipher::StringToBlock(const std::string& s, const size_t block_size) {
  Block ret(0);
  for (size_t i = 0; i < block_size; i++) {
    ret = ret * alphabet_.size() + (i < s.size() ? alphabet_.find(s[i]) : 0);
  }

  return ret;
}

std::string RSACipher::BlockToString(const Block& block,
                                     const size_t block_size) {
  std::string ret;
  Block x = block;
  for (size_t i = 0; i <= block_size; i++) {
    ret += alphabet_[x % alphabet_.size()];
    x /= alphabet_.size();
  }

  std::reverse(ret.begin(), ret.end());
  return ret;
}

}  // namespace rsa
}  // namespace cipher
