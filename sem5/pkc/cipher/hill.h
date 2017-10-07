#pragma once
#include <Eigen/Dense>
#include <string>
#include <vector>
#include "cipher.h"
#include "util/status.h"

namespace cipher {

using Block = Eigen::VectorXi;
using Key = Eigen::MatrixXi;

class HillCipher final : public Cipher<Key> {
 public:
  HillCipher() = default;
  HillCipher(const std::string& alphabet) : Cipher(alphabet) {}

  util::Status Encode(const std::string& message, const Key& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const Key& key,
                      std::string* message) override;

 private:
  void SplitMessageIntoBlocks(const std::string& message, int block_size,
                              std::vector<Block>* blocks);
  Block StringToBlock(const std::string& s, int block_size);
  std::string BlockToString(const Block& block);
  std::string MergeBlocks(const std::vector<Block>& blocks);
};

}  // namespace cipher
