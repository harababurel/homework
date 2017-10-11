#pragma once
#include <Eigen/Dense>
#include <string>
#include <vector>
#include "cipher.h"
#include "util/status.h"

namespace cipher {
namespace hill {

using Block = Eigen::VectorXi;
using MatrixKey = Eigen::MatrixXi;
using StringKey = std::string;

class HillCipher final : public Cipher<StringKey> {
 public:
  HillCipher() = default;
  HillCipher(const std::string& alphabet) : Cipher(alphabet) {}

  util::Status Encode(const std::string& message, const StringKey& key,
                      std::string* code) override;

  util::Status Decode(const std::string& code, const StringKey& key,
                      std::string* message) override;

 private:
  void SplitMessageIntoBlocks(const std::string& message, int block_size,
                              std::vector<Block>* blocks);
  Block StringToBlock(const std::string& s, int block_size);
  std::string BlockToString(const Block& block);
  std::string MergeBlocks(const std::vector<Block>& blocks);
  util::Status StringKeyToMatrixKey(const StringKey& key,
                                    MatrixKey* matrix_key);
};

}  // namespace hill
}  // namespace cipher
