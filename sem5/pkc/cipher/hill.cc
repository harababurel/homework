#include "hill.h"
#include <sstream>
#include <vector>

namespace cipher {

using Block = Eigen::VectorXi;
using Key = Eigen::MatrixXi;

util::Status HillCipher::Encode(const std::string& message, const Key& key,
                                std::string* code) {
  if (key.rows() != key.cols()) {
    return util::Status(util::error::INVALID_ARGUMENT,
                        "The key must be a square matrix.");
  }

  std::vector<Block> blocks;
  SplitMessageIntoBlocks(message, key.rows(), &blocks);

  printf("there are %d blocks.\n", int(blocks.size()));

  for (auto& block : blocks) {
    block = key * block;

    for (int i = 0; i < block.rows(); i++) {
      block[i] = (block[i] % AlphabetSize() + AlphabetSize()) % AlphabetSize();
    }
  }

  *code = MergeBlocks(blocks);

  return util::OkStatus();
}

util::Status HillCipher::Decode(const std::string& code, const Key& key,
                                std::string* message) {
  return util::Status(util::error::UNIMPLEMENTED, "Not implemented.");
}

void HillCipher::SplitMessageIntoBlocks(const std::string& message,
                                        int block_size,
                                        std::vector<Block>* blocks) {
  int block_count = int(message.size()) / block_size;
  if (int(message.size()) % block_size) {
    block_count++;
  }

  blocks->clear();
  blocks->reserve(block_count);

  for (int i = 0; i < block_count; i++) {
    std::string s = message.substr(i * block_size, block_size);
    printf("new piece: %s\n", s.c_str());
    blocks->push_back(StringToBlock(s, block_size));
  }
}

Block HillCipher::StringToBlock(const std::string& s, int block_size) {
  Block block(block_size);

  for (int i = 0; i < block_size; i++) {
    block(i) = (i < AlphabetSize() ? int(alphabet_.find(s[i])) : 0);
  }

  return std::move(block);
}

std::string HillCipher::BlockToString(const Block& block) {
  std::string s(block.rows(), 0);

  for (int i = 0; i < block.rows(); i++) {
    printf("block(%d) == %d\n", i, block(i));
    s[i] = alphabet_[block(i)];
  }

  printf("block was converted to <%s>\n", s.c_str());

  return s;
}

std::string HillCipher::MergeBlocks(const std::vector<Block>& blocks) {
  std::string s;

  for (const auto& block : blocks) {
    s += BlockToString(block);
  }

  /* return std::move(s); */
  return s;
}

}  // namespace cipher
