#include "hill.h"
#include <sstream>
#include <vector>

namespace cipher {

using Block = Eigen::VectorXi;
using MatrixKey = Eigen::MatrixXi;
using StringKey = std::string;

util::Status HillCipher::Encode(const std::string& message,
                                const StringKey& key, std::string* code) {
  for (const auto& c : message) {
    if (alphabet_.find(c) == std::string::npos) {
      std::stringstream msg;
      msg << "Character '" << c << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }
  }

  MatrixKey matrix_key;
  auto status = StringKeyToMatrixKey(key, &matrix_key);

  if (!status.ok()) {
    return status;
  }

  std::vector<Block> blocks;
  SplitMessageIntoBlocks(message, matrix_key.rows(), &blocks);

  for (auto& block : blocks) {
    block = matrix_key * block;

    for (int i = 0; i < block.rows(); i++) {
      block[i] = (block[i] % AlphabetSize() + AlphabetSize()) % AlphabetSize();
    }
  }

  *code = MergeBlocks(blocks);

  return util::OkStatus();
}

util::Status HillCipher::Decode(const std::string& code, const StringKey& key,
                                std::string* message) {
  return util::UnimplementedStatus();
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
    s[i] = alphabet_[block(i)];
  }

  return std::move(s);
}

std::string HillCipher::MergeBlocks(const std::vector<Block>& blocks) {
  std::string s;

  for (const auto& block : blocks) {
    s += BlockToString(block);
  }

  return std::move(s);
}

util::Status HillCipher::StringKeyToMatrixKey(const StringKey& key,
                                              MatrixKey* matrix_key) {
  for (const auto& c : key) {
    if (alphabet_.find(c) == std::string::npos) {
      std::stringstream msg;
      msg << "Key character '" << c << "' not in alphabet.";
      return util::Status(util::error::INVALID_ARGUMENT, msg.str());
    }
  }

  int matrix_size = ceil(sqrt(int(key.size())));
  if (matrix_size * matrix_size != int(key.size())) {
    return util::Status(util::error::INVALID_ARGUMENT,
                        "The key must have a square length so that it can be "
                        "transformed into a square matrix.");
  }

  matrix_key->resize(matrix_size, matrix_size);
  for (int i = 0; i < matrix_size; i++) {
    for (int j = 0; j < matrix_size; j++) {
      (*matrix_key)(i, j) = alphabet_.find(key[i * matrix_size + j]);
    }
  }

  return util::OkStatus();
}

}  // namespace cipher
