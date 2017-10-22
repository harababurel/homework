#include <memory>
#include "matrix.hpp"

namespace pdp {

using Bounds = std::pair<int, int>;

class MatrixGenerator {
 public:
  MatrixGenerator(const Bounds& bounds)
      : gen_(std::make_unique<std::mt19937>()),
        dis_(std::make_unique<std::uniform_int_distribution<int>>(
            bounds.first, bounds.second)) {}

  MatrixGenerator(const Bounds& bounds, const int seed)
      : gen_(std::make_unique<std::mt19937>(seed)),
        dis_(std::make_unique<std::uniform_int_distribution<int>>(
            bounds.first, bounds.second)) {}

  Matrix<int> RandomUniformMatrix(const Size& size) {
    Matrix<int> ret(size);

    for (int i = 0; i < size.first; i++) {
      for (int j = 0; j < size.second; j++) {
        ret[i][j] = (*dis_)(*gen_);
      }
    }

    return std::move(ret);
  }

 private:
  std::unique_ptr<std::mt19937> gen_;
  std::unique_ptr<std::uniform_int_distribution<int>> dis_;
};

}  // namespace pdp
