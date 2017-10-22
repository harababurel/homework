#pragma once
#include <random>
#include <sstream>
#include <stdexcept>
#include <thread>
#include <vector>

namespace pdp {

using Size = std::pair<int, int>;

template <typename T>
class Matrix {
 public:
  Matrix() = default;
  Matrix(const Size &size) : Matrix(size.first, size.second) {}
  Matrix(int n, int m) : size_({n, m}) {
    vals_.resize(n);

    for (auto &line : vals_) {
      line.resize(m);
    }
  }

  const T &operator()(int i, int j) const { return vals_[i][j]; }

  std::vector<T> &operator[](int line) { return vals_[line]; }

  bool operator==(const Matrix<T> &other) const {
    if (size() != other.size()) {
      return false;
    }

    for (int i = 0; i < size().first; i++) {
      for (int j = 0; j < size().second; j++) {
        if ((*this)(i, j) != other(i, j)) {
          return false;
        }
      }
    }

    return true;
  }

  Matrix<T> operator+(const Matrix<T> &other) {
    CheckSize(*this, other);

    Matrix<T> ret(size());

    int n_blocks = ceil(size().first / block_size);
    int m_blocks = ceil(size().second / block_size);

    std::vector<std::thread> threads;
    threads.reserve(n_blocks * m_blocks);

    for (int i = 0; i < ret.size().first; i += block_size) {
      for (int j = 0; j < ret.size().second; j += block_size) {
        threads.emplace_back([&ret, this, &other, i, j]() {
          for (int x = i; x < std::min(ret.size().first, i + block_size); x++) {
            for (int y = j; y < std::min(ret.size().second, j + block_size);
                 y++) {
              ret[x][y] = (*this)(x, y) + other(x, y);
            }
          }
        });
      }
    }

    for (auto &thread : threads) {
      thread.join();
    }

    return std::move(ret);
  }

  Matrix<T> operator*(const Matrix<T> &other) {
    CheckSizeForProduct(*this, other);

    Matrix<T> ret(size().first, other.size().second);

    int n_blocks = ceil(size().first / block_size);
    int m_blocks = ceil(size().second / block_size);

    std::vector<std::thread> threads;
    threads.reserve(n_blocks * m_blocks);

    for (int i = 0; i < ret.size().first; i += block_size) {
      for (int j = 0; j < ret.size().second; j += block_size) {
        threads.emplace_back([&ret, this, &other, i, j]() {
          for (int x = i; x < std::min(ret.size().first, i + block_size); x++) {
            for (int y = j; y < std::min(ret.size().second, j + block_size);
                 y++) {
              for (int k = 0; k < size().second; k++) {
                ret[x][y] += (*this)(x, k) * other(k, y);
              }
            }
          }
        });
      }
    }

    for (auto &thread : threads) {
      thread.join();
    }

    return std::move(ret);
  }

  Matrix<T> SeqAdd(const Matrix<T> &other) {
    CheckSize(*this, other);

    Matrix<T> ret(size());
    for (int i = 0; i < size().first; i++) {
      for (int j = 0; j < size().second; j++) {
        ret[i][j] = vals_[i][j] + other(i, j);
      }
    }

    return std::move(ret);
  }

  Matrix<T> SeqMult(const Matrix<T> &other) {
    CheckSizeForProduct(*this, other);

    Matrix<T> ret(size().first, other.size().second);
    for (int i = 0; i < ret.size().first; i++) {
      for (int j = 0; j < ret.size().second; j++) {
        for (int k = 0; k < size().second; k++) {
          ret[i][j] += vals_[i][k] * other(k, j);
        }
      }
    }
    return std::move(ret);
  }

  const std::string str() const {
    std::stringstream buffer;

    for (const auto &line : vals_) {
      for (int i = 0; i < int(line.size()); i++) {
        buffer << line[i];

        if (i + 1 < int(line.size())) {
          buffer << " ";
        }
      }
      buffer << "\n";
    }

    return buffer.str();
  }

  const Size &size() const { return size_; }

 private:
  const int block_size = 200;
  std::vector<std::vector<T>> vals_;
  Size size_;

  void CheckSize(const Matrix<T> &A, const Matrix<T> &B) {
    if (A.size() != B.size()) throw std::length_error("Size mismatch");
  }

  void CheckSizeForProduct(const Matrix<T> &A, const Matrix<T> &B) {
    if (A.size().second != B.size().first)
      throw std::length_error("Size mismatch");
  }
};

}  // namespace pdp
