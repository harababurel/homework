#pragma once
#include <future>
#include <map>
#include <random>
#include <sstream>
#include <stdexcept>
#include <thread>
#include <vector>

namespace pdp {

using Size = std::pair<int, int>;
using Coords = std::pair<int, int>;

template <typename T>
class Matrix {
 public:
  Matrix() = default;
  Matrix(const Size &size) : Matrix(size.first, size.second) {}
  Matrix(int n, int m)
      : n_threads_(std::max(static_cast<unsigned int>(1),
                            std::thread::hardware_concurrency())),
        size_({n, m}) {
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

  /* Matrix<T> operator+(const Matrix<T> &other) { */
  /*   CheckSize(*this, other); */

  /*   Matrix<T> ret(size()); */
  /*   for (int i = 0; i < size().first; i++) { */
  /*     for (int j = 0; j < size().second; j++) { */
  /*       ret[i][j] = (*this)(i, j) + other(i, j); */
  /*     } */
  /*   } */

  /*   return ret; */
  /* } */

  /* Matrix<T> operator*(const Matrix<T> &other) { */
  /*   CheckSizeForProduct(*this, other); */

  /*   Matrix<T> ret(size().first, other.size().second); */
  /*   for (int i = 0; i < ret.size().first; i++) { */
  /*     for (int j = 0; j < ret.size().second; j++) { */
  /*       for (int k = 0; k < size().second; k++) { */
  /*         ret[i][j] += (*this)(i, k) * other(k, j); */
  /*       } */
  /*     } */
  /*   } */
  /*   return ret; */
  /* } */

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

  void set_n_threads(const int n_threads) { n_threads_ = n_threads; }

 private:
  /* void CheckSize(const Matrix<T> &A, const Matrix<T> &B) { */
  /*   if (A.size() != B.size()) { */
  /*     std::stringstream buffer; */
  /*     buffer << "Size mismatch: (" << A.size().first << ", " <<
   * A.size().second */
  /*            << ") and (" << B.size().first << ", " << B.size().second <<
   * ")"; */
  /*     throw std::length_error(buffer.str()); */
  /*   } */
  /* } */

  unsigned int n_threads_;
  std::vector<std::vector<T>> vals_;
  Size size_;
};

}  // namespace pdp
