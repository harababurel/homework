#pragma once
#include <sstream>
#include <thread>
#include "ctpl.h"
#include "matrix.hpp"

namespace pdp {

class MatrixMultiplier {
 public:
  MatrixMultiplier()
      : MatrixMultiplier(std::max(static_cast<unsigned int>(1),
                                  std::thread::hardware_concurrency())) {}

  MatrixMultiplier(int n_threads)
      : n_threads_(n_threads),
        pool_(std::make_unique<ctpl::thread_pool>(n_threads, 2)) {}

  template <typename T>
  Matrix<T> SequentialProduct(const Matrix<T> &A, const Matrix<T> &B) {
    CheckSize(A, B);
    Matrix<T> ret(A.size().first, B.size().second);

    for (int i = 0; i < ret.size().first; i++) {
      for (int j = 0; j < ret.size().second; j++) {
        for (int k = 0; k < A.size().second; k++) {
          ret[i][j] += A(i, k) * B(k, j);
        }
      }
    }

    return ret;
  }

  template <typename T>
  Matrix<T> ThreadedProduct(const Matrix<T> &A, const Matrix<T> &B) {
    CheckSize(A, B);
    Matrix<T> ret(A.size().first, B.size().second);

    std::vector<std::thread> threads;
    threads.reserve(n_threads_);
    for (unsigned int t = 0; t < n_threads_; t++) {
      threads.emplace_back([this, &A, &B, &ret, t]() {
        for (int i = t; i < ret.size().first; i += n_threads_) {
          for (int j = 0; j < ret.size().second; j++) {
            for (int k = 0; k < A.size().second; k++) {
              ret[i][j] += A(i, k) * B(k, j);
            }
          }
        }
      });
    }

    for (auto &thread : threads) {
      thread.join();
    }

    return ret;
  }

  template <typename T>
  Matrix<T> ThreadpoolProduct(const Matrix<T> &A, const Matrix<T> &B) {
    CheckSize(A, B);
    Matrix<T> ret(A.size().first, B.size().second);

    std::vector<std::future<void>> results(n_threads_);
    for (unsigned int t = 0; t < n_threads_; t++) {
      results[t] = pool_->push([this, &A, &B, &ret, t](int) {
        for (int i = t; i < ret.size().first; i += n_threads_) {
          for (int j = 0; j < ret.size().second; j++) {
            for (int k = 0; k < A.size().second; k++) {
              ret[i][j] += A(i, k) * B(k, j);
            }
          }
        }
      });
    }

    for (unsigned int t = 0; t < n_threads_; t++) {
      results[t].get();
    }

    return ret;
  }

  template <typename T>
  Matrix<T> AsyncProduct(const Matrix<T> &A, const Matrix<T> &B) {
    CheckSize(A, B);
    Matrix<T> ret(A.size().first, B.size().second);

    std::vector<std::future<void>> results;
    results.reserve(n_threads_);
    for (unsigned int t = 0; t < n_threads_; t++) {
      results.emplace_back(
          std::async(std::launch::async, [this, &A, &B, &ret, t]() {
            for (int i = t; i < ret.size().first; i += n_threads_) {
              for (int j = 0; j < ret.size().second; j++) {
                for (int k = 0; k < A.size().second; k++) {
                  ret[i][j] += A(i, k) * B(k, j);
                }
              }
            }
          }));
    }

    for (unsigned int t = 0; t < n_threads_; t++) {
      results[t].get();
    }

    return ret;
  }

 private:
  unsigned int n_threads_;
  std::unique_ptr<ctpl::thread_pool> pool_;

  template <typename T>
  void CheckSize(const Matrix<T> &A, const Matrix<T> &B) {
    if (A.size().second != B.size().first) {
      std::stringstream buffer;
      buffer << "Size mismatch: (" << A.size().first << ", " << A.size().second
             << ") and (" << B.size().first << ", " << B.size().second << ")";
      throw std::length_error(buffer.str());
    }
  }
};

}  // namespace pdp
