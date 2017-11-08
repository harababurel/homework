#pragma once
#include <chrono>
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
        pool_(std::make_unique<ctpl::thread_pool>(n_threads, 10)) {}

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

  template <typename T>
  Matrix<T> TripleProduct(const Matrix<T> &A, const Matrix<T> &B,
                          const Matrix<T> &C) {
    CheckSize(A, B);
    CheckSize(B, C);

    Matrix<T> X(A.rows(), B.cols());
    Matrix<T> Y(X.rows(), C.cols());

    std::vector<int> computed_on_line;
    computed_on_line.resize(X.rows(), -1);

    std::vector<std::thread> threads;
    threads.reserve(2 * n_threads_);

    for (unsigned int t = 0; t < n_threads_; t++) {
      threads.emplace_back([this, &A, &B, &X, &computed_on_line, t]() {
        for (int i = t; i < X.rows(); i += n_threads_) {
          for (int j = 0; j < X.cols(); j++) {
            for (int k = 0; k < A.cols(); k++) {
              X[i][j] += A(i, k) * B(k, j);
            }
            computed_on_line[i] = j;
          }
        }
      });
    }

    for (unsigned int t = 0; t < n_threads_; t++) {
      threads.emplace_back([this, &A, &B, &C, &X, &Y, &computed_on_line, t]() {
        for (int i = t; i < Y.rows(); i += n_threads_) {
          for (int j = 0; j < Y.cols(); j++) {
            for (int k = 0; k < X.cols(); k++) {
              while (computed_on_line[i] < k) {
                std::this_thread::sleep_for(std::chrono::nanoseconds(5));
              }
              Y[i][j] += X(i, k) * C(k, j);
            }
          }
        }
      });
    }

    for (auto &thread : threads) {
      thread.join();
    }

    return Y;
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
