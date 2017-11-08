
template <typename T>
Matrix<T> Add(const Matrix<T> &A, const Matrix<T> &B) {
  CheckSize(A, B);

  Matrix<T> ret(A.size());

  std::vector<std::thread> threads;
  threads.reserve(n_threads_);

  for (unsigned int t = 0; t < n_threads_; t++) {
    threads.emplace_back([this, &ret, &A, &B, t]() {
      for (int i = t; i < ret.size().first; i += n_threads_) {
        for (int j = 0; j < ret.size().second; j++) {
          ret[i][j] = A(i, j) + B(i, j);
        }
      }
    });
  }

  for (auto &thread : threads) {
    thread.join();
  }

  return ret;
}
