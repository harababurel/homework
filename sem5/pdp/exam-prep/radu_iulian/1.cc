#include <iostream>
#include <thread>
#include <vector>

bool pred(std::vector<int> const& v) { return true; }

const int n = 22, k = 10;
const int n_threads = 8;
int sols[n_threads];

int main(int argc, char* argv[]) {
  int sol = 0;

  std::vector<std::thread> threads;
  for (int t = 0; t < n_threads; t++) {
    threads.emplace_back([&sols, t]() {
      for (int conf = t; conf < (1 << n); conf += n_threads) {
        int size = 0;
        for (int j = 0; j < n && size <= k; j++) {
          if ((1 << j) & conf) {
            size++;
          }
        }

        if (size == k) {
          std::vector<int> xs;
          xs.reserve(k);
          for (int j = 0; j < n; j++) {
            if ((1 << j) & conf) {
              xs.push_back(j);
            }
          }
          if (pred(xs)) {
            sols[t]++;
          }
        }
      }
    });
  }

  for (int i = 0; i < int(threads.size()); i++) {
    threads[i].join();
    sol += sols[i];
  }

  std::cout << "sol = " << sol << "\n";

  return 0;
}
