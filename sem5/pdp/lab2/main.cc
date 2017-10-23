#include <cassert>
#include <chrono>
#include <functional>
#include <random>
#include "matrix.hpp"
#include "matrix_generator.hpp"

auto measure_time(std::function<pdp::Matrix<int>()> f) {
  auto start_time = std::chrono::steady_clock::now();
  auto result = f();
  auto end_time = std::chrono::steady_clock::now();

  auto duration = end_time - start_time;

  printf("%.0f ms\n",
         std::chrono::duration<double, std::milli>(duration).count());

  return std::move(result);
}

void test_product() {
  pdp::MatrixGenerator generator(/* bounds = */ {-1e3, 1e3},
                                 /* seed = */ time(0));
  auto A = generator.RandomUniformMatrix({1000, 100});
  auto B = generator.RandomUniformMatrix({A.size().second, 1000});

  auto seq_product = measure_time([&A, &B]() { return A.SeqMult(B); });
  auto par_product = measure_time([&A, &B]() { return A * B; });

  assert(seq_product == par_product);
}

void test_sum() {
  pdp::MatrixGenerator generator(/* bounds = */ {-1e6, 1e6},
                                 /* seed = */ time(0));

  auto A = generator.RandomUniformMatrix({5000, 5000});
  auto B = generator.RandomUniformMatrix(A.size());

  auto seq_sum = measure_time([&A, &B]() { return A.SeqAdd(B); });
  auto par_sum = measure_time([&A, &B]() { return A + B; });

  assert(seq_sum == par_sum);
}

int main(int argc, char* argv[]) {
  printf("Sum:\n");
  test_sum();

  printf("\nProduct:\n");
  test_product();

  return 0;
}
