#include <cassert>
#include <chrono>
#include <functional>
#include <random>
#include "matrix.hpp"
#include "matrix_generator.hpp"
#include "matrix_multiplier.hpp"

const bool VERBOSE = false;

auto measure_time(const std::string& description,
                  std::function<pdp::Matrix<int>()> f) {
  auto start_time = std::chrono::steady_clock::now();
  auto result = f();
  auto end_time = std::chrono::steady_clock::now();

  auto duration = end_time - start_time;

  printf("%s: %.0f ms\n", description.c_str(),
         std::chrono::duration<double, std::milli>(duration).count());

  return result;
}

void test_product() {
  pdp::MatrixGenerator generator(/* bounds = */ {-1e3, 1e3},
                                 /* seed = */ time(0));

  auto A = generator.RandomUniformMatrix({2000, 2000});
  auto B = generator.RandomUniformMatrix({A.size().second, 2000});
  auto mult = pdp::MatrixMultiplier(4);

  auto seq_product = measure_time(
      "Sequential", [&mult, &A, &B]() { return mult.SequentialProduct(A, B); });

  auto thread_product = measure_time("Manual threads", [&mult, &A, &B]() {
    return mult.ThreadedProduct(A, B);
  });

  auto pool_product = measure_time(
      "Threadpool", [&mult, &A, &B]() { return mult.ThreadpoolProduct(A, B); });

  auto async_product = measure_time(
      "Async", [&mult, &A, &B]() { return mult.AsyncProduct(A, B); });

  if (VERBOSE) {
    printf("A =\n%s\n", A.str().c_str());
    printf("B =\n%s\n", B.str().c_str());
    printf("A*B =\n%s\n", pool_product.str().c_str());
  }

  assert(seq_product == thread_product);
  assert(seq_product == pool_product);
  assert(seq_product == async_product);
}

int main(int argc, char* argv[]) {
  pdp::MatrixGenerator generator(/* bounds = */ {0, 100},
                                 /* seed = */ time(0));

  auto A = generator.RandomUniformMatrix({500, 500});
  auto B = generator.RandomUniformMatrix({A.size().second, 1000});
  auto C = generator.RandomUniformMatrix({B.size().second, 500});
  auto mult = pdp::MatrixMultiplier(2);

  auto seq_triple_product =
      measure_time("Basic triple product", [&mult, &A, &B, &C]() {
        return mult.ThreadedProduct(mult.ThreadedProduct(A, B), C);
      });

  auto par_triple_product = measure_time(
      "Optimized triple product",
      [&mult, &A, &B, &C]() { return mult.TripleProduct(A, B, C); });

  /* printf("%s\n", seq_triple_product.str().c_str()); */
  /* printf("%s\n", par_triple_product.str().c_str()); */

  assert(par_triple_product == seq_triple_product);

  /* printf("\nProduct:\n"); */
  /* test_product(); */

  return 0;
}
