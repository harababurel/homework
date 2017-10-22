#include <cassert>
#include <random>
#include "matrix.hpp"
#include "matrix_generator.hpp"

void test_product() {
  pdp::MatrixGenerator generator(/* bounds = */ {1, 1e3}, time(0));
  auto A = generator.RandomUniformMatrix({600, 600});
  auto B = generator.RandomUniformMatrix({A.size().second, 600});

  auto seq_product = A.SeqMult(B);
  auto par_product = A * B;

  /* printf("seq_product =\n%s\n", seq_product.str().c_str()); */
  /* printf("par_product =\n%s\n", par_product.str().c_str()); */

  assert(seq_product == par_product);
}

void test_sum() {
  pdp::MatrixGenerator generator(/* bounds = */ {1, 10}, time(0));

  auto A = generator.RandomUniformMatrix({10, 10});
  auto B = generator.RandomUniformMatrix(A.size());

  auto seq_sum = A.SeqAdd(B);
  auto par_sum = A + B;

  assert(par_sum == seq_sum);
}

int main(int argc, char* argv[]) {
  test_sum();
  test_product();

  return 0;
}
