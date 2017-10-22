#include <cassert>
#include <random>
#include "matrix.hpp"
#include "matrix_generator.hpp"

int main(int argc, char* argv[]) {
  /* pdp::MatrixGenerator generator(/1* bounds = *1/ {0, 100}, /1* seed = *1/
   * 42); */
  pdp::MatrixGenerator generator(/* bounds = */ {-1e9, 1e9}, time(0));

  auto A = generator.RandomUniformMatrix({5000, 5000});
  auto B = generator.RandomUniformMatrix(A.size());
  auto parC = A + B; /* auto seqC = A.SeqAdd(B); */ /* assert(parC == seqC); */

  /* printf("%d, %d\n", A.size().first, A.size().second); */
  /* printf("%d, %d\n", B.size().first, B.size().second); */

  /* printf("A =\n%s\n", A.str().c_str()); */
  /* printf("B =\n%s\n", B.str().c_str()); */
  /* printf("parC =\n%s\n", parC.str().c_str()); */
  /* printf("seqC =\n%s\n", seqC.str().c_str()); */

  return 0;
}
