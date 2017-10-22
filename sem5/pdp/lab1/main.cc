#include <random>
#include <thread>
#include "tree.h"

const int MAX_VAL = 1000;
const int THREADS = 2000;

int main(int argc, char* argv[]) {
  pdp::Tree t;

  std::thread t1([&t]() { t.AddSymbol("a", 0); });
  std::thread t2([&t]() { t.AddSymbol("b", {"a"}); });

  t1.join();
  t2.join();

  std::thread t3([&t]() { t.UpdateSymbol("a", 1); });
  std::thread t4([&t]() { t.UpdateSymbol("a", 2); });
  std::thread t5([&t]() { t.UpdateSymbol("a", 3); });
  std::thread tc([&t]() { t.ConsistencyCheck(); });
  t3.join();
  t4.join();
  t5.join();
  tc.join();

  /* std::thread t1([&t]() { t.AddSymbol("a", 2); }); */
  /* std::thread t2([&t]() { t.AddSymbol("b", 5); }); */
  /* std::thread t3([&t]() { t.AddSymbol("d", 100); }); */
  /* t1.join(); */
  /* t2.join(); */
  /* t3.join(); */

  /* std::thread t4([&t]() { t.AddSymbol("c", {"a", "b"}); }); */
  /* t4.join(); */

  /* std::thread t5([&t]() { t.AddSymbol("e", {"c", "d"}); }); */
  /* t5.join(); */

  /* std::thread t6([&t]() { t.UpdateSymbol("a", 100); }); */
  /* std::thread t7([&t]() { t.UpdateSymbol("a", 0); }); */
  /* t6.join(); */
  /* t7.join(); */

  /* std::mt19937 gen(42); */
  /* std::uniform_int_distribution<int> dis(0, MAX_VAL); */
  /* std::vector<std::thread> threads; */
  /* for (int i = 0; i < THREADS; i++) { */
  /*   int val = dis(gen); */
  /*   printf("Setting \"a\" to %d\n", val); */
  /*   threads.emplace_back([&t, val]() { t.UpdateSymbol("a", val); }); */
  /*   threads.emplace_back([&t]() { t.ConsistencyCheck(); }); */
  /* } */

  /* std::thread consistency_thread([&t]() { t.ConsistencyCheck(); }); */
  /* consistency_thread.join(); */

  /* for (auto& thread : threads) { */
  /*   thread.join(); */
  /* } */

  for (auto symbol : t.symbols()) {
    printf("%s = %d\n", symbol.c_str(), t.GetValue(symbol));
  }

  return 0;
}
