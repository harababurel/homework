#include <random>
#include <thread>
#include "tree.h"

/*
 * [a]--\
 *       \-----------[f]---------\
 *        \                       \
 *         ---[c]---\              ---[g]
 * [b]----/          \            /
 *                    ----[e]----/
 *                   /
 *                  /
 *            [d]---
 */

const int MAX_VAL = 1000;
const int THREADS = 20;

int main(int argc, char* argv[]) {
  pdp::Tree t;

  std::thread t1([&t]() { t.AddSymbol("a", 2); });
  std::thread t2([&t]() { t.AddSymbol("b", 5); });
  std::thread t3([&t]() { t.AddSymbol("d", 100); });
  t1.join();
  t2.join();
  t3.join();

  t.AddSymbol("c", {"a", "b"});

  std::thread t5([&t]() { t.AddSymbol("e", {"c", "d"}); });
  std::thread t6([&t]() { t.AddSymbol("f", {"a"}); });
  t5.join();
  t6.join();

  // Making t a DAG instead of a tree will cause a deadlock.
  /* t.AddSymbol("g", {"f", "e"}); */

  std::mt19937 gen(time(0));
  std::uniform_int_distribution<int> dis(0, MAX_VAL);
  std::vector<std::thread> threads;
  for (int i = 0; i < THREADS; i++) {
    int val = dis(gen);
    threads.emplace_back([&t, val, i]() {
      printf("Update %d: setting [a] to %d\n", i + 1, val);
      t.UpdateSymbol("a", val);
    });

    if (i % 5 == 0) {
      threads.emplace_back([&t]() {
        printf("Spawning consistency check thread.\n");
        t.ConsistencyCheck();
      });
    }
  }

  for (auto& thread : threads) {
    thread.join();
  }

  printf("Running final consistency check.\n");
  t.ConsistencyCheck();

  for (auto symbol : t.symbols()) {
    printf("%s = %d\n", symbol.c_str(), t.GetValue(symbol));
  }

  return 0;
}
