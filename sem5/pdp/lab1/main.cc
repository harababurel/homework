#include <random>
#include <thread>
#include "tree.h"

const int MAX_VAL = 100000;
const int THREADS = 10000;

int main(int argc, char* argv[]) {
  pdp::Tree t;

  std::thread t1([&t]() { t.AddSymbol("a", 2); });
  std::thread t2([&t]() { t.AddSymbol("b", 5); });
  t1.join();
  t2.join();

  std::thread t3([&t]() { t.AddSymbol("c", {"a", "b"}); });
  std::thread t4([&t]() { t.AddSymbol("d", 100); });
  t3.join();
  t4.join();

  std::thread t5([&t]() { t.AddSymbol("e", {"c", "d"}); });
  t5.join();

  std::thread t6([&t]() { t.UpdateSymbol("a", 100); });
  std::thread t7([&t]() { t.UpdateSymbol("a", 0); });
  t6.join();
  t7.join();

  std::mt19937 gen(42);
  std::uniform_int_distribution<> dis(0, MAX_VAL);
  std::vector<std::thread> threads;
  for (int i = 0; i < THREADS; i++) {
    threads.emplace_back([&]() { t.UpdateSymbol("a", dis(gen)); });
  }

  for (auto& thread : threads) {
    thread.join();
  }

  for (auto symbol : t.symbols()) {
    printf("%s = %d\n", symbol.c_str(), t.GetValue(symbol));
  }

  return 0;
}
