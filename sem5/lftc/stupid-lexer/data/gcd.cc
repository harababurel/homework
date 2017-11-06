#include <iostream>

int main() {
  int a;
  int b;

  a = 121;
  b = 33;

  while (a != b) {
    if (a > b) {
      a = a - b;
    } else {
      b = b - a;
    }
  }

  std::cout << a;
  return 0;
}
