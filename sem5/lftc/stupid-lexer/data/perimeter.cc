#include <iostream>

int main() {
  float radius;
  std::cin >> radius;

  float pi;
  pi = 3.164159;

  float perim;
  perim = 2 * pi * radius;
  std::cout << perim << std::endl;

  float area;
  area = pi * radius * radius;
  std::cout << area << std::endl;
}
