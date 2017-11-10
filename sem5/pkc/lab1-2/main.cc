#include <NTL/ZZ.h>
#include <chrono>
#include <iostream>
#include <qt/QtCore/QFile>
#include <qt/QtCore/QTextStream>
#include <qt/QtWidgets/QApplication>
#include <qt/QtWidgets/QProxyStyle>
#include <qt/QtWidgets/QStyleFactory>
#include <random>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"
#include "util/gcd.h"
#include "util/status.h"

std::mt19937 gen(38);
std::uniform_int_distribution<int> dis(0, 9);

NTL::ZZ rand_ZZ(const int size) {
  std::string s;
  for (int i = 0; i < size; i++) {
    int digit = dis(gen);
    while (i == 0 && digit == 0) {
      digit = dis(gen);
    }

    s += '0' + digit;
  }
  return NTL::conv<NTL::ZZ>(s.c_str());
}

NTL::ZZ measure_time(const std::string& description,
                     std::function<NTL::ZZ()> f) {
  auto start_time = std::chrono::steady_clock::now();
  auto result = f();
  auto end_time = std::chrono::steady_clock::now();

  auto duration = end_time - start_time;

  printf("%s%.0f ms\n", description.c_str(),
         std::chrono::duration<double, std::milli>(duration).count());

  return result;
}

int main(int argc, char* argv[]) {
  /* NTL::ZZ a = rand_ZZ(size); */
  /* NTL::ZZ b = rand_ZZ(size); */

  NTL::ZZ a = NTL::conv<NTL::ZZ>("1234567891011121314151617181920");
  NTL::ZZ b = NTL::conv<NTL::ZZ>("9876543211011121314151617181920");

  NTL::ZZ sub_gcd = measure_time(
      "subtraction    ", [&a, &b]() { return GCD::subtraction_gcd(a, b); });
  NTL::ZZ div_gcd = measure_time(
      "division       ", [&a, &b]() { return GCD::division_gcd(a, b); });
  /* NTL::ZZ fac_gcd = measure_time( */
  /*     "factorization  ", [&a, &b]() { return GCD::factorization_gcd(a, b);
   * }); */

  std::cout << sub_gcd << "\n";
  std::cout << div_gcd << "\n";
  /* std::cout << fac_gcd << "\n"; */

  return 0;

  QApplication app(argc, argv);
  CipherWidget widget;

  QApplication::setStyle("Breeze");

  widget.show();
  return app.exec();
}
