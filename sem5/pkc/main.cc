#include <NTL/ZZ.h>
#include <iostream>
#include <qt/QtCore/QFile>
#include <qt/QtCore/QTextStream>
#include <qt/QtWidgets/QApplication>
#include <qt/QtWidgets/QProxyStyle>
#include <qt/QtWidgets/QStyleFactory>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"
#include "util/gcd.h"
#include "util/status.h"

int main(int argc, char *argv[]) {
  NTL::ZZ a = NTL::conv<NTL::ZZ>("37279462087332");
  NTL::ZZ b = NTL::conv<NTL::ZZ>("366983722766");

  std::cout << "GCD(" << a << ", " << b << ") = " << GCD::subtraction_gcd(a, b)
            << "\n";

  std::cout << "GCD(" << a << ", " << b << ") = " << GCD::division_gcd(a, b)
            << "\n";

  std::cout << "GCD(" << a << ", " << b
            << ") = " << GCD::factorization_gcd(a, b) << "\n";
  return 0;

  QApplication app(argc, argv);
  CipherWidget widget;

  QApplication::setStyle("Breeze");

  widget.show();
  return app.exec();
}
