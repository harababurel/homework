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
#include "cipher/rsa.h"
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
  cipher::rsa::RSACipher cipher;

  std::string code;
  cipher.Encode("the quick brown fox jumps over the lazy dog",
                cipher.public_key_, &code);
  std::cout << "Code is \"" << code << "\"\n";

  std::string message;
  cipher.Decode(code, cipher.private_key_, &message);
  std::cout << "Original message was \"" << message << "\"\n";

  return 0;

  QApplication app(argc, argv);
  CipherWidget widget;

  QApplication::setStyle("Breeze");

  widget.show();
  return app.exec();
}
