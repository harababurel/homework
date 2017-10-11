#include <iostream>
#include <qt/QtWidgets/QApplication>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"

#include <NTL/ZZ.h>
#include <Eigen/Dense>

int main(int argc, char *argv[]) {
  NTL::ZZ x, a(2), n(27);
  x = NTL::InvMod(a, n);  // functional form

  std::stringstream buffer;
  buffer << x;

  printf("%s\n", buffer.str().c_str());

  return 0;

  QApplication app(argc, argv);
  CipherWidget widget;
  widget.show();

  return app.exec();
}
