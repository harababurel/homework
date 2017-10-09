#include <iostream>
#include <qt/QtWidgets/QApplication>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"

#include <Eigen/Dense>

int main(int argc, char *argv[]) {
  QApplication app(argc, argv);
  CipherWidget widget;
  widget.show();

  return app.exec();
}
