#include <iostream>
#include <qt/QtWidgets/QApplication>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/caesar_cipher_widget.h"
#include "ui/main_window.h"

#include <Eigen/Dense>

int main(int argc, char *argv[]) {
  cipher::HillCipher cipher("abcdefghijklmnopqrstuvwxyz");

  std::string message = "actcat";

  Eigen::Matrix3i key;
  key(0, 0) = 6;
  key(0, 1) = 24;
  key(0, 2) = 1;
  key(1, 0) = 13;
  key(1, 1) = 16;
  key(1, 2) = 10;
  key(2, 0) = 20;
  key(2, 1) = 17;
  key(2, 2) = 15;

  std::string code;

  auto status = cipher.Encode(message, key, &code);

  if (status.ok()) {
    printf("code: %s\n", code.c_str());
  } else {
    printf("error: %s\n", status.error_message().c_str());
  }

  /* QApplication app(argc, argv); */
  /* CaesarCipherWidget widget; */
  /* widget.show(); */

  /* return app.exec(); */
}
