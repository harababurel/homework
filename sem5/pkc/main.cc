#include <iostream>
#include <qt/QtWidgets/QApplication>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"
#include "util/status.h"

int main(int argc, char *argv[]) {
  /* cipher::affine::AffineCipher cipher; */

  /* std::string message = "asdf"; */
  /* std::string code; */
  /* auto status = cipher.Encode(message, std::make_pair(2, 3), &code); */
  /* if (!status.ok()) { */
  /*   std::cout << status.error_message() << "\n"; */
  /* } else { */
  /*   std::cout << code << "\n"; */
  /* } */

  /* return 0; */

  QApplication app(argc, argv);
  CipherWidget widget;
  widget.show();

  return app.exec();
}
