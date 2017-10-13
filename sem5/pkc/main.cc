#include <iostream>
#include <qt/QtCore/QFile>
#include <qt/QtCore/QTextStream>
#include <qt/QtWidgets/QApplication>
#include <qt/QtWidgets/QProxyStyle>
#include <qt/QtWidgets/QStyleFactory>
#include "cipher/caesar.h"
#include "cipher/hill.h"
#include "ui/cipher_widget.h"
#include "util/status.h"

int main(int argc, char *argv[]) {
  QApplication app(argc, argv);
  CipherWidget widget;
  QApplication::setStyle("Breeze");

  /* QFile f("qdarkstyle/style.qss"); */
  /* if (!f.exists()) { */
  /*   printf("Unable to set stylesheet, file not found\n"); */
  /* } else { */
  /*   f.open(QFile::ReadOnly | QFile::Text); */
  /*   QTextStream ts(&f); */
  /*   app.setStyleSheet(ts.readAll()); */
  /* } */

  widget.show();
  return app.exec();
}
