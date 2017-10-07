#pragma once

#include <qt/QtCore/QObject>
#include <qt/QtWidgets/QMainWindow>

#include "ui/ui_main_window.h"

class MainWindow : public QMainWindow {
  Q_OBJECT

 public:
  explicit MainWindow(QWidget *parent = 0);

 private:
  Ui::MainWindow ui;  // Need for this line. Any one please help
};
