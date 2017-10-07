#pragma once

#include <memory>
#include <qt/QtWidgets/QWidget>

#include "cipher/caesar.h"
#include "ui/ui_caesar_cipher_widget.h"

namespace Ui {
class CaesarCipherWidget;
}

class CaesarCipherWidget : public QWidget {
  Q_OBJECT

 public:
  explicit CaesarCipherWidget(QWidget *parent = 0);

 private slots:
  void on_message_text_edit_textChanged();

  void on_key_spin_box_valueChanged(int x);

  void on_code_text_edit_textChanged();

  void on_alphabet_line_edit_textEdited(const QString &new_text);

 private:
  std::unique_ptr<Ui::CaesarCipherWidget> ui_;
  std::unique_ptr<cipher::CaesarCipher> cipher_;
};
