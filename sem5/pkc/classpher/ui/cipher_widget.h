#pragma once

#include <memory>
#include <qt/QtWidgets/QWidget>

#include "cipher/affine.h"
#include "cipher/caesar.h"
#include "cipher/cipher.h"
#include "cipher/hill.h"
#include "cipher/permutation.h"
#include "cipher/substitution.h"
#include "cipher/vigenere.h"
#include "ui/ui_cipher_widget.h"

namespace Ui {
class CipherWidget;
}

class CipherWidget : public QWidget {
  Q_OBJECT

 public:
  explicit CipherWidget(QWidget *parent = 0);

 private slots:
  void on_message_text_edit_textChanged();

  void on_key_spin_box_valueChanged(int x);

  void on_key_a_spin_box_valueChanged(int x);

  void on_key_b_spin_box_valueChanged(int x);

  void on_key_line_edit_textChanged(const QString &new_text);

  void on_code_text_edit_textChanged();

  void on_alphabet_line_edit_textEdited(const QString &new_text);

  void on_cipher_combo_box_currentTextChanged(const QString &new_text);

  void on_permutation_spin_box_valueChanged(int x);

  void on_permutation_keys_currentItemChanged(QListWidgetItem *x);

 private:
  std::unique_ptr<Ui::CipherWidget> ui_;
  std::map<std::string, std::unique_ptr<cipher::ICipher>> ciphers_;
  cipher::ICipher &CurrentCipher();
  std::string CurrentCipherName();
  void UpdateCodeText();
  void UpdateMessageText();
  void PopulatePermutationKeys(int x);
};
