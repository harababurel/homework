#include "cipher_widget.h"
#include <iostream>
#include <memory>
#include <qt/QtWidgets/QLCDNumber>
#include <qt/QtWidgets/QListWidget>
#include <qt/QtWidgets/QMessageBox>

CipherWidget::CipherWidget(QWidget* parent)
    : QWidget(parent), ui_(std::make_unique<Ui::CipherWidget>()) {
  ciphers_["Affine"] = std::make_unique<cipher::affine::AffineCipher>();
  ciphers_["Caesar"] = std::make_unique<cipher::caesar::CaesarCipher>();
  ciphers_["Hill"] = std::make_unique<cipher::hill::HillCipher>();
  ciphers_["Permutation"] =
      std::make_unique<cipher::permutation::PermutationCipher>();
  ciphers_["Substitution"] =
      std::make_unique<cipher::substitution::SubstitutionCipher>();
  ciphers_["Vigenère"] = std::make_unique<cipher::vigenere::VigenereCipher>();
  ciphers_["RSA"] = std::make_unique<cipher::rsa::RSACipher>();

  ui_->setupUi(this);

  for (const auto& entry : ciphers_) {
    ui_->cipher_combo_box->addItem(QString::fromStdString(entry.first));
  }

  ui_->key_line_edit->hide();
  ui_->key_spin_box->hide();
  ui_->permutation_container->hide();
  ui_->alphabet_line_edit->setText(
      QString::fromStdString(CurrentCipher().alphabet()));

  auto spin = ui_->permutation_container->findChild<QSpinBox*>(
      QString("permutation_spin_box"));
  PopulatePermutationKeys(spin->value());

  UpdateCodeText();
}

void CipherWidget::on_permutation_spin_box_valueChanged(int x) {
  PopulatePermutationKeys(x);
  UpdateCodeText();
}

void CipherWidget::on_permutation_keys_currentItemChanged(QListWidgetItem* x) {
  UpdateCodeText();
}

void CipherWidget::PopulatePermutationKeys(int x) {
  auto keys = ui_->permutation_container->findChild<QListWidget*>(
      QString("permutation_keys"));

  if (keys->count() < x) {
    for (int i = keys->count(); i < x; i++) {
      keys->addItem(QString::number(i));
    }
  } else {
    for (int i = 0; i < keys->count(); i++) {
      auto item = keys->item(i);
      if (item->text().toInt() >= x) {
        delete item;
        --i;
      }
    }
  }

  keys->viewport()->setAcceptDrops(true);
  keys->setDropIndicatorShown(true);
  keys->setSizeAdjustPolicy(QListWidget::AdjustToContentsOnFirstShow);
  keys->setDragDropMode(QAbstractItemView::InternalMove);
}
void CipherWidget::UpdateCodeText() {
  const auto& message = ui_->message_text_edit->toPlainText().toStdString();

  std::string code;
  util::Status status(util::error::UNKNOWN, "Unknown key type.");

  if (CurrentCipher().TypeName() == "int") {
    auto key = int(ui_->key_spin_box->value());
    status = CurrentCipher().Encode(message, key, &code);
  } else if (CurrentCipher().TypeName().find("string") != std::string::npos) {
    auto key = ui_->key_line_edit->text().toStdString();
    status = CurrentCipher().Encode(message, key, &code);
  } else if (CurrentCipher().TypeName() == "std::pair<int, int>") {
    int a =
        int(ui_->affine_widget->findChild<QSpinBox*>(QString("key_a_spin_box"))
                ->value());
    int b =
        int(ui_->affine_widget->findChild<QSpinBox*>(QString("key_b_spin_box"))
                ->value());
    auto key = std::make_pair(a, b);
    status = CurrentCipher().Encode(message, key, &code);
  } else if (CurrentCipher().TypeName() ==
             "std::vector<int, std::allocator<int> >") {
    std::vector<int> key;

    auto key_widget = ui_->permutation_container->findChild<QListWidget*>(
        QString("permutation_keys"));

    for (int i = 0; i < key_widget->count(); i++) {
      key.push_back(key_widget->item(i)->text().toInt());
    }
    status = CurrentCipher().Encode(message, key, &code);
  }

  if (!status.ok()) {
    code = status.error_message();
  }

  {
    const QSignalBlocker blocker(ui_->code_text_edit);
    ui_->code_text_edit->setPlainText(QString::fromStdString(code));
  }
}

void CipherWidget::UpdateMessageText() {
  const auto& code = ui_->code_text_edit->toPlainText().toStdString();

  std::string message;
  util::Status status;

  if (CurrentCipher().TypeName() == "int") {
    auto key = int(ui_->key_spin_box->value());
    status = CurrentCipher().Decode(code, key, &message);
  } else if (CurrentCipher().TypeName().find("string") != std::string::npos) {
    auto key = ui_->key_line_edit->text().toStdString();
    status = CurrentCipher().Decode(code, key, &message);
  } else if (CurrentCipher().TypeName() == "std::pair<int, int>") {
    int a =
        int(ui_->affine_widget->findChild<QSpinBox*>(QString("key_a_spin_box"))
                ->value());
    int b =
        int(ui_->affine_widget->findChild<QSpinBox*>(QString("key_b_spin_box"))
                ->value());

    auto key = std::make_pair(a, b);
    status = CurrentCipher().Decode(code, key, &message);
  } else if (CurrentCipher().TypeName() ==
             "std::vector<int, std::allocator<int> >") {
    std::vector<int> key;

    auto key_widget = ui_->permutation_container->findChild<QListWidget*>(
        QString("permutation_keys"));

    for (int i = 0; i < key_widget->count(); i++) {
      key.push_back(key_widget->item(i)->text().toInt());
    }
    status = CurrentCipher().Decode(code, key, &message);
  }

  if (!status.ok()) {
    message = status.error_message();
  }

  {
    const QSignalBlocker blocker(ui_->message_text_edit);
    ui_->message_text_edit->setPlainText(QString::fromStdString(message));
  }
}

void CipherWidget::on_code_text_edit_textChanged() { UpdateMessageText(); }

void CipherWidget::on_message_text_edit_textChanged() { UpdateCodeText(); }

void CipherWidget::on_key_spin_box_valueChanged(int x) { UpdateCodeText(); }

void CipherWidget::on_key_a_spin_box_valueChanged(int x) { UpdateCodeText(); }

void CipherWidget::on_key_b_spin_box_valueChanged(int x) { UpdateCodeText(); }

void CipherWidget::on_key_line_edit_textChanged(const QString& new_text) {
  if (new_text.isEmpty()) {
    QMessageBox::information(this, tr("Error"), tr("Key must not be empty."));
    ui_->key_line_edit->undo();
  } else {
    UpdateCodeText();
  }
}

void CipherWidget::on_alphabet_line_edit_textEdited(const QString& new_text) {
  auto status = CurrentCipher().SetAlphabet(new_text.toStdString());

  if (!status.ok()) {
    QMessageBox::information(this, tr("Error"),
                             tr(status.error_message().c_str()));
    {
      const QSignalBlocker blocker(ui_->alphabet_line_edit);
      ui_->alphabet_line_edit->setText(
          QString::fromStdString(CurrentCipher().alphabet()));
    }
  } else {
    UpdateCodeText();
  }
}

void CipherWidget::on_cipher_combo_box_currentTextChanged(
    const QString& new_text) {
  /* QMessageBox::information(this, tr("Info"), */
  /*                          tr(CurrentCipher().TypeName().c_str())); */
  UpdateCodeText();

  if (CurrentCipher().TypeName() == "int") {
    ui_->key_spin_box->show();
    ui_->key_line_edit->hide();
    ui_->affine_widget->hide();
    ui_->permutation_container->hide();
  } else if (CurrentCipher().TypeName() == "std::pair<int, int>") {
    ui_->key_spin_box->hide();
    ui_->key_line_edit->hide();
    ui_->affine_widget->show();
    ui_->permutation_container->hide();
  } else if (CurrentCipher().TypeName() ==
             "std::vector<int, std::allocator<int> >") {
    ui_->key_spin_box->hide();
    ui_->key_line_edit->hide();
    ui_->affine_widget->hide();
    ui_->permutation_container->show();
  } else {
    ui_->key_spin_box->hide();
    ui_->key_line_edit->show();
    ui_->affine_widget->hide();
    ui_->permutation_container->hide();
  }

  if (CurrentCipherName() == "Substitution") {
    std::string key = " abcdefghijklmnopqrstuvwxyz";
    std::random_shuffle(key.begin(), key.end());
    ui_->key_line_edit->setText(QString::fromStdString(key));
  }
}

cipher::ICipher& CipherWidget::CurrentCipher() {
  return *(ciphers_[CurrentCipherName()]);
}

std::string CipherWidget::CurrentCipherName() {
  return ui_->cipher_combo_box->currentText().toStdString();
}
