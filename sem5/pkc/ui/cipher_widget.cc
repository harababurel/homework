#include "cipher_widget.h"
#include <memory>
#include <qt/QtWidgets/QMessageBox>

CipherWidget::CipherWidget(QWidget* parent)
    : QWidget(parent), ui_(std::make_unique<Ui::CipherWidget>()) {
  ciphers_["Caesar"] = std::make_unique<cipher::CaesarCipher>();
  ciphers_["Hill"] = std::make_unique<cipher::HillCipher>();

  ui_->setupUi(this);
  ui_->key_line_edit->hide();
  ui_->alphabet_line_edit->setText(
      QString::fromStdString(CurrentCipher().alphabet()));
  UpdateCodeText();
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

  if (ui_->cipher_combo_box->currentText().toStdString() == "Caesar") {
    int key = int(ui_->key_spin_box->value());
    status = CurrentCipher().Decode(code, key, &message);
  } else {
    std::string key = ui_->key_line_edit->text().toStdString();
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

void CipherWidget::on_message_text_edit_textChanged() { UpdateCodeText(); }

void CipherWidget::on_key_spin_box_valueChanged(int x) { UpdateCodeText(); }

void CipherWidget::on_key_line_edit_textChanged(const QString& new_text) {
  if (new_text.isEmpty()) {
    QMessageBox::information(this, tr("Error"), tr("Key must not be empty."));
    ui_->key_line_edit->undo();
  } else {
    UpdateCodeText();
  }
}

void CipherWidget::on_code_text_edit_textChanged() { UpdateMessageText(); }

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
  UpdateCodeText();

  if (CurrentCipher().TypeName() == "int") {
    ui_->key_spin_box->show();
    ui_->key_line_edit->hide();
  } else {
    ui_->key_spin_box->hide();
    ui_->key_line_edit->show();
  }
}

cipher::ICipher& CipherWidget::CurrentCipher() {
  const std::string& cipher_name =
      ui_->cipher_combo_box->currentText().toStdString();

  return *(ciphers_[cipher_name]);
}
