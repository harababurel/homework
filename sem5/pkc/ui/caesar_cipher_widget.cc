#include "caesar_cipher_widget.h"
#include <memory>
#include <qt/QtWidgets/QMessageBox>

CaesarCipherWidget::CaesarCipherWidget(QWidget* parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::CaesarCipherWidget>()),
      cipher_(std::make_unique<cipher::CaesarCipher>()) {
  ui_->setupUi(this);
  ui_->alphabet_line_edit->setText(QString::fromStdString(cipher_->alphabet()));
}

void CaesarCipherWidget::on_message_text_edit_textChanged() {
  const auto& message = ui_->message_text_edit->toPlainText().toStdString();
  int key = int(ui_->key_spin_box->value());

  std::string code;

  auto status = cipher_->Encode(message, key, &code);

  if (!status.ok()) {
    code = status.error_message();
  }

  {
    const QSignalBlocker blocker(ui_->code_text_edit);
    ui_->code_text_edit->setPlainText(QString::fromStdString(code));
  }
}

void CaesarCipherWidget::on_key_spin_box_valueChanged(int x) {
  on_message_text_edit_textChanged();
}

void CaesarCipherWidget::on_code_text_edit_textChanged() {
  const auto& code = ui_->code_text_edit->toPlainText().toStdString();
  int key = int(ui_->key_spin_box->value());

  std::string message;

  auto status = cipher_->Decode(code, key, &message);

  if (!status.ok()) {
    message = status.error_message();
  }

  {
    const QSignalBlocker blocker(ui_->message_text_edit);
    ui_->message_text_edit->setPlainText(QString::fromStdString(message));
  }
}

void CaesarCipherWidget::on_alphabet_line_edit_textEdited(
    const QString& new_text) {
  auto status = cipher_->SetAlphabet(new_text.toStdString());

  if (!status.ok()) {
    QMessageBox::information(this, tr("Error"),
                             tr(status.error_message().c_str()));
    {
      const QSignalBlocker blocker(ui_->alphabet_line_edit);
      ui_->alphabet_line_edit->setText(
          QString::fromStdString(cipher_->alphabet()));
    }
  } else {
    on_message_text_edit_textChanged();
  }
}
