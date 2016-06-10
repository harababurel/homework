#include <QApplication>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>
#include <QHBoxLayout>
#include <QFormLayout>

int buttonExample(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QPushButton button("&Push me!");
    button.setToolTip("Look! I'm a button!");
    button.setToolTipDuration(2000);

    QFont font("Ubuntu Mono", 50, 10, false);
    button.setFont(font);

    QIcon icon("Peacock.jpg");
    button.setIcon(icon);
    button.setIconSize(QSize(400, 400));

    button.show();
    return app.exec();
}

int listExample(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QListWidget list;
    new QListWidgetItem("Gone With the Wind", &list);
    new QListWidgetItem("The Three Musketeers", &list);

    QListWidgetItem *item3 = new QListWidgetItem("The Wizard of Oz");
    QFont font("Ubuntu Mono", 20, 10, false);
    item3->setFont(font);
    list.insertItem(0, item3);

    list.show();

    return app.exec();
}

int hBoxLayout(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QWidget wnd{};
    QHBoxLayout hLay{};
    QPushButton btn1{ "Button 1" };
    QPushButton btn2{ "Button 2" };
    QPushButton btn3{ "Button 3" };
    hLay.addWidget(&btn1);
    hLay.addWidget(&btn2);
    hLay.addWidget(&btn3);
    wnd.setLayout(&hLay);
    wnd.show();

    return app.exec();
}

int vBoxLayout(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QWidget *window = new QWidget{};
    QVBoxLayout *vLay = new QVBoxLayout{};
    QPushButton *btn1 = new QPushButton{ "Button 1" };
    QPushButton *btn2 = new QPushButton{ "Button 2" };
    QPushButton *btn3 = new QPushButton{ "Button 3" };
    vLay->addWidget(btn1);
    vLay->addWidget(btn2);
    vLay->addWidget(btn3);
    window->setLayout(vLay);
    window->show();

    return app.exec();
}

int formLayout(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QFormLayout *formLayout = new QFormLayout{};

    QLineEdit *nameTextBox = new QLineEdit{};
    QLabel *nameLabel = new QLabel{ "&Name:" };
    nameLabel->setBuddy(nameTextBox);

    QLineEdit *ageTextBox = new QLineEdit{};
    QLabel *ageLabel = new QLabel{ "&Age:" };
    ageLabel->setBuddy(ageTextBox);

    QLineEdit *emailTextBox = new QLineEdit{};
    QLabel *emailLabel = new QLabel{ "&Email:" };
    emailLabel->setBuddy(emailTextBox);

    formLayout->addRow(nameLabel, nameTextBox);
    formLayout->addRow(ageLabel, ageTextBox);
    formLayout->addRow(emailLabel, emailTextBox);

    QWidget *window = new QWidget{};
    window->setLayout(formLayout);
    window->show();

    return app.exec();
}

int gridLayout(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QGridLayout *gridLayout = new QGridLayout{};

    gridLayout->addWidget(new QPushButton("Add"), 0, 0);
    gridLayout->addWidget(new QPushButton("Delete"), 1, 0);
    gridLayout->addWidget(new QPushButton("Update"), 0, 1);
    gridLayout->addWidget(new QPushButton("Filter"), 1, 1);
    gridLayout->addWidget(new QPushButton("Sort"), 2, 0, 1, 2);

    QWidget *window = new QWidget{};
    window->setLayout(gridLayout);
    window->show();

    return app.exec();
}

int multipleLayouts(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QWidget *window = new QWidget{};
    QVBoxLayout *vL = new QVBoxLayout{};
    window->setLayout(vL);

    QWidget *details = new QWidget{};
    QFormLayout *fL = new QFormLayout{};

    details->setLayout(fL);

    //TODO: continue



    return app.exec();
}


int main(int argc, char *argv[]) {
    /*
    QApplication app(argc, argv);
    QLabel label("Hello world!");
    label.show();

    QLineEdit line("asdf");
    label.setBuddy(&line);

    return app.exec();
    */

    //return buttonExample(argc, argv);
    //return listExample(argc, argv);
    //return hBoxLayout(argc, argv);
    //return vBoxLayout(argc, argv);
    //return formLayout(argc, argv);
    return gridLayout(argc, argv);
    //return multipleLayouts(argc, argv);
}
