#pragma once
#include <QApplication>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QBoxLayout>
#include <QFormLayout>
#include <QWidgetItem>
#include <QObject>
#include <QLabel>
#include <QLineEdit>
#include <QSignalMapper>
#include "controllers/GradingController.h"
#include "models/Student.h"

class GUI: public QObject {
Q_OBJECT
private:
    QApplication *_app;
    GradingController *_controller;
    Student *tmpStudent;
    QLabel *idLabel;
    QLineEdit *idEdit;
    QLabel *nameLabel;
    QLineEdit *nameEdit;
    QLabel *groupLabel;
    QLineEdit *groupEdit;
    QLabel *teacherLabel;
    QLineEdit *teacherEdit;

public:
    GUI(int argc, char *argv[]) {
        _app = new QApplication(argc, argv);
        _controller = new GradingController;
    }
    ~GUI() {
        delete _app;
        delete _controller;
    }

    void run() {
        _controller->populateFromFile("students.in");

        QWidget *mainWindow = new QWidget;
        QGridLayout *layout = new QGridLayout;
        QListWidget *studentList = new QListWidget;


        sort(_controller->getStudents()->begin(), _controller->getStudents()->end(), [](Student *a, Student *b) {
                return a->_name < b->_name;
                });

        for(auto tmp:*_controller->getStudents()) {
            QListWidgetItem *item = new QListWidgetItem(studentList);
            item->setText(QString::fromStdString(tmp->toString()));
        }

        layout->addWidget(studentList, 0, 0);


        QPushButton *addStudentButton = new QPushButton;
        addStudentButton->setText(QString::fromStdString("&Add Student"));


        QPushButton *showTeacherWindowsButton = new QPushButton;
        showTeacherWindowsButton->setText("&Show Teachers");
        QObject::connect(showTeacherWindowsButton, SIGNAL(clicked()), this, SLOT(showTeacherWindows()));

        layout->addWidget(showTeacherWindowsButton, 2, 0);


        QObject::connect(addStudentButton, SIGNAL(clicked()), this, SLOT(showAddStudentWindow()));


        QPushButton *closeButton = new QPushButton;
        closeButton->setText(QString::fromStdString("Exit"));

        QObject::connect(closeButton, SIGNAL(clicked()), QApplication::instance(), SLOT(quit()));

        layout->addWidget(addStudentButton, 1, 0);
        layout->addWidget(closeButton, 3, 0);

        mainWindow->setLayout(layout);
        mainWindow->show();

        _app->exec();

        delete studentList;
        delete layout;
        delete mainWindow;
    }


public slots:
    void showTeacherWindows() {
        std::vector <QWidget *> teacherWidgets;

        for(auto t:*_controller->getTeachers()) {
            QWidget *tmpWindow = new QWidget;
            QGridLayout *tmpLayout = new QGridLayout;
            QListWidget *tmpList = new QListWidget;


            QLabel *tmpLabel = new QLabel();
            tmpLabel->setText(QString::fromStdString(t->_name));
            tmpLayout->addWidget(tmpLabel, 0, 0);

            for(auto student:*_controller->getStudents())
                if(t->_groups->find(student->_group) != t->_groups->end()) {
                    QListWidgetItem *tmpItem = new QListWidgetItem(tmpList);
                    tmpItem->setText(QString::fromStdString(student->toString()));
                }
            tmpLayout->addWidget(tmpList, 1, 0);
            tmpWindow->setLayout(tmpLayout);

            teacherWidgets.push_back(tmpWindow);
            tmpWindow->show();
        }
    }

    void showAddStudentWindow() {
        QWidget *window = new QWidget;
        QFormLayout *layout = new QFormLayout;

        idLabel = new QLabel;
        idEdit = new QLineEdit;
        nameLabel = new QLabel;
        nameEdit = new QLineEdit;
        groupLabel = new QLabel;
        groupEdit = new QLineEdit;
        teacherLabel = new QLabel;
        teacherEdit = new QLineEdit;



        idLabel->setText(QString::fromStdString("ID"));
        layout->addWidget(idLabel);

        layout->addWidget(idEdit);

        nameLabel->setText(QString::fromStdString("Name"));
        layout->addWidget(nameLabel);

        layout->addWidget(nameEdit);

        groupLabel->setText(QString::fromStdString("Group"));
        layout->addWidget(groupLabel);

        layout->addWidget(groupEdit);

        teacherLabel->setText(QString::fromStdString("Teacher"));
        layout->addWidget(teacherLabel);

        layout->addWidget(teacherEdit);

        QPushButton *addButton = new QPushButton;
        addButton->setText("Add");

        layout->addWidget(addButton);

        window->setLayout(layout);
        window->show();

        QObject::connect(addButton, SIGNAL(clicked()), this, SLOT(addStudent()));
        QObject::connect(addButton, SIGNAL(clicked()), window, SLOT(hide()));
    }

    void addStudent() {
        int id = idEdit->text().toInt();
        std::string name = nameEdit->text().toStdString();
        int group = groupEdit->text().toInt();
        std::string teacher = teacherEdit->text().toStdString();

        tmpStudent = new Student(id, name, group, 0.0, teacher);

        std::cout<<"pula\n";
        std::cout<<name<<" "<<teacher<<"\n";
        _controller->_repo->addStudent(tmpStudent);
    }
};
