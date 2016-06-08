#include <bits/stdc++.h>
#include <QApplication>
#include <QObject>
#include <QWidget>
#include <QGridLayout>
#include <QFormLayout>
#include <QLabel>
#include <QListView>
#include <QLineEdit>
#include <QListWidgetItem>
#include <QPushButton>
#include "TaskController.h"

class GUI: public QObject {
Q_OBJECT
public:
    QApplication *app;
    TaskController *ctrl;
    QLabel *descriptionLabel;
    QPushButton *confirmButton;
    QLineEdit *descriptionLine;
    std::vector <QWidget *> *windows;

    GUI(int argc, char *argv[]) {
        app = new QApplication(argc, argv);
        ctrl = new TaskController();
    }
    ~GUI() {
        delete ctrl;
        delete app;
    }

    void run() {
        ctrl->populateFromFile();

        windows = new std::vector <QWidget *>;

        std::cout<<"Launching GUI.\n";

        showProgrammerWindows();
        app->exec();
    }

    void showProgrammerWindows() {

        for(auto t:*ctrl->getProgrammers())
            windows->push_back(createProgrammerWindow(t));


        for(auto window:*windows)
            window->show();
    }

    void resetProgrammerWindows() {

        for(auto window:*windows)
            deleteWindow(window);
        windows->clear();

        showProgrammerWindows();

    }

    QWidget *createProgrammerWindow(Programmer *t) {

        QWidget *window = new QWidget;
        QGridLayout *layout = new QGridLayout(window);

        QLabel *programmerLabel = new QLabel(window);
        programmerLabel->setText(QString::fromStdString(t->_name));

        QListWidget *taskList = new QListWidget;

        sort(ctrl->getTasks()->begin(), ctrl->getTasks()->end(),
                [](Task *a, Task *b) {
                    return a->_status < b->_status;
                    });

        for(auto task:*ctrl->getTasks()) {
            QListWidgetItem *entry = new QListWidgetItem();
            entry->setText(QString::fromStdString(task->toString()));

            taskList->addItem(entry);
        }

        QPushButton *addButton = new QPushButton;
        addButton->setText(QString::fromStdString("&Add Task"));

        QObject::connect(addButton, SIGNAL(clicked()), this, SLOT(showAddTaskWindow()));


        layout->addWidget(programmerLabel, 0, 0);
        layout->addWidget(taskList, 0, 1);
        layout->addWidget(addButton, 0, 2);
        window->setLayout(layout);

        return window;
    }

    void deleteWindow(QWidget *window) {
        window->hide();
        delete window;
    }


public slots:
    void showAddTaskWindow() {
        QWidget *window = new QWidget;
        QFormLayout *layout = new QFormLayout;

        descriptionLabel = new QLabel;
        descriptionLabel->setText(QString::fromStdString("Description"));

        descriptionLine = new QLineEdit;

        confirmButton = new QPushButton;
        confirmButton->setText(QString::fromStdString("Add"));

        layout->addWidget(descriptionLabel);
        layout->addWidget(descriptionLine);
        layout->addWidget(confirmButton);

        window->setLayout(layout);
        window->show();

        QObject::connect(confirmButton, SIGNAL(clicked()), this, SLOT(addTask()));
        QObject::connect(confirmButton, SIGNAL(clicked()), window, SLOT(hide()));

    }

    void addTask() {
        std::string description = descriptionLine->text().toStdString();

        Task *t = new Task(description, "open", 0);
        ctrl->addTask(t);

        std::cout<<"task "<<description<<" added.\n";
        resetProgrammerWindows();
    }



};
