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
#include "StatusException.h"

class GUI: public QObject {
Q_OBJECT
public:
    QApplication *app;
    TaskController *ctrl;
    QLabel *descriptionLabel;
    QPushButton *confirmButton;
    QLineEdit *descriptionLine;
    std::vector <QWidget *> *windows;
    int currentTaskRow;

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

        ctrl->saveToFile();
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

        QPushButton *removeButton = new QPushButton;
        removeButton->setText(QString::fromStdString("&Remove Task"));

        QPushButton *startButton = new QPushButton;
        startButton->setText(QString::fromStdString("&Start Task"));

        QPushButton *doneButton = new QPushButton;
        doneButton->setText(QString::fromStdString("&Finish Task"));

        QPushButton *exitButton = new QPushButton;
        exitButton->setText(QString::fromStdString("&Exit"));


        QObject::connect(addButton, SIGNAL(clicked()), this, SLOT(showAddTaskWindow()));
        QObject::connect(removeButton, SIGNAL(clicked()), this, SLOT(showRemoveTaskWindow()));
        QObject::connect(startButton, SIGNAL(clicked()), this, SLOT(startTask()));
        QObject::connect(doneButton, SIGNAL(clicked()), this, SLOT(finishTask()));
        QObject::connect(taskList, SIGNAL(currentRowChanged(int)), this, SLOT(setCurrentTaskRow(int)));
        QObject::connect(exitButton, SIGNAL(clicked()), QApplication::instance(), SLOT(quit()));


        layout->addWidget(programmerLabel, 0, 0);
        layout->addWidget(taskList, 0, 1);
        layout->addWidget(addButton, 1, 1);
        layout->addWidget(removeButton, 2, 1);
        layout->addWidget(startButton, 3, 1);
        layout->addWidget(doneButton, 4, 1);
        layout->addWidget(exitButton, 5, 1);
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

    void showRemoveTaskWindow() {
        QWidget *window = new QWidget;
        QFormLayout *layout = new QFormLayout;

        descriptionLabel = new QLabel;
        descriptionLabel->setText(QString::fromStdString("Description"));

        descriptionLine = new QLineEdit;
        confirmButton = new QPushButton;
        confirmButton->setText(QString::fromStdString("Remove"));

        layout->addWidget(descriptionLabel);
        layout->addWidget(descriptionLine);
        layout->addWidget(confirmButton);

        window->setLayout(layout);
        window->show();

        QObject::connect(confirmButton, SIGNAL(clicked()), this, SLOT(removeTask()));
        QObject::connect(confirmButton, SIGNAL(clicked()), window, SLOT(hide()));
    }



    void addTask() {
        std::string description = descriptionLine->text().toStdString();

        Task *t = new Task(description, "open", 0);
        ctrl->addTask(t);

        std::cout<<"task "<<description<<" added.\n";
        resetProgrammerWindows();
    }

    void removeTask() {
        std::string description = descriptionLine->text().toStdString();

        ctrl->removeTaskByDescription(description);
        resetProgrammerWindows();
    }

    void setCurrentTaskRow(int x) {
        std::cout<<"current row is "<<x<<"\n";
        currentTaskRow = x;
    }


    /* Method changes the status of the selected task
     * to "in_progress" and then resets all windows.
     */
    void startTask() {
        Task *t = (*ctrl->getTasks())[currentTaskRow];

        try {
            ctrl->startTask(t);
            resetProgrammerWindows();
        }
        catch(StatusException &e) {
            QWidget *errorWindow = new QWidget;
            QGridLayout *errorLayout = new QGridLayout(errorWindow);
            QLabel *errorLabel = new QLabel;

            errorLabel->setText(QString::fromStdString(e.msg()));


            QPushButton *okButton = new QPushButton;
            okButton->setText(QString::fromStdString("OK"));

            errorLayout->addWidget(errorLabel);
            errorLayout->addWidget(okButton);

            QObject::connect(okButton, SIGNAL(clicked()), errorWindow, SLOT(hide()));

            errorWindow->setLayout(errorLayout);
            errorWindow->show();

            std::cout<<"exception caught.\n";

        }

    }

    /* Method changes the status of the selected task
     * to "closed" and then resets all windows.
     */
    void finishTask() {
        Task *t = (*ctrl->getTasks())[currentTaskRow];

        try {
            ctrl->closeTask(t);
            resetProgrammerWindows();
        }
        catch(StatusException &e) {
            QWidget *errorWindow = new QWidget;
            QGridLayout *errorLayout = new QGridLayout(errorWindow);
            QLabel *errorLabel = new QLabel;

            errorLabel->setText(QString::fromStdString(e.msg()));


            QPushButton *okButton = new QPushButton;
            okButton->setText(QString::fromStdString("OK"));

            errorLayout->addWidget(errorLabel);
            errorLayout->addWidget(okButton);

            QObject::connect(okButton, SIGNAL(clicked()), errorWindow, SLOT(hide()));

            errorWindow->setLayout(errorLayout);
            errorWindow->show();

            std::cout<<"exception caught.\n";

        }
    }




};
