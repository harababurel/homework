#pragma once
#include <iostream>
#include <fstream>
#include <vector>
#include "Programmer.h"
#include "Task.h"
#include "TaskRepository.h"
#include "StatusException.h"

class TaskController {
public:
    TaskRepository *repo;

    TaskController() {
        repo = new TaskRepository;
        testStartTask();
    }
    ~TaskController() {
        delete repo;
    }

    /* Method takes a pointer to a Programmer
     * and calls the repo method that adds
     * a programmer to the repository
     */
    void addProgrammer(Programmer *t) {
        repo->addProgrammer(t);
    }

    /* Method takes a pointer to a Task
     * and calls the repo method that adds
     * a task to the repository
     */
    void addTask(Task *t) {
        repo->addTask(t);
    }

    std::vector <Programmer *> *getProgrammers() {
        return repo->getProgrammers();
    }

    std::vector <Task *> *getTasks() {
        return repo->getTasks();
    }

    void populateFromFile() {
        std::ifstream f("programmers.in");

        int id;
        std::string name, description, status;


        while(f>>id>>name) {
            Programmer *t = new Programmer(id, name);
            addProgrammer(t);
            std::cout<<"Added programmer "<<name<<" from file.\n";
        }

        f.close();
        f.open("tasks.in");

        while(f>>description>>status) {
            if(status == "in_progress" || status == "closed")
                f>>id;

            Task *t = new Task(description, status, id);
            addTask(t);
            std::cout<<"Added task "<<description<<" from file.\n";
        }
        f.close();
    }

    void saveToFile() {
        std::ofstream g("programmers.in");

        for(auto t:*getProgrammers())
            g<<t->_id<<" "<<t->_name<<"\n";

        g.close();
        g.open("tasks.in");

        for(auto t:*getTasks()) {
            g<<t->_description<<" "<<t->_status;

            if(t->_status == "in_progress" || t->_status == "closed")
                g<<" "<<t->_id;

            g<<"\n";
        }
        g.close();
    }

    void removeTaskByDescription(std::string description) {
        repo->removeTaskByDescription(description);
    }

    /* Method changes the status of the selected task
     * to "in_progress" and then resets all windows.
     */
    void startTask(Task *t) {
        if(t->_status != "open")
            throw StatusException("Cannot start a task which is not open!");

        t->_status = "in_progress";
    }

    void closeTask(Task *t) {
        if(t->_status != "in_progress")
            throw StatusException("Cannot finish a task which is not in progress!");
        t->_status = "closed";
    }

    void testStartTask() {
        Task *t = new Task("asdf", "open", 1);
        startTask(t);

        bool caught = false;
        try {
            startTask(t); // is already started
        }
        catch(...) {
            caught = true;
        }

        assert(caught);
    }

};


