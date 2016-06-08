#pragma once
#include <iostream>
#include <fstream>
#include <vector>
#include "Programmer.h"
#include "Task.h"
#include "TaskRepository.h"

class TaskController {
public:
    TaskRepository *repo;

    TaskController() {
        repo = new TaskRepository;
    }
    ~TaskController() {
        delete repo;
    }

    void addProgrammer(Programmer *t) {
        repo->addProgrammer(t);
    }

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

};


