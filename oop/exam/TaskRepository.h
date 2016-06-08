#pragma once
#include <iostream>
#include <vector>
#include "Programmer.h"
#include "Task.h"

class TaskRepository {
public:
    std::vector <Programmer *> *programmers;
    std::vector <Task *> *tasks;

    TaskRepository() {
        programmers = new std::vector <Programmer *>;
        tasks = new std::vector <Task *>;
    }

    ~TaskRepository() {
        for(auto t:*programmers)
            delete t;
        for(auto t:*tasks)
            delete t;
    }

    void addProgrammer(Programmer *t) {
        programmers->push_back(t);
    }

    void addTask(Task *t) {
        tasks->push_back(t);
    }

    std::vector <Programmer *> *getProgrammers() {
        return programmers;
    }

    std::vector <Task *> *getTasks() {
        return tasks;
    }

};
