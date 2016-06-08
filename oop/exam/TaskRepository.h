#pragma once
#include <iostream>
#include <cassert>
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
        test();
    }

    ~TaskRepository() {
        for(auto t:*programmers)
            delete t;
        for(auto t:*tasks)
            delete t;
    }

    /* Method takes a pointer to a Programmer
     * and adds it to the vector of programmers.
     */
    void addProgrammer(Programmer *t) {
        programmers->push_back(t);
    }

    /* Method takes a pointer to a Task.
     * and adds it to the vector of tasks.
     */
    void addTask(Task *t) {
        tasks->push_back(t);
    }

    void removeTaskByDescription(std::string description) {
        for(int i=0; i<int(tasks->size()); i++) {
            if((*tasks)[i]->_description == description) {
                delete (*tasks)[i];
                (*tasks)[i] = (*tasks)[tasks->size()-1];
                tasks->pop_back();

                i--;
            }
        }
    }

    std::vector <Programmer *> *getProgrammers() {
        return programmers;
    }

    std::vector <Task *> *getTasks() {
        return tasks;
    }


    void test() {
        assert(getProgrammers()->size() == 0);
        addProgrammer(new Programmer(1234, "asdf"));
        assert(getProgrammers()->size() == 1);

        addProgrammer(new Programmer(234, "assadasdf"));
        addProgrammer(new Programmer(134, "fgfdgasdf"));

        assert(getProgrammers()->size() == 3);

        assert(getTasks()->size() == 0);

        addTask(new Task("asdf", "open", 1));
        addTask(new Task("ghjk", "in_progress", 3));
        addTask(new Task("sdlkjfsd", "open", 1));

        assert(getTasks()->size() == 3);
        removeTaskByDescription("asdf");
        assert(getTasks()->size() == 2);

        getProgrammers()->clear();
        getTasks()->clear();

    }


};
