#pragma once
#include "../repositories/GradingRepository.h"
#include "../models/Student.h"
#include <fstream>

class GradingController {
public:
    GradingRepository *_repo;
public:
    GradingController() {
        _repo = new GradingRepository;
    }
    ~GradingController() {
        delete _repo;
    }

    void populateFromFile(std::string filename) {
        std::ifstream f(filename);

        int id, group;
        double grade;
        std::string name, teacher;

        while(f>>id>>name>>group>>grade>>teacher) {
            Student *tmp = new Student(id, name, group, grade, teacher);
            std::cout<<"read a new student: "<<name<<"\n";

            Teacher *t = findTeacher(teacher);
            if(t == nullptr) {
                std::set <int> *groups = new std::set <int>;
                groups->insert(group);
                t = new Teacher(teacher, groups);

                _repo->addTeacher(t);
            }
            else
                t->addGroup(group);

            _repo->addStudent(tmp);
        }
    }

    std::vector <Student *> *getStudents() {
        return _repo->getStudents();
    }

    std::vector <Teacher *> *getTeachers() {
        return _repo->getTeachers();
    }

    Teacher *findTeacher(std::string name) {
        return _repo->findTeacher(name);
    }
};
