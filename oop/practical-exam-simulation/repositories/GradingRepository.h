#pragma once
#include <bits/stdc++.h>
#include "../models/Student.h"
#include "../models/Teacher.h"

class GradingRepository {
private:
    std::vector <Student *> *_students;
    std::vector <Teacher *> *_teachers;

public:
    GradingRepository() {
        _students = new std::vector <Student *>;
        _teachers = new std::vector <Teacher *>;
    }
    ~GradingRepository() {
        delete _students;
        delete _teachers;
    }

    void addStudent(Student *tmp) {
        _students->push_back(tmp);

        Teacher *teacher = findTeacher(tmp->_teacher);
        if(teacher == nullptr) {
            std::set <int> *groups = new std::set <int>;
            groups->insert(tmp->_group);
            teacher = new Teacher(tmp->_teacher, groups);
            addTeacher(teacher);
            std::cout<<"added new teacher: "<<teacher->_name<<"\n";
        }
        else
            teacher->addGroup(tmp->_group);


    }

    void addTeacher(Teacher *tmp) {
        _teachers->push_back(tmp);
    }

    std::vector <Student *> *getStudents() {
        return _students;
    }

    std::vector <Teacher *> *getTeachers() {
        return _teachers;
    }

    Teacher *findTeacher(std::string name) {
        for(auto x:*_teachers)
            if(x->_name == name)
                return x;
        return nullptr;
    }

};
