#pragma once
#include <iostream>
#include <sstream>

class Student {
public:
    int _id;
    std::string _name;
    int _group;
    double _grade;
    std::string _teacher;
public:
    Student(int id, std::string name, int group, double grade, std::string teacher) {
        _id = id;
        _name = name;
        _group = group;
        _grade = grade;
        _teacher = teacher;
    }

    std::string toString() {
        std::ostringstream s;

        s<<_id<<". "<<_name<<" ("<<_group<<"), "<<_grade<<", "<<_teacher;
        return s.str();
    }

};
