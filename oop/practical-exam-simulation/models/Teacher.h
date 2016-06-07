#pragma once
#include <iostream>
#include <sstream>
#include <vector>
#include <set>

class Teacher {
public:
    std::string _name;
    std::set <int> *_groups;

public:
    Teacher(std::string name, std::set <int> *groups) {
        _name = name;
        _groups = groups;
    }

    ~Teacher() {
        _groups->clear();
        delete _groups;
    }

    void addGroup(int group) {
        _groups->insert(group);
    }
};
