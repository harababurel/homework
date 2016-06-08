#pragma once
#include <iostream>

class Programmer {
public:
    int _id;
    std::string _name;

    Programmer(int id, std::string name) {
        _id = id;
        _name = name;
    }

};
