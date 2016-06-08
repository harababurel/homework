#pragma once
#include <iostream>
#include <sstream>


class Task {
public:
    std::string _description;
    std::string _status;
    int _id;

    Task(std::string description, std::string status, int id) {
        _description = description;
        _status = status;
        _id = id;
    }

    std::string toString() {
        std::ostringstream s;
        s<<_description<<" ("<<_status<<") - "<<_id<<"\n";
        return s.str();
    }

};

