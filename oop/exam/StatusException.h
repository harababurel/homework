#pragma once
#include <iostream>

class StatusException {
public:
    std::string _message;

    StatusException(std::string message) {
        _message = message;
    }

    std::string msg() {
        return _message;
    }
};
