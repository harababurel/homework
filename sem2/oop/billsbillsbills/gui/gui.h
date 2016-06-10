#pragma once
#include "../controllers/controller.h"

class GUI {
private:
    Controller controller;

public:
    int run(int argc, char *argv[]);
};
