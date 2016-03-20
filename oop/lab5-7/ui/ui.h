#pragma once
#include <bits/stdc++.h>
#include "../controllers/controller.h"
using namespace std;

class UI {
private:
    Controller controller;


public:
    void run();
    Controller get_controller();
    void show_add_menu();
    void show_dogs();
};

