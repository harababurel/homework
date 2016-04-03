#pragma once
#include <bits/stdc++.h>
#include "../controllers/controller.h"
using namespace std;

class UI {
private:
    Controller controller;

public:
    void run();
    void run_admin_mode();
    void run_user_mode();
    Controller get_controller();
    void show_add_menu();
    void show_remove_menu();
    void show_update_menu();
    void show_dogs();
    void show_browse();
    void show_current_dog();

    // Validates user input for reading ints
    int read_int();
};

