#include <iostream>
#include <bits/stdc++.h>
#include "../repos/repo.h"
#include "../models/club.h"
#include "../controllers/controller.h"
using namespace std;


class UI {
private:
    Controller controller;
public:
    Controller &get_controller();
    void run();
    void show_add_menu();
    void show_remove_menu();
    void show_clubs();
};
