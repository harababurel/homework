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
    void show_help();

    /* Method lists the clubs with a certain dance type,
     * in ascending order (by rating).
     * Since the repository always contains ordered clubs,
     * they can be iterated in their default order, and only
     * those that match the given dance type are shown.
     */
    void show_filter_menu();
};
