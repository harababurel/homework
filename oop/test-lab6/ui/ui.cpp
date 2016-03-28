#include <iostream>
#include <bits/stdc++.h>
#include "../repos/repo.h"
#include "../models/club.h"
#include "../controllers/controller.h"
#include "ui.h"
using namespace std;

Controller &UI::get_controller() {
    return this->controller;
}

void UI::run() {
    string command;

    this->controller.populate_from_file("clubs.in");

    while(true) {
        cout<<"> ";
        getline(cin, command);

        if(command == "add")
            this->show_add_menu();
        else if(command == "remove")
            this->show_remove_menu();
        else if(command == "list")
            this->show_clubs();
        else if(command == "exit")
            exit(0);
        else
            cout<<"Invalid command.\n";
    }
}

void UI::show_add_menu() {
    cout<<"You want to add a club.\n";
}

void UI::show_remove_menu() {
    cout<<"You want to remove a club.\n";
}

void UI::show_clubs() {
    cout<<"You want to see all clubs.\n";

    for(auto x:this->get_controller().get_clubs())
        x.show();
}
