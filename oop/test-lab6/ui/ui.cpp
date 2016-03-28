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

        if(command == "add") {
            this->show_add_menu();
            continue;
        }
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

    string name, dance;
    int rating;

    cout<<"Name: ";
    getline(cin, name);

    cout<<"Dance: ";
    getline(cin, dance);

    cout<<"Rating: ";
    cin>>rating;
    cin.get();

    if(this->get_controller().add_club(Club(name, dance, rating)))
        cout<<"Club successfully added. :)\n";
    else
        cout<<"Club already exists. :(\n";
}

void UI::show_remove_menu() {
    cout<<"You want to remove a club.\n";

    string name;
    cout<<"Name: ";
    getline(cin, name);

    if(this->get_controller().remove_club(Club(name, "", 0)))
        cout<<"Club successfully removed. :)\n";
    else
        cout<<"Club doesn't exist. :(\n";
}

void UI::show_clubs() {
    cout<<"You want to see all clubs.\n";

    for(auto x:this->get_controller().get_clubs())
        x.show();
}
