#include <bits/stdc++.h>
#include "../controllers/controller.h"
#include "ui.h"
using namespace std;

Controller UI::get_controller() {
    return this->controller;
}

void UI::run() {
    string command;

    while(true) {
        cout<<"> ";
        getline(cin, command);

        cout<<"command is "<<command<<"\n";

        if(command == "add")
            this->show_add_menu();
        else if(command == "list")
            this->show_dogs();
        else if(command == "exit")
            exit(0);
        else
            cout<<"Invalid command.\n";

    }
}

void UI::show_add_menu() {
    cout<<"You want to add a dog.\n\n";

    string breed, photograph;
    int age;

    cout<<"Breed: ";
    getline(cin, breed);

    cout<<"Age: ";
    cin>>age;
    cin.get();

    cout<<"Photograph: ";
    getline(cin, photograph);

    Dog what = Dog(breed, age, photograph);
    this->controller.add_dog(what);
}

void UI::show_dogs() {

    for(auto i:this->controller.get_dogs())
        cout<<i.represent()<<"\n";
    return;
}
