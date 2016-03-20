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

        if(command == "add")
            this->show_add_menu();
        else if(command == "remove")
            this->show_remove_menu();
        else if(command == "update")
            this->show_update_menu();
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

void UI::show_remove_menu() {
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
    if(this->controller.remove_dog(what))
        cout<<"Dog successfully removed.\n";
    else
        cout<<"Dog not found.\n";
}

void UI::show_update_menu() {
    cout<<"You want to update a dog.\n\n";

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

    if(!this->controller.dog_exists(what)) {
        cout<<"Dog doesn't exist.\n";
        return;
    }

    string new_breed, new_photograph;
    int new_age;

    cout<<"New breed: ";
    getline(cin, new_breed);

    cout<<"New age: ";
    cin>>new_age;
    cin.get();

    cout<<"New photograph: ";
    getline(cin, new_photograph);

    Dog new_dog = Dog(new_breed, new_age, new_photograph);
    int pos = this->controller.find_dog(what);

    this->controller.update_dog(pos, new_dog);
}

void UI::show_dogs() {
    for(auto i:this->controller.get_dogs())
        cout<<i.represent()<<"\n";
    return;
}

