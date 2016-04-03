#include <bits/stdc++.h>
#include <climits>
#include "../controllers/controller.h"
#include "ui.h"
using namespace std;

Controller UI::get_controller() {
    return this->controller;
}

void UI::run() {
    string mode;

    this->controller.populate_from_file("dogs.in");

    while(true) {
        cout<<"[A]dministrator\n";
        cout<<"[U]ser\n";
        cout<<"\n";
        cout<<"Mode: ";

        getline(cin, mode);

        if(mode == "A" || mode == "a" ) {
            this->run_admin_mode();
            return;
        }
        else if(mode == "U" || mode == "u") {
            this->run_user_mode();
            return;
        }
    }
}

void UI::run_user_mode() {
    string command;

    while(true) {
        cout<<"user> ";
        getline(cin, command);

        if(command == "browse")
            this->show_browse();
        else if(command == "exit")
            exit(0);
        else
            cout<<"Invalid command.\n";
    }
}

void UI::run_admin_mode() {
    string command;

    while(true) {
        cout<<"admin> ";
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
    age = this->read_int();
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
    age = this->read_int();
    //cin>>age;
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
    age = this->read_int();
    //cin>>age;
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
    //cin>>new_age;
    new_age = this->read_int();
    cin.get();

    cout<<"New photograph: ";
    getline(cin, new_photograph);

    Dog new_dog = Dog(new_breed, new_age, new_photograph);
    int pos = this->controller.find_dog(what);

    this->controller.update_dog(pos, new_dog);
}

void UI::show_dogs() {
    cout<<"\n";
    for(auto i:this->controller.get_dogs())
        cout<<i.represent()<<"\n";
    return;
}

int UI::read_int() {
    int input = -1;
    bool valid = false;
    do {
        cin >> input;
        if (cin.good())
            valid = true;
        else {
            //something went wrong, reset the buffer's state to good
            cin.clear();
            //and empty it
            cin.ignore(numeric_limits<streamsize>::max(),'\n');
            cout<<"Not a number. Please re-enter: ";
        }
    } while (!valid);

    return input;
}

void UI::show_browse() {
    cout<<"You want to browse dogs.\n";
    string command;

    while(true) {
        this->show_current_dog();

        cout<<"next/adopt> ";
        getline(cin, command);

        if(command == "next") {
            cout<<"before next: "<<this->controller.get_user()->current_dog_id<<"\n";
            auto user = this->controller.get_user();
            user->current_dog_id = (user->current_dog_id + 1) % this->controller.get_dogs().size();
            cout<<"after next: "<<this->controller.get_user()->current_dog_id<<"\n";
        }
        else if(command == "adopt") {
            this->controller.get_user()->adoption_list.push_back(this->controller.get_dogs()[this->controller.get_user()->current_dog_id]);
            cout<<"Dog was adopted. :)\n";
            return;
        }
    }
}

void UI::show_current_dog() {
    cout<<this->controller.get_dogs()[this->controller.get_user()->current_dog_id].represent();
}

