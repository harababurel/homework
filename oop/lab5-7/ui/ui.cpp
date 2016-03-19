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
        cin>>command;

        cout<<"command is "<<command<<"\n";
    }
}
