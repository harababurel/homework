#include <bits/stdc++.h>
#include "controller.h"
#include "../repos/repository.h"
#include "../models/user.h"
using namespace std;

Repository Controller::get_repo() const {
    return this->repo;
}

User* Controller::get_user() {
    return &(this->user);
}

bool Controller::add_dog(Dog what) {
    return this->repo.add_dog(what);
}

bool Controller::remove_dog(Dog what) {
    return this->repo.remove_dog(what);
}

bool Controller::dog_exists(Dog what) {
    return (this->repo.find_dog(what) != -1);
}

int Controller::find_dog(Dog what) {
    return this->repo.find_dog(what);
}

void Controller::update_dog(int pos, Dog what) {
    this->repo.update_dog(pos, what);
}

vector <Dog> Controller::get_dogs() {
    return this->repo.get_dogs();
}

void Controller::populate_from_file(string filename) {
    this->repo.populate_from_file(filename);
}
