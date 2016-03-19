#include <bits/stdc++.h>
#include "controller.h"
#include "../repos/repository.h"
using namespace std;

Repository Controller::get_repo() const {
    return this->repo;
}

void Controller::add_dog(Dog what) {
    this->repo.add_dog(what);
}

void Controller::remove_dog(Dog what) {
    this->repo.remove_dog(what);
}

