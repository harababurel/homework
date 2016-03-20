#include <bits/stdc++.h>
#include <cstring>
#include "dog.h"

Dog::Dog(string breed, int age, string photograph) {
    this->breed = breed;
    this->age = age;
    this->photograph = photograph;
}

Dog::Dog() {
     this->breed = "";
     this->age = 0;
     this->photograph = "";
}

Dog::~Dog() {
    //cout<<"Destructor called for dog "<<this->breed<<", age "<<this->age<<".\n";
}


void Dog::set_breed(string what) {
    this->breed = what;
}

void Dog::set_age(int what) {
    this->age = what;
}

void Dog::set_photograph(string what) {
    this->photograph = what;
}

string Dog::get_breed() const {
    return this->breed;
}

int Dog::get_age() const {
    return this->age;
}

string Dog::get_photograph() const {
    return this->photograph;
}

string Dog::represent() const {
    ostringstream ret;
    ret << "\\  Breed: " << this->breed << "\n";
    ret << " > Age: " << this->age << "\n";
    ret << "/  Photograph: " << this->photograph << "\n";
    return ret.str();
}


