#include <bits/stdc++.h>
#include "../models/dog.h"
#include "repository.h"
using namespace std;

vector <Dog> Repository::get_dogs() {
    return this->dogs;
}

void Repository::add_dog(Dog what) {
    this->dogs.push_back(what);
}

int Repository::find_dog(Dog what) {
    for(int i=0; i<(int) this->dogs.size(); i++)
        if(this->dogs[i] == what)
            return i;
    return -1;
}


bool Repository::remove_dog(Dog what) {
    return true;
}

int Repository::get_population() {
    return (int)(this->dogs.size());
}
