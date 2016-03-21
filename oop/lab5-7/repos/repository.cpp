#include <bits/stdc++.h>
#include "../models/dog.h"
#include "repository.h"
using namespace std;

vector <Dog> Repository::get_dogs() {
    return this->dogs;
}

bool Repository::add_dog(Dog what) {
    if(this->find_dog(what) != -1)
        return false;

    this->dogs = this->dogs + what;
    return true;
}

int Repository::find_dog(Dog what) {
    for(int i=0; i<(int) this->dogs.size(); i++)
        if(this->dogs[i] == what)
            return i;
    return -1;
}

bool Repository::remove_dog(Dog what) {
    int pos = this->find_dog(what);

    if(pos == -1)
        return false;

    // this->dogs[pos] = this->dogs[this->get_population()-1];
    // this->dogs.pop_back();
    this->dogs = this->dogs - what;

    return true;
}

void Repository::update_dog(int pos, Dog what) {
    Dog *original = &(this->dogs[pos]);

    original->set_breed(what.get_breed());
    original->set_age(what.get_age());
    original->set_photograph(what.get_photograph());
}


int Repository::get_population() {
    return (int)(this->dogs.size());
}

vector <Dog> operator+(vector <Dog> &v, const Dog &b) {
    v.push_back(b);
    return v;
}

vector <Dog> operator+(const Dog &b, vector <Dog> &v) {
    v.push_back(b);
    return v;
}

vector <Dog> operator-(vector <Dog> &v, const Dog &b) {
    /*
    for(int i=0; i<(int)v.size(); i++)
        if(v[i] == b) {
            v[i] = v[v.size()-1];
            v.pop_back();
            break;
        }
    */

    v.erase(remove(v.begin(), v.end(), b), v.end());
    return v;
}

void Repository::populate_from_file(string filename) {
    ifstream f(filename);
    cout<<"File opened :).\n";

    string breed;
    int age;
    string photograph;

    while(f>>breed>>age>>photograph)
        this->add_dog(Dog(breed, age, photograph));

    f.close();
}
