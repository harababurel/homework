#include <iostream>
#include <bits/stdc++.h>
#include "club.h"
using namespace std;


Club::Club(string name, string dance, int rating) {
    this->name = name;
    this->dance = dance;
    this->rating = rating;
}

string Club::get_name() const {
    return this->name;
}
string Club::get_dance() const {
    return this->dance;
}
int Club::get_rating() const {
    return this->rating;
}

void Club::set_name(string name) {
    this->name = name;
}

void Club::set_dance(string dance) {
    this->dance = dance;
}

void Club::set_rating(int rating) {
    this->rating = rating;
}

void Club::show() {
    cout<<"\t"<<this->name<<" ("<<this->dance<<"): "<<this->rating<<"/6\n";
}
