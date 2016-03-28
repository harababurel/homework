#include <iostream>
#include <bits/stdc++.h>
#include "../models/club.h"
#include "repo.h"
using namespace std;

vector <Club> &Repo::get_clubs() {
    return this->v;
}

int Repo::get_size() {
    return this->get_clubs().size();
}

Club Repo::get_at_pos(int pos) {
    return this->get_clubs()[pos];
}

int Repo::find_club(const Club T) {
    for(int i=0; i<this->get_size(); i++)
        if(this->get_at_pos(i).get_name() == T.get_name())
            return i;

    return -1;
}

bool Repo::has_club(const Club T) {
    return (this->find_club(T) != -1);
}

bool Repo::add_club(const Club T) {
    if(this->has_club(T))
        return false;

    this->get_clubs().push_back(T);
    return true;
}

bool Repo::remove_club(const Club T) {
    if(!this->has_club(T))
        return false;

    int i = this->find_club(T);
    this->v[i] = this->v[this->v.size()-1];
    this->v.pop_back();

    return true;
}

void Repo::populate_from_file(string filename) {
    ifstream f(filename);
    cout<<"File opened :).\n";

    string name, dance;
    int rating;

    while(f>>name>>dance>>rating)
        this->add_club(Club(name, dance, rating));

    f.close();
}
