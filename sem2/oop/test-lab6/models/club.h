#pragma once
#include <iostream>
#include <bits/stdc++.h>
using namespace std;

class Club {
private:
    string name, dance;
    int rating;
public:
    Club(string name, string dance, int rating);
    string get_name() const;
    string get_dance() const;
    int get_rating() const;
    void set_name(string name);
    void set_dance(string dance);
    void set_rating(int rating);
    void show();
};


