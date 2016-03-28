#pragma once
#include <iostream>
#include <bits/stdc++.h>
#include "../models/club.h"
using namespace std;

class Repo {
private:
    vector <Club> v;
public:
    vector <Club> &get_clubs();
    int get_size();
    Club get_at_pos(int pos);
    int find_club(const Club T);
    bool has_club(const Club T);
    bool add_club(const Club T);
    bool remove_club(const Club T);
    void populate_from_file(string filename);
};

