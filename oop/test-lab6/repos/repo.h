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

    /* Returns the number of clubs in the repository. */
    int get_size();

    /* Returns a copy of the club situated at a given position. */
    Club get_at_pos(int pos);

    /* Returns the position of a club if it exists, -1 otherwise. */
    int find_club(const Club T);

    /* Returns true if a club exists, false otherwise. */
    bool has_club(const Club T);

    /* Tries to add a club to the repository. If possible, returns true. */
    bool add_club(const Club T);

    /* Tries to remove a club from the repository. If possible, returns true. */
    bool remove_club(const Club T);

    /* Adds some existing clubs from a given filename. */
    void populate_from_file(string filename);
};

