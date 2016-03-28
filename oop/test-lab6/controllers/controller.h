#pragma once
#include <iostream>
#include <bits/stdc++.h>
#include "../repos/repo.h"
#include "../models/club.h"
using namespace std;

class Controller {
private:
    Repo repo;
public:
    Repo& get_repo();
    vector <Club>& get_clubs();
    bool add_club(const Club T);
    void populate_from_file(string filename);
};
