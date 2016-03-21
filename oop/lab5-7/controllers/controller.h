#pragma once
#include <bits/stdc++.h>
#include "../repos/repository.h"
using namespace std;

class Controller {
private:
    Repository repo;

public:
    Repository get_repo() const;
    bool add_dog(Dog what);
    bool dog_exists(Dog what);
    bool remove_dog(Dog what);
    int find_dog(Dog what);
    void update_dog(int pos, Dog what);
    vector <Dog> get_dogs();
    void populate_from_file(string filename);
};

