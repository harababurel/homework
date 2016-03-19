#pragma once
#include <bits/stdc++.h>
#include "../models/dog.h"
using namespace std;

class Repository {
private:
    vector <Dog> dogs;

public:
    vector <Dog> get_dogs();
    void add_dog(Dog what);
    bool remove_dog(Dog what);

    /* Returns the position of `what` in the repository,
     * or -1 if dog does not exist.
     */
    int find_dog(Dog what);

    int get_population();
};

