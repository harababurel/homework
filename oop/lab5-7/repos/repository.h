#pragma once
#include <bits/stdc++.h>
#include <fstream>
#include "../models/dog.h"
using namespace std;

class Repository {
private:
    vector <Dog> dogs;

public:
    vector <Dog> get_dogs();
    bool add_dog(Dog what);
    bool remove_dog(Dog what);

    /* Returns the position of `what` in the repository,
     * or -1 if dog does not exist.
     */
    int find_dog(Dog what);

    void update_dog(int pos, Dog what);
    void populate_from_file(string filename);
    int get_population();
};

/* Operator overloading for the vector type.
 * Allows the usage of `v = v + elem` (or -)
 * with the same behaviour as v.push_back(elem);
 */
vector <Dog> operator+(vector <Dog> &v, const Dog &b);
vector <Dog> operator+(const Dog &b, vector <Dog> &v);
vector <Dog> operator-(vector <Dog> &v, const Dog &b);
