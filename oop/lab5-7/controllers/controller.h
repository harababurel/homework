#pragma once
#include <bits/stdc++.h>
#include "../repos/repository.h"
using namespace std;

class Controller {
private:
    Repository repo;

public:
    Repository get_repo() const;

    /* Returns true if dog was added,
     * false otherwise.
     */
    bool add_dog(Dog what);

    bool dog_exists(Dog what);

    /* Returns true if dog was removed,
     * false otherwise.
     */
    bool remove_dog(Dog what);

    /* Returns the vector position on
     * which the dog is found, or -1
     * otherwise.
     */
    int find_dog(Dog what);
    void update_dog(int pos, Dog what);
    vector <Dog> get_dogs();
    void populate_from_file(string filename);
};

