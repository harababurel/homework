#pragma once
#include <bits/stdc++.h>
#include "dog.h"
using namespace std;

class User {
public:
    User() {
        this->current_dog_id = 0;
        this->breed = "";
        this->maximum_age = 99999;
    }
    string breed;
    int current_dog_id, maximum_age;
    vector <Dog> adoption_list;
};
