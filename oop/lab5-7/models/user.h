#pragma once
#include <bits/stdc++.h>
#include "dog.h"
using namespace std;

class User {
public:
    User() {
        this->current_dog_id = 0;
    }
    int current_dog_id;
    vector <Dog> adoption_list;
};
