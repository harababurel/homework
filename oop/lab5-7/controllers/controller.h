#pragma once
#include <bits/stdc++.h>
#include "../repos/repository.h"
using namespace std;

class Controller {
private:
    Repository repo;

public:
    Repository get_repo() const;
    void add_dog(Dog what);
    void remove_dog(Dog what);
};

