#pragma once
#include <bits/stdc++.h>
#include <cstring>
using namespace std;

class Dog {
public:
    Dog(string breed, int age, string photograph);
    Dog();
    ~Dog();

private:
    string breed;
    int age;
    string photograph;
public:
    void set_breed(string what);
    void set_age(int what);
    void set_photograph(string what);
    string get_breed();
    int get_age();
    string get_photograph();

};

