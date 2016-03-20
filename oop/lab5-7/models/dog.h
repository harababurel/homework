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
    string get_breed() const;
    int get_age() const;
    string get_photograph() const;
    string represent() const;

};

inline bool operator==(const Dog &a, const Dog &b) {
    bool equal = a.get_breed() == b.get_breed();
    equal &= a.get_age() == b.get_age();
    equal &= a.get_photograph() == b.get_photograph();

    return equal;
}

