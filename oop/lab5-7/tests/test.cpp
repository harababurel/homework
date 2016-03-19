#include <bits/stdc++.h>
#include "../models/dog.h"
#include "test.h"
#include <cassert>

using namespace std;

int Test::test_models() {
    Dog *a = new Dog("Beagle", 5, "no_photograph");

    assert(a->get_breed() == "Beagle");
    assert(a->get_age() == 5);
    assert(a->get_photograph() == "no_photograph");

    a->set_breed("Pitbull");

    assert(a->get_breed() != "Beagle");
    assert(a->get_breed() == "Pitbull");

    Dog *b = a;

    b->set_age(1);

    assert(b->get_age() == 1);  // a and b point to the same object
    assert(a->get_age() == 1);  // so both ages should be 1

    Dog c = *a;

    c.set_age(100);

    assert(c.get_age() == 100); // this should change the age of c
    assert(a->get_age() == 1);  // but not the age of a


    Dog *d = new Dog();
    *d = *a;                    // this should be a copy

    d->set_age(99);

    assert(d->get_age() == 99); // the copy should be 99
    assert(a->get_age() == 1);  // the original should stay the same

    d->set_photograph("some_photograph");

    assert(a->get_photograph() == "no_photograph");
    assert(d->get_photograph() == "some_photograph");

    return 0;
}
