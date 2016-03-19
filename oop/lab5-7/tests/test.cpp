#include <bits/stdc++.h>
#include "../models/dog.h"
#include "../repos/repository.h"
#include "test.h"
#include <cassert>

using namespace std;

void Test::test_models() {
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
}

void Test::test_repository() {
    Repository repo;

    Dog a("Labrador Retriever", 1, "http://d21vu35cjx7sd4.cloudfront.net/dims3/MMAH/crop/0x0%2B0%2B0/resize/645x380/quality/90/?url=http%3A%2F%2Fs3.amazonaws.com%2Fassets.prod.vetstreet.com%2Ff8%2F7a54f0a10511e087a80050568d634f%2Ffile%2FLabrador-1-645mk062111.jpg");

    Dog b("German Shepherd", 5, "http://r.ddmcdn.com/s_f/o_1/APL/uploads/2014/04/1394746213009gshep.jpg");

    assert(repo.get_population() == 0);

    repo.add_dog(a);
    repo.add_dog(b);

    assert(repo.get_population() == 2);

}

void Test::test_everything() {
    this->test_models();
    this->test_repository();
}
