#include <bits/stdc++.h>
#include "../models/club.h"
#include "../repos/repo.h"
#include "test.h"
#include <cassert>

using namespace std;

void Test::test_repository() {
    Repo repo;

    Club a("OK", "disco", 5);
    Club b("BiancoNero", "house", 4);

    repo.add_club(a);
    repo.add_club(b);

    assert(repo.get_size() == 2);

    repo.remove_club(a);
    assert(repo.get_size() == 1);
}
