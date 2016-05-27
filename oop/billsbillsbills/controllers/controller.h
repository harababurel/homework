#pragma once
#include <vector>
#include <algorithm>
#include "../repos/repo.h"
#include "../models/bill.h"

class Controller {
private:
    Repository repo;
public:
    vector <Bill> &get_bills();
    void populate_from_file(string filename);
    void add_bill(Bill what);
};
