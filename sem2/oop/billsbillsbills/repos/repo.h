#pragma once
#include <vector>
#include <algorithm>
#include "../models/bill.h"

class Repository {
private:
    vector <Bill> bills;
public:
    vector <Bill> &get_bills();
    void add_bill(Bill what);
};

