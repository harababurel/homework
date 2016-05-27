#include "repo.h"
#include "../models/bill.h"

vector <Bill> &Repository::get_bills() {
    return bills;
}

void Repository::add_bill(Bill what) {
    bills.push_back(what);
}
