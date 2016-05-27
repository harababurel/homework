#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <algorithm>
#include "../repos/repo.h"
#include "../models/bill.h"
#include "controller.h"
using namespace std;

vector <Bill> &Controller::get_bills() {
    return repo.get_bills();
}

void Controller::add_bill(Bill what) {
    repo.add_bill(what);
}

void Controller::populate_from_file(string filename) {
    ifstream f(filename);

    cout<<"pula\n";

    string company_name, serial_number;
    double sum;
    bool is_paid;

    string line;
    while(getline(f, line)) {
        cout<<"line: "<<line<<"\n";
        istringstream iss(line);

        if(!(iss>>company_name))
            break;
        iss>>serial_number;
        iss>>sum;
        iss>>is_paid;

        cout<<"company_name: "<<company_name<<"\n";
        cout<<"serial_number: "<<serial_number<<"\n";
        cout<<"sum: "<<sum<<"\n";
        cout<<"is_paid: "<<is_paid<<"\n";


        Bill tmp(company_name, serial_number, sum, is_paid);
        add_bill(tmp);
        cout<<"added a new bill: "<<company_name<<"\n";
    }
}
