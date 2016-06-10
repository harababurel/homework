#include "bill.h"
using namespace std;

string Bill::get_company_name() {
    return company_name;
}

string Bill::get_serial_number() {
    return serial_number;
}

double Bill::get_sum() {
    return sum;
}

bool Bill::get_is_paid() {
    return is_paid;
}

void Bill::set_company_name(string company_name) {
    company_name = company_name;
}

void Bill::set_serial_number(string serial_number) {
    serial_number = serial_number;
}

void Bill::set_sum(double sum) {
    sum = sum;
}

void Bill::set_is_paid(bool is_paid) {
    is_paid = is_paid;
}
