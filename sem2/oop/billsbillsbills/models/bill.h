#pragma once
#include <string>
using namespace std;

class Bill {
private:
    string company_name;
    string serial_number;
    double sum;
    bool is_paid;
public:
    Bill(string company_name, string serial_number, double sum, bool is_paid) {
        company_name = company_name;
        serial_number = serial_number;
        sum = sum;
        is_paid = is_paid;
    }

    string get_company_name();
    string get_serial_number();
    double get_sum();
    bool get_is_paid();

    void set_company_name(string company_name);
    void set_serial_number(string serial_number);
    void set_sum(double sum);
    void set_is_paid(bool is_paid);
};
