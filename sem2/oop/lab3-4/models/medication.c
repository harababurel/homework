#include "medication.h"
#include <string.h>

Medication medication_create(char name[], double concentration, int quantity, double price) {
    Medication ret;

    strcpy(ret.name, name);
    ret.concentration = concentration;
    ret.quantity = quantity;
    ret.price = price;

    return ret;
}

