#include "medication.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

Medication *medication_create(char name[], double concentration, int quantity, double price) {
    Medication *ret = malloc(sizeof(Medication));

    strcpy(ret->name, name);
    ret->concentration = concentration;
    ret->quantity = quantity;
    ret->price = price;

    return ret;
}

void medication_show(Medication *this) {
    printf("%s (%.2lf%%): %d units, %.2lf leva.\n", this->name, this->concentration, this->quantity, this->price);
}
