#include "medication.h"
#include <string.h>
#include <stdio.h>

Medication medication_create(char name[], double concentration, int quantity, double price) {
    Medication ret;

    strcpy(ret.name, name);
    ret.concentration = concentration;
    ret.quantity = quantity;
    ret.price = price;

    return ret;
}

void medication_show(Medication this) {
    printf("Medication <%s>: %lf, %d units, %lf leva.\n", this.name, this.concentration, this.quantity, this.price);
}
