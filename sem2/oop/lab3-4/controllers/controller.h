#pragma once
#include "../repos/repository.h"
#include <stdbool.h>

typedef struct {
    Repository *repo;
} Controller;


Controller *controller_create();

void controller_add_medication(Controller *this, Medication *what, bool verbose);

bool controller_delete_medication(Controller *this, Medication *what);

void controller_update_medication(Controller *this, Medication *what, char *name, double *concentration, int *quantity, double *price);
Medication *controller_find_medication(Controller *this, Medication *m);

void controller_list_medications(Controller *this, bool indices);
