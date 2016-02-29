#pragma once
#include "../repos/repository.h"
#include <stdbool.h>

typedef struct {
    Repository *repo;
} Controller;


Controller *controller_create();

void controller_add_medication(Controller *this, Medication *what);

bool controller_delete_medication(Controller *this, Medication *what);

bool controller_update_medication(Controller *this, Medication *what, Medication *updated);

Medication *controller_find_medication(Controller *this, Medication *m);

void controller_list_medications(Controller *this, bool indices);
