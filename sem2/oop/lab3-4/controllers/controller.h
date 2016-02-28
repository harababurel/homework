#pragma once
#include "../repos/repository.h"
#include <stdbool.h>

typedef struct {
    Repository *repo;
} Controller;


Controller *controller_create();

void controller_add_medication(Controller *this, Medication *what);

//Medication *controller_find_medication(Controller this, char *name);

void controller_list_medications(Controller *this);
