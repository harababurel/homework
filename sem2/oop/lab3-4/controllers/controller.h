#pragma once
#include "../repos/repository.h"
#include <stdbool.h>

typedef struct {
    Repository repo;
} Controller;


Controller controller_create();

bool controller_add_medication(Controller this, Medication what);

void controller_find_medication(Controller this, char *name);

void controller_list_medications(Controller this);
