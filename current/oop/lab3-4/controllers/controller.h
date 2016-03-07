#pragma once
#include "../repos/repository.h"
#include <stdbool.h>

typedef struct {
    Repository *repo[1000];
    int index;
    int states;
} Controller;


Controller *controller_create();

void controller_add_medication(Controller *this, Medication *what, bool verbose);

bool controller_delete_medication(Controller *this, Medication *what);

void controller_update_medication(Controller *this, Medication *what, char *name, double *concentration, int *quantity, double *price);
Medication *controller_find_medication(Controller *this, Medication *m);

void controller_list_medications(Controller *this, bool indices);

void controller_search_medication(Controller *this, char *name);

void controller_filter_medication(Controller *this, int quantity);

void controller_prepare_future(Controller *this);

void controller_undo(Controller *this);
void controller_redo(Controller *this);

Repository *controller_get_current_repo(Controller *this);
