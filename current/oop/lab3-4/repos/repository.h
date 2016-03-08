#pragma once
#include <stdbool.h>
#include "../models/medication.h"
#include "../collections/vector.h"


typedef struct {
    vector *v;
} Repository;

Repository *repo_create();

bool repo_cmp_name(Medication *a, Medication *b);
bool repo_cmp_concentration(Medication *a, Medication *b);
bool repo_cmp_quantity(Medication *a, Medication *b);
bool repo_cmp_price(Medication *a, Medication *b);

void repo_sort(Repository *this, bool (*cmp)(Medication *, Medication *));
void repo_reverse(Repository *this);
