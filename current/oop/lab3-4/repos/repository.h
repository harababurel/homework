#pragma once
#include "../models/medication.h"
#include <stdbool.h>

typedef struct {
    Medication *v[100];
    int n;
} Repository;

Repository *repo_create();

bool repo_cmp_name(Medication *a, Medication *b);
bool repo_cmp_concentration(Medication *a, Medication *b);
bool repo_cmp_quantity(Medication *a, Medication *b);
bool repo_cmp_price(Medication *a, Medication *b);

void repo_sort(Repository *this, bool (*cmp)(Medication *, Medication *));
void repo_reverse(Repository *this);
