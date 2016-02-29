#pragma once
#include "../models/medication.h"
#include <stdbool.h>

typedef struct {
    Medication *v[100];
    int n;
} Repository;

Repository *repo_create();

bool repo_cmp_alpha(Medication *a, Medication *b);

void repo_sort(Repository *this, bool (*cmp)(Medication *, Medication *));
