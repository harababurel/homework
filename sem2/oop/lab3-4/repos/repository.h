#pragma once
#include "../models/medication.h"

typedef struct {
    Medication *v[100];
    int n;
} Repository;

Repository *repo_create();
