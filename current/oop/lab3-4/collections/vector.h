#pragma once
#include "../models/medication.h"

typedef struct {
    Medication **arr;
    int size, capacity;
} vector;

vector *vector_create();

void vector_push_back(vector *this, Medication *m);

int vector_size(vector *this);

void vector_pop_back(vector *this);

Medication *vector_at_pos(vector *this, int pos);
