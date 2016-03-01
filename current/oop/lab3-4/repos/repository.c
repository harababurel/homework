#include "../models/medication.h"
#include "repository.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

Repository *repo_create() {
    Repository *ret = malloc(sizeof(Repository));
    ret->n = 0;
    return ret;
}

bool repo_cmp_alpha(Medication *a, Medication *b) {
    return (strcmp(a->name, b->name) <= 0);
}

bool repo_cmp_price(Medication *a, Medication *b) {
    return (a->price <= b->price);
}

void repo_sort(Repository *this, bool (*cmp)(Medication *, Medication *)) {
    for(int i=0; i<this->n; i++)
        for(int j=i+1; j<this->n; j++)
            if(!cmp(this->v[i], this->v[j])) {
                Medication *temp = this->v[i];
                this->v[i] = this->v[j];
                this->v[j] = temp;
            }
}

