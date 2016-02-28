#include "../models/medication.h"
#include "repository.h"
#include <stdlib.h>

Repository *repo_create() {
    Repository *ret = malloc(sizeof(Repository));
    ret->n = 0;
    return ret;
}
