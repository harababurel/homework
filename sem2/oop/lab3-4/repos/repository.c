#include "../models/medication.h"
#include "repository.h"

Repository repo_create() {
    Repository ret;
    ret.n = 0;
    return ret;
}
