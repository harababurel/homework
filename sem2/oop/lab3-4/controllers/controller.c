#include "../repos/repository.h"
#include "../models/medication.h"
#include "controller.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>


Controller controller_create() {
    Controller this;
    this.repo = repo_create();
    return this;
}

bool controller_add_medication(Controller this, Medication what) {
    return false;
}

void controller_find_medication(Controller this, char *name) {
    for(int i=0; i<this.repo.n; i++)
        if(!strcmp(name, this.repo.v[i].name))
            printf("Found this medication on position %d.\n", i);
}

void controller_list_medications(Controller this) {
    if(this.repo.n == 0) {
        printf("No medications in stock.\n");
        return;
    }

    printf("There are %d medications in stock.\n", this.repo.n);

    for(int i=0; i<this.repo.n; i++)
        medication_show(this.repo.v[i]);
}
