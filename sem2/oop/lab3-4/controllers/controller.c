#include "../repos/repository.h"
#include "../models/medication.h"
#include "controller.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


Controller *controller_create() {
    Controller *this = malloc(sizeof(Controller));
    this->repo = repo_create();
    return this;
}

void controller_add_medication(Controller *this, Medication *what) {
    /*
    Medication *m = controller_find_medication(this, what->name);

    printf("should add medication with name <%s>\n", what->name);

    if(!strcmp(m->name, null_Medication->name)) {  // medication doesn't exist
        this.repo.v[++this.repo.n] = what;       // add it
        printf("added on new pos\n");
        printf("n became %d\n", this.repo.n);

        medication_show(*this.repo.v[this.repo.n]);

    }
    else                                    // medication exists
        m->quantity += what->quantity;        // update its quantity
    */

    this->repo->v[this->repo->n++] = what;
    printf("n became %d\n", this->repo->n);
}

/*
Medication *controller_find_medication(Controller this, char *name) {
    for(int i=0; i<this.repo.n; i++)
        if(!strcmp(name, this.repo.v[i]->name)) {
            printf("Found this medication on position %d.\n", i);
            return this.repo.v[i];
        }
    return NULL;
}
*/

void controller_list_medications(Controller *this) {

    if(this->repo->n == 0) {
        printf("No medications in stock.\n");
        return;
    }

    printf("There are %d medications in stock.\n", this->repo->n);


    for(int i=0; i<this->repo->n; i++)
        medication_show(this->repo->v[i]);

}
