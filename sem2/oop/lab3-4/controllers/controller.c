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

void controller_add_medication(Controller *this, Medication *what, bool verbose) {
    Medication *m = controller_find_medication(this, what);

    //printf("should add medication with name <%s>\n", what->name);

    if(m == NULL) {                                  // medication doesn't exist
        this->repo->v[this->repo->n++] = what;       // add it
        //medication_show(this->repo->v[this->repo->n-1]);
        if(verbose)
            printf("Medication added.\n");
    }
    else                                             // medication exists
        m->quantity += what->quantity;               // update its quantity

}

bool controller_delete_medication(Controller *this, Medication *what) {
    Medication *m = controller_find_medication(this, what);

    if(m == NULL)       // doesn't exist
        return false;   // can't delete


    *m = *this->repo->v[this->repo->n-1];
    this->repo->n--;
    return true;
}

void controller_update_medication(Controller *this, Medication *what, char *name, double *concentration, int *quantity, double *price) {
    strcpy(what->name, name);
    what->concentration = *concentration;
    what->quantity = *quantity;
    what->price = *price;
}


Medication *controller_find_medication(Controller *this, Medication *m) {
    for(int i=0; i<this->repo->n; i++)
        if(!strcmp(m->name, this->repo->v[i]->name) && m->concentration == this->repo->v[i]->concentration) {
            //printf("Found this medication on position %d.\n", i);
            return this->repo->v[i];
        }
    return NULL;
}


void controller_list_medications(Controller *this, bool indices) {
    if(this->repo->n == 0) {
        printf("No medications in stock.\n");
        return;
    }

    printf("There are %d medications in stock.\n", this->repo->n);

    medication_show_header();
    for(int i=0; i<this->repo->n; i++) {
        if(indices)
            printf("%d. ", i);
        medication_show(this->repo->v[i]);
    }
    medication_show_footer();
}
