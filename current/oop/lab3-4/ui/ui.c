#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../models/medication.h"
#include "../repos/repository.h"
#include "../controllers/controller.h"
#include "ui.h"

UI *ui_create() {
    UI *this = malloc(sizeof(UI));
    this->controller = controller_create();
    return this;
}

void ui_run(UI *this) {
    char command;

    while(1) {
        ui_get_command(this, &command);
        switch(command) {
            case 'a':
                ui_show_add_menu(this);
                break;
            case 'd':
                ui_show_delete_menu(this);
                break;
            case 's':
                ui_show_search_menu(this);
                break;
            case 'u':
                ui_show_update_menu(this);
                break;
            case 'x':
                exit(0);
            case 'h':
                ui_show_help(this);
                break;
            case 'l':
                controller_list_medications(this->controller, false);
                break;
            default:
                printf("Bad command. Try 'h' for help.\n");
        }
    }
}

void ui_get_command(UI *this, char *command) {
    printf("> ");
    scanf("%s", command);
}

void ui_show_help(UI *this) {
    printf("Commands:\n");
    printf("\ta - add\n");
    printf("\tu - update\n");
    printf("\td - delete\n");
    printf("\n");
    printf("\tl - list\n");
    printf("\ts - search\n");
    printf("\n");
    printf("\th - help\n");
    printf("\tx - exit\n");
}

void ui_show_add_menu(UI *this) {
    printf("You want to add a medication.\n");

    char name[50];
    double concentration;
    int quantity;
    double price = 0;

    printf("Name (str): ");
    scanf("%s", name);

    printf("Concentration (percentage): ");
    scanf("%lf", &concentration);

    printf("Quantity (int): ");
    scanf("%d", &quantity);

    Medication *m = medication_create(name, concentration, quantity, price);

    // if this medication already exists
    // i shouldn't ask again for the price
    if(controller_find_medication(this->controller, m) == NULL) {
        printf("Price (double): ");
        scanf("%lf", &price);
        m->price = price;
    }

    controller_add_medication(this->controller, m, true);
}

void ui_show_delete_menu(UI *this) {
    printf("You want to delete a medication.\n");

    char name[50];
    double concentration;

    printf("Name (str): ");
    scanf("%s", name);

    printf("Concentration (percentage): ");
    scanf("%lf", &concentration);

    Medication *m = medication_create(name, concentration, 0, 0);

    if(controller_delete_medication(this->controller, m))
        printf("Medication deleted.\n");
    else
        printf("Medication does not exist.\n");
}

void ui_show_update_menu(UI *this) {
    printf("You want to update a medication.\n");
    controller_list_medications(this->controller, false);
    printf("\nPlease identify an existing medication: \n");

    char name[50];
    double concentration;
    int quantity = 0;
    double price = 0;

    printf("Name (str): ");
    scanf("%s", name);

    printf("Concentration (percentage): ");
    scanf("%lf", &concentration);

    Medication *m = medication_create(name, concentration, quantity, price);
    Medication *what = controller_find_medication(this->controller, m);

    if(what != NULL) {
        printf("Medication found.\n");

        printf("\nNew name (str): ");
        scanf("%s", name);

        printf("New concentration (percentage): ");
        scanf("%lf", &concentration);

        printf("New quantity (int): ");
        scanf("%d", &quantity);

        printf("New price (double): ");
        scanf("%lf", &price);

        controller_update_medication(this->controller, what, name, &concentration, &quantity, &price);

        printf("Medication updated. :)\n");
    }
    else {
        printf("Medication not found.\n");
    }
}

void ui_show_search_menu(UI *this) {
    char name[50];
    printf("Term to search: ");
    scanf("%s", name);

    char sort_criteria;

    while(true) {
        printf("Sort [a]lphabetically or by [p]rice: ");
        scanf("%s", &sort_criteria);

        if(sort_criteria == 'a' || sort_criteria == 'p')
            break;
    }

    if(sort_criteria == 'a')
        repo_sort(this->controller->repo, repo_cmp_alpha);
    else
        repo_sort(this->controller->repo, repo_cmp_price);

    controller_search_medication(this->controller, name);
}
