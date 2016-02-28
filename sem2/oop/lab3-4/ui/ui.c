#include <stdio.h>
#include <stdlib.h>
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
            case 'x':
                exit(0);
            case 'h':
                ui_show_help(this);
                break;
            case 'l':
                controller_list_medications(this->controller);
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
    printf("\ta - add medication\n");
    printf("\tl - list medications.\n");
    printf("\th - show this help page\n");
    printf("\tx - exit\n");
}

void ui_show_add_menu(UI *this) {
    printf("You want to add a medication.\n");

    char name[50];
    double concentration;
    int quantity;
    double price;

    printf("Name (str): ");
    scanf("%s", name);

    printf("Concentration (percentage): ");
    scanf("%lf", &concentration);

    printf("Quantity (int): ");
    scanf("%d", &quantity);

    printf("Price (double): ");
    scanf("%lf", &price);


    Medication *m = medication_create(name, concentration, quantity, price);
    controller_add_medication(this->controller, m);
}
