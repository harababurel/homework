#include <stdio.h>
#include "../models/medication.h"
#include "ui.h"

void ui_run(UI ui) {
    char command;

    while(1) {
        ui_get_command(ui, &command);
        switch(command) {
            case 'a':
                ui_show_add_menu();
                break;
            case 'x':
                return;
            case 'h':
                ui_show_help();
                break;
            default:
                printf("Bad command. Try 'h' for help.\n");
        }
    }
}

void ui_get_command(UI ui, char *command) {
    printf("> ");
    scanf("%s", command);
}

void ui_show_help() {
    printf("Commands:\n");
    printf("\ta - add medication\n");
    printf("\th - show this help page\n");
    printf("\tx - exit\n");
}

void ui_show_add_menu() {
    printf("You want to add a medication.\n");

    char name[50];
    double concentration;
    int quantity;
    double price;

    printf("Name (str): ");
    scanf("%s", name);

    printf("Concentration (%): ");
    scanf("%2f", &concentration);

    printf("Quantity (int): ");
    scanf("%d", &quantity);

    printf("Price (double): ");
    scanf("%2f", &price);


    Medication m = medication_create(name, concentration, quantity, price);
}
