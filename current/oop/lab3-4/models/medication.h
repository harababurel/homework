#pragma once

typedef struct {
    char name[50];
    double concentration;
    int quantity;
    double price;
} Medication;


Medication *medication_create(char name[], double concentration, int quantity, double price);

void medication_show_header();
void medication_show_middle();
void medication_show_footer();

void medication_show(Medication *this);

