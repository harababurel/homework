#pragma once

typedef struct {
    char name[50];
    double concentration;
    int quantity;
    double price;
} Medication;

Medication medication_create(char name[], double concentration, int quantity, double price);
