#pragma once
#include "../controllers/controller.h"

typedef struct {
    Controller controller;
} UI;


void ui_run(UI ui);

void ui_get_command(UI ui, char *command);

void ui_show_help();

void ui_show_add_menu();
