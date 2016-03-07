#pragma once
#include "../controllers/controller.h"

typedef struct {
    Controller *controller;
} UI;

UI *ui_create();

void ui_run(UI *this);

void ui_get_command(UI *this, char *command);

void ui_show_help(UI *this);

void ui_show_add_menu(UI *this);

void ui_show_delete_menu(UI *this);

void ui_show_update_menu(UI *this);

void ui_show_search_menu(UI *this);

void ui_show_short_supply_menu(UI *this);
