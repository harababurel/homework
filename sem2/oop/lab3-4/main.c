#include <stdio.h>
#include "ui/ui.h"
#include "controllers/controller.h"
#include "repos/repository.h"
#include "models/medication.h"

void run() {
    UI ui = ui_create();
    ui_run(ui);
}

int main() {
    run();

    return 0;
}
