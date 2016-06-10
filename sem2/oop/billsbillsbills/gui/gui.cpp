#include "gui.h"
#include <QApplication>
#include <iostream>

int GUI::run(int argc, char *argv[]) {
    controller.populate_from_file("samples/1.in");

    QApplication app(argc, argv);
    return app.exec();
}
