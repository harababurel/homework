#include "tests/test.h"
#include "ui/ui.h"
using namespace std;

int main() {
    Test T;
    T.test_everything();
    cout<<"All tests passed. :)\n";

    UI ui;
    ui.run();

    return 0;
}
