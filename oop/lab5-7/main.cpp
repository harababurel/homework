#include "tests/test.h"
using namespace std;

int main() {

    Test T;
    if(!T.test_models())
        cout<<"All tests passed. :)\n";
    else
        cout<<"Some tests failed. :(\n";



    return 0;
}
