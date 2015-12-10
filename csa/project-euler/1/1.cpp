#include <iostream>
using namespace std;

int main() {
    long long sol = 0;
    for(int i=1; i<=100000000; i++)
        if(i % 3 == 0 || i % 5 == 0)
            sol += i;

    cout<<sol<<"\n";
    return 0;
}
