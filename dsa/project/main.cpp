#include <bits/stdc++.h>
#include "linkedlist.h"
using namespace std;

int n, x, y, r;

int main() {
    srand(time(0));
    std::ios::sync_with_stdio(false);
    /*
    linkedlist <pair <int, int>> l;

    cin>>n;
    for(int i=1; i<=n; i++) {
        cin>>x>>y>>r;

        l.push_back(make_pair(x-r, x+r));
    }
    */

    linkedlist <int> l;
    for(int i=1; i<=10; i++)
        l.push_back(rand() % 100);

    l.show();
    l.bubble_sort([](int a, int b) -> bool {
            return a <= b;
            });
    l.show();

    l.go_to_beginning();
    while(true) {
        cout<<l.get_current_data()<<" ";

        if(l.has_next())
            l.go_next();
        else
            break;
    }
    cout<<"\n";


    return 0;
}
