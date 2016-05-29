#include <bits/stdc++.h>
#include "linkedlist.h"
using namespace std;

int n, x, y, r;

int main() {
    std::ios::sync_with_stdio(false);
    linkedlist <pair <int, int>> l;

    cin>>n;
    for(int i=1; i<=n; i++) {
        cin>>x>>y>>r;

        l.push_back(make_pair(x-r, x+r));
    }

    l.show();

    return 0;
}
