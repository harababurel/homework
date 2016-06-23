#include <bits/stdc++.h>
using namespace std;

const int nmax = 500005;
int n, v[nmax];

int l(int i) {
    return 2*i;
}

int r(int i) {
    return 2*i+1;
}

void heapify(int heapSize, int i) {
    bool hasL = l(i) <= heapSize;
    bool hasR = r(i) <= heapSize;
    int neo;

    if(!hasL)
        return;

    if(v[i] >= v[l(i)] && (!hasR || (hasR && v[i] >= v[r(i)])))
        return;

    if(!hasR || v[l(i)] >= v[r(i)])
        neo = l(i);
    if(hasR && v[r(i)] >= v[l(i)])
        neo = r(i);

    swap(v[i], v[neo]);
    heapify(heapSize, neo);
}

int main() {
    freopen("algsort.in", "r", stdin);
    freopen("algsort.out", "w", stdout);

    scanf("%d", &n);
    for(int i=1; i<=n; i++)
        scanf("%d ", &v[i]);

    for(int i=n/2; i>=1; i--)
        heapify(n, i);

    for(int i=n; i>=1; i--) {
        swap(v[1], v[i]);
        heapify(i-1, 1);
    }

    for(int i=1; i<=n; i++)
        printf("%d ", v[i]);
    printf("\n");

    return 0;
}
