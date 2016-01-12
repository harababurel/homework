#include <iostream>
#include <vector>
#define nmax 1005
using namespace std;

int n, root, a, b, c, sol[nmax];
vector <int> v[nmax], w[nmax];
bool seen[nmax];

int solve(int x) {
    seen[x] = true;

    sol[x] = 0;


    for(int i=0; i<v[x].size(); i++) {
        int y = v[x][i];
        int edge = w[x][i];

        if(seen[y]) continue;   //don't go back up


        if(v[y].size() == 1)
            sol[x] += edge;
        else
            sol[x] += min(solve(y), edge);
    }

    return sol[x];
}

int main() {
    ios::sync_with_stdio(false);
    while(cin>>n>>root) {
        for(int i=1; i<=n; i++) {
            v[i].clear();
            w[i].clear();
            seen[i] = false;
            sol[i] = 0;
        }


        for(int i=1; i<n; i++) {
            cin>>a>>b>>c;
            v[a].push_back(b);
            w[a].push_back(c);

            v[b].push_back(a);
            w[b].push_back(c);
        }

        cout<<solve(root)<<"\n";
    }

    return 0;
}
