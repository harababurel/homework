#include <iostream>
#include <vector>
#include <cstring>
#define nmax 10005
#define mmax 20005
using namespace std;

vector <int> v[nmax];
int n, m, a, b, dim, with_three;
bool seen[nmax];
bool ok;


void dfs(int x) {
    seen[x] = true;

    if(v[x].size() == 3) with_three++;
    if(v[x].size() >= 4) ok = true;

    for(auto y:v[x]) {
        if(v[y].size() >= 4) ok = true;
        if(!seen[y])
            dfs(y);
    }
}

int main() {
    ios::sync_with_stdio(false);
    while(cin>>n>>m) {
        for(int i=1; i<=n; i++) {
            v[i].clear();
            seen[i] = false;
        }

        for(int i=1; i<=m; i++) {
            cin>>a>>b;
            v[a].push_back(b);
            v[b].push_back(a);
        }

        ok = false;
        for(int i=1; i<=n; i++) {
            with_three = 0;
            if(!seen[i])
                dfs(i);

            if(with_three >= 2)
                ok = true;
        }

        cout<<(ok? "YES\n":"NO\n");
    }

    return 0;
}
