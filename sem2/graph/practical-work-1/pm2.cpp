#include <bits/stdc++.h>
#define f cin
#define g cout
using namespace std;

const int nmax = 1001;
const int inf = (1<<30);

int n, m, x;
int duration[nmax], from_root=0;
int min_time[nmax], max_time[nmax];
int level[nmax], low[nmax], parent[nmax];
bool solved[nmax], articulation[nmax];

bool visited[nmax];

vector <int> succs[nmax], prevs[nmax], v[nmax];
vector <int> sorted;

void solve_min(int x) {
    if(solved[x])
        return;

    solved[x] = true;

    for(auto prev:prevs[x]) {
        if(!solved[prev])
            solve_min(prev);
        min_time[x] = max(min_time[x], min_time[prev] + duration[prev]);
    }
}

void solve_max(int x) {
    if(solved[x])
        return;

    solved[x] = true;

    for(auto succ:succs[x]) {
        if(!solved[succ])
            solve_max(succ);
        max_time[x] = min(max_time[x], max_time[succ] - duration[x]);
    }
}

void topsort(int x) {
    visited[x] = true;

    for(auto y:succs[x])
        if(!visited[y])
            topsort(y);

    sorted.push_back(x);
}

bool has_cycles() {
    bool *so_far = new bool[nmax];

    for(auto x:sorted) {
        so_far[x] = true;
        for(auto y:succs[x])
            if(so_far[y]) {
                cout<<"Problem: "<<x<<" -> "<<y<<"\n";
                delete[] so_far;
                return true;
            }
    }
    delete[] so_far;
    return false;
}

void articulation_points(int x, int h) {
    visited[x] = true;
    level[x] = h;

    low[x] = level[x];
    for(auto y:v[x]) {
        if(!visited[y]) {
            parent[y] = x;
            articulation_points(y, h+1);
            low[x] = min(low[x], low[y]);
            if(x == 1)
                from_root++;
        }
        else if(y != parent[x])
            low[x] = min(low[x], level[y]);

        if(low[y] >= level[x])
            articulation[x] = true;
    }

}


int main() {
    std::ios::sync_with_stdio(0);
    cin>>n;
    for(int i=1; i<=n; i++)
        cin>>duration[i];

    for(int i=1; i<=n; i++) {
        cin>>m;

        while(m--) {
            cin>>x;

            succs[x].push_back(i);
            prevs[i].push_back(x);

            v[i].push_back(x);
            v[x].push_back(i);
        }
    }

    for(int i=1; i<=n; i++) {
        if(!visited[i])
            topsort(i);
    }

    reverse(sorted.begin(), sorted.end());

    if(has_cycles()) {
        cout<<"Cycle detected, graph is not DAG.\n";
        return 0;
    }

    cout<<"topsort: ";
    for(auto x:sorted)
        cout<<x<<" ";
    cout<<"\n";


    int total_duration = 0;
    for(int i=1; i<=n; i++) {
        solve_min(i);
        total_duration = max(total_duration, min_time[i] + duration[i]);
    }

    memset(solved, 0, nmax*sizeof(bool));
    for(int i=1; i<=n; i++)
        max_time[i] = total_duration - duration[i];

    for(int i=1; i<=n; i++)
        solve_max(i);

    cout<<"total duration: "<<total_duration<<"\n";
    for(int i=1; i<=n; i++)
        cout<<"soonest: "<<min_time[i]<<", latest: "<<max_time[i]<<"\n";

    memset(visited, 0, nmax*sizeof(bool));
    articulation_points(1, 0);
    articulation[1] = from_root >= 2;

    cout<<"articulation points: ";
    for(int i=1; i<=n; i++)
        if(articulation[i])
            cout<<i<<" ";
    cout<<"\n";

    return 0;
}
