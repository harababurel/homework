#include <bits/stdc++.h>
#include <boost/functional/hash.hpp>
using namespace std;

const int nmax = 100005;
const int samples = 10000;

int n, m, x, y;
vector <int> v[nmax];

vector <int> greedy(bool smart) {
    unordered_set <pair <int, int>, boost::hash <pair <int, int>>> edges;
    vector <int> vertices, ret;
    vertices.reserve(n);

    for(int i=1; i<=n; i++) {
        for(auto y:v[i])
            edges.insert(make_pair(min(i, y), max(i, y)));
        vertices.push_back(i);
    }

    if(!smart)
        random_shuffle(vertices.begin(), vertices.end());
    else
        sort(vertices.begin(), vertices.end(),
             [](const int &a, const int &b) -> bool {
                return v[a].size() > v[b].size();
             });

    for(auto x:vertices) {
        for(auto y:v[x])
            if(edges.find(make_pair(min(x, y), max(x, y))) != edges.end())
                edges.erase(edges.find(make_pair(min(x, y), max(x, y))));

        ret.push_back(x);
        if(edges.empty())
            return ret;
    }

    return ret;
}

vector <int> get_best_dumb_greedy() {
    vector <int> ret = greedy(false);

    for(int i=1; i<=samples; i++) {
        vector <int> alternative = greedy(false);
        if(alternative.size() < ret.size())
            ret = alternative;
    }
    return ret;
}

vector <int> back() {
    vector <int> ret;

    if(n > 30)
        return ret;

    //for(int conf=0; conf<

}

int main() {
    std::ios::sync_with_stdio(false);
    srand((unsigned)time(0)^getpid());

    cin>>n>>m;
    for(int i=1; i<=m; i++) {
        cin>>x>>y;

        v[x].push_back(y);
        v[y].push_back(x);
    }


    vector <int> dumb_vertex_cover = get_best_dumb_greedy();
    vector <int> smart_vertex_cover = greedy(true);

    cout<<"Dumb greedy -- ("<<dumb_vertex_cover.size()<<"): ";
//    for(auto x:dumb_vertex_cover)
//        cout<<x<<" ";
    cout<<"\n";

    cout<<"Smart greedy - ("<<smart_vertex_cover.size()<<"): ";
//    for(auto x:smart_vertex_cover)
//        cout<<x<<" ";
    cout<<"\n";

    return 0;
}
