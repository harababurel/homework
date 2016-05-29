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

bool check(unsigned int conf) {
    unordered_set <pair <int, int>, boost::hash <pair <int, int>>> edges;

    for(int i=1; i<=n; i++)
        for(auto y:v[i])
            edges.insert(make_pair(min(i, y), max(i, y)));

    for(int i=0; i<n; i++) {
        if((conf & (1<<i)) == 0)
            continue;

        int x = i+1;

        for(auto y:v[x])
            if(edges.find(make_pair(min(x, y), max(x, y))) != edges.end())
                edges.erase(edges.find(make_pair(min(x, y), max(x, y))));
    }

    return edges.empty();
}

unsigned int get_next_conf(unsigned int conf) {
    // Source: https://graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation
    unsigned int t = conf | (conf - 1); // t gets v's least significant 0 bits set to 1
    // Next set to 1 the most significant bit to change,
    // set to 0 the least significant ones, and add the necessary 1 bits.
    return (t + 1) | (((~t & -~t) - 1) >> (__builtin_ctz(conf) + 1));
}

vector <int> back() {
    vector <int> ret;

    if(n > 20)
        return ret;

    unsigned int conf, max_conf;

    for(int bits=1; bits<=n; bits++) {
        conf = 0;
        for(int i=0; i<bits; i++)
            conf |= (1<<i);

        max_conf = conf << (n-bits);

        while(conf <= max_conf) {
            if(check(conf)) {
                for(int i=0; i<n; i++)
                    if(conf&(1<<i))
                        ret.push_back(i+1);
                return ret;
            }
            conf = get_next_conf(conf);
        }
    }
    return ret;
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

    cerr<<"Running dumb greedy.\n";
    vector <int> dumb_vertex_cover = greedy(false);

    cerr<<"Running "<<samples<<" dumb greedy instances.\n";
    vector <int> best_dumb_vertex_cover = get_best_dumb_greedy();

    cerr<<"Running smart greedy.\n";
    vector <int> smart_vertex_cover = greedy(true);

    cerr<<"Running backtracking.\n";
    vector <int> back_vertex_cover = back();

    sort(dumb_vertex_cover.begin(), dumb_vertex_cover.end());
    sort(best_dumb_vertex_cover.begin(), best_dumb_vertex_cover.end());
    sort(smart_vertex_cover.begin(), smart_vertex_cover.end());
    sort(back_vertex_cover.begin(), back_vertex_cover.end());

    cout<<"Dumb greedy once - ("<<dumb_vertex_cover.size()<<") ";
    if(dumb_vertex_cover.size() < 10)
        cout<<" ";
    for(auto x:dumb_vertex_cover)
        cout<<x<<" ";
    cout<<"\n";

    cout<<"Dumb greedy x"<<samples/1000<<"k - ("<<best_dumb_vertex_cover.size()<<") ";
    if(best_dumb_vertex_cover.size() < 10)
        cout<<" ";
    for(auto x:best_dumb_vertex_cover)
        cout<<x<<" ";
    cout<<"\n";

    cout<<"Smart greedy ----- ("<<smart_vertex_cover.size()<<") ";
    if(smart_vertex_cover.size() < 10)
        cout<<" ";
    for(auto x:smart_vertex_cover)
        cout<<x<<" ";
    cout<<"\n";

    cout<<"Backtracking ----- ("<<back_vertex_cover.size()<<") ";
    if(back_vertex_cover.size() < 10)
        cout<<" ";
    for(auto x:back_vertex_cover)
        cout<<x<<" ";
    cout<<"\n";

    return 0;
}
