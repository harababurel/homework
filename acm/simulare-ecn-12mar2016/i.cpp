#include <bits/stdc++.h>
#define nmax 2016
using namespace std;

int t, n, p, topright, x, y, top, toprightx, toprighty;
int sol[nmax];
bool inHull[nmax];
const double eps = 0.0;

struct point {
    int id, x, y;
};

vector <point> v;

int cmp(point a, point b) {

    double diff = (a.y - v[0].y) * (b.x - v[0].x) - (b.y - v[0].y) * (a.x - v[0].x);

    if(diff == 0.0) { // coliniare
        if(a.x == b.x)
            return a.y > b.y;
        return a.x < b.x;
    }
    return diff >= 0.0;

}

bool other_cmp(point a, point b) {
    if(a.x == b.x)
        return a.y < b.y;
    return a.x > b.x;
}

double cp(point a, point b, point c) {
    return (a.y - b.y) * c.x + (b.x - a.x) * c.y + a.x * b.y - b.x * a.y;
}

void hull() {
    top = 0;
    for(int i=0; i<v.size(); i++) {
        while(top > 2 && cp(v[sol[top-1]], v[sol[top]], v[i]) > 0) top--;
        sol[++top] = i;
    }
}

int main() {
    cin>>t;

    while(t--) {
        v.clear();
        memset(inHull, 0, nmax*sizeof(bool));
        memset(sol, 0, nmax*sizeof(int));

        cin>>n;

        toprightx = 0;
        toprighty = 0;

        p = 0;
        for(int i=0; i<n; i++) {
            cin>>x>>y;
            v.push_back(point {i, x, y });
            if(v[i].x < v[p].x || (v[i].x == v[p].x && v[i].y > v[p].y)) p = i;
            if(v[i].x > toprightx || (v[i].x == toprightx && v[i].y < toprighty)) {
                toprightx = v[i].x;
                toprighty = v[i].y;
            }
        }

        //cout<<"bottom-left: "<<v[p].id<<", with coordinates ("<<v[p].x<<", "<<v[p].y<<")\n";
        //cout<<"top-right: "<<v[topright].id<<" with coordinates ("<<v[topright].x<<", "<<v[topright].y<<")\n";

        swap(v[0], v[p]);

        auto it = v.begin();
        it++;
        sort(it, v.end(), cmp);

        hull();

        //for(int i=0; i<n; i++)
        //    cout<<v[i].id<<" ";

        //cout<<"hull contains: ";
        //cout<<"sol: ";
        for(int i=1; i<=top; i++) {
            inHull[v[sol[i]].id] = true;
            cout<<v[sol[i]].id<<" ";

            if(v[sol[i]].x == toprightx && v[sol[i]].y == toprighty) {
                //cout<<"pula ";
                break;
            }
        }
        //cout<<"\n";

        sort(v.begin(), v.end(), other_cmp);

        //cout<<"points to add later: ";
        for(int i=0; i<v.size(); i++) {
            if(!inHull[v[i].id])
                cout<<v[i].id<<" ";
        }
        cout<<"\n";


    }

    return 0;
}
