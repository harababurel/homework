#include <bits/stdc++.h>
#define nmax 30
using namespace std;

const long double eps = 0.0001;

long double dist(long double x1, long double x2, long double y1, long double y2) {
    return sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
}


int n;
long double x[nmax], y[nmax];

vector <string> sol;
vector <int> v;

bool cmp(int i, int j) {
    double originX = 0;
    double originY = 0;

    double slope1 = (x[v[i]] - originX) / (y[v[i]] - originY);
    double slope2 = (x[v[j]] - originX) / (y[v[j]] - originY);

    return (slope1 < slope2);
}

int main() {
    while(true) {
        cin>>n;
        if(n==0)
            break;

        for(int i=1; i<=n; i++)
            cin>>x[i]>>y[i];

        sol.clear();
        for(int i=1; i<=n; i++)
            for(int j=i+1; j<=n; j++)
                for(int k=j+1; k<=n; k++)
                    for(int l=k+1; l<=n; l++) {
                        v.clear();
                        v.push_back(i);
                        v.push_back(j);
                        v.push_back(k);
                        v.push_back(l);

                        //sort(v.begin(), v.end(), cmp);


                        bool ok = false;

                        do {
                            long double ab = dist(x[v[0]], x[v[1]], y[v[0]], y[v[1]]);
                            long double ac = dist(x[v[0]], x[v[2]], y[v[0]], y[v[2]]);
                            long double ad = dist(x[v[0]], x[v[3]], y[v[0]], y[v[3]]);
                            long double bc = dist(x[v[1]], x[v[2]], y[v[1]], y[v[2]]);
                            long double bd = dist(x[v[1]], x[v[3]], y[v[1]], y[v[3]]);
                            long double cd = dist(x[v[2]], x[v[3]], y[v[2]], y[v[3]]);

                            long double lhs = ab * cd + bc * ad;
                            long double rhs = ac * bd;

                            if(fabs(lhs - rhs) < eps)
                                ok = true;
                        } while(next_permutation(v.begin(), v.end()));

                        if(ok) {
                            string s = "";
                            s += char('A'+i-1);
                            s += char('A'+j-1);
                            s += char('A'+k-1);
                            s += char('A'+l-1);
                            sol.push_back(s);
                        }
                    }

        cout<<sol.size()<<"\n";
        for(int i=0; i<sol.size(); i++)
            cout<<sol[i]<<"\n";
    }

    return 0;
}
