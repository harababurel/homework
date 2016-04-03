#include <bits/stdc++.h>
#define nmax 100005
#define logmax 20
#define ll long long
using namespace std;

ll n, w, x;
ll v[nmax];
ll dp[logmax][nmax], expo[nmax];

int preprocess() {
    for(int i=2; i<=n; i++)
        expo[i] = 1 + expo[i/2];

    for(int i=1; i<=n; i++)
        dp[0][i] = v[i];

    for(int j=1; (1<<j)<=n; j++)
        for(int i=1; i+(1<<j)-1<=n; i++)
            dp[j][i] = max(dp[j-1][i], dp[j-1][i + (1<<(j-1))]);
}

int query(int L, int R) {
    ll k = expo[R-L+1];
    return max(dp[k][L], dp[k][R-(1<<k)+1]);
}

int main() {
    cin>>n>>w;
    for(int i=1; i<=n; i++)
        cin>>v[i];


    preprocess();

    for(int l=1; l<=n; l++) {
        ll groupsize = l;
        ll total_width = 0;

        //cout<<"Trying l="<<l<<"\n";

        for(int i=1; i<=n; i+=groupsize) {
            ll L = i;
            ll R = min(n, i+groupsize-1);

            ll width = query(L, R) + 1;

            //cout<<"Got interval ["<<L<<", "<<R<<"] with width "<<width<<".\n";

            total_width += width;
        }

        total_width--;

        if(total_width <= w) {
            cout<<l<<"\n";
            return 0;
        }
    }

    return 0;
}

