#include <bits/stdc++.h>
#define nmax 100005
#define ll long long
using namespace std;

ll n, sol=0;
set <ll> divs;

int main() {
    cin>>n;

    for(ll i=1; i<=n; i++)
        if(n % i == 0)
            divs.insert(i);

    for(auto first:divs) {
        for(ll ratio=1; ratio<=n; ratio++) {
            ll sum=first;

            for(ll second=first*ratio; sum<=n; second *= ratio) {
                sum += second;
                if(sum == n)
                    sol++;
            }
        }
    }
    cout<<sol<<"\n";

    return 0;
}
