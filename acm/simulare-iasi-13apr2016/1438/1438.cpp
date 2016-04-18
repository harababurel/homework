#include <bits/stdc++.h>
using namespace std;

const long double eps = 0.0000000001;
long long n;

long long raise(long long base, long long exp) {
    if(exp == 0)
        return 1;
    if(exp == 1)
        return base;

    long long half = raise(base, exp/2);
    if(exp % 2 == 1)
        return base * half * half;
    return half * half;
}

bool check(long long base, long long p) {
    return (raise(base, p) == n);
}

int main() {
    while(true) {
        cin>>n;

        if(n == 0)
            return 0;

        bool done = false;
        for(long long p=64; p>=1 && !done; p--) {
            long long base = round(pow((long double)(n), 1.0/(long double)(p)));

            for(long long new_base = max(2LL, base-1000); new_base <= base+1000 && !done; new_base++) {
                if(check(new_base, p)) {
                    cout<<p<<"\n";
                    done = true;
                    break;
                }
            }
        }

    }

    return 0;
}
