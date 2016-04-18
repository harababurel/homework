#include <bits/stdc++.h>
using namespace std;

int n;
string a, b;

int main() {
    cin>>n;
    while(n--) {
        cin>>a>>b;

        bool ok = false;

        int j=0;
        for(int i=0; i<=a.size(); i++)
            if(j < b.size() && a[i] == b[j])
                j++;

        if(j == b.size())
            ok = true;

        reverse(b.begin(), b.end());

        j = 0;
        for(int i=0; i<=a.size(); i++)
            if(j < b.size() && a[i] == b[j])
                j++;

        if(j == b.size())
            ok = true;

        if(ok)
            cout<<"YES\n";
        else
            cout<<"NO\n";
    }

    return 0;
}
