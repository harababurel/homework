#include <iostream>

#define MAXARR 1000000
using namespace std;
int r[MAXARR],n,a,d,c;
long long tot,rest;

int main()
{
    cin>>c;
    while (c-- > 0)
    {
        cin>>d>>n;
        r[0] = 1;
        for (int i = 1; i<=d;i++)
         r[i] = 0;
        rest = 0;
        tot = 0;
        for (int i = 1; i<=n; i++)
        {
            cin>>a;
            rest = (rest + a)%d;
            tot+=r[rest]++;
        }
        cout<<tot<<"\n";
    }
}
