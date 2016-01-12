#include <iostream>

using namespace std;
const int inf = 1e9;
int n, m, x, y, c, a[1010][1010], countMax, count;

int main()
{
    while (cin>>n)
    {
        cin>>m>>x>>y;
        for (int  i = 1; i<=n; i++)
            for (int j = 1; j<=m; j++)
            {
                cin>>c;
                a[i][j] = a[i-1][j] + a[i][j-1]-a[i-1][j-1];
                if (c == 'X') a[i][j]++;
            }
    countMax = inf;
        for (int i = 1; i<=x;i++)
        for (int j= 1; j<=y; j++)
            { count = 0;
            for (int x1 = i+x; x1<=n; x1+=x)
            for (int y1 = j+y; y1<=m; y1+=y)
                if (a[x1][y1]+a[x1-x][y1-y]-a[x1][y1-y]-a[x1-x][y1] > 0) count ++;
            countMax = min(countMax, count);
            }
    cout<<countMax<<"\n";
    }
}
