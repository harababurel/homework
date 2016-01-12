#include <iostream>
#define nmax 1500
#define inf (1<<30)
using namespace std;

int n, m, tx, ty, a[nmax][nmax], grid[nmax][nmax];
string s[nmax];

int main() {
    while(cin>>n>>m>>tx>>ty) {

        int newN = n + tx - 1;
        int newM = m + ty - 1;




        for(int i=0; i<n; i++) {
            cin>>s[i];

            for(int j=m; j<newM; j++)
                s[i] += ".";
        }

        for(int i=n; i<newN; i++) {
            s[i] = "";

            for(int j=0; j<newM; j++)
                s[i] += ".";
        }

        n = newN;
        m = newM;

        for(int i=0; i<n; i++)
            for(int j=0; j<m; j++)
                grid[i][j] = 0;


        for(int i=0; i<n; i++)
            for(int j=0; j<m; j++) {
                a[i][j] = (s[i][j] == 'X');
                if(i > 0)
                    a[i][j] += a[i-1][j];
                if(j > 0)
                    a[i][j] += a[i][j-1];

                if(i > 0 && j > 0)
                    a[i][j] -= a[i-1][j-1];
                //a[i][j] = nr. de X-uri din dreptunghiul ...
            }
        /*
        cout<<"sumele partiale:\n";
        for(int i=0; i<n; i++) {
            for(int j=0; j<m; j++)
                cout<<a[i][j]<<" ";
            cout<<"\n";
        }
        cout<<"\n\n";
        */



        for(int i = 0; i<n; i++)
            for(int j = 0; j<m; j++) {

                int drept = a[i][j];

                if(i-tx >= 0)
                    drept -= a[i-tx][j];
                if(j-ty >= 0)
                    drept -= a[i][j-ty];

                if(i-tx >= 0 && j-ty >= 0)
                    drept += a[i-tx][j-ty];

                //^ nr de X-uri din dreptunghi

                if(drept > 0){
                    //cout<<i<<" "<<j<<" in gridul "<<i%tx<<" "<<j%ty<<"\n";
                    grid[i % tx][j % ty]++;
                }

            }

        int bestOccupied = inf;
        for(int i=0; i<tx; i++)
            for(int j=0; j<ty; j++) {
                //cout<<"gridul modulo "<<i<<" "<<j<<" are "<<grid[i][j]<<" patrate\n";
                bestOccupied = min(bestOccupied, grid[i][j]);
            }

        cout<<bestOccupied<<"\n";
    }

    return 0;
}
