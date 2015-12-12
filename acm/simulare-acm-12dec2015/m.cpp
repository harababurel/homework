#include <iostream>
#include <set>
#include <cmath>
#define f cin
#define g cout
#define nmax 1005
#define ll long long
#define eps 0.000001
using namespace std;

ll m, n, times[nmax], x[nmax];
int possibleSpeeds = 0;

set <ll> sol;

int main() {
    f>>m>>n;
    for(int i=1; i<=m; i++)
        f>>times[i];

    for(int i=1; i<=n; i++)
        f>>x[i];


    for(int start=1; start<=n-m+1; start++) {
        //we begin noticing at rock #start

        ll currentDist = x[start+1] - x[start];
        ll currentTime = times[2] - times[1];

        double realSpeed = double(currentDist) / double(currentTime);

        bool ok = true;

        //cout<<"am fixat startul la piatra "<<start<<"\n";
        for(int i=2; i<=m-1; i++) {
            //compare stones start+i and start+i-1

            currentDist = x[start+i] - x[start+i-1];
            currentTime = times[i+1] - times[i];

            double currentSpeed = double(currentDist) / double(currentTime);

            if(fabs(currentSpeed - realSpeed) > eps) {
                ok = false;

                //cout<<"nu potriveste viteza intre pietrele "<<start+i-1<<" si "<<start+i<<". currentSpeed = "<<currentSpeed<<", realSpeed = "<<realSpeed<<"\n";
                break;
            }
        }

        if(ok) {
            possibleSpeeds++;
            sol.insert(x[start+1] - x[start]);
        }
    }

    g<<sol.size()<<"\n";
    for(auto x:sol)
        g<<x<<" ";
    g<<"\n";

    return 0;
}
