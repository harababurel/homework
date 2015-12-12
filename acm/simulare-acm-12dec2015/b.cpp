#include <iostream>
#include <cmath>
#include <iomanip>
#define pi 3.14159265358979323846264
using namespace std;

int n;
double g, d[10];
double theta[10];
double vi=0.0, vf;

int main() {
    cin>>n>>g;

    for(int i=1; i<=n; i++) {
        cin>>d[i]>>theta[i];
        theta[i] *= pi / 180.0;
    }


    for(int start=1; start<=n; start++) {
        vi = 0.0;
        vf = 0.0;

        for(int i=start; i<=n; i++) {
            double acc = double(g) * cos(theta[i]);

            vf = sqrt(vi*vi + 2.0 * acc * double(d[i]));
            vi = vf;
        }
        cout<<fixed;
        cout<<setprecision(5);
        cout<<vf<<"\n";
    }

    return 0;
}
