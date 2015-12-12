#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <cmath>
#define rmax 60
#define pmax 500
#define tmax 130
#define f cin
#define g cout
#define timp first
#define coord second
using namespace std;

int r, c, v[rmax], k=0, guards, steps[pmax];
int startX, startY, stopX, stopY, x, y, timeCycle=1;
bool seen[tmax][rmax][rmax];
string line;
char jeg;

string a[tmax][rmax];
vector <pair <int, int>> phase[pmax];

queue <pair<int, pair <int, int>>> Q;

int dx[] = {0, 0, -1, 1, 0};
int dy[] = {-1, 1, 0, 0, 0};

bool inside(int x, int y) {
    return (0 <= x && x < r && 0 <= y && y < c);
}

int gcd(int a, int b) {
    if(a==0 ) return 1;
    if(b==0) return a;
    return gcd(b, a%b);
}

int cmmmc(int a, int b) {
    return (a*b) / gcd(a, b);
}

int main() {
    f>>r>>c;

    /*
    f.get();
    getline(f, line);
    for(int i=0; i<line.size(); i++) {

        if('0' <= line[i] && line[i] <= '9')
            v[k] = v[k] * 10 + line[i] - '0';

        else
    */

    f>>jeg;
    f>>startX>>startY;
    f>>jeg>>jeg;
    f>>stopX>>stopY;
    f>>jeg;

    startX--;
    startY--;
    stopX--;
    stopY--;
    //cout << "MASA";
    if (startX == stopX && startY == stopY) {
        cout << 0;
        return 0;
    }

    //cout<<startX<<" "<<startY<<" "<<stopX<<" "<<stopY<<"\n";
    for(int i=0; i<r; i++)
        f>>a[0][i];

    //for(int i=0; i<r; i++)
    //    cout<<a[0][i]<<"\n";

    f>>guards;
    //cout<<guards<<" guards.\n";

    for(int i=1; i<=guards; i++) {
        f>>steps[i];

        for(int j=1; j<=steps[i]; j++) {
            f>>jeg>>x>>y>>jeg;
            x--; y--;
            phase[i].push_back(make_pair(x, y));
        }

        for(int j=steps[i]-2; j>=1; j--) {
            //cout<<"pula\n";
            phase[i].push_back(phase[i][j]);
            steps[i]++;
        }
    }

    /*
    for(int guard=1; guard<=guards; guard++) {
        cout<<"Guard #"<<guard<<" has the following phases:\n";

        for(int i=0; i<steps[guard]; i++)
            cout<<"    "<<phase[guard][i].first<<" "<<phase[guard][i].second<<"\n";

    }*/
    //cout << "nu";
    for(int guard=1; guard<=guards; guard++)
        timeCycle = cmmmc(timeCycle, steps[guard]);
    //cout<<"Pila";


    // generam matrici
    for(int t=1; t<timeCycle; t++)
        for(int i=0; i<r; i++)
            a[t][i] = a[0][i];  //all matrices are identical to the initial one

    for(int t=0; t<tmax; t++) {

        for(int guard=1; guard<=guards; guard++) {
            pair <int, int> currentPhase = phase[guard][t % steps[guard]];

            a[t][currentPhase.first][currentPhase.second] = '#';

            int x = currentPhase.first;
            int y = currentPhase.second;

            for(int i=x-1; i>=0 && a[t][i][y] != '#'; i--) // in sus
                a[t][i][y] = '#';

            for(int i=x+1; i<r && a[t][i][y] != '#'; i++) // in jos
                a[t][i][y] = '#';

            for(int j=y-1; j>=0 && a[t][x][j] != '#'; j--) // in stanga
                a[t][x][j] = '#';

            for(int j=y+1; j<c &&  a[t][x][j] != '#'; j++) // in dreapta
                a[t][x][j] = '#';

        }

    }


    /*
    for(int t=0; t<=6; t++) {
        cout<<"Time: "<<t<<"\n";
        cout<<"Guardians at:\n";
        for(int guard=1; guard<=guards; guard++)
            cout<<"    ("<<phase[guard][t % steps[guard]].first<<", "<<phase[guard][t % steps[guard]].second<<")\n";
        cout<<"\n";

        for(int i=0; i<r; i++)
            cout<<a[t][i]<<"\n";

        cout<<"\n\n";
    }*/

    //(t=0, startX, startY) -> (t=minim, stopX, stopY)


    //facem bfs (timp, (x, y))
    Q.push(make_pair(0, make_pair(startX, startY)));
    while(!Q.empty()) {
        pair<int, pair<int, int>> current = Q.front();
        Q.pop();

        //cout<<"day "<<current.timp<<": am intrat in "<<current.coord.first<<", "<<current.coord.second<<"\n";
        seen[current.timp % timeCycle][current.coord.first][current.coord.second] = true;

        for(int i=0; i<=4; i++) {
            int newX = current.coord.first + dx[i];
            int newY = current.coord.second + dy[i];
            int newT = current.timp + 1;

            if(!inside(newX, newY) || seen[newT % timeCycle][newX][newY])
                continue;

            if(newX == stopX && newY == stopY) {
                //am gasit tortul
                //gata
                cout<<current.timp+1<<"\n";
                return 0;
            }

            if(a[newT % timeCycle][newX][newY] != '#') {
                seen[newT % timeCycle][newX][newY] = true;
                Q.push(make_pair(newT, make_pair(newX, newY)));
            }
        }
    }

    cout<<"IMPOSSIBLE\n";
    return 0;
}

