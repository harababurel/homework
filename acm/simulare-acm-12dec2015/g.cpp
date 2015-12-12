#include <iostream>
#include <fstream>
#include <map>
using namespace std;

double m, u, d;
string names[10];
int costs[10];
int alc[10];

map<string, int> rez;


const int MAXBEST = 13000;
const int MAXCOST = 1010;

int best[MAXBEST][MAXCOST];
int used[MAXBEST][MAXCOST];

int main() {
    cin >> m >> u >> d;

    int uInt = 1.0 * u * 600.0; // vezi cum merge
    int mInt = 1.0 * m * 100.0;

    string cuv;

    for (int i = 1; i <= d; ++i) {
        cin >> names[i];
        int proc;
        cin >> proc;


        cin.get(); cin.get(); cin.get();
        char c = cin.get();
        int q = (int)c - '0';
        q = 600 / q;
        alc[i] = q * proc;
        double cost;
        cin >> cost;
        costs[i] =1.0 *  cost * 100.0;

        //cout<<"names: "<<names[i]<<", alc: "<<alc[i]<<", costs: "<<costs[i];
        //cout<<"\n";
    }
    best[0][0] = 1; //nu uita scade 1 pe final vtm

    for (int i = 0; i <= uInt; ++i) {
        for (int j = 0; j <= mInt; ++j)
            for (int l = 1; l <= d; ++l) {
                if (i < alc[l] || j < costs[l] || !best[i - alc[l]][j - costs[l]]) continue;
                best[i][j] = 1;
                used[i][j] = l;

                //cout<<"pula";
            }
    }

    //cout<<"uInt: "<<uInt<<"\n";
    //cout<<"mInt: "<<mInt<<"\n";

    if (best[uInt][mInt] == 0) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }

    int curu = uInt, curm = mInt;


    while (curu > 0 && curm > 0) {
        int c = used[curu][curm];

        if(rez.find(names[c]) == rez.end())
            rez[names[c]] = 0;

        rez[names[c]] += 1;
        curu -= alc[c];
        curm -= costs[c];
    }

    for (map<string, int>::iterator it=rez.begin(); it != rez.end(); it++)
        cout << it->first << " " << it->second << "\n";

    return 0;

}
