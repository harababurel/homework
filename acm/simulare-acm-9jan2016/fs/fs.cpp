#include <iostream>
#include <cstring>
#include <vector>
#include <map>
#include <algorithm>
#define alpha 150
using namespace std;

string morse[alpha];
string s, code;
vector <int> v;
map <string, char> back_to_ascii;

int main() {
    ios::sync_with_stdio(false);
    morse['A']= ".-";
    morse['H']= "....";
    morse['O']= "---";
    morse['V']= "...-";
    morse['B']= "-...";
    morse['I']= "..";
    morse['P']= ".--.";
    morse['W']= ".--";
    morse['C']= "-.-.";
    morse['J']= ".---";
    morse['Q']= "--.-";
    morse['X']= "-..-";
    morse['D']= "-..";
    morse['K']= "-.-";
    morse['R']= ".-.";
    morse['Y']= "-.--";
    morse['E']= ".";
    morse['L']= ".-..";
    morse['S']= "...";
    morse['Z']= "--..";
    morse['F']= "..-.";
    morse['M']= "--";
    morse['T']= "-";
    morse['G']= "--.";
    morse['N']= "-.";
    morse['U']= "..-";
    morse['_']= "..--";
    morse['.']= "---.";
    morse[',']= ".-.-";
    morse['?']= "----";

    for(int i='A'; i<='Z'; i++)
        back_to_ascii[morse[i]] = i;
    back_to_ascii["..--"] = '_';
    back_to_ascii["---."] = '.';
    back_to_ascii[".-.-"] = ',';
    back_to_ascii["----"] = '?';


    while(getline(cin, s)) {
        v.clear();
        code = "";

        for(int i=0; i<s.size(); i++) {
            code += morse[s[i]];
            v.push_back(morse[s[i]].size());
        }

        //cout<<"code is "<<code<<"\n";
        //reverse(code.begin(), code.end());
        reverse(v.begin(), v.end());


        int so_far = 0;
        for(auto x:v) {
            string morse_char = code.substr(so_far, x);
            so_far += x;

            //cout<<morse_char<<" ";
            cout<<back_to_ascii[morse_char];
            //cout<<"\n";
        }
        cout<<"\n";

    }

    return 0;
}
