#include <iostream>
#include <string>
#include <cmath>
using namespace std;

int lcd(int a, int b)
{
    if (b == 0) return a;
    return lcd(b, a%b);
}
string s;
int n;
int main()
{
    ios::sync_with_stdio(false);
    while (getline(cin, s)){
        n = 0;
        for (int i = 2; i<=s.length()-1; i++)
            n*=10,n+=s[i]-'0';
        int count = 0;
        for (int i = n+1; i<=2*n; i++)
        {
            if (n %((i-n)/lcd(n, i-n)) == 0) count++;
        }
        cout << count<<"\n";
    }
}
