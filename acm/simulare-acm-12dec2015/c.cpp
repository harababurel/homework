#include <fstream>
#include <string>
#include <iostream>
#include <map>
#include <set>
using namespace std;

ifstream fin("c.in");
ofstream fout("c.out");	

int m;
string line, username;

map<string, set<string> > mapping;
map<string, int> freq;
set<string> names;
set< pair<int, string> > sol;


int main() {
	cin >> m;
	cin.get();
	for (int i = 1; i <= m; ++i) {
		cin >> username;
		getline(cin, line);

		//cout<<"usernameul <"<<username<<"> si restul liniei <"<<line<<">\n";
		names.insert(username);

		string buffer = "";
		for (int j = 1; j <= line.size(); ++j) {
			if (line[j] == ' ' || j == line.size()) {
				if (freq.find(buffer) == freq.end())
					freq[buffer] = 0;

				freq[buffer] = freq[buffer] + 1;
				mapping[buffer].insert(username);

				//cout<<"current buff = <"<<buffer<<">\n";
				buffer = "";
			}
			else
				buffer = buffer + line[j];
		}
	}

	for (auto it : mapping) {
		//it = ("cuvant", {set de useri})
		if (it.second.size() == names.size()) {
			sol.insert(make_pair(-freq[it.first], it.first));
		}
	}

	for (auto it : sol) {
		cout<<it.second<<"\n";
	}

	if(sol.empty()) {
		cout<<"ALL CLEAR\n";
	}

return 0;

}