#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <map>

using namespace std;

set<char> goodLetters, goodLettersB;
map<char, string> goodFinals;
map<char, char> mapping, mapping2;

char get_nearest_small(char a) {
	for (int i = 0; i <= 30; ++i) {
		if ((int)a - i >= (int)'a' && goodLetters.find((char)((int)a - i)) != goodLetters.end())
			return (char)((int)a - i);
		if ((int)a + i <= (int)'z' && goodLetters.find((char)((int)a + i)) != goodLetters.end())
			return (char)((int)a + i);
	}
}

char get_nearest_big(char a) {
	for (int i = 0; i <= 30; ++i) {
		if ((int)a - i >= (int)'A' && goodLettersB.find((char)((int)a - i)) != goodLettersB.end())
			return (char)((int)a - i);
		if ((int)a + i <= (int)'Z' && goodLettersB.find((char)((int)a + i)) != goodLettersB.end())
			return (char)((int)a + i);
	}
}

string get_nearest_final(char a) {
	for (int i = 0; i <= 30; ++i) {
		if ((int)a - i >= (int)'a' && goodFinals.find((char)((int)a - i)) != goodFinals.end())
			return goodFinals.find((char)((int)a - i))->second; //vezi
		if ((int)a + i <= (int)'z' && goodFinals.find((char)((int)a + i)) != goodFinals.end())
			return goodFinals.find((char)((int)a + i))->second;
	}

}

string process (string cuv) {
	int isStartBig = 0;
	string rez = "";
	if ('A' <= cuv[0] && cuv[0] <= 'Z') {
		isStartBig = 1;
		cuv[0] = (char)(int(cuv[0]) - int('A' - 'a'));
	}

	cuv[0] = mapping[cuv[0]];
	char letter = cuv[0];
	rez = rez + cuv[0];

	int SUPERPOWER = 0;

	for (int i = 1; i < cuv.size(); ++i) { //oare =?
		if (cuv[i] == '-') {
            SUPERPOWER = 1; //vezi superpower
		}
		else {
            if(SUPERPOWER && goodLetters.find(cuv[i]) != goodLetters.end())
                cuv[i] = letter;
			rez = rez + cuv[i];
		}
	}

	if (goodLetters.find(cuv[cuv.size() - 1]) != goodLetters.end())
		rez += get_nearest_final(cuv[cuv.size() - 1]); //maybe timp
	if (isStartBig)
		rez[0] = (char)(int(rez[0]) + int('A' - 'a'));
	return rez;
}

int main() {
	string cuv; // fii atent la uppercase first letter of the word
	goodLetters.insert('b');
	goodLetters.insert('c');
	goodLetters.insert('d');
	goodLetters.insert('g');
	goodLetters.insert('k');
	goodLetters.insert('n');
	goodLetters.insert('p');
	goodLetters.insert('t');

	goodLettersB.insert('B');
	goodLettersB.insert('C');
	goodLettersB.insert('G');
	goodLettersB.insert('K');
	goodLettersB.insert('N');
	goodLettersB.insert('P');
	goodLettersB.insert('T');

	goodFinals['a'] = "ah";
	goodFinals['o'] = "oh";
	goodFinals['u'] = "uh";

	for (int i = (int)'a'; i <= (int)'z'; ++i)
		mapping[(char)i] = get_nearest_small((char)i);
	for (int i = (int)'A'; i <= (int)'Z'; ++i)
		mapping[(char)i] = get_nearest_small((char)i);

	while (cin >> cuv)
		cout << process(cuv) << ' ';

	cout<<"\n";
	return 0;


}
