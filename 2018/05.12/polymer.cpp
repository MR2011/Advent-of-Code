#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

string load_polymer(string filename){
	string polymer;
	ifstream file(filename);
	if(file.is_open())
		getline(file, polymer);
	return polymer;
}

int react(string &polymer){
	string units = "";

	for(char c: polymer){
		if(units.empty())
			units.push_back(c);
		else{
			if(abs(c - units.back()) == 32)
				units.pop_back();
			else
				units.push_back(c);
		}
	}
	return units.size();
}

int find_shortest_polymer(string polymer){
	string alphabet = "abcdefghijklmnopqrstuvwxyz";
	vector<int> sizes;
	for(char c : alphabet){
		string reduced = polymer;
		reduced.erase(remove(reduced.begin(), reduced.end(), c), reduced.end());
		reduced.erase(remove(reduced.begin(), reduced.end(), c-32), reduced.end());
		sizes.push_back(react(reduced));
	}
	return *min_element(sizes.begin(), sizes.end());

}

int main(){
	string polymer = load_polymer("input.txt");
	cout << "Size: " << react(polymer) << endl;
	cout << "Min size: " << find_shortest_polymer(polymer) << endl; 
}
