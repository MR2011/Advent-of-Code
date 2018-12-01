#include <iostream>
#include <fstream>
#include <string>
#include <numeric>
#include <unordered_set>
#include <vector>

using namespace std;

vector<int> load_changes(string filename){
	vector<int> changes;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			changes.push_back(stoi(line));
		}
		file.close();
	}
	return changes;
}

void part1(const vector<int> &changes){
	int sum = accumulate(changes.begin(), changes.end(), 0);
	cout << "Frequency: " << sum << endl;
}

void part2(const vector<int> &changes){
	unordered_set<int> frequencies;
	int i = 0, sum = 0;
	while(frequencies.insert(sum).second){
			sum += changes[i++ % changes.size()];
	}
	cout << "First frequency reached twice: " << sum << endl;
}

int main(){
	vector<int> changes = load_changes("input.txt");
	part1(changes);
	part2(changes);
}
