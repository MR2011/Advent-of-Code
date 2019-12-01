#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

vector<string> load_ids(string filename){
	vector<string> ids;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line))
			ids.push_back(line);
	}
	return ids;
}

vector<int> get_frequencies(string id){
	vector<int> frequencies(256, 0);
	for(char c : id)
		frequencies[int(c)]++;
	return frequencies;
}

int calc_checksum(const vector<string> &ids){
	int twice = 0, triple = 0;
	for(string id: ids){
		vector<int> frequencies = get_frequencies(id);
		if(any_of(frequencies.begin(), frequencies.end(), [](int c){ return c==2;}))
			twice++;
		if(any_of(frequencies.begin(), frequencies.end(), [](int c){ return c==3;}))
			triple++;
	}
	return twice * triple;
}

bool similar(string a, string b){
	int differences = 0;
	for(int i=0; i < a.size(); ++i){
		if(a[i] != b[i] && ++differences > 1)
			false;
	}
	return differences == 1;
}

void find_common_letters(const vector<string> &ids){
	for(int i=0; i < ids.size() - 1; ++i){
		for(int j=i+1; j < ids.size(); ++j){
			if(similar(ids[i], ids[j])){
				cout << ids[i] << endl;
				cout << ids[j] << endl;
			}
		}
	}
}

int main(){
	vector<string> ids = load_ids("input.txt");
	int checksum = calc_checksum(ids);
	cout << "Checksum: " << checksum << endl;
	find_common_letters(ids);
}
