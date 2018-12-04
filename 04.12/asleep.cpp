#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <string>
#include <unordered_map>
#include <regex>
#include <array>
#include <numeric>

using namespace std;

typedef unordered_map<int, array<int,60>> Sleepmap;

vector<string> load_logs(string filename){
	vector<string> logs;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line))
			logs.push_back(line);
	}
	sort(logs.begin(), logs.end());
	return logs;
}

void print(int id, Sleepmap &sleep){
	int* value = max_element(sleep[id].begin(), sleep[id].end());
	int index = distance(sleep[id].begin(), value);
	cout << "Id: " << id << " Minute: " << index << " => " << id * index << endl;
}

void locate_ids(Sleepmap sleep){
	int id1 = -1, id2 = -1, max1 = -1, max2 = -1;
	for(auto it : sleep){
		int sum = accumulate(it.second.begin(), it.second.end(), 0);
		int* max = max_element(it.second.begin(), it.second.end());
		if(*max > max2){
			max2 = *max;
			id2 = it.first;
		}
		if(sum > max1){
			id1 = it.first;
			max1 = sum;
		}
	}
	print(id1, sleep);
	print(id2, sleep);
}

Sleepmap fill_sleepmap(const vector<string> &logs){
	int id, start;
	smatch m;
	Sleepmap sleep;
	for(string entry: logs){
		if(regex_match(entry, m, regex(R"(.*Guard #(\d+) begins shift)"))){
			id = stoi(m[1]);
		}else if(regex_match(entry, m, regex(R"(.*(\d\d):(\d\d).*wakes up)"))){
			for(int i=start; i < stoi(m[2]); ++i){
				sleep[id][i]++;
			}
		}else if(regex_match(entry, m, regex(R"(.*(\d\d):(\d\d).*falls asleep)"))){
			start = stoi(m[2]);
		}
	}
	return sleep;
}

int main(){
	vector<string> logs = load_logs("input.txt");
	Sleepmap sleep = fill_sleepmap(logs);
	locate_ids(sleep);
}
