#include <string>
#include <numeric>
#include <tuple>
#include <fstream>
#include <iostream>
#include <vector>
#include <deque>
#include <regex>
#include <cmath>

using namespace std;

const string INIT_REGEX = R"(initial state: ([#|.]+))";
const string RULE_REGEX = R"(([#|.]+)\s=>\s([#|.]))";

const int OVERHEAD = 100;

int parse_rule(string rule){
	int value = 0;
	for(int i=0; i < rule.size(); i++){
		if(rule[i] == '#'){
			value += pow(2, i);
		}
	}
	return value;
}

deque<int> parse_plants(string plants_string){
	deque<int> plants;
	for(char c : plants_string){
		if(c == '#'){
			plants.push_back(1);
		}else if(c == '.'){
			plants.push_back(0);
		}
	}
	for(int i=0; i < OVERHEAD; i++){
		plants.push_back(0);
		plants.push_front(0);
	}
	return plants;
}

tuple<deque<int>, vector<int>> load_input(string filename){
	vector<int> rules(32, 0);
	deque<int> plants;
	string line;
	smatch m;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			if(regex_match(line, m, regex(INIT_REGEX))){
				plants = parse_plants(line);
			}else if(regex_match(line, m, regex(RULE_REGEX))){
				int index = parse_rule(m[1]);
				int value = parse_rule(m[2]);
				rules[index] = value;
			}
		}
	}
	return make_tuple(plants, rules);
}

deque<int> update(deque<int> plants, vector<int> rules){
	deque<int> new_plants;
	for(int i=0; i < plants.size(); i++){
		int sum = 0;
		for(int j=0; j < 5; j++){
			int index = i - 2 + j;
			if(index >= 0 && index < plants.size() && plants[index] == 1){
				sum += pow(2, j);
			}
		}
		new_plants.push_back(rules[sum]);
	}
	return new_plants;
}

int sum_plants(deque<int> plants){
	int sum = 0;
	for(int i=0; i < plants.size(); i++){
		if(i < OVERHEAD && plants[i] == 1){
			sum -= (OVERHEAD - i - 1);
		}else if(plants[i] == 1){
			sum += i - OVERHEAD;
		}
	}
	return sum;
}
int main(){
	auto input  = load_input("input.txt");
	deque<int> plants = get<0>(input);
	vector<int> rules = get<1>(input);
	long sum = 0; 
	long last_sum = sum_plants(plants);
	int delta = last_sum;
	for(long i=0; i < 20; i++){
		plants = update(plants, rules);
		sum = sum_plants(plants);
		if(sum - last_sum == delta){
			sum = sum + delta * (50000000000 - i - 1);
			break;
		}else{
			delta = sum - last_sum;
		}
		delta = sum - last_sum;
		last_sum = sum;
	}
	cout << "Sum: " << sum << endl;
}
