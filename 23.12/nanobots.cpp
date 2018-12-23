#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <tuple>

using namespace std;

typedef tuple<long, long, long, long> Bot;

pair<vector<Bot>, Bot> load_nanobots(string filename){
	vector<Bot> bots;
	string line;
	ifstream file(filename);
	Bot max_bot = make_tuple(0, 0, 0, 0);
	if(file.is_open()){
		while(getline(file, line)){
			long x, y, z, r;
			sscanf(line.c_str(), "pos=<%ld, %ld, %ld>, r=%ld", &x, &y, &z, &r);
			Bot bot = make_tuple(x, y, z, r);
			bots.push_back(bot);
			if(r > get<3>(max_bot)){
				max_bot = bot;	
			}
		}
	}
	cout << "Size: " << bots.size() << endl;
	return make_pair(bots, max_bot);
}

bool in_range(Bot u, Bot v){
	long d = 0;
	d += abs(get<0>(u) - get<0>(v));
	d += abs(get<1>(u) - get<1>(v));
	d += abs(get<2>(u) - get<2>(v));
	if(get<3>(u) >= d || get<3>(v) >= d){
		return true;
	}else{
		return false;
	}
}

void count_range(vector<Bot> bots, Bot max_bot){
	long range = 0;
	for(int i=0; i < bots.size(); i++){
		if(in_range(bots[i], max_bot)){
			range++;
		}
	}
	cout << "In range: " << range << endl;
}

int main(){
	auto result = load_nanobots("input.txt");
	cout << "Max: " << get<3>(result.second) << endl;
	count_range(result.first, result.second);
}

