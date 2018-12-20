#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <stack>
#include <map>

using namespace std;
map<char, pair<int, int>> COORDS;
const string DIRECTIONS = "NSWE";

struct Room{
	int x;
	int y;
	int dist = 0;
	Room(int x, int y) : x(x), y(y) {}
	bool operator < (const Room &r) const {
		return make_pair(x, y) < make_pair(r.x, r.y);
	}
	bool operator == (const Room &r){
		return x == r.x && y == r.y;
	}
};

void update(Room &current, vector<Room> &rooms, int &max_distance){
	current.dist++;
	auto it = find(rooms.begin(), rooms.end(), current);
	if(it != rooms.end()){
		if(it->dist == 0)
			it->dist = current.dist;
		else if( current.dist < it->dist)
			it->dist = current.dist;
		else if(it->dist < current.dist){
			current.dist = it->dist;
		}
	}else{
		rooms.push_back(current);
	}
	max_distance = max(max_distance, current.dist);
}

void parse_regex(string regex){
	Room current(0, 0);
	vector<Room> rooms;
	stack<Room> branches;
	rooms.push_back(current);
	int max_distance = 0;
	for(char c : regex){
		if(DIRECTIONS.find(c) != string::npos){
			current.y += COORDS[c].second;
			current.x += COORDS[c].first;
			update(current, rooms, max_distance);	
		}else if(c == '('){
			branches.push(current);
		}else if(c == ')'){
			current = branches.top();
			branches.pop();
		}else if(c == '|'){
			current = branches.top();
		}
	}
	cout << "Part 1: " << max_distance << endl;
	int thousand = 0;
	for(Room r : rooms){
		if(r.dist >= 1000)
			thousand++;
	}
	cout << "Part 2: " << thousand << endl;
}

string load_regex(string filename){
	string regex;
	ifstream file(filename);
	
	if(file.is_open()){
		getline(file, regex);
	}
	return regex;
}

int main(){
	COORDS['N'] = make_pair(0, 1);
	COORDS['S'] = make_pair(0, -1);
	COORDS['W'] = make_pair(-1, 0);
	COORDS['E'] = make_pair(1, 0);

	string regex = load_regex("input.txt");
	parse_regex(regex);
}
