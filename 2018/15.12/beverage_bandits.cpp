#include <algorithm>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>

using namespace std;

struct Unit{
	int x;
	int y;
	char type;
	Unit(int x, int y, char type) : x(x), y(y), type(type) {}
	bool operator < (const Unit &unit){
		return make_pair(y, x) < make_pair(unit.y, unit.x);
	}
};

vector<string> load_grid(string filename){
	vector<string> grid;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line))
			grid.push_back(line);
	}
	return grid;
}

vector<Unit> extract_units(vector<string> &grid){
	vector<Unit> units;
	for(int row=0; row < grid.size(); row++){
		for(int col=0; col < grid[row].size(); col++){
			if(grid[row][col] == 'G' || grid[row][col] == 'E'){
				units.push_back(Unit(col, row, grid[row][col]));
				grid[row][col] = '.';
			}
		}
	}
	return units;
}

vector<pair<int, int>> 
find_ranges(vector<string> &grid, vector<Unit> units, char type){
	vector<pair<int, int>> ranges;
	for(int row=0; row < grid.size(); row++){
		for(int col=0; col < grid[row].size(); col++){
			if(grid[row][col] == 'G' && type == 'E' ||
			   grid[row][col] == 'E' && type == 'G'){
				if(grid[row+1][col] == '.')
					ranges.push_back(make_pair(col, row));
				if(grid[row-1][col] == '.')
					ranges.push_back(make_pair(col, row));
				if(grid[row][col+1] == '.')
					ranges.push_back(make_pair(col, row));
				if(grid[row][col-1] == '.')
					ranges.push_back(make_pair(col, row));
			}
		}
	}
	return ranges;
}


void remove_unit_nodes(const vector<Unit> &units, vector<pair<int, int>> &nodes){
	for(Unit unit : units){
		for(int i=0; i < nodes.size(); i++){
			if(unit.x == nodes[i].first && unit.y == nodes[i].second){
				nodes.erase(nodes.begin() + i);
				break;
			}
		}
	}
	return nodes;
}

vector<pair<int, int> 
find_nodes(vector<string> grid, vector<Unit> units){
	vector<pair<int, int>> nodes;
	for(int row=1; row < grid.size() - 1; row++){
		for(int col=1; col < grid[row].size() - 1; col++){
			if(grid[row][col] == '.')
				nodes.push_back(make_pair(col, row));
		}
	}
	remove_unit_nodes(units, nodes);
	return nodes;
}

void find_shortest_path(pair<int, int> start, pair<int, int>, vector<string> &grid, vector<Unit> units){
	int total_costs = 0;


}

int main(){
	vector<string> grid = load_grid("input_movement.txt");
	vector<Unit> units = extract_units(grid);
	while(true){
		sort(units.begin(), units.end());
			
	}
}
