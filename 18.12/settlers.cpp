#include <string>
#include <sstream>
#include <algorithm>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>

using namespace std;

vector<string> load_area(string filename){
	vector<string> area;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			area.push_back(line);
		}
		file.close();
	}
	return area;
}

vector<int> scan_surroundings(vector<string> area, int row, int col){
	int grounds = 0, trees = 0, lumberyards = 0;
	for(int i=row-1; i <= row+1; i++){
		for(int j=col-1; j <= col+1; j++){
			if(i >= 0 && i < area.size() && j >= 0 && j < area[row].size() && !(row == i && col == j)){
				if(area[i][j] == '.')
					grounds++;
				if(area[i][j] == '|')
					trees++;
				if(area[i][j] == '#')
					lumberyards++;	
			}
		}
	}
	return {grounds, trees, lumberyards};
}

char determine_acre(vector<string> area, int row, int col, vector<int> surroundings){
	char acre;
	if(area[row][col] == '.'){
		acre = (surroundings[1] > 2) ? '|' : '.'; 
	}else if(area[row][col] == '|'){
		acre = (surroundings[2] > 2) ? '#' : '|';
	}else if(area[row][col] == '#'){
		acre = (surroundings[2] > 0 && surroundings[1] > 0) ? '#' : '.';
	}
	return acre;
}

vector<string> next(vector<string> area){
	vector<string> new_area;
	string new_line = "";
	for(int row=0; row < area.size(); row++){
		new_line = "";
		for(int col=0; col < area[row].size(); col++){
			vector<int> surroundings = scan_surroundings(area, row, col);	
			new_line += determine_acre(area, row, col, surroundings);
		}
		new_area.push_back(new_line);
	}
	return new_area;
}

void print_area(vector<string> area){
	for(string line : area){
		cout << line << endl;
	}
	cout << endl;
}

int calculate_resources(vector<string> area){
	int trees = 0, lumberyards = 0;
	for(string line : area){
		trees += count(line.begin(), line.end(), '|');
		lumberyards += count(line.begin(), line.end(), '#');
	}
	return trees * lumberyards;
}

string get_key(vector<string> area){
	stringstream id;
	for(string line : area){
		id << line;
	}
	return id.str();
}

int main(){
	vector<string> area = load_area("input.txt");
	map<string, int> areas;
	long minutes = 1000000000;
	for(long i=0; i < minutes; i++){
		area = next(area);
		string key = get_key(area);
		if(areas.count(key) == 1){
			int skip = (minutes - i) / (i-areas[key]);
			i += skip * (i - areas[key]);
		}else{
			areas[key] = i;
		}
		if(i == 9 || i == minutes-1)
			cout << "Resources: " << calculate_resources(area) << endl;
	}
}

