#include <string>
#include <algorithm>
#include <set>
#include <limits>
#include <array>
#include <vector>
#include <iostream>
#include <fstream>

using namespace std;

struct Point{
	int x;
	int y;
};
int SIZE = 500;

vector<Point> load_points(string filename){
	vector<Point> points;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			Point point;
			sscanf(line.c_str(), "%d,%d\n", &point.x, &point.y);
			points.push_back(point);
		}
	}	
	return points;
}

int distance(Point p, Point q){
	return abs(p.x - q.x) + abs(p.y - q.y);
}

vector<vector<int>> create_grid(const vector<Point> &points){
	vector<vector<int>> grid(SIZE, vector<int>(SIZE, 0));
	for(int x=0; x < SIZE; x++){
		for(int y=0; y < SIZE; y++){
			Point q = {x, y};
			int min_d = numeric_limits<int>::max();
			for(int i=0; i < points.size(); i++){
				int d = distance(q, points[i]);
				if(d == min_d){
					grid[y][x] = -1;
				}else if(d < min_d){
					grid[y][x] = i;
					min_d = d;
				}
			}
		}
	}
	return grid;
}

void filter_areas(vector<int> &areas, const set<int> &borders){
	for(int i=0; i < areas.size(); i++){
		if(borders.find(i) != borders.end()){
			areas[i] = 0;
		}
	}
}
vector<int> determine_areas(vector<vector<int>> grid){
	vector<int> areas(100, 0);
	set<int> borders;
	for(int x=0; x < SIZE; x++){
		for(int y=0; y < SIZE; y++){
			int value = grid[y][x];
			if(value != -1){
				areas[value]++;
				if(x==0 || x==SIZE-1 || y == 0 || y == SIZE-1)
					borders.insert(value);
			}
		}
	}	
	filter_areas(areas, borders);
	return areas;
}
int part2(vector<Point> points){
	int area = 0;
	for(int x=0; x < SIZE; x++){
		for(int y=0; y < SIZE; y++){
			Point q = {x, y};
			int total_d = 0;
			for(Point p : points){
				total_d += distance(p, q);
				if(total_d >= 10000)
					break;
			}
			if(total_d < 10000)
				area++;
		}
	}
	return area;
}
int main(){
	vector<Point> points = load_points("input.txt");
	vector<vector<int>> grid = create_grid(points);
	vector<int> areas = determine_areas(grid);
	cout << *max_element(areas.begin(), areas.end()) << endl;
	cout << part2(points) << endl;
}
