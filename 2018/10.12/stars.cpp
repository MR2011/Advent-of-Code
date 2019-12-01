#include <regex>
#include <vector>
#include <iostream>
#include <fstream>
#include <limits>
using namespace std;

struct Point{
	int x;
	int y;
	Point(int x, int y): x(x), y(y){}; 
};

struct Star{
	Point position;
	Point velocity;
};

typedef vector<vector<char>> Sky;
string REGEX = R"(position=<\s*(-?\d+),\s*(-?\d+)>\svelocity=<\s*(-?\d+),\s*(-?\d+)>)";

vector<Star> load_stars(string filename){
	vector<Star> stars;
	string line;
	smatch m;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			if(regex_match(line, m, regex(REGEX))){
				Star star = {Point(stoi(m[1]), stoi(m[2])), Point(stoi(m[3]), stoi(m[4]))};
				stars.push_back(star);
			}
		}
		file.close();
	}
	return stars;
}

void print_sky(Sky &sky){
	for(vector<char> row : sky){
		for(char col : row){
			cout << col;
		}
		cout << endl;
	}
	cout << endl;
}
int calculate_boundary(vector<Star> stars, int step){
	int min_x = numeric_limits<int>::max();
	int min_y = numeric_limits<int>::max();

	int max_x = 0;
	int max_y = 0;
	for(int i=0; i < stars.size(); i++){
		stars[i].position.x += (step * stars[i].velocity.x);
		stars[i].position.y += (step * stars[i].velocity.y);
		min_x = min(min_x, stars[i].position.x);
		min_y = min(min_y, stars[i].position.y);
		max_x = max(max_x, stars[i].position.x);
		max_y = max(max_y, stars[i].position.y);
	}
	int x_size = max_x - min_x + 1;
	int y_size = max_y - min_y + 1;
	return x_size + y_size;

}
void create_sky(vector<Star> stars, int step){
	int points = 0;
	int min_x = numeric_limits<int>::max();
	int min_y = numeric_limits<int>::max();
	int max_x = 0;
	int max_y = 0;
	for(int i=0; i < stars.size(); i++){
		stars[i].position.x += (step * stars[i].velocity.x);
		stars[i].position.y += (step * stars[i].velocity.y);
		min_x = min(min_x, stars[i].position.x);
		min_y = min(min_y, stars[i].position.y);
		max_x = max(max_x, stars[i].position.x);
		max_y = max(max_y, stars[i].position.y);
	}
	int x_size = max_x - min_x + 1;
	int y_size = max_y - min_y + 1;
	Sky sky(y_size, vector<char>(x_size, '.'));
	for(int i=0; i < stars.size(); i++){
		stars[i].position.x -= min_x;
		stars[i].position.y -= min_y;
		sky[stars[i].position.y][stars[i].position.x] = '#';
	}	
		print_sky(sky);
}


int main(){
	vector<Star> stars = load_stars("input.txt");
	int last_boundary = calculate_boundary(stars, 0);
	int index;
	for(int i=1; i > 0; i++){
		int boundary = calculate_boundary(stars, i);
		if(boundary > last_boundary){
			index = --i;
			cout << "Index: " << index << endl;
			break;
		}
		last_boundary = boundary;
	}
	create_sky(stars, index);
}
