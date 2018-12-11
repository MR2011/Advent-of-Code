#include <iostream>
#include <vector>

using namespace std;

const int SIZE = 300;
const int SERIAL = 3214;

struct MaxPos{
	int x;
	int y;
	int power;
};

int get_power_lvl(int x, int y){
	int rack_id =  x + 10;
	int power_lvl = (rack_id * y + SERIAL) * rack_id;
	power_lvl = (power_lvl / 100) % 10;
	power_lvl -= 5;
	return power_lvl;
}

vector<vector<int>> calculate_power_lvls(){
	vector<vector<int>> grid(SIZE, vector<int>(SIZE, 0));
	for(int x=1; x <= SIZE; x++){
		for(int y=1; y <= SIZE; y++){
			grid[y-1][x-1] = get_power_lvl(x, y);
		}
	}
	return grid;
}

MaxPos scan_squares(vector<vector<int>> grid, int square_size){
	MaxPos max = {0, 0, 0};
	for(int x=0; x <= SIZE - square_size; x++){
		for(int y=0; y <= SIZE - square_size; y++){
			int sum = 0;
			for(int i=0; i < square_size; i++){
				for(int j=0; j < square_size; j++){
					sum += grid[y+i][x+j];
				}
			}
			if(sum > max.power)
				max = {x+1, y+1, sum};
		}
	}
	return max;
}

void part1(vector<vector<int>> grid){
	MaxPos max = scan_squares(grid, 3);
	cout << "(" << max.x << "," << max.y << ",3)" << endl;

}

void part2(vector<vector<int>> grid){
	MaxPos total_max = {0, 0, 0};
	for(int square_size=0; square_size < 300; square_size++){
		MaxPos max = scan_squares(grid, square_size);
		if(max.power > total_max.power){
			total_max = max;
			cout << "(" << max.x << "," << max.y << "," << square_size << ")" << endl;
		}
	}
}

int main(){
	vector<vector<int>> grid = calculate_power_lvls();	
	cout << "PART 1" << endl;
	part1(grid);
	cout << "PART 2" << endl;
	part2(grid);
};


