#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int OFFSET = 400;
int X_START = 1000;
int X_END = 0;
int Y_END = 0;
int Y_START = 1000;
void add_clay(int x_start, int x_end, int y_start, int y_end, vector<vector<char>> &grid){
	for(int row=y_start; row <= y_end; row++){
		for(int col=x_start; col <= x_end; col++){
			grid[row][col] = '#';
		}
	}
}

vector<vector<char>> create_grid(string filename){
	string line;
	ifstream file(filename);
	int x1, x2, y1, y2;
	vector<vector<char>> grid(5000, vector<char>(5000, '.'));
	grid[0][500] = '+';
	if(file.is_open()){
		while(getline(file, line)){
			if(sscanf(line.c_str(), "x=%d, y=%d..%d", &x1, &y1, &y2) == 3){
				add_clay(x1, x1, y1, y2, grid);
				X_START = min(X_START, x1);
				X_END = max(X_END, x1);
				Y_END = max(Y_END, y2);
				Y_START = min(Y_START, y1);
			}else if(sscanf(line.c_str(), "y=%d, x=%d..%d", &y1, &x1, &x2) == 3){
				add_clay(x1, x2, y1, y1, grid);
				X_START = min(X_START, x1);
				X_END = max(X_END, x2);
				Y_END = max(Y_END, y1);
				Y_START = min(Y_START, y1);
			}
		}
		file.close();
	}
	return grid;
}

void print_grid(vector<vector<char>> grid){
	for(int row=0; row < grid.size(); row++){
		if(row <= Y_END){
			for(int col=0; col < grid[row].size(); col++){
				if(col >= X_START - 1 && col <= X_END + 1)
					cout << grid[row][col];
			}	
			cout << endl;
		}
	}
}


int flow2(vector<vector<char>> &grid, int x, int y, int d){
	if(grid[y][x] == '.')
		grid[y][x] = '|';
	if(y > Y_END)
		return -2;
	if(grid[y][x] == '#')
		return x;
	if(grid[y+1][x] == '.')
		flow2(grid, x,  y+1, 0); // flow down
	if(grid[y+1][x] == '#' || grid[y+1][x] == '~'){ //flow sideway
		if(d == 1){
			return flow2(grid, x+d, y, d);
		}else{
			int leftX = flow2(grid, x-1, y, -1);
			int rightX = flow2(grid, x+1, y, 1);
			if(grid[y][leftX] == '#' && grid[y][rightX] == '#'){
				for(int i=leftX + 1; i < rightX; i++){
					grid[y][i] = '~';
				}
			}
		}
	}else{
		return x;
	}
}
void count_tiles(vector<vector<char>> grid){
	int flow = 0;
	int still = 0;
	for(int row=Y_START; row < grid.size(); row++){
		if(row > Y_END){
			cout << "Part1: " << flow + still << endl;
			cout << "Part2: " << still << endl;
			return;
		}	
		for(int col=0; col < grid[row].size(); col++){	
			if(grid[row][col] == '~')
				still++;
			if(grid[row][col] == '|')
				flow++;
		}
	}
}
int main(){
	vector<vector<char>> grid = create_grid("input.txt");
	/* vector<vector<char>> grid = create_grid("input_test.txt"); */
	/* fill(500, 1, grid); */
	flow2(grid, 500, 0, 0);
	count_tiles(grid);
	/* print_grid(grid); */
}
