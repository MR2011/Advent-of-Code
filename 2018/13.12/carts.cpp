#include <vector>
#include <algorithm>
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

struct Cart{
	int x;
	int y;
	int direction; // left = 0, right = 1, up = 2, down = 3
	int turn = 0;
	int id;
	bool crashed = false;
	Cart(int x, int y, int direction, int id) : x(x), y(y), direction(direction), id(id){}
	bool operator < (const Cart& cart){
		return make_pair(y, x) < make_pair(cart.y, cart.x);
	}
	bool move(vector<string> &grid, vector<Cart> &carts){
		if(direction == 0){
			x--;
		}else if(direction == 1){
			x++;
		}else if(direction == 2){
			y--;
		}else if(direction == 3){
			y++;
		}
		if(grid[y][x] == '/'){
			if(direction == 2){
				direction = 1;
			}else if(direction == 0){
				direction = 3;
			}else if(direction == 1){
				direction = 2;
			}else if(direction == 3){
				direction = 0;
			}
		}else if(grid[y][x] == '\\'){
			if(direction == 2){
				direction = 0;
			}else if(direction == 1){
				direction = 3;
			}else if(direction == 3){
				direction = 1;
			}else if(direction == 0){
				direction = 2;
			}
		}else if(grid[y][x] == '+'){
			if(turn == 0 && direction == 0 || turn == 2 && direction == 1){
				direction = 3;
			}else if(turn == 0 && direction == 1 || turn == 2 && direction == 0){
				direction = 2;
			}else if(turn == 0 && direction == 2 || turn == 2 && direction == 3){
				direction = 0;
			}else if(turn == 0 && direction == 3 || turn == 2 && direction == 2){
				direction = 1;
			}
			turn = ++turn % 3;
		}
		for(int i=0; i < carts.size(); i++){
			if(carts[i].x == x && carts[i].y == y && id != carts[i].id && !carts[i].crashed){
				crashed = true;
				carts[i].crashed = true;
			}
		}
		return crashed;
	}
};

vector<string> load_grid(string filename){
	vector<string> grid;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			grid.push_back(line);
		}
		file.close();
	}
	return grid;
}

vector<Cart> load_carts(vector<string> &grid){
	vector<Cart> carts;
	for(int row=0; row < grid.size(); row++){
		for(int col=0; col < grid[row].size(); col++){
			if(grid[row][col] == '<'){
				carts.push_back(Cart(col, row, 0, carts.size()));
				grid[row][col] = '-';
			}else if(grid[row][col] == '>'){
				carts.push_back(Cart(col, row, 1, carts.size()));
				grid[row][col] = '-';
			}else if(grid[row][col] == '^'){
				carts.push_back(Cart(col, row, 2, carts.size()));
				grid[row][col] = '|';
			}else if(grid[row][col] == 'v'){
				carts.push_back(Cart(col, row, 3, carts.size()));
				grid[row][col] = '|';
			}
		}
	}
	return carts;
}

void print_last_cart(vector<Cart> carts){
	for(Cart c : carts){
		if(!c.crashed){
			cout << c.x << "," << c.y << endl;
		}
	}
}

int main(){
	vector<string> grid = load_grid("input.txt");
	vector<Cart> carts = load_carts(grid);
	int crashes = 0;
	while(true){
		sort(carts.begin(), carts.end());
		for(int i=0; i < carts.size(); i++){
			if(!carts[i].crashed && carts[i].move(grid, carts)){
				cout << "Crash (X,Y)=(" << carts[i].x << "," << carts[i].y << ")" << endl;
				crashes += 2;
			}
			
		}
		if(crashes == carts.size()-1){
			print_last_cart(carts);
			return 0;
		}

	}
}
