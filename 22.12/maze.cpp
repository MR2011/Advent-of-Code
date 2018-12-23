#include <vector>
#include <tuple>
#include <queue>
#include <set>
#include <iostream>

using namespace std;

vector<pair<int, int>> NEIGHBORS = {{0,-1}, {1,0}, {0, 1}, {-1, 0}};

int TORCH = 1;

int DEPTH = 6969;
int START_X = 0, START_Y = 0;
int TARGET_X = 9, TARGET_Y = 796;

int get_geologic_index(int x, int y, const vector<vector<int>> &maze){
	if(x == START_X && y == START_Y)
		return 0;
	else if(x == TARGET_X && y == TARGET_Y)
		return 0;
	else if(y == 0)
		return (x * 16807);
	else if(x == 0)
		return (y * 48271);
	else
		return (maze[y][x-1] * maze[y-1][x]);
}

int get_erosion_lvl(int geo_index){
	return (geo_index + DEPTH) % 20183;
}

int get_risk_lvl(vector<vector<int>> maze){
	int risk = 0;
	for(int row=0; row <= TARGET_Y; row++){
		for(int col=0; col <= TARGET_X; col++){
			risk += (maze[row][col] % 3);
		}
	}
	return risk;
}

struct Node{
	int x;
	int y;
	int dist;
	int item; 
	Node(int x, int y, int dist, int item) : x(x), y(y), dist(dist), item(item) {}
};

struct SetCompare{
	bool operator()(const Node &lhs, const Node &rhs) const {
		return make_tuple(lhs.y, lhs.x, lhs.item) < make_tuple(rhs.y, rhs.x, rhs.item);
	}
};

struct QueueCompare{
	bool operator()(const Node &lhs, const Node &rhs) const {
		return make_tuple(lhs.dist, lhs.x, lhs.y, lhs.item) > make_tuple(rhs.dist, rhs.x, rhs.y, rhs.item);
	}
};

void dijkstra(vector<vector<int>> maze){
	priority_queue<Node, vector<Node>, QueueCompare> q;
	set<Node, SetCompare> visited;
	Node start = Node(0, 0, 0, TORCH);
	q.push(start);
	while(!q.empty()){
		Node u = q.top();
		q.pop();
		if(u.x == TARGET_X && u.y == TARGET_Y && u.item == TORCH){
			cout << "Distance: " << u.dist << endl;
			break;
		}
		if(visited.count(u) == 1){
			continue;
		}
		visited.insert(u);
		for(int tool=0; tool < 3; tool++){
			if((maze[u.y][u.x] % 3) !=  tool){
				q.push(Node(u.x, u.y, u.dist + 7, tool));
			}
		}
		for(pair<int, int> p : NEIGHBORS){
			int row = u.y + p.second;
			int col = u.x + p.first;
			if( row >= 0 && col >= 0 && row < maze.size() && col < maze[row].size()){
				if((maze[row][col] % 3) !=  u.item){
					q.push(Node(col, row, u.dist + 1, u.item));
				}
			}
		}
	}
}

int main(){
	int rows = 1000;
	int cols = 1000;
	vector<vector<int>> maze(rows, vector<int>(cols, 0));
	for(int row=0; row < rows; row++){
		for(int col=0; col < cols; col++){
			int geo_index = get_geologic_index(col, row, maze);
			maze[row][col] = get_erosion_lvl(geo_index);
		}
	}
	cout << "Risk: " << get_risk_lvl(maze) << endl;
	dijkstra(maze);
}
