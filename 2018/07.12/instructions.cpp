#include <string>
#include <set>
#include <vector>
#include <array>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

const int SIZE=26;

struct Vertex{
	int id;
	vector<Vertex*> previous;
	vector<Vertex*> next;
	int duration;
};

typedef array<Vertex, SIZE> Graph;

Graph generate_graph(string filename){
	Graph graph;
	for(int i=0; i < SIZE; i++){
		graph[i].id = i;
		graph[i].duration = 60 + i + 1;
	}
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			int first = int(line[5]) - 65;
			int second = int(line[36]) - 65;
			graph[first].next.push_back(&graph[second]);
			graph[second].previous.push_back(&graph[first]);
		}
	}
	return graph;
}

set<int> find_start_vertices(Graph &graph){
	set<int> todo;
	for(Vertex v : graph){
		if(v.previous.size() == 0 && v.next.size() != 0){
			todo.insert(v.id);
		}	
	}
	return todo;
}

bool is_rdy(Vertex v, set<int> visited){
	bool is_rdy = true;
	for(Vertex* p: v.previous){
		if(visited.find(p->id) == visited.end())
			is_rdy = false;
	}
	return is_rdy;
}

void find_order(Graph graph){
	string order = "";
	set<int> todo = find_start_vertices(graph); 
	set<int> visited;
	Vertex* current;
	while(todo.size() > 0){
		current = &graph[*todo.begin()];
		order += char(current->id + 65); 
		todo.erase(current->id);
		visited.insert(current->id);
		for(Vertex* v: current->next){
			if(visited.find(v->id) == visited.end() && is_rdy(*v, visited))
				todo.insert(v->id);
		}
	}
	cout << order << endl;
}

vector<int> decrement_duration(vector<int> &workers, Graph &graph){
	vector<int> to_delete;
	for(int worker: workers){
		graph[worker].duration--;
		if(graph[worker].duration == 0){
			to_delete.push_back(worker);
		}
	}
	return to_delete;
}

void remove_finished_workers(vector<int> finished, set<int> &visited, string &order, vector<int> &workers){
	for(int worker : finished){
		workers.erase(remove(workers.begin(), workers.end(), worker), workers.end());
		visited.insert(worker);
		order += char(worker + 65);
	}
}

void simulation(Graph graph){
	string order = "";
	int t = 0;
	set<int> todo = find_start_vertices(graph);
	set<int> visited;
	vector<int> workers;
	while(order.size() < SIZE){
		while(workers.size() < 5 && todo.size() > 0){
			workers.push_back(*todo.begin());
			todo.erase(todo.begin());
		}
		vector<int> finished = decrement_duration(workers, graph);	
		remove_finished_workers(finished, visited, order, workers);
		if(workers.size() < 5){
			for(int v=0; v < graph.size(); v++){
				if(is_rdy(graph[v], visited) && visited.find(v) == visited.end() && find(workers.begin(), workers.end(), v) == workers.end())
					todo.insert(v);
			}	
		}
		t++;
	}
	cout << t << endl;
}

int main(){
	Graph graph = generate_graph("input.txt");
	find_order(graph);
	simulation(graph);
}
