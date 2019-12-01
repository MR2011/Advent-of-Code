#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <numeric>

int meta_sum = 0;
using namespace std;

struct Node{
	vector<int> metadata ;
	vector<Node> children;
	int value = 0;
};

Node create_node(ifstream &input){
	int num_children, num_metadata;
	input >> num_children; 
	input >> num_metadata;
	Node node;
	for(int i=0; i < num_children; i++){
		node.children.push_back(create_node(input));
	}
	for(int i=0; i < num_metadata; i++){
		int metadata;
		input >> metadata;
		meta_sum += metadata;
		node.metadata.push_back(metadata);
	}
	node.value = 0;
	if(num_children == 0){
		node.value = accumulate(node.metadata.begin(), node.metadata.end(), 0); 
	}else{
		for(int m : node.metadata){
			if(node.children.size() > (m-1)){
				node.value += node.children[m-1].value;
			}
		}
	}
	return node;

}
void build_tree(string filename){
	string line;
	ifstream input(filename);
	Node root = create_node(input);
	cout << meta_sum << endl;
	cout << root.value << endl;
}

int main(){
	build_tree("input.txt");
}
