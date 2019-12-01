#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

struct Claim {
	int id;
	int l; //left
	int t; //top
	int w; //width
	int h; //height
};

vector<string> load_claims(string filename){
	vector<string> claims;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line))
			claims.push_back(line);
	}
	return claims;
}

Claim insert_rectangle(string s, vector<vector<int>> &rectangles){
	Claim claim;
	sscanf(s.c_str(), "#%d @ %d,%d: %dx%d\n", &claim.id, &claim.l, &claim.t, &claim.w, &claim.h);
	for(int i=claim.t; i < (claim.t+claim.h); ++i){
		for(int j=claim.l; j < (claim.l+claim.w); ++j){
			rectangles[i][j]++;
		}
	}
	return claim;
}

int count_overlaps(vector<vector<int>> &rectangles){
	int overlaps = 0;
	for(vector<int> row: rectangles)
		overlaps += count_if(row.begin(), row.end(), [](int i){return i > 1;});
	return overlaps;
}

bool is_alone(const Claim claim, vector<vector<int>> &rectangles){
	for(int i=claim.t; i < (claim.t+claim.h); ++i){
		for(int j=claim.l; j < (claim.l+claim.w); ++j){
			if(rectangles[i][j] != 1)
				return false;
		}
	}
	return true;
}

int find_alone(const vector<Claim> &claims, vector<vector<int>> &rectangles){
	for(Claim claim: claims){
		if(is_alone(claim, rectangles))
			return claim.id;	
	}
	return -1;
}

int main(){
	vector<string> claim_strings = load_claims("input.txt");
	vector<vector<int>> rectangles(1024, vector<int>(1024,0));
	vector<Claim> claims;
	for(string claim_string: claim_strings){
		Claim claim = insert_rectangle(claim_string, rectangles);
		claims.push_back(claim);		
	}
	cout << "Overlaps: " << count_overlaps(rectangles) << endl;
	cout << "Alone: " << find_alone(claims, rectangles) << endl;
}
