#include <vector>
#include <algorithm>
#include <string>
#include <iostream>

using namespace std;

int main(){
	vector<int> scores = {3, 7};
	long first = 0, second = 1;
	int sum;
	int recipes = 47801;
	vector<int> pattern = {0,4,7,8,0,1};
	while(true){
		sum = scores[first] + scores[second];
		if(sum > 9){
			scores.push_back(sum / 10);
			scores.push_back(sum % 10);
		}else{
			scores.push_back(sum);
		}
		first = (first + 1 + scores[first]) % scores.size();
		second =( second + 1 + scores[second]) % scores.size();
		auto res = mismatch(pattern.rbegin(), pattern.rend(), scores.rbegin() + 1, scores.rend());
		if(res.first == pattern.rend()){ 
			cout << "Part 2: " << scores.size() - pattern.size() << endl;
			break;
		}
	}
	cout << "Part 1: ";
	for(int i=recipes; i < recipes + 10; i++){
		cout << scores[i];
	}
	cout << endl;
}
