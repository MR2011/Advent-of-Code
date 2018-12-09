#include <list>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

const int PLAYERS = 438;
const int LAST_MARBLE_POINTS = 71626 * 100;

void print_list(list<int> l){
	for(auto i=l.begin(); i != l.end(); i++){
		cout << *i << " ";
	}
	cout << endl;
}

list<int>::iterator next(list<int>::iterator &it, list<int> &l){
	if(++it == l.end())
		it = l.begin();
	return ++it;
}

list<int>::iterator previous(list<int>::iterator &it, list<int> &l){
	for(int i=0; i < 6; i++){
		if(it == l.begin())
			it = l.end();
		it--;
	}
	return --it;
}

int main(){
	list<int> circle = {0};
	vector<long> scores(PLAYERS, 0);
	auto current(circle.begin());
	for(int i=1; i <= LAST_MARBLE_POINTS; i++){
		if(i % 23 == 0){
			auto to_remove = previous(current, circle);
			scores[(i-1) % PLAYERS] += *to_remove + i;
			current = to_remove;
			current++;
			circle.erase(to_remove);
		}else{
			current = circle.insert(next(current, circle), i);
		}
	}
	cout << "Score: " << *max_element(scores.begin(), scores.end()) << endl;
}
