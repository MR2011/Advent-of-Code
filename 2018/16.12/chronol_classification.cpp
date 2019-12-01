#include <iostream>
#include <set>
#include <fstream>
#include <vector>
#include <map>

using namespace std;
int OP=0, A=1, B=2, C=3;

struct Observation{
	vector<int> br;
	vector<int> ar;
	vector<int> instruction;
	Observation(vector<int> br, vector<int> instruction, vector<int> ar) : 
			    br(br), ar(ar), instruction(instruction){}
};

vector<Observation> load_observations(string filename){
	vector<Observation> observations;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			if(line.find("Before") == 0){
				int bef0, bef1, bef2, bef3;
				sscanf(line.c_str(), "Before: [%d, %d, %d, %d]", &bef0, &bef1, &bef2, &bef3);
				
				int op, a, b, c;
				getline(file, line);
				sscanf(line.c_str(), "%d %d %d %d", &op, &a, &b, &c);

				int af0, af1, af2, af3;
				getline(file, line);
				sscanf(line.c_str(), "After:  [%d, %d, %d, %d]", &af0, &af1, &af2, &af3);

				Observation observation = Observation({bef0, bef1, bef2, bef3}, {op, a, b, c}, {af0, af1, af2, af3});
				observations.push_back(observation);
			}
		}
		file.close();
	}
	return observations;
}

vector<vector<int>> load_instructions(string filename){
	vector<vector<int>> instructions;
	string line;
	ifstream file(filename);
	if(file.is_open()){
		while(getline(file, line)){
			int op, a, b, c;
			sscanf(line.c_str(), "%d %d %d %d", &op, &a, &b, &c);
			instructions.push_back({op, a, b, c});
		}
		file.close();
	}
	return instructions;

}

int execute_opcode(int op, vector<int> b, vector<int> i){
	switch(op){
		case 0:	return (b[i[A]] + b[i[B]]); //addr
		case 1:	return (b[i[A]] + i[B]);	 //addi
		case 2:	return (b[i[A]] * b[i[B]]); //mulr
		case 3: return (b[i[A]] * i[B]) ;	 //muli
		case 4: return (b[i[A]] & b[i[B]]); //banr
		case 5: return (b[i[A]] & i[B]);	 //bani
		case 6: return (b[i[A]] | b[i[B]]); //borr
		case 7: return (b[i[A]] | i[B]);	 //bori
		case 8: return b[i[A]]; 			 //setr
		case 9: return i[A];				 //seti
		case 10: return (i[A] > b[i[B]]) ?  1 :  0; //gtir
		case 11: return (b[i[A]] > i[B]) ?  1 :  0; //gtri
		case 12: return (b[i[A]] > b[i[B]]) ? 1 : 0; //gtrr
		case 13: return (i[A] == b[i[B]]) ? 1 : 0; //eqir
		case 14: return (b[i[A]] == i[B]) ? 1 : 0; //eqri
		case 15: return (b[i[A]] == b[i[B]]) ? 1 : 0; //eqrr
	}
	return 0;
}

vector<set<int>> part1(vector<Observation> observations){
	int total = 0;
	vector<set<int>> opcodes(16);
	for(Observation o: observations){
		int matches = 0;
		vector<int> op_matches;
		for(int i=0; i < 16; i++){
			if(execute_opcode(i, o.br, o.instruction) == o.ar[o.instruction[C]]){
				op_matches.push_back(i);
				opcodes[o.instruction[OP]].insert(i);
			}
		}
		if(op_matches.size() > 2)
			total++;
	}
	cout << "Part 1: " << total << endl;
	return opcodes;
}

map<int, int> find_mapping(vector<set<int>> opcodes){
	map<int, int> real_opcodes;
	while(real_opcodes.size() < 16){
		for(int i=0; i < opcodes.size(); i++){
			if(opcodes[i].size() == 1){
				real_opcodes[i] = *opcodes[i].begin();
				for(int j=0; j < opcodes.size(); j++){
					if(i != j)
						opcodes[j].erase(*opcodes[i].begin());
				}
			}
		}
	}
	return real_opcodes;
}

void part2(map<int, int> mapping, vector<vector<int>> instructions){
	vector<int> registers(4, 0);
	for(int i=0; i < instructions.size(); i++){
		int op = mapping[instructions[i][OP]];
		int result = execute_opcode(op, registers, instructions[i]);
		registers[instructions[i][C]] = result;
	}
	cout << "Part 2: R[0]: " << registers[0] << endl;
}

int main(){
	vector<Observation> observations = load_observations("input1.txt");	
	vector<set<int>> opcodes = part1(observations);
	vector<vector<int>> instructions = load_instructions("input2.txt");
	map<int, int> mapping = find_mapping(opcodes);
	part2(mapping, instructions);
}
