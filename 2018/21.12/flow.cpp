#include <vector>
#include <algorithm>
#include <utility>
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

struct Instruction{
	int a;
	int b;
	int c;
	string op;
	Instruction(int a, int b, int c, string op) : a(a), b(b), c(c), op(op) {}
};

pair<int, vector<Instruction>> load_instructions(string filename){
	vector<Instruction> instructions;
	string line;
	int ip;
	ifstream file(filename);
	if(file.is_open()){
		getline(file, line);
		sscanf(line.c_str(), "#ip %d", &ip);
		while(getline(file, line)){
			char op[4];
			int a, b, c;
			sscanf(line.c_str(), "%s %d %d %d", &op, &a, &b, &c);
			instructions.push_back(Instruction(a, b, c, op));
		}
		file.close();
	}
	return make_pair(ip, instructions);
}

void execute_instruction(Instruction i, vector<int> &r){
		if(i.op == "addr") r[i.c] = (r[i.a] + r[i.b]); //addr
		else if(i.op == "addi") r[i.c] = (r[i.a] + i.b);	 //addi
		else if(i.op == "mulr") r[i.c] = (r[i.a] * r[i.b]); //mulr
		else if(i.op == "muli") r[i.c] = (r[i.a] * i.b) ;	 //muli
		else if(i.op == "banr") r[i.c] = (r[i.a] & r[i.b]); //banr
		else if(i.op == "bani") r[i.c] = (r[i.a] & i.b);	 //bani
		else if(i.op == "borr") r[i.c] = (r[i.a] | r[i.b]); //borr
		else if(i.op == "bori") r[i.c] = (r[i.a] | i.b);	 //bori
		else if(i.op == "setr") r[i.c] = r[i.a]; 			 //setr
		else if(i.op == "seti") r[i.c] = i.a;				 //seti
		else if(i.op == "gtir") r[i.c] = (i.a > r[i.b]) ?  1 :  0; //gtir
		else if(i.op == "gtri") r[i.c] = (r[i.a] > i.b) ?  1 :  0; //gtri
		else if(i.op == "gtrr") r[i.c] = (r[i.a] > r[i.b]) ? 1 : 0; //gtrr
		else if(i.op == "eqir") r[i.c] = (i.a == r[i.b]) ? 1 : 0; //eqir
		else if(i.op == "eqri") r[i.c] = (r[i.a] == i.b) ? 1 : 0; //eqri
		else if(i.op == "eqrr") r[i.c] = (r[i.a] == r[i.b]) ? 1 : 0; //eqrr
}

void part1(vector<Instruction> instructions, int ip, int init){
	vector<int> r(6, 0);
	int i =0;
	/* r[0] = init; */
	vector<int> values;
	while(r[ip] < instructions.size()){
		/* cout << "Ip: " << r[ip] << " " << instructions[r[ip]].op << " "; */ 
		execute_instruction(instructions[r[ip]], r);
		r[ip]++;
		if(r[ip] == 29){
			if(find(values.begin(), values.end(), r[2]) != values.end()){
				cout << values[values.size() - 1] << endl;
			}else{
				values.push_back(r[2]);
			}

		/* cout << r[0] << ", " << r[1] << ", " << r[2] << ", " << r[3] << ", " */
			 /* << r[4] << ", " << r[5] << endl; */
		}
		/* i++; */
	}
	/* if(i < 100000){ */
		/* cout << "Halt after: " << i << endl; */
	/* } */
}
int main(){
	auto result = load_instructions("input.txt");
	int ip = result.first;
	vector<Instruction> instructions = result.second;
	for(int i=0; i<1; i++){
		part1(instructions, ip, i);
	}
}

