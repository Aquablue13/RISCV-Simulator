#include <iostream>
#include <cmath>
#include <cstring>
#include <cstdio>
using namespace std;
typedef unsigned int uint;
const int N = 34;
const int M = 2e5;
const int K = 10;
uint pc, npc, op, op_, op__, rd;
uint sh, Rs2, tr[200];
bool con;

uint mem[M], reg[N], cur, imm, ALUOutput, lmd, rs1, rs2;

void extend(uint &x, int p){
	if ((x >> p) & 1)
		for (int i = 31; i > p; i--)
			x |= (1u << i);
}

uint les(uint x, uint y){
	if (!((x >> 31) & 1) && ((y >> 31) & 1))
		return 0;
	if (((x >> 31) & 1) && !((y >> 31) & 1))
		return 1;
	for (int i = 30; i >= 0; i--){
		if (((x >> i) & 1) < ((y >> i) & 1))
			return 1;
		if (((x >> i) & 1) > ((y >> i) & 1))
			return 0;
	}
	return 0;
}

void pre(){
	for (int i = 0; i < 10; i++)
		tr[int('0') + i] = i;
	for (int i = 10; i < 16; i++)
		tr[int('A') + i - 10] = i;
	char s[10];
	uint x;
	while (scanf("%s", s) != EOF){
		if (s[0] == '@'){
			x = 0;
			for (int i = 1; i < 9; i++)
				x = (x << 4) + tr[s[i]];
			pc = x;
		}
		else{
			x = 0;
			for (int i = 0; i < 2; i++)
				x = (x << 4) + tr[s[i]];
			mem[pc] = x;
			for (int j = 1; j < 4; j++){
				scanf("%s", s);
				x = 0;
				for (int i = 0; i < 2; i++)
					x = (x << 4) + tr[s[i]];
				mem[pc + j] = x;
			}
			pc += 4;
		}
	}
	
}

void IF(int id){
	int len = 32;
	cur = 0;
	for (int i = 3; i >= 0; i--)
		cur = (cur << 8) + mem[pc + i];
	npc = (pc += 4);
}

uint get(uint x, int l, int r){
	uint s = 0;
	for (int i = r; i >= l; i--)
		s = (s << 1) | ((x >> i) & 1);
	return s;
}

void ID(int id){
	op = get(cur, 0, 6);
	switch (op){
	case 23: case 55: //LUI AUIPC
		rd = get(cur, 7, 11);
		imm = 0;
		for (int i = 31; i > 11; i--)
			imm |= (1u << i) * ((cur >> i) & 1);
		return;
	
	case 111: //JAL
		rd = get(cur, 7, 11);
		imm = 0;
		imm |= (1 << 20) * ((cur >> 31) & 1);
		for (int i = 30; i > 20; i--)
			imm |= (1 << (i - 20)) * ((cur >> i) & 1);
		imm |= (1 << 11) * ((cur >> 20) & 1);
		for (int i = 19; i > 11; i--)
			imm |= (1 << i) * ((cur >> i) & 1);
		extend(imm, 19);
		return;

	case 103: case 3: //JALR 
		rd = get(cur, 7, 11);
		rs1 = reg[get(cur, 15, 19)];
		op_ = get(cur, 12, 14);
		imm = 0;
		for (int i = 31; i > 19; i--)
			imm |= (1 << (i - 20)) * ((cur >> i) & 1);
		extend(imm, 11);
		return;

	case 99:
		rs1 = reg[get(cur, 15, 19)];
		Rs2 = get(cur, 20, 24);
		rs2 = reg[Rs2];
		op_ = get(cur, 12, 14);
		imm = 0;
		imm |= (1 << 12) * ((cur >> 31) & 1);
		for (int i = 30; i > 24; i--)
			imm |= (1 << (i - 20)) * ((cur >> i) & 1);
		for (int i = 11; i > 7; i--)
			imm |= (1 << (i - 7)) * ((cur >> i) & 1);
		imm |= (1 << 11) * ((cur >> 7) & 1);
		extend(imm, 12);
		return;

	case 19:
		rd = get(cur, 7, 11);
		rs1 = reg[get(cur, 15, 19)];
		op_ = get(cur, 12, 14);
		if (op_ == 1 || op_ == 5){
			sh = get(cur, 20, 25);
		}
		else{
			imm = 0;
			for (int i = 31; i > 19; i--)
				imm |= (1 << (i - 20)) * ((cur >> i) & 1);
			extend(imm, 11);
		}
		op__ = get(cur, 30, 30);;
		return;

	case 35:
		rs1 = reg[get(cur, 15, 19)];
		Rs2 = get(cur, 20, 24);
		rs2 = reg[Rs2];
		op_ = get(cur, 12, 14);
		imm = 0;
		for (int i = 31; i > 24; i--)
			imm |= (1 << (i - 20)) * ((cur >> i) & 1);
		for (int i = 11; i > 6; i--)
			imm |= (1 << (i - 7)) * ((cur >> i) & 1);
		extend(imm, 11);
		return;

	case 51:
		rd = get(cur, 7, 11);
		rs1 = reg[get(cur, 15, 19)];
		Rs2 = get(cur, 20, 24);
		rs2 = reg[Rs2];
		op_ = get(cur, 12, 14);
		op__ = get(cur, 30, 30);
		return;
	}
}

void EX(int id){
	switch (op){
	case 55:
		ALUOutput = imm;
		return;

	case 23:
		ALUOutput =  imm + npc - 4;
		return;

	case 111:
		ALUOutput =  imm + npc - 4;
		return;

	case 103:
		ALUOutput = imm + rs1;
		return;

	case 99:
		ALUOutput =  imm + npc - 4;
		switch (op_){
			case 0:
				con = (rs1 == rs2);
				return;

			case 1:
				con = !(rs1 == rs2);
				return;

			case 4:
				con = les(rs1, rs2);
				return;

			case 5:
				con = !les(rs1, rs2);
				return;

			case 6:
				con = (rs1 < rs2);
				return;

			case 7:
				con = (rs1 >= rs2);
				return;
		}
		return;

	case 3:
		ALUOutput = imm + rs1;
		return;

	case 35:
		ALUOutput = imm + rs1;
		return;

	case 19:
		switch (op_){
			case 0:
				ALUOutput = imm + rs1;
				return;

			case 2:
				ALUOutput = les(rs1, imm);
				return;

			case 3:
				ALUOutput = (rs1 < imm);
				return;

			case 4:
				ALUOutput = imm ^ rs1;
				return;

			case 6:
				ALUOutput = imm | rs1;
				return;

			case 7:
				ALUOutput = imm & rs1;
				return;

			case 1:
				ALUOutput = (rs1 << sh);
				return;

			case 5:
				ALUOutput = rs1 >> sh;
				if (op__)
					extend(ALUOutput, 31 - sh);
			//	ALUOutput = (rs1.sr(sh, op__);
				return;
		}
		return;

	case 51:
		switch (op_){
			case 0:
				if (op__)
					ALUOutput = rs1 - rs2;
				else
					ALUOutput = rs1 + rs2;
				return;

			case 1:
				ALUOutput = rs1 << rs2;
				return;

			case 2:
				ALUOutput = les(rs1, rs2);
				return;

			case 3:
				ALUOutput = (rs1 < rs2);
				return;

			case 4:
				ALUOutput = rs1 ^ rs2;
				return;

			case 5:
				ALUOutput = rs1 >> rs2;
				if (op__)
					extend(ALUOutput, 31 - rs2);
			//	ALUOutput = rs1.sr(rs2.calc(), op__);
				return;

			case 6:
				ALUOutput = rs1 | rs2;
				return;

			case 7:
				ALUOutput = rs1 & rs2;
				return;

		}
		return;

	}
}

void MEM(int id){
	switch (op){
	case 111: case 103:
		pc = ALUOutput;
		return;

	case 99:
		if (con)
			pc = ALUOutput;
		return;

	case 3:
		switch (op_){
			case 0: case 4:
				lmd = get(mem[ALUOutput], 0, 7);
				if (!op_)
					extend(lmd, 7);
				return;

			case 1: case 5:
				for (int i = 1; i >= 0; i--)
					lmd = (lmd << 8) + get(mem[ALUOutput + i], 0, 7);
				if (op_ == 1)
					extend(lmd, 15);
				return;

			case 2:
				for (int i = 3; i >= 0; i--)
					lmd = (lmd << 8) + get(mem[ALUOutput + i], 0, 7);
				return;
		}
		return;

	case 35:
	//	cout << "!!!" << ALUOutput << endl;
		switch (op_){
			case 0:
				mem[ALUOutput] = get(rs2, 0, 7);
				return;

			case 1:
				for (int i = 1; i >= 0; i--)
					for (int j = 7; j >= 0; j--)
						mem[ALUOutput + i] = get(rs2, i * 8, i * 8 + 7);
				return;

			case 2:
				for (int i = 3; i >= 0; i--)
					for (int j = 7; j >= 0; j--)
						mem[ALUOutput + i] = get(rs2, i * 8, i * 8 + 7);
				return;

		}
		return;

	}
}

void WB(int id){
	switch (op){
	case 55: case 23: case 19: case 51:
		reg[rd] = ALUOutput;
		break;

	case 111: case 103:
		reg[rd] = npc;
		break;

	case 3:
		reg[rd] = lmd;
		break;

	default:
		return;

	}
	reg[0] = 0;	

//	printf("%d %d %d %d %d %d %d\n", pc, rd, reg[rd], cur, imm, rs1, rs2);
//	printf("%d %d %d\n", pc, rd, reg[rd]);
}

int main(){
//	freopen("hanoi.data", "r", stdin);
//	freopen("1.out", "w", stdout);
	pre();
	pc = 0;
	while (1){
//	for (int i = 1; i <= 10000; i++){
//		if (pc == 4120)
//			cerr << '!';
		IF(1);
		if (cur == 267388179)
			break;
		ID(1);
		EX(1);
		MEM(1);
		WB(1);
	}
	printf("%d\n", reg[10] & 255u);
	return 0;
}
