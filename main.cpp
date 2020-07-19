#include <iostream>
#include <cmath>
#include <cstring>
#include <cstdio>
using namespace std;
typedef unsigned int uint;
const int N = 34;
const int M = 2e5;
const int K = 10;
int cur[5];
uint pc, mem[M], reg[N], tr[200];

class IFID{
public:
	uint cur, npc;
}IFID[5];

class IDEX{
public:
	uint imm, op, op_, op__, sh, rd, rs1, rs2, npc;
	IDEX(){
		op = 0;
	}
}IDEX[5];

class EXMEM{
public:
	uint ALUOutput, rd, op, op_, rs2, npc;
	bool con;
	EXMEM(){
		op = 0;
	}
}EXMEM[5];

class MEMWB{
public:
	uint rd, op, lmd, npc, ALUOutput;
	MEMWB(){
		op = 0;
	}
}MEMWB[5];

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

void IF(int id, bool isn){
	int len = 32;
	IFID[id].cur = 0;
	if (isn)
		return;
	for (int i = 3; i >= 0; i--)
		IFID[id].cur = (IFID[id].cur << 8) + mem[pc + i];
	IFID[id].npc = (pc += 4);
}

uint get(uint x, int l, int r){
	return (x >> l) & ((1 << (r - l + 1)) - 1);/*
	uint s = 0;
	for (int i = r; i >= l; i--)
		s = (s << 1) | ((x >> i) & 1);
	return s;*/
}

void ID(int id){
	IDEX[id].op = get(IFID[id].cur, 0, 6);
	switch (IDEX[id].op){
	case 0: //nop
		break;
	case 23: case 55: //LUI AUIPC
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		IDEX[id].imm = 0;
		for (int i = 31; i > 11; i--)
			IDEX[id].imm |= (1u << i) * ((IFID[id].cur >> i) & 1);
		break;
	
	case 111: //JAL
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		IDEX[id].imm = 0;
		IDEX[id].imm |= (1 << 20) * ((IFID[id].cur >> 31) & 1);
		for (int i = 30; i > 20; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		IDEX[id].imm |= (1 << 11) * ((IFID[id].cur >> 20) & 1);
		for (int i = 19; i > 11; i--)
			IDEX[id].imm |= (1 << i) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 19);
		break;

	case 103: case 3: //JALR 
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		IDEX[id].rs1 = reg[get(IFID[id].cur, 15, 19)];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].imm = 0;
		for (int i = 31; i > 19; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 11);
		break;

	case 99:
		IDEX[id].rs1 = reg[get(IFID[id].cur, 15, 19)];
	//	IDEX[id].Rs2 = get(IFID[id].cur, 20, 24);
		IDEX[id].rs2 = reg[get(IFID[id].cur, 20, 24)];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].imm = 0;
		IDEX[id].imm |= (1 << 12) * ((IFID[id].cur >> 31) & 1);
		for (int i = 30; i > 24; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		for (int i = 11; i > 7; i--)
			IDEX[id].imm |= (1 << (i - 7)) * ((IFID[id].cur >> i) & 1);
		IDEX[id].imm |= (1 << 11) * ((IFID[id].cur >> 7) & 1);
		extend(IDEX[id].imm, 12);
		break;

	case 19:
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		IDEX[id].rs1 = reg[get(IFID[id].cur, 15, 19)];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		if (IDEX[id].op_ == 1 || IDEX[id].op_ == 5){
			IDEX[id].sh = get(IFID[id].cur, 20, 25);
		}
		else{
			IDEX[id].imm = 0;
			for (int i = 31; i > 19; i--)
				IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
			extend(IDEX[id].imm, 11);
		}
		IDEX[id].op__ = get(IFID[id].cur, 30, 30);;
		break;

	case 35:
		IDEX[id].rs1 = reg[get(IFID[id].cur, 15, 19)];
	//	Rs2 = get(IFID[id].cur, 20, 24);
		IDEX[id].rs2 = reg[get(IFID[id].cur, 20, 24)];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].imm = 0;
		for (int i = 31; i > 24; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		for (int i = 11; i > 6; i--)
			IDEX[id].imm |= (1 << (i - 7)) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 11);
		break;

	case 51:
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		IDEX[id].rs1 = reg[get(IFID[id].cur, 15, 19)];
	//	Rs2 = get(IFID[id].cur, 20, 24);
		IDEX[id].rs2 = reg[get(IFID[id].cur, 20, 24)];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].op__ = get(IFID[id].cur, 30, 30);
		break;
	}

	IDEX[id].npc = IFID[id].npc;
}

void EX(int id){
	switch (IDEX[id].op){
	case 0:
		break;
	case 55:
		EXMEM[id].ALUOutput = IDEX[id].imm;
		break;

	case 23:
		EXMEM[id].ALUOutput =  IDEX[id].imm + IDEX[id].npc - 4;
		break;

	case 111:
		EXMEM[id].ALUOutput =  IDEX[id].imm + IDEX[id].npc - 4;
		break;

	case 103:
		EXMEM[id].ALUOutput = IDEX[id].imm + IDEX[id].rs1;
		break;

	case 99:
		EXMEM[id].ALUOutput =  IDEX[id].imm + IDEX[id].npc - 4;
		switch (IDEX[id].op_){
			case 0:
				EXMEM[id].con = (IDEX[id].rs1 == IDEX[id].rs2);
				break;

			case 1:
				EXMEM[id].con = !(IDEX[id].rs1 == IDEX[id].rs2);
				break;

			case 4:
				EXMEM[id].con = les(IDEX[id].rs1, IDEX[id].rs2);
				break;

			case 5:
				EXMEM[id].con = !les(IDEX[id].rs1, IDEX[id].rs2);
				break;

			case 6:
				EXMEM[id].con = (IDEX[id].rs1 < IDEX[id].rs2);
				break;

			case 7:
				EXMEM[id].con = (IDEX[id].rs1 >= IDEX[id].rs2);
				break;
		}
		break;

	case 3:
		EXMEM[id].ALUOutput = IDEX[id].imm + IDEX[id].rs1;
		break;

	case 35:
		EXMEM[id].ALUOutput = IDEX[id].imm + IDEX[id].rs1;
		break;

	case 19:
		switch (IDEX[id].op_){
			case 0:
				EXMEM[id].ALUOutput = IDEX[id].imm + IDEX[id].rs1;
				break;

			case 2:
				EXMEM[id].ALUOutput = les(IDEX[id].rs1, IDEX[id].imm);
				break;

			case 3:
				EXMEM[id].ALUOutput = (IDEX[id].rs1 < IDEX[id].imm);
				break;

			case 4:
				EXMEM[id].ALUOutput = IDEX[id].imm ^ IDEX[id].rs1;
				break;

			case 6:
				EXMEM[id].ALUOutput = IDEX[id].imm | IDEX[id].rs1;
				break;

			case 7:
				EXMEM[id].ALUOutput = IDEX[id].imm & IDEX[id].rs1;
				break;

			case 1:
				EXMEM[id].ALUOutput = (IDEX[id].rs1 << IDEX[id].sh);
				break;

			case 5:
				EXMEM[id].ALUOutput = IDEX[id].rs1 >> IDEX[id].sh;
				if (IDEX[id].op__)
					extend(EXMEM[id].ALUOutput, 31 - IDEX[id].sh);
			//	EXMEM[id].ALUOutput = (IDEX[id].rs1.sr(IDEX[id].sh, IDEX[id].op__);
				break;
		}
		break;

	case 51:
		switch (IDEX[id].op_){
			case 0:
				if (IDEX[id].op__)
					EXMEM[id].ALUOutput = IDEX[id].rs1 - IDEX[id].rs2;
				else
					EXMEM[id].ALUOutput = IDEX[id].rs1 + IDEX[id].rs2;
				break;

			case 1:
				EXMEM[id].ALUOutput = IDEX[id].rs1 << IDEX[id].rs2;
				break;

			case 2:
				EXMEM[id].ALUOutput = les(IDEX[id].rs1, IDEX[id].rs2);
				break;

			case 3:
				EXMEM[id].ALUOutput = (IDEX[id].rs1 < IDEX[id].rs2);
				break;

			case 4:
				EXMEM[id].ALUOutput = IDEX[id].rs1 ^ IDEX[id].rs2;
				break;

			case 5:
				EXMEM[id].ALUOutput = IDEX[id].rs1 >> IDEX[id].rs2;
				if (IDEX[id].op__)
					extend(EXMEM[id].ALUOutput, 31 - IDEX[id].rs2);
			//	EXMEM[id].ALUOutput = IDEX[id].rs1.sr(IDEX[id].rs2.calc(), IDEX[id].op__);
				break;

			case 6:
				EXMEM[id].ALUOutput = IDEX[id].rs1 | IDEX[id].rs2;
				break;

			case 7:
				EXMEM[id].ALUOutput = IDEX[id].rs1 & IDEX[id].rs2;
				break;

		}
		break;

	}

	EXMEM[id].npc = IDEX[id].npc;
	EXMEM[id].rd = IDEX[id].rd;
	EXMEM[id].op = IDEX[id].op;
	EXMEM[id].op_ = IDEX[id].op_;
	EXMEM[id].rs2 = IDEX[id].rs2;
}

void MEM(int id){
	switch (EXMEM[id].op){
	case 0:
		break;
	case 111: case 103:
		pc = EXMEM[id].ALUOutput;
		break;

	case 99:
		if (EXMEM[id].con)
			pc = EXMEM[id].ALUOutput;
		break;

	case 3:
		switch (EXMEM[id].op_){
			case 0: case 4:
				MEMWB[id].lmd = get(mem[EXMEM[id].ALUOutput], 0, 7);
				if (!EXMEM[id].op_)
					extend(MEMWB[id].lmd, 7);
				break;

			case 1: case 5:
				for (int i = 1; i >= 0; i--)
					MEMWB[id].lmd = (MEMWB[id].lmd << 8) + get(mem[EXMEM[id].ALUOutput + i], 0, 7);
				if (EXMEM[id].op_ == 1)
					extend(MEMWB[id].lmd, 15);
				break;

			case 2:
				for (int i = 3; i >= 0; i--)
					MEMWB[id].lmd = (MEMWB[id].lmd << 8) + get(mem[EXMEM[id].ALUOutput + i], 0, 7);
				break;
		}
		break;

	case 35:
	//	cout << "!!!" << EXMEM[id].ALUOutput << endl;
		switch (EXMEM[id].op_){
			case 0:
				mem[EXMEM[id].ALUOutput] = get(EXMEM[id].rs2, 0, 7);
				break;

			case 1:
				for (int i = 1; i >= 0; i--)
					for (int j = 7; j >= 0; j--)
						mem[EXMEM[id].ALUOutput + i] = get(EXMEM[id].rs2, i * 8, i * 8 + 7);
				break;

			case 2:
				for (int i = 3; i >= 0; i--)
					for (int j = 7; j >= 0; j--)
						mem[EXMEM[id].ALUOutput + i] = get(EXMEM[id].rs2, i * 8, i * 8 + 7);
				break;

		}
		break;

	}

	MEMWB[id].ALUOutput = EXMEM[id].ALUOutput;
	MEMWB[id].npc = EXMEM[id].npc;
	MEMWB[id].rd = EXMEM[id].rd;
	MEMWB[id].op = EXMEM[id].op;
}

void WB(int id){
	switch (MEMWB[id].op){
	case 0:
		break;
	case 55: case 23: case 19: case 51:
		reg[MEMWB[id].rd] = MEMWB[id].ALUOutput;
		break;

	case 111: case 103:
		reg[MEMWB[id].rd] = MEMWB[id].npc;
		break;

	case 3:
		reg[MEMWB[id].rd] = MEMWB[id].lmd;
		break;

	default:
		break;

	}
	reg[0] = 0;	

//	printf("%d %d %d %d %d %d %d\n", pc, rd, reg[rd], cur, IDEX[id].imm, rs1, rs2);
//	printf("%d %d %d\n", pc, rd, reg[rd]);
}

int main(){
//	freopen("tak.data", "r", stdin);
//	freopen("1.out", "w", stdout);
	pre();
	pc = 0;
	for (int i = 0; i < 4; i++)
		cur[i] = 0;
	while (1){
//	for (int i = 1; i <= 10000; i++){
//		if (pc == 4120)
//			cerr << '!';
		IF(cur[0], 0);
		if (IFID[cur[0]].cur == 267388179)
			break;
		ID(cur[1]);
		EX(cur[2]);
		MEM(cur[3]);
		WB(cur[4]);
	}
	printf("%d\n", reg[10] & 255u);
	return 0;
}
