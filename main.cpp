#include <iostream>
#include <cmath>
#include <cstring>
#include <cstdio>
using namespace std;
typedef unsigned int uint;
const int N = 34;
const int M = 2e5;
const int K = 10;
int cur[2][5], op, ist, cnt(0), pre[4096];
uint pc, mem[M], reg[N], tr[200];
bool fl;
int fr[N];
long long A(0), B(0);

class IFID{
public:
	uint cur, npc;
}IFID[6];

class IDEX{
public:
	uint imm, op, op_, op__, sh, rd, rs1, rs2, npc;
	IDEX(){
		op = 0;
	}
}IDEX[6];

class EXMEM{
public:
	uint ALUOutput, rd, op, op_, rs2, npc;
	bool con;
	EXMEM(){
		op = 0;
	}
}EXMEM[6];

class MEMWB{
public:
	uint rd, op, lmd, npc, ALUOutput;
	MEMWB(){
		op = 0;
	}
}MEMWB[6];

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

void Pre(){
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

uint get(uint x, int l, int r){
	return (x >> l) & ((1 << (r - l + 1)) - 1);/*
	uint s = 0;
	for (int i = r; i >= l; i--)
		s = (s << 1) | ((x >> i) & 1);
	return s;*/
}

void IF(int id){
	int len = 32;
	IFID[id].cur = 0;
	for (int i = 3; i >= 0; i--)
		IFID[id].cur = (IFID[id].cur << 8) + mem[pc + i];
	IFID[id].npc = (pc += 4);

	IDEX[id].op = get(IFID[id].cur, 0, 6);
	if (IDEX[id].op == 111){
		IDEX[id].imm = 0;
		IDEX[id].imm |= (1 << 20) * ((IFID[id].cur >> 31) & 1);
		for (int i = 30; i > 20; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		IDEX[id].imm |= (1 << 11) * ((IFID[id].cur >> 20) & 1);
		for (int i = 19; i > 11; i--)
			IDEX[id].imm |= (1 << i) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 19);
		pc = IDEX[id].imm + IFID[id].npc - 4;
	}

	cur[op ^ 1][1] = cur[op][0];
}

void ID(int id){
	if (!id)
		return;
	IDEX[id].npc = IFID[id].npc;
	int x;
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
		IDEX[id].rd = get(IFID[id].cur, 7, 11);/*
		IDEX[id].imm = 0;
		IDEX[id].imm |= (1 << 20) * ((IFID[id].cur >> 31) & 1);
		for (int i = 30; i > 20; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		IDEX[id].imm |= (1 << 11) * ((IFID[id].cur >> 20) & 1);
		for (int i = 19; i > 11; i--)
			IDEX[id].imm |= (1 << i) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 19);*/
		break;

	case 103: case 3: //JALR 
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		x = get(IFID[id].cur, 15, 19);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs1 = reg[x];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].imm = 0;
		for (int i = 31; i > 19; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		extend(IDEX[id].imm, 11);
		if (IDEX[id].op == 103)
			pc = IDEX[id].imm + IDEX[id].rs1;
		break;

	case 99:
		x = get(IFID[id].cur, 15, 19);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs1 = reg[x];
		x = get(IFID[id].cur, 20, 24);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs2 = reg[x];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].imm = 0;
		IDEX[id].imm |= (1 << 12) * ((IFID[id].cur >> 31) & 1);
		for (int i = 30; i > 24; i--)
			IDEX[id].imm |= (1 << (i - 20)) * ((IFID[id].cur >> i) & 1);
		for (int i = 11; i > 7; i--)
			IDEX[id].imm |= (1 << (i - 7)) * ((IFID[id].cur >> i) & 1);
		IDEX[id].imm |= (1 << 11) * ((IFID[id].cur >> 7) & 1);
		extend(IDEX[id].imm, 12);
		bool con;
		switch (IDEX[id].op_){
			case 0:
				con = (IDEX[id].rs1 == IDEX[id].rs2);
				break;

			case 1:
				con = !(IDEX[id].rs1 == IDEX[id].rs2);
				break;

			case 4:
				con = les(IDEX[id].rs1, IDEX[id].rs2);
				break;

			case 5:
				con = !les(IDEX[id].rs1, IDEX[id].rs2);
				break;

			case 6:
				con = (IDEX[id].rs1 < IDEX[id].rs2);
				break;

			case 7:
				con = (IDEX[id].rs1 >= IDEX[id].rs2);
				break;
		}
		if (con)
			pc = IDEX[id].imm + IDEX[id].npc - 4;
		break;

	case 19:
		IDEX[id].rd = get(IFID[id].cur, 7, 11);
		x = get(IFID[id].cur, 15, 19);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs1 = reg[x];
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
		x = get(IFID[id].cur, 15, 19);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs1 = reg[x];
		x = get(IFID[id].cur, 20, 24);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs2 = reg[x];
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
		x = get(IFID[id].cur, 15, 19);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs1 = reg[x];
		x = get(IFID[id].cur, 20, 24);
		if (fr[x]){
			cur[op ^ 1][1] = cur[op][1];
			ist = 1;
			return;
		}
		IDEX[id].rs2 = reg[x];
		IDEX[id].op_ = get(IFID[id].cur, 12, 14);
		IDEX[id].op__ = get(IFID[id].cur, 30, 30);
		break;
	}

	switch (IDEX[id].op){
	case 55: case 23: case 19: case 51: case 111: case 103: case 3:
		fr[IDEX[id].rd]++;
		break;

	}

	cur[op ^ 1][2] = cur[op][1];
}

void EX(int id){
	if (!id)
		return;
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
	//	EXMEM[id].ALUOutput =  IDEX[id].imm + IDEX[id].npc - 4;
		break;

	case 103:
	//	EXMEM[id].ALUOutput = IDEX[id].imm + IDEX[id].rs1;
		break;

	case 99:
	//	EXMEM[id].ALUOutput =  IDEX[id].imm + IDEX[id].npc - 4;	
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

	cur[op ^ 1][3] = cur[op][2];
}

void MEM(int id){
	fl = 0;
	if (!id)
		return;
//	if (ist[op][3]){
//		ist[op ^ 1][]
//	}
	switch (EXMEM[id].op){
	case 0:
		break;/*
	case 111: case 103:
		pc = EXMEM[id].ALUOutput;
		break;

	case 99:
		if (EXMEM[id].con)
			pc = EXMEM[id].ALUOutput;
		break;
*/
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
		fl = 1;
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
		fl = 1;
		break;

	}

	MEMWB[id].ALUOutput = EXMEM[id].ALUOutput;
	MEMWB[id].npc = EXMEM[id].npc;
	MEMWB[id].rd = EXMEM[id].rd;
	MEMWB[id].op = EXMEM[id].op;

	cur[op ^ 1][4] = cur[op][3];
}

void WB(int id){
	if (!id)
		return;
	switch (MEMWB[id].op){
	case 0:
		break;
	case 55: case 23: case 19: case 51:
		reg[MEMWB[id].rd] = MEMWB[id].ALUOutput;
		fr[MEMWB[id].rd]--;
		break;

	case 111: case 103:
		reg[MEMWB[id].rd] = MEMWB[id].npc;
		fr[MEMWB[id].rd]--;
		break;

	case 3:
		reg[MEMWB[id].rd] = MEMWB[id].lmd;
		fr[MEMWB[id].rd]--;
		break;

	default:
		break;

	}
	reg[0] = 0;	

//	printf("%d %d %d %d %d %d %d\n", pc, rd, reg[rd], cur, IDEX[id].imm, rs1, rs2);
//	if ((IFID[id].cur & 127u) != 0b0100011 && (IFID[id].cur & 127u) != 0b1100011)
//		printf("%d %d %d\n", MEMWB[id].npc - 4, MEMWB[id].rd, reg[MEMWB[id].rd]);
}

void work(int x, bool y){
	A += y;
	B++;
	if (y && pre[x] < 1)
		pre[x]++;
	if (!y && pre[x] > -2)
		pre[x]--;
}

int main(){
//	freopen("qsort.data", "r", stdin);
//	freopen("1.out", "w", stdout);
	Pre();
	int T(0);
	pc = op = cnt = 0;
	int x, ise(0);
	while (1){
//	for (int i = 1; i <= 10; i++){
//		if (pc == 4120)
//			cerr << '!';
		ist = ise;
		for (int i = 0; i < 5; i++)
			cur[op ^ 1][i] = 0;
		if (IFID[cur[op][4]].cur == 267388179)
			break;
		WB(cur[op][4]);
		MEM(cur[op][3]);
		EX(cur[op][2]);
		ID(cur[op][1]);

		if (!ist){
			if (cur[op][1] && IDEX[cur[op][1]].op == 99){
				x = (IDEX[cur[op][1]].npc - 4) >> 2;
				bool p = (pre[x] > -1);
			//	if (x == 1103)
			//		cerr << '!';
			//	printf("%u %d %d\n", x, pre[x], p ^ (pc == IDEX[cur[op][1]].npc));
			//	printf("%d %d %d\n", p, pc, IDEX[cur[op][1]].npc);
			//	cerr << "!!!!!!" << pc << ' ' << p << endl;
				if (p ^ (pc == IDEX[cur[op][1]].npc)){
					work(x, 1);
				}
				else{
					work(x, 0);
					ist = 1;
				}
			}
		}

		if (!ist){
			cnt++;
			if (cnt > 5)
				cnt -= 5;
			IF(cur[op][0] = cnt);
			if (IFID[cur[op][0]].cur == 267388179)
				ise = 1;
		}
		
		op ^= 1;
		if (fl)
			T += 3;
		else
			T++;
	//	cerr << T << ' ' << pc << endl;
	}
//	cerr << B << ' ' << (double)A/B << endl;
	printf("%d\n", reg[10] & 255u);
	return 0;
}
