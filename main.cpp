#include <iostream>
#include <cmath>
#include <cstring>
#include <cstdio>
using namespace std;
const int N = 34;
const int M = 2e5;
const int K = 10;
int pc, npc, op, op_, op__, rd;
int sh, Rs2, tr[200];
bool con;

struct data{
	int a[N];

	void extend(bool iss, int len){
		if (iss && a[len])
			for (int i = 31; i > len; i--)
				a[i] = 1;
		else
			for (int i = 31; i > len; i--)
				a[i] = 0;
	}

	bool operator ==(const data &b){
		for (int i = 0; i < 32; i++)
			if (a[i] != b.a[i])
				return 0;
		return 1;
	}

	data& operator =(const data &b){
		for (int i = 31; i >= 0; i--)
			a[i] = b.a[i];
		return *this;
	}

	data operator +(const data &b){
		data s;
		for (int i = 0; i < 32; i++)
		//	s.a[i] = 0;
			s.a[i] = a[i] + b.a[i];
		for (int i = 0; i < 32; i++){
			if (s.a[i] >= 2)
				s.a[i + 1] += 1, s.a[i] -= 2;
		}
		return s;
	}

	data operator -(const data &b){
		data s;
		for (int i = 0; i < 32; i++)
			s.a[i] = b.a[i] ^ 1;
		s.a[0]++;
		for (int i = 0; i < 32; i++){
			if (s.a[i] >= 2)
				s.a[i + 1] += 1, s.a[i] -= 2;
		}
		return *this + s;
	}

	data operator |(const data &b){
		data s;
		for (int i = 0; i < 32; i++)
			s.a[i] = a[i] | b.a[i];
		return s;
	}

	data operator &(const data &b){
		data s;
		for (int i = 0; i < 32; i++)
			s.a[i] = a[i] & b.a[i];
		return s;
	}

	data operator ^(const data &b){
		data s;
		for (int i = 0; i < 32; i++)
			s.a[i] = a[i] ^ b.a[i];
		return s;
	}

	data sl(int x){
		data s;
		for (int i = 31; i >= x; i--)
			s.a[i] = a[i - x];
		for (int i = x - 1; i >= 0; i--)
			s.a[i] = 0;
		return s;
	}

	data sr(int x, bool op){
		data s;
		for (int i = 31 - x; i >= 0; i--)
			s.a[i] = a[i + x];
		for (int i = 31; i >= 32 - x; i--)
			s.a[i] = (op & a[31]);
		return s;
	}

	bool les(const data &b){
		if (!a[31] && b.a[31])
			return 0;
		if (a[31] && !b.a[31])
			return 1;
		for (int i = 30; i >= 0; i--){
			if (a[i] < b.a[i])
				return 1;
			if (a[i] > b.a[i])
				return 0;
		}
		return 0;
	}

	bool lesu(const data &b){
		for (int i = 31; i >= 0; i--){
			if (a[i] < b.a[i])
				return 1;
			if (a[i] > b.a[i])
				return 0;
		}
		return 0;
	}

	int calc(){
		int s = 0;
		for (int i = 31; i >= 0; i--)
			s = (s << 1) | a[i];
		return s;
	}

}mem[M], reg[N], cur, imm, ALUOutput, lmd, rs1, rs2;

data trans(int x){
	data s;
	for (int i = 0; i < 32; i++){
		s.a[i] = x & 1;
		x >>= 1;
	}
	return s;
}

void pre(){
	for (int i = 0; i < 10; i++)
		tr[int('0') + i] = i;
	for (int i = 10; i < 16; i++)
		tr[int('A') + i - 10] = i;
	char s[10];
	int x;
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
			mem[pc + 3] = trans(x);
			for (int j = 2; j >= 0; j--){
				scanf("%s", s);
				x = 0;
				for (int i = 0; i < 2; i++)
					x = (x << 4) + tr[s[i]];
				mem[pc + j] = trans(x);
			}
			pc += 4;
		}
	}
	
}

void IF(int id){
	int len = 32;
	for (int i = 0; i < 4; i++)
		for (int j = 7; j >= 0; j--)
			cur.a[--len] = mem[pc + i].a[j];
	npc = pc + 4;
}

void ID(int id){
	op = 0;
	for (int i = 6; i >= 0; i--)
		op = (op << 1) | cur.a[i];

	int x;
	switch (op){
	case 23: case 55: //LUI AUIPC
		x = 0;
		for (int i = 11; i > 6; i--)
			x = (x << 1) | cur.a[i];
		rd = x;
		for (int i = 31; i > 11; i--)
			imm.a[i] = cur.a[i];
		for (int i = 11; i >= 0; i--)
			imm.a[i] = 0;
		return;
	
	case 111: //JAL
		x = 0;
		for (int i = 11; i > 6; i--)
			x = (x << 1) | cur.a[i];
		rd = x;
		imm.a[0] = 0;
		imm.a[20] = cur.a[31];
		for (int i = 30; i > 20; i--)
			imm.a[i - 20] = cur.a[i];
		imm.a[11] = cur.a[20];
		for (int i = 19; i > 11; i--)
			imm.a[i] = cur.a[i];
		imm.extend(1, 19);
		return;

	case 103: case 3: //JALR 
		x = 0;
		for (int i = 11; i > 6; i--)
			x = (x << 1) | cur.a[i];
		rd = x;
		x = 0;
		for (int i = 19; i > 14; i--)
			x = (x << 1) | cur.a[i];
		rs1 = reg[x];
		op_ = 0;
		for (int i = 14; i > 11; i--)
			op_ = (op_ << 1) | cur.a[i];
		for (int i = 31; i > 19; i--)
			imm.a[i - 20] = cur.a[i];
		imm.extend(1, 11);
		return;

	case 99:
		x = 0;
		for (int i = 19; i > 14; i--)
			x = (x << 1) | cur.a[i];
		rs1 = reg[x];
		x = 0;
		for (int i = 24; i > 19; i--)
			x = (x << 1) | cur.a[i];
		Rs2 = x;
		rs2 = reg[x];
		op_ = 0;
		for (int i = 14; i > 11; i--)
			op_ = (op_ << 1) | cur.a[i];
		imm.a[0] = 0;
		imm.a[12] = cur.a[31];
		for (int i = 30; i > 24; i--)
			imm.a[i - 20] = cur.a[i];
		for (int i = 11; i > 7; i--)
			imm.a[i - 7] = cur.a[i];
		imm.a[11] = cur.a[7];
		imm.extend(1, 11);
		return;

	case 19:
		x = 0;
		for (int i = 11; i > 6; i--)
			x = (x << 1) | cur.a[i];
		rd = x;
		x = 0;
		for (int i = 19; i > 14; i--)
			x = (x << 1) | cur.a[i];
		rs1 = reg[x];
		op_ = 0;
		for (int i = 14; i > 11; i--)
			op_ = (op_ << 1) | cur.a[i];
		if (op_ == 1 || op_ == 5){
			sh = 0;
			for (int i = 25; i > 19; i--)
				sh = (sh << 1) | cur.a[i];
		}
		else{
			for (int i = 31; i > 19; i--)
				imm.a[i - 20] = cur.a[i];
			imm.extend(1, 11);
		}
		op__ = cur.a[30];
		return;

	case 35:
		x = 0;
		for (int i = 19; i > 14; i--)
			x = (x << 1) | cur.a[i];
		rs1 = reg[x];
		x = 0;
		for (int i = 24; i > 19; i--)
			x = (x << 1) | cur.a[i];
		Rs2 = x;
		rs2 = reg[x];
		op_ = 0;
		for (int i = 14; i > 11; i--)
			op_ = (op_ << 1) | cur.a[i];
		for (int i = 31; i > 24; i--)
			imm.a[i - 20] = cur.a[i];
		for (int i = 11; i > 6; i--)
			imm.a[i - 7] = cur.a[i];
		imm.extend(1, 11);
		return;

	case 51:
		x = 0;
		for (int i = 11; i > 6; i--)
			x = (x << 1) | cur.a[i];
		rd = x;
		x = 0;
		for (int i = 19; i > 14; i--)
			x = (x << 1) | cur.a[i];
		rs1 = reg[x];
		x = 0;
		for (int i = 24; i > 19; i--)
			x = (x << 1) | cur.a[i];
		Rs2 = x;
		rs2 = reg[x];
		op_ = 0;
		for (int i = 14; i > 11; i--)
			op_ = (op_ << 1) | cur.a[i];
		op__ = cur.a[30];
		return;
	}
}

void EX(int id){
	switch (op){
	case 55:
		ALUOutput = imm;
		return;

	case 23:
		ALUOutput =  imm + trans(npc - 4);
		return;

	case 111:
		ALUOutput =  imm + trans(npc - 4);
		return;

	case 103:
		ALUOutput = imm + rs1;
		return;

	case 99:
		ALUOutput =  imm + trans(npc - 4);
		switch (op_){
			case 0:
				con = (rs1 == rs2);
				return;

			case 1:
				con = !(rs1 == rs2);
				return;

			case 4:
				con = rs1.les(rs2);
				return;

			case 5:
				con = !rs1.les(rs2);
				return;

			case 6:
				con = rs1.lesu(rs2);
				return;

			case 7:
				con = !rs1.lesu(rs2);
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
				ALUOutput = trans(rs1.les(imm));
				return;

			case 3:
				ALUOutput = trans(rs1.lesu(imm));
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
				ALUOutput = rs1.sl(sh);
				return;

			case 5:
				ALUOutput = rs1.sr(sh, op__);
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
				ALUOutput = rs1.sl(rs2.calc());
				return;

			case 2:
				ALUOutput = trans(rs1.les(rs2));
				return;

			case 3:
				ALUOutput = trans(rs1.lesu(rs2));
				return;

			case 4:
				ALUOutput = rs1 ^ rs2;
				return;

			case 5:
				ALUOutput = rs1.sr(rs2.calc(), op__);
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
	pc = npc;
	int x, len;
	switch (op){
	case 111: case 103:
		pc = ALUOutput.calc();
		return;

	case 99:
		if (con)
			pc = ALUOutput.calc();
		return;

	case 3:
		x = ALUOutput.calc();
		switch (op_){
			case 0: case 4:
				for (int i = 7; i >= 0; i--)
					lmd.a[i] = mem[x].a[i];
				lmd.extend((!op_), 7);
				return;

			case 1: case 5:
				for (int i = 15; i > 7; i--)
					lmd.a[i] = mem[x].a[i - 8];
				for (int i = 7; i >= 0; i--)
					lmd.a[i] = mem[x + 1].a[i];
				lmd.extend((op_ == 1), 15);
				return;

			case 2:
				len = 32;
				for (int i = 0; i < 4; i++)
					for (int j = 7; j >= 0; j--)
						lmd.a[--len] = mem[x + i].a[j];
				return;
		}
		return;

	case 35:
		x = ALUOutput.calc();
	//	cerr << "!!!" << x << endl;
		switch (op_){
			case 0:
				for (int i = 7; i >= 0; i--)
					mem[x].a[i] = rs2.a[i];
				return;

			case 1:
				len = 16;
				for (int i = 0; i < 2; i++)
					for (int j = 7; j >= 0; j--)
						mem[x + i].a[j] = rs2.a[--len];
				return;

			case 2:
				len = 32;
				for (int i = 0; i < 4; i++)
					for (int j = 7; j >= 0; j--)
						mem[x + i].a[j] = rs2.a[--len];
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
		reg[rd] = trans(npc);
		break;

	case 3:
		reg[rd] = lmd;
		break;

	default:
		return;

	}
	reg[0] = trans(0);	

//	printf("%d %d %d\n", pc, rd, reg[rd].calc());
}

int main(){
//	freopen("pi.data", "r", stdin);
	pre();
	pc = 0;
	while (1){
//		if (pc == 8)
//			cerr << '!';
		IF(1);
		if (cur.calc() == 267388179)
			break;
		ID(1);
		EX(1);
		MEM(1);
		WB(1);
	}
	data x = reg[10];
	x.extend(0, 7);
	printf("%d\n", x.calc());
	return 0;
}
