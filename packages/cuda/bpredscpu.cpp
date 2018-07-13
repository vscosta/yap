#include "pred.h"

int bpredscpu(int *dop1, int rows, int *bin, int3 numpreds, int **ret)
{
	int i, x, y, op1, op2;
	int size = 0, rowact, flag = 0;
	int predn = numpreds.x * 3;
	int total = predn + numpreds.z;
	int *fres, *ptr;
	int div, fin, ini[NUM_T + 1];
	vector<int> vec[NUM_T];

	for(x = 0; x < NUM_T; x++)
		vec[x].reserve(INISIZE);

	//omp_set_num_threads(NUM_T);
	div = rows / NUM_T;
	ini[0] = 0;
	for(x = 1; x < NUM_T; x++)
		ini[x] = div * x;
	ini[NUM_T] = rows;

	#pragma omp parallel for private(x,rowact,y,fin,op1,op2) firstprivate(flag,total)
	for(i = 0; i < NUM_T; i++)
	{
		fin = ini[i+1];
		for(x = ini[i]; x < fin; x++)
		{
			rowact = x * numpreds.y;
			for(y = 0; y < predn; y += 3)
			{
				op1 = bin[y+1];
				if(op1 < 0)
					op1 *= -1;
				else
					op1 = dop1[rowact + op1];
				op2 = bin[y+2];
				if(op2 < 0)
					op2 *= -1;
				else
					op2 = dop1[rowact + op2];
				switch(bin[y])
				{
					case SBG_EQ: if(op1 != op2)
							flag = 1;
							break;
					case SBG_GT: if(op1 <= op2)
							flag = 1;
							break;
					case SBG_LT: if(op1 >= op2)
							flag = 1;
							break;
					case SBG_GE: if(op1 < op2)
							flag = 1;
							break;
					case SBG_LE: if(op1 > op2)
							flag = 1;
							break;
					case SBG_DF: if(op1 == op2)
							flag = 1;
				}
				if(flag)
					break;
			}
			if(flag != 1)
			{
				for(y = predn; y < total; y++)
					vec[i].push_back(dop1[rowact+bin[y]]);
			}
			else
				flag = 0;
		}
	}
	for(x = 0; x < NUM_T; x++)
	{
		ini[x] = vec[x].size();
		size += ini[x];
	}
	fres = (int *)malloc(size * sizeof(int));
	ptr = fres;
	for(x = 0; x < NUM_T; x++)
	{
		memmove(ptr, vec[x].data(), ini[x] * sizeof(int));
		ptr += ini[x];
	}
	*ret = fres;
	return size / numpreds.z;
}
