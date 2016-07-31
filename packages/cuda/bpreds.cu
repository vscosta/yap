#include "hip/hip_runtime.h"
#include <thrust/device_vector.h>
#include <thrust/scan.h>
#include <cstdarg>
#include "pred.h"

/*Determines the maximum from a set of values*/
int maximo(int count, ...)
{
	va_list ap;
    	int j, temp, mx = 0;
    	va_start(ap, count);

	for(j = 0; j < count; j++)
	{
		temp = va_arg(ap, int);
		if(temp > mx)
			mx = temp;
	}

    	va_end(ap);
    	return mx;
}


__global__ void bpreds(int *dop1, int *dop2, int rows, int of1, int of2, int *cons, int numc, int nx, int *res, int *res2)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, rowact1, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		rowact1 = id * of1;
		rowact = id * of2;
		for(x = nx; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 = dop1[rowact1 - op1 - 1];
			else
				op1 = dop2[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 = dop1[rowact1 - op2 - 1];
			else
				op2 = dop2[rowact + op2];
			switch(shared[x] - BPOFFSET)
			{
				case SBG_EQ: if(op1 != op2)
						return;
				break;
				case SBG_GT: if(op1 <= op2)
						return;
				break;
				case SBG_LT: if(op1 >= op2)
						return;
				break;
				case SBG_GE: if(op1 < op2)
						return;
				break;
				case SBG_LE: if(op1 > op2)
						return;
				break;
				case SBG_DF: if(op1 == op2)
						return;
			}
		}
		if(res2 != NULL)
			res2[id] = 1; 
		for(x = 0; x < nx; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop2[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop2[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 != op2)
						return;
				break;
				case SBG_GT: if(op1 <= op2)
						return;
				break;
				case SBG_LT: if(op1 >= op2)
						return;
				break;
				case SBG_GE: if(op1 < op2)
						return;
				break;
				case SBG_LE: if(op1 > op2)
						return;
				break;
				case SBG_DF: if(op1 == op2)
						return;
			}
		}
		res[id] = 1;
	}
}

/*Mark all rows that comply with the comparison predicates*/
__global__ void bpredsnormal2(int *dop1, int rows, int of1, int *cons, int numc, int *res)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		rowact = id * of1; 
		for(x = 0; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop1[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop1[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 != op2)
						return;
				break;
				case SBG_GT: if(op1 <= op2)
						return;
				break;
				case SBG_LT: if(op1 >= op2)
						return;
				break;
				case SBG_GE: if(op1 < op2)
						return;
				break;
				case SBG_LE: if(op1 > op2)
						return;
				break;
				case SBG_DF: if(op1 == op2)
						return;
			}
		}
		res[id] = 1;
	}
}

/*Unmark all rows that do not comply with the comparison predicates*/
__global__ void bpredsnormal(int *dop1, int rows, int of1, int *cons, int numc, int *res)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		if(res[id] == 0)
			return;
		rowact = id * of1; 
		for(x = 0; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop1[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop1[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 != op2)
					     {
						res[id] = 0;
						return;
					     }
				break;
				case SBG_GT: if(op1 <= op2)
					     {
						res[id] = 0;
						return;
					     }
				break;
				case SBG_LT: if(op1 >= op2)
					     {
						res[id] = 0;
						return;
					     }
				break;
				case SBG_GE: if(op1 < op2)
					     {
						res[id] = 0;
						return;
					     }
				break;
				case SBG_LE: if(op1 > op2)
					     {
						res[id] = 0;
						return;
					     }
				break;
				case SBG_DF: if(op1 == op2)
					     {
						res[id] = 0;
						return;
					     }
			}
		}
	}
}

__global__ void bpredsOR(int *dop1, int *dop2, int rows, int of1, int of2, int *cons, int numc, int nx, int *res, int *res2)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, rowact1, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		rowact1 = id * of1;
		rowact = id * of2;
		for(x = nx; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 = dop1[rowact1 - op1 - 1];
			else
				op1 = dop2[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 = dop1[rowact1 - op2 - 1];
			else
				op2 = dop2[rowact + op2];
			switch(shared[x] - BPOFFSET)
			{
				case SBG_EQ: if(op1 == op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
				break;
				case SBG_GT: if(op1 > op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
				break;
				case SBG_LT: if(op1 < op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
				break;
				case SBG_GE: if(op1 >= op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
				break;
				case SBG_LE: if(op1 <= op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
				break;
				case SBG_DF: if(op1 != op2)
					     {
						res2[id] = 1;
						x = numc;
					     }
			}
		}
		for(x = 0; x < nx; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop2[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop2[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 == op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_GT: if(op1 > op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_LT: if(op1 < op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_GE: if(op1 >= op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_LE: if(op1 <= op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_DF: if(op1 != op2)
					     {
						res[id] = 1;
						return;
					     }
			}
		}
	}
}

/*Mark all rows that comply with the comparison predicates using disjunctions (i.e. a row is marked if it complies with at least one predicate)*/
__global__ void bpredsorlogic2(int *dop1, int rows, int of1, int *cons, int numc, int *res)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		rowact = id * of1; 
		for(x = 0; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop1[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop1[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 == op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_GT: if(op1 > op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_LT: if(op1 < op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_GE: if(op1 >= op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_LE: if(op1 <= op2)
					     {
						res[id] = 1;
						return;
					     }
				break;
				case SBG_DF: if(op1 != op2)
					     {
						res[id] = 1;
						return;
					     }
			}
		}
		
	}
}

/*Unmark all rows that do not comply with the comparison predicates using disjunctions (i.e. a row is unmarked only if it complies with none of the predicates)*/
__global__ void bpredsorlogic(int *dop1, int rows, int of1, int *cons, int numc, int *res)
{
 	extern __shared__ int shared[];
	int id = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
	int x, rowact, op1, op2;
	if(hipThreadIdx_x < numc)
		shared[hipThreadIdx_x] = cons[hipThreadIdx_x];
	__syncthreads();
	if(id < rows)
	{
		if(res[id] == 0)
			return;
		rowact = id * of1; 
		for(x = 0; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop1[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop1[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ: if(op1 == op2)
						return;
				break;
				case SBG_GT: if(op1 > op2)
						return;
				break;
				case SBG_LT: if(op1 < op2)
						return;
				break;
				case SBG_GE: if(op1 >= op2)
						return;
				break;
				case SBG_LE: if(op1 <= op2)
						return;
				break;
				case SBG_DF: if(op1 != op2)
						return;
			}
		}
		res[id] = 0;
	}
}

