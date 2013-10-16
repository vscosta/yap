#include <thrust/device_vector.h>
//#include <thrust/device_ptr.h>
#include <thrust/scan.h>
#include <stdlib.h>
#include "memory.h"

__global__ void marcar(int *dop1, int rows, int cols, int *cons, int numc, int *res) /*a libreria*/
{
 	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int x, rowact, posact;
	if(threadIdx.x < numc)
		shared[threadIdx.x] = cons[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{
		rowact = id * cols;
		for(x = 0; x < numc; x += 2)
		{
			posact = rowact + shared[x];
			if(dop1[posact] != shared[x+1])
				return;
		}
		res[id] = 1;
	}
}

__global__ void marcar2(int *dop1, int rows, int cols, int *cons, int numc, int *res) /*a libreria*/
{
 	extern __shared__ int shared[];
    	int *spos = &shared[numc];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int x, rowact, posact;
	if(threadIdx.x < (numc * 2))
		shared[threadIdx.x] = cons[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{
		if(res[id] == 0)
			return;
		rowact = id * cols;
		for(x = 0; x < numc; x++)
		{
			posact = rowact + spos[x];
			if(dop1[posact] != shared[x])
			{
				res[id] = 0;
				return;
			}
		}
	}
}

__global__ void samejoin(int *dop1, int rows, int cols, int *dhead, int cont, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int temp, temp2, pos, x, y;
	if(threadIdx.x < cont)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{	
		if(res[id] == 0)
			return;
		pos = id * cols;
		for(x = 0; x < cont; x++)
		{
			temp = shared[x];
			y = x + 1;
			temp2 = shared[y];
			while(temp2 > -1)
			{
				if(dop1[temp+pos] != dop1[temp2+pos])
				{
					res[id] = 0;
					return;
				}
				y++;
				temp2 = shared[y];
			}
			x = y;
		}
	}
}

__global__ void samejoin2(int *dop1, int rows, int cols, int *dhead, int cont, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int temp, temp2, pos, x, y;
	if(threadIdx.x < cont)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{	
		pos = id * cols;
		for(x = 0; x < cont; x++)
		{
			temp = shared[x];
			y = x + 1;
			temp2 = shared[y];
			while(temp2 > -1)
			{
				if(dop1[temp+pos] != dop1[temp2+pos])
					return;
				y++;
				temp2 = shared[y];
			}
			x = y;
		}
		res[id] = 1;
	}
}

__global__ void proyectar(int *dop1, int rows, int cols, int *dhead, int hsize, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int pos, posr, x;
	if(threadIdx.x < hsize)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{	
		pos = id * cols;
		posr = id * hsize;
		for(x = 0; x < hsize; x++, posr++)
			res[posr] = dop1[pos+shared[x]];
	}
}

__global__ void llenarproyectar(int *dop1, int rows, int cols, int *temp, int *dhead, int hsize, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int pos, posr, x;
	if(threadIdx.x < cols)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{		
		posr = temp[id+1];
		if(temp[id] != posr && posr > 0)
		{
			pos = id * cols;
			posr = (posr - 1) * hsize;			
			for(x = 0; x < hsize; x++, posr++)
				res[posr] = dop1[pos+shared[x]];
		}
	}
}

/*__global__ void removedup()
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	if(threadIdx.x < cols)
		shared[threadIdx.x] = dhead[threadIdx.x];
	if(id < rows)
	{
		
	}
}*/

template<typename T> /*a libreria*/
struct suma : public binary_function<T,T,T>
{
	__host__ __device__ 
	T operator()(const T &r1, const T &r2)
	{
		if(r1 > -1)
		{
			if(r2 > 0)
				return r1 + r2;
			return -r1;
		}
		else
		{
			if(r2 > 0)
				return abs(r1) + r2;
			return r1;
		}
	}
};

int mayor(int a, int b, int c)
{
	if(a > b)
	{
		if(a > c)
			return a;
	}
	else
	{
		if(b > c)
			return b;
	}
	return c;
}

int selectproyect(int *dop1, int rows, int cols, int head_size, int *select, int numselect, int *selfjoin, int numselfj, int *project, int **ret)
{
	int *fres = NULL, *temp = NULL;
	int *dhead = NULL, tmplen;
	int size, size2, num;
	thrust::device_ptr<int> res;

#if TIMER
	cuda_stats.selects++;
#endif
	int head_bytes = mayor(numselect, numselfj, head_size) * sizeof(int);
	reservar(&dhead, head_bytes);
	// DEBUG_MEM cerr << "+ " << dhead << " dhead  " << head_bytes << endl;

	int blockllen = rows / 1024 + 1;
	int numthreads = 1024;

	//removerep(dop1, rows, cols, dhead,) 
	if(numselect > 0)
	{		
		tmplen = rows + 1;
		size2 = tmplen * sizeof(int);
		reservar(&temp, size2);
		// DEBUG_MEM cerr << "+ " << temp << " temp  select " << size2 << endl;
		cudaMemset(temp, 0, size2);

		size = numselect * sizeof(int);
		cudaMemcpy(dhead, select, size, cudaMemcpyHostToDevice);

		marcar<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselect, temp + 1);
		
		if(numselfj > 0)
		{
			size = numselfj * sizeof(int);
			cudaMemcpy(dhead, selfjoin, size, cudaMemcpyHostToDevice);
			samejoin<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselfj, temp + 1);
		}

		res = thrust::device_pointer_cast(temp);
		thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
		num = res[rows];
		if(num == 0)
			return 0;

		size = head_size * sizeof(int);
		reservar(&fres, num * size);
		// DEBUG_MEM cerr << "+ " << fres << " fres select  " << num*size << endl;
		cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
		llenarproyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, temp, dhead, head_size, fres);
		liberar(dhead, head_bytes);
		liberar(temp, size2);
		*ret = fres;
		return num;
	}
	else
	{
		if(numselfj > 0)
		{
			tmplen = rows + 1;
			size2 = tmplen * sizeof(int);
			reservar(&temp, size2);
			// DEBUG_MEM cerr << "+ " << temp << " temp select  " << size2 << endl;
			cudaMemset(temp, 0, size2);
			
			size = numselfj * sizeof(int);
			cudaMemcpy(dhead, selfjoin, size, cudaMemcpyHostToDevice);
			samejoin2<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselfj, temp + 1);

			res = thrust::device_pointer_cast(temp);
			thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
			num = res[rows];
			if(num == 0)
				return 0;

			size = head_size * sizeof(int);
			reservar(&fres, num * size);
			// DEBUG_MEM cerr << "+ " << fres << " fres select again  " << num*size << endl;
			cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
			llenarproyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, temp, dhead, head_size, fres);
			liberar(dhead, head_bytes);
			liberar(temp, size2);
			*ret = fres;
			return num;
		}
		else
		{
			size = head_size * sizeof(int);
			reservar(&fres, rows * size);
			// DEBUG_MEM cerr << "+ " << fres << " fres select third  " << rows*size << endl;
			cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
			proyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, head_size, fres);
			liberar(dhead, head_bytes);
			*ret = fres;
			return rows;
		}
	}
}
