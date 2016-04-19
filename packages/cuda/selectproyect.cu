#include <thrust/device_vector.h>
#include <thrust/scan.h>
#include <stdlib.h>
#include "memory.h"
#include "bpreds.h"

/*Mark all rows that comply with the selections*/
__global__ void marcar2(int *dop1, int rows, int cols, int *cons, int numc, int *res)
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
/*If we already have an array of marks (perhaps because the selfjoin was applied first), 
we unmark any rows that do not comply with the selections*/
__global__ void marcar(int *dop1, int rows, int cols, int *cons, int numc, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int x, rowact, posact;
	if(threadIdx.x < numc)
		shared[threadIdx.x] = cons[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{
		if(res[id] == 0)
			return;
		rowact = id * cols;
		for(x = 0; x < numc; x += 2)
		{
			posact = rowact + shared[x];
			if(dop1[posact] != shared[x+1])
			{
				res[id] = 0;
				return;
			}
		}
	}
}

/*Unmark all rows that do not comply with the selfjoins.*/
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
			temp = dop1[pos+shared[x]];
			y = x + 1;
			temp2 = shared[y];
			while(temp2 > -1)
			{
				if(temp != dop1[temp2+pos])
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

/*Mark all rows that comply with the selfjoins*/
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
			temp = dop1[pos+shared[x]];
			y = x + 1;
			temp2 = shared[y];
			while(temp2 > -1)
			{
				if(temp != dop1[temp2+pos])
					return;
				y++;
				temp2 = shared[y];
			}
			x = y;
		}
		res[id] = 1;
	}
}

/*Project all columns found in 'dhead' to a new array 'res'*/
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

/*Project all columns found in 'dhead' using only the rows marked as valid (i.e. those that complied with 
selections, selfjoins, etc.). The array 'temp' holds the result of the prefix sum of said marks.*/
__global__ void llenarproyectar(int *dop1, int rows, int cols, int *temp, int *dhead, int hsize, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int pos, posr, x;
	if(threadIdx.x < hsize)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{		
		posr = temp[id];
		if(temp[id+1] != posr)
		{
			pos = id * cols;
			posr *= hsize;			
			for(x = 0; x < hsize; x++, posr++)
				res[posr] = dop1[pos+shared[x]];
		}
	}
}

/*Performs selections, selfjoins and comparison predicates when the rule has a single normal predicate.*/
int selectproyect(int *dop1, int rows, int cols, int head_size, int *select, int numselect, int *selfjoin, int numselfj, int *preds, int numpreds, int *project, int **ret, int ANDlogic)
{
	int *fres = NULL, *temp = NULL;
	int *dhead = NULL, tmplen;
	int size, size2, num;
	thrust::device_ptr<int> res;

#if TIMER
	cuda_stats.selects++;
#endif

	int head_bytes = maximo(4, numselect, numselfj, numpreds, head_size) * sizeof(int);
	reservar(&dhead, head_bytes);
	int numthreads = 1024;
	//int numthreads = 32;
	int blockllen = rows / numthreads + 1;

	#ifdef ROCKIT
		ANDlogic = 1;
	#endif

	if(numselect > 0)
	{		
		tmplen = rows + 1;
		size2 = tmplen * sizeof(int);
		reservar(&temp, size2);
		cudaMemset(temp, 0, size2);
		size = numselect * sizeof(int);
		cudaMemcpy(dhead, select, size, cudaMemcpyHostToDevice);

		marcar2<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselect, temp + 1);
		
		if(numselfj > 0)
		{
			size = numselfj * sizeof(int);
			cudaMemcpy(dhead, selfjoin, size, cudaMemcpyHostToDevice);
			samejoin<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselfj, temp + 1);
		}

		if(numpreds > 0)
		{
			size = numpreds * sizeof(int);
			cudaMemcpy(dhead, preds, size, cudaMemcpyHostToDevice);
			if(ANDlogic)
				bpredsnormal<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);
			else
				bpredsorlogic<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);
		}

		res = thrust::device_pointer_cast(temp);
		thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
		num = res[rows];
		if(num == 0)
			return 0;

		size = head_size * sizeof(int);
		reservar(&fres, num * size);
		cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
		llenarproyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, temp, dhead, head_size, fres);
		cudaFree(dhead);
		cudaFree(temp);
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
			cudaMemset(temp, 0, size2);
			size = numselfj * sizeof(int);
			cudaMemcpy(dhead, selfjoin, size, cudaMemcpyHostToDevice);
			samejoin2<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numselfj, temp + 1);

			if(numpreds > 0)
			{
				size = numpreds * sizeof(int);
				cudaMemcpy(dhead, preds, size, cudaMemcpyHostToDevice);
				if(ANDlogic)
					bpredsnormal<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);
				else
					bpredsorlogic<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);

			}

			res = thrust::device_pointer_cast(temp);
			thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
			num = res[rows];
			if(num == 0)
				return 0;

			size = head_size * sizeof(int);
			reservar(&fres, num * size);
			cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
			llenarproyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, temp, dhead, head_size, fres);
			cudaFree(dhead);
			cudaFree(temp);
			*ret = fres;
			return num;
		}
		else
		{
			if(numpreds > 0)
			{
				tmplen = rows + 1;
				size2 = tmplen * sizeof(int);
				reservar(&temp, size2);
				cudaMemset(temp, 0, size2);		
				size = numpreds * sizeof(int);
				cudaMemcpy(dhead, preds, size, cudaMemcpyHostToDevice);

				if(ANDlogic)
					bpredsnormal2<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);					
				else
					bpredsorlogic2<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, numpreds, temp + 1);
				res = thrust::device_pointer_cast(temp);
				thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
				num = res[rows];

				if(num == 0)
					return 0;

				size = head_size * sizeof(int);
				reservar(&fres, num * size);
				cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
				llenarproyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, temp, dhead, head_size, fres);
				cudaFree(dhead);
				cudaFree(temp);
				*ret = fres;
				return num;
			}
			else
			{
				size = head_size * sizeof(int);
				reservar(&fres, rows * size);
				cudaMemcpy(dhead, project, size, cudaMemcpyHostToDevice);
				proyectar<<<blockllen, numthreads, size>>>(dop1, rows, cols, dhead, head_size, fres);
				cudaFree(dhead);
				*ret = fres;
				return rows;
			}
		}
	}
}
