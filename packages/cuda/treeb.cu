#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/sequence.h>
#include <thrust/sort.h>
#include <thrust/gather.h>
#include <thrust/scan.h>
#include <iostream>
#include <cstdio>
#include "lista.h"
#include "bpreds.h"

#define WARP_SIZE		32
#define TREE_NODE_SIZE		WARP_SIZE
#define TREE_FANOUT		(TREE_NODE_SIZE + 1)

#define N_MULTI_P 		16
#define BLCK_PER_MP_create	256	// blocks per multiprocessor during tree creation
#define BLCK_PER_MP_search	512	// blocks per multiprocessor during tree searching
#define WAPRS_PER_BLCK_join	8//16	// blocks per multiprocessor during tree creation
#define BLCK_PER_MP_join	512//256	// blocks per multiprocessor during tree searching

#define THRD_PER_BLCK_create	TREE_NODE_SIZE
#define BLCK_PER_GRID_create	(N_MULTI_P * BLCK_PER_MP_create)
#define THRD_PER_BLCK_search	TREE_NODE_SIZE
#define BLCK_PER_GRID_search	(N_MULTI_P * BLCK_PER_MP_search)
#define THRD_PER_GRID_search	(THRD_PER_BLCK_search * BLCK_PER_GRID_search)
#define THRD_PER_BLCK_join	(WARP_SIZE * WAPRS_PER_BLCK_join)
#define BLCK_PER_GRID_join	(N_MULTI_P * BLCK_PER_MP_join)
#define THRD_PER_GRID_join	(THRD_PER_BLCK_join * BLCK_PER_GRID_join)

#define TEST_MAX		100

typedef int IKeyType;
typedef int Record;

typedef struct {
	int keys[TREE_NODE_SIZE];
} IDirectoryNode;

typedef struct {
	Record records[TREE_NODE_SIZE];
} IDataNode;

typedef struct {
	IDataNode* data;
	unsigned int nDataNodes;
	IDirectoryNode* dir;
	unsigned int nDirNodes;
} CUDA_CSSTree;

struct to_neg
{
	__host__ __device__
	bool operator()(const int &r1)
	{
		if(r1 < 0)
			return 1;
		return 0;
	}
};

__host__ __device__ unsigned int uintCeilingLog(unsigned int base, unsigned int num)
{
	unsigned int result = 0;

	for(unsigned int temp = 1; temp < num; temp *= base)
		result++;

	return result;
}

__host__ __device__ unsigned int uintCeilingDiv(unsigned int dividend, unsigned int divisor)
{
	return (dividend + divisor - 1) / divisor;
}

__host__ __device__ unsigned int uintPower(unsigned int base, unsigned int pow)
{
	unsigned int result = 1;

	for(; pow; pow--)
		result *= base;

	return result;
}

__device__ int getRightMostDescIdx(int tree_size, int nodeIdx)
{
	int tmp = nodeIdx * TREE_NODE_SIZE + TREE_FANOUT;
	int n = uintCeilingLog(TREE_FANOUT, uintCeilingDiv(TREE_NODE_SIZE * tree_size + TREE_FANOUT, tmp)) - 1;

	int result = (tmp * uintPower(TREE_FANOUT, n) - TREE_FANOUT) / TREE_NODE_SIZE;
    	return result; 
}

__device__ int getDataArrayIdx(int dirSize, int tree_size, int bottom_start, int treeIdx)
{
	int idx;
	if(treeIdx < dirSize) {
		idx = tree_size - bottom_start - 1;
	}
	else if( treeIdx < bottom_start ) {
		idx = tree_size - bottom_start + treeIdx - dirSize;
	}
	else {
		idx = treeIdx - bottom_start;
	}
	return idx;
}

// Binary Search
__device__ int firstMatchingKeyInDirNode1(int keys[], int key)
{
	int min = 0;
	int max = TREE_NODE_SIZE;
	int mid;
	int cut;
	while(max - min > 1) {
		mid = (min + max) / 2;
		cut = keys[mid];

		if(key > cut)
			min = mid;
		else
			max = mid;
	}

	if(keys[min] >= key)
		return min;

	return max;

}

// Binary Search
__device__ int firstMatchingKeyInDataNode2(Record records[], IKeyType key)
{
	int min = 0;
	int max = TREE_NODE_SIZE;
	int mid;
	int cut;
	while(max - min > 1) {
		mid = (min + max) / 2;
		cut = records[mid];

		if(key > cut)
			min = mid;
		else
			max = mid;
	}

	if(records[min] == key)
		return min;

	if(max < TREE_NODE_SIZE && records[max] == key)
		return max;

	return -1;
}

__global__ void gCreateIndex(IDataNode data[], IDirectoryNode dir[], int dirSize, int tree_size, int bottom_start, int nNodesPerBlock)
{
        int startIdx = blockIdx.x * nNodesPerBlock;
        int endIdx = startIdx + nNodesPerBlock;
        if(endIdx > dirSize)
                endIdx = dirSize;
        int keyIdx = threadIdx.x;

        // Proceed only when in internal nodes
        for(int nodeIdx = startIdx; nodeIdx < endIdx; nodeIdx++)
        {
                int childIdx = nodeIdx * TREE_FANOUT + keyIdx + 1;        // One step down to the left
                // Then look for the right most descendent
                int rightMostDesIdx;
                // Common cases
                if(childIdx < tree_size) {
                        rightMostDesIdx = getRightMostDescIdx(tree_size, childIdx);
                }
                // versus the unusual case when the tree is incomplete and the node does not have the full set of children
                else {
                        // pick the last node in the tree (largest element of the array)
                        rightMostDesIdx = tree_size - 1;
                }

                int dataArrayIdx = getDataArrayIdx(dirSize, tree_size, bottom_start, rightMostDesIdx);
	        dir[nodeIdx].keys[keyIdx] = data[dataArrayIdx].records[TREE_NODE_SIZE - 1];
        }
}

__global__ void gSearchTree(IDataNode* data, int nDataNodes, IDirectoryNode* dir, int nDirNodes, int lvlDir, Record* arr, int locations[], int nSearchKeys, int nKeysPerThread, int tree_size, int bottom_start)
{
	// Bringing the root node (visited by every tuple) to the faster shared memory
	__shared__ IKeyType RootNodeKeys[TREE_NODE_SIZE];
	RootNodeKeys[threadIdx.x] = dir->keys[threadIdx.x];

	__syncthreads();

	int OverallThreadIdx = blockIdx.x * THRD_PER_BLCK_search + threadIdx.x;

	for(int keyIdx = OverallThreadIdx; keyIdx < nSearchKeys; keyIdx += THRD_PER_GRID_search)
	{
		IKeyType val = arr[keyIdx];
		int loc = firstMatchingKeyInDirNode1(RootNodeKeys, val) + 1;
		for(int i = 1; i < lvlDir && loc < nDirNodes; i++) {
			int kid = firstMatchingKeyInDirNode1(dir[loc].keys, val);
			loc = loc * TREE_FANOUT + kid + 1;
		}

		if(loc >= tree_size)
			loc = nDataNodes - 1;
		else
			loc = getDataArrayIdx(nDirNodes, tree_size, bottom_start, loc);

		int offset = firstMatchingKeyInDataNode2(data[loc].records, val);
		locations[keyIdx] = (offset <0)?-1:(loc * TREE_NODE_SIZE + offset);
	}
}

/*Counts the number of times a row in 'S' is to be joined to a row in 'R'.*/
__global__ void gIndexJoin(int *R, int *S, int g_locations[], int sLen, int g_ResNums[])
{
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(s_cur < sLen) 
	{
		int count = 1;
		int r_cur = g_locations[s_cur];
		int s_key;
		if(r_cur >= 0)
		{
			s_key = S[s_cur];
			r_cur++;
			while(s_key == R[r_cur]) 
			{
				count++;
				r_cur++;
			}
			g_ResNums[s_cur] = count;
		}
	}
}

/*Corrects 'gSearchTree' results when dealing with a negative multijoin. Uses the values found in 'g_locations' which indicate, for each row in 'R', if its going
to be joined (positive number) or not (-1). Works by checking the additional columns to be joined (i.e. all except the two used by 'gSearchTree') and changing to -1 
in 'g_locations' those rows that have equal values in the checked columns.*/
__global__ void gIndexMultiJoinNegative(int *R, int *S, int g_locations[], int rLen, int *p1, int *p2, int of1, int of2, int *mloc, int *sloc, int *muljoin, int wj)
{
	extern __shared__ int shared[];
	int r_cur = blockIdx.x * blockDim.x + threadIdx.x;
	int posr, poss, x;

	if(threadIdx.x < wj)
		shared[threadIdx.x] = muljoin[threadIdx.x];
	__syncthreads();

	if(r_cur < rLen) 
	{
		int s_cur = g_locations[r_cur];
		int r_key;
		if(s_cur >= 0)
		{
			r_key = R[r_cur];
			if(mloc == NULL)
				posr = r_cur * of1;
			else
				posr = mloc[r_cur] * of1;
			while(r_key == S[s_cur])
			{
				poss = sloc[s_cur] * of2;
				for(x = 0; x < wj; x += 2)
				{
					if(p1[posr + shared[x]] != p2[poss + shared[x+1]])
						break;
				}
				if(x >= wj)
					return;
				s_cur++;
			}
			g_locations[r_cur] = -1;
		}
	}
}

/*Corrects 'gSearchTree' results when dealing with a multijoin. Uses the values found in 'g_locations' which indicate, for each row in 'S', if its going
to be joined (positive number) or not (-1). Works by checking the additional columns to be joined (i.e. all except the two used by 'gSearchTree') and counting the number of 
times a row in 'S' is to be joined to its corresponding row in 'R', storing the new result in 'g_locations'.*/
__global__ void gIndexMultiJoin(int *R, int *S, int g_locations[], int sLen, int g_ResNums[], int *p1, int *p2, int of1, int of2, int *mloc, int *sloc, int *muljoin, int wj)
{
	extern __shared__ int shared[];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;
	int posr, poss, x;

	if(threadIdx.x < wj)
		shared[threadIdx.x] = muljoin[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen) 
	{
		int count = 0;
		int r_cur = g_locations[s_cur];
		int s_key;
		if(r_cur >= 0)
		{
			s_key = S[s_cur];
			if(sloc == NULL)
				poss = s_cur * of2;
			else
				poss = sloc[s_cur] * of2;
			while(s_key == R[r_cur]) 
			{
				posr = mloc[r_cur] * of1;
				for(x = 0; x < wj; x += 2)
				{
					if(p1[posr + shared[x]] != p2[poss + shared[x+1]])
						break;
				}
				if(x >= wj)
					count++;
				r_cur++;
			}
			if(count > 0)
				g_ResNums[s_cur] = count;
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'. The difference between this function and 'gJoinWithWrite' is the comparison of the additional join
columns.*/
__global__ void multiJoinWithWrite(int g_locations[], int sLen, int g_PrefixSums[], int g_joinResultBuffers[], int *p1, int *p2, int of1, int of2, int *rule, int halfrul, int lenrul, int *mloc, int *sloc, int wj)
{
	extern __shared__ int shared[];
	int *extjoins = &shared[lenrul];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(threadIdx.x < (lenrul + wj))
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen)
	{
		int r_cur = g_locations[s_cur];
		if(r_cur >= 0)
		{
			int x, y, pos, posr, poss;
			int num1 = g_PrefixSums[s_cur];
			int num2 = g_PrefixSums[s_cur+1];
			
			int tmp1, tmp2;

			if(sloc == NULL)	
				poss = s_cur * of2;
			else
				poss = sloc[s_cur] * of2;
			for(x = num1; x < num2; x++, r_cur++)
			{
				pos = mloc[r_cur] * of1;
				for(y = 0; y < wj; y += 2) /*Additional comparison*/
				{
					tmp1 = p1[pos + extjoins[y]];
					tmp2 = p2[poss + extjoins[y+1]];
					if(tmp1 != tmp2)
						break;
				}
				if(y < wj)
				{
					x--;
					continue;
				}
				posr = x * lenrul;
				for(y = 0; y < halfrul; y++)
					g_joinResultBuffers[posr + y] = p1[pos + shared[y]];
				for(; y < lenrul; y++)
					g_joinResultBuffers[posr + y] = p2[poss + shared[y]];
			}
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'. The difference between this function and 'gJoinWithWrite2' is the comparison of the additional join
columns.*/
__global__ void multiJoinWithWrite2(int g_locations[], int sLen, int g_PrefixSums[], int g_joinResultBuffers[], int *p1, int *p2, int of1, int of2, int *rule, int cols, int *mloc, int *sloc, int wj)
{
	extern __shared__ int shared[];
	int *extjoins = &shared[cols];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(threadIdx.x < (cols + wj))
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen)
	{
		int r_cur = g_locations[s_cur];
		if(r_cur >= 0)
		{
			int x, y, pos, pos2, posr, cond;
			int num1 = g_PrefixSums[s_cur];
			int num2 = g_PrefixSums[s_cur+1];
			if(sloc == NULL)
				pos2 = s_cur * of2 - 1;
			else
				pos2 = sloc[s_cur] * of2 - 1;
			for(x = num1; x < num2; x++, r_cur++)
			{
				pos = mloc[r_cur] * of1 - 1;
				for(y = 0; y < wj; y += 2) /*Additional comparison*/
				{
					if(p1[pos + extjoins[y] + 1] != p2[pos2 + extjoins[y+1] + 1])
						break;
				}
				if(y < wj)
				{
					x--;
					continue;
				}
				posr = x * cols;
				for(y = 0; y < cols; y++)
				{
					cond = shared[y];
					if(cond > 0)
						g_joinResultBuffers[posr + y] = p1[pos + cond];
					else
						g_joinResultBuffers[posr + y] = p2[pos2 - cond];
				}
			}
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'. The difference between this function and 'gJoinWithWrite2' is that only the columns in the positve
predicate are projected.*/
__global__ void gJoinWithWriteNegative(int g_locations[], int rLen, int g_joinResultBuffers[], int *p1, int of1, int *rule, int halfrul, int *mloc)
{
	extern __shared__ int shared[];
	int r_cur = blockIdx.x * blockDim.x + threadIdx.x;
	int posr;

	if(threadIdx.x < halfrul)
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(r_cur < rLen)
	{
		posr = g_locations[r_cur];
		if(g_locations[r_cur+1] != posr)
		{
			int y, pos;
			if(mloc == NULL)	
				pos = r_cur * of1;
			else
				pos = mloc[r_cur] * of1;
			posr *= halfrul;
			for(y = 0; y < halfrul; y++)
				g_joinResultBuffers[posr + y] = p1[pos + shared[y]];
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'. The difference between this function and 'gJoinWithWrite' is that only the columns in the positve
predicate are projected.*/
__global__ void gJoinWithWriteNegative2(int g_locations[], int rLen, int g_joinResultBuffers[], int *p1, int of1, int *rule, int cols, int *mloc)
{
	extern __shared__ int shared[];
	int r_cur = blockIdx.x * blockDim.x + threadIdx.x;
	int posr;

	if(threadIdx.x < cols)
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(r_cur < rLen)
	{
		posr = g_locations[r_cur];
		if(g_locations[r_cur+1] != posr)
		{
			int y, pos;
			if(mloc == NULL)
				pos = r_cur * of1 - 1;
			else
				pos = mloc[r_cur] * of1 - 1;
			posr *= cols;
			for(y = 0; y < cols; y++)
				g_joinResultBuffers[posr + y] = p1[pos + shared[y]];
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'.*/
__global__ void gJoinWithWrite(int g_locations[], int sLen, int g_PrefixSums[], int g_joinResultBuffers[], int *p1, int *p2, int of1, int of2, int *rule, int halfrul, int lenrul, int *mloc, int *sloc)
{
	extern __shared__ int shared[];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(threadIdx.x < lenrul)
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen)
	{
		int r_cur = g_locations[s_cur];
		if(r_cur >= 0)
		{
			int x, y, pos, posr, poss;
			int num1 = g_PrefixSums[s_cur];
			int num2 = g_PrefixSums[s_cur+1];
			if(sloc == NULL)	
				poss = s_cur * of2;
			else
				poss = sloc[s_cur] * of2;
			for(x = num1; x < num2; x++, r_cur++)
			{
				pos = mloc[r_cur] * of1;
				posr = x * lenrul;
				for(y = 0; y < halfrul; y++)
					g_joinResultBuffers[posr + y] = p1[pos + shared[y]];
				for(; y < lenrul; y++)
					g_joinResultBuffers[posr + y] = p2[poss + shared[y]];
			}
		}
	}
}

/*Writes the result of the join and projects the necessary columns as defined by 'rule'. This version is used when performing the final join of the rule and its only difference is the 
projection, which is performed based on the variables in the head of the rule.*/
__global__ void gJoinWithWrite2(int g_locations[], int sLen, int g_PrefixSums[], int g_joinResultBuffers[], int *p1, int *p2, int of1, int of2, int *rule, int cols, int *mloc, int *sloc)
{
	extern __shared__ int shared[];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(threadIdx.x < cols)
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen)
	{
		int r_cur = g_locations[s_cur];
		if(r_cur >= 0)
		{
			int x, y, pos, pos2, posr, cond;
			int num1 = g_PrefixSums[s_cur];
			int num2 = g_PrefixSums[s_cur+1];
			if(sloc == NULL)
				pos2 = s_cur * of2 - 1;
			else
				pos2 = sloc[s_cur] * of2 - 1;
			for(x = num1; x < num2; x++, r_cur++)
			{
				pos = mloc[r_cur] * of1 - 1;
				posr = x * cols;
				for(y = 0; y < cols; y++)
				{
					cond = shared[y];
					if(cond > 0)
						g_joinResultBuffers[posr + y] = p1[pos + cond];
					else
						g_joinResultBuffers[posr + y] = p2[pos2 - cond];
				}
			}
		}
	}
}

/*Load part of column 'wj' of 'p' in 'R'. Which values are loaded is defined by the prefix sum results in 'pos'.*/
__global__ void llenar(int *p, int *R, int len, int of, int wj, int *pos, int *ids)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int cond;
	if(id < len)
	{
		cond = pos[id];
		if(pos[id+1] != cond)
		{
			R[cond] = p[id * of + wj];
			ids[cond] = id;
		}
	}
}

/*Load an entire column from 'p' into 'R'.*/
__global__ void llenarnosel(int *p, int *R, int len, int of, int wj)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	if(id < len)
		R[id] = p[id * of + wj];
}

__global__ void projectfinal(int *res, int rows, int cols, int *rule, int *out)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;

	if(threadIdx.x < cols)
		shared[threadIdx.x] = rule[threadIdx.x];
	__syncthreads();
	
	if(id < rows)
	{
		id *= cols;
		for(int y = 0; y < cols; y++)
			out[id + y] = res[id + shared[y]];
	}
}

void project(int *res, int resrows, int numcols1, int numcols2, int *proj, int **ret, int type)
{
	int z, *dcons, *d_Rout;
	int numthreads = 1024;
	//numthreads = 32;
	int blockllen = resrows / numthreads + 1;
	int sizepro = numcols2 * sizeof(int);
	reservar(&dcons, sizepro);
	if(type)
	{
		int *pt = (int *)malloc(sizepro);
		for(z = 0; z < numcols2; z++)
			pt[z] = proj[z] - 1;
		cudaMemcpy(dcons, pt, sizepro, cudaMemcpyHostToDevice); 
		//cudaDeviceSynchronize(); //Small cudaMemcpys are asynchronous, uncomment this line if the pointer is being liberated before it is copied.
		free(pt);
	}
	else
		cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
	reservar(&d_Rout, resrows * sizepro);
	projectfinal<<<blockllen, numthreads, sizepro>>>(res, resrows, numcols1, dcons, d_Rout);
	cudaFree(dcons);
	cudaFree(*ret);
	*ret = d_Rout;
}

__global__ void projectadd(int *dop1, int *dop2, int rows1, int rows2, int cols1, int cols2, int *dhead, int hsize, int *res)
{
	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int pos2, posr, x, y, cond;
	if(threadIdx.x < hsize)
		shared[threadIdx.x] = dhead[threadIdx.x];
	__syncthreads();
	if(id < rows2)
	{
		posr = id * hsize * rows1;
		pos2 = id * cols2 - 1;
		for(x = 0; x < rows1; x++)
		{
			for(y = 0; y < hsize; y++)
			{
				cond = shared[y];
				if(cond > 0)
					res[posr + y] = dop1[cond-1];
				else
					res[posr + y] = dop2[pos2 - cond];
			}
			posr += hsize;
		}
	}
}

void juntar(int *dop1, int *dop2, int rows1, int rows2, int cols1, int cols2, int *proj, int pcols, int **ret)
{
	int sizepro, *dcons, *d_Rout;
	int numthreads = 1024;
	//numthreads = 32;
	int blockllen = rows2 / numthreads + 1;
	sizepro = pcols * sizeof(int);
	reservar(&dcons, sizepro);
	cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
	reservar(&d_Rout, rows1 * rows2 * sizepro);
	projectadd<<<blockllen, numthreads, sizepro>>>(dop1, dop2, rows1, rows2, cols1, cols2, dcons, pcols, d_Rout);
	cudaFree(dcons);
	*ret = d_Rout;
}

/*Joins two predicates. Starts by performing all preliminary operations (selections, selfjoins, comparisons) on both predicates. Then a column pair is used to construct 
a CSS-Tree and that tree is searched for join positions. The positions are used in a prefix sum and its result allows us to write the result. Multijoins and negative 
predicates follow roughly the same process, but use different kernels.*/
int join(int *p1, int *p2, int rLen, int sLen, int of1, int of2, list<rulenode>::iterator rule, int pos, int bothops, int **ret, int ANDlogic)
{
	int pos2 = pos + 1;
	int *sel1 = NULL, nsel1 = 0;
	int *sel2 = rule->select[pos2];
	int nsel2 = rule->numsel[pos2];
	int *proj = rule->project[pos];
	int2 projp = rule->projpos[pos];
	int *sjoin1 = NULL, nsj1 = 0;
	int *sjoin2 = rule->selfjoin[pos2];
	int nsj2 = rule->numselfj[pos2];
	int *pred1 = NULL;
	int2 npred1 = make_int2(0,0);
	int *pred2 = rule->preds[pos2];
	int2 npred2 = rule->numpreds[pos2];
	int npred2tot = npred2.x + npred2.y;
	int *wherej = rule->wherejoin[pos];
	int numj = rule->numjoin[pos];
	int negative = rule->negatives[pos2+1];
	int flag;

	#ifdef ROCKIT
		ANDlogic = 0;
	#endif
	if(negative)
		ANDlogic = 1;

#if TIMER
	cuda_stats.joins++;
#endif

	int size, sizet, sizet2;
	if(bothops)
	{
		sel1 = rule->select[pos];
		nsel1 = rule->numsel[pos];
		sjoin1 = rule->selfjoin[pos];
		nsj1 = rule->numselfj[pos];
		pred1 = rule->preds[pos];
		npred1 = rule->numpreds[pos];
		sizet = maximo(10, of1, of2, nsel1, nsel2, projp.y + numj - 2, nsj1, nsj2, numj, npred1.x, npred2tot) * sizeof(int);
	}
	else
		sizet = maximo(7, of1, of2, nsel2, projp.y + numj - 2, nsj2, numj, npred2tot) * sizeof(int);
	
	int *dcons, *temp, *temp2 = NULL;
	int *d_R, *d_S;
	int blockllen, numthreads;
	
	int extraspace = TREE_NODE_SIZE - rLen % TREE_NODE_SIZE;
	int m32rLen = rLen + extraspace;
	int extraspaceS = TREE_NODE_SIZE - sLen % TREE_NODE_SIZE;
	int m32sLen = sLen + extraspaceS;
	if(m32rLen > m32sLen)
		sizet2 = (m32rLen + 1) * sizeof(int);
	else
		sizet2 = (m32sLen + 1) * sizeof(int);

	reservar(&dcons, sizet);
	reservar(&temp, sizet2);
	thrust::device_ptr<int> res = thrust::device_pointer_cast(temp);

	numthreads = 1024;
	//numthreads = 32;
	blockllen = sLen / numthreads + 1;
	int memSizeS, newLen = 0;
	int *posR = NULL, *posS = NULL;
	int sizem32S = 0, sizextra;

	#ifdef TIMER
	//cout << "INICIO" << endl;
	cudaEvent_t start, stop;
	float time;
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	if(npred2.x > 0 || npred2.y > 0 || nsel2 > 0 || nsj2 > 0)
	{
		newLen = sLen + 1;
		cudaMemsetAsync(temp, 0, newLen * sizeof(int));
	}

	if(npred2.x > 0 || npred2.y > 0)
	{
		size = npred2tot * sizeof(int);
		cudaMemcpy(dcons, pred2, size, cudaMemcpyHostToDevice);

		if(npred2.y > 0) /*Fix case when a(X,Y),b(Y,Z),Z > Y*/
		{
			reservar(&temp2, sizet2);
			cudaMemsetAsync(temp2, 0, newLen * sizeof(int));
			//res = thrust::device_pointer_cast(temp2);
			bpreds<<<blockllen, numthreads, size>>>(p1, p2, sLen, of1, of2, dcons, npred2tot, npred2.x, temp + 1, temp2 + 1);
		}
		else
		{
			if(negative)
				bpreds<<<blockllen, numthreads, size>>>(p1, p2, sLen, of1, of2, dcons, npred2tot, npred2.x, temp + 1, NULL);
			else
				bpredsOR<<<blockllen, numthreads, size>>>(p1, p2, sLen, of1, of2, dcons, npred2tot, npred2.x, temp + 1, NULL);
		}

		if(nsel2 > 0)
		{
			size = nsel2 * sizeof(int);
			cudaMemcpy(dcons, sel2, size, cudaMemcpyHostToDevice);
			marcar<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsel2, temp + 1);
		}

		if(nsj2 > 0)
		{
			size = nsj2 * sizeof(int);
			cudaMemcpy(dcons, sjoin2, size, cudaMemcpyHostToDevice);
			samejoin<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsj2, temp + 1);
		}
	}
	else
	{
		if(nsel2 > 0)
		{
			size = nsel2 * sizeof(int);
			cudaMemcpy(dcons, sel2, size, cudaMemcpyHostToDevice);
			marcar2<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsel2, temp + 1);

			if(nsj2 > 0)
			{
				size = nsj2 * sizeof(int);
				cudaMemcpy(dcons, sjoin2, size, cudaMemcpyHostToDevice);
				samejoin<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsj2, temp + 1);
			}
		}
		else
		{
			if(nsj2 > 0)
			{
				size = nsj2 * sizeof(int);
				cudaMemcpy(dcons, sjoin2, size, cudaMemcpyHostToDevice);
				samejoin2<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsj2, temp + 1);	
			}
			else
			{
				sizem32S = m32sLen * sizeof(int);
				reservar(&d_S, sizem32S);
				cudaMemsetAsync(d_S + sLen, 0x7f, extraspaceS * sizeof(int));
				llenarnosel<<<blockllen, numthreads>>>(p2, d_S, sLen, of2, wherej[1]);
			}
		}
	}
	
	if(npred2.x > 0 || npred2.y > 0 || nsel2 > 0 || nsj2 > 0)
	{
		flag = 0;
		while(flag != 1)
		{
			try
			{
				thrust::inclusive_scan(res + 1, res + newLen, res + 1);		
				flag = 1;
			}
			catch(std::bad_alloc &e)
			{
				limpiar("inclusive scan in join", 0);
			}				
		}	
		newLen = res[sLen];
	
		if(newLen == 0) // && !negative) ARREGLAR
		{
			cudaFree(temp);
			cudaFree(dcons);
			return 0;
		}

		extraspaceS = TREE_NODE_SIZE - newLen % TREE_NODE_SIZE;
		sizextra = extraspaceS * sizeof(int);
		m32sLen = newLen + extraspaceS;
		sizem32S = m32sLen * sizeof(int);

		reservar(&d_S, sizem32S);
		reservar(&posS, sizem32S);
		cudaMemsetAsync(d_S + newLen, 0x7f, sizextra);
		cudaMemsetAsync(posS + newLen, 0x7f, sizextra);
		llenar<<<blockllen, numthreads>>>(p2, d_S, sLen, of2, wherej[1], temp, posS);
		sLen = newLen;
	}

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	//cout << "Select1 = " << time << endl;
	cuda_stats.select1_time += time;

	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	blockllen = rLen / numthreads + 1;
	int sizem32;
	if(bothops)
	{
		if(temp2 != NULL)
		{
			cudaFree(temp);
			temp = temp2;
			res = thrust::device_pointer_cast(temp);
			newLen = rLen + 1;
			if(nsel1 > 0)
			{
				size = nsel1 * sizeof(int);
				cudaMemcpy(dcons, sel1, size, cudaMemcpyHostToDevice);
				marcar<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsel1, temp + 1);
			}
			if(nsj1 > 0)
			{
				size = nsj1 * sizeof(int);
				cudaMemcpy(dcons, sjoin1, size, cudaMemcpyHostToDevice);
				samejoin<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsj1, temp + 1);
			}
			if(npred1.x > 0)
			{
				size = npred1.x * sizeof(int);
				cudaMemcpy(dcons, pred1, size, cudaMemcpyHostToDevice);
				if(ANDlogic)
					bpredsnormal<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
				else
					bpredsorlogic<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
			}
		}
		else
		{
			if(npred1.x > 0 || nsel1 > 0 || nsj1 > 0)
			{
				newLen = rLen + 1;
				cudaMemsetAsync(temp, 0, newLen * sizeof(int));
			}

			if(nsel1 > 0)
			{
				size = nsel1 * sizeof(int);
				cudaMemcpy(dcons, sel1, size, cudaMemcpyHostToDevice);
				marcar2<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsel1, temp + 1);

				if(nsj1 > 0)
				{
					size = nsj1 * sizeof(int);
					cudaMemcpy(dcons, sjoin1, size, cudaMemcpyHostToDevice);
					samejoin<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsj1, temp + 1);
				}

				if(npred1.x > 0)
				{
					size = npred1.x * sizeof(int);
					cudaMemcpy(dcons, pred1, size, cudaMemcpyHostToDevice);
					if(ANDlogic)
						bpredsnormal<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
					else
						bpredsorlogic<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);		
				}
			}
			else
			{
				if(nsj1 > 0)
				{
					size = nsj1 * sizeof(int);
					cudaMemcpy(dcons, sjoin1, size, cudaMemcpyHostToDevice);
					samejoin2<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsj1, temp + 1);

					if(npred1.x > 0)
					{
						size = npred1.x * sizeof(int);
						cudaMemcpy(dcons, pred1, size, cudaMemcpyHostToDevice);
						if(ANDlogic)
							bpredsnormal<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
						else
							bpredsorlogic<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
					}
				}
				else
				{
					if(npred1.x > 0)
					{
						size = npred1.x * sizeof(int);
						cudaMemcpy(dcons, pred1, size, cudaMemcpyHostToDevice);
						if(ANDlogic)
							bpredsnormal2<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);
						else
							bpredsorlogic2<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, npred1.x, temp + 1);	
					}
				}
			}
		}

		if(temp2 != NULL || npred1.x > 0 || nsel1 > 0 || nsj1 > 0)
		{
			thrust::inclusive_scan(res + 1, res + newLen, res + 1);
			newLen = res[rLen];
			if(newLen == 0)
			{
				cudaFree(temp);
				cudaFree(dcons);
				cudaFree(d_S);
				if(posS != NULL)
					cudaFree(posS);
				return 0;
			}	

			extraspace = TREE_NODE_SIZE - newLen % TREE_NODE_SIZE;
			sizextra = extraspace * sizeof(int);
			m32rLen = newLen + extraspace;
			sizem32 = m32rLen * sizeof(int);

			reservar(&d_R, sizem32);
			reservar(&posR, sizem32);
			cudaMemsetAsync(d_R + newLen, 0x7f, sizextra);
			cudaMemsetAsync(posR + newLen, 0x7f, sizextra);
			llenar<<<blockllen, numthreads>>>(p1, d_R, rLen, of1, wherej[0], temp, posR);
			rLen = newLen;
		}
		else
		{
			sizem32 = m32rLen * sizeof(int);
			reservar(&d_R, sizem32);
			cudaMemsetAsync(d_R + rLen, 0x7f, extraspace * sizeof(int));
			llenarnosel<<<blockllen, numthreads>>>(p1, d_R, rLen, of1, wherej[0]);
		}
	}
	else
	{
		sizem32 = m32rLen * sizeof(int);
		reservar(&d_R, sizem32);
		cudaMemsetAsync(d_R + rLen, 0x7f, extraspace * sizeof(int));
		llenarnosel<<<blockllen, numthreads>>>(p1, d_R, rLen, of1, wherej[0]);
	}

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	//cout << "Select2 = " << time << endl;
	cuda_stats.select2_time += time;
	#endif

	#ifdef TIMER
	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	thrust::device_ptr<Record> dvp1;
	thrust::device_ptr<Record> permutation;
	if(negative)
	{
		dvp1 = thrust::device_pointer_cast(d_S);
		if(posS == NULL)
		{
			reservar(&posS, sizem32S);
			permutation = thrust::device_pointer_cast(posS);
			thrust::sequence(permutation, permutation + m32sLen);
		}
		else
			permutation = thrust::device_pointer_cast(posS);

		flag = 0;
		while(flag != 1)
		{
			try
			{
				thrust::stable_sort_by_key(dvp1, dvp1 + m32sLen, permutation);
				flag = 1;
			}
			catch(std::bad_alloc &e)
			{
				limpiar("inclusive scan in join", 0);
			}
		}
	}
	else
	{
		dvp1 = thrust::device_pointer_cast(d_R);
		if(posR == NULL)
		{
			reservar(&posR, sizem32);
			permutation = thrust::device_pointer_cast(posR);
			thrust::sequence(permutation, permutation + m32rLen);
		}
		else
			permutation = thrust::device_pointer_cast(posR);

		flag = 0;
		while(flag != 1)
		{
			try
			{
				thrust::stable_sort_by_key(dvp1, dvp1 + m32rLen, permutation);
				flag = 1;
			}
			catch(std::bad_alloc &e)
			{
				limpiar("inclusive scan in join", 0);
			}
		}
	}

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	//cout << "Sort = " << time << endl;
	cuda_stats.sort_time += time;
	
	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	IDataNode* d_data;
	IDirectoryNode* d_dir;
	unsigned int nDataNodes;

	if(negative)
	{
		nDataNodes = uintCeilingDiv(sLen, TREE_NODE_SIZE);
		d_data=(IDataNode *)d_S;
	}
	else
	{
		nDataNodes = uintCeilingDiv(rLen, TREE_NODE_SIZE);
		d_data=(IDataNode *)d_R;
	}

	unsigned int lvlDir = uintCeilingLog(TREE_FANOUT, nDataNodes);
	unsigned int nDirNodes = uintCeilingDiv(nDataNodes - 1, TREE_NODE_SIZE);
	unsigned int tree_size = nDirNodes + nDataNodes;
	unsigned int bottom_start = (uintPower(TREE_FANOUT, lvlDir) - 1) / TREE_NODE_SIZE;
	d_dir = (IDirectoryNode *)temp;

	unsigned int nNodesPerBlock = uintCeilingDiv(nDirNodes, BLCK_PER_GRID_create);

	dim3 Dbc(THRD_PER_BLCK_create, 1, 1);
	dim3 Dgc(BLCK_PER_GRID_create, 1, 1);

	gCreateIndex <<<Dgc, Dbc>>> (d_data, d_dir, nDirNodes, tree_size, bottom_start, nNodesPerBlock);

	int *d_locations;
	int memSizeR;
	unsigned int nSearchKeys;
	if(negative)
	{
		memSizeR = (rLen + 1) * sizeof(int);
		reservar(&d_locations, memSizeR);
		cudaMemsetAsync(d_locations, 0, sizeof(int));
		nSearchKeys = rLen;
	}
	else
	{
		memSizeS = sLen * sizeof(int);
		reservar(&d_locations, memSizeS);
		nSearchKeys = sLen;
	}
	dim3 Dbs(THRD_PER_BLCK_search, 1, 1);
	dim3 Dgs(BLCK_PER_GRID_search, 1, 1);
	unsigned int nKeysPerThread = uintCeilingDiv(nSearchKeys, THRD_PER_GRID_search);
	if(negative)
	{
		gSearchTree <<<Dgs, Dbs>>> (d_data, nDataNodes, d_dir, nDirNodes, lvlDir, d_R, d_locations + 1, nSearchKeys, nKeysPerThread, tree_size, bottom_start);
		cudaMemsetAsync(temp, 0, memSizeR);
	}
	else
	{
		gSearchTree <<<Dgs, Dbs>>> (d_data, nDataNodes, d_dir, nDirNodes, lvlDir, d_S, d_locations, nSearchKeys, nKeysPerThread, tree_size, bottom_start);
		cudaMemsetAsync(temp, 0, memSizeS);
	}

	int muljoin = 0, muljoinsize = 0, sum;
	int *d_Rout;
	int resSize, sizepro;
	if(negative)
	{
		blockllen = rLen / numthreads + 1;
		if(numj > 2)
		{
			muljoin = numj - 2;
			muljoinsize = muljoin * sizeof(int);
			cudaMemcpy(dcons, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
			gIndexMultiJoinNegative<<<blockllen, numthreads, muljoinsize>>> (d_R, d_S, d_locations + 1, rLen, p1, p2, of1, of2, posR, posS, dcons, muljoin);
		}

		res = thrust::device_pointer_cast(d_locations);	
		thrust::transform(res + 1, res + rLen + 1, res + 1, to_neg());
		thrust::inclusive_scan(res + 1, res + rLen + 1, res + 1);
		sum = res[rLen];

		if(pos == (rule->num_rows - 3))
		{
			sizepro = rule->num_columns * sizeof(int);
			cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
			resSize = sum * sizepro;
			reservar(&d_Rout, resSize);
			gJoinWithWriteNegative2<<<blockllen, numthreads, sizepro>>> (d_locations, rLen, d_Rout, p1, of1, dcons, rule->num_columns, posR);
		}
		else
		{	
			sizepro = projp.x * sizeof(int);
			cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
			resSize = sum * sizepro;
			reservar(&d_Rout, resSize);
			gJoinWithWriteNegative<<<blockllen, numthreads, sizepro>>> (d_locations, rLen, d_Rout, p1, of1, dcons, projp.x, posR);
		}
		cudaFree(d_R);
		cudaFree(d_S);
	}
	else
	{
		blockllen = sLen / numthreads + 1;
		if(numj > 2)
		{
			muljoin = numj - 2;
			muljoinsize = muljoin * sizeof(int);
			cudaMemcpy(dcons, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
			gIndexMultiJoin<<<blockllen, numthreads, muljoinsize>>> (d_R, d_S, d_locations, sLen, temp, p1, p2, of1, of2, posR, posS, dcons, muljoin);
		}
		else
			gIndexJoin<<<blockllen, numthreads>>> (d_R, d_S, d_locations, sLen, temp);
		cudaFree(d_R);
		cudaFree(d_S);

		sum = res[sLen-1];
		thrust::exclusive_scan(res, res + sLen, res);
		sum += res[sLen-1];
		if(sum == 0)
		{
			cudaFree(dcons);
			cudaFree(d_locations);
			cudaFree(temp);
			if(posS != NULL)
				cudaFree(posS);
			if(posR != NULL)
				cudaFree(posR);
			return 0;
		}
		res[sLen] = sum;

		if(pos == (rule->num_rows - 3))
		{
			sizepro = rule->num_columns * sizeof(int);
			cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
			resSize = sum * sizepro;
			reservar(&d_Rout, resSize);
			if(numj > 2)
			{
				cudaMemcpy(dcons + rule->num_columns, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
				multiJoinWithWrite2<<<blockllen, numthreads, sizepro + muljoinsize>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, rule->num_columns, posR, posS, muljoin);
			}
			else
				gJoinWithWrite2<<<blockllen, numthreads, sizepro>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, rule->num_columns, posR, posS);
		}
		else
		{
			sizepro = projp.y * sizeof(int);
			cudaMemcpy(dcons, proj, sizepro, cudaMemcpyHostToDevice);
			resSize = sum * sizepro;
			reservar(&d_Rout, resSize);
			if(numj > 2)
			{
				cudaMemcpy(dcons + projp.y, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
				multiJoinWithWrite<<<blockllen, numthreads, sizepro + muljoinsize>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, projp.x, projp.y, posR, posS, muljoin);
			}
			else
				gJoinWithWrite<<<blockllen, numthreads, sizepro>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, projp.x, projp.y, posR, posS);
		}
	}

	cudaFree(dcons);
	cudaFree(d_locations);
	cudaFree(temp);
	if(posS != NULL)
		cudaFree(posS);
	if(posR != NULL)
		cudaFree(posR);
	
	if(*ret != NULL)
		cudaFree(*ret);
	*ret = d_Rout;

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	//cout << "Join = " << time << endl;
	//cout << "FIN" << endl;
	cuda_stats.join_time += time;
	#endif

	return sum;
}
