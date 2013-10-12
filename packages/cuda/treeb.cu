#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/sequence.h>
#include <thrust/sort.h>
#include <thrust/gather.h>
#include <thrust/scan.h>
#include <iostream>
#include <cstdarg>
#include <cstdio>
#include "lista.h"
//#include "scanImpl.cu"

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

__global__ void gIndexJoin(int *R, int *S, int g_locations[], int sLen, int g_ResNums[])
{
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;

	if(s_cur < sLen) 
	{
		int count = 1;
		int r_cur = g_locations[s_cur];
		int s_key;
		if(r_cur >= 0) /*&& r_cur < rLen) Tal vez la segunda parte no sea necesaria*/
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

__global__ void gIndexMultiJoin(int *R, int *S, int g_locations[], int sLen, int g_ResNums[], int *p1, int *p2, int of1, int of2, int *mloc, int *sloc, int *muljoin, int wj)
{
	extern __shared__ int shared[];
	int s_cur = blockIdx.x * blockDim.x + threadIdx.x;
	int posr, poss, x, y, temp, ini;

	if(threadIdx.x < wj)
		shared[threadIdx.x] = muljoin[threadIdx.x];
	__syncthreads();

	if(s_cur < sLen) 
	{
		int count = 1;
		int r_cur = g_locations[s_cur];
		int s_key;
		if(r_cur >= 0) /*&& r_cur < rLen) Tal vez la segunda parte no sea necesaria*/
		{
			s_key = S[s_cur];				
			r_cur++;
			while(s_key == R[r_cur]) 
			{
				count++;
				r_cur++;
			}
			if(sloc == NULL)
				poss = s_cur * of2;
			else
				poss = sloc[s_cur] * of2;
			ini = r_cur - count;			
			for(x = 0; x < wj; x += 2)
			{
				posr = shared[x];
				temp = p2[poss + shared[x+1]];
				for(y = ini; y < r_cur; y++)
				{
					if(p1[mloc[y] * of1 + posr] != temp)
						count--;
				}
			}
			if(count > 0)
				g_ResNums[s_cur] = count;
		}
		
	}
}

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
				for(y = 0; y < wj; y += 2)
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
				for(y = 0; y < wj; y += 2)
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

int compare2 (const void * a, const void * b)
{
  return ( ((int2*)a)->y - ((int2*)b)->y );
}

/*void generateSort(Record *R, int maxmax, int rLen, int seed)
{
	int i=0;
	const int offset=(1<<15)-1;
	srand(seed);
	for(i=0;i<rLen;i++)
	{
		R[i].y=((((rand()& offset)<<15)+(rand()&1))+(rand()<<1)+(rand()&1))%maxmax;
		
	}
	qsort(R,rLen,sizeof(Record),compare);
	for(i=0;i<rLen;i++)
	R[i].x=i;

}

void generateRand(Record *R, int maxmax, int rLen, int seed)
{
	int i=0;
	const int offset=(1<<15)-1;
	srand(seed);
	for(i=0;i<rLen;i++)
	{
		R[i].y=((((rand()& offset)<<15)+(rand()&1))+(rand()<<1)+(rand()&1))%maxmax;
		//R[i].x=i+1;
		R[i].x=i;
	}
}*/

__global__ void llenar(int *p, int *R, int len, int of, int wj, int *pos, int *ids)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int cond;
	if(id < len)
	{
		cond = pos[id+1];
		if(pos[id] != cond && cond > 0)
		{
			R[cond-1] = p[id * of + wj];
			ids[cond-1] = id;
		}
	}
}

__global__ void llenar2(int *p, int *R, int len, int of, int wj, int *pos)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int cond;
	if(id < len)
	{
		cond = pos[id+1];
		if(pos[id] != cond && cond > 0)
			R[cond-1] = p[id * of + wj];
	}
}

__global__ void llenarnosel(int *p, int *R, int len, int of, int wj)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	if(id < len)
		R[id] = p[id * of + wj];
}

/*__global__ smalljoinc(int *p1, int *p2, int rLen, int sLen, int2 wj, int *r)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	if(id < rlen)
	{
		int comp = p1[id * of1 + wj.x];
		int x, cnt = 0;
		for(x = 0; x < sLen; x++)
		{
			if(comp == p2[x * of2 ])
				cnt++;
		}
		r[id] = cnt;
	}
}

__global__ smalljoinw(int *p1, int *p2, int wj, int *r)
{
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	if(id < rlen)
	{
		int comp = p1[id];
		int x;
		for(x = 0; x < sLen; x++)
		{
			if(comp == p2[x])
				cnt++;
		}
		r[id] = cnt;
	}
}*/

int buscarunion(int *tmprule, int tmplen, int *rule, int pos2, int tam2, int *joins)
{
	int x, y;
	int cont = 0;
	for(x = 0; x < tmplen; x++)
	{
		for(y = pos2; y < (pos2 + tam2); y++)
		{
			if(tmprule[x] == rule[y])
			{
				joins[cont] = x;
				cont++;
				joins[cont] = y - pos2;
				cont++;
			}
		}
	}
	return cont;
}

int not_in(int *rule, int len, int bus)
{
	int x;
	for(x = 0; x < len; x++)
	{
		if(rule[x] == bus)
			return 0;
	}
	return 1;
}

int posiciones(int ini, int of1, int *firstpart, int *hpos, int *hcons, int *rule, int *temprule, int *tmprulpos, int *lenrul, int pos2, int of2, int posf)
{
	int cont = 0, x, y;
	for(y = ini; y < of1; y++)
	{
		if(firstpart[y] < 0)
		{
			hpos[cont] = y - ini;
			hcons[cont] = -firstpart[y];
			cont++;
		}
		else
		{
			x = 1;
			while(rule[x] != 0)
			{
				if(firstpart[y] == rule[x])
				{
					if(not_in(temprule, *lenrul, firstpart[y]))
					{
						temprule[*lenrul] = firstpart[y];
						tmprulpos[*lenrul] = y - ini;
						*lenrul = *lenrul + 1;
					}
					break;
				}
				x++;
			}
			if(rule[x] != 0)
				continue;
			for(x = (pos2 + of2 + 1); x < posf; x++)
			{
				if(rule[x] == 0)
				{
					x++;
					continue;
				}
				if(firstpart[y] == rule[x])
				{
					if(not_in(temprule, *lenrul, firstpart[y]))
					{
						temprule[*lenrul] = firstpart[y];
						tmprulpos[*lenrul] = y - ini;
						*lenrul = *lenrul + 1;
					}
					break;
				}
			}
		}
	}	
	return cont;
}

void join_final(int cols, int of1, int of2, int *rule, int *firstpart, int *secondpart, int *tmprulpos)
{
	int x, y;
	for(y = 0; y < cols; y++)
	{
		for(x = 0; x < of1; x++)
		{
			if(rule[y] == firstpart[x])
			{
				tmprulpos[y] = x + 1;
				break;
			}
		}
		if(x != of1)
			continue;
		for(x = 0; x < of2; x++)
		{
			if(rule[y] == secondpart[x])
			{
				tmprulpos[y] = -x - 1;
				break;
			}
		}
	}
}

int select_pos(int of1, int *firstpart, int *hpos, int *hcons)
{
	int cont = 0, y;
	for(y = 0; y < of1; y++)
	{
		if(firstpart[y] < 0)
		{
			hpos[cont] = y;
			hcons[cont] = -firstpart[y];
			cont++;
		}
	}
	return cont;
}

int checkquery(int *hpos, int *hcons, int cont, int *pred, int of1, int *rule, int *query, int cols)
{
	int x, y;
	if(rule[0] != query[0])
		return cont;
	for(x = 1; x <= cols; x++)
	{
		if(query[x] < 0)
		{
			for(y = 0; y < of1; y++)
			{
				if(pred[y] == rule[x])
				{
					hpos[cont] = y;
					hcons[cont] = -query[x];
					cont++;
				}
			}
		}
	}
	return cont;
}

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

template <typename KeyVector, typename PermutationVector, typename TempVector>
void update_permutation(KeyVector& keys, PermutationVector& permutation, TempVector& temporary, int rows)
{
    // permute the keys with the current reordering
	thrust::gather(permutation, permutation + rows, keys, temporary);

    // stable_sort the permuted keys and update the permutation
	thrust::stable_sort_by_key(temporary, temporary + rows, permutation);
}


template <typename KeyVector, typename PermutationVector, typename TempVector>
void apply_permutation(KeyVector& keys, PermutationVector& permutation, TempVector& temporary, int rows)
{
    // permute the keys
    thrust::gather(permutation, permutation + rows, temporary, keys);
}

int join(int *p1, int *p2, int rLen, int sLen, int of1, int of2, list<rulenode>::iterator rule, int pos, int bothops, int **ret)
{
	int pos2 = pos + 1;
	int *sel1, nsel1;
	int *sel2 = rule->select[pos2];
	int nsel2 = rule->numsel[pos2];
	int *proj = rule->project[pos];
	int2 projp = rule->projpos[pos];
	int *sjoin1, nsj1;
	int *sjoin2 = rule->selfjoin[pos2];
	int nsj2 = rule->numselfj[pos2];
	int *wherej = rule->wherejoin[pos];
	int numj = rule->numjoin[pos];
	int flag;

	int porLiberar = rLen * of1 * sizeof(int);
	int size, sizet, sizet2;
	if(bothops)
	{
		sel1 = rule->select[pos];
		nsel1 = rule->numsel[pos];
		sjoin1 = rule->selfjoin[pos];
		nsj1 = rule->numselfj[pos];
		sizet = maximo(7, of1, of2, nsel1, nsel2, projp.y + numj - 2, nsj1, nsj2) * sizeof(int);
	}
	else
		sizet = maximo(6, of1, of2, nsel2, projp.y + numj - 2, nsj2, numj) * sizeof(int);
	
	int *dcons, *temp;
	int *d_R, *d_S;
	int blockllen, numthreads;
	
	//int por_liberar = rLen * sizeof(int);
	int extraspace = TREE_NODE_SIZE - rLen % TREE_NODE_SIZE;
	int m32rLen = rLen + extraspace;
	if(m32rLen > sLen)
		sizet2 = (m32rLen + 1) * sizeof(int);
	else
		sizet2 = (sLen + 1) * sizeof(int);

	/*hcons = (int *)malloc(sizet);
	hpos = (int *)malloc(sizet);
	int dconsize = sizet * 2;*/

	reservar(&dcons, sizet);
	reservar(&temp, sizet2);
	thrust::device_ptr<int> res = thrust::device_pointer_cast(temp);

	numthreads = 1024;
	blockllen = sLen / numthreads + 1;
	int memSizeS, newLen;
	int *posR = NULL, *posS = NULL;

	#ifdef TIMER
	cout << "INICIO" << endl;
	cudaEvent_t start, stop;
	float time;
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	//cout << "sLen y rLen = " << sLen << " " << rLen << endl;

	if(nsel2 > 0)
	{
		size = nsel2 * sizeof(int);
		newLen = sLen + 1;
		cudaMemsetAsync(temp, 0, newLen * sizeof(int));
		cudaMemcpy(dcons, sel2, size, cudaMemcpyHostToDevice);
		marcar<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsel2, temp + 1);

		/*int y;
		int *htemp = (int *)malloc(newLen * sizeof(int));
		cout << "temp =" << endl;
		cudaMemcpy(htemp, temp, newLen * sizeof(int), cudaMemcpyDeviceToHost);
		for(y = 0; y < newLen; y++)
			cout << htemp[y] << " ";
		cout << endl;
		free(htemp);*/

		if(nsj2 > 0)
		{
			size = nsj2 * sizeof(int);
			cudaMemcpy(dcons, sjoin2, size, cudaMemcpyHostToDevice);
			samejoin<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsj2, temp + 1);
		}

		/*htemp = (int *)malloc(newLen * sizeof(int));
		cout << "temp =" << endl;
		cudaMemcpy(htemp, temp, newLen * sizeof(int), cudaMemcpyDeviceToHost);
		for(y = 0; y < newLen; y++)
			cout << res[y] << " ";
		cout << endl;
		free(htemp);*/

		/*cout << "Despues de marcar" << endl;
		cout << "newLen = " << newLen << endl;*/

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
				limpiar("inclusive scan in join");
			}				
		}
		//thrust::inclusive_scan(res + 1, res + newLen, res + 1);	
		newLen = res[sLen];
	
		if(newLen == 0)
			return 0;

		memSizeS = newLen * sizeof(int);
		reservar(&d_S, memSizeS);
		reservar(&posS, memSizeS);
		llenar<<<blockllen, numthreads>>>(p2, d_S, sLen, of2, wherej[1], temp, posS);
		sLen = newLen;
	}
	else
	{
		if(nsj2 > 0)
		{
			size = nsj2 * sizeof(int);
			newLen = sLen + 1;
			cudaMemsetAsync(temp, 0, newLen * sizeof(int));
			cudaMemcpy(dcons, sjoin2, size, cudaMemcpyHostToDevice);
			samejoin2<<<blockllen, numthreads, size>>>(p2, sLen, of2, dcons, nsj2, temp + 1);

			thrust::inclusive_scan(res + 1, res + newLen, res + 1);
			newLen = res[sLen];
			if(newLen == 0)
				return 0;

			memSizeS = newLen * sizeof(int);
			reservar(&d_S, memSizeS);
			reservar(&posS, memSizeS);
			llenar<<<blockllen, numthreads>>>(p2, d_S, sLen, of2, wherej[1], temp, posS);
			sLen = newLen;
		}
		else
		{
			memSizeS = sLen * sizeof(int);
			reservar(&d_S, memSizeS);
			llenarnosel<<<blockllen, numthreads>>>(p2, d_S, sLen, of2, wherej[1]);
		}
	}

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	cout << "Select1 = " << time << endl;

	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	blockllen = rLen / numthreads + 1;
	int sizem32, sizextra;
	if(bothops)
	{
		if(nsel1 > 0)
		{
			size = nsel1 * sizeof(int);
			newLen = rLen + 1;
			cudaMemsetAsync(temp, 0, newLen * sizeof(int));
			cudaMemcpy(dcons, sel1, size, cudaMemcpyHostToDevice);
			marcar<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsel1, temp + 1);

			if(nsj1 > 0)
			{
				size = nsj1 * sizeof(int);
				cudaMemcpy(dcons, sjoin1, size, cudaMemcpyHostToDevice);
				samejoin<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsj1, temp + 1);
			}

			thrust::inclusive_scan(res + 1, res + newLen, res + 1);
			newLen = res[rLen];
			if(newLen == 0)
				return 0;		

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
			if(nsj1 > 0)
			{
				size = nsj1 * sizeof(int);
				newLen = rLen + 1;
				cudaMemsetAsync(temp, 0, newLen * sizeof(int));
				cudaMemcpy(dcons, sjoin1, size, cudaMemcpyHostToDevice);
				samejoin2<<<blockllen, numthreads, size>>>(p1, rLen, of1, dcons, nsj1, temp + 1);

				thrust::inclusive_scan(res + 1, res + newLen, res + 1);
				newLen = res[rLen];
				if(newLen == 0)
					return 0;

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
	cout << "Select2 = " << time << endl;
	#endif
	
	/*free(hcons);
	free(hpos);

	h_R = (int *)malloc(sizem32);
	cudaMemcpy(h_R, d_S, memSizeS, cudaMemcpyDeviceToHost);
	cout << "H_S " << "cont " << cont << " sLen " << sLen << endl;
	for(x = 0; x < sLen; x++)
		cout << h_R[x] << endl;
	free(h_R);*/

	#ifdef TIMER
	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	thrust::device_ptr<Record> dvp1 = thrust::device_pointer_cast(d_R);
	thrust::device_ptr<Record> permutation;
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
			limpiar("inclusive scan in join");
		}
	}

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	cout << "Sort = " << time << endl;
	
	cudaEventDestroy(start);
	cudaEventDestroy(stop);
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);
	#endif

	IDataNode* d_data;
	IDirectoryNode* d_dir;
	unsigned int nDataNodes;

	nDataNodes = uintCeilingDiv(rLen, TREE_NODE_SIZE);
	d_data=(IDataNode *)d_R;

	unsigned int lvlDir = uintCeilingLog(TREE_FANOUT, nDataNodes);
	unsigned int nDirNodes = uintCeilingDiv(nDataNodes - 1, TREE_NODE_SIZE);
	unsigned int tree_size = nDirNodes + nDataNodes;
	unsigned int bottom_start = (uintPower(TREE_FANOUT, lvlDir) - 1) / TREE_NODE_SIZE;
	d_dir = (IDirectoryNode *)temp;

	unsigned int nNodesPerBlock = uintCeilingDiv(nDirNodes, BLCK_PER_GRID_create);

	dim3 Dbc(THRD_PER_BLCK_create, 1, 1);
	dim3 Dgc(BLCK_PER_GRID_create, 1, 1);

	gCreateIndex <<<Dgc, Dbc>>> (d_data, d_dir, nDirNodes, tree_size, bottom_start, nNodesPerBlock);

	/*int y;
	IDirectoryNode *h_dir = (IDirectoryNode*)malloc(sizeof(IDirectoryNode) * nDirNodes);
	cudaMemcpy(h_dir, d_dir, sizeof(IDirectoryNode) * nDirNodes, cudaMemcpyDeviceToHost);
	for(x = 0; x < nDirNodes; x++)
	{
		for(y = 0; y < TREE_NODE_SIZE; y++)
			printf("%d ", h_dir[x].keys[y]);
		printf("\n");
	}
	free(h_dir);*/

	int *d_locations;
	reservar(&d_locations, memSizeS);

	dim3 Dbs(THRD_PER_BLCK_search, 1, 1);
	dim3 Dgs(BLCK_PER_GRID_search, 1, 1);

	unsigned int nSearchKeys = sLen;
	unsigned int nKeysPerThread = uintCeilingDiv(nSearchKeys, THRD_PER_GRID_search);

	gSearchTree <<<Dgs, Dbs>>> (d_data, nDataNodes, d_dir, nDirNodes, lvlDir, d_S, d_locations, nSearchKeys, nKeysPerThread, tree_size, bottom_start);
	cudaMemsetAsync(temp, 0, memSizeS);

	blockllen = sLen / numthreads + 1;
	int muljoin = 0, muljoinsize = 0;
	if(numj > 2)
	{
		muljoin = numj - 2;
		muljoinsize = muljoin * sizeof(int);
		cudaMemcpy(dcons, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
		gIndexMultiJoin<<<blockllen, numthreads, muljoinsize>>> (d_R, d_S, d_locations, sLen, temp, p1, p2, of1, of2, posR, posS, dcons, muljoin);
	}
	else
		gIndexJoin<<<blockllen, numthreads>>> (d_R, d_S, d_locations, sLen, temp);
	liberar(d_R, sizem32);
	liberar(d_S, memSizeS);

	int sum = res[sLen-1];
	thrust::exclusive_scan(res, res + sLen, res);
	sum += res[sLen-1];
	if(sum == 0)
		return 0;	
	res[sLen] = sum;

	int *d_Rout;
	int resSize, sizepro;
	if(pos == (rule->num_rows - 3) && rule->num_bpreds.x == 0)
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
			cudaMemcpy(dcons + rule->num_columns, wherej + 2, muljoinsize, cudaMemcpyHostToDevice);
			multiJoinWithWrite<<<blockllen, numthreads, sizepro + muljoinsize>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, projp.x, projp.y, posR, posS, muljoin);
		}
		else
			gJoinWithWrite<<<blockllen, numthreads, sizepro>>> (d_locations, sLen, temp, d_Rout, p1, p2, of1, of2, dcons, projp.x, projp.y, posR, posS);
	}

	liberar(dcons, sizet);
	liberar(d_locations, memSizeS);
	liberar(temp, sizet2);
	liberar(posR, sizem32);
	if(posS != NULL)
		liberar(posS, memSizeS);
	
	/*if(posS != NULL)
		liberar(posS, memSizeS);
	liberar(dtmprulpos, sizerul);
	if(*ret != NULL)
		liberar(*ret, por_liberar);
	free(tmprulpos);
	if(final_cond != posf)
	{
		free(*newrule);
		*newrule = (int *)malloc(sizerul);
		memcpy(*newrule, temprule, sizerul);
		*newrullen = lenrul;
	}*/
	
	if(*ret != NULL)
		liberar(*ret, porLiberar);
	*ret = d_Rout;

	#ifdef TIMER
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);
	cout << "Join = " << time << endl;
	cout << "FIN" << endl;
	#endif

	return sum;
}
