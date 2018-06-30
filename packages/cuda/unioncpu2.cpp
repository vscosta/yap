#include <thrust/unique.h>
#include <thrust/distance.h>
#include <thrust/system/omp/execution_policy.h>
#include <iostream>
#include "union2.h"

int unircpu(int *res, int rows, int tipo, int **ret)
{

	//cout << "En union = " << rows << " " << tipo << endl;

	s2 *t2, *re2;
	s3 *t3, *re3;
	int nrows, *nres;
	//int size;

	switch(tipo)
	{
		case 1: 
		{
			thrust::sort(thrust::omp::par, res, res + rows);
			nres = thrust::unique(thrust::omp::par, res, res + rows);
			nrows = thrust::distance(res, nres);
			/*if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				nres = (int *)malloc(size);
				memmove(nres, res, size);
				free(*ret);
				*ret = nres;
			}*/
			return nrows;	
		}			
		case 2: 
		{
			t2 = (s2*)res;
			thrust::sort(thrust::omp::par, t2, t2 + rows, o2());
			re2 = thrust::unique(thrust::omp::par, t2, t2 + rows, p2());
			nrows = thrust::distance(t2, re2);
			/*if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				nres = (int *)malloc(size);
				memmove(nres, res, size);
				free(*ret);
				*ret = nres;
			}*/
			return nrows;
		}
		case 3: 
		{
			t3 = (s3*)res;
			thrust::sort(thrust::omp::par, t3, t3 + rows, o3());
			re3 = thrust::unique(thrust::omp::par, t3, t3 + rows, p3());
			nrows = thrust::distance(t3, re3);
			/*if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				nres = (int *)malloc(size);
				memmove(nres, res, size);
				free(*ret);
				*ret = nres;
			}*/
			return nrows;
		}
	}
	return 0;
}
