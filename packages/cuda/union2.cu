#include <thrust/device_vector.h>
#include <thrust/unique.h>
#include <thrust/distance.h>
#include <iostream>

typedef struct n2
{
	int v[2];
}s2;

typedef struct n3
{
	int v[3];
}s3;

struct p2
{
	__host__ __device__
    	bool operator()(const s2 &r1, const s2 &r2)
    	{
     		int x;
		for(x = 0; x < 2; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o2
{
	__host__ __device__
    	bool operator()(const s2 &r1, const s2 &r2)
    	{
     		int x;
		for(x = 0; x < 2; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
    	}
};

struct p3
{
	__host__ __device__
    	bool operator()(const s3 &r1, const s3 &r2)
    	{
     		int x;
		for(x = 0; x < 3; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o3
{
	__host__ __device__
    	bool operator()(const s3 &r1, const s3 &r2)
    	{
     		int x;
		for(x = 0; x < 3; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
    	}
};

int unir(int *res, int rows, int tipo)
{
	thrust::device_ptr<int> pt, re;
	thrust::device_ptr<s2> pt2, re2;
	thrust::device_ptr<s3> pt3, re3;
	s2 *t2;
	s3 *t3;
	int flag, nrows;

#if TIMER
	cuda_stats.unions++;
#endif
	switch(tipo)
	{
		case 1: 
		{
			pt = thrust::device_pointer_cast(res);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt, pt + rows);
					re = thrust::unique(pt, pt + rows);
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir");
				}				
			}
			nrows = thrust::distance(pt, re);
			thrust::device_vector<int> iVec(pt, pt + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;	
		}			
		case 2: 
		{
			t2 = (s2*)res;
			
			/*int *a, x, y;
			a = (int *)malloc(rows * 2 * sizeof(int));
			cudaMemcpy(a, res, rows * 2 * sizeof(int), cudaMemcpyDeviceToHost);
			cout << "INI" << endl;
			for(x = 0; x < rows; x++)
			{
				for(y = 0; y < 2; y++)
					cout << a[x * 2 + y] << " ";
				cout << endl;	
			}
			cout << "INI fin" << endl;
			free(a);*/

			pt2 = thrust::device_pointer_cast(t2);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt2, pt2 + rows, o2());
					re2 = thrust::unique(pt2, pt2 + rows, p2());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir");
				}				
			}
			nrows = thrust::distance(pt2, re2);
			thrust::device_vector<s2> iVec(pt2, pt2 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();

			/*tam = (int)(re2.get() - pt2.get());
			a = (int *)malloc(tam * 2 * sizeof(int));
			cudaMemcpy(a, res, tam * 2 * sizeof(int), cudaMemcpyDeviceToHost);
			cout << "FIN" << endl;
			for(x = 0; x < tam; x++)
			{
				for(y = 0; y < 2; y++)
					cout << a[x * 2 + y] << " ";
				cout << endl;	
			}
			cout << "FIN fin" << endl;
			free(a);
			cout << "antes = " << rows << " despues = " << thrust::distance(pt2, re2) << endl;*/

			return nrows;
		}
		case 3: 
		{
			t3 = (s3*)res;
			pt3 = thrust::device_pointer_cast(t3);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt3, pt3 + rows, o3());
					re3 = thrust::unique(pt3, pt3 + rows, p3());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir");
				}				
			}
			nrows = thrust::distance(pt3, re3);
			thrust::device_vector<s3> iVec(pt3, pt3 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
	}
	return 0;
}
