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

typedef struct n4
{
	int v[4];
}s4;

typedef struct n5
{
	int v[5];
}s5;

typedef struct n6
{
	int v[6];
}s6;

typedef struct n7
{
	int v[7];
}s7;

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

struct p4
{
	__host__ __device__
    	bool operator()(const s4 &r1, const s4 &r2)
    	{
     		int x;
		for(x = 0; x < 4; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o4
{
	__host__ __device__
    	bool operator()(const s4 &r1, const s4 &r2)
    	{
     		int x;
		for(x = 0; x < 4; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
    	}
};

struct p5
{
	__host__ __device__
    	bool operator()(const s5 &r1, const s5 &r2)
    	{
     		int x;
		for(x = 0; x < 5; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o5
{
	__host__ __device__
    	bool operator()(const s5 &r1, const s5 &r2)
    	{
     		int x;
		for(x = 0; x < 5; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
    	}
};

struct p6
{
	__host__ __device__
    	bool operator()(const s6 &r1, const s6 &r2)
    	{
     		int x;
		for(x = 0; x < 6; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o6
{
	__host__ __device__
    	bool operator()(const s6 &r1, const s6 &r2)
    	{
     		int x;
		for(x = 0; x < 6; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
    	}
};

struct p7
{
	__host__ __device__
    	bool operator()(const s7 &r1, const s7 &r2)
    	{
     		int x;
		for(x = 0; x < 7; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
    	}
};

struct o7
{
	__host__ __device__
    	bool operator()(const s7 &r1, const s7 &r2)
    	{
     		int x;
		for(x = 0; x < 7; x++)
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
	int flag, nrows;

#if TIMER
	cuda_stats.unions++;
#endif
	switch(tipo)
	{
		case 1: 
		{
			thrust::device_ptr<int> pt, re;

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
					limpiar("sort/unique in unir", 0);
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
			thrust::device_ptr<s2> pt2, re2;
			s2 *t2;
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
					limpiar("sort/unique in unir", 0);
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
			thrust::device_ptr<s3> pt3, re3;
			s3 *t3;
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
					limpiar("sort/unique in unir", 0);
				}				
			}
			nrows = thrust::distance(pt3, re3);
			thrust::device_vector<s3> iVec(pt3, pt3 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
		case 4: 
		{
			thrust::device_ptr<s4> pt4, re4;
			s4 *t4;
			t4 = (s4*)res;
			pt4 = thrust::device_pointer_cast(t4);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt4, pt4 + rows, o4());
					re4 = thrust::unique(pt4, pt4 + rows, p4());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}				
			}
			nrows = thrust::distance(pt4, re4);
			thrust::device_vector<s4> iVec(pt4, pt4 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
		case 5: 
		{
			thrust::device_ptr<s5> pt5, re5;
			s5 *t5;
			t5 = (s5*)res;
			pt5 = thrust::device_pointer_cast(t5);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt5, pt5 + rows, o5());
					re5 = thrust::unique(pt5, pt5 + rows, p5());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}				
			}
			nrows = thrust::distance(pt5, re5);
			thrust::device_vector<s5> iVec(pt5, pt5 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
		case 6: 
		{
			thrust::device_ptr<s6> pt6, re6;
			s6 *t6;
			t6 = (s6*)res;
			pt6 = thrust::device_pointer_cast(t6);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt6, pt6 + rows, o6());
					re6 = thrust::unique(pt6, pt6 + rows, p6());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}				
			}
			nrows = thrust::distance(pt6, re6);
			thrust::device_vector<s6> iVec(pt6, pt6 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
		case 7: 
		{
			thrust::device_ptr<s7> pt7, re7;
			s7 *t7;
			t7 = (s7*)res;
			pt7 = thrust::device_pointer_cast(t7);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt7, pt7 + rows, o7());
					re7 = thrust::unique(pt7, pt7 + rows, p7());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}				
			}
			nrows = thrust::distance(pt7, re7);
			thrust::device_vector<s7> iVec(pt7, pt7 + rows);
			iVec.resize(nrows);
			iVec.shrink_to_fit();
			return nrows;
		}
	default:
	  cerr << "Union: " << tipo << " columns are too many." << endl;
	  exit(1);
	}
	return 0;
}
