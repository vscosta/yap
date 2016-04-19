/*Computer generated file to remove duplicates. Since Thrust's unique and sort, unlike their std's counterparts, don't have a way to specify the size of each element in
the array, comparing pairs, triplets and other sets is not possible without defining a new pointer and all related operations for each set. If you have a better idea to do
this, please don't hesitate to email us.*/

#include <thrust/device_vector.h>
#include <thrust/unique.h>
#include <thrust/distance.h>
#include <thrust/sort.h>
#include <iostream>
#include "memory.h"
#include "union2.h"

int unir(int *res, int rows, int tipo, int **ret, int final)
{
	thrust::device_ptr<int> pt, re;
	thrust::device_ptr<s2> pt2, re2;
	thrust::device_ptr<s3> pt3, re3;
	thrust::device_ptr<s4> pt4, re4;
	thrust::device_ptr<s5> pt5, re5;
	thrust::device_ptr<s6> pt6, re6;
	thrust::device_ptr<s7> pt7, re7;
	thrust::device_ptr<s8> pt8, re8;
	thrust::device_ptr<s9> pt9, re9;
	thrust::device_ptr<s10> pt10, re10;
	thrust::device_ptr<s11> pt11, re11;
	thrust::device_ptr<s12> pt12, re12;
	thrust::device_ptr<s13> pt13, re13;
	thrust::device_ptr<s14> pt14, re14;
	thrust::device_ptr<s15> pt15, re15;
	thrust::device_ptr<s16> pt16, re16;
	thrust::device_ptr<s17> pt17, re17;
	thrust::device_ptr<s18> pt18, re18;
	thrust::device_ptr<s19> pt19, re19;
	thrust::device_ptr<s20> pt20, re20;
	s2 *t2;
	s3 *t3;
	s4 *t4;
	s5 *t5;
	s6 *t6;
	s7 *t7;
	s8 *t8;
	s9 *t9;
	s10 *t10;
	s11 *t11;
	s12 *t12;
	s13 *t13;
	s14 *t14;
	s15 *t15;
	s16 *t16;
	s17 *t17;
	s18 *t18;
	s19 *t19;
	s20 *t20;
	int flag, nrows, *nres, size;

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
					if(final)
					{
						re = thrust::unique(pt, pt + rows, q1());
						re = thrust::unique(pt, re);
					}
					else
						re = thrust::unique(pt, pt + rows);
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt, re);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 2:
		{
			t2 = (s2*)res;
			pt2 = thrust::device_pointer_cast(t2);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt2, pt2 + rows, o2());
					if(final)
					{
						re2 = thrust::unique(pt2, pt2 + rows, q2());
						re2 = thrust::unique(pt2, re2, p2());
					}
					else
						re2 = thrust::unique(pt2, pt2 + rows, p2());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt2, re2);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
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
					if(final)
					{
						re3 = thrust::unique(pt3, pt3 + rows, q3());
						re3 = thrust::unique(pt3, re3, p3());
					}
					else
						re3 = thrust::unique(pt3, pt3 + rows, p3());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt3, re3);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 4:
		{
			t4 = (s4*)res;
			pt4 = thrust::device_pointer_cast(t4);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt4, pt4 + rows, o4());
					if(final)
					{
						re4 = thrust::unique(pt4, pt4 + rows, q4());
						re4 = thrust::unique(pt4, re4, p4());
					}
					else
						re4 = thrust::unique(pt4, pt4 + rows, p4());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt4, re4);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 5:
		{
			t5 = (s5*)res;
			pt5 = thrust::device_pointer_cast(t5);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt5, pt5 + rows, o5());
					if(final)
					{
						re5 = thrust::unique(pt5, pt5 + rows, q5());
						re5 = thrust::unique(pt5, re5, p5());
					}
					else
						re5 = thrust::unique(pt5, pt5 + rows, p5());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt5, re5);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 6:
		{
			t6 = (s6*)res;
			pt6 = thrust::device_pointer_cast(t6);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt6, pt6 + rows, o6());
					if(final)
					{
						re6 = thrust::unique(pt6, pt6 + rows, q6());
						re6 = thrust::unique(pt6, re6, p6());
					}
					else
						re6 = thrust::unique(pt6, pt6 + rows, p6());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt6, re6);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 7:
		{
			t7 = (s7*)res;
			pt7 = thrust::device_pointer_cast(t7);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt7, pt7 + rows, o7());
					if(final)
					{
						re7 = thrust::unique(pt7, pt7 + rows, q7());
						re7 = thrust::unique(pt7, re7, p7());
					}
					else
						re7 = thrust::unique(pt7, pt7 + rows, p7());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt7, re7);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 8:
		{
			t8 = (s8*)res;
			pt8 = thrust::device_pointer_cast(t8);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt8, pt8 + rows, o8());
					if(final)
					{
						re8 = thrust::unique(pt8, pt8 + rows, q8());
						re8 = thrust::unique(pt8, re8, p8());
					}
					else
						re8 = thrust::unique(pt8, pt8 + rows, p8());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt8, re8);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 9:
		{
			t9 = (s9*)res;
			pt9 = thrust::device_pointer_cast(t9);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt9, pt9 + rows, o9());
					if(final)
					{
						re9 = thrust::unique(pt9, pt9 + rows, q9());
						re9 = thrust::unique(pt9, re9, p9());
					}
					else
						re9 = thrust::unique(pt9, pt9 + rows, p9());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt9, re9);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 10:
		{
			t10 = (s10*)res;
			pt10 = thrust::device_pointer_cast(t10);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt10, pt10 + rows, o10());
					if(final)
					{
						re10 = thrust::unique(pt10, pt10 + rows, q10());
						re10 = thrust::unique(pt10, re10, p10());
					}
					else
						re10 = thrust::unique(pt10, pt10 + rows, p10());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt10, re10);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 11:
		{
			t11 = (s11*)res;
			pt11 = thrust::device_pointer_cast(t11);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt11, pt11 + rows, o11());
					if(final)
					{
						re11 = thrust::unique(pt11, pt11 + rows, q11());
						re11 = thrust::unique(pt11, re11, p11());
					}
					else
						re11 = thrust::unique(pt11, pt11 + rows, p11());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt11, re11);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 12:
		{
			t12 = (s12*)res;
			pt12 = thrust::device_pointer_cast(t12);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt12, pt12 + rows, o12());
					if(final)
					{
						re12 = thrust::unique(pt12, pt12 + rows, q12());
						re12 = thrust::unique(pt12, re12, p12());
					}
					else
						re12 = thrust::unique(pt12, pt12 + rows, p12());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt12, re12);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 13:
		{
			t13 = (s13*)res;
			pt13 = thrust::device_pointer_cast(t13);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt13, pt13 + rows, o13());
					if(final)
					{
						re13 = thrust::unique(pt13, pt13 + rows, q13());
						re13 = thrust::unique(pt13, re13, p13());
					}
					else
						re13 = thrust::unique(pt13, pt13 + rows, p13());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt13, re13);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 14:
		{
			t14 = (s14*)res;
			pt14 = thrust::device_pointer_cast(t14);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt14, pt14 + rows, o14());
					if(final)
					{
						re14 = thrust::unique(pt14, pt14 + rows, q14());
						re14 = thrust::unique(pt14, re14, p14());
					}
					else
						re14 = thrust::unique(pt14, pt14 + rows, p14());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt14, re14);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 15:
		{
			t15 = (s15*)res;
			pt15 = thrust::device_pointer_cast(t15);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt15, pt15 + rows, o15());
					if(final)
					{
						re15 = thrust::unique(pt15, pt15 + rows, q15());
						re15 = thrust::unique(pt15, re15, p15());
					}
					else
						re15 = thrust::unique(pt15, pt15 + rows, p15());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt15, re15);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 16:
		{
			t16 = (s16*)res;
			pt16 = thrust::device_pointer_cast(t16);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt16, pt16 + rows, o16());
					if(final)
					{
						re16 = thrust::unique(pt16, pt16 + rows, q16());
						re16 = thrust::unique(pt16, re16, p16());
					}
					else
						re16 = thrust::unique(pt16, pt16 + rows, p16());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt16, re16);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 17:
		{
			t17 = (s17*)res;
			pt17 = thrust::device_pointer_cast(t17);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt17, pt17 + rows, o17());
					if(final)
					{
						re17 = thrust::unique(pt17, pt17 + rows, q17());
						re17 = thrust::unique(pt17, re17, p17());
					}
					else
						re17 = thrust::unique(pt17, pt17 + rows, p17());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt17, re17);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 18:
		{
			t18 = (s18*)res;
			pt18 = thrust::device_pointer_cast(t18);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt18, pt18 + rows, o18());
					if(final)
					{
						re18 = thrust::unique(pt18, pt18 + rows, q18());
						re18 = thrust::unique(pt18, re18, p18());
					}
					else
						re18 = thrust::unique(pt18, pt18 + rows, p18());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt18, re18);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 19:
		{
			t19 = (s19*)res;
			pt19 = thrust::device_pointer_cast(t19);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt19, pt19 + rows, o19());
					if(final)
					{
						re19 = thrust::unique(pt19, pt19 + rows, q19());
						re19 = thrust::unique(pt19, re19, p19());
					}
					else
						re19 = thrust::unique(pt19, pt19 + rows, p19());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt19, re19);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
		case 20:
		{
			t20 = (s20*)res;
			pt20 = thrust::device_pointer_cast(t20);
			flag = 0;
			while(flag != 1)
			{
				try
				{
					thrust::sort(pt20, pt20 + rows, o20());
					if(final)
					{
						re20 = thrust::unique(pt20, pt20 + rows, q20());
						re20 = thrust::unique(pt20, re20, p20());
					}
					else
						re20 = thrust::unique(pt20, pt20 + rows, p20());
					flag = 1;
				}
				catch(std::bad_alloc &e)
				{
					limpiar("sort/unique in unir", 0);
				}
			}
			nrows = thrust::distance(pt20, re20);
			if(nrows < rows / 2)
			{
				size = nrows * tipo * sizeof(int);
				reservar(&nres, size);
				cudaMemcpyAsync(nres, res, size, cudaMemcpyDeviceToDevice);
				cudaFree(*ret);
				*ret = nres;
			}
			return nrows;
		}
	}
	return 0;
}
