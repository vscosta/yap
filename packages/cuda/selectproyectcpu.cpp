#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <vector>
#include "pred.h"

using namespace std;

int selectproyectcpu(int *dop1, int rows, int cols, int head_size, int *select, int numselect, int *selfjoin, int numselfj, int *project, int **ret)
{
	int size = 0, pos, temp;
	int i, x, y, z, w;
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

	if(numselect > 0)
	{	
		#pragma omp parallel for private(x,pos,y,z,fin,temp,w)
		for(i = 0; i < NUM_T; i++)
		{
			fin = ini[i+1];
			for(x = ini[i]; x < fin; x++)
			{
				pos = x * cols;
				for(y = 0; y < numselect; y += 2)
				{
					if(dop1[pos+select[y]] != select[y+1])
						break;
				}
				for(z = 0; z < numselfj; z++)
				{
					temp = dop1[pos+selfjoin[z]];
					w = z + 1;
					while(selfjoin[w] > -1)
					{
						if(temp != dop1[pos+selfjoin[w]])
							break;
						w++;
					}
					z = w;
					if(selfjoin[w] != -1)
						break;
				}
				if(y == numselect && z == numselfj)
				{
					for(y = 0; y < head_size; y++)
						vec[i].push_back(dop1[pos+project[y]]);
				}
			}
		}
	}
	else
	{
		if(numselfj > 0)
		{
			#pragma omp parallel for private(x,pos,y,z,fin,w,temp)
			for(i = 0; i < NUM_T; i++)
			{
				fin = ini[i+1];
				for(x = ini[i]; x < fin; x++)
				{
					pos = x * cols;
					for(z = 0; z < numselfj; z++)
					{
						temp = dop1[pos+selfjoin[z]];
						w = z + 1;
						while(selfjoin[w] > -1)
						{
							if(temp != dop1[pos+selfjoin[w]])
								break;
							w++;
						}
						z = w;
						if(selfjoin[w] != -1)
							break;
					}
					if(z == numselfj)
					{
						for(y = 0; y < head_size; y++)
							vec[i].push_back(dop1[pos+project[y]]);
					}
				}
			}
		}
		else
		{
			fres = (int *)malloc(rows * cols * sizeof(int));
			#pragma omp parallel for private(pos,y,z)
			for(x = 0; x < rows; x++)
			{
				pos = x * cols;
				z = pos;
				for(y = 0; y < head_size; y++, z++)
					fres[z] = dop1[pos+project[y]];
			}
			*ret = fres;
			return rows;
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
	return size / head_size;
}

int selectproyectcpu2(int *dop1, int rows, int cols, int *select, int numselect, int *selfjoin, int numselfj, int wj, int **ret, int **list)
{
	int size = 0, pos, temp;
	int i, x, y, z, w;
	int *fres, *lres, *ptr;
	int div, fin, ini[NUM_T + 1];
	int *vec[NUM_T];
	int *lis[NUM_T];
	int cont[NUM_T];
	
	//omp_set_num_threads(NUM_T);
	div = rows / NUM_T;
	ini[0] = 0;
	for(x = 1; x < NUM_T; x++)
		ini[x] = div * x;
	ini[NUM_T] = rows;

	pos = div + NUM_T;
	for(x = 0; x < NUM_T; x++)
	{
		vec[x] = (int *)malloc(pos * sizeof(int));
		lis[x] = (int *)malloc(pos * sizeof(int));
		cont[x] = 0;
	}

	/*cout << "numselect = " << numselect << endl;
	for(x = 0; x < numselect; x++)
		cout << select[x] << " ";
	cout << endl;*/

	if(numselect > 0)
	{	
		#pragma omp parallel for private(x,pos,y,z,fin,temp,w)
		for(i = 0; i < NUM_T; i++)
		{
			fin = ini[i+1];
			for(x = ini[i]; x < fin; x++)
			{
				pos = x * cols;
				for(y = 0; y < numselect; y += 2)
				{
					if(dop1[pos+select[y]] != select[y+1])
						break;
				}
				for(z = 0; z < numselfj; z++)
				{
					temp = dop1[pos+selfjoin[z]];
					w = z + 1;
					while(selfjoin[w] > -1)
					{
						if(temp != dop1[pos+selfjoin[w]])
							break;
						w++;
					}
					z = w;
					if(selfjoin[w] != -1)
						break;
				}
				if(y == numselect && z == numselfj)
				{
					lis[i][cont[i]] = x;
					if(list != NULL)
						vec[i][cont[i]] = dop1[pos+wj];
					cont[i]++;
				}
			}
		}
	}
	else
	{
		if(numselfj > 0)
		{
			#pragma omp parallel for private(x,pos,y,z,fin,w,temp) firstprivate(cont)
			for(i = 0; i < NUM_T; i++)
			{
				fin = ini[i+1];
				for(x = ini[i]; x < fin; x++)
				{
					pos = x * cols;
					for(z = 0; z < numselfj; z++)
					{
						temp = dop1[pos+selfjoin[z]];
						w = z + 1;
						while(selfjoin[w] > -1)
						{
							if(temp != dop1[pos+selfjoin[w]])
								break;
							w++;
						}
						z = w;
						if(selfjoin[w] != -1)
							break;
					}
					if(z == numselfj)
					{
						lis[i][cont[i]] = x;
						if(list != NULL)
							vec[i][cont[i]] = dop1[pos+wj];
						cont[i]++;
					}
				}
			}
		}
	}

	//cout << "despues sel" << endl;

	for(x = 0; x < NUM_T; x++)
		size += cont[x];
	lres = (int *)malloc(size * sizeof(int));
	ptr = lres;
	for(x = 0; x < NUM_T; x++)
	{
		memmove(ptr, lis[x], cont[x] * sizeof(int));
		ptr += cont[x];
	}
	if(list != NULL)
	{
		fres = (int *)malloc(size * sizeof(int));
		ptr = fres;
		for(x = 0; x < NUM_T; x++)
		{
			memmove(ptr, vec[x], cont[x] * sizeof(int));
			ptr += cont[x];
		}
		*ret = fres;	
		*list = lres;
	}
	else
		*ret = lres;
	for(x = 0; x < NUM_T; x++)
	{
		free(lis[x]);
		free(vec[x]);
	}
	return size;
}

int selectproyectcpu3(int *dop1, int rows, int cols, int *select, int numselect, int *selfjoin, int numselfj, int wj, int **ret, int **list)
{
	int size = 0, pos, temp;
	int i, x, y, z, w;
	int *fres, *lres, *ptr;
	int div, fin, ini[NUM_T + 1];
	vector<int> vec[NUM_T];
	vector<int> lis[NUM_T];

	for(x = 0; x < NUM_T; x++)
	{
		vec[x].reserve(INISIZE);
		lis[x].reserve(INISIZE);
	}
	
	//omp_set_num_threads(NUM_T);
	div = rows / NUM_T;
	ini[0] = 0;
	for(x = 1; x < NUM_T; x++)
		ini[x] = div * x;
	ini[NUM_T] = rows;

	/*cout << "numselect = " << numselect << endl;
	for(x = 0; x < numselect; x++)
		cout << select[x] << " ";
	cout << endl;*/

	if(numselect > 0)
	{	
		#pragma omp parallel for private(x,pos,y,z,fin,temp,w)
		for(i = 0; i < NUM_T; i++)
		{
			fin = ini[i+1];
			for(x = ini[i]; x < fin; x++)
			{
				pos = x * cols;
				for(y = 0; y < numselect; y += 2)
				{
					if(dop1[pos+select[y]] != select[y+1])
						break;
				}
				for(z = 0; z < numselfj; z++)
				{
					temp = dop1[pos+selfjoin[z]];
					w = z + 1;
					while(selfjoin[w] > -1)
					{
						if(temp != dop1[pos+selfjoin[w]])
							break;
						w++;
					}
					z = w;
					if(selfjoin[w] != -1)
						break;
				}
				if(y == numselect && z == numselfj)
				{
					lis[i].push_back(x);
					if(list != NULL)
						vec[i].push_back(dop1[pos+wj]);
				}
			}
		}
	}
	else
	{
		if(numselfj > 0)
		{
			#pragma omp parallel for private(x,pos,y,z,fin,w,temp)
			for(i = 0; i < NUM_T; i++)
			{
				fin = ini[i+1];
				for(x = ini[i]; x < fin; x++)
				{
					pos = x * cols;
					for(z = 0; z < numselfj; z++)
					{
						temp = dop1[pos+selfjoin[z]];
						w = z + 1;
						while(selfjoin[w] > -1)
						{
							if(temp != dop1[pos+selfjoin[w]])
								break;
							w++;
						}
						z = w;
						if(selfjoin[w] != -1)
							break;
					}
					if(z == numselfj)
					{
						lis[i].push_back(x);
						if(list != NULL)
							vec[i].push_back(dop1[pos+wj]);
					}
				}
			}
		}
	}

	//cout << "despues sel" << endl;

	for(x = 0; x < NUM_T; x++)
	{
		ini[x] = lis[x].size();
		size += ini[x];
	}
	lres = (int *)malloc(size * sizeof(int));
	ptr = lres;
	for(x = 0; x < NUM_T; x++)
	{
		memmove(ptr, lis[x].data(), ini[x] * sizeof(int));
		ptr += ini[x];
	}
	if(list != NULL)
	{
		fres = (int *)malloc(size * sizeof(int));
		ptr = fres;
		for(x = 0; x < NUM_T; x++)
		{
			memmove(ptr, vec[x].data(), ini[x] * sizeof(int));
			ptr += ini[x];
		}
		*ret = fres;	
		*list = lres;
	}
	else
		*ret = lres;
	return size;
}
