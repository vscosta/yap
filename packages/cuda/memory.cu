#include <list>
#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <thrust/device_vector.h>
#include "lista.h"
#include "memory.h"

#define MAX_REC 200
#define HALF_REC (MAX_REC / 2)
#define MAX_FIX_POINTS 100

unsigned int avmem;
memnode temp_storage[MAX_REC];
list<memnode> GPUmem;
list<memnode> CPUmem;

bool compareiteration(const memnode &r1, const memnode &r2)
{
	return (r1.iteration < r2.iteration); 
}

bool comparename(const memnode &r1, const memnode &r2)
{
	return (r1.name > r2.name); 
}

void calcular_mem(int dev)
{
	cudaDeviceProp p;
	cudaGetDeviceProperties(&p, dev);
	avmem = p.totalGlobalMem;
	temp_storage[0].dev_address = NULL;
	temp_storage[0].size = 0;
	temp_storage[HALF_REC].dev_address = NULL;
	temp_storage[HALF_REC].size = 0;

	//cout << "Initial memory available " << avmem << endl;
}

template<class InputIterator>
InputIterator buscarhecho(InputIterator first, InputIterator last, int name)
{
	while(first!=last) 
	{
		if(first->name == name) return first;
			++first;
	}
	return last;
}

list<memnode>::iterator buscarpornombre(int name, int itr, int *totalrows, int *gpunum)
{
	int x = 1, sum = 0;
	memnode temp;
	temp.name = name;
	temp.iteration = itr;
	pair<list<memnode>::iterator, list<memnode>::iterator> rec = equal_range(GPUmem.begin(), GPUmem.end(), temp, compareiteration);
	while(rec.first != rec.second)
	{		
		
		//cout << "itr = " << itr << " rec.first = " << rec.first->name << endl;	
		
		if(rec.first->name == name)
		{
			temp_storage[x] = *rec.first;
			rec.first = GPUmem.erase(rec.first);
			sum += temp_storage[x].rows;
			x++;
		}	
		else
			rec.first++;
	}
	//if(x > 1)
	rec.first = GPUmem.insert(rec.first, temp);
	*totalrows = sum;
	*gpunum = x;
	return rec.first;
}

int buscarpornombrecpu(int name, int itr, int *totalrows)
{
	int x = HALF_REC + 1, sum = 0;
	memnode temp;
	temp.iteration = itr;
	pair<list<memnode>::iterator, list<memnode>::iterator> rec = equal_range(CPUmem.begin(), CPUmem.end(), temp, compareiteration);

	/*if(rec.first != rec.second)
		cout << "bscnomcpu = " << rec.first->name << " " << rec.first->iteration << endl;*/

	while(rec.first != rec.second)
	{
		if(rec.first->name == name)
		{
			temp_storage[x] = *rec.first;
			rec.first = CPUmem.erase(rec.first);
			sum += temp_storage[x].rows;
			x++;
		}	
		else
			rec.first++;
	}
	*totalrows += sum;
	return x;
}

void limpiar()
{
	list<memnode>::iterator ini;
	memnode temp;

	if(GPUmem.size() == 0)
	{
		cerr << "Not enough GPU memory: have " << avmem << endl;
		exit(1);
	}		

	ini = GPUmem.begin();
	if(ini->isrule)
	{	
		temp = *ini;
		temp.dev_address = (int *)malloc(ini->size);
		cudaMemcpyAsync(temp.dev_address, ini->dev_address, temp.size, cudaMemcpyDeviceToHost);
		CPUmem.push_back(temp);
	}
	liberar(ini->dev_address, ini->size);
	GPUmem.erase(ini);
}

void limpiartodo(int *p1, int *p2)
{
	list<memnode>::iterator ini;
	memnode temp;
	int cont = 0;
	if(p1 != NULL)
		cont++;	
	if(p2 != NULL)
		cont++;
	ini = GPUmem.begin();

	/*cout << "ANTES" << endl;
	mostrar_memoria();
	mostrar_memcpu();
	cout << "FIN ANTES" << endl;*/
	//cout << "mem = " << GPUmem.size() << " " << avmem << endl;

	while(GPUmem.size() > cont)
	{
		if(ini->dev_address == p1 || ini->dev_address == p2)
		{
			ini++;
			continue;
		}
		if(ini->isrule)
		{
			temp = *ini; 
			temp.dev_address = (int *)malloc(ini->size);
			cudaMemcpy(temp.dev_address, ini->dev_address, temp.size, cudaMemcpyDeviceToHost);
			CPUmem.push_back(temp);
		}
		liberar(ini->dev_address, temp.size);
		ini = GPUmem.erase(ini);
	}

	/*cout << "DESPUES" << endl;
	mostrar_memoria();
	mostrar_memcpu();
	cout << "FIN DESPUES" << endl;*/
	//cout << "memfinal = " << GPUmem.size() << " " << avmem << endl;

}

void liberar(int *ptr, int size)
{
	//cout << "L " << avmem << " " << size; 

	cudaFree(ptr);
	avmem += size;
	
	//cout << " " << avmem << endl;
}

void reservar(int **ptr, int size)
{
        // cout << "R " << avmem << " " << size

	while(avmem < size)
		limpiar();
	while(cudaMalloc(ptr, size) == cudaErrorMemoryAllocation)
		limpiar();
	avmem -= size;

	// cout << " " << avmem << endl;
}

void registrar(int name, int num_columns, int *ptr, int rows, int itr, int rule)
{
	memnode temp;
	temp.name = name;
	temp.dev_address = ptr;
	temp.rows = rows;
	temp.size = rows * num_columns * sizeof(int);
	temp.iteration = itr;
	temp.isrule = rule;
	GPUmem.push_back(temp);
}

template<class InputIterator>
void actualizar(int num_columns, int *ptr, int rows, InputIterator i)
{
	i->dev_address = ptr;
	i->rows = rows;
	i->size = rows * num_columns * sizeof(int);
}

int numrows(int name, int itr)
{
	int sum = 0;
	memnode temp;
	temp.iteration = itr;
	pair<list<memnode>::iterator, list<memnode>::iterator> rec = equal_range(GPUmem.begin(), GPUmem.end(), temp, compareiteration);
	while(rec.first != rec.second)
	{
		if(rec.first->name == name)
			sum += rec.first->rows;
		rec.first++;
	}
	rec = equal_range(CPUmem.begin(), CPUmem.end(), temp, compareiteration);
	while(rec.first != rec.second)
	{
		if(rec.first->name == name)
			sum += rec.first->rows;
		rec.first++;
	}
	return sum;
}

int cargar(int name, int num_rows, int num_columns, int is_fact, int *address_host_table, int **ptr, int itr)
{
	int numgpu, numcpu, totalrows = 0;
	int *temp, x;
	int size, itrant;
	list<memnode>::iterator i;
	memnode fact;
	if(is_fact)
	{
		i = buscarhecho(GPUmem.begin(), GPUmem.end(), name);
		if(i != GPUmem.end())
		{
			fact = *i;
			GPUmem.erase(i);
			fact.iteration = itr;
			*ptr = fact.dev_address;
			GPUmem.push_back(fact);
			return fact.rows;
		}
		size = num_rows * num_columns * sizeof(int);
		reservar(&temp, size);
		cudaMemcpyAsync(temp, address_host_table, size, cudaMemcpyHostToDevice);
		registrar(name, num_columns, temp, num_rows, itr, 0);
		*ptr = temp;
		return num_rows;
	}
	if(itr > 0)
	{
		itrant = itr - 1;
		i = buscarpornombre(name, itrant, &totalrows, &numgpu);
		numcpu = buscarpornombrecpu(name, itrant, &totalrows);

		if((numgpu == 2) && (numcpu == (HALF_REC + 1)))
		{
			actualizar(num_columns, temp_storage[1].dev_address, temp_storage[1].rows, i);
			*ptr = temp_storage[1].dev_address;
			return temp_storage[1].rows;
		}
		size = totalrows * num_columns * sizeof(int);
		reservar(&temp, size);
		for(x = 1; x < numgpu; x++)
		{
			cudaMemcpyAsync(temp + temp_storage[x-1].size, temp_storage[x].dev_address, temp_storage[x].size, cudaMemcpyDeviceToDevice);
			liberar(temp_storage[x].dev_address, temp_storage[x].size);
		}
		for(x = HALF_REC + 1; x < numcpu; x++)
		{
			cudaMemcpyAsync(temp + temp_storage[x-1].size, temp_storage[x].dev_address, temp_storage[x].size, cudaMemcpyHostToDevice);
			free(temp_storage[x].dev_address);
		}
		actualizar(num_columns, temp, totalrows, i);
		*ptr = temp;
		return totalrows;
	}
	return 0;
}

int cargafinal(int name, int cols, int **ptr)
{
	int *temp, *ini, cont = 0;
	memnode bus;
	bus.name = name;
	GPUmem.sort(comparename);
	CPUmem.sort(comparename);
	list<memnode>::iterator endg = GPUmem.end();
	list<memnode>::iterator endc = CPUmem.end();
	list<memnode>::iterator pos = lower_bound(GPUmem.begin(), endg, bus, comparename);
	list<memnode>::iterator gpu = pos;
	while(pos != endg && pos->name == name)
	{
		cont += pos->rows;
		pos++;
	}
	pos = lower_bound(CPUmem.begin(), endc, bus, comparename);
	list<memnode>::iterator cpu = pos;
	while(pos != endc && pos->name == name)
	{
		cont += pos->rows;
		pos++;
	}
	
	reservar(&temp, cont * cols * sizeof(int));
	ini = temp;	

	pos = gpu;
	while(pos != endg && pos->name == name)
	{
		cudaMemcpy(temp, pos->dev_address, pos->size, cudaMemcpyDeviceToDevice);
		temp += pos->size / sizeof(int);
		pos++;
	}
	pos = cpu;
	while(pos != endc && pos->name == name)
	{
		cudaMemcpy(temp, pos->dev_address, pos->size, cudaMemcpyHostToDevice);
		temp += pos->size / sizeof(int);
		pos++;
	}

	/*int x, y;
	int *hop1 = (int *)malloc(cont * cols * sizeof(int));
	cudaMemcpy(hop1, ini, cont * cols * sizeof(int), cudaMemcpyDeviceToHost);
	cout << "select finala" << endl;
	for(x = 0; x < cont; x++)
	{
		for(y = 0; y < cols; y++)
			cout << hop1[x * cols + y] << " ";
		cout << endl;
	}
	cout << "select finala" << endl;*/

	*ptr = ini;
	return cont;
}

bool generadas(int name, int filas, int cols, int itr)
{
	int r1, r2, x, fin;
	int *dop1, *dop2;

	r2 = numrows(name, itr);
	if(itr < MAX_FIX_POINTS)
		fin = itr;
	else
		fin = MAX_FIX_POINTS;
	for(x = 1; x <= fin; x++)
	{
		r1 = numrows(name, itr - x);
		if(r1 == r2)
		{
			r2 = cargar(name, filas, cols, 0, NULL, &dop2, itr + 1);
			thrust::device_ptr<int> pt2 = thrust::device_pointer_cast(dop2);
			r1 = cargar(name, filas, cols, 0, NULL, &dop1, itr - x + 1);
			thrust::device_ptr<int> pt1 = thrust::device_pointer_cast(dop1);

			/*int y;
			int *a = (int *)malloc(r1 * cols * sizeof(int));
			cudaMemcpy(a, dop1, r1 * cols * sizeof(int), cudaMemcpyDeviceToHost);
			for(x = 0; x < r1; x++)
			{
				for(y = 0; y < cols; y++)
					cout << a[x * cols + y] << " ";
			}
			cout << endl;
			cudaMemcpy(a, dop2, r1 * cols * sizeof(int), cudaMemcpyDeviceToHost);
			for(x = 0; x < r1; x++)
			{
				for(y = 0; y < cols; y++)
					cout << a[x * cols + y] << " ";
			}
			cout << endl;
			free(a);*/

			if(thrust::equal(pt1, pt1 + r1, pt2) == true)
				return true;
		}
	}

	return false;
}

void mostrar_memoria()
{
	int x;
	list<memnode>::iterator i = GPUmem.begin();
	cout << "Memoria inicio GPU" << endl;
	for(x = 0; x < GPUmem.size(); x++, i++)
		cout << i->name << " " << i->iteration << " " << i->size << endl;
	cout << "Memoria fin GPU" << endl;
}

void mostrar_memcpu()
{
	int x;
	list<memnode>::iterator i = CPUmem.begin();
	cout << "Memoria inicio CPU" << endl;
	for(x = 0; x < CPUmem.size(); x++, i++)
		cout << i->name << " " << i->iteration << endl;
	cout << "Memoria fin CPU" << endl;
}

void resultados(vector<rulenode>::iterator first, vector<rulenode>::iterator last)
{
	GPUmem.sort(comparename);
	CPUmem.sort(comparename);
	list<memnode>::iterator gpu = GPUmem.begin();
	list<memnode>::iterator cpu = CPUmem.begin();
	int x, y, of, cols;
	int *temp, cont = 0;
	while(first != last)
	{
		while(first->name == gpu->name)
		{
			temp = (int *)malloc(gpu->size);
			cudaMemcpy(temp, gpu->dev_address, gpu->size, cudaMemcpyDeviceToHost);
			cols = gpu->size / (gpu->rows * sizeof(int));
			cont += gpu->rows;
			for(x = 0, of = 0; x < gpu->rows; x++)
			{
				for(y = 0; y < cols; y++, of++)
					cout << temp[of] << " ";
				cout << endl;
			}
			cudaFree(gpu->dev_address);
			free(temp);
			gpu++;
		}
		while(first->name == cpu->name)
		{
			cols = cpu->size / (cpu->rows * sizeof(int));
			cont += cpu->rows;
			for(x = 0, of = 0; x < cpu->rows; x++)
			{
				for(y = 0; y < cols; y++, of++)
					cout << cpu->dev_address[of] << " ";
				cout << endl;
			}
			free(cpu->dev_address);
			cpu++;
		}
		first++;
	}
	cout << cont << endl;
}

void clear_memory()
{
	list<memnode>::iterator ini;
	list<memnode>::iterator fin;
	ini = GPUmem.begin();
	fin = GPUmem.end();
	while(ini != fin)
	{
		cudaFree(ini->dev_address);
		ini++;
	}
	ini = CPUmem.begin();
	fin = CPUmem.end();
	while(ini != fin)
	{
		free(ini->dev_address);
		ini++;
	}
	GPUmem.clear();
	CPUmem.clear();
}
