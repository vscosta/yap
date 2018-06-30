#include <list>
#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <thrust/device_vector.h>
#include "lista.h"
#include "memory.h"
#include "pred.h"

#define MAX_REC 200
#define MAX_FIX_POINTS 100

memnode temp_storage[MAX_REC];
/*List used to store information (address, size, etc.) about facts and rule results loaded in the GPU*/
list<memnode> GPUmem;
/*List used to store information about rule results offloaded from the GPU to the CPU*/
list<memnode> CPUmem;

/*Auxiliary function to sort rule list*/
bool comparer(const rulenode &r1, const rulenode &r2)
{
	return (r1.name > r2.name); 
}

/*Used in search functions to compare iterations*/
bool compareiteration(const memnode &r1, const memnode &r2)
{
	return (r1.iteration < r2.iteration); 
}

/*Used in search functions to compare names*/
bool comparename(const memnode &r1, const memnode &r2)
{
	return (r1.name > r2.name); 
}

/*Linear search of 'name' fact*/
template<class InputIterator>
InputIterator buscarhecho(InputIterator first, InputIterator last, int name)
{
	while(first!=last) 
	{
		if(first->name == name && first->isrule == 0) return first;
			++first;
	}
	return last;
}

/*Finds all results of rule 'name' in iteration 'itr' in both CPU and GPU memory. Every result found is removed from its respective list*/
list<memnode>::iterator buscarpornombre(int name, int itr, int *totalrows, int *gpunum, int *cpunum)
{
	int x = 0, sum = 0;
	memnode temp;
	list<memnode>::iterator i;
	temp.iteration = itr;
	pair<list<memnode>::iterator, list<memnode>::iterator> rec = equal_range(GPUmem.begin(), GPUmem.end(), temp, compareiteration);

	while(rec.first != rec.second)
	{
		if(rec.first->name == name && rec.first->isrule == 1)
		{
			temp_storage[x] = *rec.first;
			rec.first = GPUmem.erase(rec.first);
			sum += temp_storage[x].rows;
			x++;
		}	
		else
			rec.first++;
	}
	*gpunum = x;
	temp.name = name;
	temp.isrule = 1;
	i = GPUmem.insert(rec.first, temp);
	rec = equal_range(CPUmem.begin(), CPUmem.end(), temp, compareiteration);

	while(rec.first != rec.second)
	{				
		if(rec.first->name == name && rec.first->isrule == 1)
		{
			temp_storage[x] = *rec.first;
			rec.first = CPUmem.erase(rec.first);
			sum += temp_storage[x].rows;
			x++;
		}	
		else
			rec.first++;
	}
	*totalrows = sum;
	*cpunum = x;
	return i;
}

list<memnode>::iterator buscarpornombrecpu(int name, int itr, int *totalrows, int *gpunum, int *cpunum)
{
	int x = 0, sum = 0;
	memnode temp;
	list<memnode>::iterator i;
	temp.iteration = itr;
	pair<list<memnode>::iterator, list<memnode>::iterator> rec = equal_range(GPUmem.begin(), GPUmem.end(), temp, compareiteration);

	while(rec.first != rec.second)
	{				
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

	*gpunum = x;
	temp.name = name;
	temp.isrule = 1;
	rec = equal_range(CPUmem.begin(), CPUmem.end(), temp, compareiteration);

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
	i = CPUmem.insert(rec.first, temp);
	*totalrows = sum;
	*cpunum = x;
	return i;
}

/*Removes the least recently used memory block from GPU memory, sending it to CPU memory if it's a rule result. 
If there are no used memory blocks in the GPU and we still don't have enough memory, the program exits with error*/
void limpiar(const char s[], size_t sz)
{
	list<memnode>::iterator ini;
	memnode temp;
	size_t free, total;

	if(GPUmem.size() == 0)
	{
		hipMemGetInfo(&free,&total);
		cerr << s << ": not enough GPU memory: have " << free << " of " << total << ", need " << sz << " bytes." << endl;
		exit(1);
	}		

	ini = GPUmem.begin();
	if(ini->isrule)
	{	
		temp = *ini;
		temp.dev_address = (int *)malloc(ini->size);
		hipMemcpyAsync(temp.dev_address, ini->dev_address, temp.size, hipMemcpyDeviceToHost);
		list<memnode>::iterator pos = lower_bound(CPUmem.begin(), CPUmem.end(), temp, compareiteration);
		CPUmem.insert(pos, temp);
	}
	hipFree(ini->dev_address);
	GPUmem.erase(ini);
}

/*Allocs 'size' amount of bytes in GPU memory. If not enough memory is available, removes least recently used memory blocks until 
enough space is available*/
void reservar(int **ptr, size_t size)
{
	size_t free, total;

        if (size == 0) { 
                *ptr = NULL; 
                return;
        }

	hipMemGetInfo(&free, &total);
	while(free < size)
	{
		cout << "Se limpio memoria " << free << " " << total << endl;
		limpiar("not enough memory", size);
		hipMemGetInfo(&free, &total);
	}

	while(hipMalloc(ptr, size) == hipErrorMemoryAllocation)
		limpiar("Error in memory allocation", size);
	if (! *ptr ) {
	  size_t free, total;
	  hipMemGetInfo(      &free, &total	 );
	  cerr << "Could not allocate " << size << " bytes, only " << free << " avaliable from total of " << total << " !!!" << endl;
	  cerr << "Exiting CUDA...." << endl;
	  exit(1);
	}
}

/*Creates a new entry in the GPU memory list*/
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

void registrarcpu(int name, int num_columns, int *ptr, int rows, int itr, int rule)
{
	memnode temp;
	temp.name = name;
	temp.dev_address = ptr;
	temp.rows = rows;
	temp.size = rows * num_columns * sizeof(int);
	temp.iteration = itr;
	temp.isrule = rule;
	CPUmem.push_back(temp);
}

/*Updates the information of an element in a list*/
template<class InputIterator>
void actualizar(int num_columns, int *ptr, int rows, InputIterator i)
{
	i->dev_address = ptr;
	i->rows = rows;
	i->size = rows * num_columns * sizeof(int);
}

/*Count the total number of rows generated by rule 'name' in iteration 'iter'*/
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

	extern "C" void * YAP_IntToAtom(int);
	extern  "C" char * YAP_AtomName(void *);

/*Loads facts or rule results in GPU memory. If a fact is already in GPU memory, its pointer is simply returned. Otherwise, 
memory is reserved and the fact is loaded. Rule results are loaded based on the current iteration 'itr' and both GPU and 
CPU memories are searched for all instances of said results. The instances are combined into a single one in GPU memory.*/
int cargar(int name, int num_rows, int num_columns, int is_fact, int *address_host_table, int **ptr, int itr)
{
	int numgpu, numcpu, totalrows = 0;
	int *temp, x;
	int size, itrant, inc = 0;
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
		hipMemcpyAsync(temp, address_host_table, size, hipMemcpyHostToDevice);
		registrar(name, num_columns, temp, num_rows, itr, 0);
		*ptr = temp;
		return num_rows;
	}
	if(itr > 0)
	{
		itrant = itr - 1;
		i = buscarpornombre(name, itrant, &totalrows, &numgpu, &numcpu);
		if((numgpu == 1) && (numcpu == 1))
		{
			actualizar(num_columns, temp_storage[0].dev_address, temp_storage[0].rows, i);
			*ptr = temp_storage[0].dev_address;
			return temp_storage[0].rows;
		}
		size = totalrows * num_columns * sizeof(int);
		reservar(&temp, size);
		for(x = 0; x < numgpu; x++)
		{
			hipMemcpyAsync(temp + inc, temp_storage[x].dev_address, temp_storage[x].size, hipMemcpyDeviceToDevice);
			inc += temp_storage[x].size / sizeof(int);
			hipFree(temp_storage[x].dev_address);
		}
		for(; x < numcpu; x++)
		{
			hipMemcpyAsync(temp + inc, temp_storage[x].dev_address, temp_storage[x].size, hipMemcpyHostToDevice);
			inc += temp_storage[x].size / sizeof(int);
			free(temp_storage[x].dev_address);
		}
		actualizar(num_columns, temp, totalrows, i);
		*ptr = temp;
		return totalrows;
	}
	return 0;
}

int cargarcpu(int name, int num_rows, int num_columns, int is_fact, int *address_host_table, int **ptr, int itr)
{
	int numgpu, numcpu, totalrows = 0;
	int *temp, x;
	int size, itrant, inc = 0;
	list<memnode>::iterator i;

	if(is_fact)
	{
		*ptr = address_host_table;
		return num_rows;
	}
	if(itr > 0)
	{
		itrant = itr - 1;
		i = buscarpornombrecpu(name, itrant, &totalrows, &numgpu, &numcpu);

		if((numgpu == 0) && (numcpu == 1))
		{
			actualizar(num_columns, temp_storage[0].dev_address, temp_storage[0].rows, i);
			*ptr = temp_storage[0].dev_address;
			return temp_storage[0].rows;
		}
		size = totalrows * num_columns * sizeof(int);
		temp = (int *)malloc(size);
		for(x = 0; x < numgpu; x++)
		{
			hipMemcpyAsync(temp + inc, temp_storage[x].dev_address, temp_storage[x].size, hipMemcpyDeviceToHost);
			inc += temp_storage[x].size / sizeof(int);
			hipFree(temp_storage[x].dev_address);
		}
		for(; x < numcpu; x++)
		{
			memmove(temp + inc, temp_storage[x].dev_address, temp_storage[x].size);
			inc += temp_storage[x].size / sizeof(int);
			free(temp_storage[x].dev_address);
		}
		actualizar(num_columns, temp, totalrows, i);
		*ptr = temp;
		return totalrows;
	}
	return 0;
}

/*Loads all results of rule 'name' from both GPU and CPU memories into the GPU*/
int cargafinal(int name, int cols, int **ptr)
{
	int *temp, *ini, cont = 0, numg = 0, numc = 0;
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
		numg++;
		pos++;
	}
	pos = lower_bound(CPUmem.begin(), endc, bus, comparename);
	list<memnode>::iterator cpu = pos;
	while(pos != endc && pos->name == name)
	{
		cont += pos->rows;
		numc++;
		pos++;
	}

	if(numg == 0 && numc == 0)
		return 0;
	if(numg == 1 && numc == 0) 
	{
		pos = gpu;
		*ptr = pos->dev_address;
		cont = pos->rows;
		GPUmem.erase(pos);
		#ifdef TUFFY
		return -cont;
		#else
		return cont;
		#endif
	}
	if(numg == 0 && numc == 1)
	{
		pos = cpu;
		cont = pos->rows;
		#ifdef TUFFY
		reservar(&temp, pos->size);
		hipMemcpy(temp, pos->dev_address, pos->size, hipMemcpyHostToDevice);
		*ptr = temp;
		#else
		*ptr = pos->dev_address;
		#endif
		CPUmem.erase(pos);
		return -cont;
	}

	reservar(&temp, cont * cols * sizeof(int));
	ini = temp;
	pos = gpu;
	while(pos != endg && pos->name == name)
	{
		hipMemcpy(temp, pos->dev_address, pos->size, hipMemcpyDeviceToDevice);
		temp += pos->size / sizeof(int);
		pos++;
	}
	pos = cpu;
	while(pos != endc && pos->name == name)
	{
		hipMemcpy(temp, pos->dev_address, pos->size, hipMemcpyHostToDevice);
		temp += pos->size / sizeof(int);
		pos++;
	}
	*ptr = ini;
	return cont;
}

/*Compares the results of the current iteration against the results of older iterations. 
Used to avoid infinite computations when the result is not a single fixed-point, but an 
orbit of points.*/
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
			if(thrust::equal(pt1, pt1 + r1, pt2) == true)
				return true;
		}
	}
	return false;
}

void mostrar_memoria()
{
	unsigned int x;
	list<memnode>::iterator i = GPUmem.begin();
	cout << "Memoria inicio GPU" << endl;
	for(x = 0; x < GPUmem.size(); x++, i++)
		cout << i->name << " " << i->iteration << " " << i->isrule << " " << i->rows << " " << i->size << endl;
	cout << "Memoria fin GPU" << endl;
}

void mostrar_memcpu()
{
	unsigned int x;
	list<memnode>::iterator i = CPUmem.begin();
	cout << "Memoria inicio CPU" << endl;
	for(x = 0; x < CPUmem.size(); x++, i++)
		cout << i->name << " " << i->iteration << endl;
	cout << "Memoria fin CPU" << endl;
}

/*Clear all rule results from both GPU and CPU memory*/
void clear_memory()
{
	list<memnode>::iterator ini;
	list<memnode>::iterator fin;
       	ini = GPUmem.begin();
	fin = GPUmem.end();
	while(ini != fin)
	{
		if(ini->isrule)
		{
			hipFree(ini->dev_address);
			ini = GPUmem.erase(ini);
		}
		else
			ini++;
	}
	ini = CPUmem.begin();
	fin = CPUmem.end();
	while(ini != fin)
	{
		free(ini->dev_address);
		ini++;
	}
	CPUmem.clear();
}

/*Clear everything from both GPU and CPU memory*/
void clear_memory_all()
{
	list<memnode>::iterator ini;
	list<memnode>::iterator fin;
       	ini = GPUmem.begin();
	fin = GPUmem.end();
	while(ini != fin)
	{
		hipFree(ini->dev_address);
		ini++;
	}
	GPUmem.clear();
	ini = CPUmem.begin();
	fin = CPUmem.end();
	while(ini != fin)
	{
		free(ini->dev_address);
		ini++;
	}
	CPUmem.clear();
}

/*Remove all instances of fact 'name' from both CPU and GPU memories*/
void liberar(int name)
{
	list<memnode>::iterator i;
	memnode fact;
	i = buscarhecho(GPUmem.begin(), GPUmem.end(), name);
	if(i != GPUmem.end())
	{
		fact = *i;
		GPUmem.erase(i);
		hipFree(fact.dev_address);
	}
	i = buscarhecho(CPUmem.begin(), CPUmem.end(), name);
	if(i != CPUmem.end())
	{
		fact = *i;
		CPUmem.erase(i);
		free(fact.dev_address);
	}
}

/*Add all rows in 'dop1' to the fact 'name' by creating a new array capable of holding both.*/
void sumar(int name, int *dop1, int cols, int rows)
{
	list<memnode>::iterator i;
	memnode fact;
	i = buscarhecho(GPUmem.begin(), GPUmem.end(), name);
	int *res, newrows, offset;
	if(i != GPUmem.end())
	{
		fact = *i;
		newrows = rows + fact.rows;
		reservar(&res, newrows * cols * sizeof(int));
		offset = fact.rows * cols;
		hipMemcpyAsync(res, fact.dev_address, offset * sizeof(int), hipMemcpyDeviceToDevice);
		GPUmem.erase(i);
		registrar(name, cols, res, newrows, 0, 0);
		hipMemcpyAsync(res + offset, dop1, rows * cols * sizeof(int), hipMemcpyDeviceToDevice);
		hipFree(fact.dev_address);
	}
}
