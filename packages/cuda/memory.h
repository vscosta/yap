#ifndef _MEMORY_H_
#define _MEMORY_H_

//#include <thrust/device_vector.h>
#include <list>
#include <vector>
#include "lista.h"

using namespace std;
//using namespace thrust;

void calcular_mem(int);
void liberar(int*, int);
void limpiar(const char []);
void limpiartodo(int*, int*);
int cargar(int, int, int, int, int*, int**, int);
int cargafinal(int, int, int**);
void reservar(int**, int);
void registrar(int, int, int*, int, int, int);
bool generadas(int, int, int, int);
void mostrar_memoria(void);
void mostrar_memcpu(void);
void clear_memory(void);
void resultados(vector<rulenode>::iterator, vector<rulenode>::iterator);

#endif
