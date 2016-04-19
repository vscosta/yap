#ifndef _MEMORY_H_
#define _MEMORY_H_

#include <list>
#include <vector>
#include "lista.h"

using namespace std;

bool comparer(const rulenode&, const rulenode&);
void limpiar(const char [], size_t);
void limpiartodo(int*, int*);
int cargar(int, int, int, int, int*, int**, int);
int cargarcpu(int, int, int, int, int*, int**, int);
int cargafinal(int, int, int**);
void reservar(int**, size_t);
void registrar(int, int, int*, int, int, int);
void registrarcpu(int, int, int*, int, int, int);
bool generadas(int, int, int, int);
void sumar(int, int*, int, int);
void liberar(int);
void mostrar_memoria(void);
void mostrar_memcpu(void);
void clear_memory(void);
void clear_memory_all(void);

#endif
