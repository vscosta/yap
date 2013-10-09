#ifndef _PRED_H_
#define _PRED_H_

typedef struct Nodo{
        int name;
	int num_rows;
	int num_columns;
	int is_fact;
	int *address_host_table;
}gpunode;

typedef gpunode predicate;

int Cuda_Eval(predicate**, int, predicate**, int, predicate*, int**);

#endif
