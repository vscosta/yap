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

#define SBG_EQ  (-1)
#define SBG_GT  (-2)
#define SBG_LT  (-3)
#define SBG_GE  (-4)
#define SBG_LE  (-5)
#define SBG_DF  (-6)

int Cuda_Eval(predicate**, int, predicate**, int, predicate*, int**);

#endif
