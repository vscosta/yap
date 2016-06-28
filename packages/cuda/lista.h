#ifndef _LISTA_H_
#define _LISTA_H_

typedef struct Node{
	int name;
	int *dev_address;
	int rows;
	int size;
	int iteration;
	int isrule;
}memnode;

typedef struct auxiliar{
	int name;
	int num_rows;
	int num_columns;
	int *address_host_table;
	int *rule_names;
	int *referencias;
	int **select;
	int *numsel;
	int **project;
	int2 *projpos;
	int **selfjoin;
	int *numselfj;
	int **wherejoin;
	int *numjoin;
	int totalpreds;
	int **preds;
	int2 *numpreds;
	int *negatives;
	char *rulename;
	int gen_act;
	int gen_ant;
}rulenode;

typedef struct completed{
	int name;
	int numrules;
	int reduce;
	int reset;
}compnode;

#endif
