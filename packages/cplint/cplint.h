/*
	LPAD and CP-Logic interpreter

Copyright (c) 2007, Fabrizio Riguzzi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.

*/

#include "util.h"
#include "cuddInt.h"
#include "array.h"
#include "mtr.h"
#include "avl.h"
#include "YapInterface.h"
#include <glib.h>
 
typedef struct
  {
    int var,value;
  } factor;

typedef struct
  {
    int nVal,nBit;
    array_t * probabilities;
    array_t * booleanVars;
  } variable;


void createVars(array_t * vars, YAP_Term t,DdManager * mgr, array_t * bVar2mVar,int create_dot, char inames[1000][20]);
void createExpression(array_t * expression, YAP_Term t);
void init_my_predicates(void);
int compare(char *a, char *b);
gint my_equal(gconstpointer v,gconstpointer v2);
guint my_hash(gconstpointer key);
void  dealloc(gpointer key,gpointer value,gpointer user_data);



DdNode * retFunction(DdManager * mgr, array_t * expression,array_t * v);
DdNode * retTerm(DdManager * mgr,array_t *factors,array_t * v);
DdNode * retFactor(DdManager * mgr, factor f, array_t * v);

double Prob(DdNode *node, array_t * vars,array_t * bVar2mVar, GHashTable * nodes);

double ProbBool(DdNode *node, int bits, int nBit,int posBVar,variable v,
array_t * vars,array_t * bVar2mVar, GHashTable * nodes);
