/*
	LPAD and CP-Logic interpreter

Copyright (c) 2007, Fabrizio Riguzzi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.

*/

#include "util.h"
#include "cuddInt.h"
#include "mtr.h"
#include "YapInterface.h"
#include <glib.h>
 
typedef struct
  {
    int var,value;
  } factor;

typedef struct
  {
    int nFact;
    factor * factors;
  } term;

typedef struct
  {
    int nTerms;
    term * terms;
  } expr;

typedef struct
  {
    int nVal,nBit;
    double * probabilities;
    DdNode * * booleanVars;
  } variable;

typedef struct
  {
    int nVar;
    int nBVar;
    variable * varar;
    int * bVar2mVar;
  } variables;

variables createVars(YAP_Term t,DdManager * mgr, int create_dot, char inames[1000][20]);
expr createExpression(YAP_Term t);
void init_my_predicates(void);



DdNode * retFunction(DdManager * mgr, expr expression,variables v);
DdNode * retTerm(DdManager * mgr,term t,variables v);
DdNode * retFactor(DdManager * mgr, factor f, variables v);

double Prob(DdNode *node, variables vars,GHashTable * nodes);

double ProbBool(DdNode *node, int bits, int nBit,int posBVar,variable v,
int mVarIndex,
variables vars,GHashTable * nodes, int comp);
