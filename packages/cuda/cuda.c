
// interface to CUDD Datalog evaluation
#include "config.h"
#include "YapInterface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct predicate_struct {
  int name;
  int num_rows;
  int num_columns;
  int is_fact;
  int *address_host_table;
} predicate;

// initialize CUDA system
void Cuda_Initialize( void );

// add/replace a set of facts for predicate pred
int Cuda_NewFacts(predicate *pred);

// add/replace a rule for predicate pred
int Cuda_NewRule(predicate *pred);

// erase predicate pred
int Cuda_Erase(predicate *pred);

// evaluate predicate pred, mat is bound to a vector of solutions, and
// output the count
int Cuda_Eval(predicate *pred, int **mat);

void init_cuda( void );

static void
dump_mat(int mat[], int nrows, int ncols)
{
  int i, j;
  for ( i=0; i< nrows; i++) {
    printf("%d", mat[i*ncols]);
    for (j=1; j < ncols; j++) {
      printf(", %d", mat[i*ncols+j]);
    }
    printf("\n");
  }
}

static void
dump_vec(int vec[], int rows)
{
  int i = 1;
  int j = 0;
  printf("%d", vec[0]);
  for (j = 0; j < rows; j++) {
    for ( ; vec[i]; i++ ) {
      printf(", %d", vec[i]);
    }
    printf(", 0");
    i++;
  }
  printf("\n");
}

// stubs, will point at Carlos code.

void Cuda_Initialize( void )
{
}

int Cuda_NewFacts(predicate *pe)
{
  dump_mat( pe->address_host_table, pe->num_rows, pe->num_columns );
  return TRUE;
}

int Cuda_NewRule(predicate *pe)
{
  dump_vec( pe->address_host_table, pe->num_rows);
  return TRUE;
}

int Cuda_Erase(predicate *pe)
{
  if (pe->address_host_table)
    free( pe->address_host_table );
  free( pe );
  return TRUE;
}

static int
load_facts( void ) {

  int nrows = YAP_IntOfTerm(YAP_ARG1);
  int ncols = YAP_IntOfTerm(YAP_ARG2), i = 0;
  YAP_Term t3 = YAP_ARG3;
  int *mat = (int *)malloc(sizeof(int)*nrows*ncols);
  int pname = YAP_AtomToInt(YAP_NameOfFunctor(YAP_FunctorOfTerm(YAP_HeadOfTerm(t3))));
  predicate *pred;

  while(YAP_IsPairTerm(t3)) {
    int j = 0;
    YAP_Term th = YAP_HeadOfTerm(t3);

    for (j = 0; j < ncols; j++) {
      YAP_Term ta = YAP_ArgOfTerm(j+1, th);
      if (YAP_IsAtomTerm(ta)) {
	mat[i*ncols+j] = YAP_AtomToInt(YAP_AtomOfTerm(ta));
      } else {
	mat[i*ncols+j] = YAP_IntOfTerm(ta);
      }
    }
    t3 = YAP_TailOfTerm( t3 );
    i++;
  }
  if (YAP_IsVarTerm( YAP_ARG4)) {
    // new 
    pred = (predicate *)malloc(sizeof(predicate));
  } else {
    pred = (predicate *)YAP_IntOfTerm(YAP_ARG4);
    if (pred->address_host_table)
      free( pred->address_host_table );
  }
  pred->name = pname;
  pred->num_rows = nrows;
  pred->num_columns = ncols;
  pred->is_fact = TRUE;
  pred->address_host_table =  mat;
  Cuda_NewFacts(pred);
  if (YAP_IsVarTerm( YAP_ARG4)) {
    return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)pred));
  } else {
    return TRUE;
  }
}

static int
load_rule( void ) {
  // maximum of 2K symbols per rule, should be enough for ILP
  int vec[2048], *ptr = vec, *nvec;
  // qK different variables;
  YAP_Term vars[1024];
  int nvars = 0;
  int ngoals = YAP_IntOfTerm(YAP_ARG1);   /* gives the number of goals */
  int ncols = YAP_IntOfTerm(YAP_ARG2);
  YAP_Term t3 = YAP_ARG3;
  int pname = YAP_AtomToInt(YAP_NameOfFunctor(YAP_FunctorOfTerm(YAP_HeadOfTerm(t3))));
  predicate *pred;

  while(YAP_IsPairTerm(t3)) {
    int j = 0;
    YAP_Term th = YAP_HeadOfTerm(t3);
    YAP_Functor f = YAP_FunctorOfTerm( th );
    int n = YAP_ArityOfFunctor( f ); 

    *ptr++ = YAP_AtomToInt( YAP_NameOfFunctor( f ) );
    for (j = 0; j < n; j++) {
      YAP_Term ta = YAP_ArgOfTerm(j+1, th);

      if (YAP_IsVarTerm(ta)) {
	int k;
	for (k = 0; k< nvars; k++) {
	  if (vars[k] == ta) {
	    *ptr++ = k+1;
	    break;
	  }
	}
	if (k == nvars) {
	  vars[k] = ta;
	  *ptr++ = k+1;
	  nvars++;
	}
      } else if (YAP_IsAtomTerm(ta))  {
	*ptr++ = -YAP_AtomToInt(YAP_AtomOfTerm(ta));
      } else {
	*ptr++ = -YAP_IntOfTerm(ta);
      }
    }
    *ptr++ = 0;
    t3 = YAP_TailOfTerm( t3 );
  }
  if (YAP_IsVarTerm( YAP_ARG4)) {
    // new 
    pred = (predicate *)malloc(sizeof(predicate));
  } else {
    pred = (predicate *)YAP_IntOfTerm(YAP_ARG4);
    if (pred->address_host_table)
      free( pred->address_host_table );
  }
  pred->name = pname;
  pred->num_rows = ngoals;
  pred->num_columns = ncols;
  pred->is_fact = FALSE;
  nvec = (int *)malloc(sizeof(int)*(ptr-vec));
  memcpy(nvec, vec, sizeof(int)*(ptr-vec));
  pred->address_host_table =  nvec;
  Cuda_NewRule( pred );
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)pred));
}

static int
cuda_erase( void )
{
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  return Cuda_Erase( ptr );
}

static int
cuda_eval( void )
{
  int *mat;
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  int n = Cuda_Eval( ptr, & mat);
  int ncols = ptr->num_columns;
  YAP_Term out = YAP_TermNil();
  YAP_Functor f = YAP_MkFunctor(YAP_IntToAtom(ptr->name), ncols);
  YAP_Term vec[256];
  int i;

  if (n < 0)
    return FALSE;
  for (i=0; i<n; i++) {
    int ni = ((n-1)-i)*ncols, j;
    for (j=0; j<ncols; j++) {
      vec[i] = YAP_MkIntTerm(mat[ni+j]);
    }
    out = YAP_MkPairTerm(YAP_MkApplTerm( f, ncols, vec ), out);
  }
  return YAP_Unify(YAP_ARG2, out);
}

static int cuda_count( void )
{
  int *mat;
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  int n = Cuda_Eval( ptr, & mat);

  if (n < 0)
    return FALSE;
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(n));
}

static int first_time = TRUE;

void
init_cuda(void)
{
  if (first_time) Cuda_Initialize();
  first_time = FALSE;

  YAP_UserCPredicate("load_facts", load_facts, 4);
  YAP_UserCPredicate("load_rule", load_rule, 4);
  YAP_UserCPredicate("cuda_erase", cuda_erase, 1);
  YAP_UserCPredicate("cuda_eval", cuda_eval, 2);
  YAP_UserCPredicate("cuda_count", cuda_count, 2);
}

