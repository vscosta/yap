
// interface to CUDD Datalog evaluation
#include "config.h"
#include "YapInterface.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "pred.h"

YAP_Atom AtomEq,
  AtomGt,
  AtomLt,
  AtomGe,
  AtomLe,
  AtomDf;

predicate *facts[100]; /*Temporary solution to maintain facts and rules*/
predicate *rules[100];
int32_t cf = 0, cr = 0;

// initialize CUDA system
void Cuda_Initialize( void );

// add/replace a set of facts for predicate pred
int32_t Cuda_NewFacts(predicate *pred);

// add/replace a rule for predicate pred
int32_t Cuda_NewRule(predicate *pred);

// erase predicate pred
int32_t Cuda_Erase(predicate *pred);

// evaluate predicate pred, mat is bound to a vector of solutions, and
// output the count
//int32_t Cuda_Eval(predicate *pred, int32_t **mat); This functions arguments were changed, please see pred.h

void init_cuda( void );

#if DEBUG_INTERFACE
static void
dump_mat(int32_t mat[], int32_t nrows, int32_t ncols)
{
  int32_t i, j;
  for ( i=0; i< nrows; i++) {
    printf("%d", mat[i*ncols]);
    for (j=1; j < ncols; j++) {
      printf(", %d", mat[i*ncols+j]);
    }
    printf("\n");
  }
}

static void
dump_vec(int32_t vec[], int32_t rows)
{
  int32_t i = 1;
  int32_t j = 0;
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
#endif /* DEBUG_INTERFACE */


// stubs, will point at Carlos code.

void Cuda_Initialize( void )
{
}

int32_t Cuda_NewFacts(predicate *pe)
{
#if DEBUG_INTERFACE
  dump_mat( pe->address_host_table, pe->num_rows, pe->num_columns );
#endif
  facts[cf] = pe;
  cf++;
  return TRUE;
}

int32_t Cuda_NewRule(predicate *pe)
{
#if DEBUG_INTERFACE
  dump_vec( pe->address_host_table, pe->num_rows);
#endif
  rules[cr] = pe;
  cr++;
  return TRUE;
}

int32_t Cuda_Erase(predicate *pe)
{
  int i = 0;
  while ( rules[i] != pe )
    i++;
  while (i < cr-1) {
    rules[i] = rules[i+1];
    i++;
  }
  rules[i] = NULL;
  cr--;
  if (pe->address_host_table)
    free( pe->address_host_table );
  free( pe );
  return TRUE;
}

static int
load_facts( void ) {

  int32_t nrows = YAP_IntOfTerm(YAP_ARG1);
  int32_t ncols = YAP_IntOfTerm(YAP_ARG2), i = 0;
  YAP_Term t3 = YAP_ARG3;
  int32_t *mat = (int32_t *)malloc(sizeof(int32_t)*nrows*ncols);
  int32_t pname = YAP_AtomToInt(YAP_NameOfFunctor(YAP_FunctorOfTerm(YAP_HeadOfTerm(t3))));
  predicate *pred;

  while(YAP_IsPairTerm(t3)) {
    int32_t j = 0;
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
  int32_t vec[2048], *ptr = vec, *nvec;
  // qK different variables;
  YAP_Term vars[1024];
  int32_t nvars = 0;
  int32_t ngoals = YAP_IntOfTerm(YAP_ARG1);   /* gives the number of goals */
  int32_t ncols = YAP_IntOfTerm(YAP_ARG2);
  YAP_Term t3 = YAP_ARG3;
  int32_t pname = YAP_AtomToInt(YAP_NameOfFunctor(YAP_FunctorOfTerm(YAP_HeadOfTerm(t3))));
  predicate *pred;

  while(YAP_IsPairTerm(t3)) {
    int32_t j = 0;
    YAP_Term th = YAP_HeadOfTerm(t3);
    YAP_Functor f = YAP_FunctorOfTerm( th );
    int32_t n = YAP_ArityOfFunctor( f ); 
    YAP_Atom at = YAP_NameOfFunctor( f );

    if (at == AtomEq)
      *ptr++ = SBG_EQ;
    else if (at == AtomGt)
      *ptr++ = SBG_GT;
    else if (at == AtomLt)
      *ptr++ = SBG_LT;
    else if (at == AtomGe)
      *ptr++ = SBG_GE;
    else if (at == AtomLe)
      *ptr++ = SBG_LE;
    else if (at == AtomDf)
      *ptr++ = SBG_DF;
    else
      *ptr++ = YAP_AtomToInt( at );
    for (j = 0; j < n; j++) {
      YAP_Term ta = YAP_ArgOfTerm(j+1, th);

      if (YAP_IsVarTerm(ta)) {
	int32_t k;
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
  nvec = (int32_t *)malloc(sizeof(int32_t)*(ptr-vec));
  memcpy(nvec, vec, sizeof(int32_t)*(ptr-vec));
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
  int32_t *mat;
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  int32_t n = Cuda_Eval(facts, cf, rules, cr, ptr, & mat);
  int32_t ncols = ptr->num_columns;
  YAP_Term out = YAP_TermNil();
  YAP_Functor f = YAP_MkFunctor(YAP_IntToAtom(ptr->name), ncols);
  YAP_Term vec[256];
  int32_t i;

  if (n < 0)
    return FALSE;
  for (i=0; i<n; i++) {
    int32_t ni = ((n-1)-i)*ncols, j;
    for (j=0; j<ncols; j++) {
      vec[j] = YAP_MkIntTerm(mat[ni+j]);
    }
    out = YAP_MkPairTerm(YAP_MkApplTerm( f, ncols, vec ), out);
  }
  return YAP_Unify(YAP_ARG2, out);
}

static int cuda_count( void )
{
  int32_t *mat;
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  int32_t n = Cuda_Eval(facts, cf, rules, cr, ptr, & mat);

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

  AtomEq = YAP_LookupAtom("=");
  AtomGt = YAP_LookupAtom(">");
  AtomLt = YAP_LookupAtom("<");
  AtomGe = YAP_LookupAtom(">=");
  AtomLe = YAP_LookupAtom("=<");
  AtomDf = YAP_LookupAtom("\\=");
  YAP_UserCPredicate("load_facts", load_facts, 4);
  YAP_UserCPredicate("load_rule", load_rule, 4);
  YAP_UserCPredicate("cuda_erase", cuda_erase, 1);
  YAP_UserCPredicate("cuda_eval", cuda_eval, 2);
  YAP_UserCPredicate("cuda_count", cuda_count, 2);
}

