
// interface to CUDD Datalog evaluation
#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "YapInterface.h"

void Cuda_Initialize( void );

void *Cuda_NewFacts(int nrows, int ncols, int mat[]);

void *Cuda_NewRule(int nrows, int ncols, int vec[], int len);

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
dump_vec(int vec[], int sz)
{
  int i;
  printf("%d", vec[0]);
  for ( i=1; i< sz; i++) {
    printf(", %d", vec[i]);
  }
  printf("\n");
}

// stubs, for now.

void Cuda_Initialize( void )
{
}

void *Cuda_NewFacts(int nrows, int ncols, int mat[])
{
  dump_mat( mat, nrows, ncols );
  return NULL;
}

void *Cuda_NewRule(int nrows, int ncols, int vec[], int len)
{
  dump_vec(vec, len);
  return NULL;
}

static int
p_load_facts( void ) {

  int nrows = YAP_IntOfTerm(YAP_ARG1);
  int ncols = YAP_IntOfTerm(YAP_ARG2), i = 0;
  int *mat = (int *)malloc(sizeof(int)*nrows*ncols);
  YAP_Term t3 = YAP_ARG3;
  void *cudaobj;

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
  cudaobj = Cuda_NewFacts(nrows, ncols, mat);
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)cudaobj));
}

static int
p_load_rule( void ) {
  // maximum of 2K symbols per rule, should be enough for ILP
  int vec[2048], *ptr = vec;
  // qK different variables;
  YAP_Term vars[1024];
  int nvars = 0;

  int ngoals = YAP_IntOfTerm(YAP_ARG1);   /* gives the number of goals */
  int ncols = YAP_IntOfTerm(YAP_ARG2);
  YAP_Term t3 = YAP_ARG3;
  void *cudaobj;

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
  cudaobj = Cuda_NewRule(ngoals, ncols, vec, ptr-vec);
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)cudaobj));
}

static int first_time = TRUE;

void
init_cuda(void)
{
  if (first_time) Cuda_Initialize();
  first_time = FALSE;

  YAP_UserCPredicate("load_facts", p_load_facts, 4);
  YAP_UserCPredicate("load_rule", p_load_rule, 4);
}

