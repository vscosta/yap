
// interface to CUDD Datalog evaluation
#include "config.h"
#include "YapInterface.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>
#include "pred.h"

#define MAXARG 100

YAP_Atom AtomEq,
  AtomGt,
  AtomLt,
  AtomGe,
  AtomLe,
  AtomDf,
  AtomNt;

predicate *facts[MAXARG]; /*Temporary solution to maintain facts and rules*/
predicate *rules[MAXARG];
int32_t cf = 0, cr = 0;

char names[1024];

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

//#define DEBUG_INTERFACE 1

#ifdef ROCKIT
static int32_t query[100];
static int32_t qcont = 0;
static int cuda_init_query(void)
{
	int32_t pname = YAP_AtomToInt(YAP_AtomOfTerm(YAP_ARG1));
	query[qcont] = pname;
	qcont++;
	query[qcont] = 0;
	return TRUE;
}
#endif

#if DEBUG_INTERFACE
static void
dump_mat(int32_t mat[], int32_t nrows, int32_t ncols)
{
  return;
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

#ifdef ROCKIT
  if(cf >= 0)
  {
  	facts[cf] = pe;
	cf++;
  }
#else
  facts[cf] = pe;
  cf++;
#endif

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

static int currentFact = 0;
static predicate *currentPred = NULL;

static int
cuda_init_facts( void ) {

  int32_t nrows = YAP_IntOfTerm(YAP_ARG1);
  int32_t ncols = YAP_IntOfTerm(YAP_ARG2);
  int32_t *mat = (int32_t *)malloc(sizeof(int32_t)*nrows*ncols);
  int32_t pname = YAP_AtomToInt(YAP_AtomOfTerm(YAP_ARG3));
  predicate *pred;

	strcat(names, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)));
	strcat(names, " ");

  if (!mat)
    return FALSE;
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
  currentPred = pred;
  currentFact = 0;

  if (YAP_IsVarTerm( YAP_ARG4)) {
    return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)pred));
  } else {
    return TRUE;
  }
}

static int
cuda_load_fact( void ) {

  int i = currentFact;

#if defined(DATALOG) || defined(TUFFY)
  YAP_Term th = YAP_ARG1;
  int ncols = currentPred->num_columns;
  int j;
  int *mat = currentPred->address_host_table;
  for (j = 0; j < ncols; j++) {
    YAP_Term ta = YAP_ArgOfTerm(j+1, th);
    if (YAP_IsAtomTerm(ta)) {
      mat[i*ncols+j] = YAP_AtomToInt(YAP_AtomOfTerm(ta));
    } else {
      mat[i*ncols+j] = YAP_IntOfTerm(ta);
    }
  }
#endif

  i++;
  if (i == currentPred->num_rows) {
    Cuda_NewFacts(currentPred);
    currentPred = NULL;
    currentFact = 0;
  } else {
    currentFact = i;
  }
  return TRUE;
}

static int
load_rule( void ) {
  // maximum of 2K symbols per rule, should be enough for ILP
  int32_t vec[2048], *ptr = vec, *nvec, neg[2048];
  // qK different variables;
  YAP_Term vars[1024];
  int32_t nvars = 0, x;
  int32_t ngoals = YAP_IntOfTerm(YAP_ARG1);   /* gives the number of goals */
  int32_t ncols = YAP_IntOfTerm(YAP_ARG2);
  YAP_Term t3 = YAP_ARG3;
	YAP_Atom name = YAP_NameOfFunctor(YAP_FunctorOfTerm(YAP_HeadOfTerm(t3)));
  int32_t pname = YAP_AtomToInt(name);

	const char *strname = YAP_AtomName(name);
  predicate *pred;
  int32_t cont = 0;
  memset(neg, 0x0, 2048 * sizeof(int32_t));

  while(YAP_IsPairTerm(t3)) {
    int32_t j = 0, m;
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
    else if (at == AtomNt)
	{
      		neg[cont] = 1;
		cont++;
	}
    else
	{
      		*ptr++ = YAP_AtomToInt( at );
		cont++;
	}

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
      } else if (YAP_IsApplTerm(ta))  {
	f = YAP_FunctorOfTerm( ta );
	at = YAP_NameOfFunctor( f );
	m = YAP_ArityOfFunctor( f );
	*ptr++ = YAP_AtomToInt( at );

	for (x = 0; x < m; x++) {
      		YAP_Term ta2 = YAP_ArgOfTerm(x+1, ta);

      		if (YAP_IsVarTerm(ta2)) {
			int32_t k;
			for (k = 0; k < nvars; k++) {
	  			if (vars[k] == ta2) {
	    				*ptr++ = k+1;
	    				break;
	  			}
			}
			if (k == nvars) {
	  			vars[k] = ta2;
	  			*ptr++ = k+1;
	  			nvars++;
			}
      		} else if (YAP_IsAtomTerm(ta2))  {
			*ptr++ = -YAP_AtomToInt(YAP_AtomOfTerm(ta));
      		} else {
			*ptr++ = -YAP_IntOfTerm(ta);
      		}
    	}
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
	x = (strlen(strname) + 1) * sizeof(char);
	pred->predname = (char *)malloc(x);
	memmove(pred->predname, strname, x);
  nvec = (int32_t *)malloc(sizeof(int32_t)*(ptr-vec));
  memmove(nvec, vec, sizeof(int32_t)*(ptr-vec));
  pred->address_host_table =  nvec;
  pred->negatives = (int32_t *)malloc(sizeof(int32_t) * cont);
  memmove(pred->negatives, neg, sizeof(int32_t) * cont);
  Cuda_NewRule( pred );
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)pred));
}

static int
cuda_erase( void )
{
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  return Cuda_Erase( ptr );
}

void setQuery(YAP_Term t1, int32_t **res)
{
	int32_t *query = (int32_t *)malloc(MAXARG * sizeof(int32_t));
	int32_t x, y = 0, *itr;
	predicate *ptr = NULL;
	if(YAP_IsPairTerm(t1))
	{
		while(YAP_IsPairTerm(t1))
		{
			ptr = (predicate *)YAP_IntOfTerm(YAP_HeadOfTerm(t1));
			query[y] = ptr->name;
			itr = ptr->address_host_table;
			x = 2;
			while(itr[x] != 0)
				x++;
			query[y+1] = itr[x+1];
			t1 = YAP_TailOfTerm(t1);
			y+=2;
		}
	}
	else
	{
		ptr = (predicate *)YAP_IntOfTerm(t1);
		query[y] = ptr->name;
		itr = ptr->address_host_table;
		x = 2;
		while(itr[x] != 0)
			x++;
		query[y+1] = itr[x+1];
		y += 2;
	}
	query[y] = -1;
	query[y+1] = -1;
	*res = query;
}

static int
cuda_eval( void )
{
  int32_t *mat;

#if defined(DATALOG) || defined(TUFFY)
	int32_t *query = NULL;
	setQuery(YAP_ARG1, &query);
#endif

	int32_t finalDR = YAP_IntOfTerm(YAP_ARG3);
  int32_t n = Cuda_Eval(facts, cf, rules, cr, query, & mat, names, finalDR);

#ifdef TUFFY
	cf = 0;
#endif
#ifdef ROCKIT
	if(cf > 0)
		cf *= -1;
#endif
#if defined(TUFFY) || defined(ROCKIT)
	cr = 0;
	names[0] = '\0';
	return FALSE;
#else
  int32_t i;
  predicate *ptr = (predicate *)YAP_IntOfTerm(YAP_ARG1);
  int32_t ncols = ptr->num_columns;
  YAP_Term out = YAP_TermNil();
  YAP_Functor f = YAP_MkFunctor(YAP_IntToAtom(ptr->name), ncols);
  YAP_Term vec[256];

	YAP_Atom at;

  if (n < 0)
    return FALSE;
  for (i=0; i<n; i++) {
    int32_t ni = ((n-1)-i)*ncols, j;

	printf("%s(", YAP_AtomName(YAP_IntToAtom(ptr->name)));

    for (j=0; j<ncols; j++) {
      vec[j] = YAP_MkIntTerm(mat[ni+j]);

	at = YAP_IntToAtom(mat[ni+j]);
	if(at != NULL)
		printf("%s", YAP_AtomName(at));
	else
		printf("%d", mat[ni+j]);	
	if(j < (ncols - 1))
		printf(",");

    }
    out = YAP_MkPairTerm(YAP_MkApplTerm( f, ncols, vec ), out);

	printf(")\n");

  }
  if (n > 0)
    free( mat );
  return YAP_Unify(YAP_ARG2, out);
#endif
}

static int
cuda_coverage( void )
{
  int32_t *mat;

#if defined(DATALOG) || defined(TUFFY)
	int32_t *query = NULL;
	setQuery(YAP_ARG1, &query);
#endif

  int32_t n = Cuda_Eval(facts, cf, rules, cr, query, & mat, 0, 0);
  int32_t post = YAP_AtomToInt(YAP_AtomOfTerm(YAP_ARG2));
  int32_t i = n/2, min = 0, max = n-1;
  int32_t t0, t1;

  if (n < 0)
    return FALSE;
  if (n == 0) {
    return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(0)) && 
      YAP_Unify(YAP_ARG3, YAP_MkIntTerm(0));
  }
  t0 = mat[0], t1 = mat[(n-1)*2];
  if (t0 == t1) { /* all sametype */
    free( mat );
    /* all pos */
    if (t0 == post) 
      return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(n)) && 
	YAP_Unify(YAP_ARG4, YAP_MkIntTerm(0));
    /* all neg */
    return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(n)) && 
      YAP_Unify(YAP_ARG3, YAP_MkIntTerm(0));
  }
  do {
    i = (min+max)/2;
    if (i == min) i++;
    if (mat[i*2] == t0) {
      min = i;
    } else {
      max = i;
    }
    if (min+1 == max) {      
      free( mat );
      if (t0 == post) 
	return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(max)) && 
	  YAP_Unify(YAP_ARG4, YAP_MkIntTerm(n-max));
      /* all neg */
      return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(max)) && 
	YAP_Unify(YAP_ARG3, YAP_MkIntTerm(n-max));
    }
  } while ( TRUE );
}

static int cuda_count( void )
{
  int32_t *mat;

#if defined(DATALOG) || defined(TUFFY)
	int32_t *query = NULL;
	setQuery(YAP_ARG1, &query);
#endif

  int32_t n = Cuda_Eval(facts, cf, rules, cr, query, & mat, 0, 0);

  if (n < 0)
    return FALSE;
  free( mat );
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(n));
}

static int cuda_statistics( void )
{
  Cuda_Statistics();
  return TRUE;
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
  AtomNt = YAP_LookupAtom("not");
  YAP_UserCPredicate("load_facts", load_facts, 4);
  YAP_UserCPredicate("cuda_init_facts", cuda_init_facts, 4);
  YAP_UserCPredicate("cuda_load_fact", cuda_load_fact, 1);
  YAP_UserCPredicate("load_rule", load_rule, 4);
  YAP_UserCPredicate("cuda_erase", cuda_erase, 1);
  YAP_UserCPredicate("cuda_eval", cuda_eval, 3);
  YAP_UserCPredicate("cuda_coverage", cuda_coverage, 4);
  YAP_UserCPredicate("cuda_count", cuda_count, 2);
  YAP_UserCPredicate("cuda_statistics", cuda_statistics, 0);

#ifdef ROCKIT
  YAP_UserCPredicate("cuda_init_query", cuda_init_query, 1);
#endif

}

