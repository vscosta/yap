#include <YapInterface.h>
#if CRYPTOMINISAT
#include "cryptominisat5/cryptominisat.h"
#define Solver SATSolver

using namespace std;
#include <vector>
#define vec std::vector
#define push(E) push_back(E)
#define pop() pop_back()
#define last() back()
#define setSeed(seed) set_seed(seed)
#define newVar() new_var()
#define addClause(C) add_clause(C)
#define model get_model()
#else
#include "Solver.h"
#endif

namespace @Solver@ {

Solver      *s = NULL;
int         seed=0;

vec<Solver*> cache_slvrs;


extern "C" YAP_Term v2t(int i)
{
  if (s->model[i] == l_True) {
    return  YAP_MkIntTerm(i+1);
  }
  if (s->model[i] == l_False) {
    return  YAP_MkIntTerm(-i-1);
  }

  return YAP_MkIntTerm(0);
}

extern "C" bool @solver@_default_seed()
{
  seed = YAP_IntOfTerm(YAP_ARG1);
  return true;
}


extern "C" bool @solver@_new_solver()
{
  if (s) {
    fprintf(stderr,"%% Warning: @solver@_new_solver deleted existing solver !\n");
    delete s;
    s = NULL;
  }
  s = new Solver;
  if (seed != 0) {
    #if CRYPTOMINISAT
        s->setSeed(seed);
    #else
    s->random_seed = seed;
#endif

  }
  //    s->verbosity=10;
  return true;
}

extern "C" bool @solver@_delete_solver()
{
    if (s) {
      delete s;
      s = NULL;
    } else {
    fprintf(stderr,"%% Warning: @solver@_delete_solver didn't had a solver to delete !\n");
    }
    return true;
}


extern "C" bool @solver@_cache_push_solver()
{
  if (s) {
    cache_slvrs.push(s);
    s = NULL;
    return true;
  } else {
  fprintf(stderr,"%% Error: @solver@_cache_push_solver failed since no active SAT solver !\n");
    return   false;
  }
}


extern "C" bool @solver@_cache_pop_solver()
{
  if (cache_slvrs.size() == 0) {
fprintf(stderr,"%% Error: @solver@_cache_pop_solver failed since no cached SAT solver !\n");
    return   false;
  }

  if (s) {
fprintf(stderr,"%% Warning: @solver@_cache_pop_solver deleted existing solver !\n");
    delete s;
    s = NULL;
  }
  s = cache_slvrs.last();
  cache_slvrs.pop();
  return true;
}


static inline Lit pl2lit(YAP_Term t)
{
  int pl_lit_int, var;
  pl_lit_int = YAP_IntOfTerm(t);
  var = abs(pl_lit_int)-1;
  while (var >= s->nVars()) s->newVar();
  #if CRYPTOMINISAT
    return Lit(var,!(pl_lit_int > 0));
  #else
  return mkLit(var,!(pl_lit_int > 0));
  #endif

}


extern "C" bool @solver@_add_clause()
{
    YAP_Term head;      /* variable for the elements */
    YAP_Term list = YAP_ARG1;    /* copy as we need to write */
    
    vec<Lit> lits;

    while( YAP_IsPairTerm(list)) {
	head = YAP_HeadOfTerm(list);
      lits.push( pl2lit(head) );
	list = YAP_TailOfTerm( list);
    }
      if (list != YAP_TermNil())
	return false;

      //assert(PL_get_nil(list));
    
    return s->addClause(lits);
}


extern "C" bool @solver@_solve() {
#if CRYPTOMINISAT
lbool x=s->solve();
return x==l_True;
#else
return s->solve();
#endif
}


extern "C" bool @solver@_solve_assumptions() {
YAP_Term head;      /* variable for the elements */
YAP_Term list = YAP_ARG1;    /* copy as we need to write */

    vec<Lit> lits;


    while( YAP_IsPairTerm(list)) {
	head = YAP_HeadOfTerm(list);
      lits.push( pl2lit(head) );
	list = YAP_TailOfTerm( list);
    }
if (list != YAP_TermNil())
	return false;
      
      // assert(PL_get_nil(list));

  #if CRYPTOMINISAT
  lbool rc =  s->solve();
  return rc == l_True;
  #else
     return s->solve();
     #endif
}


extern "C" bool @solver@_get_var_assignment()
{
  int i;

  i = YAP_IntOfTerm(YAP_ARG1);
  i--;

  if (i < s->nVars()) {
    return YAP_Unify(YAP_ARG2,v2t(i));
  } else {
    return false;
  }
}


extern "C" bool @solver@_get_model()
{
    YAP_Term l = YAP_TermNil();

    int i=s->nVars();
    while( --i >= 0 ) {
      l = YAP_MkPairTerm(  v2t(i),l );
    }

    return YAP_Unify(YAP_ARG1,l);
}


extern "C" bool @solver@_assign_model()
{
    YAP_Term asgnList = YAP_ARG1;      /* variable for the elements */

    int indx=0;
	while( YAP_IsPairTerm(asgnList) ) {
	  YAP_Term asgnVar = YAP_HeadOfTerm(asgnList);
	  YAP_Unify(v2t(indx), asgnVar);
	  indx++;
	  asgnList = YAP_TailOfTerm(asgnList);
	}

    return true;
}


extern "C" bool @solver@_nvars()
{
  return YAP_Unify(YAP_ARG1,YAP_MkIntTerm(s->nVars()));
}

} // @solver@



//=============================================================================


 typedef struct {
   const char *name;
     int arity;
   YAP_UserCPred f;
   int opts;
 } extension;
 
static extension predicates[] =
    {
        //
        //  { "name", arity, function, PL_FA_<flags> },
        //

      { "solver_new_solver",         0, @Solver@::@solver@_new_solver,         0 },
      { "solver_delete_solver",      0, @Solver@::@solver@_delete_solver,      0 },
      { "solver_cache_push_solver",  0, @Solver@::@solver@_cache_push_solver,  0 },
      { "solver_cache_pop_solver",   0, @Solver@::@solver@_cache_pop_solver,   0 },
      { "solver_add_clause",         1, @Solver@::@solver@_add_clause,         0 },
      { "solver_solve",              0, @Solver@::@solver@_solve,              0 },
      { "solver_solve_assumptions",  1, @Solver@::@solver@_solve_assumptions,  0 },
      { "solver_get_var_assignment", 2, @Solver@::@solver@_get_var_assignment, 0 },
      { "solver_get_model",          1, @Solver@::@solver@_get_model,          0 },
      { "solver_assign_model",       1, @Solver@::@solver@_assign_model,       0 },
      { "solver_nvars",              1, @Solver@::@solver@_nvars,              0 },
      { "solver_default_seed",       1, @Solver@::@solver@_default_seed,       0 },
      { nullptr,                         0, nullptr,                              0 }    // terminating line
    };

//-----------------------------------------------------------------------------
extern "C" bool install()
{
extension *pt = predicates;

    fprintf(stderr,"%% Yap interface to  @Solver@::@solver@ ... ");

    while (pt->name) {
        YAP_UserCPredicate(pt->name, pt->f, pt->arity);
	pt++;
    }
    /* This is the only PL_ call allowed */
    /* before PL_initialise().  It */
    /* ensures the foreign predicates */
    /* are available before loading */
    /* Prolog code */

    fprintf(stderr,"OK\n");
    return true;
}
