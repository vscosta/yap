//#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdio.h>
#include <assert.h>

#include "Solver.h"

#define val(i) ((s->model[i] != l_Undef) ? ((s->model[i]==l_True)? i+1:-1*(i+1)):0)

Solver      *s = NULL;


extern "C" foreign_t minisat_new_solver()
{
  s = new Solver;
  PL_succeed;
}


extern "C"  foreign_t minisat_delete_solver()
{
    if (s) {
      delete s;
      s = NULL;
    }
    PL_succeed;
}

static inline Lit pl2lit(term_t pl_literal)
{
  int pl_lit_int, var;
  PL_get_integer(pl_literal,&pl_lit_int);
  var = abs(pl_lit_int)-1;
  while (var >= s->nVars()) s->newVar();
  return (pl_lit_int > 0) ? Lit(var) : ~Lit(var);
}


extern "C" foreign_t minisat_set_minvars(term_t l)
{
    term_t head = PL_new_term_ref();      /* variable for the elements */
    term_t list = PL_copy_term_ref(l);    /* copy as we need to write */
    
    vec<Lit> lits;

    while( PL_get_list(list, head, list) ) {
      lits.push( pl2lit(head) );
    }

    assert(PL_get_nil(list));

    if (s->setminVars(lits)) PL_succeed; else PL_fail;
}

extern "C" foreign_t minisat_add_clause(term_t l)
{
    term_t head = PL_new_term_ref();      /* variable for the elements */
    term_t list = PL_copy_term_ref(l);    /* copy as we need to write */
    
    vec<Lit> lits;

    while( PL_get_list(list, head, list) ) {
      lits.push( pl2lit(head) );
    }

    assert(PL_get_nil(list));
    
    if (s->addClause(lits)) PL_succeed; else PL_fail;
}


extern "C" foreign_t minisat_solve(term_t assum) {

    term_t head = PL_new_term_ref();      /* variable for the elements */
    term_t list = PL_copy_term_ref(assum);    /* copy as we need to write */
  
    vec<Lit> assumptions;

    while( PL_get_list(list, head, list) ) {
      assumptions.push( pl2lit(head) );
    }

    if (s->solve(assumptions)) PL_succeed; else PL_fail;
}


extern "C" foreign_t minisat_get_var_assignment(term_t var, term_t res)
{
  int i;

  PL_get_integer(var,&i);
  i--;

  if (i < s->nVars()) {
    term_t a = PL_new_term_ref();      /* variable for the elements */  
    PL_put_integer(a, val(i));
    return PL_unify(a,res);
  } else {
    PL_fail;
  }
}

extern "C" foreign_t minisat_nvars(term_t res)
{
  term_t a = PL_new_term_ref();      /* variable for the elements */  
  PL_put_integer(a, s->nVars());
  return PL_unify(a,res);
}








//=============================================================================
static const PL_extension predicates[] =
    {
        //
        //  { "name", arity, function, PL_FA_<flags> },
        //

      { "minisat_new_solver",         0, (void*)minisat_new_solver,         0 },
      { "minisat_delete_solver",      0, (void*)minisat_delete_solver,      0 },
      { "minisat_add_clause",         1, (void*)minisat_add_clause,         0 },
      { "minisat_solve",              1, (void*)minisat_solve,              0 },
      { "minisat_get_var_assignment", 2, (void*)minisat_get_var_assignment, 0 },
      { "minisat_nvars",              1, (void*)minisat_nvars,              0 },
      { NULL,                         0, NULL,                              0 }    // terminating line
    };

//-----------------------------------------------------------------------------
extern "C" install_t install()
{
    //Sdprintf("%% SWI-Prolog interface to MiniSat");
    //Sdprintf(" - built on ");
    //Sdprintf(__DATE__);
    //Sdprintf(", ");
    //Sdprintf(__TIME__);
    //Sdprintf(" ... ");
    PL_register_extensions(predicates);	/* This is the only PL_ call allowed */
					/* before PL_initialise().  It */
					/* ensures the foreign predicates */
					/* are available before loading */
					/* Prolog code */

    //Sdprintf("OK\n");
}

//-----------------------------------------------------------------------------
// This part is for compiling into a standalone executable

#ifdef READLINE
static void install_readline(int argc, char**argv)
{
    PL_install_readline();
}
#endif

int main(int argc, char **argv)
{

#ifdef READLINE
    PL_initialise_hook(install_readline);
#endif

    install();
    if ( !PL_initialise(argc, argv) )
	PL_halt(1);
    
    PL_halt(PL_toplevel() ? 0 : 1);
    
    return 0;
}
