
#include <YapInterface.h>

#define TERM YAP_Term

//extern TERM bp_get_call_arg(int i, int arity);
#define bp_get_call_arg( i,  arity) YAP_A(i)

//extern int bp_is_atom(TERM t)
#define bp_is_atom(t) YAP_IsAtomTerm(t)

//extern int bp_is_integer(TERM t)
#define bp_is_integer(t) YAP_IsIntTerm(t)

//extern int bp_is_float(TERM t)
#define bp_is_float(t) YAP_IsFloatTerm(t)

//extern int bp_is_nil(TERM t)
#define bp_is_nil(t) YAP_IsTermNil(t)

//extern int bp_is_list(TERM t)
#define bp_is_nil(t) YAP_IsPairTerm(t)

//extern int bp_is_structure(TERM t)
#define bp_is_nil(t) YAP_IsApplTerm(t)

//extern int bp_is_compound(TERM t)
#define bp_is_nil(t) ( YAP_IsApplTerm(t) || YAP_IsPairTerm(t) )

//extern int bp_is_unifiable(TERM t1, Term t2)
#define bp_is_unifiable(t1, t2) YAP_unifiable_NOT_IMPLEMENTED(t1, t2)

//extern int bp_is_identical(TERM t1, Term t2)
#define bp_is_identical(t1, t2) YAP_ExactlyEqual(t1, t2)

// int bp_get_integer(TERM t)
#define bp_get_integer(t) YAP_IntOfTerm(t)

// double bp_get_float(TERM t)
#define bp_get_float(t)  YAP_FloatOfTerm(t)

// char *bp_get_name(TERM t)
static inline char *
bp_get_name(TERM t)
{
  if (YAP_IsAtomTerm(t)) {
    return YAP_AtomName(YAP_AtomOfTerm(t));
  }
  if (YAP_IsApplTerm(t)) {
    return YAP_AtomName(YAP_NameOfFunctor(YAP_FunctorOfTerm(t)));
  }
  // exception = illegal_arguments;
  return NULL;
}


// char *bp_get_name(TERM t)
static inline int
bp_get_arity(TERM t)
{
  if (YAP_IsAtomTerm(t)) {
    return 0;
  }
  if (YAP_IsApplTerm(t)) {
    return (int)YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)));
  }
  // exception = illegal_arguments;
  return NULL;
}

//extern int bp_unify(TERM t1, TERM t2)
#define bp_unify(t1, t2) YAP_Unify(t1, t2)

//TERM bp_get_arg(int i, TERM t)
#define bp_get_arg(i, t) YAP_ArgOfTerm(i, t)

//TERM bp_get_car(Term t)
#define bp_get_car(t) YAP_HeadOfTerm(i, t)

//TERM bp_get_cdr(Term t)
#define bp_get_cdr(t) YAP_TailOfTerm(i, t)

// void bp_write(TERM t)
#define bp_write(t) YAP_WriteTerm(t, NULL, 0)

// TERM bp_build_var()
#define bp_build_var(t) YAP_MkVarTerm()

// TERM bp_build_integer(int i)
#define bp_build_integer(i) YAP_MkIntTerm(i)

// TERM bp_build_float(double f)
#define bp_build_float(f) YAP_MkFloatTerm(f)

// TERM bp_build_atom(char *name)
#define bp_build_atom(name) YAP_MkAtomTerm(YAP_LookupAtom(name))

// TERM bp_build_nil()
#define bp_build_nil() YAP_TermNil()

// TERM bp_build_list()
#define bp_build_list() YAP_MkNewPairTerm()

// TERM bp_build_structure(char *name, int arity)
#define bp_build_structure(name, arity) YAP_MkNewApplTerm(YAP_MkFunctor(YAP_LookupAtom(name),arity), arity)

// TERM bp_insert_pred(char *name, int arity, int (*func)())
#define bp_insert_pred(name, arity, func) YAP_UserCPredicate(name, func, arity)

// int bp_call_string(char *goal)
#define bp_call_string(goal) YAP_RunGoal(YAP_ReadBuffer(goal, NULL))

// int bp_call_term(TERM goal)
#define bp_call_term(goal) YAP_RunGoal(goal)

// void bp_mount_query_string(char *goal)
#define bp_mount_query_string(goal) bp_t = YAP_ReadBuffer(goal, NULL);

// void bp_mount_query_term(TERM goal)
// #define bp_mount_query_term(goal) bp_t = t;

// TERM bp_next_solution()
static int bp_next_solution(void) 
{
  if (bp_t) {
    Term t = bp_t;
    bp_t = NULL;
    return YAP_RunGoal(YAP_ReadBuffer(goal, NULL));
  }
  return YAP_RestartGoal();
}










