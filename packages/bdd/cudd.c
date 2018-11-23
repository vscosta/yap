/**
    @defgroup CUDD CUDD Interface
@ingroup BDDs

   @brief Interface to the CUDD Library

   CUDD represents a BDD as a tree of DdNode structures. Each tree has a manager
DdManager and a list of booleaan variables, also represented as DdNode
structures. Mapping from an Prolog tree to a ground BDD involves the following
steps:

1. Collect all logical variables in the Prolog term, and map each
variable $V$ to a boolean variable $i$ in the BDD. This is easily done
by having the variable as the argument argument $i$ of a Prolog
term. The implementation uses vars_of_term/2 and =../2.

2. Allocate an array of boolean variables.

3. Perform a posfix visit of the Prolog term, so that a new DdNode is
always obtained by composing its children nodes.

YAP supports a few tricks:

+ A term of the form `cudd(_Address_)` refers to a compiled BDD. Thus,
we can pass a BDD to another BDD, ie:

~~~~~.pl
bdd(BDD) :-
     Vs = vs(X,Y,Z),
     bdd_new(X+(Y*Z),Vs,BDD0),
     bdd_new(xor(BDD0,-(nand(X,BDD0) + nor(Y,BDD0)) ), Vs, BDD).
~~~~~

This is useful to construct complex BDDs quickly, but does not mean
CUDD will generate better/faster code.


2.

 */
#include <stdio.h>

#include "YapInterface.h"
#include "cudd_config.h"

#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_CUDDINT_H
#include "cuddInt.h"
#elif HAVE_CUDD_CUDDINT_H
#include "cudd/cuddInt.h"
#endif

static YAP_Functor FunctorDollarVar, FunctorCudd, FunctorAnd, FunctorAnd4,
    FunctorOr, FunctorOr4, FunctorLAnd, FunctorLOr, FunctorNot, FunctorMinus1,
    FunctorXor, FunctorNand, FunctorNor, FunctorTimes, FunctorImplies,
    FunctorPlus, FunctorMinus, FunctorTimes4, FunctorPlus4, FunctorOutAdd,
    FunctorOutPos, FunctorOutNeg;

static YAP_Term TermMinusOne, TermZero, TermPlusOne, TermTrue, TermFalse;

void init_cudd(void);

static DdNode *cudd_and(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddAnd(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *cudd_nand(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddNand(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *cudd_or(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddOr(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *cudd_nor(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddNor(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *cudd_xor(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddXor(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *term_to_cudd(DdManager *manager, YAP_Term t) {
  if (YAP_IsApplTerm(t)) {
    YAP_Functor f = YAP_FunctorOfTerm(t);
    if (f == FunctorDollarVar) {
      int i = YAP_IntOfTerm(YAP_ArgOfTerm(1, t));
      DdNode *var = Cudd_bddIthVar(manager, i);
      if (!var)
        return NULL;
      Cudd_Ref(var);
      return var;
    } else if (f == FunctorAnd || f == FunctorLAnd || f == FunctorTimes) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp;
      if (!x1 || !x2)
        return NULL;
      tmp = cudd_and(manager, x1, x2);
      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorAnd4) {
      YAP_Term t1 = YAP_ArgOfTerm(2, t);
      if (YAP_IsVarTerm(t1)) {
        YAP_Int refs = YAP_IntOfTerm(YAP_ArgOfTerm(1, t)), i;
        DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(3, t));
        DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(4, t));
        DdNode *tmp;
        if (!x1 || !x2)
          return NULL;
        tmp = cudd_and(manager, x1, x2);
        for (i = 0; i < refs; i++) {
          Cudd_Ref(tmp);
        }
        Cudd_RecursiveDeref(manager, x1);
        Cudd_RecursiveDeref(manager, x2);
        YAP_Unify(t1, YAP_MkIntTerm((YAP_Int)tmp));
        return tmp;
      } else {
        return (DdNode *)YAP_IntOfTerm(t1);
      }
    } else if (f == FunctorCudd) {
      YAP_Term t1 = YAP_ArgOfTerm(1, t);
      DdNode *tmp = (DdNode *)YAP_IntOfTerm(t1);
      Cudd_Ref(tmp);
      return tmp;
    } else if (f == FunctorOr || f == FunctorLOr || f == FunctorPlus) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp;
      if (!x1 || !x2)
        return NULL;
      tmp = cudd_or(manager, x1, x2);
      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorXor) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp;
      if (!x1 || !x2)
        return NULL;
      tmp = cudd_xor(manager, x1, x2);
      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorOr4) {
      YAP_Term t1 = YAP_ArgOfTerm(2, t);
      if (YAP_IsVarTerm(t1)) {
        YAP_Int refs = YAP_IntOfTerm(YAP_ArgOfTerm(1, t)), i;
        DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(3, t));
        DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(4, t));
        DdNode *tmp;
        if (!x1 || !x2)
          return NULL;
        tmp = cudd_or(manager, x1, x2);
        for (i = 0; i < refs; i++) {
          Cudd_Ref(tmp);
        }
        Cudd_RecursiveDeref(manager, x1);
        Cudd_RecursiveDeref(manager, x2);
        YAP_Unify(t1, YAP_MkIntTerm((YAP_Int)tmp));
        return tmp;
      } else {
        return (DdNode *)YAP_IntOfTerm(t1);
      }
    } else if (f == FunctorNor) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp;
      if (!x1 || !x2)
        return NULL;
      tmp = cudd_nor(manager, x1, x2);
      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorNand) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_cudd(manager, YAP_ArgOfTerm(2, t));
      if (!x1 || !x2)
        return NULL;
      DdNode *tmp = cudd_nand(manager, x1, x2);
      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorNot || FunctorMinus1) {
      DdNode *x1 = term_to_cudd(manager, YAP_ArgOfTerm(1, t));
      if (!x1)
        return NULL;
      return Cudd_Not(x1);
    } else {
      YAP_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "unsupported operator in CUDD");
      return NULL;
    }
  } else if (YAP_IsIntTerm(t)) {
    YAP_Int i = YAP_IntOfTerm(t);
    if (i == 0)
      return Cudd_ReadLogicZero(manager);
    else if (i == 1)
      return Cudd_ReadOne(manager);
    else {
      YAP_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "unsupported number in CUDD");
      return NULL;
    }
  } else if (YAP_IsFloatTerm(t)) {
    YAP_Int i = YAP_FloatOfTerm(t);
    if (i == 0.0)
      return Cudd_ReadLogicZero(manager);
    else if (i == 1.0)
      return Cudd_ReadOne(manager);
    else {
      YAP_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "unsupported number in CUDD");
      return NULL;
    }
  } else if (YAP_IsAtomTerm(t)) {
    if (t == TermFalse)
      return Cudd_ReadLogicZero(manager);
    else if (t == TermTrue)
      return Cudd_ReadOne(manager);
    else {
      YAP_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "unsupported atom %s in CUDD",
                YAP_AtomName(YAP_AtomOfTerm(t)));
      return NULL;
    }
  } else if (YAP_IsVarTerm(t)) {
    YAP_Error(INSTANTIATION_ERROR, t, "unsupported unbound term in CUDD");
    return NULL;
  }
  YAP_Error(DOMAIN_ERROR_OUT_OF_RANGE, t, "unsupported number in CUDD");
  return NULL;
}

static YAP_Bool p_term_to_cudd(void) {
  DdManager *manager;
  DdNode *t;

  if (YAP_IsVarTerm(YAP_ARG2)) {
    manager = Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
    // Cudd_AutodynEnable(manager, CUDD_REORDER_SIFT);
    if (!YAP_Unify(YAP_ARG2, YAP_MkIntTerm((YAP_Int)manager)))
      return FALSE;
  } else {
    manager = (DdManager *)YAP_IntOfTerm(YAP_ARG2);
  }
  t = term_to_cudd(manager, YAP_ARG1);
  if (!t)
    return FALSE;
  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm((YAP_Int)t));
}

static DdNode *add_times(DdManager *manager, DdNode *x1, DdNode *x2) {
  DdNode *tmp;

  tmp = Cudd_addApply(manager, Cudd_addTimes, x2, x1);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *add_implies(DdManager *manager, DdNode *x1, DdNode *x2) {
  DdNode *tmp;

  tmp = Cudd_addConst(manager, Cudd_addLeq(manager, x1, x2));
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *add_plus(DdManager *manager, DdNode *x1, DdNode *x2) {
  DdNode *tmp;

  tmp = Cudd_addApply(manager, Cudd_addPlus, x2, x1);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *add_minus(DdManager *manager, DdNode *x1, DdNode *x2) {
  DdNode *tmp;

  tmp = Cudd_addApply(manager, Cudd_addMinus, x1, x2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *add_lor(DdManager *manager, DdNode *x1, DdNode *x2) {
  DdNode *tmp;

  tmp = Cudd_addApply(manager, Cudd_addOr, x1, x2);
  Cudd_Ref(tmp);
  return tmp;
}

static DdNode *term_to_add(DdManager *manager, YAP_Term t) {
  if (YAP_IsApplTerm(t)) {
    YAP_Functor f = YAP_FunctorOfTerm(t);
    if (f == FunctorDollarVar) {
      int i = YAP_IntOfTerm(YAP_ArgOfTerm(1, t));
      DdNode *var = Cudd_addIthVar(manager, i);
      return var;
    } else if (f == FunctorTimes) {
      DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp = add_times(manager, x1, x2);

      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorTimes4) {
      YAP_Term t1 = YAP_ArgOfTerm(2, t);
      if (YAP_IsVarTerm(t1)) {
        YAP_Int refs = YAP_IntOfTerm(YAP_ArgOfTerm(1, t)), i;
        DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(3, t));
        DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(4, t));
        DdNode *tmp = add_times(manager, x1, x2);

        for (i = 0; i < refs; i++) {
          Cudd_Ref(tmp);
        }
        Cudd_RecursiveDeref(manager, x1);
        Cudd_RecursiveDeref(manager, x2);
        YAP_Unify(t1, YAP_MkIntTerm((YAP_Int)tmp));
        return tmp;
      } else {
        return (DdNode *)YAP_IntOfTerm(t1);
      }
    } else if (f == FunctorPlus) {
      DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp = add_plus(manager, x1, x2);

      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorLOr || f == FunctorOr) {
      DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp = add_lor(manager, x1, x2);

      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorMinus) {
      DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp = add_minus(manager, x1, x2);

      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorImplies) {
      DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(1, t));
      DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(2, t));
      DdNode *tmp = add_implies(manager, x1, x2);

      Cudd_RecursiveDeref(manager, x1);
      Cudd_RecursiveDeref(manager, x2);
      return tmp;
    } else if (f == FunctorTimes4) {
      YAP_Term t1 = YAP_ArgOfTerm(2, t);
      if (YAP_IsVarTerm(t1)) {
        YAP_Int refs = YAP_IntOfTerm(YAP_ArgOfTerm(1, t)), i;
        DdNode *x1 = term_to_add(manager, YAP_ArgOfTerm(3, t));
        DdNode *x2 = term_to_add(manager, YAP_ArgOfTerm(4, t));
        DdNode *tmp = add_plus(manager, x1, x2);

        for (i = 0; i < refs; i++) {
          Cudd_Ref(tmp);
        }
        Cudd_RecursiveDeref(manager, x1);
        Cudd_RecursiveDeref(manager, x2);
        YAP_Unify(t1, YAP_MkIntTerm((YAP_Int)tmp));
        return tmp;
      } else {
        return (DdNode *)YAP_IntOfTerm(t1);
      }
    }
  } else if (YAP_IsIntTerm(t)) {
    YAP_Int i = YAP_IntOfTerm(t);
    DdNode *tmp = Cudd_addConst(manager, i);

    Cudd_Ref(tmp);
    return tmp;
  } else if (YAP_IsFloatTerm(t)) {
    double d = YAP_FloatOfTerm(t);
    DdNode *tmp = Cudd_addConst(manager, d);

    Cudd_Ref(tmp);
    return tmp;
  }
  return NULL;
}

static YAP_Bool p_term_to_add(void) {
  DdManager *manager = Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
  int sz = YAP_IntOfTerm(YAP_ARG2), i;
  DdNode *t;
  for (i = sz - 1; i >= 0; i--) {
    Cudd_addIthVar(manager, i);
  }
  t = term_to_add(manager, YAP_ARG1);
  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm((YAP_Int)manager)) &&
         YAP_Unify(YAP_ARG4, YAP_MkIntTerm((YAP_Int)t));
}

static YAP_Bool complement(int i) { return i == 0 ? 1 : 0; }

static YAP_Bool var(DdManager *manager, DdNode *n, YAP_Int *vals) {
  return (int)vals[Cudd_ReadPerm(manager, Cudd_NodeReadIndex(n))];
}

static YAP_Bool cudd_eval(DdManager *manager, DdNode *n, YAP_Int *vals) {
  if (Cudd_IsConstant(n)) {
    //    fprintf(stderr,"v=%f\n",Cudd_V(n));
    return Cudd_V(n);
  } else {
    //    fprintf(stderr,"%x %d->%d %d\n",n->index,var(manager, n,
    //    vals),(Cudd_IsComplement(Cudd_E(n))!=0));
    if (var(manager, n, vals) == 1)
      return cudd_eval(manager, Cudd_T(n), vals);
    else {
      DdNode *r = Cudd_E(n);
      if (Cudd_IsComplement(r)) {
        return complement(cudd_eval(manager, Cudd_Regular(r), vals));
      } else {
        return cudd_eval(manager, r, vals);
      }
    }
  }
}

static YAP_Bool cudd_eval_top(DdManager *manager, DdNode *n, YAP_Int *vals) {
  if (Cudd_IsComplement(n)) {
    return complement(cudd_eval(manager, Cudd_Regular(n), vals));
  } else {
    return cudd_eval(manager, n, vals);
  }
}

static YAP_Bool p_eval_cudd(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n = (DdNode *)YAP_IntOfTerm(YAP_ARG2);
  size_t sz = YAP_ArityOfFunctor(YAP_FunctorOfTerm(YAP_ARG3));
  int val;
  YAP_Int *ar;
  YAP_Term t = YAP_ARG3;
  YAP_Int i;

  if (sz <= 0)
    return FALSE;
  ar = (YAP_Int *)malloc(sz * sizeof(YAP_Int));
  if (!ar)
    return FALSE;
  for (i = 0; i < sz; i++) {
    YAP_Term tj = YAP_ArgOfTerm(i + 1, t);
    if (!YAP_IsIntTerm(tj))
      return FALSE;
    ar[i] = YAP_IntOfTerm(tj);
  }
  val = cudd_eval_top(manager, n, ar);
  free(ar);
  return YAP_Unify(YAP_ARG4, YAP_MkIntTerm(val));
}

static double add_eval(DdManager *manager, DdNode *n, YAP_Int *vals) {
  if (Cudd_IsConstant(n)) {
    return Cudd_V(n);
  } else {
    if (var(manager, n, vals) == 1)
      return add_eval(manager, Cudd_T(n), vals);
    else {
      return add_eval(manager, Cudd_E(n), vals);
    }
  }
}

static YAP_Bool p_eval_add(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n = (DdNode *)YAP_IntOfTerm(YAP_ARG2);
  size_t sz = YAP_ArityOfFunctor(YAP_FunctorOfTerm(YAP_ARG3));
  double val;
  YAP_Int *ar;
  YAP_Term t = YAP_ARG3;
  YAP_Int i;

  if (sz <= 0)
    return FALSE;
  ar = (YAP_Int *)malloc(sz * sizeof(YAP_Int));
  if (!ar)
    return FALSE;
  for (i = 0; i < sz; i++) {
    YAP_Term tj = YAP_ArgOfTerm(i + 1, t);
    if (!YAP_IsIntTerm(tj))
      return FALSE;
    ar[i] = YAP_IntOfTerm(tj);
  }
  val = add_eval(manager, n, ar);
  free(ar);
  return YAP_Unify(YAP_ARG4, YAP_MkFloatTerm(val));
}

typedef struct {
  DdNode *key;
  YAP_Term val;
} hash_table_entry;

static void insert(hash_table_entry *p, DdNode *key, YAP_Term val, size_t sz) {
  size_t el = (((YAP_Term)key) / sizeof(DdNode *)) % sz;
  while (p[el].key) {
    el = (el + 1) % sz;
  }
  p[el].key = key;
  p[el].val = val;
}

static YAP_Term lookup(hash_table_entry *p, DdNode *key, size_t sz) {
  size_t el = (((YAP_Term)key) / sizeof(DdNode *)) % sz;
  while (p[el].key != key) {
    el = (el + 1) % sz;
  }
  return p[el].val;
}

static YAP_Term build_prolog_cudd(DdManager *manager, DdNode *n, YAP_Term *ar,
                                  hash_table_entry *hash, YAP_Term t0,
                                  size_t sz) {
  if (Cudd_IsConstant(n)) {
    YAP_Term t = YAP_MkIntTerm(Cudd_V(n));
    insert(hash, n, t, sz);
    return t0;
  } else {
    // fprintf(stderr,"%x %d->%d %d\n",n->index,
    // Cudd_ReadPerm(manager,Cudd_NodeReadIndex(n)),var(manager, n,
    // vals),Cudd_IsComplement(Cudd_E(n)));
    YAP_Term t[4], nt;
    YAP_Functor f;

    // fprintf(stderr,"refs=%d\n", n->ref);
    t[0] = YAP_MkVarTerm();
    t[1] = ar[Cudd_ReadPerm(manager, Cudd_NodeReadIndex(n))];
    t[2] = lookup(hash, Cudd_T(n), sz);
    t[3] = lookup(hash, Cudd_Regular(Cudd_E(n)), sz);
    if (Cudd_IsComplement(Cudd_E(n))) {
      f = FunctorOutNeg;
    } else {
      f = FunctorOutPos;
    }
    nt = YAP_MkApplTerm(f, 4, t);
    insert(hash, n, t[0], sz);
    return YAP_MkPairTerm(nt, t0);
  }
}

static inline int max(int a, int b) { return a < b ? b : a; }

static YAP_Int get_vars(YAP_Term t3) {
  if (YAP_IsAtomTerm(t3))
    return 0;
  return YAP_ArityOfFunctor(YAP_FunctorOfTerm(t3));
}

static YAP_Bool p_cudd_reorder(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  return Cudd_ReduceHeap(manager, CUDD_REORDER_EXACT, 1);
}

static YAP_Bool p_cudd_to_term(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2), *node;
  YAP_Term t, t3 = YAP_ARG3, td;
  YAP_Int i, vars = get_vars(t3);
  int nodes = max(0, Cudd_ReadNodeCount(manager)) + vars + 1;
  size_t sz = nodes * 4;
  DdGen *dgen = Cudd_FirstNode(manager, n0, &node);
  hash_table_entry *hash =
      (hash_table_entry *)calloc(sz, sizeof(hash_table_entry));
  YAP_Term *ar;

  if (!dgen || !hash)
    return FALSE;
  ar = (YAP_Term *)malloc(vars * sizeof(YAP_Term));
  if (!ar)
    return FALSE;
restart:
  t = YAP_TermNil();
  for (i = 0; i < vars; i++) {
    ar[i] = YAP_ArgOfTerm(i + 1, t3);
  }
  while (node) {
    /* ensure we have enough memory */
    if (YAP_RequiresExtraStack(0)) {
      Cudd_GenFree(dgen);
      t3 = YAP_ARG3;
      dgen = Cudd_FirstNode(manager, n0, &node);
      bzero(hash, sizeof(hash_table_entry) * sz);
      goto restart;
    }
    t = build_prolog_cudd(manager, node, ar, hash, t, sz);
    if (!Cudd_NextNode(dgen, &node))
      break;
  }
  if (node != n0 && Cudd_IsComplement(n0)) {
    td = YAP_MkIntTerm(-1);
  } else {
    td = YAP_MkIntTerm(1);
  }
  Cudd_GenFree(dgen);
  free(hash);
  free(ar);
  return YAP_Unify(YAP_ARG4, td) && YAP_Unify(YAP_ARG5, t);
}

static YAP_Term build_prolog_add(DdManager *manager, DdNode *n, YAP_Term *ar,
                                 hash_table_entry *hash, YAP_Term t0,
                                 size_t sz) {
  if (Cudd_IsConstant(n)) {
    YAP_Term t = YAP_MkFloatTerm(Cudd_V(n));
    insert(hash, n, t, sz);
    return t0;
  } else {
    YAP_Term t[4], nt;
    YAP_Functor f;

    // fprintf(stderr,"refs=%d\n", n->ref);
    t[0] = YAP_MkVarTerm();
    t[1] = ar[Cudd_ReadPerm(manager, Cudd_NodeReadIndex(n))];
    t[2] = lookup(hash, Cudd_T(n), sz);
    t[3] = lookup(hash, Cudd_E(n), sz);
    f = FunctorOutAdd;
    nt = YAP_MkApplTerm(f, 4, t);
    insert(hash, n, t[0], sz);
    return YAP_MkPairTerm(nt, t0);
  }
}

static YAP_Bool p_add_to_term(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2), *node;
  YAP_Term t, t3 = YAP_ARG3;
  YAP_Int i, vars = get_vars(t3);
  int nodes = max(0, Cudd_ReadNodeCount(manager)) + vars + 1;
  size_t sz = nodes * 4;
  DdGen *dgen = Cudd_FirstNode(manager, n0, &node);
  hash_table_entry *hash =
      (hash_table_entry *)calloc(sz, sizeof(hash_table_entry));
  YAP_Term *ar;

  if (!dgen)
    return FALSE;
  ar = (YAP_Term *)malloc(vars * sizeof(YAP_Term));
  if (!ar)
    return FALSE;
restart:
  t = YAP_TermNil();
  for (i = 0; i < vars; i++) {
    ar[i] = YAP_ArgOfTerm(i + 1, t3);
  }
  while (node) {
    /* ensure we have enough memory */
    if (YAP_RequiresExtraStack(0)) {
      Cudd_GenFree(dgen);
      t3 = YAP_ARG3;
      dgen = Cudd_FirstNode(manager, n0, &node);
      bzero(hash, sizeof(hash_table_entry) * sz);
      goto restart;
    }
    t = build_prolog_add(manager, node, ar, hash, t, sz);
    if (!Cudd_NextNode(dgen, &node))
      break;
  }
  Cudd_GenFree(dgen);
  free(hash);
  free(ar);
  return YAP_Unify(YAP_ARG4, t);
}

static YAP_Bool p_cudd_size(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2), *node;
  YAP_Int i = 0;
  DdGen *dgen = Cudd_FirstNode(manager, n0, &node);

  if (!dgen)
    return FALSE;
  while (node) {
    i++;
    if (!Cudd_NextNode(dgen, &node))
      break;
  }
  Cudd_GenFree(dgen);
  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(i));
}

typedef struct {
  DdNode *key;
  double val;
} hash_table_entry_dbl;

static void insert2(hash_table_entry_dbl *p, DdNode *key, double val,
                    size_t sz) {
  size_t el = (((YAP_Term)key) / sizeof(DdNode *)) % sz;
  while (p[el].key) {
    el = (el + 1) % sz;
  }
  p[el].key = key;
  p[el].val = val;
}

static double lookup2(hash_table_entry_dbl *p, DdNode *key, size_t sz) {
  size_t el = (((YAP_Term)key) / sizeof(DdNode *)) % sz;
  while (p[el].key != key) {
    el = (el + 1) % sz;
  }
  return p[el].val;
}

static double build_sp_cudd(DdManager *manager, DdNode *n, double *ar,
                            hash_table_entry_dbl *hash, size_t sz) {
  if (Cudd_IsConstant(n)) {
    insert2(hash, n, Cudd_V(n), sz);
    return Cudd_V(n);
  } else {
    // fprintf(stderr,"%x %d->%d %d\n",n->index,
    // Cudd_ReadPerm(manager,Cudd_NodeReadIndex(n)),var(manager, n,
    // vals),Cudd_IsComplement(Cudd_E(n)));
    double pl, pr, p, prob;

    prob = ar[Cudd_ReadPerm(manager, Cudd_NodeReadIndex(n))];
    pl = lookup2(hash, Cudd_T(n), sz);
    pr = lookup2(hash, Cudd_Regular(Cudd_E(n)), sz);
    if (Cudd_IsComplement(Cudd_E(n))) {
      p = prob * pl + (1 - prob) * (1 - pr);
    } else {
      p = prob * pl + (1 - prob) * pr;
    }
    insert2(hash, n, p, sz);
    return p;
  }
}

static YAP_Bool p_cudd_to_p(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2), *node;
  YAP_Term t3 = YAP_ARG3;
  double p = 0.0;
  YAP_Int vars = YAP_ListLength(t3);
  int nodes = max(Cudd_ReadNodeCount(manager), 0) + vars + 1;
  size_t sz = nodes * 4;
  DdGen *dgen = Cudd_FirstNode(manager, n0, &node);
  hash_table_entry_dbl *hash =
      (hash_table_entry_dbl *)calloc(sz, sizeof(hash_table_entry_dbl));
  double *ar;

  if (!dgen)
    return FALSE;
  ar = (double *)malloc(vars * sizeof(double));
  if (!ar)
    return FALSE;
  if (YAP_ListToFloats(t3, ar, vars) < 0)
    return FALSE;
  while (node) {
    p = build_sp_cudd(manager, node, ar, hash, sz);
    if (!Cudd_NextNode(dgen, &node))
      break;
  }
  if (node != n0 && Cudd_IsComplement(n0)) {
    p = 1 - p;
  }
  Cudd_GenFree(dgen);
  free(hash);
  free(ar);
  return YAP_Unify(YAP_ARG4, YAP_MkFloatTerm(p));
}

static YAP_Bool p_cudd_print(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2);
  const char *s = YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3));
  FILE *f;
  if (!strcmp(s, "user_output"))
    f = stdout;
  else if (!strcmp(s, "user_error"))
    f = stderr;
  else if (!strcmp(s, "user"))
    f = stdout;
  else
    f = fopen(s, "w");
  Cudd_DumpDot(manager, 1, &n0, NULL, NULL, f);
  if (f != stdout && f != stderr)
    fclose(f);
  return TRUE;
}

static YAP_Bool p_cudd_print_with_names(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2);
  const char *s = YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3));
  char **namesp;
  YAP_Term names = YAP_ARG4;
  FILE *f;
  YAP_Int len;
  YAP_Int i = 0;

  if (!strcmp(s, "user_output"))
    f = stdout;
  else if (!strcmp(s, "user_error"))
    f = stderr;
  else if (!strcmp(s, "user"))
    f = stdout;
  else
    f = fopen(s, "w");
  if ((len = YAP_ListLength(names)) < 0)
    return FALSE;
  if ((namesp = malloc(sizeof(const char *) * len)) == NULL)
    return FALSE;
  while (YAP_IsPairTerm(names)) {
    YAP_Term hd = YAP_HeadOfTerm(names);
    char *f;

    if (YAP_IsAtomTerm(hd)) {
      const char *s = YAP_AtomName(YAP_AtomOfTerm(hd));
      char *ns = malloc(strlen(s) + 1);
      strncpy(ns, s, strlen(s) + 1);
      f = ns;
    } else {
      size_t sz = 256;
      char *s = malloc(sz + 256);
      while (!YAP_WriteBuffer(hd, s, sz - 1, 0)) {
        sz += 1024;
        s = realloc(s, sz);
      }
      f = s;
    }
    names = YAP_TailOfTerm(names);
    namesp[i++] = f;
  }
  Cudd_DumpDot(manager, 1, &n0, (const char *const *)namesp, NULL, f);
  if (f != stdout && f != stderr)
    fclose(f);
  while (i > 0) {
    i--;
    free((void *)namesp[i]);
  }
  free((void *)namesp);
  return TRUE;
}

static YAP_Bool p_cudd_die(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  // Cudd_FreeTree(manager);
  // cuddFreeTable(manager);
  Cudd_CheckZeroRef(manager);
  Cudd_Quit(manager);
  return TRUE;
}

static YAP_Bool p_cudd_release_node(void) {
  DdManager *manager = (DdManager *)YAP_IntOfTerm(YAP_ARG1);
  DdNode *n0 = (DdNode *)YAP_IntOfTerm(YAP_ARG2);
  Cudd_RecursiveDeref(manager, n0);
  return TRUE;
}

void init_cudd(void) {

  FunctorDollarVar = YAP_MkFunctor(YAP_LookupAtom("$VAR"), 1);
  FunctorAnd = YAP_MkFunctor(YAP_LookupAtom("/\\"), 2);
  FunctorOr = YAP_MkFunctor(YAP_LookupAtom("\\/"), 2);
  FunctorLAnd = YAP_MkFunctor(YAP_LookupAtom("and"), 2);
  FunctorLOr = YAP_MkFunctor(YAP_LookupAtom("or"), 2);
  FunctorAnd4 = YAP_MkFunctor(YAP_LookupAtom("and"), 4);
  FunctorOr4 = YAP_MkFunctor(YAP_LookupAtom("or"), 4);
  FunctorXor = YAP_MkFunctor(YAP_LookupAtom("xor"), 2);
  FunctorNor = YAP_MkFunctor(YAP_LookupAtom("nor"), 2);
  FunctorNand = YAP_MkFunctor(YAP_LookupAtom("nand"), 2);
  FunctorTimes = YAP_MkFunctor(YAP_LookupAtom("*"), 2);
  FunctorPlus = YAP_MkFunctor(YAP_LookupAtom("+"), 2);
  FunctorMinus = YAP_MkFunctor(YAP_LookupAtom("-"), 2);
  FunctorTimes4 = YAP_MkFunctor(YAP_LookupAtom("*"), 4);
  FunctorPlus4 = YAP_MkFunctor(YAP_LookupAtom("+"), 4);
  FunctorImplies = YAP_MkFunctor(YAP_LookupAtom("->"), 2);
  FunctorNot = YAP_MkFunctor(YAP_LookupAtom("not"), 1);
  FunctorMinus1 = YAP_MkFunctor(YAP_LookupAtom("-"), 1);
  FunctorOutPos = YAP_MkFunctor(YAP_LookupAtom("pp"), 4);
  FunctorOutNeg = YAP_MkFunctor(YAP_LookupAtom("pn"), 4);
  FunctorOutAdd = YAP_MkFunctor(YAP_LookupAtom("add"), 4);
  FunctorCudd = YAP_MkFunctor(YAP_LookupAtom("cudd"), 1);
  TermMinusOne = YAP_MkIntTerm(-1);
  TermPlusOne = YAP_MkIntTerm(+1);
  TermZero = YAP_MkIntTerm(0);
  TermFalse = YAP_MkAtomTerm(YAP_LookupAtom("false"));
  TermTrue = YAP_MkAtomTerm(YAP_LookupAtom("true"));
  YAP_UserCPredicate("term_to_cudd", p_term_to_cudd, 3);
  YAP_UserCPredicate("term_to_add", p_term_to_add, 4);
  YAP_UserCPredicate("cudd_eval", p_eval_cudd, 4);
  YAP_UserCPredicate("add_eval", p_eval_add, 4);
  YAP_UserCPredicate("cudd_to_term", p_cudd_to_term, 5);
  YAP_UserCPredicate("add_to_term", p_add_to_term, 4);
  YAP_UserCPredicate("cudd_to_probability_sum_product", p_cudd_to_p, 4);
  YAP_UserCPredicate("cudd_size", p_cudd_size, 3);
  YAP_UserCPredicate("cudd_die", p_cudd_die, 1);
  YAP_UserCPredicate("cudd_reorder", p_cudd_reorder, 2);
  YAP_UserCPredicate("cudd_release_node", p_cudd_release_node, 2);
  YAP_UserCPredicate("cudd_print", p_cudd_print, 3);
  YAP_UserCPredicate("cudd_print", p_cudd_print_with_names, 4);
}

/**
 *@}
 */
