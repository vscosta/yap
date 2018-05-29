
#include "Yap.h"
#include "Yatom.h"

/**
 * Scan a list of arguments and output results to a pre-processed vector.
 *
 * @param listl: input list
 * @param def parameter definition
 *
 * @return all arguments, some of them set, some of them not.
 */

static xarg *matchKey(Atom key, xarg *e0, int n, const param_t *def) {
  int i;
  for (i = 0; i < n; i++) {
    if (!strcmp((char *)def->name, (char *)RepAtom(key)->StrOfAE)) {
      return e0;
    }
    def++;
    e0++;
  }
  return NULL;
}

/**
 * Returns the index of an argument key, or -1 if not found.
 *
 */
int Yap_ArgKey(Atom key, const param_t *def, int n) {
  int i;
  for (i = 0; i < n; i++) {
    if (!strcmp((char *)def->name, (char *)RepAtom(key)->StrOfAE)) {
      return i;
    }
    def++;
  }
  return -1;
}

#define YAP_XARGINFO(Error, Message)
#define failed(e, t, a) failed__(e, t, a PASS_REGS)

static xarg *failed__(yap_error_number e, Term t, xarg *a USES_REGS) {
  free(a);
  LOCAL_ActiveError->errorNo = e;
  LOCAL_ActiveError->errorRawTerm = t;
  return NULL;
}

xarg *Yap_ArgListToVector(Term listl, const param_t *def, int n) {
  CACHE_REGS
  listl = Deref(listl);
    if (IsVarTerm(listl)) {
      Yap_ThrowError(INSTANTIATION_ERROR, listl, "while opening a list of options");
    }    
  xarg *a = calloc(n, sizeof(xarg));
  
  if (IsApplTerm(listl) && FunctorOfTerm(listl) == FunctorModule)
    listl = ArgOfTerm(2, listl);
  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsAtomTerm(listl)) {
      xarg *na = matchKey(AtomOfTerm(listl), a, n, def);
      if (!na) {
        return failed(TYPE_ERROR_LIST, listl, a);
      }
    } else if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm(listl);
      if (IsExtensionFunctor(f)) {
        return failed(TYPE_ERROR_LIST, listl, a);
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        return failed(TYPE_ERROR_LIST, listl, a);
      }
      xarg *na = matchKey(NameOfFunctor(f), a, n, def);
      if (!na) {
        return failed(TYPE_ERROR_LIST, listl, a);
      }
      na->used = true;
      na->tvalue = ArgOfTerm(1, listl);
      return a;
    } else {
      return failed(TYPE_ERROR_LIST, listl, a);
    }
    listl = MkPairTerm(listl, TermNil);
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm(listl);
    listl = TailOfTerm(listl);
    if (IsVarTerm(hd)) {
      return failed(INSTANTIATION_ERROR, hd, a);
    }
    if (IsVarTerm(listl)) {
      return failed(INSTANTIATION_ERROR, listl, a);
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey(AtomOfTerm(hd), a, n, def);
      if (!na)
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, hd, a);
      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        return failed(TYPE_ERROR_PARAMETER, hd, a);
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        return failed(DOMAIN_ERROR_OUT_OF_RANGE, hd, a);
      }
      xarg *na = matchKey(NameOfFunctor(f), a, n, def);
      if (!na) {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, hd, a);
      }
      na->used = true;
      na->tvalue = ArgOfTerm(1, hd);
    } else {
      return failed(TYPE_ERROR_PARAMETER, hd, a);
    }
  }
  if (IsVarTerm(listl)) {
    return failed(INSTANTIATION_ERROR, listl, a);
  } else if (listl != TermNil) {
    return failed(TYPE_ERROR_LIST, listl, a);
  }
  return a;
}

static xarg *matchKey2(Atom key, xarg *e0, int n, const param2_t *def) {
  int i;
  for (i = 0; i < n; i++) {
    if (!strcmp((char *)def->name, (char *)RepAtom(key)->StrOfAE)) {
      return e0;
    }
    def++;
    e0++;
  }
  return NULL;
}

/// Yap_ArgList2ToVector is much the same as before,
/// but assumes parameters also have something called a
/// scope
xarg *Yap_ArgList2ToVector(Term listl, const param2_t *def, int n) {
  CACHE_REGS
    listl = Deref(listl);
    if (IsVarTerm(listl)) {
      Yap_ThrowError(INSTANTIATION_ERROR, listl, "while opening a list of options");
    }    
    xarg *a = calloc(n, sizeof(xarg));
  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsVarTerm(listl)) {
      return failed(INSTANTIATION_ERROR, listl, a);
    }
    if (IsAtomTerm(listl)) {
      xarg *na = matchKey2(AtomOfTerm(listl), a, n, def);
      if (!na) {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, listl, a);
      }
    }
    if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm(listl);
      if (IsExtensionFunctor(f)) {
        return failed(TYPE_ERROR_PARAMETER, listl, a);
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        return failed(TYPE_ERROR_LIST, listl, a);
      }
      xarg *na = matchKey2(NameOfFunctor(f), a, n, def);
      if (!na) {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, listl, a);
      }
    } else {
      return failed(TYPE_ERROR_LIST, listl, a);
    }
    listl = MkPairTerm(listl, TermNil);
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm(listl);
    if (IsVarTerm(hd)) {
      return failed(INSTANTIATION_ERROR, hd, a);
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey2(AtomOfTerm(hd), a, n, def);
      if (!na) {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, hd, a);
      }
      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        return failed(TYPE_ERROR_PARAMETER, hd, a);
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, hd, a);
      }
      xarg *na = matchKey2(NameOfFunctor(f), a, n, def);
      if (na) {
        na->used = 1;
        na->tvalue = ArgOfTerm(1, hd);
      } else {
        return failed(DOMAIN_ERROR_GENERIC_ARGUMENT, hd, a);
      }
    } else {
      return failed(INSTANTIATION_ERROR, hd, a);
    }
    listl = TailOfTerm(listl);
  }
  if (IsVarTerm(listl)) {
    return failed(INSTANTIATION_ERROR, listl, a);
  }
  if (TermNil != listl) {
    return failed(TYPE_ERROR_LIST, listl, a);
  }
  return a;
}
