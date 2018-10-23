
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


xarg *Yap_ArgListToVector__(const char *file, const char *function, int lineno,
                            Term listl, const param_t *def, int n,
                            yap_error_number err) {
  CACHE_REGS
  xarg *a;
      listl = Deref(listl);
  if (IsVarTerm(listl)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
  a = calloc(n, sizeof(xarg));

  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsAtomTerm(listl)) {
      xarg *na = matchKey(AtomOfTerm(listl), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "match key");
      }
    } else if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm(listl);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "bad arity");
      }
      xarg *na = matchKey(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, listl, "no match");
      }
      na->used = true;
      na->tvalue = ArgOfTerm(1, listl);
    } else {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_ATOM, listl, "not atom");
    }
    listl = MkPairTerm(listl, TermNil);
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm(listl);
    listl = TailOfTerm(listl);
    if (IsVarTerm(hd)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "sub-element");
    }
    if (IsVarTerm(listl)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl, "sub-list");
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey(AtomOfTerm(hd), a, n, def);
      if (!na)
        Yap_ThrowError__(file, function, lineno, err, hd, "bad match in list");
      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, DOMAIN_ERROR_OUT_OF_RANGE, hd,
                         "high arity");
      }
      xarg *na = matchKey(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, hd, "no match");
      }
      na->used = true;
      na->tvalue = ArgOfTerm(1, hd);
    } else {
      Yap_ThrowError__(file, function, lineno, err, hd, "bad type");
    }
  }
  if (IsVarTerm(listl)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl, "unbound");
  } else if (listl != TermNil) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "bad list");
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
xarg *Yap_ArgList2ToVector__(const char *file, const char *function, int lineno,Term listl, const param2_t *def, int n, yap_error_number err) {
  CACHE_REGS
  xarg *a = calloc(n, sizeof(xarg));
  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsVarTerm(listl)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl, "unbound");
    }
    if (IsAtomTerm(listl)) {
      xarg *na = matchKey2(AtomOfTerm(listl), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err,
                         listl, "bad match");
      }
    }
    if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm(listl);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_PARAMETER, listl,
                         "bad compound");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "bad arity");
      }
      xarg *na = matchKey2(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, DOMAIN_ERROR_GENERIC_ARGUMENT,
                         listl, "bad match");
      }
    } else {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "");
    }
    listl = MkPairTerm(listl, TermNil);
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm(listl);
    if (IsVarTerm(hd)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "");
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey2(AtomOfTerm(hd), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, DOMAIN_ERROR_GENERIC_ARGUMENT,
                         hd, "bad match");
      }
      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_PARAMETER, hd, "bad compound");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, DOMAIN_ERROR_GENERIC_ARGUMENT,
                         hd, "bad arity");
      }
      xarg *na = matchKey2(NameOfFunctor(f), a, n, def);
      if (na) {
        na->used = 1;
        na->tvalue = ArgOfTerm(1, hd);
      } else {
        Yap_ThrowError__(file, function, lineno, err,
                         hd, "bad key");
      }
      return a;
    } else {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "unbound");
    }
    listl = TailOfTerm(listl);
  }
  if (IsVarTerm(listl)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl, "");
  }
  if (TermNil != listl) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "");
  }
  return a;
}
