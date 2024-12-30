/**

@file C/args.c

@defgroup PredicateArgsC How to access a list of arguments from C
@ingroup YAPImplementation

@brief Lists of arguments in C
@
@{

  */

#include "Yap.h"
#include "Yatom.h"
#include "amiops.h"

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
			    xarg *a,
                            yap_error_number err) {
  CACHE_REGS
    Term *endp = NULL;
  listl = Deref(listl);
   if (!a)
    a = calloc(n, sizeof(xarg));
  if (listl==TermNil)
    return a;

   if (IsVarTerm(listl)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "unbound parameter list");
   }
   if (IsAtomTerm(listl)) {
      xarg *na = matchKey(AtomOfTerm(listl), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, listl, "match key");
      }
      na->used = true;
      na->source = listl;
      na->tvalue = TermNil;
      return a;
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
         na->source = listl;
   na->used = true;
      na->tvalue = ArgOfTerm(1, listl);
      return a;
   }
     Yap_SkipList(&listl,&endp);
     if (IsVarTerm(*endp)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
    if (TermNil != *endp) {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, *endp,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
   while (IsPairTerm(listl)) {
     xarg *na;
     Term hd = HeadOfTerm(listl);
    listl = TailOfTerm(listl);
    if (IsVarTerm(hd)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "sub-element");
    }
    if (IsAtomTerm(hd)) {
      na = matchKey(AtomOfTerm(hd), a, n, def);
      if (!na)
        Yap_ThrowError__(file, function, lineno, err, hd, "bad match in list");
      na->tvalue = TermNil;
    } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, err, hd,
                         "high arity");
      }
      na = matchKey(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, hd, "no match");
      }
           na->tvalue = ArgOfTerm(1, hd);
    }
      na->used = true;
      na->source = hd;
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
    Term *endp = NULL;
  listl = Deref(listl);
   xarg * a = calloc(n, sizeof(xarg));
  if (listl==TermNil)
    return a;

   if (IsVarTerm(listl)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "unbound parameter list");
   }
  if (IsAtomTerm(listl)) {
    xarg * na = matchKey2(AtomOfTerm(listl), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, listl, "match key2");
      }
      na->used = true;
      na->source = listl;
      na->tvalue = TermNil;
      return a;
    } else if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm(listl);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "bad arity");
      }
      xarg *na = matchKey2(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, listl, "no match");
      }
         na->source = listl;
   na->used = true;
      na->tvalue = ArgOfTerm(1, listl);
      return a;
   }
   Yap_SkipList(&listl,&endp);
  if (IsVarTerm(*endp)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
    if (TermNil != *endp) {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, *endp,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
   while (IsPairTerm(listl)) {
     xarg *na;
     Term hd = HeadOfTerm(listl);
    listl = TailOfTerm(listl);
    if (IsVarTerm(hd)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "sub-element");
    } else if (IsAtomTerm(hd)) {
      na = matchKey2(AtomOfTerm(hd), a, n, def);
      if (!na)
        Yap_ThrowError__(file, function, lineno, err, hd, "bad match in list");
         na->tvalue = TermNil;
 } else if (IsApplTerm(hd)) {
      Functor f = FunctorOfTerm(hd);
      if (IsExtensionFunctor(f)) {
        Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
      }
      arity_t arity = ArityOfFunctor(f);
      if (arity != 1) {
        Yap_ThrowError__(file, function, lineno, err, hd,
                         "high arity");
      }
      na = matchKey2(NameOfFunctor(f), a, n, def);
      if (!na) {
        Yap_ThrowError__(file, function, lineno, err, hd, "no match");
      }
          na->tvalue = ArgOfTerm(1, listl);
}
      na->used = true;
      na->source = hd;
   }
  return a;
  }



/**
@}
*/
