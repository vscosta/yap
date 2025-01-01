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

static void handle_entry(Term hd,const char *file, const char *function, int lineno,
                              const param_t *def, int n,
                             xarg *a,
                             yap_error_number err)
{  xarg *na;
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
    } else {
        Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
    }
      na->used = true;
      na->source = hd;
}


xarg *Yap_ArgListToVector__(const char *file, const char *function, int lineno,
                            Term listl, const param_t *def, int n,
			    xarg *a,
                            yap_error_number err) {
  CACHE_REGS
  Term *endp = NULL, l;
  listl = Deref(listl);
   if (!a)
    a = calloc(n, sizeof(xarg));
  if (listl==TermNil)
    return a;
   l =  listl;
  if (IsVarTerm(l)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, l,
                     "unbound parameter list");
   }
 Yap_SkipList(&l,&endp);
  if (IsVarTerm(*endp)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "unbound parameter list");
   }
  if (*endp == TermNil) {
    while(l != TermNil) {
      handle_entry( HeadOfTerm(l),file, function,  lineno,
		    def, n,a,
		    err      );
      l = TailOfTerm(l);
    }
    return a;
  } else if (IsPairTerm(listl)){
    Yap_ThrowError__(file, function, lineno,  TYPE_ERROR_LIST, listl,  "bad list");
  } else {
      handle_entry( listl,file, function,  lineno,
		    def, n,a,
		    err      );
   }
  return a;
}

static xarg *matchKey2(Atom key, xarg *e0, int n, const
		       param2_t *def) { 
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


static void handle_entry2(Term hd,const char *file, const char *function, int lineno,
                               const param2_t *def, int n,
                             xarg *a,
                             yap_error_number err)
{  xarg *na;
    if (IsVarTerm(hd)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, hd, "sub-element");
    }
    if (IsAtomTerm(hd)) {
      na = matchKey2(AtomOfTerm(hd),a, n,  def);
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
           na->tvalue = ArgOfTerm(1, hd);
    } else {
        Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
    }
      na->used = true;
      na->source = hd;
}



/// Yap_ArgList2ToVector is much the same as before,
/// but assumes parameters also have something called a
/// scope
xarg *Yap_ArgList2ToVector__(const char *file, const char *function, int lineno,
                            Term listl, const param2_t *def, int n,
			     yap_error_number err) {
  CACHE_REGS
  Term *endp = NULL, l;
  listl = Deref(listl);
xarg *    a = calloc(n, sizeof(xarg));
  if (listl==TermNil)
    return a;
   l =  listl;
  if (IsVarTerm(l)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, l,
                     "unbound parameter list");
   }
 Yap_SkipList(&l,&endp);
  if (IsVarTerm(*endp)) {
      Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, *endp,
                     "unbound parameter list");
   }
  if (*endp == TermNil) {
    while(l != TermNil) {
      handle_entry2( HeadOfTerm(l),file, function,  lineno,
		    def, n,a,
		    err      );
      l = TailOfTerm(l);
    }
    return a;
  } else if (IsPairTerm(listl)) {
    Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, TermNil,NULL );
  } else {
      handle_entry2( listl,file, function,  lineno,
		    def, n,a,
		    err      );
  }
  return a;
}


 /// @}



