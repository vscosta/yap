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

/**
 * Returns the index of an argument key, or -1 if not found.
 *
 */
int Yap_ArgKey(Atom key, const param_t def[], int n) {
  int i;
  for (i = 0; i <n ; i++) {
    if (!def[i].name)
      return -1;
    if (!strcmp((char *)(def[i].name), (char *)RepAtom(key)->StrOfAE)) {
      return i;
    }
  }
  return -1;
}

#define YAP_XARGINFO(Error, Message)


xarg *Yap_ArgListToVector__(const char *file, const char *function, int lineno,
                            Term listl, const param_t def[], int n,
			    xarg *a,
                            yap_error_number err) {
  CACHE_REGS
    listl = Deref(listl);
  if (IsVarTerm(listl)) {
    Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl,
                     "while opening    listl = ArgOfTerm(2, listl ,k)");
  }
   if (!a)
    a = calloc(n, sizeof(xarg));

     if (IsAtomTerm(listl))
       {
	 if (listl !=  TermNil) {
	   listl = MkPairTerm(listl,TermNil);
	 }
       }
     else if  (IsApplTerm(listl)) {
       Term hd = listl;
	 Functor f = FunctorOfTerm(hd);
	 if (IsExtensionFunctor(f)) {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
       return NULL;
 	 }
	 arity_t arity = ArityOfFunctor(f);
	 if (arity != 1) {
      Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
       return NULL;
     	 }
	Atom at =	 NameOfFunctor(f);    
         int pos =Yap_ArgKey(at, def, n);
     if (pos < 0) {
       Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
       return NULL;
     }
       if (!a[pos].used) {
	 if (IsApplTerm(hd)) {
	   a[pos].tvalue = ArgOfTerm(1, hd);
	 } else {
	   a[pos].tvalue = TermNil;
	 };
	 a[pos].used = true;
	 a[pos].source = hd;
       }
       return a;
     }
     else if (!IsPairTerm(listl)) {
       Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
       return NULL;
    }
     Term *tailp;
     Yap_SkipList(&listl, &tailp);
     if (IsVarTerm(*tailp))  {
       Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl,
			"tyring to open    listl = [...|_]");
       return NULL;
     }
     if (*tailp != TermNil) {
       Yap_ThrowError__(file, function, lineno, TYPE_ERROR_LIST, listl, "callable");
       return NULL;
    }
     if (!a) {
       a = calloc(n, sizeof(xarg));
     }
     
     while (listl!=TermNil) {
       Term  hd = HeadOfTerm(listl);
       Atom at;
      if (IsVarTerm(hd))  {
	 Yap_ThrowError__(file, function, lineno, INSTANTIATION_ERROR, listl,
			  "argument unbound");
	 return NULL;
       } else if (IsApplTerm(hd)) {
	
	 Functor f = FunctorOfTerm(hd);
	 if (IsExtensionFunctor(f)) {
           Yap_ThrowError__(file, function, lineno, err, hd, "bad compound");
	   return NULL;
	 }
	 arity_t arity = ArityOfFunctor(f);
	 if (arity != 1) {
           Yap_ThrowError__(file, function, lineno, err, hd,
                            "high arity");
	 }
	 at =	 NameOfFunctor(f);    
       } else if (IsAtomTerm(hd)) {
	 at = AtomOfTerm(hd);
       } else {
          Yap_ThrowError__(file, function, lineno, err, hd,                                            "high arity");                                 
	 return NULL;
       }
      int pos =Yap_ArgKey(at, def, n);
      if (pos < 0) {
           Yap_ThrowError__(file, function, lineno, err, hd,                                            "high arity");                                 
      }
       if (!a[pos].used) {
	 if (IsApplTerm(hd)) {
	   a[pos].tvalue = ArgOfTerm(1, hd);
	 } else {
	   a[pos].tvalue = TermNil;
	 };
	 a[pos].used = true;
	 a[pos].source = hd;
       }
        listl = TailOfTerm(listl);
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
  // step 1
  xarg *a=calloc(sizeof(xarg),n);
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
        Yap_ThrowError__(file, function, lineno, err,
                         listl, NULL);
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
        Yap_ThrowError__(file, function, lineno, err,
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
        Yap_ThrowError__(file, function, lineno, err,
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

/**
@}
*/
