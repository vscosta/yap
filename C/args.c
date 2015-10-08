
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

static xarg *
matchKey(Atom key, xarg *e0, int n, const param_t *def)
{
  int i;
  for (i=0; i< n; i++) {
    if (!strcmp((char *)def->name, (char *)RepAtom(key)->StrOfAE)) {
      return e0;
    }
    def++;
    e0++;
  }
  return NULL;
}

xarg *
Yap_ArgListToVector (Term listl, const param_t *def, int n)
{
  CACHE_REGS
    xarg *a = calloc(  n , sizeof(xarg) );
  if (IsApplTerm(listl) && FunctorOfTerm(listl) == FunctorModule)
    listl = ArgOfTerm(2,listl);
  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsVarTerm(listl) ) {
	free( a );
	LOCAL_Error_TYPE = INSTANTIATION_ERROR;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    if (IsAtomTerm(listl) ) {
      xarg *na = matchKey( AtomOfTerm(listl), a, n, def);
      if (!na) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    } else if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm( listl );
      if (IsExtensionFunctor(f)) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;    
      }
      arity_t arity = ArityOfFunctor( f );
      if (arity != 1) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
      xarg *na = matchKey( NameOfFunctor( f ), a, n, def);
      if (!na) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    } else {
      free( a );
      LOCAL_Error_TYPE = TYPE_ERROR_LIST;
      LOCAL_Error_Term = listl;
      return NULL;
    }
    listl = MkPairTerm( listl, TermNil );
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm( listl );
    listl = TailOfTerm( listl );
    if (IsVarTerm(hd) || IsVarTerm(listl))  {
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
      if (IsVarTerm(hd)) {
	LOCAL_Error_Term = hd;
      } else {
	LOCAL_Error_Term = listl;
      }
      free( a );
      return NULL;
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey( AtomOfTerm( hd ), a, n, def);
      if (!na)
	return NULL;

      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm( hd )) {
      Functor f = FunctorOfTerm( hd );
      if (IsExtensionFunctor(f)) {
	LOCAL_Error_TYPE = TYPE_ERROR_PARAMETER;
	LOCAL_Error_Term = hd;
	free( a );
	return NULL;    
      }
      arity_t arity = ArityOfFunctor( f );
      if (arity != 1) {
	LOCAL_Error_TYPE = DOMAIN_ERROR_OUT_OF_RANGE;
	LOCAL_Error_Term = hd;
	free( a );
	return NULL;
      }
      xarg *na = matchKey( NameOfFunctor( f ), a, n, def);
      if (!na)
	return NULL;
      na->used = 1;
      na->tvalue = ArgOfTerm(1, hd);      
    } else {
      LOCAL_Error_TYPE = TYPE_ERROR_PARAMETER;
      free( a );
      return NULL;    
    }
  }
  if (IsVarTerm(listl)) {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    LOCAL_Error_Term = listl;
    free( a );
    return NULL;
  } else if (listl != TermNil) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
    LOCAL_Error_Term = listl;
    free( a );
    return NULL;
  }
  return a;
}              

static xarg *
matchKey2(Atom key, xarg *e0, int n, const param2_t *def)
{
  int i;
  for (i=0; i< n; i++) {
    if (!strcmp((char*)def->name, (char*)RepAtom(key)->StrOfAE)) {
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
xarg *
Yap_ArgList2ToVector (Term listl, const param2_t *def, int n)
{
  CACHE_REGS
    xarg *a = calloc(  n , sizeof(xarg) );
  if (!IsPairTerm(listl) && listl != TermNil) {
    if (IsVarTerm(listl) ) {
	free( a );
	LOCAL_Error_TYPE = INSTANTIATION_ERROR;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    if (IsAtomTerm(listl) ) {
      xarg *na = matchKey2( AtomOfTerm(listl), a, n, def);
      if (!na) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    }
    if (IsApplTerm(listl)) {
      Functor f = FunctorOfTerm( listl );
      if (IsExtensionFunctor(f)) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;    
      }
      arity_t arity = ArityOfFunctor( f );
      if (arity != 1) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
      xarg *na = matchKey2( NameOfFunctor( f ), a, n, def);
      if (!na) {
	free( a );
	LOCAL_Error_TYPE = TYPE_ERROR_LIST;
	LOCAL_Error_Term = listl;
	return NULL;
      }
    } else {
      free( a );
      LOCAL_Error_TYPE = TYPE_ERROR_LIST;
      LOCAL_Error_Term = listl;
      return NULL;
    }
    listl = MkPairTerm( listl, TermNil );
  }
  while (IsPairTerm(listl)) {
    Term hd = HeadOfTerm( listl );
    if (IsVarTerm(hd))  {
      LOCAL_Error_TYPE = INSTANTIATION_ERROR;
      free( a );
      return NULL;
    }
    if (IsAtomTerm(hd)) {
      xarg *na = matchKey2( AtomOfTerm( hd ), a, n, def);
      if (!na) {
	LOCAL_Error_TYPE = DOMAIN_ERROR_OUT_OF_RANGE;
	LOCAL_Error_Term = hd;
	free( a );
	return NULL;
      }
      na->used = true;
      na->tvalue = TermNil;
      continue;
    } else if (IsApplTerm( hd )) {
      Functor f = FunctorOfTerm( hd );
      if (IsExtensionFunctor(f)) {
	LOCAL_Error_TYPE = TYPE_ERROR_PARAMETER;
	LOCAL_Error_Term = hd;
	free( a );
	return NULL;    
      }
      arity_t arity = ArityOfFunctor( f );
      if (arity != 1) {
	LOCAL_Error_TYPE = DOMAIN_ERROR_OUT_OF_RANGE;
	LOCAL_Error_Term = hd;
	free( a );
	return NULL;
      }
      xarg *na = matchKey2( NameOfFunctor( f ), a, n, def);
      if (na) {
	na->used = 1;
	na->tvalue = ArgOfTerm(1, hd);
      }
    } else {
      LOCAL_Error_TYPE = TYPE_ERROR_PARAMETER;
      LOCAL_Error_Term = hd;
      free( a );
      return NULL;    
    }
    listl = TailOfTerm(listl);
  }
  if (IsVarTerm(listl))  {
    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
    free( a );
    return NULL;
  }
  if (TermNil != listl) {
    LOCAL_Error_TYPE = TYPE_ERROR_LIST;
    LOCAL_Error_Term = listl;
    free( a );
    return NULL;
  }
  return a;
}		
