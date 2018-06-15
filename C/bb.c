/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		bb.c							 *
* Last rev:	12/29/99						 *
* mods:									 *
* comments:	YAP's blackboard routines				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/**
 * @file   bb.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 09:32:54 2018
 * 
 * @brief  blackboard
 * 
 * @namespace prolog
 * 
 */


/** @defgroup BlackBoard The Blackboard
@ingroup builtins
@{

YAP implements a blackboard in the style of the SICStus Prolog
blackboard. The blackboard uses the same underlying mechanism as the
internal data-base but has several important differences:

+ It is module aware, in contrast to the internal data-base.
+ Keys can only be atoms or integers, and not compound terms.
+ A single term can be stored per key.
+ An atomic update operation is provided; this is useful for
parallelism.



 
*/

#include "Yap.h"
#include "clause.h"
#ifndef NULL
#define NULL (void *)0
#endif

static BBProp 
PutBBProp(AtomEntry *ae, Term mod USES_REGS)		/* get BBentry for at; */
{
  Prop          p0;
  BBProp        p;

  WRITE_LOCK(ae->ARWLock);
  p = RepBBProp(p0 = ae->PropsOfAE);
  while (p0 != NIL && (!IsBBProperty(p->KindOfPE) ||
		(p->ModuleOfBB != mod))) {
    p = RepBBProp(p0 = p->NextOfPE);
  }
  if (p0 == NIL) {
    p = (BBProp)Yap_AllocAtomSpace(sizeof(*p));
    if (p == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      Yap_Error(RESOURCE_ERROR_HEAP,ARG1,"could not allocate space in bb_put/2");
      return(NULL);
    }
    AddPropToAtom(ae, (PropEntry *)p);
    p->ModuleOfBB = mod;
    p->Element = 0L;
    p->KeyOfBB = AbsAtom(ae);
    p->KindOfPE = BBProperty;
    INIT_RWLOCK(p->BBRWLock);    
  }
  WRITE_UNLOCK(ae->ARWLock);
  return (p);
}

static BBProp 
PutIntBBProp(Int key, Term mod USES_REGS)	/* get BBentry for at; */
{
  Prop          p0;
  BBProp        p;
  UInt hash_key;

  if (INT_BB_KEYS == NULL) {
    INT_BB_KEYS = (Prop *)Yap_AllocCodeSpace(sizeof(Prop)*INT_BB_KEYS_SIZE);
    if (INT_BB_KEYS != NULL) {
      UInt i = 0;
      Prop *pp = INT_BB_KEYS;
      for (i = 0; i < INT_BB_KEYS_SIZE; i++) {
	pp[0] = NIL;
	pp++;
      }
    } else {
      Yap_Error(RESOURCE_ERROR_HEAP,ARG1,"could not allocate space in bb_put/2");
      return(NULL);
    }
  }
  hash_key = (CELL)key % INT_BB_KEYS_SIZE;
  p0 = INT_BB_KEYS[hash_key];
  p = RepBBProp(p0);
  while (p0 != NIL && (!IsBBProperty(p->KindOfPE) ||
		       key != (Int)(p->KeyOfBB) ||
		(p->ModuleOfBB != mod))) {
    p = RepBBProp(p0 = p->NextOfPE);
  }
  if (p0 == NIL) {
    YAPEnterCriticalSection();
    p = (BBProp)Yap_AllocAtomSpace(sizeof(*p));
    if (p == NULL) {
      YAPLeaveCriticalSection();
      Yap_Error(RESOURCE_ERROR_HEAP,ARG1,"could not allocate space in bb_put/2");
      return(NULL);
    }
    p->ModuleOfBB = mod;
    p->Element = 0L;
    p->KeyOfBB = (Atom)key;
    p->KindOfPE = BBProperty;
    p->NextOfPE = INT_BB_KEYS[hash_key];
    INT_BB_KEYS[hash_key] = AbsBBProp(p);
    YAPLeaveCriticalSection();
  }
  return (p);
}

static BBProp 
GetBBProp(AtomEntry *ae, Term mod)		/* get BBentry for at; */
{
  Prop          p0;
  BBProp        p;

  READ_LOCK(ae->ARWLock);
  p = RepBBProp(p0 = ae->PropsOfAE);
  while (p0 != NIL && (!IsBBProperty(p->KindOfPE) ||
		(p->ModuleOfBB != mod))) {
    p = RepBBProp(p0 = p->NextOfPE);
  }
  READ_UNLOCK(ae->ARWLock);
  if (p0 == NIL) {
    return(NULL);
  }
  return (p);
}

static BBProp 
GetIntBBProp(Int key, Term mod)		/* get BBentry for at; */
{
  Prop          p0;
  BBProp        p;
  UInt hash_key;

  if (INT_BB_KEYS == NULL)
    return(NULL);
  hash_key = (CELL)key % INT_BB_KEYS_SIZE;
  p0 = INT_BB_KEYS[hash_key];
  p = RepBBProp(p0);
  while (p0 != NIL && (!IsBBProperty(p->KindOfPE) ||
		       key != (Int)(p->KeyOfBB) ||
		(p->ModuleOfBB != mod))) {
    p = RepBBProp(p0 = p->NextOfPE);
  }
  if (p0 == NIL) {
    return(NULL);
  }
  return (p);
}

static int
resize_bb_int_keys(UInt new_size) {
  CACHE_REGS
  Prop *new;
  UInt i;

  YAPEnterCriticalSection();
  if (INT_BB_KEYS == NULL) {
    INT_BB_KEYS_SIZE = new_size;
    YAPLeaveCriticalSection();
    return(TRUE);
  }
  new = (Prop *)Yap_AllocCodeSpace(sizeof(Prop)*new_size);
  if (new == NULL) {
    YAPLeaveCriticalSection();
    Yap_Error(RESOURCE_ERROR_HEAP,ARG1,"could not allocate space");
    return(FALSE);
  }
  for (i = 0; i < new_size; i++) {
    new[i] = NIL;
  }
  for (i = 0; i < INT_BB_KEYS_SIZE; i++) {
    if (INT_BB_KEYS[i] != NIL) {
      Prop p0 = INT_BB_KEYS[i];
      while (p0 != NIL) {
	BBProp p = RepBBProp(p0);
	CELL key = (CELL)(p->KeyOfBB);
	UInt hash_key = (CELL)key % new_size;
	p0 = p->NextOfPE;
	p->NextOfPE = new[hash_key];
	new[hash_key] = AbsBBProp(p);
      }
    }
  }
  Yap_FreeCodeSpace((char *)INT_BB_KEYS);
  INT_BB_KEYS = new;
  INT_BB_KEYS_SIZE = new_size;
  YAPLeaveCriticalSection();
  return(TRUE);
}

static BBProp
AddBBProp(Term t1, char *msg, Term mod USES_REGS)
{
  BBProp p;

 restart:
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, msg);
    return(NULL);
  } if (IsAtomTerm(t1)) {
    p = PutBBProp(RepAtom(AtomOfTerm(t1)), mod PASS_REGS);
  } else if (IsIntegerTerm(t1)) {
    p = PutIntBBProp(IntegerOfTerm(t1), mod PASS_REGS);
  } else if (IsApplTerm(t1) && FunctorOfTerm(t1) == FunctorModule) {
    Term tmod = ArgOfTerm(1, t1);
    if (!IsVarTerm(tmod) ) {
      t1 = ArgOfTerm(2, t1);
      mod = tmod;
      goto restart;
    } else {
      Yap_Error(INSTANTIATION_ERROR, t1, msg);
      return(NULL);
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t1, msg);
    return(NULL);
  }
  return(p);
}

static BBProp
FetchBBProp(Term t1, char *msg, Term mod)
{
  BBProp p;

 restart:
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, msg);
    return(NULL);
  } if (IsAtomTerm(t1)) {
    p = GetBBProp(RepAtom(AtomOfTerm(t1)), mod);
  } else if (IsIntegerTerm(t1)) {
    p = GetIntBBProp(IntegerOfTerm(t1), mod);
  } else if (IsApplTerm(t1) && FunctorOfTerm(t1) == FunctorModule) {
    Term tmod = ArgOfTerm(1, t1);
    if (!IsVarTerm(tmod) ) {
      mod = tmod;
      t1 = ArgOfTerm(2, t1);
      goto restart;
    } else {
      Yap_Error(INSTANTIATION_ERROR, t1, msg);
      return(NULL);
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t1, msg);
    return(NULL);
  }
  return(p);
}

static Term 
BBPut(Term t0, Term t2)
{
  if (!IsVarTerm(t0) && IsApplTerm(t0)) {
    Yap_ErLogUpdCl((LogUpdClause *)DBRefOfTerm(t0));
  }
  if (IsVarTerm(t2) || IsAtomOrIntTerm(t2)) {
    return t2;
  } else {
    LogUpdClause *cl = Yap_new_ludbe(t2, NULL, 0);

    if (cl == NULL) {
      return 0L;
    }
    return MkDBRefTerm((DBRef)cl);
  }
}

/** @pred  bb_put(+ _Key_,? _Term_) 


Store term table  _Term_ in the blackboard under key  _Key_. If a
previous term was stored under key  _Key_ it is simply forgotten.

 
*/
static Int
p_bb_put( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  BBProp p = AddBBProp(t1, "bb_put/2", CurrentModule PASS_REGS);

  if (p == NULL) {
    return(FALSE);
  }
  WRITE_LOCK(p->BBRWLock);    
  /*
    if (p->Element)
    fprintf(stderr,"putting %p, size %d\n", p, p->Element->NOfCells);
  */
  p->Element = BBPut(p->Element, Deref(ARG2));
  WRITE_UNLOCK(p->BBRWLock);
  return (p->Element != 0L);
}

static Term
BBGet(Term t, UInt arity USES_REGS)
{
  if (IsVarTerm(t)) {
    return MkVarTerm();
  } else if (IsAtomOrIntTerm(t)) {
    return t;
  } else {
    return Yap_LUInstance((LogUpdClause *)DBRefOfTerm(t), arity);
  }
}

/** @pred  bb_get(+ _Key_,? _Term_) 


Unify  _Term_ with a term stored in the blackboard under key
 _Key_, or fail silently if no such term exists.

 
*/
static Int
p_bb_get( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  BBProp p = FetchBBProp(t1, "bb_get/2", CurrentModule);
  Term out, t0;
  if (p == NULL || p->Element == 0L)
    return(FALSE);
  READ_LOCK(p->BBRWLock);  
  /*
    if (p->Element)
      fprintf(stderr,"getting %p, size %d\n", p, p->Element->NOfCells);
  */
  t0 = p->Element;
  READ_UNLOCK(p->BBRWLock);  
  out = BBGet(t0, 2 PASS_REGS);
  return Yap_unify(ARG2,out);
}

/** @pred  bb_delete(+ _Key_,? _Term_) 


Delete any term stored in the blackboard under key  _Key_ and unify
it with  _Term_. Fail silently if no such term exists.

 
*/
static Int
p_bb_delete( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  BBProp p;
  Term out;

  p = FetchBBProp(t1, "bb_delete/2", CurrentModule);
  if (p == NULL || p->Element == 0L)
    return(FALSE);
  WRITE_LOCK(p->BBRWLock);  
  out = BBGet(p->Element, 2 PASS_REGS);
  if (!IsVarTerm(p->Element) && IsApplTerm(p->Element)) {
    Yap_ErLogUpdCl((LogUpdClause *)DBRefOfTerm(p->Element));
  }
  p->Element = 0L;
  WRITE_UNLOCK(p->BBRWLock);  
  return Yap_unify(ARG2,out);
}

/** @pred  bb_update( +_Key_, ?_Term_, ?_New_) 


Atomically  unify a term stored in the blackboard under key  _Key_
with  _Term_, and if the unification succeeds replace it by
 _New_. Fail silently if no such term exists or if unification fails.

 */
static Int
p_bb_update( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  BBProp p;
  Term out;

  p = FetchBBProp(t1, "bb_update/3", CurrentModule);
  if (p == NULL || p->Element == 0L)
    return FALSE;
  WRITE_LOCK(p->BBRWLock);  
  out = BBGet(p->Element, 3 PASS_REGS);
  if (!Yap_unify(out,ARG2)) {
    WRITE_UNLOCK(p->BBRWLock);
    return FALSE;
  }
  p->Element = BBPut(p->Element, Deref(ARG3));
  WRITE_UNLOCK(p->BBRWLock);
  return (p->Element != 0L);
}

static Int
p_resize_bb_int_keys( USES_REGS1 )
{
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    return(Yap_unify(ARG1,MkIntegerTerm((Int)INT_BB_KEYS_SIZE)));
  }
  if (!IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "yap_flag(resize_bb_int_keys,T)");
    return(FALSE);
  }
  return(resize_bb_int_keys(IntegerOfTerm(t1)));
}

void 
Yap_InitBBPreds(void)
{
  Yap_InitCPred("bb_put", 2, p_bb_put, 0);
  Yap_InitCPred("bb_get", 2, p_bb_get, 0);
  Yap_InitCPred("bb_delete", 2, p_bb_delete, 0);
  Yap_InitCPred("bb_update", 3, p_bb_update, 0);
  Yap_InitCPred("$resize_bb_int_keys", 1, p_resize_bb_int_keys, SafePredFlag|SyncPredFlag);
}

/**
 @}
*/
