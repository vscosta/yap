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

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#ifndef NULL
#define NULL (void *)0
#endif

static BBProp 
PutBBProp(AtomEntry *ae, SMALLUNSGN mod)		/* get BBentry for at; */
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
      Yap_Error(SYSTEM_ERROR,ARG1,"could not allocate space in bb_put/2");
      return(NULL);
    }
    p->NextOfPE = ae->PropsOfAE;
    ae->PropsOfAE = AbsBBProp(p);
    p->ModuleOfBB = mod;
    p->Element = NULL;
    p->KeyOfBB = AbsAtom(ae);
    p->KindOfPE = BBProperty;
    INIT_RWLOCK(p->BBRWLock);    
  }
  WRITE_UNLOCK(ae->ARWLock);
  return (p);
}

static BBProp 
PutIntBBProp(Int key, SMALLUNSGN mod)	/* get BBentry for at; */
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
      Yap_Error(SYSTEM_ERROR,ARG1,"could not allocate space in bb_put/2");
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
      Yap_Error(SYSTEM_ERROR,ARG1,"could not allocate space in bb_put/2");
      return(NULL);
    }
    p->ModuleOfBB = mod;
    p->Element = NULL;
    p->KeyOfBB = (Atom)key;
    p->KindOfPE = BBProperty;
    p->NextOfPE = INT_BB_KEYS[hash_key];
    INT_BB_KEYS[hash_key] = AbsBBProp(p);
    YAPLeaveCriticalSection();
  }
  return (p);
}

static BBProp 
GetBBProp(AtomEntry *ae, SMALLUNSGN mod)		/* get BBentry for at; */
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
GetIntBBProp(Int key, SMALLUNSGN mod)		/* get BBentry for at; */
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
    Yap_Error(SYSTEM_ERROR,ARG1,"could not allocate space");
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
AddBBProp(Term t1, char *msg, SMALLUNSGN mod)
{
  BBProp p;

 restart:
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, msg);
    return(NULL);
  } if (IsAtomTerm(t1)) {
    p = PutBBProp(RepAtom(AtomOfTerm(t1)), mod);
  } else if (IsIntegerTerm(t1)) {
    p = PutIntBBProp(IntegerOfTerm(t1), mod);
  } else if (IsApplTerm(t1) && FunctorOfTerm(t1) == FunctorModule) {
    Term tmod = ArgOfTerm(1, t1);
    if (!IsVarTerm(tmod) ) {
      t1 = ArgOfTerm(2, t1);
      mod = Yap_LookupModule(tmod);
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
FetchBBProp(Term t1, char *msg, SMALLUNSGN mod)
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
      mod = Yap_LookupModule(tmod);
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

static Int
p_bb_put(void)
{
  Term t1 = Deref(ARG1);
  BBProp p = AddBBProp(t1, "bb_put/2", CurrentModule);
  if (p == NULL)
    return(FALSE);
  WRITE_LOCK(p->BBRWLock);    
  if (p->Element != NULL) {
    Yap_ReleaseTermFromDB(p->Element);
  }
  p->Element = Yap_StoreTermInDB(Deref(ARG2),2);
  WRITE_UNLOCK(p->BBRWLock);
  return(p->Element != NULL);
}

static Int
p_bb_get(void)
{
  Term t1 = Deref(ARG1);
  BBProp p = FetchBBProp(t1, "bb_get/2", CurrentModule);
  Term out;
  if (p == NULL || p->Element == NULL)
    return(FALSE);
  READ_LOCK(p->BBRWLock);  
  out = Yap_FetchTermFromDB(p->Element,3);
  READ_UNLOCK(p->BBRWLock);
  return(Yap_unify(ARG2,out));
}

static Int
p_bb_delete(void)
{
  Term t1 = Deref(ARG1);
  BBProp p;
  Term out;

  p = FetchBBProp(t1, "bb_delete/2", CurrentModule);
  if (p == NULL || p->Element == NULL)
    return(FALSE);
  out = Yap_FetchTermFromDB(p->Element,3);
  WRITE_LOCK(p->BBRWLock);  
  Yap_ReleaseTermFromDB(p->Element);
  p->Element = NULL;
  WRITE_UNLOCK(p->BBRWLock);  
  return(Yap_unify(ARG2,out));
}

static Int
p_bb_update(void)
{
  Term t1 = Deref(ARG1);
  BBProp p;
  Term out;

  p = FetchBBProp(t1, "bb_update/3", CurrentModule);
  if (p == NULL || p->Element == NULL)
    return(FALSE);
  WRITE_LOCK(p->BBRWLock);  
  out = Yap_FetchTermFromDB(p->Element,3);
  if (!Yap_unify(ARG2,out)) {
    WRITE_UNLOCK(p->BBRWLock);  
    return(FALSE);
  }

  Yap_ReleaseTermFromDB(p->Element);
  p->Element = Yap_StoreTermInDB(Deref(ARG3),3);
  WRITE_UNLOCK(p->BBRWLock);
  return(p->Element != NULL);
}

static Int
p_resize_bb_int_keys(void)
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

