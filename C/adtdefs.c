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
* File:		adtdefs.c						 *
* Last rev:								 *
* mods:									 *
* comments:	abstract machine definitions				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif

#define ADTDEFS_C

#ifdef __SUNPRO_CC
#define inline
#endif

#include "Yap.h"
Prop	STD_PROTO(PredPropByFunc,(Functor, SMALLUNSGN));
Prop	STD_PROTO(PredPropByAtom,(Atom, SMALLUNSGN));
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif

/* this routine must be run at least having a read lock on ae */
static Prop
GetFunctorProp(AtomEntry *ae, unsigned int arity)
{				/* look property list of atom a for kind  */
  FunctorEntry *pp;

  pp = RepFunctorProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) &&
	 (!IsFunctorProperty(pp->KindOfPE) ||
	  pp->ArityOfFE != arity))
    pp = RepFunctorProp(pp->NextOfPE);
  return (AbsFunctorProp(pp));
}

/* vsc: We must guarantee that IsVarTerm(functor) returns true! */
static inline Functor
InlinedUnlockedMkFunctor(AtomEntry *ae, unsigned int arity)
{
  FunctorEntry *p;
  Prop p0;

  p0 = GetFunctorProp(ae, arity);
  if (p0 != NIL) {
    return ((Functor) RepProp(p0));
  }
  p = (FunctorEntry *) _YAP_AllocAtomSpace(sizeof(*p));
  p->KindOfPE = FunctorProperty;
  p->NameOfFE = AbsAtom(ae);
  p->ArityOfFE = arity;
  p->PropsOfFE = NIL;
  p->NextOfPE = ae->PropsOfAE;
  INIT_RWLOCK(p->FRWLock);
  ae->PropsOfAE = AbsProp((PropEntry *) p);
  return ((Functor) p);
}

Functor
_YAP_UnlockedMkFunctor(AtomEntry *ae, unsigned int arity)
{
  return(InlinedUnlockedMkFunctor(ae, arity));
}

/* vsc: We must guarantee that IsVarTerm(functor) returns true! */
Functor
_YAP_MkFunctor(Atom ap, unsigned int arity)
{
  AtomEntry *ae = RepAtom(ap);
  Functor f;

  WRITE_LOCK(ae->ARWLock);
  f = InlinedUnlockedMkFunctor(ae, arity);
  WRITE_UNLOCK(ae->ARWLock);
  return (f);
}

/* vsc: We must guarantee that IsVarTerm(functor) returns true! */
void
_YAP_MkFunctorWithAddress(Atom ap, unsigned int arity, FunctorEntry *p)
{
  AtomEntry *ae = RepAtom(ap);
  
  WRITE_LOCK(ae->ARWLock);
  p->KindOfPE = FunctorProperty;
  p->NameOfFE = ap;
  p->ArityOfFE = arity;
  p->NextOfPE = RepAtom(ap)->PropsOfAE;
  ae->PropsOfAE = AbsProp((PropEntry *) p);
  WRITE_UNLOCK(ae->ARWLock);
}

inline static Atom
SearchInInvisible(char *atom)
{
  AtomEntry *chain;

  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr(chain) && strcmp(chain->StrOfAE, atom) != 0) {
    chain = RepAtom(chain->NextOfAE);
  }
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr(chain))
    return (NIL);
  else
    return(AbsAtom(chain));
}

static inline Atom
SearchAtom(unsigned char *p, Atom a) {
  AtomEntry *ae;

  /* search atom in chain */
  while (a != NIL) {
    ae = RepAtom(a);
    if (strcmp(ae->StrOfAE, (const char *)p) == 0) {
      return(a);
    }
    a = ae->NextOfAE;
  }
  return(NIL);
}

static Atom
LookupAtom(char *atom)
{				/* lookup atom in atom table            */
  register CELL hash;
  register unsigned char *p;
  Atom a;
  AtomEntry *ae;

  /* compute hash */
  p = (unsigned char *)atom;
  HashFunction(p, hash);
  if (!strcmp("in line 284, system predicate see/1 at prolog:$$compile_stat/5 (clause 1)",atom))
    fprintf(stderr,"vsc: has is %d\n", hash);
  WRITE_LOCK(HashChain[hash].AERWLock);
  a = HashChain[hash].Entry;
  /* search atom in chain */
  a = SearchAtom((unsigned char *)atom, a);
  if (a != NIL) {
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return(a);
  }
  /* add new atom to start of chain */
  ae = (AtomEntry *) _YAP_AllocAtomSpace((sizeof *ae) + strlen(atom));
  a = AbsAtom(ae);
  ae->NextOfAE = HashChain[hash].Entry;
  HashChain[hash].Entry = a;
  ae->PropsOfAE = NIL;
  if (ae->StrOfAE != atom)
    strcpy(ae->StrOfAE, atom);
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
  return (a);
}

Atom
_YAP_LookupAtom(char *atom)
{				/* lookup atom in atom table            */
  return(LookupAtom(atom));
}

Atom
_YAP_FullLookupAtom(char *atom)
{				/* lookup atom in atom table            */
  Atom t;

  if ((t = SearchInInvisible(atom)) != NIL) {
    return (t);
  }
  return(LookupAtom(atom));
}

void
_YAP_LookupAtomWithAddress(char *atom, AtomEntry *ae)
{				/* lookup atom in atom table            */
  register CELL hash;
  register unsigned char *p;
  Atom a;

  /* compute hash */
  p = (unsigned char *)atom;
  HashFunction(p, hash);
  /* ask for a WRITE lock because it is highly unlikely we shall find anything */
  WRITE_LOCK(HashChain[hash].AERWLock);
  a = HashChain[hash].Entry;
  /* search atom in chain */
  if (SearchAtom(p, a) != NIL) {
    _YAP_Error(FATAL_ERROR,TermNil,"repeated initialisation for atom %s", ae);
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return;
  }
  /* add new atom to start of chain */
  ae->NextOfAE = a;
  HashChain[hash].Entry = AbsAtom(ae);
  ae->PropsOfAE = NIL;
  strcpy(ae->StrOfAE, atom);
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
}

void
_YAP_ReleaseAtom(Atom atom)
{				/* Releases an atom from the hash chain */
  register Int hash;
  register unsigned char *p;
  AtomEntry *inChain;
  AtomEntry *ap = RepAtom(atom);
  char *name = ap->StrOfAE;

  /* compute hash */
  p = (unsigned char *)name;
  HashFunction(p, hash);
  WRITE_LOCK(HashChain[hash].AERWLock);
  if (HashChain[hash].Entry == atom) {
    HashChain[hash].Entry = ap->NextOfAE;
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return;
  }
  /* else */
  inChain = RepAtom(HashChain[hash].Entry);
  while (inChain->NextOfAE != atom)
    inChain = RepAtom(inChain->NextOfAE);
  WRITE_LOCK(inChain->ARWLock);
  inChain->NextOfAE = ap->NextOfAE;
  WRITE_UNLOCK(inChain->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
}

static Prop
GetAPropHavingLock(AtomEntry *ae, PropFlags kind)
{				/* look property list of atom a for kind  */
  PropEntry *pp;

  pp = RepProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) && pp->KindOfPE != kind)
    pp = RepProp(pp->NextOfPE);
  return (AbsProp(pp));
}

Prop
_YAP_GetAPropHavingLock(AtomEntry *ae, PropFlags kind)
{				/* look property list of atom a for kind  */
  return (GetAPropHavingLock(ae,kind));
}

static Prop
GetAProp(Atom a, PropFlags kind)
{				/* look property list of atom a for kind  */
  AtomEntry *ae = RepAtom(a);
  Prop out;

  READ_LOCK(ae->ARWLock);
  out = GetAPropHavingLock(ae, kind);
  READ_UNLOCK(ae->ARWLock);
  return (out);
}

Prop
_YAP_GetAProp(Atom a, PropFlags kind)
{				/* look property list of atom a for kind  */
  return GetAProp(a,kind);
}

inline static Prop
GetPredPropByAtomHavingLock(AtomEntry* ae, SMALLUNSGN cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  p0 = ae->PropsOfAE;
  while (p0) {
    PredEntry *pe = RepPredProp(p0);
    if ( pe->KindOfPE == PEProp && 
	 (pe->ModuleOfPred == cur_mod || !pe->ModuleOfPred)) {
      return(p0);
    }
    p0 = pe->NextOfPE;
  }
  return(NIL);
}

Prop
_YAP_GetPredPropByAtom(Atom at, SMALLUNSGN cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = GetPredPropByAtomHavingLock(ae, cur_mod);
  READ_UNLOCK(ae->ARWLock);
  return(p0);
}


static inline Prop
GetPredPropByFuncHavingLock(Functor f, SMALLUNSGN cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  FunctorEntry *fe = (FunctorEntry *)f;

  p0 = fe->PropsOfFE;
  while (p0) {
    PredEntry *p = RepPredProp(p0);
    if (/* p->KindOfPE != 0 || only props */
	(p->ModuleOfPred == cur_mod || !(p->ModuleOfPred))) {
      return (p0);
    }
    p0 = p->NextOfPE;
  }
  return(NIL);
}

Prop
_YAP_GetPredPropByFunc(Functor f, SMALLUNSGN cur_mod)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;

  READ_LOCK(f->FRWLock);
  p0 = GetPredPropByFuncHavingLock(f, cur_mod);
  READ_UNLOCK(f->FRWLock);
  return (p0);
}

Prop
_YAP_GetPredPropHavingLock(Atom ap, unsigned int arity, SMALLUNSGN mod)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;
  AtomEntry *ae = RepAtom(ap);
  Functor f;

  if (arity == 0) {
    GetPredPropByAtomHavingLock(ae, mod);
  }
  f = InlinedUnlockedMkFunctor(ae, arity);
  READ_LOCK(f->FRWLock);
  p0 = GetPredPropByFuncHavingLock(f, mod);
  READ_UNLOCK(f->FRWLock);
  return (p0);
}

/* get expression entry for at/arity;               */
Prop
_YAP_GetExpProp(Atom at, unsigned int arity)
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  ExpEntry *p;

  READ_LOCK(ae->ARWLock);
  p = RepExpProp(p0 = ae->PropsOfAE);
  while (p0 && (p->KindOfPE != ExpProperty || p->ArityOfEE != arity))
    p = RepExpProp(p0 = p->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  return (p0);
}

/* get expression entry for at/arity, at is already locked;         */
Prop
_YAP_GetExpPropHavingLock(AtomEntry *ae, unsigned int arity)
{
  Prop p0;
  ExpEntry *p;

  p = RepExpProp(p0 = ae->PropsOfAE);
  while (p0 && (p->KindOfPE != ExpProperty || p->ArityOfEE != arity))
    p = RepExpProp(p0 = p->NextOfPE);
  return (p0);
}

Prop
_YAP_NewPredPropByFunctor(FunctorEntry *fe, SMALLUNSGN cur_mod)
{
  Prop p0;
  PredEntry *p = (PredEntry *) _YAP_AllocAtomSpace(sizeof(*p));

  /*  printf("entering %s:%s/%d\n", RepAtom(AtomOfTerm(ModuleName[cur_mod]))->StrOfAE, RepAtom(fe->NameOfFE)->StrOfAE, fe->ArityOfFE); */

  INIT_RWLOCK(p->PRWLock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = fe->ArityOfFE;
  p->FirstClause = p->LastClause = NIL;
  p->PredFlags = 0L;
  p->StateOfPred = 0;
  p->OwnerFile = AtomNil;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->TrueCodeOfPred = p->CodeOfPred = (CODEADDR)(&(p->OpcodeOfPred)); 
  p->ModuleOfPred = cur_mod;
  p->NextPredOfModule = ModulePred[cur_mod];
  ModulePred[cur_mod] = p;
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = fe->PropsOfFE;
  fe->PropsOfFE = p0 = AbsPredProp(p);
  p->FunctorOfPred = (Functor)fe;
  WRITE_UNLOCK(fe->FRWLock);
  return (p0);
}

Prop
_YAP_NewPredPropByAtom(AtomEntry *ae, SMALLUNSGN cur_mod)
{
  Prop p0;
  PredEntry *p = (PredEntry *) _YAP_AllocAtomSpace(sizeof(*p));

/* Printf("entering %s:%s/0\n", RepAtom(AtomOfTerm(ModuleName[cur_mod]))->StrOfAE, ae->StrOfAE); */

  INIT_RWLOCK(p->PRWLock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = 0;
  p->FirstClause = p->LastClause = NIL;
  p->PredFlags = 0L;
  p->StateOfPred = 0;
  p->OwnerFile = AtomNil;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->TrueCodeOfPred = p->CodeOfPred = (CODEADDR)(&(p->OpcodeOfPred)); 
  p->ModuleOfPred = cur_mod;
  p->NextPredOfModule = ModulePred[cur_mod];
  ModulePred[cur_mod] = p;
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = ae->PropsOfAE;
  ae->PropsOfAE = p0 = AbsPredProp(p);
  p->FunctorOfPred = (Functor)AbsAtom(ae);
  WRITE_UNLOCK(ae->ARWLock);
  return (p0);
}

Term
_YAP_GetValue(Atom a)
{
  Prop p0 = GetAProp(a, ValProperty);
  Term out;

  if (p0 == NIL)
    return (TermNil);
  READ_LOCK(RepValProp(p0)->VRWLock);
  out = RepValProp(p0)->ValueOfVE;
  if (IsApplTerm(out)) {
    Functor f = FunctorOfTerm(out);
    if (f == FunctorDouble) {
      out = MkFloatTerm(FloatOfTerm(out));
    } else if (f == FunctorLongInt) {
      out = MkLongIntTerm(LongIntOfTerm(out));
    }
#ifdef USE_GMP
    else {
      out = _YAP_MkBigIntTerm(_YAP_BigIntOfTerm(out));
    }
#endif
  }
  READ_UNLOCK(RepValProp(p0)->VRWLock);
  return (out);
}

void
_YAP_PutValue(Atom a, Term v)
{
  AtomEntry *ae = RepAtom(a);
  Prop p0;
  ValEntry *p;
  Term t0;

  WRITE_LOCK(ae->ARWLock);
  p0 = GetAPropHavingLock(ae, ValProperty);
  if (p0 != NIL) {
    p = RepValProp(p0);
    WRITE_LOCK(p->VRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  } else {
    p = (ValEntry *) _YAP_AllocAtomSpace(sizeof(ValEntry));
    p->NextOfPE = RepAtom(a)->PropsOfAE;
    RepAtom(a)->PropsOfAE = AbsValProp(p);
    p->KindOfPE = ValProperty;
    p->ValueOfVE = TermNil;
    /* take care that the lock for the property will be inited even
       if someone else searches for the property */
    INIT_RWLOCK(p->VRWLock);
    WRITE_LOCK(p->VRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  }
  t0 = p->ValueOfVE;
  if (IsFloatTerm(v)) {
    /* store a float in code space, so that we can access the property */
    union {
      Float f;
      CELL ar[sizeof(Float) / sizeof(CELL)];
    } un;
    CELL *pt, *iptr;
    unsigned int i;

    un.f = FloatOfTerm(v);
    if (IsFloatTerm(t0)) {
      pt = RepAppl(t0);
    } else {
      if (IsApplTerm(t0)) {
	_YAP_FreeCodeSpace((char *) (RepAppl(t0)));
      }
      pt = (CELL *) _YAP_AllocAtomSpace(sizeof(CELL)*(1 + 2*sizeof(Float)/sizeof(CELL)));
      p->ValueOfVE = AbsAppl(pt);
      pt[0] = (CELL)FunctorDouble;
    }

    iptr = pt+1;
    for (i = 0; i < sizeof(Float) / sizeof(CELL); i++) {
      *iptr++ = (CELL)un.ar[i];
    }
  } else if (IsLongIntTerm(v)) {
    CELL *pt;
    Int val = LongIntOfTerm(v);

    if (IsLongIntTerm(t0)) {
      pt = RepAppl(t0);
    } else {
      if (IsApplTerm(t0)) {
	_YAP_FreeCodeSpace((char *) (RepAppl(t0)));
      }
      pt = (CELL *) _YAP_AllocAtomSpace(2*sizeof(CELL));
      p->ValueOfVE = AbsAppl(pt);
      pt[0] = (CELL)FunctorLongInt;
    }
    pt[1] = (CELL)val;
#ifdef USE_GMP
  } else if (IsBigIntTerm(v)) {
    CELL *ap = RepAppl(v);
    Int sz = 
      sizeof(MP_INT)+sizeof(CELL)+
      (((MP_INT *)(ap+1))->_mp_alloc*sizeof(mp_limb_t));
    CELL *pt = (CELL *) _YAP_AllocAtomSpace(sz);
    if (IsApplTerm(t0)) {
      _YAP_FreeCodeSpace((char *) RepAppl(t0));
    }
    memcpy((void *)pt, (void *)ap, sz);
    p->ValueOfVE = AbsAppl(pt);
#endif
  } else {
    if (IsApplTerm(t0)) {
      /* recover space */
      _YAP_FreeCodeSpace((char *) (RepAppl(p->ValueOfVE)));
    }
    p->ValueOfVE = v;
  }
  WRITE_UNLOCK(p->VRWLock);
}

Term
_YAP_StringToList(char *s)
{
  register Term t;
  register unsigned char *cp = (unsigned char *)s + strlen(s);

  t = MkAtomTerm(AtomNil);
  while (cp > (unsigned char *)s) {
    t = MkPairTerm(MkIntTerm(*--cp), t);
  }
  return (t);
}

Term
_YAP_StringToListOfAtoms(char *s)
{
  register Term t;
  char so[2];
  register unsigned char *cp = (unsigned char *)s + strlen(s);

  so[1] = '\0';
  t = MkAtomTerm(AtomNil);
  while (cp > (unsigned char *)s) {
    so[0] = *--cp;
    t = MkPairTerm(MkAtomTerm(LookupAtom(so)), t);
  }
  return (t);
}

Term
_YAP_ArrayToList(register Term *tp, int nof)
{
  register Term *pt = tp + nof;
  register Term t;

  t = MkAtomTerm(AtomNil);
  while (pt > tp) {
    Term tm = *--pt;
#if SBA
    if (tm == 0)
      t = MkPairTerm((CELL)pt, t);
    else
#endif
      t = MkPairTerm(tm, t);
  }
  return (t);
}

int
_YAP_GetName(char *s, UInt max, Term t)
{
  register Term Head;
  register Int i;

  if (IsVarTerm(t) || !IsPairTerm(t))
    return (FALSE);
  while (IsPairTerm(t)) {
    Head = HeadOfTerm(t);
    if (!IsNumTerm(Head))
      return (FALSE);
    i = IntOfTerm(Head);
    if (i < 0 || i > 255)
      return (FALSE);
    *s++ = i;
    t = TailOfTerm(t);
    if (--max == 0) {
      _YAP_Error(FATAL_ERROR,t,"not enough space for GetName");      
    }
  }
  *s = '\0';
  return (TRUE);
}

#ifdef SFUNC

Term
MkSFTerm(Functor f, int n, Term *a, empty_value)
{
  Term t, p = AbsAppl(H);
  int i;

  *H++ = f;
  RESET_VARIABLE(H);
  ++H;
  for (i = 1; i <= n; ++i) {
    t = Derefa(a++);
    if (t != empty_value) {
      *H++ = i;
      *H++ = t;
    }
  }
  *H++ = 0;
  return (p);
}

CELL *
ArgsOfSFTerm(Term t)
{
  CELL *p = RepAppl(t) + 1;

  while (*p != (CELL) p)
    p = CellPtr(*p) + 1;
  return (p + 1);
}

#endif

long
_YAP_NewSlots(int n)
{
  Int old_slots = IntOfTerm(ASP[0]), oldn = n;
  while (n > 0) {
    RESET_VARIABLE(ASP);
    ASP--;
    n--;
  }
  ASP[0] = MkIntTerm(old_slots+oldn);
  return((ASP+1)-LCL0);
}

long
_YAP_InitSlot(Term t)
{
  Int old_slots = IntOfTerm(ASP[0]);
  *ASP = t;
  ASP--;
  ASP[0] = MkIntTerm(old_slots+1);
  return((ASP+1)-LCL0);
}

void
_YAP_RecoverSlots(int n)
{
  Int old_slots = IntOfTerm(ASP[0]);
  ASP += n;
  ASP[0] = MkIntTerm(old_slots-n);
}

Term
_YAP_GetFromSlot(long slot)
{
  return(Deref(LCL0[slot]));
}

Term *
_YAP_AddressFromSlot(long slot)
{
  return(LCL0+slot);
}

void
_YAP_PutInSlot(long slot, Term t)
{
  LCL0[slot] = t;
}


