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

#include "Yap.h"
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

  pp = RepFunctorProp(ae->PropOfAE);
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
  p = (FunctorEntry *) AllocAtomSpace(sizeof(*p));
  p->KindOfPE = FunctorProperty;
  p->NameOfFE = AbsAtom(ae);
  p->ArityOfFE = arity;
  p->NextOfPE = ae->PropOfAE;
  ae->PropOfAE = AbsProp((PropEntry *) p);
  return ((Functor) p);
}

Functor
UnlockedMkFunctor(AtomEntry *ae, unsigned int arity)
{
  return(InlinedUnlockedMkFunctor(ae, arity));
}

/* vsc: We must guarantee that IsVarTerm(functor) returns true! */
Functor
MkFunctor(Atom ap, unsigned int arity)
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
MkFunctorWithAddress(Atom ap, unsigned int arity, FunctorEntry *p)
{
  AtomEntry *ae = RepAtom(ap);
  
  WRITE_LOCK(ae->ARWLock);
  p->KindOfPE = FunctorProperty;
  p->NameOfFE = ap;
  p->ArityOfFE = arity;
  p->NextOfPE = RepAtom(ap)->PropOfAE;
  ae->PropOfAE = AbsProp((PropEntry *) p);
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

Atom
LookupAtom(char *atom)
{				/* lookup atom in atom table            */
  register CELL hash;
  register unsigned char *p;
  Atom a;
  AtomEntry *ae;

  /* compute hash */
  p = (unsigned char *)atom;
  HashFunction(p, hash);
  WRITE_LOCK(HashChain[hash].AERWLock);
  a = HashChain[hash].Entry;
  /* search atom in chain */
  a = SearchAtom((unsigned char *)atom, a);
  if (a != NIL) {
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return(a);
  }
  /* add new atom to start of chain */
  ae = (AtomEntry *) AllocAtomSpace((sizeof *ae) + strlen(atom));
  a = AbsAtom(ae);
  ae->NextOfAE = HashChain[hash].Entry;
  HashChain[hash].Entry = a;
  ae->PropOfAE = NIL;
  if (ae->StrOfAE != atom)
    strcpy(ae->StrOfAE, atom);
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
  return (a);
}

Atom
FullLookupAtom(char *atom)
{				/* lookup atom in atom table            */
  Atom t;

  if ((t = SearchInInvisible(atom)) != NIL) {
    return (t);
  }
  return(LookupAtom(atom));
}

void
LookupAtomWithAddress(char *atom, AtomEntry *ae)
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
    Error(FATAL_ERROR,TermNil,"repeated initialisation for atom %s", ae);
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return;
  }
  /* add new atom to start of chain */
  ae->NextOfAE = a;
  HashChain[hash].Entry = AbsAtom(ae);
  ae->PropOfAE = NIL;
  strcpy(ae->StrOfAE, atom);
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
}

void
ReleaseAtom(Atom atom)
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
StaticLockedGetAProp(AtomEntry *ae, PropFlags kind)
{				/* look property list of atom a for kind  */
  PropEntry *pp;

  pp = RepProp(ae->PropOfAE);
  while (!EndOfPAEntr(pp) && pp->KindOfPE != kind)
    pp = RepProp(pp->NextOfPE);
  return (AbsProp(pp));
}

Prop
LockedGetAProp(AtomEntry *ae, PropFlags kind)
{				/* look property list of atom a for kind  */
  return (StaticLockedGetAProp(ae,kind));
}

Prop
GetAProp(Atom a, PropFlags kind)
{				/* look property list of atom a for kind  */
  AtomEntry *ae = RepAtom(a);
  Prop out;

  READ_LOCK(ae->ARWLock);
  out = StaticLockedGetAProp(ae, kind);
  READ_UNLOCK(ae->ARWLock);
  return (out);
}

Prop
GetPredProp(Atom ap, unsigned int arity)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;
  AtomEntry *ae = RepAtom(ap);
  PredEntry *p;

  READ_LOCK(ae->ARWLock);
  p = RepPredProp(p0 = ae->PropOfAE);
  while (p0 && (p->KindOfPE != PEProp || p->ArityOfPE != arity ||
		(p->ModuleOfPred && p->ModuleOfPred != CurrentModule)))
    p = RepPredProp(p0 = p->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  return (p0);
}

Prop
LockedGetPredProp(Atom ap, unsigned int arity)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;
  AtomEntry *ae = RepAtom(ap);
  PredEntry *p;

  p = RepPredProp(p0 = ae->PropOfAE);
  while (p0 && (p->KindOfPE != PEProp || p->ArityOfPE != arity ||
		(p->ModuleOfPred && p->ModuleOfPred != CurrentModule)))
    p = RepPredProp(p0 = p->NextOfPE);
  return (p0);
}

/* get expression entry for at/arity;               */
Prop
GetExpProp(Atom at, unsigned int arity)
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  ExpEntry *p;

  READ_LOCK(ae->ARWLock);
  p = RepExpProp(p0 = ae->PropOfAE);
  while (p0 && (p->KindOfPE != ExpProperty || p->ArityOfEE != arity))
    p = RepExpProp(p0 = p->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  return (p0);
}

/* get expression entry for at/arity, at is already locked;         */
Prop
LockedGetExpProp(AtomEntry *ae, unsigned int arity)
{
  Prop p0;
  ExpEntry *p;

  p = RepExpProp(p0 = ae->PropOfAE);
  while (p0 && (p->KindOfPE != ExpProperty || p->ArityOfEE != arity))
    p = RepExpProp(p0 = p->NextOfPE);
  return (p0);
}

Prop
PredProp(Atom ap, unsigned int arity)
     /* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(ap);
  PredEntry *p;

  WRITE_LOCK(ae->ARWLock);
  p = RepPredProp(p0 = RepAtom(ap)->PropOfAE);
  while (p0 && (p->KindOfPE != 0 || p->ArityOfPE != arity ||
		(p->ModuleOfPred && p->ModuleOfPred != CurrentModule)))
    p = RepPredProp(p0 = p->NextOfPE);

  if (p0 != NIL) {
    WRITE_UNLOCK(ae->ARWLock);
    return (p0);
  }
  p = (PredEntry *) AllocAtomSpace(sizeof(*p));
  INIT_RWLOCK(p->PRWLock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = arity;
  p->FirstClause = p->LastClause = NIL;
  p->PredFlags = 0L;
  p->StateOfPred = 0;
  p->OwnerFile = AtomNil;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->TrueCodeOfPred = p->CodeOfPred = (CODEADDR)(&(p->OpcodeOfPred)); 
  p->ModuleOfPred = CurrentModule;
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = ae->PropOfAE;
  ae->PropOfAE = p0 = AbsPredProp(p);
  if (arity == 0)
    p->FunctorOfPred = (Functor) ap;
  else {
    p->FunctorOfPred = InlinedUnlockedMkFunctor(ae, arity);
  }
  WRITE_UNLOCK(ae->ARWLock);
  return (p0);
}

Term
GetValue(Atom a)
{
  Prop p0 = GetAProp(a, ValProperty);
  Term out;

  if (p0 == NIL)
    return (TermNil);
  READ_LOCK(RepValProp(p0)->VRWLock);
  out = RepValProp(p0)->ValueOfVE;
  READ_UNLOCK(RepValProp(p0)->VRWLock);
  return (out);
}

void
PutValue(Atom a, Term v)
{
  AtomEntry *ae = RepAtom(a);
  Prop p0;
  ValEntry *p;

  WRITE_LOCK(ae->ARWLock);
  p0 = LockedGetAProp(ae, ValProperty);
  if (p0 != NIL) {
    p = RepValProp(p0);
    WRITE_LOCK(p->VRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  } else {
    p = (ValEntry *) AllocAtomSpace(sizeof(ValEntry));
    p->NextOfPE = RepAtom(a)->PropOfAE;
    RepAtom(a)->PropOfAE = AbsValProp(p);
    p->KindOfPE = ValProperty;
    /* take care that the lock for the property will be inited even
       if someone else searches for the property */
    INIT_RWLOCK(p->VRWLock);
    WRITE_LOCK(p->VRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  }
  if (IsFloatTerm(v)) {
    /* store a float in code space, so that we can access the property */
    union {
      Float f;
      CELL ar[sizeof(Float) / sizeof(CELL)];
    } un;
    CELL *pt, *iptr;
    unsigned int i;

    un.f = FloatOfTerm(v);
    if (p0 != NIL && IsApplTerm(p->ValueOfVE))
      pt = RepAppl(p->ValueOfVE);
    else {
      pt = (CELL *) AllocAtomSpace(sizeof(CELL)*(1 + 2*sizeof(Float)/sizeof(CELL)));
    }

    pt[0] = (CELL)FunctorDouble;
    iptr = pt+1;
    for (i = 0; i < sizeof(Float) / sizeof(CELL); i++) {
      *iptr++ = MkIntTerm(un.ar[i]/65536);
      *iptr++ = MkIntTerm(un.ar[i]%65536);
    }
    p->ValueOfVE = AbsAppl(pt);
  } else if (IsLongIntTerm(v)) {
    CELL *pt;
    Int val = LongIntOfTerm(v);
    if (p0 != NIL && IsApplTerm(p->ValueOfVE)) { 
      pt = RepAppl(p->ValueOfVE);
    } else {
      pt = (CELL *) AllocAtomSpace(3 * sizeof(CELL));
    }
    pt[0] = (CELL)FunctorLongInt;
    pt[1] = MkIntTerm(val/65536);
    pt[2] = MkIntTerm(val%65536);
    p->ValueOfVE = AbsAppl(pt);
  } else {
    if (p0 != NIL && IsApplTerm(p->ValueOfVE)) {
      /* recover space */
      FreeCodeSpace((char *) (RepAppl(p->ValueOfVE)));
    }
    p->ValueOfVE = v;
  }
  WRITE_UNLOCK(p->VRWLock);
}

Term
StringToList(char *s)
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
StringToListOfAtoms(char *s)
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
ArrayToList(register Term *tp, int nof)
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
GetName(char *s, Term t)
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
