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
ADDR    STD_PROTO(Yap_PreAllocCodeSpace, (void));
Prop	STD_PROTO(PredPropByFunc,(Functor, Term));
Prop	STD_PROTO(PredPropByAtom,(Atom, Term));
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
  p = (FunctorEntry *) Yap_AllocAtomSpace(sizeof(*p));
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
Yap_UnlockedMkFunctor(AtomEntry *ae, unsigned int arity)
{
  return(InlinedUnlockedMkFunctor(ae, arity));
}

/* vsc: We must guarantee that IsVarTerm(functor) returns true! */
Functor
Yap_MkFunctor(Atom ap, unsigned int arity)
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
Yap_MkFunctorWithAddress(Atom ap, unsigned int arity, FunctorEntry *p)
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
  Atom a, na;
  AtomEntry *ae;

  /* compute hash */
  p = (unsigned char *)atom;
  hash = HashFunction(p) % AtomHashTableSize;
  /* we'll start by holding a read lock in order to avoid contention */
  READ_LOCK(HashChain[hash].AERWLock);
  a = HashChain[hash].Entry;
  /* search atom in chain */
  na = SearchAtom((unsigned char *)atom, a);
  if (na != NIL) {
    READ_UNLOCK(HashChain[hash].AERWLock);
    return(na);
  }
  READ_UNLOCK(HashChain[hash].AERWLock);
  /* we need a write lock */
  WRITE_LOCK(HashChain[hash].AERWLock);
  /* concurrent version of Yap, need to take care */
#if defined(YAPOR) || defined(THREADS)
  if (a != HashChain[hash].Entry) {
    a = HashChain[hash].Entry;
    na = SearchAtom((unsigned char *)atom, a);
    if (na != NIL) {
      WRITE_UNLOCK(HashChain[hash].AERWLock);
      return(na);
    }
  }
#endif  
  /* add new atom to start of chain */
  ae = (AtomEntry *) Yap_AllocAtomSpace((sizeof *ae) + strlen(atom) + 1);
  if (ae == NULL) {
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return NIL;
  }
  NOfAtoms++;
  na = AbsAtom(ae);
  ae->PropsOfAE = NIL;
  if (ae->StrOfAE != atom)
    strcpy(ae->StrOfAE, atom);
  ae->NextOfAE = a;
  HashChain[hash].Entry = na;
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
  if (NOfAtoms > 2*AtomHashTableSize) {
    Yap_signal(YAP_CDOVF_SIGNAL);
  }
  return na;
}

Atom
Yap_LookupAtom(char *atom)
{				/* lookup atom in atom table            */
  return LookupAtom(atom);
}

Atom
Yap_FullLookupAtom(char *atom)
{				/* lookup atom in atom table            */
  Atom t;

  if ((t = SearchInInvisible(atom)) != NIL) {
    return (t);
  }
  return(LookupAtom(atom));
}

void
Yap_LookupAtomWithAddress(char *atom, AtomEntry *ae)
{				/* lookup atom in atom table            */
  register CELL hash;
  register unsigned char *p;
  Atom a;

  /* compute hash */
  p = (unsigned char *)atom;
  hash = HashFunction(p) % AtomHashTableSize;
  /* ask for a WRITE lock because it is highly unlikely we shall find anything */
  WRITE_LOCK(HashChain[hash].AERWLock);
  a = HashChain[hash].Entry;
  /* search atom in chain */
  if (SearchAtom(p, a) != NIL) {
    Yap_Error(INTERNAL_ERROR,TermNil,"repeated initialisation for atom %s", ae);
    WRITE_UNLOCK(HashChain[hash].AERWLock);
    return;
  }
  /* add new atom to start of chain */
  NOfAtoms++;
  ae->NextOfAE = a;
  HashChain[hash].Entry = AbsAtom(ae);
  ae->PropsOfAE = NIL;
  strcpy(ae->StrOfAE, atom);
  INIT_RWLOCK(ae->ARWLock);
  WRITE_UNLOCK(HashChain[hash].AERWLock);
}

void
Yap_ReleaseAtom(Atom atom)
{				/* Releases an atom from the hash chain */
  register Int hash;
  register unsigned char *p;
  AtomEntry *inChain;
  AtomEntry *ap = RepAtom(atom);
  char *name = ap->StrOfAE;

  /* compute hash */
  p = (unsigned char *)name;
  hash = HashFunction(p) % AtomHashTableSize;
  WRITE_LOCK(HashChain[hash].AERWLock);
  if (HashChain[hash].Entry == atom) {
    NOfAtoms--;
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
Yap_GetAPropHavingLock(AtomEntry *ae, PropFlags kind)
{				/* look property list of atom a for kind  */
  return GetAPropHavingLock(ae,kind);
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
Yap_GetAProp(Atom a, PropFlags kind)
{				/* look property list of atom a for kind  */
  return GetAProp(a,kind);
}

OpEntry *
Yap_GetOpProp(Atom a)
{				/* look property list of atom a for kind  */
  AtomEntry *ae = RepAtom(a);
  PropEntry *pp;

  READ_LOCK(ae->ARWLock);
  pp = RepProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) &&
	 ( pp->KindOfPE != OpProperty ||
	   (((OpEntry *)pp)->OpModule &&
	    ((OpEntry *)pp)->OpModule != CurrentModule)))
    pp = RepProp(pp->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  if (EndOfPAEntr(pp))
    return NULL;
  else
    return (OpEntry *)pp;
}


inline static Prop
GetPredPropByAtomHavingLock(AtomEntry* ae, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  p0 = ae->PropsOfAE;
  while (p0) {
    PredEntry *pe = RepPredProp(p0);
    if ( pe->KindOfPE == PEProp && 
	 (pe->ModuleOfPred == cur_mod || !pe->ModuleOfPred)) {
      return(p0);
#if THREADS
      /* Thread Local Predicates */
      if (pe->PredFlags & ThreadLocalPredFlag) {
	return AbsPredProp(Yap_GetThreadPred(pe));
      }
#endif
    }
    p0 = pe->NextOfPE;
  }
  return(NIL);
}

Prop
Yap_GetPredPropByAtom(Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = GetPredPropByAtomHavingLock(ae, cur_mod);
  READ_UNLOCK(ae->ARWLock);
  return(p0);
}


inline static Prop
GetPredPropByAtomHavingLockInThisModule(AtomEntry* ae, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  p0 = ae->PropsOfAE;
  while (p0) {
    PredEntry *pe = RepPredProp(p0);
    if ( pe->KindOfPE == PEProp && pe->ModuleOfPred == cur_mod ) {
#if THREADS
      /* Thread Local Predicates */
      if (pe->PredFlags & ThreadLocalPredFlag) {
	return AbsPredProp(Yap_GetThreadPred(pe));
      }
#endif
      return(p0);
    }
    p0 = pe->NextOfPE;
  }
  return(NIL);
}

Prop
Yap_GetPredPropByAtomInThisModule(Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = GetPredPropByAtomHavingLockInThisModule(ae, cur_mod);
  READ_UNLOCK(ae->ARWLock);
  return(p0);
}


static inline Prop
GetPredPropByFuncHavingLock(Functor f, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  FunctorEntry *fe = (FunctorEntry *)f;

  p0 = fe->PropsOfFE;
  while (p0) {
    PredEntry *p = RepPredProp(p0);
    if (/* p->KindOfPE != 0 || only props */
	(p->ModuleOfPred == cur_mod || !(p->ModuleOfPred))) {
#if THREADS
      /* Thread Local Predicates */
      if (p->PredFlags & ThreadLocalPredFlag) {
	return AbsPredProp(Yap_GetThreadPred(p));
      }
#endif
      return (p0);
    }
    p0 = p->NextOfPE;
  }
  return(NIL);
}

Prop
Yap_GetPredPropByFunc(Functor f, Term cur_mod)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;

  READ_LOCK(f->FRWLock);
  p0 = GetPredPropByFuncHavingLock(f, cur_mod);
  READ_UNLOCK(f->FRWLock);
  return (p0);
}

static inline Prop
GetPredPropByFuncHavingLockInThisModule(Functor f, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  FunctorEntry *fe = (FunctorEntry *)f;

  p0 = fe->PropsOfFE;
  while (p0) {
    PredEntry *p = RepPredProp(p0);
    if (p->ModuleOfPred == cur_mod) {
#if THREADS
      /* Thread Local Predicates */
      if (p->PredFlags & ThreadLocalPredFlag) {
	return AbsPredProp(Yap_GetThreadPred(p));
      }
#endif
      return (p0);
    }
    p0 = p->NextOfPE;
  }
  return(NIL);
}

Prop
Yap_GetPredPropByFuncInThisModule(Functor f, Term cur_mod)
     /* get predicate entry for ap/arity;               */
{
  Prop p0;

  READ_LOCK(f->FRWLock);
  p0 = GetPredPropByFuncHavingLockInThisModule(f, cur_mod);
  READ_UNLOCK(f->FRWLock);
  return (p0);
}

Prop
Yap_GetPredPropHavingLock(Atom ap, unsigned int arity, Term mod)
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
Yap_GetExpProp(Atom at, unsigned int arity)
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
Yap_GetExpPropHavingLock(AtomEntry *ae, unsigned int arity)
{
  Prop p0;
  ExpEntry *p;

  p = RepExpProp(p0 = ae->PropsOfAE);
  while (p0 && (p->KindOfPE != ExpProperty || p->ArityOfEE != arity))
    p = RepExpProp(p0 = p->NextOfPE);
  return (p0);
}

/* fe is supposed to be locked */
Prop
Yap_NewPredPropByFunctor(FunctorEntry *fe, Term cur_mod)
{
  Prop p0;
  PredEntry *p = (PredEntry *) Yap_AllocAtomSpace(sizeof(*p));

  INIT_RWLOCK(p->PRWLock);
  INIT_LOCK(p->PELock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = fe->ArityOfFE;
  p->cs.p_code.FirstClause = p->cs.p_code.LastClause = NULL;
  p->cs.p_code.NOfClauses = 0;
  p->PredFlags = 0L;
  p->src.OwnerFile = AtomNil;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->CodeOfPred = p->cs.p_code.TrueCodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
  p->cs.p_code.ExpandCode = EXPAND_OP_CODE; 
  if (cur_mod == TermProlog)
    p->ModuleOfPred = 0L;
  else
    p->ModuleOfPred = cur_mod;
  Yap_NewModulePred(cur_mod, p);
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
#ifdef BEAM
  p->beamTable = NULL;
#endif  /* BEAM */
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = fe->PropsOfFE;
  if (PRED_GOAL_EXPANSION_FUNC) {
    if (fe->PropsOfFE &&
	(RepPredProp(fe->PropsOfFE)->PredFlags & GoalExPredFlag)) {
      p->PredFlags |= GoalExPredFlag;
    }
  }
  fe->PropsOfFE = p0 = AbsPredProp(p);
  p->FunctorOfPred = (Functor)fe;
  WRITE_UNLOCK(fe->FRWLock);
#ifdef LOW_PROF
    if (ProfilerOn &&
	Yap_OffLineProfiler) {
      Yap_inform_profiler_of_clause((yamop *)&(p->OpcodeOfPred), (yamop *)(&(p->OpcodeOfPred)+1), p, 1);
      if (!(p->PredFlags & (CPredFlag|AsmPredFlag))) {
	Yap_inform_profiler_of_clause((yamop *)&(p->cs.p_code.ExpandCode), (yamop *)(&(p->cs.p_code.ExpandCode)+1), p, 1);
      }
    }
#endif /* LOW_PROF */
  return p0;
}

#if THREADS
Prop
Yap_NewThreadPred(PredEntry *ap)
{
  PredEntry *p = (PredEntry *) Yap_AllocAtomSpace(sizeof(*p));

  INIT_RWLOCK(p->PRWLock);
  INIT_LOCK(p->PELock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = ap->ArityOfPE;
  p->cs.p_code.FirstClause = p->cs.p_code.LastClause = NULL;
  p->cs.p_code.NOfClauses = 0;
  p->PredFlags = ap->PredFlags & ~(IndexedPredFlag|SpiedPredFlag);
  p->src.OwnerFile = ap->src.OwnerFile;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->CodeOfPred = p->cs.p_code.TrueCodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
  p->cs.p_code.ExpandCode = EXPAND_OP_CODE; 
  p->ModuleOfPred = ap->ModuleOfPred;
  p->NextPredOfModule = NULL;
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
#ifdef BEAM
  p->beamTable = NULL;
#endif 
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = AbsPredProp(ThreadHandle[worker_id].local_preds);
  ThreadHandle[worker_id].local_preds = p;
  p->FunctorOfPred = ap->FunctorOfPred;
#ifdef LOW_PROF
    if (ProfilerOn &&
	Yap_OffLineProfiler) {
      Yap_inform_profiler_of_clause((yamop *)&(p->OpcodeOfPred), (yamop *)(&(p->OpcodeOfPred)+1), p, 1);
      if (!(p->PredFlags & (CPredFlag|AsmPredFlag))) {
	Yap_inform_profiler_of_clause((yamop *)&(p->cs.p_code.ExpandCode), (yamop *)(&(p->cs.p_code.ExpandCode)+1), p, 1);
      }
    }
#endif /* LOW_PROF */
  return AbsPredProp(p);
}
#endif

Prop
Yap_NewPredPropByAtom(AtomEntry *ae, Term cur_mod)
{
  Prop p0;
  PredEntry *p = (PredEntry *) Yap_AllocAtomSpace(sizeof(*p));

/* Printf("entering %s:%s/0\n", RepAtom(AtomOfTerm(cur_mod))->StrOfAE, ae->StrOfAE); */

  INIT_RWLOCK(p->PRWLock);
  INIT_LOCK(p->PELock);
  p->KindOfPE = PEProp;
  p->ArityOfPE = 0;
  p->cs.p_code.FirstClause = p->cs.p_code.LastClause = NULL;
  p->cs.p_code.NOfClauses = 0;
  p->PredFlags = 0L;
  p->src.OwnerFile = AtomNil;
  p->OpcodeOfPred = UNDEF_OPCODE;
  p->cs.p_code.ExpandCode = EXPAND_OP_CODE; 
  p->CodeOfPred = p->cs.p_code.TrueCodeOfPred = (yamop *)(&(p->OpcodeOfPred)); 
  if (cur_mod == TermProlog)
    p->ModuleOfPred = 0;
  else
    p->ModuleOfPred = cur_mod;
  Yap_NewModulePred(cur_mod, p);
  INIT_LOCK(p->StatisticsForPred.lock);
  p->StatisticsForPred.NOfEntries = 0;
  p->StatisticsForPred.NOfHeadSuccesses = 0;
  p->StatisticsForPred.NOfRetries = 0;
#ifdef TABLING
  p->TableOfPred = NULL;
#endif /* TABLING */
#ifdef BEAM
  p->beamTable = NULL;
#endif 
  /* careful that they don't cross MkFunctor */
  p->NextOfPE = ae->PropsOfAE;
  if (PRED_GOAL_EXPANSION_FUNC) {
    Prop p1 = ae->PropsOfAE;

    while (p1) {
      PredEntry *pe = RepPredProp(p1);

      if (pe->KindOfPE == PEProp) {
	if (pe->PredFlags & GoalExPredFlag) {
	  p->PredFlags |= GoalExPredFlag;
	}
	break;
      }
      p1 = pe->NextOfPE;
    }
  }
  ae->PropsOfAE = p0 = AbsPredProp(p);
  p->FunctorOfPred = (Functor)AbsAtom(ae);
  WRITE_UNLOCK(ae->ARWLock);
#ifdef LOW_PROF
    if (ProfilerOn &&
	Yap_OffLineProfiler) {
      Yap_inform_profiler_of_clause((yamop *)&(p->OpcodeOfPred), (yamop *)(&(p->OpcodeOfPred)+1), p, 1);
      if (!(p->PredFlags & (CPredFlag|AsmPredFlag))) {
	Yap_inform_profiler_of_clause((yamop *)&(p->cs.p_code.ExpandCode), (yamop *)(&(p->cs.p_code.ExpandCode)+1), p, 1);
      }
    }
#endif /* LOW_PROF */
  return p0;
}

Prop
Yap_PredPropByFunctorNonThreadLocal(Functor f, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  FunctorEntry *fe = (FunctorEntry *)f;

  WRITE_LOCK(fe->FRWLock);
  p0 = fe->PropsOfFE;
  while (p0) {
    PredEntry *p = RepPredProp(p0);
    if (/* p->KindOfPE != 0 || only props */
	(p->ModuleOfPred == cur_mod || !(p->ModuleOfPred))) {
      WRITE_UNLOCK(fe->FRWLock);
      return (p0);
    }
    p0 = p->NextOfPE;
  }
  return Yap_NewPredPropByFunctor(fe,cur_mod);
}

Prop
Yap_PredPropByAtomNonThreadLocal(Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  WRITE_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    PredEntry *pe = RepPredProp(p0);
    if ( pe->KindOfPE == PEProp && 
	 (pe->ModuleOfPred == cur_mod || !pe->ModuleOfPred)) {
      WRITE_UNLOCK(ae->ARWLock);
      return(p0);
    }
    p0 = pe->NextOfPE;
  }
  return Yap_NewPredPropByAtom(ae,cur_mod);
}



Term
Yap_GetValue(Atom a)
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
      out = Yap_MkBigIntTerm(Yap_BigIntOfTerm(out));
    }
#endif
  }
  READ_UNLOCK(RepValProp(p0)->VRWLock);
  return (out);
}

void
Yap_PutValue(Atom a, Term v)
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
    p = (ValEntry *) Yap_AllocAtomSpace(sizeof(ValEntry));
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
	Yap_FreeCodeSpace((char *) (RepAppl(t0)));
      }
      pt = (CELL *) Yap_AllocAtomSpace(sizeof(CELL)*(1 + 2*sizeof(Float)/sizeof(CELL)));
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
	Yap_FreeCodeSpace((char *) (RepAppl(t0)));
      }
      pt = (CELL *) Yap_AllocAtomSpace(2*sizeof(CELL));
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
    CELL *pt = (CELL *) Yap_AllocAtomSpace(sz);
    if (IsApplTerm(t0)) {
      Yap_FreeCodeSpace((char *) RepAppl(t0));
    }
    memcpy((void *)pt, (void *)ap, sz);
    p->ValueOfVE = AbsAppl(pt);
#endif
  } else {
    if (IsApplTerm(t0)) {
      /* recover space */
      Yap_FreeCodeSpace((char *) (RepAppl(p->ValueOfVE)));
    }
    p->ValueOfVE = v;
  }
  WRITE_UNLOCK(p->VRWLock);
}

Term
Yap_StringToList(char *s)
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
Yap_StringToListOfAtoms(char *s)
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
Yap_ArrayToList(register Term *tp, int nof)
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
Yap_GetName(char *s, UInt max, Term t)
{
  register Term Head;
  register Int i;

  if (IsVarTerm(t) || !IsPairTerm(t))
    return FALSE;
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
      Yap_Error(FATAL_ERROR,t,"not enough space for GetName");      
    }
  }
  *s = '\0';
  return TRUE;
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
Yap_NewSlots(int n)
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
Yap_InitSlot(Term t)
{
  Int old_slots = IntOfTerm(ASP[0]);
  *ASP = t;
  ASP--;
  ASP[0] = MkIntTerm(old_slots+1);
  return((ASP+1)-LCL0);
}

void
Yap_RecoverSlots(int n)
{
  Int old_slots = IntOfTerm(ASP[0]);
  ASP += n;
  ASP[0] = MkIntTerm(old_slots-n);
}

Term
Yap_GetFromSlot(long slot)
{
  return(Deref(LCL0[slot]));
}

Term *
Yap_AddressFromSlot(long slot)
{
  return(LCL0+slot);
}

void
Yap_PutInSlot(long slot, Term t)
{
  LCL0[slot] = t;
}


