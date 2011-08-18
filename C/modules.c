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
 File:		modules.c						 *
* Last rev:								 *
* mods:									 *
* comments:	module support						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"

STATIC_PROTO(Int p_current_module, ( USES_REGS1 ));
STATIC_PROTO(Int p_current_module1, ( USES_REGS1 ));


inline static ModEntry *
FetchModuleEntry(Atom at)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    ModEntry *me = RepModProp(p0);
    if ( me->KindOfPE == ModProperty
	 ) {
      READ_UNLOCK(ae->ARWLock);
      return me;
    }
    p0 = me->NextOfPE;
  }
  READ_UNLOCK(ae->ARWLock);
  return NULL;
}

inline static ModEntry *
GetModuleEntry(Atom at)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  ModEntry *new;

  p0 = ae->PropsOfAE;
  while (p0) {
    ModEntry *me = RepModProp(p0);
    if ( me->KindOfPE == ModProperty
	 ) {
      return me;
    }
    p0 = me->NextOfPE;
  }
  new = (ModEntry *) Yap_AllocAtomSpace(sizeof(*new));
  INIT_RWLOCK(new->ModRWLock);
  new->KindOfPE = ModProperty;
  new->PredForME = NULL;
  new->NextME = CurrentModules;
  CurrentModules = new;
  new->AtomOfME = ae;
  AddPropToAtom(ae, (PropEntry *)new);
  return new;
}


#define ByteAdr(X) ((char *) &(X))
Term 
Yap_Module_Name(PredEntry *ap)
{
  CACHE_REGS
  Term mod;
  if (!ap->ModuleOfPred)
    /* If the system predicate is a metacall I should return the
       module for the metacall, which I will suppose has to be
       reachable from the current module anyway.

       So I will return the current module in case the system
       predicate is a meta-call. Otherwise it will still work.
    */
    mod =  CurrentModule;
  else {
    mod = ap->ModuleOfPred;
  }
  if (mod) return mod;
  return TermProlog;
}

static ModEntry * 
LookupModule(Term a)
{
  Atom at;
  ModEntry *me;

  /* prolog module */
  if (a == 0)
    return GetModuleEntry(AtomOfTerm(TermProlog));
  at = AtomOfTerm(a);
  me = GetModuleEntry(at);
  return me;
}

Term
Yap_Module(Term tmod)
{
  LookupModule(tmod);
  return tmod;
}

struct pred_entry *
Yap_ModulePred(Term mod)
{
  ModEntry *me;
  if (!(me = LookupModule(mod)))
    return NULL;
  return me->PredForME;
}

void
Yap_NewModulePred(Term mod, struct pred_entry *ap)
{
  ModEntry *me;

  if (!(me = LookupModule(mod)))
    return;
  WRITE_LOCK(me->ModRWLock);
  ap->NextPredOfModule = me->PredForME;
  me->PredForME = ap;
  WRITE_UNLOCK(me->ModRWLock);
}

static Int 
p_current_module( USES_REGS1 )
{				/* $current_module(Old,New)		 */
  Term            t;
	
  if (CurrentModule) {
    if(!Yap_unify_constant(ARG1, CurrentModule))
      return FALSE;
  } else {
    if (!Yap_unify_constant(ARG1, TermProlog))
      return FALSE;
  }
  t = Deref(ARG2);
  if (IsVarTerm(t) || !IsAtomTerm(t))
    return FALSE;
  if (t == TermProlog) {
    CurrentModule = PROLOG_MODULE;
  } else {
    CurrentModule = t;
    LookupModule(CurrentModule);
  }
  return TRUE;
}

static Int
p_current_module1( USES_REGS1 )
{				/* $current_module(Old)		 */
  if (CurrentModule)
    return Yap_unify_constant(ARG1, CurrentModule);
  return Yap_unify_constant(ARG1, TermProlog);
}

static Int
p_change_module( USES_REGS1 )
{				/* $change_module(New)		 */
  Term mod = Deref(ARG1);
  LookupModule(mod);
  CurrentModule = mod;
  return TRUE;
}

static Int 
cont_current_module( USES_REGS1 )
{
  ModEntry  *imod = (ModEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(1,1)), *next;
  Term t = MkAtomTerm(imod->AtomOfME);
  next = imod->NextME;

  /* ARG1 is unbound */
  Yap_unify(ARG1,t);
  if (!next)
    cut_succeed();
  EXTRA_CBACK_ARG(1,1) = MkIntegerTerm((Int)next);
  return TRUE;
}

static Int 
init_current_module( USES_REGS1 )
{				/* current_module(?ModuleName)		 */
  Term t = Deref(ARG1);
  if (!IsVarTerm(t)) {
    if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM,t,"module name must be an atom");
      return FALSE;
    }
    if (FetchModuleEntry(AtomOfTerm(t)) != NULL)
      cut_succeed();
    cut_fail();
  }
  EXTRA_CBACK_ARG(1,1) = MkIntegerTerm((Int)CurrentModules);
  return cont_current_module( PASS_REGS1 );
}

static Int
p_strip_module( USES_REGS1 )
{
  Term t1 = Deref(ARG1), t2, tmod = CurrentModule;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  if (IsVarTerm(t1) ||
      !IsApplTerm(t1) ||
      FunctorOfTerm(t1) != FunctorModule ||
      IsVarTerm(t2 = ArgOfTerm(1,t1)) ||
      !IsAtomTerm(t2)) {
    return Yap_unify(ARG3, t1) &&
      Yap_unify(ARG2, tmod);
  }
  do {
    tmod = t2;
    t1 = ArgOfTerm(2,t1);
  } while (!IsVarTerm(t1) &&
	   IsApplTerm(t1) &&
	   FunctorOfTerm(t1) == FunctorModule &&
	   !IsVarTerm(t2 = ArgOfTerm(1,t1)) &&
	   IsAtomTerm(t2));
  return Yap_unify(ARG3, t1) &&
    Yap_unify(ARG2, tmod);      
}

static Int
p_context_module( USES_REGS1 )
{
  yamop *parentcp = P;
  CELL *yenv;
  PredEntry *ap = EnvPreg(parentcp);
  if (ap->ModuleOfPred &&
      !(ap->PredFlags & MetaPredFlag))
    return Yap_unify(ARG1, ap->ModuleOfPred);
  parentcp = CP;
  yenv = ENV;
  do {
    ap = EnvPreg(parentcp);
    if (ap->ModuleOfPred &&
	!(ap->PredFlags & MetaPredFlag))
      return Yap_unify(ARG1, ap->ModuleOfPred);
    parentcp = (yamop *)yenv[E_CP];
    yenv = (CELL *)yenv[E_E];
  } while(yenv);
  return Yap_unify(ARG1, CurrentModule);
}

Term
Yap_StripModule(Term t,  Term *modp)
{
  CACHE_REGS
  Term tmod;

  tmod = CurrentModule;
 restart:
  if (IsVarTerm(t)) {
    return 0L;
  } else if (IsAtomTerm(t) || IsPairTerm(t)) {
    *modp = tmod;
    return t;
  } else if (IsApplTerm(t)) {
    Functor    fun = FunctorOfTerm(t);
    if (fun == FunctorModule) {
      tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod) ) {
	return 0L;
      }
      if (!IsAtomTerm(tmod) ) {
	return 0L;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    *modp = tmod;
    return t;
  }
  return 0L;
}



void 
Yap_InitModulesC(void)
{
  Yap_InitCPred("$current_module", 2, p_current_module, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$current_module", 1, p_current_module1, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$change_module", 1, p_change_module, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("strip_module", 3, p_strip_module, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("context_module", 1, p_context_module, 0);
  Yap_InitCPredBack("$all_current_modules", 1, 1, init_current_module, cont_current_module,
		SafePredFlag|SyncPredFlag|HiddenPredFlag);
}


void 
Yap_InitModules(void)
{
  CACHE_REGS
  LookupModule(MkAtomTerm(AtomProlog));
  LookupModule(USER_MODULE);
  LookupModule(IDB_MODULE);
  LookupModule(ATTRIBUTES_MODULE);
  LookupModule(CHARSIO_MODULE);
  LookupModule(TERMS_MODULE);
  LookupModule(SYSTEM_MODULE);
  LookupModule(READUTIL_MODULE);
  LookupModule(HACKS_MODULE);
  LookupModule(ARG_MODULE);
  LookupModule(GLOBALS_MODULE);
  LookupModule(DBLOAD_MODULE);
  CurrentModule = PROLOG_MODULE;
}
