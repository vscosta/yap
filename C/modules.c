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


#ifdef SCCSLookupSystemModule
static char SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "YapHeap.h"
#include "Yatom.h"

static Int current_module(USES_REGS1);
static Int current_module1(USES_REGS1);
static ModEntry *LookupModule(Term a);
static ModEntry *LookupSystemModule(Term a);
static ModEntry *GetModuleEntry(Atom at USES_REGS);
static ModEntry *FetchModuleEntry(Atom at);

/**
 * initialize module data-structure
 *
 * @param to parent module (CurrentModule)
 * @param ae module name.
 *
 * @return a new module structure
 */ /**               */
static ModEntry *initMod(AtomEntry *toname, AtomEntry *ae) {
  CACHE_REGS
  ModEntry *n, *parent;

  if (toname == NULL)
    parent = NULL;
  else {
    parent = FetchModuleEntry(toname);
  }
  n = (ModEntry *)Yap_AllocAtomSpace(sizeof(*n));
  INIT_RWLOCK(n->ModRWLock);
  n->KindOfPE = ModProperty;
  n->PredForME = NULL;
  n->OpForME = NULL;
  n->NextME = CurrentModules;
  CurrentModules = n;
  n->AtomOfME = ae;
  n->NextOfPE = NULL;
  n->OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  AddPropToAtom(ae, (PropEntry *)n);
  Yap_setModuleFlags(n, parent);
  return n;
}

/**
 * get predicate entry for ap/arity; create it if neccessary
 *
 * @param[in] at
 *
 * @return module descriptorxs
 */
static ModEntry *GetModuleEntry(Atom at USES_REGS) {
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    ModEntry *me = RepModProp(p0);
    if (me->KindOfPE == ModProperty) {
      READ_UNLOCK(ae->ARWLock);
      return me;
    }
    p0 = me->NextOfPE;
  }
  READ_UNLOCK(ae->ARWLock);

  return initMod(
      (CurrentModule == PROLOG_MODULE ? NULL : AtomOfTerm(CurrentModule)), at);
}

/** get  entry for ap/arity; assumes one is there.              */
static ModEntry *FetchModuleEntry(Atom at) {
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    ModEntry *me = RepModProp(p0);
    if (me->KindOfPE == ModProperty) {
      READ_UNLOCK(ae->ARWLock);
      return me;
    }
    p0 = me->NextOfPE;
  }
  READ_UNLOCK(ae->ARWLock);
  return NULL;
}

Term Yap_getUnknownModule(ModEntry *m) {
  if (m && m->flags & UNKNOWN_ERROR) {
    return TermError;
  } else if (m && m->flags & UNKNOWN_WARNING) {
    return TermWarning;
  } else if (m && m->flags & UNKNOWN_FAST_FAIL) {
    return TermFastFail;
  } else {
    return TermFail;
  }
}

bool Yap_getUnknown(Term mod) {
  ModEntry *m = LookupModule(mod);
  return Yap_getUnknownModule(m);
}

bool Yap_CharacterEscapes(Term mt) {
  CACHE_REGS
  if (mt == PROLOG_MODULE)
    mt = TermProlog;
  return GetModuleEntry(AtomOfTerm(mt) PASS_REGS)->flags & M_CHARESCAPE;
}

#define ByteAdr(X) ((char *)&(X))
Term Yap_Module_Name(PredEntry *ap) {
  CACHE_REGS

  if (!ap)
    return TermUser;
  if (!ap->ModuleOfPred)
    /* If the system predicate is a meta-call I should return the
       module for the metacall, which I will suppose has to be
       reachable from the current module anyway.

       So I will return the current module in case the system
       predicate is a meta-call. Otherwise it will still work.
    */
    return TermProlog;
  else {
    return ap->ModuleOfPred;
  }
}

static ModEntry *LookupSystemModule(Term a) {
  CACHE_REGS
  Atom at;
  ModEntry *me;

  /* prolog module */
  if (a == 0) {
    a = TermProlog;
  }
  at = AtomOfTerm(a);
  me = GetModuleEntry(at PASS_REGS);
  if (!me)
    return NULL;
  me->flags |= M_SYSTEM;
  me->OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  return me;
}

static ModEntry *LookupModule(Term a) {
  CACHE_REGS
  Atom at;
  ModEntry *me;

  /* prolog module */
  if (a == 0) {
    return GetModuleEntry(AtomProlog PASS_REGS);
  }
  at = AtomOfTerm(a);
  me = GetModuleEntry(at PASS_REGS);
  return me;
}

bool Yap_isSystemModule(Term a) {
  ModEntry *me = LookupModule(a);
  return me != NULL && me->flags & M_SYSTEM;
}

Term Yap_Module(Term tmod) {
  LookupModule(tmod);
  return tmod;
}

ModEntry *Yap_GetModuleEntry(Term mod) {
  ModEntry *me;

  if (!(me = LookupModule(mod)))
    return NULL;
  return me;
}

Term Yap_GetModuleFromEntry(ModEntry *me) {
  return MkAtomTerm(me->AtomOfME);
  ;
}

struct pred_entry *Yap_ModulePred(Term mod) {
  ModEntry *me;
  if (!(me = LookupModule(mod)))
    return NULL;
  return me->PredForME;
}

void Yap_NewModulePred(Term mod, struct pred_entry *ap) {
  ModEntry *me;

  if (mod == 0)
    mod = TermProlog;
  if (!(me = LookupModule(mod)))
    return;
  WRITE_LOCK(me->ModRWLock);
  ap->NextPredOfModule = me->PredForME;
  me->PredForME = ap;
  WRITE_UNLOCK(me->ModRWLock);
}

static Int
    current_module(USES_REGS1) { /* $current_module(Old,N)		 */
  Term t;

  if (CurrentModule) {
    if (!Yap_unify_constant(ARG1, CurrentModule))
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
    // make it very clear that t inherits from cm.
    LookupModule(t);
    CurrentModule = t;
  }
  LOCAL_SourceModule = CurrentModule;
  return TRUE;
}

static Int change_module(USES_REGS1) { /* $change_module(N)		 */
  Term mod = Deref(ARG1);
  LookupModule(mod);
  CurrentModule = mod;
  LOCAL_SourceModule = mod;
  return TRUE;
}

static Int current_module1(USES_REGS1) { /* $current_module(Old)
                                          */
  if (CurrentModule)
    return Yap_unify_constant(ARG1, CurrentModule);
  return Yap_unify_constant(ARG1, TermProlog);
}

static Int cont_current_module(USES_REGS1) {
  ModEntry *imod = AddressOfTerm(EXTRA_CBACK_ARG(1, 1)), *next;
  Term t = MkAtomTerm(imod->AtomOfME);
  next = imod->NextME;

  /* ARG1 is unbound */
  Yap_unify(ARG1, t);
  if (!next)
    cut_succeed();
  EXTRA_CBACK_ARG(1, 1) = MkAddressTerm(next);
  return TRUE;
}

static Int init_current_module(
    USES_REGS1) { /* current_module(?ModuleName)		 */
  Term t = Deref(ARG1);
  if (!IsVarTerm(t)) {
    if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM, t, "module name must be an atom");
      return FALSE;
    }
    if (FetchModuleEntry(AtomOfTerm(t)) != NULL)
      cut_succeed();
    cut_fail();
  }
  EXTRA_CBACK_ARG(1, 1) = MkIntegerTerm((Int)CurrentModules);
  return cont_current_module(PASS_REGS1);
}

static Int cont_ground_module(USES_REGS1) {
  ModEntry *imod = AddressOfTerm(EXTRA_CBACK_ARG(3, 1)), *next;
  Term t2 = MkAtomTerm(imod->AtomOfME);
  next = imod->NextME;

  /* ARG2 is unbound */
  if (!next)
    cut_succeed();
  EXTRA_CBACK_ARG(3, 1) = MkAddressTerm(next);
  return Yap_unify(ARG2, t2);
}

static Int init_ground_module(USES_REGS1) {
  /* current_module(?ModuleName)		 */
  Term t1 = Deref(ARG1), tmod = CurrentModule, t3;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  t3 = Yap_YapStripModule(t1, &tmod);
  if (!t3) {
    Yap_Error(TYPE_ERROR_CALLABLE, t3, "trying to obtain module");
    return FALSE;
  }
  if (!IsVarTerm(tmod)) {
    if (!IsAtomTerm(tmod)) {
      Yap_Error(TYPE_ERROR_ATOM, tmod, "module name must be an atom");
      cut_fail();
    }
    if (FetchModuleEntry(AtomOfTerm(tmod)) != NULL && Yap_unify(tmod, ARG2) &&
        Yap_unify(t3, ARG3)) {
      cut_succeed();
    }
    cut_fail();
  }
  if (!Yap_unify(ARG2, tmod) || !Yap_unify(ARG3, t3)) {
    cut_fail();
  }
  // make sure we keep the binding
  B->cp_tr = TR;
  B->cp_h = HR;
  EXTRA_CBACK_ARG(3, 1) = MkAddressTerm(CurrentModules);
  return cont_ground_module(PASS_REGS1);
}

/**
 * @pred system_module( + _Mod_)
 *
 * @param module
 *
 * @return
 */
static Int is_system_module(USES_REGS1) {
  Term t;
  if (IsVarTerm(t = Deref(ARG1))) {
    return false;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "load_files/2");
    return false;
  }
  return Yap_isSystemModule(t);
}

static Int new_system_module(USES_REGS1) {
  ModEntry *me;
  Term t;
  if (IsVarTerm(t = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, NULL);
    return false;
  }
  if ((me = LookupSystemModule(t)))
    me->OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  return me != NULL;
}

static Int strip_module(USES_REGS1) {
  Term t1 = Deref(ARG1), tmod = CurrentModule;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  t1 = Yap_StripModule(t1, &tmod);
  if (!t1) {
    Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
    return FALSE;
  }
  return Yap_unify(ARG3, t1) && Yap_unify(ARG2, tmod);
}

static Int yap_strip_clause(USES_REGS1) {
  Term t1 = Deref(ARG1), tmod = LOCAL_SourceModule;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  t1 = Yap_StripModule(t1, &tmod);
  if (IsVarTerm(t1) || IsVarTerm(tmod)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "trying to obtain module");
    return false;
  } else if (IsApplTerm(t1)) {
    Functor f = FunctorOfTerm(t1);
    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
      return false;
    }
    if (f == FunctorAssert || f == FunctorDoubleArrow) {
      Term thmod = tmod;
      Term th = ArgOfTerm(1, t1);
      th = Yap_StripModule(th, &thmod);
      if (IsVarTerm(th)) {
        Yap_Error(INSTANTIATION_ERROR, t1, "trying to obtain module");
        return false;
      } else if (IsVarTerm(thmod)) {
        Yap_Error(INSTANTIATION_ERROR, thmod, "trying to obtain module");
        return false;
      } else if (IsIntTerm(th) ||
                 (IsApplTerm(th) && IsExtensionFunctor(FunctorOfTerm(t1)))) {
        Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
        return false;
      } else if (!IsAtomTerm(thmod)) {
        Yap_Error(TYPE_ERROR_ATOM, thmod, "trying to obtain module");
        return false;
      }
    }

  } else if (IsIntTerm(t1) || IsIntTerm(tmod)) {
    Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
    return false;
  }
  return Yap_unify(ARG3, t1) && Yap_unify(ARG2, tmod);
}

Term Yap_YapStripModule(Term t, Term *modp) {
  CACHE_REGS
  Term tmod;

  if (modp) {
    tmod = *modp;
    if (tmod == PROLOG_MODULE) {
      *modp = tmod = TermProlog;
    }
  } else {
    tmod = CurrentModule;
    if (tmod == PROLOG_MODULE) {
      tmod = TermProlog;
    }
  }
restart:
  if (IsVarTerm(t) || !IsApplTerm(t)) {
    if (modp)
      *modp = tmod;
    return t;
  } else {
    Functor fun = FunctorOfTerm(t);
    if (fun == FunctorModule) {
      Term t1 = ArgOfTerm(1, t);
      tmod = t1;
      if (!IsVarTerm(tmod) && !IsAtomTerm(tmod)) {
        if (modp)
          *modp = tmod;
        return t;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    if (modp)
      *modp = tmod;
    return t;
  }
  return 0L;
}

static Int yap_strip_module(USES_REGS1) {
  Term t1 = Deref(ARG1), tmod = CurrentModule;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  t1 = Yap_YapStripModule(t1, &tmod);
  if (!t1 || (!IsVarTerm(tmod) && !IsAtomTerm(tmod))) {
    Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
    return FALSE;
  }
  return Yap_unify(ARG3, t1) && Yap_unify(ARG2, tmod);
}

static Int context_module(USES_REGS1) {
  yamop *parentcp = P;
  CELL *yenv;
  PredEntry *ap = EnvPreg(parentcp);
  if (ap->ModuleOfPred && !(ap->PredFlags & MetaPredFlag))
    return Yap_unify(ARG1, ap->ModuleOfPred);
  parentcp = CP;
  yenv = ENV;
  do {
    ap = EnvPreg(parentcp);
    if (ap->ModuleOfPred && !(ap->PredFlags & MetaPredFlag))
      return Yap_unify(ARG1, ap->ModuleOfPred);
    parentcp = (yamop *)yenv[E_CP];
    yenv = (CELL *)yenv[E_E];
  } while (yenv);
  return Yap_unify(ARG1, CurrentModule);
}

/**
 * @pred source_module(-Mod)
 *
 * @param Mod is the current text source module.
 *
 *  : _Mod_ is the current read-in or source module.
 */
static Int source_module(USES_REGS1) {
  if (LOCAL_SourceModule == PROLOG_MODULE) {
    return Yap_unify(ARG1, TermProlog);
  }
  return Yap_unify(ARG1, LOCAL_SourceModule);
}

/**
 * @pred source_module(-Mod)
 *
 * @param Mod is the current text source module.
 *
 *  : _Mod_ is the current read-in or source module.
 */
static Int current_source_module(USES_REGS1) {
  Term t;
  if (LOCAL_SourceModule == PROLOG_MODULE) {
    LOCAL_SourceModule = TermProlog;
  }
  if (!Yap_unify(ARG1, LOCAL_SourceModule)) {
    return false;
  };
  if (IsVarTerm(t = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return false;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, NULL);
    return false;
  }
  LOCAL_SourceModule = CurrentModule = t;
  return true;
}

/**
 * @pred $copy_operators(+Mode, +ModTarget)
 *
 * Copy all operators in ModSource to ModTarget
 *
 *  : _Mod_ is the current read-in or source module.
 */
static Int copy_operators(USES_REGS1) {
  ModEntry *me = LookupModule(Deref(ARG1));
  if (!me)
    return true;
  ModEntry *she = LookupModule(Deref(ARG2));
  if (!she)
    return true;
  OpEntry *op = me->OpForME;
  while (op) {
    if (!Yap_dup_op(op, she)) {
      return false;
    }
    op = op->NextForME;
  }
  return true;
}

Term Yap_StripModule(Term t, Term *modp) {
  CACHE_REGS
  Term tmod;

  if (modp)
    tmod = *modp;
  else {
    tmod = CurrentModule;
    if (tmod == PROLOG_MODULE) {
      tmod = TermProlog;
    }
  }
restart:
  if (IsVarTerm(t) || !IsApplTerm(t)) {
    if (modp)
      *modp = tmod;
    return t;
  } else {
    Functor fun = FunctorOfTerm(t);
    if (fun == FunctorModule) {
      Term t1 = ArgOfTerm(1, t);
      if (IsVarTerm(t1)) {
        *modp = tmod;
        return t;
      }
      tmod = t1;
      if (!IsVarTerm(tmod) && !IsAtomTerm(tmod)) {
        return 0L;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    if (modp)
      *modp = tmod;
    return t;
  }
  return 0L;
}

void Yap_InitModulesC(void) {
  Yap_InitCPred("$current_module", 2, current_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$current_module", 1, current_module1,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$change_module", 1, change_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("strip_module", 3, strip_module, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$yap_strip_module", 3, yap_strip_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("source_module", 1, source_module, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("current_source_module", 2, current_source_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$yap_strip_clause", 3, yap_strip_clause,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("context_module", 1, context_module, 0);
  Yap_InitCPred("$is_system_module", 1, is_system_module, SafePredFlag);
  Yap_InitCPred("$copy_operators", 2, copy_operators, 0);
  Yap_InitCPred("new_system_module", 1, new_system_module, SafePredFlag);
  Yap_InitCPredBack("$all_current_modules", 1, 1, init_current_module,
                    cont_current_module, SafePredFlag | SyncPredFlag);
  Yap_InitCPredBack("$ground_module", 3, 1, init_ground_module,
                    cont_ground_module, SafePredFlag | SyncPredFlag);
}

void Yap_InitModules(void) {
  CACHE_REGS
  CurrentModules = NULL;
  LookupSystemModule(MkAtomTerm(AtomProlog));
  LOCAL_SourceModule = MkAtomTerm(AtomProlog);
  LookupModule(USER_MODULE);
  LookupModule(IDB_MODULE);
  LookupModule(ATTRIBUTES_MODULE);
  LookupSystemModule(CHARSIO_MODULE);
  LookupSystemModule(TERMS_MODULE);
  LookupSystemModule(SYSTEM_MODULE);
  LookupSystemModule(READUTIL_MODULE);
  LookupSystemModule(HACKS_MODULE);
  LookupModule(ARG_MODULE);
  LookupSystemModule(GLOBALS_MODULE);
  LookupSystemModule(DBLOAD_MODULE);
  LookupSystemModule(RANGE_MODULE);
  CurrentModule = PROLOG_MODULE;
}
