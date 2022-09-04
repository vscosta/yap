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

const char *Yap_CurrentModuleName(void) {
  Term m =  CurrentModule  ? CurrentModule : TermProlog;
  return RepAtom(AtomOfTerm(m))->StrOfAE;
}


/**
 * initialize module data-structure
 *
 * @param to parent module (CurrentModule)
 * @param ae module name.
 *
 * @return a new module structure
 */ /**               */
static ModEntry *initMod(UInt inherit, AtomEntry *ae) {
  CACHE_REGS
  ModEntry *n;

  n = (ModEntry *)Yap_AllocAtomSpace(sizeof(*n));
  INIT_RWLOCK(n->ModRWLock);
  n->KindOfPE = ModProperty;
  n->PredForME = NULL;
  n->OpForME = NULL;
  n->NextME = CurrentModules;
  CurrentModules = n;
  n->AtomOfME = ae;
  n->NextOfPE = NULL;
  n->flags = inherit;
  if (ae == AtomProlog || GLOBAL_Stream == NULL)
    n->OwnerFile = AtomUserIn;
  else
    n->OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  AddPropToAtom(ae, (PropEntry *)n);
  return n;
}

static ModEntry *initTermMod(Term t, UInt flags)
{
  if (t == PROLOG_MODULE) t = TermProlog;
  return initMod(flags, RepAtom(AtomOfTerm(t)));
}

/// This routine assumes a lock is held on the atom a,
/// that a holds the name of the module.
/// and that inheit are the module specific properties.
static ModEntry *
GetModule( AtomEntry *ae)
{
  Prop p0 = ae->PropsOfAE;
  while (p0) {
    ModEntry *me = RepModProp(p0);
    if (me->KindOfPE == ModProperty) {
      return me;
    }
    p0 = me->NextOfPE;
  }
  return NULL;
}

static ModEntry *
LookupModule( Term a)
{
  if (!a) a=TermProlog;
  return  GetModule(RepAtom(AtomOfTerm(a)));
}

static UInt
module_Flags(Term at){
  CACHE_REGS
  Atom parent = AtomOfTerm(at);
    if (parent == NULL || at == TermProlog || CurrentModule == PROLOG_MODULE) {
      return M_SYSTEM | UNKNOWN_ERROR  | DBLQ_CODES |
                 BCKQ_STRING | SNGQ_ATOM;
  } else {
      ModEntry *cme;
      READ_LOCK(RepAtom(parent)->ARWLock);
      cme = GetModule( RepAtom(parent));
      if  (!cme)
	return 0;
      UInt flags = cme->flags;
      READ_UNLOCK(RepAtom(parent)->ARWLock);
      return flags;
    }
}
 

/**
 * get predicate entry for ap/arity; create it if neccessary
 *
 * @param[in] at
 *
 * @return module descriptorxs
 */ 
ModEntry *Yap_GetModuleEntry(Term at) {
  CACHE_REGS
  UInt inherit ;
  ModEntry *me;
  Term parent;
  if (at==0) at =  TermProlog;
  Atom a = AtomOfTerm(at);
  READ_LOCK(RepAtom(a)->ARWLock);
  me = GetModule( RepAtom(a));
  READ_UNLOCK(RepAtom(a)->ARWLock);
  if (me)
    return me;
  parent =  (CurrentModule == PROLOG_MODULE ? TermProlog : CurrentModule);
  if ( GetModule( RepAtom(AtomOfTerm( parent))))
  inherit = Yap_GetModuleEntry(parent)->flags;
  else
    inherit = UNKNOWN_ERROR | M_CHARESCAPE | DBLQ_CODES |
                 BCKQ_STRING | SNGQ_ATOM;
  WRITE_LOCK(RepAtom(a)->ARWLock);
#if THREADS
  me = LookupModule(at);
#endif
  if (!me) {
  me = initMod( inherit, a);
  }
  WRITE_UNLOCK(RepAtom(a)->ARWLock);

  return me;
}

ModEntry *Yap_GetModuleEntry_HoldingLock(Term at) {
  UInt inherit ;
  ModEntry *me;
  Term parent;
  CACHE_REGS
  if (at==0) at =  TermProlog;
  Atom a = AtomOfTerm(at);
  me = LookupModule(at);
  if (me)
    return me;
  parent =  CurrentModule == PROLOG_MODULE ? TermProlog : CurrentModule;
  if (at == parent)
    inherit = false;
  else
    inherit = module_Flags(parent);
  me = initMod( inherit, a);
  return me;
}

/**
 * get predicate entry for ap/arity; create it if neccessary
 *
 * @param[in] at
 *
 * @return module descriptorxs
 */ 
ModEntry *Yap_AddOpToModuleEntry(Atom at, OpEntry * info USES_REGS) {
  WRITE_LOCK(RepAtom(at)->ARWLock);
  ModEntry *me =   Yap_GetModuleEntry_HoldingLock(MkAtomTerm(at));
  if (me && info) {
    info->NextForME = me->OpForME;
    me->OpForME = info;
  }
  WRITE_UNLOCK(RepAtom(at)->ARWLock);
  return me;
}

void Yap_NewModulePred(struct pred_entry *ap) {

  Term mod = ap->ModuleOfPred;
  if (mod ==0) mod = TermProlog;
  WRITE_LOCK(RepAtom(AtomOfTerm(mod))->ARWLock);
  ModEntry *me =   Yap_GetModuleEntry_HoldingLock(mod);		    
  WRITE_UNLOCK( RepAtom(AtomOfTerm(mod) )->ARWLock);
  if (me) {
  ap->NextPredOfModule =  me->PredForME;
  me->PredForME = ap;
  }
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
  ModEntry *m = Yap_GetModuleEntry(mod);
  return Yap_getUnknownModule(m);
}

bool Yap_CharacterEscapes(Term mt) {
  
  if (mt == PROLOG_MODULE)
    mt = TermProlog;
  return Yap_GetModuleEntry(mt)->flags & M_CHARESCAPE;
}

#define ByteAdr(X) ((char *)&(X))
Term Yap_Module_Name(PredEntry *ap) {

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

bool Yap_isSystemModule(Term a) {
  ModEntry *me = Yap_GetModuleEntry(a);
  return me != NULL && me->flags & M_SYSTEM;
}

bool Yap_isSystemModule_HoldingLock(Term a, AtomEntry *ae) {
  ModEntry *me;
  if (a==0) a = TermProlog;
  if (ae == RepAtom(AtomOfTerm(a)))
    me = Yap_GetModuleEntry_HoldingLock(a);
  else
    me = Yap_GetModuleEntry(a);
  return me != NULL && me->flags & M_SYSTEM;
}

Term Yap_Module(Term tmod) {
  Yap_GetModuleEntry(tmod);
  return tmod;
}


Term Yap_GetModuleFromEntry(ModEntry *me) {
  return MkAtomTerm(me->AtomOfME);
  ;
}

struct pred_entry *Yap_ModulePred(Term mod) {
  ModEntry *me;
  if (!(me = Yap_GetModuleEntry(mod)))
    return NULL;
  return me ->PredForME;
}


void Yap_NewModulePred_HoldingLock ( struct pred_entry *ap) {
  Term mod = ap->ModuleOfPred;
  if (mod == 0)
    mod = TermProlog;
  if (mod != MkAtomTerm( (Atom)ap->FunctorOfPred))
    WRITE_LOCK(RepAtom(AtomOfTerm(mod))->ARWLock);
  ModEntry *me =   Yap_GetModuleEntry_HoldingLock(mod);
  if (mod != MkAtomTerm( (Atom)ap->FunctorOfPred))
    WRITE_UNLOCK(RepAtom(AtomOfTerm(mod))->ARWLock);
  if (me) {
  ap->NextPredOfModule = me->PredForME;
  me->PredForME = ap;
  }
}

void Yap_RemovePredFromModule( struct pred_entry *ap) {
  Term mod = ap->ModuleOfPred;
  ModEntry *me;
    if (mod == 0)
        mod = TermProlog;
    if (!(me = GetModule(  RepAtom(AtomOfTerm(mod)))))
        return;
    WRITE_LOCK(AtomOfTerm(mod)->ARWLock);
    PredEntry **o = &me->PredForME, *p = me->PredForME;
    while (p && p != ap) {
        o = &(p->NextPredOfModule);
        p = p->NextPredOfModule;
    }
    if (p) *o = p->NextPredOfModule;
    ap->NextPredOfModule = NULL;
    WRITE_UNLOCK(AtomOfTerm(mod)->ARWLock);
}


//
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
    GetModule(RepAtom(AtomOfTerm(t)));
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
static Int set_source_module(USES_REGS1) { /* $change_module(N)		 */
  Term mod;
  if (!Yap_unify(ARG1,(LOCAL_SourceModule? LOCAL_SourceModule:TermProlog)))
    return false;
  mod =Deref(ARG2);
  CurrentModule = mod;
  LOCAL_SourceModule = mod;
  return true;
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
    if (Yap_GetModuleEntry(t) != NULL)
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
    if (Yap_GetModuleEntry(tmod) != NULL && Yap_unify(tmod, ARG2) &&
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
  me = Yap_GetModuleEntry( t );
  me->flags = module_Flags(TermProlog);
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
  Term t1 = Deref(ARG1), th, tbody, tmod = LOCAL_SourceModule, thmod;
  if (tmod == PROLOG_MODULE) {
    tmod = TermProlog;
  }
  t1 = Yap_StripModule(t1, &tmod);
  thmod=tmod;
  if (IsVarTerm(t1) || IsVarTerm(tmod)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "trying to obtain module");
    return false;
  } else if (IsApplTerm(t1)) {
    Functor f = FunctorOfTerm(t1);
    if (IsExtensionFunctor(f)) {
      Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
      return false;
    }
    if (f == FunctorAssert ) {
       thmod = tmod;
       th = ArgOfTerm(1, t1);
       tbody = ArgOfTerm(2, t1);
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
    } else {
    th = t1;
    thmod = tmod;
      tbody = TermTrue;
    }
  } else if (IsIntTerm(t1) || IsIntTerm(tmod)) {
    Yap_Error(TYPE_ERROR_CALLABLE, t1, "trying to obtain module");
    return false;
  } else {
    th = t1;
    thmod = tmod;
    tbody = TermTrue;
  }
    return Yap_unify(ARG4, th) && Yap_unify(ARG2, tmod)
      && Yap_unify(ARG3,thmod)
      && Yap_unify(ARG5,tbody);
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
      tmod = CurrentModule = TermProlog;
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
  if (CurrentModule)
    return Yap_unify(ARG1, CurrentModule);
  else
        return Yap_unify(ARG1, TermProlog);
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
  Yap_InitCPred("set_source_module", 2, set_source_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$change_module", 1, change_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("strip_module", 3, strip_module, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$yap_strip_module", 3, yap_strip_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("source_module", 1, source_module, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("current_source_module", 2, current_source_module,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$yap_strip_clause", 5, yap_strip_clause,
                SafePredFlag);
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
  UInt ifl =  UNKNOWN_ERROR | M_CHARESCAPE | DBLQ_CODES |
                 BCKQ_STRING | SNGQ_ATOM;
  initTermMod(TermProlog, ifl|M_SYSTEM);
  initTermMod(USER_MODULE, ifl);
  initTermMod(ATTRIBUTES_MODULE, ifl|M_SYSTEM);
  initTermMod(HACKS_MODULE, ifl|M_SYSTEM);
  initTermMod(IDB_MODULE, ifl|M_SYSTEM);
  initTermMod(TERMS_MODULE, ifl|M_SYSTEM);
  initTermMod(CHARSIO_MODULE, ifl|M_SYSTEM);
  initTermMod(SYSTEM_MODULE, ifl|M_SYSTEM);
  initTermMod(ARG_MODULE, ifl);
  initTermMod(DBLOAD_MODULE, ifl);
  initTermMod(GLOBALS_MODULE, ifl);
  initTermMod(RANGE_MODULE, ifl);
  initTermMod (READUTIL_MODULE, ifl);
  CurrentModule = PROLOG_MODULE;
  LOCAL_SourceModule = PROLOG_MODULE;
}
