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
#include "Heap.h"

STATIC_PROTO(Int p_current_module, (void));
STATIC_PROTO(Int p_current_module1, (void));

#define ByteAdr(X) ((char *) &(X))
Term 
Yap_Module_Name(PredEntry *ap)
{
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

static Term 
LookupModule(Term a)
{
  unsigned int             i;

  for (i = 0; i < NoOfModules; ++i) {
    if (ModuleName[i] == a) {       
      return i;
    }
  }
  ModuleName[i = NoOfModules++] = a;
  if (NoOfModules == MaxModules) {
    Yap_Error(SYSTEM_ERROR,a,"number of modules overflowed");
  }
  return (i);
}

struct pred_entry *
Yap_ModulePred(Term mod)
{
  return ModulePred[LookupModule(mod)];
}

void
Yap_NewModulePred(Term mod, struct pred_entry *ap)
{
  Term imod =  LookupModule(mod);
  ap->NextPredOfModule = ModulePred[imod];
  ModulePred[imod] = ap;
}

static Int 
p_current_module(void)
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
    CurrentModule = 0;
  } else {
    CurrentModule = t;
    LookupModule(CurrentModule);
  }
  return (TRUE);
}

static Int
p_current_module1(void)
{				/* $current_module(Old)		 */
  if (CurrentModule)
    return Yap_unify_constant(ARG1, CurrentModule);
  return Yap_unify_constant(ARG1, TermProlog);
}

static Int
p_change_module(void)
{				/* $change_module(New)		 */
  Term mod = Deref(ARG1);
  LookupModule(mod);
  CurrentModule = mod;
  return TRUE;
}

static Int 
cont_current_module(void)
{
  Int             mod = IntOfTerm(EXTRA_CBACK_ARG(1,1));
  Term t = ModuleName[mod];

  if (mod == NoOfModules) {
    cut_fail();
  }
  EXTRA_CBACK_ARG(1,1) = MkIntTerm(mod+1);
  return(Yap_unify(ARG1,t));
}

static Int 
init_current_module(void)
{				/* current_module(?ModuleName)		 */
  EXTRA_CBACK_ARG(1,1) = MkIntTerm(0);
  return (cont_current_module());
}

void 
Yap_InitModulesC(void)
{
  Yap_InitCPred("$current_module", 2, p_current_module, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$current_module", 1, p_current_module1, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$change_module", 1, p_change_module, SafePredFlag|SyncPredFlag);
  Yap_InitCPredBack("$all_current_modules", 1, 1, init_current_module, cont_current_module,
		SafePredFlag|SyncPredFlag);
}


void 
Yap_InitModules(void)
{
  ModuleName[0] =
    TermProlog;
  ModuleName[1] = 
    USER_MODULE;
  ModuleName[2] =
    IDB_MODULE;
  ModuleName[3] =
    ATTRIBUTES_MODULE;
  ModuleName[4] =
    CHARSIO_MODULE;
  ModuleName[5] =
    TERMS_MODULE;
  NoOfModules = 6;
  CurrentModule = PROLOG_MODULE;
}
