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
_YAP_Module_Name(CODEADDR cap)
{
  PredEntry      *ap = (PredEntry *)cap;

  if (!ap->ModuleOfPred)
    /* If the system predicate is a metacall I should return the
       module for the metacall, which I will suppose has to be
       reachable from the current module anyway.

       So I will return the current module in case the system
       predicate is a meta-call. Otherwise it will still work.
    */
    return(ModuleName[CurrentModule]);
  else {
    return (ModuleName[ap->ModuleOfPred]);
  }
}

static SMALLUNSGN 
LookupModule(Term a)
{
  unsigned int             i;
	
  for (i = 0; i < NoOfModules; ++i) {
    if (ModuleName[i] == a) {       
      return (i);
    }
  }
  ModuleName[i = NoOfModules++] = a;
  if (NoOfModules == MaxModules) {
    _YAP_Error(SYSTEM_ERROR,a,"number of modules overflowed");
  }
  return (i);
}

SMALLUNSGN 
_YAP_LookupModule(Term a)
{
  return(LookupModule(a));
}

static Int 
p_current_module(void)
{				/* $current_module(Old,New)		 */
  Term            t;
	
  if (!_YAP_unify_constant(ARG1, ModuleName[CurrentModule]))
    return (0);
  t = Deref(ARG2);
  if (IsVarTerm(t) || !IsAtomTerm(t))
    return (0);
  CurrentModule = LookupModule(t);
  return (TRUE);
}

static Int
p_current_module1(void)
{				/* $current_module(Old)		 */
  if (!_YAP_unify_constant(ARG1, ModuleName[CurrentModule]))
    return (0);
  return (1);
}

static Int
p_change_module(void)
{				/* $change_module(New)		 */
  SMALLUNSGN mod = LookupModule(Deref(ARG1));
  CurrentModule = mod;
  return (TRUE);
}

static Int
p_module_number(void)
{				/* $module_number(Mod,Num)		 */
  Term tname = Deref(ARG1);
  Term t;
  if (IsVarTerm(tname)) {
    return(_YAP_unify(tname, ModuleName[IntOfTerm(Deref(ARG2))]));
  }else {
    t = MkIntTerm(LookupModule(Deref(ARG1)));
    _YAP_unify(t,ARG2);
    ARG2 = t;
  }
  return(TRUE);
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
  return(_YAP_unify(ARG1,t));
}

static Int 
init_current_module(void)
{				/* current_module(?ModuleName)		 */
  EXTRA_CBACK_ARG(1,1) = MkIntTerm(0);
  return (cont_current_module());
}

void 
_YAP_InitModules(void)
{
  ModuleName[PrimitivesModule = 0] =
    MkAtomTerm(_YAP_LookupAtom("prolog"));
  ModuleName[1] =
    MkAtomTerm(_YAP_LookupAtom("user"));
  NoOfModules = 2;
  CurrentModule = 0;
  _YAP_InitCPred("$current_module", 2, p_current_module, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$current_module", 1, p_current_module1, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$change_module", 1, p_change_module, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$module_number", 2, p_module_number, SafePredFlag);
  _YAP_InitCPredBack("$all_current_modules", 1, 1, init_current_module, cont_current_module,
		SafePredFlag|SyncPredFlag);
}
