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
 * File:		load_foreign.c					 *
 * comments:	dynamic loader of external routines			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%.2";
#endif


#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "pl-shared.h"
#include "YapText.h"
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#include "Foreign.h"

#if _WIN32 || defined(__CYGWIN__)
#ifndef SO_EXT
#define SO_EXT "dll"
#endif
#endif

Int p_load_foreign( USES_REGS1 );

Int
p_load_foreign( USES_REGS1 )
{
  StringList ofiles = NULL;
  StringList libs = NULL;
  char *InitProcName;
  YapInitProc InitProc = NULL;
  Term t, t1;
  StringList new;
  Int returncode = FALSE;

  strcpy(LOCAL_ErrorSay,"Invalid arguments");

  /* collect the list of object files */
  t = Deref(ARG1);
  while(1) {
    if (t == TermNil) break;
    t1 = HeadOfTerm(t);
    t = TailOfTerm(t);
    new = (StringList) Yap_AllocCodeSpace(sizeof(StringListItem));
    new->next = ofiles;
    new->name = AtomOfTerm(t1);
    ofiles = new;
  }

  /* collect the list of library files */
  t = Deref(ARG2);
  while(1) {
    if (t == TermNil) break;
    t1 = HeadOfTerm(t);
    t = TailOfTerm(t);
    new = (StringList) Yap_AllocCodeSpace(sizeof(StringListItem));
    new->next = libs;
    new->name = AtomOfTerm(t1);
    libs = new;
  }

  /* get the initialization function name */
  t1 = Deref(ARG3);
  InitProcName = RepAtom(AtomOfTerm(t1))->StrOfAE;

  
  
  /* call the OS specific function for dynamic loading */
  if(Yap_LoadForeign(ofiles,libs,InitProcName,&InitProc)==LOAD_SUCCEEDED) {
    Int CurSlot =   Yap_StartSlots( PASS_REGS1 );
    (*InitProc)();
    LOCAL_CurSlot = CurSlot;
    returncode = TRUE;
  }
  
  /* I should recover space if load foreign fails */
  if (returncode == TRUE) {
    ForeignObj *f_code = (ForeignObj *)Yap_AllocCodeSpace(sizeof(ForeignObj));
    f_code->objs = ofiles;
    f_code->libs = libs;
    f_code->f = AtomOfTerm(t1);
    f_code->next = ForeignCodeLoaded;
    f_code->module = CurrentModule;
    ForeignCodeLoaded = f_code;
  } else {
    while (ofiles) {
      new = ofiles->next;
      Yap_FreeCodeSpace((ADDR)ofiles);
      ofiles = new;
    }
    while (libs) {
      new = libs->next;
      Yap_FreeCodeSpace((ADDR)libs);
      libs = new;
    }
  }
  return returncode;
}

static Int
p_open_shared_object( USES_REGS1 ) {
  Term t = Deref(ARG1);
  Term tflags = Deref(ARG2);
  char *s;
  void *handle;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"open_shared_object/3");
    return FALSE;
  } 
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"open_shared_object/3");
    return FALSE;
  }
  
  if (IsVarTerm(tflags)) {
    Yap_Error(INSTANTIATION_ERROR,tflags,"open_shared_object/3");
    return FALSE;
  } 
  if (!IsIntegerTerm(tflags)) {
    Yap_Error(TYPE_ERROR_INTEGER,tflags,"open_shared_object/3");
    return FALSE;
  }
  
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if ((handle = Yap_LoadForeignFile(s, IntegerOfTerm(tflags)))==NULL) {
    Yap_Error(EXISTENCE_ERROR_SOURCE_SINK,t,"open_shared_object_failed for %s with %s\n", s, LOCAL_ErrorSay);
    return FALSE;
  } else {
    return Yap_unify(MkIntegerTerm((Int)handle),ARG3);
  }
}

static Int
p_close_shared_object( USES_REGS1 ) {
  Term t = Deref(ARG1);
  void *handle;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"close_shared_object/1");
    return FALSE;
  } 
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER,t,"close_shared_object/1");
    return FALSE;
  }
  handle = (char *)IntegerOfTerm(t);
 
  return Yap_CloseForeignFile(handle);
}

static Int
p_call_shared_object_function( USES_REGS1 ) {
  Term t = Deref(ARG1);
  Term tfunc = Deref(ARG2);
  Term tmod;
  void *handle;
  Term OldCurrentModule = CurrentModule;
  Int res;

  tmod = CurrentModule;
 restart:
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR,t,"call_shared_object_function/2");
    return FALSE;
  } else if (IsApplTerm(t)) {
    Functor    fun = FunctorOfTerm(t);
    if (fun == FunctorModule) {
      tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod) ) {
	Yap_Error(INSTANTIATION_ERROR,t,"call_shared_object_function/2");
	return FALSE;
      }
      if (!IsAtomTerm(tmod) ) {
	Yap_Error(TYPE_ERROR_ATOM,ARG1,"call_shared_object_function/2");
	return FALSE;
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
  } else if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_INTEGER,t,"call_shared_object_function/2");
    return FALSE;
  }
  handle = (void *)IntegerOfTerm(t);
  if (IsVarTerm(tfunc)) {
    Yap_Error(INSTANTIATION_ERROR,t,"call_shared_object_function/2");
    return FALSE;
  } 
  if (!IsAtomTerm(tfunc)) {
    Yap_Error(TYPE_ERROR_ATOM,t,"call_shared_object_function/2/3");
    return FALSE;
  }
  CurrentModule = tmod;
  res = Yap_CallForeignFile(handle, RepAtom(AtomOfTerm(tfunc))->StrOfAE);
  CurrentModule = OldCurrentModule;
  return res;
}

static Int
p_obj_suffix( USES_REGS1 ) {
  return Yap_unify(Yap_CharsToListOfCodes(SO_EXT PASS_REGS),ARG1);
}

static Int
p_open_shared_objects( USES_REGS1 ) {
#ifdef SO_EXT
  return TRUE;
#else
  return FALSE;
#endif
}

void
Yap_InitLoadForeign( void )
{
  Yap_InitCPred("$load_foreign_files", 3, p_load_foreign, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$open_shared_objects", 0, p_open_shared_objects, SafePredFlag);
  Yap_InitCPred("$open_shared_object", 3, p_open_shared_object, SyncPredFlag);
  Yap_InitCPred("close_shared_object", 1, p_close_shared_object, SyncPredFlag|SafePredFlag);
/** @pred close_shared_object(+ _Handle_) 

Detach the shared object identified by  _Handle_. 

 
*/
  Yap_InitCPred("$call_shared_object_function", 2, p_call_shared_object_function, SyncPredFlag);
  Yap_InitCPred("$obj_suffix", 1, p_obj_suffix, SafePredFlag);
}

void 
Yap_ReOpenLoadForeign(void)
{
  CACHE_REGS
  ForeignObj *f_code = ForeignCodeLoaded;
  Term OldModule = CurrentModule;

  while (f_code != NULL) {
    YapInitProc InitProc = NULL;

    CurrentModule = f_code->module;
    if(Yap_ReLoadForeign(f_code->objs,f_code->libs,RepAtom(f_code->f)->StrOfAE,&InitProc)==LOAD_SUCCEEDED) {
      if (InitProc)
	(*InitProc)();
    }
    f_code = f_code->next;
  }
  CurrentModule = OldModule;
}








