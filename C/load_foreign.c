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
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#include "Foreign.h"

#if _WIN32 || defined(__CYGWIN__)
#ifndef YAP_SHLIB_SUFFIX
#define YAP_SHLIB_SUFFIX ".dll"
#endif
#endif

STD_PROTO(Int p_load_foreign, (void));

Int
p_load_foreign(void)
{
  StringList ofiles = NULL;
  StringList libs = NULL;
  char *InitProcName;
  YapInitProc InitProc = NULL;
  Term t, t1;
  StringList new;
  Int returncode = FALSE;

  strcpy(Yap_ErrorSay,"Invalid arguments");

  /* collect the list of object files */
  t = Deref(ARG1);
  while(1) {
    if (t == TermNil) break;
    t1 = HeadOfTerm(t);
    t = TailOfTerm(t);
    new = (StringList) Yap_AllocCodeSpace(sizeof(StringListItem));
    new->next = ofiles;
    new->s = RepAtom(AtomOfTerm(t1))->StrOfAE;
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
    new->s = RepAtom(AtomOfTerm(t1))->StrOfAE;
    libs = new;
  }

  /* get the initialization function name */
  t1 = Deref(ARG3);
  InitProcName = RepAtom(AtomOfTerm(t1))->StrOfAE;

  
  
  /* call the OS specific function for dynamic loading */
  if(Yap_LoadForeign(ofiles,libs,InitProcName,&InitProc)==LOAD_SUCCEEDED) {
    (*InitProc)();
    returncode = TRUE;
  }
  
  /* I should recover space if load foreign fails */
  if (returncode == TRUE) {
    ForeignObj *f_code = (ForeignObj *)Yap_AllocCodeSpace(sizeof(ForeignObj));
    f_code->objs = ofiles;
    f_code->libs = libs;
    f_code->f = InitProcName;
    f_code->next = ForeignCodeLoaded;
    f_code->module = CurrentModule;
    ForeignCodeLoaded = (void *)f_code;
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
p_obj_suffix(void) {
  return(Yap_unify(Yap_StringToList(YAP_SHLIB_SUFFIX),ARG1));
}

void
Yap_InitLoadForeign(void)
{
  if (Yap_argv == NULL)
    Yap_FindExecutable("yap");
  else
    Yap_FindExecutable(Yap_argv[0]);
  Yap_InitCPred("$load_foreign_files", 3, p_load_foreign, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$obj_suffix", 1, p_obj_suffix, SafePredFlag|HiddenPredFlag);
}

void 
Yap_ReOpenLoadForeign(void)
{
  ForeignObj *f_code = ForeignCodeLoaded;
  Term OldModule = CurrentModule;
  YapInitProc InitProc = NULL;

  while (f_code != NULL) {
    CurrentModule = f_code->module;
    if(Yap_ReLoadForeign(f_code->objs,f_code->libs,f_code->f,&InitProc)==LOAD_SUCCEEDED) {
      (*InitProc)();
    }
    f_code = f_code->next;
  }
  CurrentModule = OldModule;
}








