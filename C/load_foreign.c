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
#include "Heap.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#include "Foreign.h"

#if _WIN32
#ifndef SHLIB_SUFFIX
#define SHLIB_SUFFIX "dll"
#endif
#endif

char LoadMsg[512];
char YapExecutable[YAP_FILENAME_MAX];

STD_PROTO(Int p_load_foreign, (void));

Int
p_load_foreign(void)
{
  StringList ofiles = NIL;
  StringList libs = NIL;
  char *InitProcName;
  YapInitProc InitProc = NULL;
  Term t, t1;
  StringList new;
  Int returncode = FALSE;

  strcpy(LoadMsg,"Invalid arguments");

  /* collect the list of object files */
  t = Deref(ARG1);
  while(1) {
    if (t == TermNil) break;
    t1 = HeadOfTerm(t);
    t = TailOfTerm(t);
    new = (StringList) AllocCodeSpace(sizeof(StringListItem));
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
    new = (StringList) AllocCodeSpace(sizeof(StringListItem));
    new->next = libs;
    new->s = RepAtom(AtomOfTerm(t1))->StrOfAE;
    libs = new;
  }

  /* get the initialization function name */
  t1 = Deref(ARG3);
  InitProcName = RepAtom(AtomOfTerm(t1))->StrOfAE;

  
  
  /* call the OS specific function for dynamic loading */
  if(LoadForeign(ofiles,libs,InitProcName,&InitProc)==LOAD_SUCCEEDED) {
    (*InitProc)();
    returncode = TRUE;
  }
  
  /* I should recover space if load foreign fails */
  if (returncode == TRUE) {
    ForeignObj *f_code = (ForeignObj *)AllocCodeSpace(sizeof(ForeignObj));
    f_code->objs = ofiles;
    f_code->libs = libs;
    f_code->f = InitProcName;
    f_code->next = ForeignCodeLoaded;
    f_code->module = CurrentModule;
    ForeignCodeLoaded = (void *)f_code;
  }
  return returncode;
}

static Int
p_obj_suffix(void) {
  return(unify(StringToList(SHLIB_SUFFIX),ARG1));
}

void
InitLoadForeign(void)
{
  if (yap_args == NULL)
      YAPFindExecutable(NULL);
    else
      YAPFindExecutable(yap_args[0]);
  InitCPred("$load_foreign_files", 3, p_load_foreign, SafePredFlag|SyncPredFlag);
  InitCPred("$obj_suffix", 1, p_obj_suffix, SafePredFlag);
}

void 
ReOpenLoadForeign(void)
{
  ForeignObj *f_code = ForeignCodeLoaded;
  int OldModule = CurrentModule;
  YapInitProc InitProc = NULL;

  while (f_code != NULL) {
    *CurrentModulePtr = MkIntTerm(f_code->module);
    if(ReLoadForeign(f_code->objs,f_code->libs,f_code->f,&InitProc)==LOAD_SUCCEEDED) {
      (*InitProc)();
    }
    f_code = f_code->next;
  }
  *CurrentModulePtr = MkIntTerm(OldModule);
}








