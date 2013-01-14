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
* File:		load_dld.c						 *
* comments:	dld based dynamic loaderr of external routines		 *
*               tested on i486-linuxaout				 *
*************************************************************************/

#if defined(linux) && !defined(__ELF__) && !defined(__LCC__)

#include "Foreign.h"
#include <dld.h>
#include <malloc.h>
#include <stdio.h>

this code is no being maintained anymore

/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
char *
Yap_FindExecutable(void)
{
  /* use dld_find_executable */
  char *res;
  if(name != NULL && (res=dld_find_executable(name))) {
    return GLOBAL_Executable;
  } else {
    return "yap";
  }
}


static void *
Yap_LoadForeignFile(char *file, int flags)
{
  /* not implemented */
  return NULL;
}

int
Yap_CallForeignFile(void *handle, char *f)
{
  return FALSE;
}

int
Yap_CloseForeignFile(void *handle)
{
  return -1;
}

/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
static int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  static int firstTime = 1;
  int error;
  
  if(firstTime) {
    error = dld_init(GLOBAL_Executable);
    if(error) {
      strcpy(LOCAL_ErrorSay,dld_strerror(error));
      return LOAD_FAILLED;
    }
    firstTime=0;
  }

  while (ofiles) {
    if((error=dld_link(AtomName(ofiles->name))) !=0) {
      strcpy(LOCAL_ErrorSay,dld_strerror(error));
      return LOAD_FAILLED;
    }
    ofiles = ofiles->next;
  }


  /* TODO: handle libs */
  *init_proc = (YapInitProc) dld_get_func(proc_name);
  if(! *init_proc) {
    strcpy(LOCAL_ErrorSay,"Could not locate initialization routine");
    return LOAD_FAILLED;
  }
  if(!dld_function_executable_p(proc_name)) {
    char **undefs = dld_list_undefined_sym();
    char **p = undefs;
    int k = dld_undefined_sym_count;
    strcpy(LOCAL_ErrorSay,"Could not resolve all symbols");
    while(k) {
      YP_printf("[undefined symbol %s]\n",*p++);
      --k;
    }
    free(undefs);
    return LOAD_FAILLED;
  }

  return LOAD_SUCCEEDED;
}

Int
Yap_LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return LoadForeign(ofiles, libs, proc_name, init_proc);
}

void 
Yap_ShutdownLoadForeign(void)
{
}

Int
Yap_ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}

#endif
