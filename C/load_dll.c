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
* File:		load_dl.c						 *
* comments:	dl based dynamic loader  of external routines		 *
*               tested on i486-linuxelf					 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "Foreign.h"

#if LOAD_DLL

#include <windows.h>

/*
 *   YAPFindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
void
YAPFindExecutable(char *name)
{
  /* not really needed for dl version */
  strcpy(YapExecutable,"yap");
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
Int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{

  while (ofiles) {
    HINSTANCE handle;

    if (TrueFileName(ofiles->s, FileNameBuf, TRUE) &&
	(handle=LoadLibrary(FileNameBuf)) != 0)
	{
	  if (*init_proc == NULL)
	    *init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name);
	}
    ofiles = ofiles->next;
  }

  /* load libraries first so that their symbols are available to
     other routines */
  while (libs) {
    HINSTANCE handle;

    if (libs->s[0] == '-') {
      strcat(FileNameBuf,libs->s+2);
      strcat(FileNameBuf,".dll");
    } else {
      strcpy(FileNameBuf,libs->s);
    }

    if((handle=LoadLibrary(FileNameBuf)) == 0)
    {
/*      strcpy(LoadMsg,dlerror());*/
      return LOAD_FAILLED;
    }

    if (*init_proc == NULL)
      *init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name);

    libs = libs->next;
  }

  if(*init_proc == NULL) {
    strcpy(LoadMsg,"Could not locate initialization routine");
    return LOAD_FAILLED;
  }

  return LOAD_SUCCEEDED;
}

void 
ShutdownLoadForeign(void)
{
}

Int
ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}

#endif

