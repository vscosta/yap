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
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
void
_YAP_FindExecutable(char *name)
{
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
static Int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{

  while (ofiles) {
    HINSTANCE handle;

    if (_YAP_TrueFileName(ofiles->s, _YAP_FileNameBuf, TRUE) &&
	(handle=LoadLibrary(_YAP_FileNameBuf)) != 0)
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
      strcat(_YAP_FileNameBuf,libs->s+2);
      strcat(_YAP_FileNameBuf,".dll");
    } else {
      strcpy(_YAP_FileNameBuf,libs->s);
    }

    if((handle=LoadLibrary(_YAP_FileNameBuf)) == 0)
    {
/*      strcpy(_YAP_ErrorSay,dlerror());*/
      return LOAD_FAILLED;
    }

    if (*init_proc == NULL)
      *init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name);

    libs = libs->next;
  }

  if(*init_proc == NULL) {
    strcpy(_YAP_ErrorSay,"Could not locate initialization routine");
    return LOAD_FAILLED;
  }

  return LOAD_SUCCEEDED;
}

Int
_YAP_LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return LoadForeign(ofiles, libs, proc_name, init_proc);
}

void 
_YAP_ShutdownLoadForeign(void)
{
}

Int
_YAP_ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}

#endif

