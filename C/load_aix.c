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
* comments:	dl based dynamic loaderr of external routines		 *
*               tested on i486-linuxelf					 *
*************************************************************************/

#ifdef _AIX

#include "Yap.h"
#include "Yatom.h"
#include "yapio.h"
#include "Foreign.h"
#include <stdio.h>
#include <errno.h>

/*
 *   FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
char *
Yap_FindExecutable(void)
{
  return NULL;
}


void *
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
static Int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{

  extern char *sys_errlist[ ];

  /* load wants to follow the LIBRARY_PATH */
  if (ofiles->next != NULL || libs != NULL) {
    strcpy(LOCAL_ErrorSay," Load Failed: in AIX you must load a single object file");
    return LOAD_FAILLED;
  }
  if (!Yap_AbsoluteFileInBuffer(AtomName(ofiles->name), LOCAL_FileNameBuf, YAP_FILENAME_MAX, true)) {
    strcpy(LOCAL_ErrorSay, " Trying to open unexisting file in LoadForeign ");
    return LOAD_FAILLED;
  }
  /* In AIX, just call load and everything will go in */
  if ((*init_proc=((YapInitProc *)load(LOCAL_FileNameBuf,0,NULL))) == NULL) {
    strcpy(LOCAL_ErrorSay,sys_errlist[errno]);
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

#endif /* _AIX */




