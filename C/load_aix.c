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
void
YAPFindExecutable(char *name)
{
  /* not really needed for dl version */
  strcpy(YapExecutable,"./yap");
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
Int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{

  extern char *sys_errlist[ ];

  /* load wants to follow the LIBRARY_PATH */
  if (ofiles->next != NULL || libs != NULL) {
    strcpy(LoadMsg," Load Failed: in AIX you must load a single object file");
    return LOAD_FAILLED;
  }
  if (!TrueFileName(ofiles->s, FileNameBuf, TRUE)) {
    strcpy(LoadMsg, " Trying to open unexisting file in LoadForeign ");
    return LOAD_FAILLED;
  }
  /* In AIX, just call load and everything will go in */
  if ((*init_proc=((YapInitProc *)load(FileNameBuf,0,NULL))) == NULL) {
    strcpy(LoadMsg,sys_errlist[errno]);
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

#endif /* _AIX */




