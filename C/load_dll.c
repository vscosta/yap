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
#include "YapHeap.h"
#include "yapio.h"
#include "Foreign.h"

#if _WIN32

#include <windows.h>

/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
char *
Yap_FindExecutable(void)
{
  enum { BUFFERSIZE = 1024 };
  char *buf = malloc(BUFFERSIZE);

  if (!GetModuleFileName(NULL, buf, BUFFERSIZE-1))
    return NULL;

  return buf;
}

void *
Yap_LoadForeignFile(char *file, int flags)
{
  char *buf = malloc(1024);
  void *ptr= (void *)LoadLibrary(file);
 if (!ptr) {
   FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		 NULL, GetLastError(),
		 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), buf, 1023,
		 NULL);
		 }
 return ptr;
}

int
Yap_CallForeignFile(void *handle, char *f)
{
  YapInitProc proc = (YapInitProc)GetProcAddress((HMODULE)handle, f);
  if (!proc)
    return FALSE;
  (*proc)();
  return TRUE;
}

int
Yap_CloseForeignFile(void *handle)
{
  return FreeLibrary((HMODULE)handle);
}

/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
static Int
LoadForeign(StringList ofiles, StringList libs,
	       const char *proc_name,	YapInitProc *init_proc)
{
  CACHE_REGS
  while (ofiles) {
    HINSTANCE handle;

    if (*init_proc == NULL &&
      (*init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name)))
       {
         YapInitProc f = *init_proc;
        f();
        return true;
      }

    const char *file = AtomName(ofiles->name);
    if ((file=Yap_AbsoluteFile(file, true)) &&
	(handle=LoadLibrary(file)) != 0)
      {
       LOCAL_ErrorMessage = NULL;
	if (*init_proc == NULL)
	  *init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name);
      } else {
      char *buf = malloc(1024);
   FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		 NULL, GetLastError(),
		 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), buf, 1023,
		 NULL);
	//fprintf(stderr,"WinError: %s\n", LOCAL_ErrorSay);
      }
    ofiles = ofiles->next;
  }
  /* load libraries first so that their symbols are available to
     other routines */
  while (libs) {
    HINSTANCE handle;
    const char * s = AtomName(libs->name);

    if (s[0] == '-') {
      strcat(LOCAL_FileNameBuf,s+2);
      strcat(LOCAL_FileNameBuf,".dll");
    } else {
      strcpy(LOCAL_FileNameBuf,s);
    }

    if((handle=LoadLibrary(LOCAL_FileNameBuf)) == 0)
    {
/*      strcpy(LOCAL_ErrorSay,dlerror());*/
      return LOAD_FAILLED;
    }

    if (*init_proc == NULL)
      *init_proc = (YapInitProc)GetProcAddress((HMODULE)handle, proc_name);

    libs = libs->next;
  }

  if(*init_proc == NULL) {
    LOCAL_ErrorMessage = "Could not locate initialization routine";
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
