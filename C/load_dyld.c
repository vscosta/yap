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
* File:		load_dyld.c						 *
* comments:	dyld based dynamic loaderr of external routines		 *
*               tested on MacOS						 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "Foreign.h"

#if LOAD_DYLD

#include <string.h>

/* Code originally  from Rex A. Dieter's posting in comp.sys.next.programmer
   and from dynload_next.c in the Python sources 
*/
#import <mach-o/dyld.h>



static char *
mydlerror(void)
{
  char *errString;
  switch(LOCAL_dl_errno) {
  default:
  case NSObjectFileImageFailure:
  case NSObjectFileImageFormat:
    /* for these a message is printed on stderr by dyld */
    errString = "Can't create object file image";
    break;
  case NSObjectFileImageSuccess:
    errString = NULL;
    break;
  case NSObjectFileImageInappropriateFile:
    errString = "Inappropriate file type for dynamic loading";
    break;
  case NSObjectFileImageArch:
    errString = "Wrong CPU type in object file";
    break;
  case NSObjectFileImageAccess:
    errString = "Can't read object file (no access)";
    break;
  }
  return(errString);
}


/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
char *
Yap_FindExecutable(void)
{
  char path[1024];
  uint32_t size = sizeof(path);
  if (_NSGetExecutablePath(path, &size) == 0) {
    char *rc = malloc(size+1);
    strncpy(rc, path, size);
    return rc;
  } else {
    char *rc = malloc(size+1);
    if (_NSGetExecutablePath(rc, &size) == 0)
      return "yap";
    return rc;
  }
}


static void *
mydlopen(char *path)
{
    int dyld_result;
    NSObjectFileImage ofile;
    NSModule handle = NULL;
    dyld_result = NSCreateObjectFileImageFromFile(path, &ofile);
    if (dyld_result != NSObjectFileImageSuccess) {
      LOCAL_dl_errno = dyld_result;
    } else {
      /* NSLinkModule will cause the run to abort on any link error's */
      /* not very friendly but the error recovery functionality is limited */
        handle = NSLinkModule(ofile, path, TRUE);
    }
    return handle;
}

static void *
mydlsym(char *symbol)
{
    void *addr;
    char funcname[256];

#if HAVE_SNPRINTF
    snprintf(funcname, sizeof(funcname), "_%.200s", symbol);
#else
    sprintf(funcname, "_%.200s", symbol);
#endif
    if (NSIsSymbolNameDefined(funcname))
        addr = NSAddressOfSymbol(NSLookupAndBindSymbol(funcname));
    else
        addr = NULL;
    return addr;
} 

static int
mydlclose(void *handle)
{
  NSUnLinkModule(handle, NSUNLINKMODULE_OPTION_NONE);
  return TRUE;
} 

void *
Yap_LoadForeignFile(char *file, int flags)
{
  return (void *)mydlopen(file);
}

int
Yap_CallForeignFile(void *handle, char *f)
{
  YapInitProc proc = (YapInitProc) mydlsym(f);
  if (!proc)
    return FALSE;
  (*proc)();
  return TRUE;
}

int
Yap_CloseForeignFile(void *handle)
{
  return mydlclose(handle);
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/
static Int
LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  int lvl = push_text_stack();
  while (ofiles) {
    void *handle;

    /* mydlopen wants to follow the LD_CONFIG_PATH */
    iconst char *file = AtomName(ofiles->name);
    if (!(file=Yap_findFile(file, true) )) {
      strcpy(LOCAL_ErrorSay, "%% Trying to open unexisting file in LoadForeign");
      popen_text_stack(lvl);
      return LOAD_FAILLED;
    }
    if((handle=mydlopen(file)) == 0)
    {
      fprintf(stderr,"calling dlopen with error %s\n", mydlerror());
/*      strcpy(LOCAL_ErrorSay,dlerror());*/
      popen_text_stack(lvl);
      return LOAD_FAILLED;
    }

    ofiles->handle = handle;

    ofiles = ofiles->next;
  }
  /* load libraries first so that their symbols are available to
     other routines */
  while (libs) {
    char *s = AtomName(lib->name);
    
    if (ls[0] == '-') {
      strcpy(LOCAL_FileNameBuf,"lib");
      strcat(LOCAL_FileNameBuf,s+2);
      strcat(LOCAL_FileNameBuf,".so");
    } else {
      strcpy(LOCAL_FileNameBuf,s);
    }

    if((libs->handle=mydlopen(LOCAL_FileNameBuf)) == NULL)
    {
      strcpy(LOCAL_ErrorSay,mydlerror());
      popen_text_stack(lvl);
      return LOAD_FAILLED;
    }
    libs = libs->next;
  }

  *init_proc = (YapInitProc) mydlsym(proc_name);

  if(! *init_proc) {
    strcpy(LOCAL_ErrorSay,"Could not locate initialization routine");
      popen_text_stack(lvl);
    return LOAD_FAILLED;
  }

      popen_text_stack(lvl);
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
  ForeignObj *f_code;

  f_code = ForeignCodeLoaded;
  while (f_code != NULL) {
    StringList objs, libs;

    objs = f_code->objs;
    while (objs != NULL) {
      if (mydlclose(objs->handle) != 0)
	return; /* ERROR */
      objs = objs->next;
    }
    libs = f_code->libs;
    while (libs != NULL) {
      if (mydlclose(libs->handle) != 0)
	return; /* ERROR */
      objs = libs->next;
    }
    f_code = f_code->next;
  }
}

Int
Yap_ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}

#endif
