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

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "Foreign.h"

#if LOAD_DYDL

#include <string.h>

/* This code is originally from Rex A. Dieter posting in comp.sys.next.programmer */
#import <mach-o/dyld.h>

enum dyldErrorSource
{
    OFImage,
};

#define NUM_OFI_ERRORS (sizeof(OFIErrorStrings) /  sizeof(OFIErrorStrings[0]))

static char *
mydlerror(int dl_errno)
{
  char *errString;
  switch(dl_errno) {
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


static void *
mydlopen(char *path)
{
    int dyld_result;
    NSObjectFileImage ofile;
    NSModule handle = NULL;
    dyld_result = NSCreateObjectFileImageFromFile(path, &ofile);
    if (dyld_result != NSObjectFileImageSuccess) {
      dl_errno = dyld_result;
    } else {
      /* NSLinkModule will cause the run to abort on any link error's */
      /* not very friendly but the error recovery functionality is limited */
        handle = NSLinkModule(ofile, path, TRUE);
    } return handle;
}

static void *
mydlsym(char *symbol)
{
    void *addr;
    if (NSIsSymbolNameDefined(symbol))
        addr = NSAddressOfSymbol(NSLookupAndBindSymbol(symbol));
    else
        addr = NULL;
    return addr;
} 

static void
mydlclose(void *handle)
{
  NSUnLinkModule(handle, NSUNLINKMODULE_OPTION_NONE);
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
    void *handle;

    /* mydlopen wants to follow the LD_CONFIG_PATH */
    if (!TrueFileName(ofiles->s, FileNameBuf, TRUE)) {
      strcpy(LoadMsg, "[ Trying to open unexisting file in LoadForeign ]");
      return LOAD_FAILLED;
    }
    if((handle=mydlopen(FileNameBuf)) == 0)
    {
      fprintf(stderr,"calling dlopen with error %s\n", mydlerror());
/*      strcpy(LoadMsg,dlerror());*/
      return LOAD_FAILLED;
    }

    ofiles->handle = handle;

    ofiles = ofiles->next;
  }
  /* load libraries first so that their symbols are available to
     other routines */
  while (libs) {

    if (libs->s[0] == '-') {
      strcpy(FileNameBuf,"lib");
      strcat(FileNameBuf,libs->s+2);
      strcat(FileNameBuf,".so");
    } else {
      strcpy(FileNameBuf,libs->s);
    }

    if((libs->handle=mydlopen(FileNameBuf)) == NULL)
    {
      strcpy(LoadMsg,mydlerror());
      return LOAD_FAILLED;
    }
    libs = libs->next;
  }

  *init_proc = (YapInitProc) mydlsym(proc_name);

  if(! *init_proc) {
    strcpy(LoadMsg,"Could not locate initialization routine");
    return LOAD_FAILLED;
  }

  return LOAD_SUCCEEDED;
}

void 
ShutdownLoadForeign(void)
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
ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}

#endif
