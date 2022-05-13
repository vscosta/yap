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
 * File:		load_dl.c *
 * comments:	dl based dynamic loaderr of external routines		 *
 *               tested on i486-linuxelf *
 *************************************************************************/

#include "Yap.h"
#include "YapHeap.h"
#include "Yatom.h"
#include "yapio.h"

#include "Foreign.h"

#if LOAD_DL

// use SWI-Prolog code if all else fails
char *findExecutable(const char *av0, char *buffer);

#include <dlfcn.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <dlfcn.h>
#include <mach-o/dyld.h>
#endif

typedef void (*prismf)(void);

/* only works for dlls */
int Yap_CallFunctionByName(const char *thing_string);

int Yap_CallFunctionByName(const char *thing_string) {
  void *handle = dlopen(NULL, RTLD_LAZY
#ifndef __CYGWIN__
#ifdef RTLD_NOLOAD
                                  | RTLD_NOLOAD
#endif
#endif
  );
  // you could do RTLD_NOW as well.  shouldn't matter
  if (!handle) {
    CACHE_REGS
    Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1,
              "Dynamic linking on main module : %s\n", dlerror());
  }
  prismf *addr = (prismf *)dlsym(handle, thing_string);
  if (addr)
    (*addr)();
  dlclose(handle);
  return TRUE;
}

/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
 */
char *Yap_FindExecutable(void) {
#if HAVE_GETEXECNAME
  // Solaris
  return getexecname();
#elif __APPLE__
  char *buf = malloc(MAX_PATH);

  uint32_t size;
  if (!_NSGetExecutablePath(buf, &size)) {
    buf = realloc(buf, size + 1);
    return buf;
  }
  return "yap";
#elif defined(__linux__)
  char *buf = malloc(MAX_PATH);
  ssize_t len = readlink("/proc/self/exe", buf, MAX_PATH - 1);

  if (len != -1) {
    buf[len] = '\0';
    return buf;
  }
// follow through to standard method
#elif defined(__FreeBSD__) || defined(__DragonFly__)
  enum { BUFFERSIZE = 1024 };
  char *buf = malloc(BUFFERSIZE);
  ssize_t len = readlink("/proc/curproc/file", buf, sizeof(buf) - 1);

  if (len != -1) {
    buf[len] = '\0';
    return buf;
  }
  int mib[4];
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1; // current process
  size_t cb = BUFFERSIZE;
  sysctl(mib, 4, buf, &cb, NULL, 0);
// follow through to standard method
#endif
  return NULL;
}

void *Yap_LoadForeignFile(char *file, int flags) {
  CACHE_REGS
  int dlflag;
  void *out;

  if (flags & EAGER_LOADING)
    dlflag = RTLD_NOW;
  else
    dlflag = RTLD_LAZY;
  if (flags & GLOBAL_LOADING)
    dlflag |= RTLD_GLOBAL;
#ifndef __CYGWIN__
  else
    dlflag |= RTLD_LOCAL;
#endif
  out = (void *)dlopen(file, dlflag);
  if (out == NULL) {
    const char *m_os = dlerror();
    if (m_os) {
      LOCAL_ErrorMessage = malloc(MAX_ERROR_MSG_SIZE);
      strncpy(LOCAL_ErrorMessage, m_os, MAX_ERROR_MSG_SIZE - 1);
    } else {
      LOCAL_ErrorMessage = "dlopen failed";
    }
  }
  return out;
}

int Yap_CallForeignFile(void *handle, char *f) {
  YapInitProc proc = (YapInitProc)dlsym(handle, f);
  if (!proc) {
    /* Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1, "dlsym error %s\n", dlerror());*/
    return FALSE;
  }
  (*proc)();
  return TRUE;
}

int Yap_CloseForeignFile(void *handle) {
  if (dlclose(handle) < 0) {
    CACHE_REGS
    Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1, "dlclose error %s\n", dlerror());
    return -1;
  }
  return 0;
}

static char *
error_found(char *omsg, char *text, ...)
{
    CACHE_REGS
  va_list ap;
  va_start(ap,text);
  if (!omsg) {
    omsg = Malloc(MAX_PATH+1);
    strcpy(omsg,"Got the following OS errors:\n");
      
  }
  else{
  omsg = Realloc(omsg,strlen(omsg)+MAX_PATH);
  }
  if (text && text[0])
    vsnprintf(omsg+strlen(omsg),MAX_PATH-1,text, ap);
  return omsg;
}

/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
 */
static YapInitProc LoadForeign(StringList 
  ofiles, StringList libs, char *proc_name) {
  CACHE_REGS
  LOCAL_ErrorMessage = NULL;
  char *omsg = NULL;
  StringList o0 = ofiles;
  int lvl = push_text_stack();

  while (libs || ofiles) {
    const char *file;
    StringList path = libs? libs : ofiles;
    /* load libraries first so that their symbols are available to
x       other routines */
    file = AtomName(path->name);

    if ((path->handle = dlopen(file, RTLD_LAZY | RTLD_GLOBAL)) ==
        NULL)
    {
      omsg = error_found(omsg, "Tried to load %s, failed: %s\n", file, dlerror());
      if (!libs) {
	Yap_ThrowError(SYSTEM_ERROR_DYNAMIC_LOADER,MkAtomTerm(path->name),omsg);
	pop_text_stack(lvl);
	return NULL;
      }
    } else {
      omsg = error_found(omsg, "loaded %s\n", file );
    }
    if (!libs) ofiles = ofiles->next;
    else libs = libs->next;
    }
    ofiles = o0;
    while (ofiles) {
    /* load libraries first so that their symbols are available to
       other routines */

    YapInitProc o = dlsym(ofiles
			   ->handle,proc_name);
    if (o) {
      pop_text_stack(lvl);
      return o;
    }
    const char *
      file = AtomName(ofiles->name);
    omsg = error_found(omsg, "%s no symbol in %s, failed: %s\n", proc_name, file, dlerror());
    ofiles = ofiles->next;
  }
  Yap_ThrowError(SYSTEM_ERROR_DYNAMIC_LOADER, MkAtomTerm(o0->name),omsg);
  pop_text_stack(lvl);
  return NULL;
}

Int Yap_LoadForeign(StringList ofiles, StringList libs, char *proc_name,
                    YapInitProc *init_proc) {
  *init_proc = LoadForeign(ofiles, libs, proc_name);
  return *init_proc != NULL? LOAD_SUCCEEDED :LOAD_FAILLED;
}

void Yap_ShutdownLoadForeign(void) {
  ForeignObj *f_code;

  f_code = ForeignCodeLoaded;
  while (f_code != NULL) {
    StringList objs, libs, old;
    ForeignObj *of_code = f_code;

    objs = f_code->objs;
    while (objs != NULL) {
      old = objs;
      if (dlclose(objs->handle) != 0)
        return; /* ERROR */
      objs = objs->next;
      Yap_FreeCodeSpace((ADDR)old);
    }
    libs = f_code->libs;
    while (libs != NULL) {
      old = libs;
      if (dlclose(libs->handle) != 0)
        return; /* ERROR */
      libs = libs->next;
      Yap_FreeCodeSpace((ADDR)old);
    }
    f_code = f_code->next;
    Yap_FreeCodeSpace((ADDR)of_code);
  }

  /*
    make sure that we don't try to close foreign code several times, eg,
    from within an error handler
  */
  ForeignCodeLoaded = NULL;
}

Int Yap_ReLoadForeign(StringList ofiles, StringList libs, char *proc_name,
                      YapInitProc *init_proc) {
  return (*init_proc = LoadForeign(ofiles, libs, proc_name)) != NULL;
}


#endif

#if SIMICS

void dlopen(void) {}

void dlclose(void) {}

void dlsym(void) {}

#endif
