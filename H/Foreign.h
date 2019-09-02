/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-97	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		Foreign.h *
 * comments:	header file for dynamic loading routines  	 	 *
 *************************************************************************/

#define NO_DYN 1

#ifndef FOREIGN_H
#define FOREIGN_H

/**
 *
 * @file Foreign.h
 *
 *  @{
 *
 * load_foreign_files/3 has works for the following configurations:
 *
 * - linux: should work both for a.out (untested by me) and ELF;
 *
 * - WIN32: works (notice that symbols are not exported by default)
 *
 * - OSX: works using Mach dynamic libs.
 *
 * - osf:  should work, but isn't working yet.
 *
 * - sunos4: should work, using A.OUT format;
 *
 * - svr4, eg solaris: should work, using ELF format;
 *
 * - AIX: should work for 3.2 and 4.1 at least, using ECOFF;
 *
 * YAP should be able to load on most BSD Unixes, but you will need to
 *   say that here.
 *
 * YAP also supports COFF loading (pretty much the same technique as
 *   used for A.OUT loading) but that is untested so far.
 *
 */

#include "Yap.h"
#include "YapHeap.h"

#ifdef _AIX
#undef NO_DYN
#endif /* __AIX */

#ifdef HAVE_DLOPEN
#define LOAD_DL 1
#ifdef NO_DYN
#undef NO_DYN
#endif
#endif /* LOAD_DL */

#if defined(sparc) || defined(__sparc)
#undef NO_DYN
#if (!defined(__svr4__) && !defined(__SVR4))
#define A_OUT 1
#else
#ifdef SIMICS
#define NO_DYN 1
#else
#define LOAD_DL 1
#endif
#endif
#endif

#if defined(_WIN32)
#ifdef NO_DYN
#undef NO_DYN
#define LOAD_DLL 1
#endif
#if LOAD_DL
#undef LOAD_DL
#endif
#endif

#ifdef __hpux
#ifdef NO_DYN
#undef NO_DYN
#endif
#define LOAD_SHL 1
#endif

#ifdef HAVE_NSLINKMODULE
#ifdef NO_DYN
#undef NO_DYN
#endif
#if !HAVE_DLOPEN
#define LOAD_DYLD 1
#endif
#endif /* LOAD_DYLD */

#define LOAD_SUCCEEDED 0
#define LOAD_FAILLED -1

typedef struct StringListItem {
  Atom name;
  void *handle;
  struct StringListItem *next;
} StringListItem, *StringList;

typedef struct ForeignLoadItem {
  StringList objs;
  StringList libs;
  Atom f;
  Term module;
  struct ForeignLoadItem *next;
} ForeignObj;

typedef void (*YapInitProc)(void);

void *Yap_LoadForeignFile(char *, int);
int Yap_CallForeignFile(void *, char *);
int Yap_CloseForeignFile(void *);
Int Yap_LoadForeign(StringList, StringList, char *, YapInitProc *);
Int Yap_ReLoadForeign(StringList, StringList, char *, YapInitProc *);
void Yap_ReOpenLoadForeign(void);
void Yap_ShutdownLoadForeign(void);

#define EAGER_LOADING 1
#define GLOBAL_LOADING 2

/**
 * stub can always be called at DLL loading.
 *
 */
X_API bool load_none(void);

#endif

/// @}
