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
* File:		Foreign.h						 *
* comments:	header file for dynamic loading routines  	 	 *
*************************************************************************/

#define NO_DYN 1

/* Currently load_foreign_files works for the following machines:

   AIX: should work for 3.2 and 4.1 at least, using ECOFF;
   linux: should work both for a.out (untested by me) and ELF;
   osf:  should work, but isn't working yet.
   sunos4: should work, using A.OUT format;
   svr4, eg solaris: should work, using ELF format;

  YAP should be able to load on most BSD Unixes, but you will need to
  say that here.

  YAP also supports COFF loading (pretty much the same technique as
  used for A.OUT loading) but that is untested so far. 

*/

#ifdef _AIX
#undef NO_DYN
#endif /* __AIX */

#if HAVE_DLOPEN
#define LOAD_DL 1
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

#if defined(_WIN32) || defined(__CYGWIN__)
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

/*
#if defined(__MACH__) && defined(__APPLE__)
#ifdef NO_DYN
#undef NO_DYN
#endif
#ifndef LOAD_DL
#define LOAD_DL 1
#endif
#endif
*/

extern char LoadMsg[];

#define LOAD_SUCCEEDED   0
#define LOAD_FAILLED    -1

typedef struct StringListItem {
  char *s;
  void *handle;
  struct StringListItem *next;
} StringListItem, *StringList;

typedef struct ForeignLoadItem {
  StringList objs;
  StringList libs;
  char *f;
  int module;
  struct ForeignLoadItem *next;
} ForeignObj;

typedef void (*YapInitProc)(void);

#ifndef STD_PROTO
#define STD_PROTO(F,A)  F A
#endif

void STD_PROTO(YAP_FindExecutable,(char *));
Int STD_PROTO(LoadForeign,(StringList, StringList, char *, YapInitProc *));
Int STD_PROTO(ReLoadForeign,(StringList, StringList, char *, YapInitProc *));
void	STD_PROTO(ReOpenLoadForeign,(void));
void	STD_PROTO(ShutdownLoadForeign,(void));








