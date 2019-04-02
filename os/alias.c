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
* File:		alias.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file   alias.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:53:20 2015
 *
 * @brief  File Aliases
 *
 *
 */


/**
 * @defgroup Aliases Aliases to Stream Names
 * @ingroup InputOutput
 *
 * Aliases:
 * This file defines the main operations on aliases, a second name for a file. Aliases are always
 * textual constants (atoms).
 *
 * Their first advantage is that they allow cleaning up code, by separating name from operation, eg
 * YAP has a loop stream used to run the main top-level, which can be std0 originally but then
 * changed to a pipe, a file, or a memory region. Other important streams are the user streams. Finally,
 * the debugger uses debugger input and output.
 *
 * Predefined stream aliases are:
 *  + user: special alias, initially refers to all the three standard streams.
 *  + `user_input: initially refers to the stdandard input stream;
 *  + `user_output: initially refers to the stdandard output stream;
 *  + `user_error: initially refers to the stdandard error stream. Often this is the same device
 *    as `stderr`, just accessed in different ways.
 *  + loop_stream: refers to the stream for the file or object being current consulted
 *  + debugger_input: refers to the stream used to send debugger commands, by default `user_input`.
 *  + debugger_output: refers to the stream used to output debugging, by default `user_error`.
 *    It must always be interactive.
 */

#include "sysbits.h"
#if HAVE_FCNTL_H
/* for O_BINARY and O_TEXT in WIN32 */
#include <fcntl.h>
#endif
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "YapEval.h"
#include "YapText.h"
#include <stdlib.h>
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_SELECT_H && !_MSC_VER && !defined(__MINGW32__)
#include <sys/select.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef _WIN32
// WIN32 API support
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#endif

#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#endif
#include "iopreds.h"

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static Atom FetchAlias (int sno);
static bool ExistsAliasForStream (int sno, Atom al);

/**
 * Specify an alias to the stream. The alias <tt>Name</tt> must be an atom. The
 * alias can be used instead of the stream descriptor for every operation
 * concerning the stream.
 *
 * @param + _tname_ Name of Alias
 * @param + _tstream_ stream identifier
 *
 * @return
 */
static Int add_alias_to_stream (USES_REGS1)
{
  Term tname = Deref(ARG1);
  Term tstream = Deref(ARG2);
  Atom at;
  Int sno;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "$add_alias_to_stream");
    return (FALSE);
  }
  if (IsVarTerm(tstream)) {
    Yap_Error(INSTANTIATION_ERROR, tstream, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsApplTerm (tstream) || FunctorOfTerm (tstream) != FunctorStream ||
	     !IsIntTerm(ArgOfTerm(1,tstream))) {
    Yap_Error(DOMAIN_ERROR_STREAM_OR_ALIAS, tstream, "$add_alias_to_stream");
    return (FALSE);
  }
  at = AtomOfTerm(tname);
  sno = (int)IntOfTerm(ArgOfTerm(1,tstream));
  if (Yap_AddAlias(at, sno))
    return(TRUE);
  /* we could not create the alias, time to close the stream */
  Yap_CloseStream(sno);
  Yap_Error(PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM, tname, "open/3");
  return (FALSE);
}

static Int check_if_valid_new_alias (USES_REGS1)
{
  Term tname = Deref(ARG1);
  Atom at;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "$add_alias_to_stream");
    return (FALSE);
  } else if (!IsAtomTerm (tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "$add_alias_to_stream");
    return (FALSE);
  }
  at = AtomOfTerm(tname);
  return(Yap_CheckAlias(at) == -1);
}


bool
Yap_FetchStreamAlias (int sno, Term t2 USES_REGS)
{

  if (IsVarTerm(t2)) {
    Atom at = FetchAlias(sno);
    if (at == NULL)
      return false;
    else {
      return Yap_unify_constant(t2, MkAtomTerm(at));
    }
  } else if (IsAtomTerm(t2)) {
    Atom at = AtomOfTerm(t2);
    return  ExistsAliasForStream(sno,at);
  } else {
     Yap_Error(TYPE_ERROR_ATOM, t2, "stream_property(_,alias( ))");
    return false;
  }
}

static void
ExtendAliasArray(void)
{
    CACHE_REGS
  AliasDesc new;
  UInt new_size = GLOBAL_SzOfFileAliases+ALIASES_BLOCK_SIZE;

  new = (AliasDesc)Yap_AllocCodeSpace(sizeof(AliasDesc *)*new_size);
  memmove((void *)new, (void *)GLOBAL_FileAliases, sizeof(AliasDesc *)*GLOBAL_SzOfFileAliases);
  Yap_FreeCodeSpace((ADDR) GLOBAL_FileAliases);
  GLOBAL_FileAliases = new;
  GLOBAL_SzOfFileAliases = new_size;
}

void
Yap_SetAlias (Atom arg, int sno)
{
  CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

      if (arg == AtomUserIn)
	  LOCAL_c_input_stream = sno;
       if (arg == AtomUserOut)
	  LOCAL_c_output_stream = sno;
       if (arg == AtomUserErr)
	  LOCAL_c_error_stream = sno;
  while (aliasp < aliasp_max) {
    // replace alias
    if (aliasp->name == arg) {
      aliasp->alias_stream = sno;
      return;
    }
    aliasp++;
  }
  // set new alias
  /* we have not found an alias, create one */
  if (aliasp == GLOBAL_FileAliases+ GLOBAL_SzOfFileAliases)
    ExtendAliasArray();
  GLOBAL_NOfFileAliases++;
  aliasp->name = arg;
  aliasp->alias_stream = sno;
}

/* purge all aliases for stream sno */
void
Yap_DeleteAliases (int sno)
{
    CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+ GLOBAL_NOfFileAliases, new_aliasp = aliasp;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno) {
      if (aliasp - GLOBAL_FileAliases < 3) {
	/* get back to std streams, but keep alias around */
	Int alno = aliasp-GLOBAL_FileAliases;
	new_aliasp->alias_stream = alno;
	new_aliasp++;
     } else {
	GLOBAL_NOfFileAliases--;
	//       printf("RM %p at %d/%d %d\n", new_aliasp->name, new_aliasp-GLOBAL_FileAliases, new_aliasp->alias_stream, sno);
      }
    } else {
      /* avoid holes in alias array */
      if (new_aliasp != aliasp) {
	new_aliasp->alias_stream = aliasp->alias_stream;
	new_aliasp->name = aliasp->name;
      }
      new_aliasp++;
    }
    aliasp++;
  }/////
}

/* check if name is an alias */
int
Yap_CheckAlias (Atom arg)
{
    CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;


  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      return(aliasp->alias_stream);
    }
    aliasp++;
  }
  return(-1);
}

/* check if stream has an alias */
static Atom
FetchAlias (int sno)
{
    CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno) {
      return(aliasp->name);
    }
    aliasp++;
  }
  return NULL;
}

/* check if arg is an alias */
static bool
ExistsAliasForStream (int sno, Atom al)
{
    CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->alias_stream == sno && aliasp->name == al) {
       if (al == AtomUserIn) {
	  LOCAL_c_input_stream = sno;
	  aliasp->alias_stream = sno;
      } else
       if (al == AtomUserOut) {
	  LOCAL_c_output_stream = sno;
	  aliasp->alias_stream = sno;
      }
      if (al == AtomUserErr) {
	LOCAL_c_error_stream = sno;
	  aliasp->alias_stream = sno;
      }
      return true;
    }
    aliasp++;
  }
  return false;
}

/* check if arg is an alias */
bool
Yap_FindStreamForAlias (Atom al)
{
    CACHE_REGS
  AliasDesc aliasp = GLOBAL_FileAliases,
      aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->name == al) {
      return aliasp->alias_stream > 0;
    }
    aliasp++;
  }
  LOCAL_Error_TYPE = DOMAIN_ERROR_STREAM;
  return false;
}

/* create a new alias arg for stream sno */
int
Yap_RemoveAlias (Atom arg, int sno)
{
    CACHE_REGS

  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      if (aliasp->alias_stream != sno) {
	return(FALSE);
      }
      return(TRUE);
    }
    aliasp++;
  }
  //printf("RM %p at %d\n", arg, aliasp-GLOBAL_FileAliases);
  /* we have not found an alias neither a hole */
  if (aliasp == GLOBAL_FileAliases+GLOBAL_SzOfFileAliases)
    ExtendAliasArray();
  GLOBAL_NOfFileAliases--;
  aliasp->name = arg;
  aliasp->alias_stream = sno;
  return(TRUE);
}

/* create a new alias arg for stream sno */
bool
Yap_AddAlias (Atom arg, int sno)
{
    CACHE_REGS

  AliasDesc aliasp = GLOBAL_FileAliases, aliasp_max = GLOBAL_FileAliases+GLOBAL_NOfFileAliases;

  if (arg == AtomUserIn)
    LOCAL_c_input_stream = sno;
  else if (arg == AtomUserOut)
    LOCAL_c_output_stream = sno;
  else  if (arg == AtomUserErr)
    LOCAL_c_error_stream = sno;
  while (aliasp < aliasp_max) {
    if (aliasp->name == arg) {
      aliasp->alias_stream = sno;
      return true;
    }
    aliasp++;
  }
    
  /* we have not found an alias neither a hole */
  if (aliasp == GLOBAL_FileAliases+GLOBAL_SzOfFileAliases)
    ExtendAliasArray();
  GLOBAL_NOfFileAliases++;
  //  printf("ADD %p at %d\n", arg, aliasp-GLOBAL_FileAliases);
  aliasp->name = arg;
  aliasp->alias_stream = sno;
  return true;
}

/* create a new alias arg for stream sno */
struct AliasDescS *
Yap_InitStandardAliases(void)
{
    CACHE_REGS
  /* init standard aliases */

    /* alloca alias array */
  GLOBAL_FileAliases = (AliasDesc)Yap_AllocCodeSpace(sizeof(struct AliasDescS)*ALIASES_BLOCK_SIZE);

  if (GLOBAL_FileAliases == NULL)
    return NULL;

  GLOBAL_FileAliases[0].name = AtomUserIn;
  GLOBAL_FileAliases[0].alias_stream = 0;
  GLOBAL_FileAliases[1].name = AtomUserOut;
  GLOBAL_FileAliases[1].alias_stream = 1;
  GLOBAL_FileAliases[2].name = AtomUserErr;
  GLOBAL_FileAliases[2].alias_stream = 2;
  GLOBAL_FileAliases[3].name = AtomLoopStream;
  GLOBAL_FileAliases[3].alias_stream = 0;
  GLOBAL_FileAliases[4].name = AtomDebuggerInput;
  GLOBAL_FileAliases[4].alias_stream = 0;
  GLOBAL_NOfFileAliases = 5;
  GLOBAL_SzOfFileAliases = ALIASES_BLOCK_SIZE;

  return GLOBAL_FileAliases;
}

  /* create a new alias arg for stream sno */
void
Yap_InitAliases(void)
{
  Yap_InitCPred ("$check_if_valid_new_alias", 1, check_if_valid_new_alias, TestPredFlag|SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$add_alias_to_stream", 2, add_alias_to_stream, SafePredFlag|SyncPredFlag|HiddenPredFlag);
}
