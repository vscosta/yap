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
* File:		iopreds.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Files and GLOBAL_Streams, Simple Input/Output,
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "YapEval.h"
#include "YapText.h"
#include <stdlib.h>
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_FCNTL_H
/* for O_BINARY and O_TEXT in WIN32 */
#include <fcntl.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#endif
#if !HAVE_STRNCAT
#define strncat(X,Y,Z) strcat(X,Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif





#ifdef BEAM
int beam_write ( USES_REGS1 )
{
  Yap_StartSlots();
  Yap_plwrite (ARG1, GLOBAL_Stream+LOCAL_output_stream, 0, 0, GLOBAL_MaxPriority);
  Yap_CloseSlots();
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = 0L;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}
#endif

static Int
p_write ( USES_REGS1 )
{
    /* '$write'(+Flags,?Term) */
  int flags = (int) IntOfTerm (Deref (ARG1));
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  yhandle_t mySlots = Yap_StartSlots();
  Yap_plwrite (ARG2, GLOBAL_Stream+LOCAL_output_stream, 0, flags, GLOBAL_MaxPriority);
  Yap_CloseSlots( mySlots );
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write_prio ( USES_REGS1 )
{
    /* '$write'(+Flags,?Term) */
  int flags = (int) IntOfTerm (Deref (ARG1));
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  yhandle_t my_slots = Yap_StartSlots();
  Yap_plwrite (ARG3, GLOBAL_Stream+LOCAL_output_stream, 0, flags, (int)IntOfTerm(Deref(ARG2)));
  Yap_CloseSlots(my_slots);
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write2_prio ( USES_REGS1 )
{				/* '$write'(+Stream,+Flags,?Term) */
  int old_output_stream = LOCAL_output_stream;
  Int flags = IntegerOfTerm(Deref(ARG2));
  int stream_f;

  LOCAL_output_stream = CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (LOCAL_output_stream == -1) {
    LOCAL_output_stream = old_output_stream;
    return(FALSE);
  }
  UNLOCK(GLOBAL_Stream[LOCAL_output_stream].streamlock);
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  yhandle_t myslots = Yap_StartSlots();
  Yap_plwrite (ARG4, GLOBAL_Stream+LOCAL_output_stream, 0, (int) flags, (int) IntOfTerm (Deref (ARG3)));
  Yap_CloseSlots(myslots);
  LOCAL_output_stream = old_output_stream;
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}

static Int
p_write2 ( USES_REGS1 )
{				/* '$write'(+Stream,+Flags,?Term) */
  int old_output_stream = LOCAL_output_stream;
  LOCAL_output_stream = CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (LOCAL_output_stream == -1) {
    LOCAL_output_stream = old_output_stream;
    return(FALSE);
  }
  UNLOCK(GLOBAL_Stream[LOCAL_output_stream].streamlock);
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  yhandle_t myslots = Yap_StartSlots();
  Yap_plwrite (ARG3, GLOBAL_Stream+LOCAL_output_stream, 0, (int) IntOfTerm (Deref (ARG2)), GLOBAL_MaxPriority);
  Yap_CloseSlots(myslots);
  LOCAL_output_stream = old_output_stream;
  if (EX != 0L) {
    Term ball = Yap_PopTermFromDB(EX);
    EX = NULL;
    Yap_JumpToEnv(ball);
    return(FALSE);
  }
  return (TRUE);
}
