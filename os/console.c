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
* File:		readline.c							 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the interface to the console IO, tty style. Refer also to the readline library.
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif 
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"

static Int prompt( USES_REGS1 );
static Int prompt1( USES_REGS1 );

static int ConsoleGetc( int);
static int ConsolePutc( int, int);

/* check if we read a newline or an EOF */
int
console_post_process_read_char(int ch, StreamDesc *s)
{
  /* the character is also going to be output by the console handler */
  console_count_output_char(ch,GLOBAL_Stream+StdErrStream);
  if (ch == '\n') {
      CACHE_REGS
    ++s->linecount;
    ++s->charcount;
    s->linepos = 0;
    LOCAL_newline = TRUE;
  } else {
      CACHE_REGS
    ++s->charcount;
    ++s->linepos;
    LOCAL_newline = FALSE;
  }
  return ch;
}


bool
is_same_tty(FILE *f1, FILE *f2)
{
#if HAVE_TTYNAME
  return(ttyname(fileno(f1)) == ttyname(fileno(f2)));
#else
  return;
#endif  
}

static Int
is_same_tty2 (USES_REGS1)
{				/* 'prompt(Atom)                 */
  int sni = Yap_CheckStream (ARG1, Input_Stream_f, "put/2");
  int sno = Yap_CheckStream (ARG2, Output_Stream_f, "put/2");
  bool out = (GLOBAL_Stream[sni].status & Tty_Stream_f) &&
    (GLOBAL_Stream[sno].status & Tty_Stream_f) &&
    is_same_tty(GLOBAL_Stream[sno].file,GLOBAL_Stream[sni].file);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  UNLOCK(GLOBAL_Stream[sni].streamlock);
  return out;
}

void
Yap_ConsoleOps( StreamDesc *s )
{
  /* the putc routine only has to check it is putting out a newline */
  s->stream_putc = ConsolePutc;
  /* if a tty have a special routine to call readline */
  if (!Yap_ReadlineOps( s )) {
    /* else just PlGet plus checking for prompt */
    s->stream_getc = ConsoleGetc;
  }
  Yap_DefaultStreamOps( s );
}

/* static */
static int
ConsolePutc (int sno, int ch)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
#if MAC || _MSC_VER || defined(__MINGW32__)
  if (ch == 10)
    {
      putc ('\n', s->file);
    }
  else
#endif
    putc (ch, s->file);
  console_count_output_char(ch,s);
  return ((int) ch);
}

/* send a prompt, and use the system for internal buffering. Speed is
   not of the essence here !!! */
static int
ConsoleGetc(int sno)
{
  CACHE_REGS
  register StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;

 restart:
  /* keep the prompt around, just in case, but don't actually
     show it in silent mode */
  if (LOCAL_newline) {
    if (silentMode()) {
      char *cptr = LOCAL_Prompt, ch;

      /* use the default routine */
      while ((ch = *cptr++) != '\0') {
	GLOBAL_Stream[StdErrStream].stream_putc(StdErrStream, ch);
      }
    }
    strncpy (LOCAL_Prompt, (char *)RepAtom (LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
    LOCAL_newline = FALSE;
  }
#if HAVE_SIGINTERRUPT
  siginterrupt(SIGINT, TRUE);
#endif
  LOCAL_PrologMode |= ConsoleGetcMode;
  ch = fgetc(s->file);
#if HAVE_SIGINTERRUPT
  siginterrupt(SIGINT, FALSE);
#endif
  if (LOCAL_PrologMode & InterruptMode) {
    Yap_external_signal( 0, YAP_INT_SIGNAL );
    LOCAL_PrologMode &= ~ConsoleGetcMode;
    LOCAL_newline = TRUE;
    if (LOCAL_PrologMode & AbortMode) {
      Yap_Error(ABORT_EVENT, TermNil, "");
      LOCAL_ErrorMessage = "Abort";
      return console_post_process_eof(s);
    }
    goto restart;
  } else {
    LOCAL_PrologMode &= ~ConsoleGetcMode;
  }
  if (ch == EOF)
    return console_post_process_eof(s);    
  return console_post_process_read_char(ch, s);
}


static Int
prompt1 ( USES_REGS1 )
{				/* prompt1(Atom)                 */
  Term t = Deref(ARG1);
  Atom a;
  if (IsVarTerm (t) || !IsAtomTerm (t))
    return (FALSE);
  LOCAL_AtPrompt = a = AtomOfTerm (t);
  if (strlen ((char *)RepAtom (a)->StrOfAE) > MAX_PROMPT) {
    Yap_Error(SYSTEM_ERROR_INTERNAL,t,"prompt %s is too long", RepAtom (a)->StrOfAE);
    return(FALSE);
  }
  strncpy(LOCAL_Prompt, (char *)RepAtom (a)->StrOfAE, MAX_PROMPT);
  return (TRUE);
}


static Int
prompt ( USES_REGS1 )
{				/* prompt(Old,New)       */
  Term t = Deref (ARG2);
  Atom a;
  if (!Yap_unify_constant (ARG1, MkAtomTerm (LOCAL_AtPrompt)))
    return (FALSE);
  if (IsVarTerm (t) || !IsAtomTerm (t))
    return (FALSE);
  a = AtomOfTerm (t);
  if (strlen(RepAtom (a)->StrOfAE) > MAX_PROMPT) {
    Yap_Error(SYSTEM_ERROR_INTERNAL,t,"prompt %s is too long", RepAtom (a)->StrOfAE);
    return(FALSE);
  }
  strncpy(LOCAL_Prompt, (char *)RepAtom (LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
  LOCAL_AtPrompt = a;
  return (TRUE);
}

int
Yap_GetCharForSIGINT(void)
{
    CACHE_REGS
  int ch;
  if ((ch = Yap_ReadlineForSIGINT()) == 0)
    {  /* ask for a new line */
      fprintf(stderr, "Action (h for help): ");
      ch = getc(stdin);
      /* first process up to end of line */
      while ((fgetc(stdin)) != '\n');
    }
  LOCAL_newline = TRUE;
  return ch;
}



void Yap_InitConsole(void) {
  Yap_InitCPred ("prompt", 1, prompt1, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("prompt1", 1, prompt1, SafePredFlag|SyncPredFlag);
  Yap_InitCPred ("$is_same_tty", 2, is_same_tty2, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("prompt", 2, prompt, SafePredFlag|SyncPredFlag);

}

