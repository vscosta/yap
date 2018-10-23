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
* File:		readline.c *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/

/**
 * @file   console.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Jan 20 00:56:23 2016
 *
 * @brief
 *
 *
 */
/*
 * This file includes the interface to the console IO, tty style. Refer also to
 the readline library.
 * @defgroup console Support for console-based interaction.
 * @ingroup InputOutput

 */

#include "sysbits.h"

static Int prompt(USES_REGS1);
static Int prompt1(USES_REGS1);

static int ConsoleGetc(int);
static int ConsolePutc(int, int);



bool Yap_DoPrompt(StreamDesc *s) {
  if (s->status & Tty_Stream_f) {
    if (GLOBAL_Stream[LOCAL_c_input_stream].status & Tty_Stream_f &&
	GLOBAL_Stream[LOCAL_c_error_stream].status & Tty_Stream_f) {
      return LOCAL_newline;
    }
  }
  return false;
}


/* check if we read a newline or an EOF */
int console_post_process_read_char(int ch, StreamDesc *s) {
  /* the character is also going to be output by the console handler */
  console_count_output_char(ch, GLOBAL_Stream + LOCAL_c_error_stream);
  if (ch == '\r') {
    s->linepos = 0;
    LOCAL_newline = true;
} else
 if (ch == '\n') {
    CACHE_REGS
    ++s->linecount;
    ++s->charcount;
    s->linepos = 0;
    LOCAL_newline = true;
  } else {
    CACHE_REGS
    ++s->charcount;
    ++s->linepos;
    LOCAL_newline = false;
  }
  return ch;
}

bool is_same_tty(FILE *f1, FILE *f2) {
#if HAVE_TTYNAME
  return ttyname_r(fileno(f1), LOCAL_FileNameBuf, YAP_FILENAME_MAX - 1) ==
         ttyname_r(fileno(f2), LOCAL_FileNameBuf, YAP_FILENAME_MAX - 1);
#endif
  // assume a single console, for now
  return true;
}

static Int is_same_tty2(USES_REGS1) { /* 'prompt(Atom)                 */
  int sni = Yap_CheckStream(ARG1, Input_Stream_f, "put/2");
  int sno = Yap_CheckStream(ARG2, Output_Stream_f, "put/2");
  bool out = (GLOBAL_Stream[sni].status & Tty_Stream_f) &&
             (GLOBAL_Stream[sno].status & Tty_Stream_f) &&
             is_same_tty(GLOBAL_Stream[sno].file, GLOBAL_Stream[sni].file);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  UNLOCK(GLOBAL_Stream[sni].streamlock);
  return out;
}

void Yap_ConsoleOps(StreamDesc *s) {
  /* the putc routine only has to check it is putting out a newline */
  s->stream_putc = ConsolePutc;
  s->stream_getc = ConsoleGetc;
#if USE_READLINE
  /* if a tty have a special routine to call readline */
  if ((s->status & Readline_Stream_f) && trueGlobalPrologFlag(READLINE_FLAG)) {
    if (Yap_ReadlineOps(s))
      return;
  }
#endif
}

/* static */
static int ConsolePutc(int sno, int ch) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  if (ch == 10) {
#if MAC || _WIN32
    fputs("\n", s->file);
#else
    putc('\n', s->file);
#endif
    LOCAL_newline = true;
  } else
    putc(ch, s->file);
#if MAC || _WIN32
  fflush( NULL );
#endif
  console_count_output_char(ch, s);
  return ((int)ch);
}

/* send a prompt, and use the system for internal buffering. Speed is
   not of the essence here !!! */
static int ConsoleGetc(int sno) {
  CACHE_REGS
  register StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;

restart:
  /* keep the prompt around, just in case, but don't actually
     show it in silent mode */
  if (Yap_DoPrompt(s)) {
    if (!silentMode()) {
      char *cptr = LOCAL_Prompt, ch;
    /* use the default routine */
      while ((ch = *cptr++) != '\0') {
        GLOBAL_Stream[StdErrStream].stream_putc(StdErrStream, ch);
      }
    }
    Yap_clearInput(LOCAL_c_error_stream);
    strncpy(LOCAL_Prompt, (char *)RepAtom(LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
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
    Yap_external_signal(0, YAP_INT_SIGNAL);
    LOCAL_PrologMode &= ~ConsoleGetcMode;
    LOCAL_newline = TRUE;
    if (LOCAL_PrologMode & AbortMode) {
      Yap_Error(ABORT_EVENT, TermNil, "");
      LOCAL_ErrorMessage = "Abort";
      return EOF;
    }
    goto restart;
  } else {
    LOCAL_PrologMode &= ~ConsoleGetcMode;
  }
  if (ch == EOF)
    return EOF;
  return ch;
}

/** @pred prompt1(+ _A__)


Changes YAP input prompt for the .


*/

static Int prompt1(USES_REGS1) { /* prompt1(Atom)                 */
  Term t = Deref(ARG1);
  Atom a;
  if (IsVarTerm(t) || !IsAtomTerm(t))
    return (FALSE);
  LOCAL_AtPrompt = a = AtomOfTerm(t);
  if (strlen((char *)RepAtom(a)->StrOfAE) > MAX_PROMPT) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, t, "prompt %s is too long",
              RepAtom(a)->StrOfAE);
    return (FALSE);
  }
  strncpy(LOCAL_Prompt, (char *)RepAtom(a)->StrOfAE, MAX_PROMPT);
  return (TRUE);
}

/** @pred prompt(- _A_,+ _B_)

Changes YAP input prompt from  _A_ to  _B_, active on *next* standard input
interaction.

*/
static Int prompt(USES_REGS1) { /* prompt(Old,New)       */
  Term t = Deref(ARG2);
  Atom a;
  if (!Yap_unify_constant(ARG1, MkAtomTerm(LOCAL_AtPrompt)))
    return (FALSE);
  if (IsVarTerm(t) || !IsAtomTerm(t))
    return (FALSE);
  a = AtomOfTerm(t);
  if (strlen(RepAtom(a)->StrOfAE) > MAX_PROMPT) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, t, "prompt %s is too long",
              RepAtom(a)->StrOfAE);
    return false;
  }
  strncpy(LOCAL_Prompt, (char *)RepAtom(LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
  LOCAL_AtPrompt = a;
  return (TRUE);
}

/** @pred ensure_prompting

Make sure we have a prompt at this point, even if we have to
introduce a new line.

*/
static Int ensure_prompting(USES_REGS1) { /* prompt(Old,New)       */
  if (!LOCAL_newline) {
    GLOBAL_Stream[2].stream_wputc(2, 10); // hack!
  }
  return true;
}

int Yap_GetCharForSIGINT(void) {
  CACHE_REGS
  int ch;
#if USE_READLINE
  if (trueGlobalPrologFlag(READLINE_FLAG) ||
      (ch = Yap_ReadlineForSIGINT()) == 0)
#endif
  { /* ask for a new line */
    fprintf(stderr, "Action (h for help): ");
    ch = getc(stdin);
    /* first process up to end of line */
    while ((fgetc(stdin)) != '\n')
      ;
  }
  LOCAL_newline = TRUE;
  fflush(NULL);
  return ch;
}

void Yap_InitConsole(void) {
  CACHE_REGS
  LOCAL_newline = true;
  Yap_InitCPred("prompt", 1, prompt1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("prompt1", 1, prompt1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_same_tty", 2, is_same_tty2,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("prompt", 2, prompt, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$ensure_prompting", 0, ensure_prompting,
                SafePredFlag | SyncPredFlag);
}
