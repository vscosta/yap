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
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the interface to the readline library, if installed in the
 *system.
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
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif

#include "iopreds.h"

#if USE_READLINE

#include <readline/readline.h>
#include <readline/history.h>

static int ReadlineGetc(int);
static int ReadlinePutc(int, int);

static const char *history_file;

#define READLINE_OUT_BUF_MAX 256

typedef struct scan_atoms {
  Int pos;
  Atom atom;
} scan_atoms_t;

static char *atom_enumerate(const char *prefix, int state) {
  CACHE_REGS
  struct scan_atoms *index;
  Atom catom;
  Int i;

  if (!state) {
    index = (struct scan_atoms *)malloc(sizeof(struct scan_atoms));
    i = 0;
    catom = NIL;
  } else {
    CACHE_REGS
    index = LOCAL_search_atoms;
    catom = index->atom;
    i = index->pos;
  }

  while (catom != NIL || i < AtomHashTableSize) {
    //    if ( is_signalled() )		/* Notably allow windows version */
    //      PL_handle_signals();		/* to break out on ^C */
    AtomEntry *ap;

    if (catom == NIL) {
      /* move away from current hash table line */
      READ_LOCK(HashChain[i].AERWLock);
      catom = HashChain[i].Entry;
      READ_UNLOCK(HashChain[i].AERWLock);
      i++;
    } else {
      ap = RepAtom(catom);
      READ_LOCK(ap->ARWLock);
      if (strstr((char *)ap->StrOfAE, prefix) == (char *)ap->StrOfAE) {
        index->pos = i;
        index->atom = ap->NextOfAE;
        LOCAL_search_atoms = index;
        READ_UNLOCK(ap->ARWLock);
        return ap->StrOfAE;
      }
      catom = ap->NextOfAE;
      READ_UNLOCK(ap->ARWLock);
    }
  }
  LOCAL_search_atoms = NULL;
  free(index);
  return NULL;
}

static char *atom_generator(const char *prefix, int state) {
  char *s = atom_enumerate(prefix, state);

  if (s) {
    char *copy = malloc(1 + strlen(s));

    if (copy) /* else pretend no completion */
      strcpy(copy, s);
    s = copy;
  }

  return s;
}

static char **prolog_completion(const char *text, int start, int end) {
  char **matches = NULL;

  if ((start == 1 && rl_line_buffer[0] == '[') || /* [file */
      (start == 2 && strncmp(rl_line_buffer, "['", 2)))
    matches = rl_completion_matches((char *)text, /* for pre-4.2 */
                                    rl_filename_completion_function);
  else
    matches = rl_completion_matches((char *)text, atom_generator);

  return matches;
}

void Yap_ReadlineFlush(int sno) {
  if (GLOBAL_Stream[sno].status & Tty_Stream_f &&
      GLOBAL_Stream[sno].status & Output_Stream_f) {
    rl_redisplay();
  }
}

bool Yap_ReadlinePrompt(StreamDesc *s) {
  if (s->status & Tty_Stream_f) {
    s->stream_getc = ReadlineGetc;
    if (GLOBAL_Stream[0].status & Tty_Stream_f &&
        s->name == GLOBAL_Stream[0].name)
      s->stream_putc = ReadlinePutc;
    return true;
  }
  return false;
}

bool Yap_ReadlineOps(StreamDesc *s) {
  if (s->status & Tty_Stream_f) {
    if (GLOBAL_Stream[0].status & Tty_Stream_f &&
        is_same_tty(s->file, GLOBAL_Stream[0].file))
      s->stream_putc = ReadlinePutc;
    s->stream_getc = ReadlineGetc;
    s->status |= Readline_Stream_f;
    return true;
  }
  return false;
}

static int prolog_complete(int ignore, int key) {
  if (rl_point > 0 && rl_line_buffer[rl_point - 1] != ' ') {
#if HAVE_DECL_RL_CATCH_SIGNALS_ /* actually version >= 1.2, or true readline   \
                                   */
    rl_begin_undo_group();
    rl_complete(ignore, key);
    if (rl_point > 0 && rl_line_buffer[rl_point - 1] == ' ') {
      rl_delete_text(rl_point - 1, rl_point);
      rl_point -= 1;
      rl_delete(-1, key);
    }
    rl_end_undo_group();
#endif
  } else
    rl_complete(ignore, key);

  return 0;
}

bool Yap_InitReadline(Term enable) {
  // don't call readline within emacs
  // if (getenv("ËMACS"))
  //  return;
  if (enable == TermFalse)
    return true;
  GLOBAL_Stream[StdInStream].u.irl.buf = NULL;
  GLOBAL_Stream[StdInStream].u.irl.ptr = NULL;
  GLOBAL_Stream[StdInStream].status |= Readline_Stream_f;
#if _MSC_VER || defined(__MINGW32__)
  rl_instream = stdin;
#endif
  rl_outstream = stderr;
  using_history();
  const char *s = Yap_AbsoluteFile("~/.YAP.history", true);
  if (!read_history(s)) {
    FILE *f = fopen(s, "w");
    if (f) {
      fclose(f);
      read_history(s);
    }
  }
  rl_readline_name = "Prolog";
  rl_attempted_completion_function = prolog_completion;
#ifdef HAVE_RL_COMPLETION_FUNC_T
  rl_add_defun("prolog-complete", prolog_complete, '\t');
#else
  rl_add_defun("prolog-complete", (void *)prolog_complete, '\t');
#endif
  return Yap_ReadlineOps(GLOBAL_Stream + StdInStream);
}

static bool getLine(int inp, int out) {
  CACHE_REGS
  rl_instream = GLOBAL_Stream[inp].file;
  rl_outstream = GLOBAL_Stream[out].file;
  const unsigned char *myrl_line;
  StreamDesc *s = GLOBAL_Stream + inp;

  if (!(s->status & Tty_Stream_f))
    return false;

  /* window of vulnerability opened */
  fflush(NULL);
  LOCAL_PrologMode |= ConsoleGetcMode;
  if (LOCAL_newline) { // no output so far
    myrl_line = (unsigned char *)readline(LOCAL_Prompt);
  } else {
    myrl_line = (unsigned char *)readline(NULL);
  }
  /* Do it the gnu way */
  if (LOCAL_PrologMode & InterruptMode) {
    Yap_external_signal(0, YAP_INT_SIGNAL);
    LOCAL_PrologMode &= ~ConsoleGetcMode;
    if (LOCAL_PrologMode & AbortMode) {
      Yap_Error(ABORT_EVENT, TermNil, "");
      LOCAL_ErrorMessage = "Abort";
      return console_post_process_eof(s);
    }
  } else {
    LOCAL_PrologMode &= ~ConsoleGetcMode;
    LOCAL_newline = true;
  }
  strncpy(LOCAL_Prompt, RepAtom(LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
  /* window of vulnerability closed */
  if (myrl_line == NULL)
    return false;
  if (myrl_line[0] != '\0' && myrl_line[1] != '\0') {
    add_history((char *)myrl_line);
    append_history(1, history_file);
  }
  s->u.irl.ptr = s->u.irl.buf = myrl_line;
  return true;
}

static int ReadlinePutc(int sno, int ch) {
    CACHE_REGS
  StreamDesc *s = &GLOBAL_Stream[sno];
#if MAC || _MSC_VER || defined(__MINGW32__)
  if (ch == 10) {
    putc('\n', s->file);
  } else
#endif
    putc(ch, s->file);
  console_count_output_char(ch, s);
  if (ch == 10) {
    Yap_ReadlineFlush(sno);
    LOCAL_newline = true;
  }
  return ((int)ch);
}

/**
  @brief reading from the console is complicated because we need to
  know whether to prompt and so on...

  EOF must be handled by resetting the file.
*/
static int ReadlineGetc(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;
  bool fetch = (s->u.irl.buf == NULL);

  if (!fetch || getLine(sno, StdErrStream)) {
    const unsigned char *ttyptr = s->u.irl.ptr++, *myrl_line = s->u.irl.buf;
    ch = *ttyptr;
    if (ch == '\0') {
      ch = '\n';
      free((void *)myrl_line);
      s->u.irl.ptr = s->u.irl.buf = NULL;
    }
  } else {
    return EOF;
  }
  return console_post_process_read_char(ch, s);
}

/**
  @brief  Yap_ReadlinePeekChar peeks the next char from the
  readline buffer, but does not actually grab it.

  The idea is to take advantage of the buffering. Special care must be taken
  with EOF, though.

*/
Int Yap_ReadlinePeekChar(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;

  if (s->u.irl.buf) {
    const unsigned char *ttyptr = s->u.irl.ptr;
    ch = *ttyptr;
    if (ch == '\0') {
      ch = '\n';
    }
    return ch;
  }
  if (getLine(sno, StdErrStream)) {
    CACHE_REGS
    ch = s->u.irl.ptr[0];
    if (ch == '\0') {
      ch = '\n';
    }
    if (ch == '\n') {
      LOCAL_newline = true;
    } else {
      LOCAL_newline = false;
    }
  } else {
    return EOF;
  }
  return ch;
}

int Yap_ReadlineForSIGINT(void) {
  CACHE_REGS
  int ch;
  StreamDesc *s = &GLOBAL_Stream[StdInStream];
  const unsigned char *myrl_line = s->u.irl.buf;

  if ((LOCAL_PrologMode & ConsoleGetcMode) && myrl_line != NULL) {
    ch = myrl_line[0];
    free((void *)myrl_line);
    myrl_line = NULL;
    return ch;
  } else {
    myrl_line = (const unsigned char *)readline("Action (h for help): ");
    if (!myrl_line) {
      ch = EOF;
      return ch;
    } else {
      ch = myrl_line[0];
      free((void *)myrl_line);
      myrl_line = NULL;
      return ch;
    }
  }
}

static Int has_readline(USES_REGS1) {
#if USE_READLINE
  return true;
#else
  return false;
#endif
}

void Yap_InitReadlinePreds(void) {
  Yap_InitCPred("$has_readline", 0, has_readline,
                SafePredFlag | HiddenPredFlag);
}

#else
bool Yap_InitReadline(Term enable) {
  if (enable == TermTrue)
    return true;
  return false;
}

void Yap_InitReadlinePreds(void) {}
#endif
