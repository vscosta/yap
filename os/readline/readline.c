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
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/** @file readline.c
 *
 *
 * This file includes the interface to the readline library, if installed in the
 *system.
 *
 */

#include "Yap.h"
#include "YapHeap.h"
#include "Yatom.h"
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

#if defined(HAVE_LIBREADLINE)

#include <readline/history.h>
#include <readline/readline.h>

static const char *history_file;

#define READLINE_OUT_BUF_MAX 256

static Int usable_readline(USES_REGS1) {
#if USE_READLINE
  if ((!(GLOBAL_Stream[StdInStream].status & Tty_Stream_f) ||
       !(GLOBAL_Stream[StdOutStream].status & Tty_Stream_f))) {
    return false;
  }
  if (getenv("PMIX_ID")) {
    return false;
  }
  if (getenv("INSIDE_EMACS")) {
    return false;
  }
  if (Yap_Embedded) {
    return false;
  }
  return true;
#else
  return false;
#endif
}

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

      if (strstr(ap->StrOfAE, prefix) == (char *)ap->StrOfAE) {
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

typedef struct chain {
  struct chain *next;
  char data[2];
} chain_t;

static char *predicate_enumerate(const char *prefix, int state, char *buf0,
                                 size_t sz) {
  CACHE_REGS
  PredEntry *p;
  ModEntry m0, *mod;
  AtomEntry *ap;

  if (!state) {
    p = NULL;
    mod = &m0;
    m0.NextME = CurrentModules;
    if (mod->AtomOfME == AtomIDB)
      mod = mod->NextME;
  } else {
    Term cmod;
    p = LOCAL_SearchPreds;
    cmod = (p->ModuleOfPred != PROLOG_MODULE ? p->ModuleOfPred : TermProlog);
    mod = Yap_GetModuleEntry(cmod);
  }
  while (mod) {
    // move to next o;
    if (p)
      p = p->NextPredOfModule;
    while (p == NULL) {
      mod = mod->NextME;
      if (!mod) {
        // done
        LOCAL_SearchPreds = NULL;
        return NULL;
      }
      if (mod->AtomOfME == AtomIDB)
        mod = mod->NextME;
      p = mod->PredForME;
    }

    char *c = RepAtom(ap = NameOfPred(p))->StrOfAE;
    if (strlen(c) > strlen(prefix) && strstr(c, prefix) == c &&
        !(p->PredFlags & HiddenPredFlag)) {
      LOCAL_SearchPreds = p;
      arity_t ar = p->ArityOfPE;
      int l, r;
      if (Yap_IsPrefixOp(AbsAtom(ap), &l, &r) && ar == 1) {
        return c;
      }
      char *buf = buf0;
      strncpy(buf, c, sz);
      sz -= strlen(c);
      if (p->ArityOfPE)
        strncat(buf0, "(", MAX_PATH);
      return buf0;
    }
  }
  LOCAL_SearchPreds = NULL;
  return NULL;
}

static char *predicate_generator(const char *prefix, int state) {

  char *buf = malloc(PATH_MAX + 1);
  char *s = predicate_enumerate(prefix, state, buf, PATH_MAX);

  if (s) {
    char *copy = malloc(1 + strlen(s));

    if (copy) /* else pretend no completion */
      strcpy(copy, s);
    s = copy;
  }
  free(buf);
  return s;
}

static char **prolog_completion(const char *text, int start, int end) {
  char **matches = NULL;
  const char *pt = text + strlen(text), *pt0 = pt;

  while (--pt >= text) {
    if (isalnum(pt[0]) || pt[0] == '_')
      continue;
    else
      break;
  }
  matches = rl_completion_matches(pt + 1, predicate_generator);

  if (pt == pt0) {
    int i = 0;
    const char *p;
    while (isspace(text[i++]) && i <= end)
      ;
    p = text + i;

    if ((strstr(p, "[") == p) || (strstr(p, "compile(") == p) ||
        (strstr(p, "consult(") == p) || (strstr(p, "load_files(") == p) ||
        (strstr(p, "reconsult(") == p) || (strstr(p, "use_module(") == p) ||
        (strstr(p, "cd(") == p))
      matches = rl_completion_matches((char *)p, /* for pre-4.2 */
                                      rl_filename_completion_function);
    return matches;
  }
  return rl_completion_matches((char *)text, atom_generator);
}

void Yap_ReadlineFlush(int sno) {
  Yap_readline_clear_pending_input(GLOBAL_Stream+sno);
  return rl_redisplay();
}

bool Yap_readline_clear_pending_input(StreamDesc *s) {
  if (GLOBAL_Flags && trueGlobalPrologFlag(READLINE_FLAG)) {
    rl_line_buffer[0] = '\0';
    s->u.irl.ptr = s->u.irl.buf;
    return true;
  }
  return false;
}

bool Yap_ReadlineOps(StreamDesc *s) {
  if (GLOBAL_Flags && trueGlobalPrologFlag(READLINE_FLAG)) {
    if (GLOBAL_Stream[0].status & (Input_Stream_f | Tty_Stream_f) &&
        is_same_tty(s->file, GLOBAL_Stream[0].file)) {
      s->stream_getc = ReadlineGetc;
      s->stream_peek = Yap_ReadlinePeekChar;
      s->stream_wpeek = Yap_ReadlinePeekChar;
      s->status |= Readline_Stream_f;
    }
    return true;
  }
  return false;
}
extern bool Yap_Embedded;

bool Yap_InitReadline(Term enable) {
  // don't call readline within emacs
  CACHE_REGS
  if (enable != TermTrue || !usable_readline(PASS_REGS1)) {
    if (GLOBAL_Flags)
      setBooleanGlobalPrologFlag(READLINE_FLAG, false);
    return false;
  }
  GLOBAL_Stream[StdInStream].u.irl.buf = NULL;
  GLOBAL_Stream[StdInStream].u.irl.ptr = NULL;
  GLOBAL_Stream[StdInStream].status |= Readline_Stream_f;
#if _WIN32
  rl_instream = stdin;
#endif
  // rl_outstream = stderr;
  using_history();
  const char *s = Yap_AbsoluteFile("~/.YAP.history", true);
  history_file = s;
  if (read_history(s) != 0) {
    FILE *f = fopen(s, "a");
    if (f) {
      fclose(f);
    }
  }
  rl_readline_name = "YAP Prolog";
  rl_attempted_completion_function = prolog_completion;
  // rl_prep_terminal(1);
  if (GLOBAL_Flags)
    setBooleanGlobalPrologFlag(READLINE_FLAG, true);
  return Yap_ReadlineOps(GLOBAL_Stream + StdInStream);
}

#if !HAVE_RL_SET_SIGNALS
#define rl_clear_signals()
#define rl_set_signals()

#endif

static bool getLine(int inp) {
  CACHE_REGS
  rl_instream = GLOBAL_Stream[inp].file;
  const unsigned char *myrl_line = NULL;
  StreamDesc *s = GLOBAL_Stream + inp;
  LOCAL_PrologMode |= ConsoleGetcMode;
  //rl_set_signals();
  //rl_catch_signals=false;
  myrl_line = (unsigned char *)readline(LOCAL_Prompt);
  //rl_clear_signals();
  LOCAL_PrologMode &= ~ConsoleGetcMode;
     strncpy(LOCAL_Prompt, RepAtom(LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
  /* window of vulnerability closed */
  if (myrl_line == NULL)
    return false;
  if (myrl_line[0] != '\0' && myrl_line[1] != '\0') {
    add_history((char *)myrl_line);
    history_truncate_file(history_file, 300);
    write_history(history_file);
  }
  s->u.irl.ptr = s->u.irl.buf = myrl_line;
  myrl_line = NULL;
  return true;
}

/**
  @brief reading from the console is complicated because we need to
  know whether to prompt and so on...

  EOF must be handled by resetting the file.
*/
int ReadlineGetc(int sno) {
    CACHE_REGS
  StreamDesc *s = &GLOBAL_Stream[sno];
  int ch = 0;
  bool fetch = (s->u.irl.buf == NULL);

  if (fetch) {
    /* window of vulnerability opened */
    LOCAL_PrologMode |= ConsoleGetcMode;
    if (!getLine(sno)) {
      return console_post_process_eof(s);
    }
  }
  const unsigned char *ttyptr = s->u.irl.ptr, *myrl_line = s->u.irl.buf;
  if (!myrl_line)
    ch = '\n';
  else {
    ch = *ttyptr;
    if (ch == '\0') {
      ch = '\n';
      free((void *)myrl_line);
      s->u.irl.ptr = s->u.irl.buf = NULL;
    } else {
      s->u.irl.ptr++;
    }
  }
  return console_post_process_read_char(ch, s);
}

/**
  @brief  Yap_ReadlinePeekChar peeks the next char from the
  readline buffer, but does not actually grab it.

  The idea is to take advantage of the buffering. Special care must be taken
  with EOF, though.

*/
int Yap_ReadlinePeekChar(int sno) {
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
  ch = 0;
  if (getLine(sno)) {
    CACHE_REGS
    if (!ch)
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
    ch = EOF;
  }
  return ch;
}

int Yap_ReadlineForSIGINT(void) {
  int ch;
  //StreamDesc *s = &GLOBAL_Stream[StdInStream];
  const unsigned char *myrl_line;
  rl_prep_terminal(1);
    
    myrl_line = (const unsigned char *)readline("RK Action (h for help): ");
    while (!myrl_line || myrl_line[0] == '\0') {};
    ch = myrl_line[0];
    return ch;
    }

#else


bool Yap_InitReadline(Term enable) {
  if (GLOBAL_Flags)
    setBooleanGlobalPrologFlag(READLINE_FLAG, false);
  return false;
}
#endif

static Int has_readline(USES_REGS1) { return usable_readline(PASS_REGS1); }

void Yap_InitReadlinePreds(void) {
  Yap_InitCPred("$has_readline", 0, has_readline,
                SafePredFlag | HiddenPredFlag);
}

void Yap_CloseReadline(void) {
#if USE_READLINE
  write_history(history_file);
  history_truncate_file(history_file, 300);
#endif
}
