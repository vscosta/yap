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
 * File:		iopreds.c *
 * Last rev:	5/2/88							 *
 * mods: *
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
#include "eval.h"
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
#define strncat(X, Y, Z) strcat(X, Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#include "iopreds.h"

static int get_wchar(int);
static int get_wchar_from_file(int);

FILE *Yap_stdin;
FILE *Yap_stdout;
FILE *Yap_stderr;

static bool issolutions(Term t) {
  if (t == TermFirst || t == TermAll)
    return true;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "solutions in {first, all}.");
    return false;
  }
  if (IsAtomTerm(t)) {
    Yap_Error(DOMAIN_ERROR_SOLUTIONS, t, "solutions in {first, all}");
    return false;
  }
  Yap_Error(TYPE_ERROR_ATOM, t, "solutions in {first, all}}");
  return false;
}

static bool is_file_type(Term t) {
  if (t == TermTxt || t == TermProlog || t == TermSource ||
      t == TermExecutable || t == TermQly || t == TermDirectory)
    return true;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t,
              "file_type in {txt,prolog,exe,directory...}");
    return false;
  }
  if (IsAtomTerm(t)) {
    Yap_Error(DOMAIN_ERROR_FILE_TYPE, t,
              "file_type in {txt,prolog,exe,directory...}");
    return false;
  }
  Yap_Error(TYPE_ERROR_ATOM, t, "file_type in {txt,prolog,exe,directory...}");
  return false;
}

static bool is_file_errors(Term t) {
  if (t == TermFail || t == TermError)
    return true;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "file_error in {fail,error}.");
    return false;
  }
  if (IsAtomTerm(t)) {
    Yap_Error(DOMAIN_ERROR_FILE_ERRORS, t, "file_error in {fail,error}.");
    return false;
  }
  Yap_Error(TYPE_ERROR_ATOM, t, "file_error in {fail,error}.");
  return false;
}

void Yap_DefaultStreamOps(StreamDesc *st) {
  st->stream_wputc = put_wchar;
  if (!(st->status & (Tty_Stream_f | Reset_Eof_Stream_f | Promptable_Stream_f)))
    st->stream_wgetc = get_wchar_from_file;
  else
    st->stream_wgetc = get_wchar;
  if (GLOBAL_CharConversionTable != NULL)
    st->stream_wgetc_for_read = ISOWGetc;
  else
    st->stream_wgetc_for_read = st->stream_wgetc;
  if (st->encoding == ENC_ISO_UTF8)
    st->stream_getc_for_utf8 = st->stream_getc;
  else
    st->stream_getc_for_utf8 = GetUTF8;
}

static void unix_upd_stream_info(StreamDesc *s) {
  if (s->status & InMemory_Stream_f) {
    s->status |= Seekable_Stream_f;
    return;
  }
  Yap_socketStream(s);
#if _MSC_VER || defined(__MINGW32__)
  {
    if (_isatty(_fileno(s->u.file.file))) {
      s->status |= Tty_Stream_f | Reset_Eof_Stream_f | Promptable_Stream_f;
      /* make all console descriptors unbuffered */
      setvbuf(s->u.file.file, NULL, _IONBF, 0);
      return;
    }
#if _MSC_VER
    /* standard error stream should never be buffered */
    else if (StdErrStream == s - Stream) {
      setvbuf(s->u.file.file, NULL, _IONBF, 0);
    }
#endif
    s->status |= Seekable_Stream_f;
    return;
  }
#else
#if HAVE_ISATTY
#if __simplescalar__
  /* isatty does not seem to work with simplescar. I'll assume the first
   three streams will probably be ttys (pipes are not thatg different) */
  if (s - Stream < 3) {
    s->name = AtomTty;
    s->status |= Tty_Stream_f | Reset_Eof_Stream_f | Promptable_Stream_f;
  }
#else
  {
    int filedes; /* visualc */
    if (!s->file) {
      s->name = AtomNil;
      return;
    }
    filedes = fileno(s->file);
    if (isatty(filedes)) {
#if HAVE_TTYNAME
      char *ttys = ttyname(filedes);
      if (ttys == NULL)
        s->name = AtomTty;
      else
        s->name = AtomTtys;
#else
      s->name = AtomTty;
#endif
      s->status |= Tty_Stream_f | Reset_Eof_Stream_f | Promptable_Stream_f;
      return;
    }
  }
#endif
#endif /* HAVE_ISATTY */
#endif /* _MSC_VER */
  s->status |= Seekable_Stream_f;
}

GetsFunc PlGetsFunc(void) {
  if (GLOBAL_CharConversionTable)
    return DefaultGets;
  else
    return PlGets;
}

static void InitFileIO(StreamDesc *s) {
  s->stream_gets = PlGetsFunc();
  if (s->status & Socket_Stream_f) {
    /* Console is a socket and socket will prompt */
    Yap_ConsoleSocketOps(s);
    s->stream_wputc = put_wchar;
  } else if (s->status & Pipe_Stream_f) {
    /* Console is a socket and socket will prompt */
    Yap_ConsolePipeOps(s);
    s->stream_wputc = put_wchar;
  } else if (s->status & InMemory_Stream_f) {
    Yap_MemOps(s);
    s->stream_wputc = put_wchar;
  } else {
    /* check if our console is promptable: may be tty or pipe */
    if (s->status & (Promptable_Stream_f)) {
      Yap_ConsoleOps(s);
    } else {
      /* we are reading from a file, no need to check for prompts */
      s->stream_putc = FilePutc;
      s->stream_wputc = put_wchar;
      s->stream_getc = PlGetc;
      s->stream_gets = PlGetsFunc();
      s->stream_wgetc = get_wchar_from_file;
    }
  }
  s->stream_wputc = put_wchar;
  s->stream_wgetc = get_wchar;
}

static void InitStdStream(int sno, SMALLUNSGN flags, FILE *file) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  s->file = file;
  s->status = flags;
  s->linepos = 0;
  s->linecount = 1;
  s->charcount = 0.;
  s->encoding = ENC_ISO_UTF8;
  INIT_LOCK(s->streamlock);
  unix_upd_stream_info(s);
  /* Getting streams to prompt is a mess because we need for cooperation
   between readers and writers to the stream :-(
   */
  InitFileIO(s);
  switch (sno) {
  case 0:
    s->name = AtomUserIn;
    break;
  case 1:
    s->name = AtomUserOut;
    break;
  default:
    s->name = AtomUserErr;
    break;
  }
  s->user_name = MkAtomTerm(s->name);
  Yap_DefaultStreamOps(s);
#if LIGHT
  s->status |= Tty_Stream_f | Promptable_Stream_f;
#endif
#if HAVE_SETBUF
  if (s->status & Tty_Stream_f && sno == 0) {
    /* make sure input is unbuffered if it comes from stdin, this
     makes life simpler for interrupt handling */
    setbuf(stdin, NULL);
    //    fprintf(stderr,"here I am\n");
  }
#endif /* HAVE_SETBUF */
}

Term Yap_StreamUserName(int sno) {
  Term atname;
  StreamDesc *s = &GLOBAL_Stream[sno];
  if (s->user_name != 0L) {
    return (s->user_name);
  }
  if ((atname = StreamName(sno)))
    return atname;
  return TermNil;
}

static void InitStdStreams(void) {
  CACHE_REGS
  if (LOCAL_sockets_io) {
    InitStdStream(StdInStream, Input_Stream_f, NULL);
    InitStdStream(StdOutStream, Output_Stream_f, NULL);
    InitStdStream(StdErrStream, Output_Stream_f, NULL);
  } else {
    InitStdStream(StdInStream, Input_Stream_f, stdin);
    InitStdStream(StdOutStream, Output_Stream_f, stdout);
    InitStdStream(StdErrStream, Output_Stream_f, stderr);
  }
  GLOBAL_Stream[StdInStream].name = Yap_LookupAtom("user_input");
  GLOBAL_Stream[StdOutStream].name = Yap_LookupAtom("user_output");
  GLOBAL_Stream[StdErrStream].name = Yap_LookupAtom("user_error");
  LOCAL_c_input_stream = StdInStream;
  LOCAL_c_output_stream = StdOutStream;
  LOCAL_c_error_stream = StdErrStream;
}

void Yap_InitStdStreams(void) { InitStdStreams(); }

Int PlIOError__(const char *file, const char *function, int lineno,
                yap_error_number type, Term culprit, ...) {
  if (trueLocalPrologFlag(FILEERRORS_FLAG) ||
      type == RESOURCE_ERROR_MAX_STREAMS /* do not catch resource errors */) {
    va_list args;
    const char *format;
    char who[1024];

    va_start(args, culprit);
    format = va_arg(args, char *);
    if (format) {
      vsnprintf(who, 1023, format, args);
    } else {
      who[0] = '\0';
    }
    va_end(args);
    Yap_Error__(file, function, lineno, type, culprit, who);
    /* and fail */
    return false;
  } else {
    return false;
  }
}

#ifdef DEBUG

static int eolflg = 1;

static char my_line[200] = {0};
static char *lp = my_line;

FILE *curfile, *Yap_logfile;

bool Yap_Option[256];

#ifdef MACC

static void InTTYLine(char *line) {
  char *p = line;
  char ch;
  while ((ch = InKey()) != '\n' && ch != '\r')
    if (ch == 8) {
      if (line < p)
        BackupTTY(*--p);
    } else
      TTYChar(*p++ = ch);
  TTYChar('\n');
  *p = 0;
}
#endif

void Yap_DebugSetIFile(char *fname) {
  if (curfile)
    fclose(curfile);
  curfile = fopen(fname, "r");
  if (curfile == NULL) {
    curfile = stdin;
    Yap_Warning("%% YAP  open %s for input\n", fname);
  }
}

void Yap_DebugEndline() { *lp = 0; }

int Yap_DebugGetc() {
  int ch;
  if (eolflg) {
    if (curfile != NULL) {
      if (fgets(my_line, 200, curfile) == 0)
        curfile = NULL;
    }
    if (curfile == NULL)
      if (fgets(my_line, 200, stdin) == NULL) {
        return EOF;
      }
    eolflg = 0;
    lp = my_line;
  }
  if ((ch = *lp++) == 0)
    ch = '\n', eolflg = 1;
  if (Yap_Option['l' - 96])
    putc(ch, Yap_logfile);
  return (ch);
}

int Yap_DebugPutc(FILE *s, wchar_t ch) {
  if (Yap_Option['l' - 96])
    (void)putc(ch, Yap_logfile);
  return (putc(ch, s));
}

int Yap_DebugPuts(FILE *s, const char *sch) {
  if (Yap_Option['l' - 96])
    (void)fputs(sch, Yap_logfile);
  return fputs(sch, s);
}

void Yap_DebugErrorPuts(const char *s) { Yap_DebugPuts(stderr, s); }

void Yap_DebugPlWrite(Term t) {
  if (t != 0)
    Yap_plwrite(t, GLOBAL_Stream + 2, 0, 0, GLOBAL_MaxPriority);
}

void Yap_DebugPlWriteln(Term t) {
  CACHE_REGS
  Yap_plwrite(t, NULL, 15, 0, GLOBAL_MaxPriority);
  Yap_DebugPutc(GLOBAL_Stream[LOCAL_c_error_stream].file, '.');
  Yap_DebugPutc(GLOBAL_Stream[LOCAL_c_error_stream].file, 10);
}

void Yap_DebugErrorPutc(int c) {
  CACHE_REGS
  Yap_DebugPutc(GLOBAL_Stream[LOCAL_c_error_stream].file, c);
}

void Yap_DebugWriteIndicator(PredEntry *ap) {
  CACHE_REGS
  Term tmod = ap->ModuleOfPred;
  if (!tmod)
    tmod = TermProlog;
#if THREADS
  Yap_DebugPlWrite(MkIntegerTerm(worker_id));
  Yap_DebugPutc(stderr, ' ');
#endif
  Yap_DebugPutc(stderr, '>');
  Yap_DebugPutc(stderr, '\t');
  Yap_DebugPlWrite(tmod);
  Yap_DebugPutc(stderr, ':');
  if (ap->ModuleOfPred == IDB_MODULE) {
    Term t = Deref(ARG1);
    if (IsAtomTerm(t)) {
      Yap_DebugPlWrite(t);
    } else if (IsIntegerTerm(t)) {
      Yap_DebugPlWrite(t);
    } else {
      Functor f = FunctorOfTerm(t);
      Atom At = NameOfFunctor(f);
      Yap_DebugPlWrite(MkAtomTerm(At));
      Yap_DebugPutc(stderr, '/');
      Yap_DebugPlWrite(MkIntegerTerm(ArityOfFunctor(f)));
    }
  } else {
    if (ap->ArityOfPE == 0) {
      Atom At = (Atom)ap->FunctorOfPred;
      Yap_DebugPlWrite(MkAtomTerm(At));
    } else {
      Functor f = ap->FunctorOfPred;
      Atom At = NameOfFunctor(f);
      Yap_DebugPlWrite(MkAtomTerm(At));
      Yap_DebugPutc(stderr, '/');
      Yap_DebugPlWrite(MkIntegerTerm(ArityOfFunctor(f)));
    }
  }

  Yap_DebugPutc(stderr, '\n');
}

#endif

/* static */
int FilePutc(int sno, int ch) {
  StreamDesc *s = &GLOBAL_Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10) {
    ch = '\n';
  }
#endif
  putc(ch, s->file);
#if MAC || _MSC_VER
  if (ch == 10) {
    fflush(s->file);
  }
#endif
  count_output_char(ch, s);
  return ((int)ch);
}

static int NullPutc(int sno, int ch) {
  StreamDesc *s = &GLOBAL_Stream[sno];
#if MAC || _MSC_VER
  if (ch == 10) {
    ch = '\n';
  }
#endif
  count_output_char(ch, s);
  return ((int)ch);
}

int ResetEOF(StreamDesc *s) {
  s->status &= ~Push_Eof_Stream_f;
  if (s->status & Eof_Error_Stream_f) {
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, MkAtomTerm(s->name),
              "GetC");
    return FALSE;
  } else if (s->status & Reset_Eof_Stream_f) {
    /* reset the eof indicator on file */
    if (feof(s->file))
      clearerr(s->file);
/* reset our function for reading input */
#if HAVE_SOCKET
    if (s->status & Socket_Stream_f) {
      if (s->status & Promptable_Stream_f)
        Yap_ConsoleSocketOps(s);
      else
        Yap_SocketOps(s);
      s->stream_wputc = put_wchar;
    } else
#endif
        if (s->status & Pipe_Stream_f) {
      if (s->status & Promptable_Stream_f)
        Yap_ConsolePipeOps(s);
      else
        Yap_PipeOps(s);
    } else if (s->status & InMemory_Stream_f) {
      Yap_MemOps(s);
    } else if (s->status & Promptable_Stream_f) {
      Yap_ConsoleOps(s);
    } else {
      s->stream_getc = PlGetc;
      Yap_DefaultStreamOps(s);
      s->stream_gets = PlGetsFunc();
    }
    /* next, reset our own error indicator */
    s->status &= ~Eof_Stream_f;
    /* try reading again */
    return TRUE;
  } else {
    s->status |= Past_Eof_Stream_f;
    return FALSE;
  }
}

/* handle reading from a stream after having found an EOF */
static int EOFWGetc(int sno) {
  register StreamDesc *s = &GLOBAL_Stream[sno];

  if (s->status & Push_Eof_Stream_f) {
    /* ok, we have pushed an EOF, send it away */
    s->status &= ~Push_Eof_Stream_f;
    return EOF;
  }
  if (ResetEOF(s)) {
    Yap_ConsoleOps(s); 
    return (s->stream_wgetc(sno));
  }
  return EOF;
}

static int EOFGetc(int sno) {
  register StreamDesc *s = &GLOBAL_Stream[sno];

  if (s->status & Push_Eof_Stream_f) {
    /* ok, we have pushed an EOF, send it away */
    s->status &= ~Push_Eof_Stream_f;
    ResetEOF(s);
    return EOF;
  }
  if (ResetEOF(s)) {
    Yap_ConsoleOps(s); 
    return s->stream_getc(sno);
  }
  return EOF;
}

/* check if we read a LOCAL_newline or an EOF */
int console_post_process_eof(StreamDesc *s) {
  CACHE_REGS
  if (!ResetEOF(s)) {
    s->status |= Eof_Stream_f;
    s->stream_getc = EOFGetc;
    s->stream_wgetc = EOFWGetc;
    s->stream_wgetc_for_read = EOFWGetc;
    s->stream_getc_for_utf8 = EOFGetc;
    LOCAL_newline = true;
  }
  return EOFCHAR;
}

/* check if we read a newline or an EOF */
int post_process_read_char(int ch, StreamDesc *s) {
  ++s->charcount;
  ++s->linepos;
  if (ch == '\n') {
    ++s->linecount;
    s->linepos = 0;
    /* don't convert if the stream is binary */
    if (!(s->status & Binary_Stream_f))
      ch = 10;
  }
  return ch;
}

/* check if we read a newline or an EOF */
int post_process_eof(StreamDesc *s) {
  if (!ResetEOF(s)) {
    s->status |= Eof_Stream_f;
    s->stream_wgetc = EOFWGetc;
    s->stream_getc = EOFGetc;
    s->stream_wgetc_for_read = EOFWGetc;
    s->stream_getc_for_utf8 = EOFGetc;
  }
  return EOFCHAR;
}

int post_process_weof(StreamDesc *s) {
  if (!ResetEOF(s)) {
    s->status |= Eof_Stream_f;
    s->stream_wgetc = EOFWGetc;
    s->stream_wgetc = EOFWGetc;
    s->stream_wgetc_for_read = EOFWGetc;
    s->stream_getc_for_utf8 = EOFGetc;
  }
  return EOFCHAR;
}

/** 
 * caled after EOF found a peek, it just calls console_post_process to conclude the job.
 * 
 * @param sno 
 * 
 * @return EOF
 */
int EOFPeek(int sno) {
  return EOFGetc( sno );
}

int EOFWPeek(int sno) {
  return EOFWGetc( sno );
}

/* standard routine, it should read from anything pointed by a FILE *.
 It could be made more efficient by doing our own buffering and avoiding
 post_process_read_char, something to think about */
int PlGetc(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  Int ch;

  ch = getc(s->file);
  if (ch == EOF) {
    return post_process_eof(s);
  }
  return post_process_read_char(ch, s);
}

/* standard routine, it should read from anything pointed by a FILE *.
 It could be made more efficient by doing our own buffering and avoiding
 post_process_read_char, something to think about. It assumes codification in 8
 bits. */
int PlGets(int sno, UInt size, char *buf) {
  register StreamDesc *s = &GLOBAL_Stream[sno];
  UInt len;

  if (fgets(buf, size, s->file) == NULL) {
    return post_process_eof(s);
  }
  len = strlen(buf);
  s->charcount += len - 1;
  post_process_read_char(buf[len - 2], s);
  return strlen(buf);
}

/* standard routine, it should read from anything pointed by a FILE *.
 It could be made more efficient by doing our own buffering and avoiding
 post_process_read_char, something to think about */
int DefaultGets(int sno, UInt size, char *buf) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  char ch;
  char *pt = buf;

  if (!size)
    return 0;
  while ((ch = *buf++ = s->stream_getc(sno)) != -1 && ch != 10 && --size)
    ;
  *buf++ = '\0';
  return (buf - pt) - 1;
}

int GetUTF8(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  uint64_t bufi = s->utf8_buf;
  unsigned char *buf = (unsigned char *)&bufi;

  if (!bufi) {
    int32_t ch = get_wchar(sno);
    if (ch < 128)
      return ch;
    put_utf8((unsigned char *)&bufi, ch);
  } else {
    while (*buf++ == '\0')
      ;
  }
  unsigned char c = *buf;
  buf[0] = '\0';
  return c;
}

static int utf8_nof(char ch) {
  if (!(ch & 0x20))
    return 1;
  if (!(ch & 0x10))
    return 2;
  if (!(ch & 0x08))
    return 3;
  if (!(ch & 0x04))
    return 4;
  return 5;
}

#define wide_char()                                                            \
  switch (GLOBAL_Stream[sno].encoding) {                                       \
  case ENC_OCTET:                                                              \
    return ch;                                                                 \
  case ENC_ISO_LATIN1:                                                         \
    return ch;                                                                 \
  case ENC_ISO_ASCII:                                                          \
    if (ch & 0x80) {                                                           \
      /* error */                                                              \
    }                                                                          \
    return ch;                                                                 \
  case ENC_ISO_ANSI: {                                                         \
    char buf[1];                                                               \
    int out;                                                                   \
                                                                               \
    if (!how_many) {                                                           \
      memset((void *)&(GLOBAL_Stream[sno].mbstate), 0, sizeof(mbstate_t));     \
    }                                                                          \
    buf[0] = ch;                                                               \
    if ((out = mbrtowc(&wch, buf, 1, &(GLOBAL_Stream[sno].mbstate))) == 1)     \
      return wch;                                                              \
    if (out == -1) {                                                           \
      /* error */                                                              \
    }                                                                          \
    how_many++;                                                                \
    break;                                                                     \
  }                                                                            \
  case ENC_ISO_UTF8: {                                                         \
    if (!how_many) {                                                           \
      if (ch & 0x80) {                                                         \
        how_many = utf8_nof(ch);                                               \
        /*                                                                     \
         keep a backup of the start character in case we meet an error,        \
         useful if we are scanning ISO files.                                  \
         */                                                                    \
        GLOBAL_Stream[sno].och = ch;                                           \
        wch = (ch & ((1 << (6 - how_many)) - 1)) << (6 * how_many);            \
      } else {                                                                 \
        return ch;                                                             \
      }                                                                        \
    } else {                                                                   \
      how_many--;                                                              \
      if ((ch & 0xc0) == 0x80) {                                               \
        wch += (ch & ~0xc0) << (how_many * 6);                                 \
      } else {                                                                 \
        /* error */                                                            \
        /* try to recover character, assume this is our first character */     \
        wchar_t och = GLOBAL_Stream[sno].och;                                  \
        return och;                                                            \
      }                                                                        \
      if (!how_many) {                                                         \
        return wch;                                                            \
      }                                                                        \
    }                                                                          \
  } break;                                                                     \
  case ENC_UTF16_BE:                                                           \
    if (how_many) {                                                            \
      return wch + ch;                                                         \
    }                                                                          \
    how_many = 1;                                                              \
    wch = ch << 8;                                                             \
    break;                                                                     \
  case ENC_UTF16_LE:                                                           \
    if (how_many) {                                                            \
      return wch + (ch << 8);                                                  \
    }                                                                          \
    how_many = 1;                                                              \
    wch = ch;                                                                  \
    break;                                                                     \
  case ENC_ISO_UTF32_LE:                                                       \
    if (!how_many) {                                                           \
      how_many = 4;                                                            \
      wch = 0;                                                                 \
    }                                                                          \
    how_many--;                                                                \
    wch += ((unsigned char)(ch & 0xff)) << (how_many * 8);                     \
    if (how_many == 0)                                                         \
      return wch;                                                              \
    break;                                                                     \
  case ENC_ISO_UTF32_BE:                                                       \
    if (!how_many) {                                                           \
      how_many = 4;                                                            \
      wch = 0;                                                                 \
    }                                                                          \
    how_many--;                                                                \
    wch += ((unsigned char)(ch & 0xff)) << ((3 - how_many) * 8);               \
    if (how_many == 0)                                                         \
      return wch;                                                              \
    break;                                                                     \
  }

static int get_wchar(int sno) {
  int ch;
  wchar_t wch;
  int how_many = 0;

  while (true) {
    ch = GLOBAL_Stream[sno].stream_getc(sno);
    if (ch == -1) {
      if (how_many) {
        /* error */
      }
      return post_process_weof(GLOBAL_Stream+sno);
    }
    wide_char();
  }
  return EOF;
}

// layered version
static int get_wchar__(int sno) {
  int ch;
  wchar_t wch;
  int how_many = 0;
  StreamDesc *s = GLOBAL_Stream + sno;

  while (TRUE) {
    ch = getc(GLOBAL_Stream[sno].file);
    if (ch == -1) {
      if (how_many) {
        /* error */
      }
      return post_process_weof(s);
    }
    wide_char();
  }
  return EOF;
}

static int get_wchar_from_file(int sno) {
  return post_process_read_char(get_wchar__(sno), GLOBAL_Stream + sno);
}

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

static int handle_write_encoding_error(int sno, wchar_t ch) {
  if (GLOBAL_Stream[sno].status & RepError_Xml_f) {
    /* use HTML/XML encoding in ASCII */
    int i = ch, digits = 1;
    GLOBAL_Stream[sno].stream_putc(sno, '&');
    GLOBAL_Stream[sno].stream_putc(sno, '#');
    while (digits < i)
      digits *= 10;
    if (digits > i)
      digits /= 10;
    while (i) {
      GLOBAL_Stream[sno].stream_putc(sno, i / digits);
      i %= 10;
      digits /= 10;
    }
    GLOBAL_Stream[sno].stream_putc(sno, ';');
    return ch;
  } else if (GLOBAL_Stream[sno].status & RepError_Prolog_f) {
    /* write quoted */
    GLOBAL_Stream[sno].stream_putc(sno, '\\');
    GLOBAL_Stream[sno].stream_putc(sno, 'u');
    GLOBAL_Stream[sno].stream_putc(sno, ch >> 24);
    GLOBAL_Stream[sno].stream_putc(sno, 256 & (ch >> 16));
    GLOBAL_Stream[sno].stream_putc(sno, 256 & (ch >> 8));
    GLOBAL_Stream[sno].stream_putc(sno, 256 & ch);
    return ch;
  } else {
    CACHE_REGS
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, MkIntegerTerm(ch),
              "charater %ld cannot be encoded in stream %d",
              (unsigned long int)ch, sno);
    return -1;
  }
}

int put_wchar(int sno, wchar_t ch) {
  /* pass the bucck if we can */
  switch (GLOBAL_Stream[sno].encoding) {
  case ENC_OCTET:
    return GLOBAL_Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_LATIN1:
    if (ch >= 0xff) {
      return handle_write_encoding_error(sno, ch);
    }
    return GLOBAL_Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_ASCII:
    if (ch >= 0x80) {
      return handle_write_encoding_error(sno, ch);
    }
    return GLOBAL_Stream[sno].stream_putc(sno, ch);
  case ENC_ISO_ANSI: {
    char buf[MB_LEN_MAX];
    int n;

    memset((void *)&(GLOBAL_Stream[sno].mbstate), 0, sizeof(mbstate_t));
    if ((n = wcrtomb(buf, ch, &(GLOBAL_Stream[sno].mbstate))) < 0) {
      /* error */
      GLOBAL_Stream[sno].stream_putc(sno, ch);
      return -1;
    } else {
      int i;

      for (i = 0; i < n; i++) {
        GLOBAL_Stream[sno].stream_putc(sno, buf[i]);
      }
      return ch;
    }
  case ENC_ISO_UTF8:
    if (ch < 0x80) {
      return GLOBAL_Stream[sno].stream_putc(sno, ch);
    } else if (ch < 0x800) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xC0 | ch >> 6);
      return GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else if (ch < 0x10000) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xE0 | ch >> 12);
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 6 & 0x3F));
      return GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else if (ch < 0x200000) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xF0 | ch >> 18);
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 12 & 0x3F));
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 6 & 0x3F));
      return GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else {
      /* should never happen */
      return -1;
    }
    break;
  case ENC_UTF16_BE:
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
    return GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
  case ENC_UTF16_LE:
    GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
    return GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
  case ENC_ISO_UTF32_BE:
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 24) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 16) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8) & 0xff);
    return GLOBAL_Stream[sno].stream_putc(sno, ch & 0xff);
  case ENC_ISO_UTF32_LE:
    GLOBAL_Stream[sno].stream_putc(sno, ch & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 16) & 0xff);
    return GLOBAL_Stream[sno].stream_putc(sno, (ch >> 24) & 0xff);
  }
  }
  return -1;
}

/* used by user-code to read characters from the current input stream */
int Yap_PlGetchar(void) {
  CACHE_REGS
  return (
      GLOBAL_Stream[LOCAL_c_input_stream].stream_getc(LOCAL_c_input_stream));
}

int Yap_PlGetWchar(void) {
  CACHE_REGS
  return get_wchar(LOCAL_c_input_stream);
}

/* avoid using a variable to call a function */
int Yap_PlFGetchar(void) {
  CACHE_REGS
  return (PlGetc(LOCAL_c_input_stream));
}

Term Yap_MkStream(int n) {
  Term t[1];
  t[0] = MkIntTerm(n);
  return (Yap_MkApplTerm(FunctorStream, 1, t));
}

/* given a stream index, get the corresponding fd */
Int GetStreamFd(int sno) {
#if HAVE_SOCKET
  if (GLOBAL_Stream[sno].status & Socket_Stream_f) {
    return (GLOBAL_Stream[sno].u.socket.fd);
  } else
#endif
      if (GLOBAL_Stream[sno].status & Pipe_Stream_f) {
#if _MSC_VER || defined(__MINGW32__)
    return ((Int)(GLOBAL_Stream[sno].u.pipe.hdl));
#else
    return (GLOBAL_Stream[sno].u.pipe.fd);
#endif
  } else if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
    return (-1);
  }
  return (fileno(GLOBAL_Stream[sno].file));
}

Int Yap_GetStreamFd(int sno) { return GetStreamFd(sno); }

static int binary_file(char *file_name) {
#if HAVE_STAT
#if _MSC_VER || defined(__MINGW32__)
  struct _stat ss;
  if (_stat(file_name, &ss) != 0)
#else
  struct stat ss;
  if (stat(file_name, &ss) != 0)
#endif
  {
    /* ignore errors while checking a file */
    return (FALSE);
  }
  return (S_ISDIR(ss.st_mode));
#else
  return (FALSE);
#endif
}

static int write_bom(int sno, StreamDesc *st) {
  /* dump encoding */
  switch (st->encoding) {
  case ENC_ISO_UTF8:
    if (st->stream_putc(sno, 0xEF) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xBB) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xBF) < 0)
      return FALSE;
    st->status |= HAS_BOM_f;
    return TRUE;
  case ENC_UTF16_BE:
    if (st->stream_putc(sno, 0xFE) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xFF) < 0)
      return FALSE;
    st->status |= HAS_BOM_f;
    return TRUE;
  case ENC_UTF16_LE:
    if (st->stream_putc(sno, 0xFF) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xFE) < 0)
      return FALSE;
  case ENC_ISO_UTF32_BE:
    if (st->stream_putc(sno, 0x00) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0x00) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xFE) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xFF) < 0)
      return FALSE;
  case ENC_ISO_UTF32_LE:
    if (st->stream_putc(sno, 0xFF) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0xFE) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0x00) < 0)
      return FALSE;
    if (st->stream_putc(sno, 0x00) < 0)
      return FALSE;
  default:
    return TRUE;
  }
}

static void check_bom(int sno, StreamDesc *st) {
  int ch1, ch2, ch3, ch4;

  ch1 = st->stream_getc(sno);
  switch (ch1) {
  case 0x00: {
    ch2 = st->stream_getc(sno);
    if (ch2 != 0x00) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return;
    } else {
      ch3 = st->stream_getc(sno);
      if (ch3 == EOFCHAR || ch3 != 0xFE) {
        ungetc(ch1, st->file);
        ungetc(ch2, st->file);
        ungetc(ch3, st->file);
        return;
      } else {
        ch4 = st->stream_getc(sno);
        if (ch4 == EOFCHAR || ch3 != 0xFF) {
          ungetc(ch1, st->file);
          ungetc(ch2, st->file);
          ungetc(ch3, st->file);
          ungetc(ch4, st->file);
          return;
        } else {
          st->status |= HAS_BOM_f;
          st->encoding = ENC_ISO_UTF32_BE;
          return;
        }
      }
    }
  }
  case 0xFE: {
    ch2 = st->stream_getc(sno);
    if (ch2 != 0xFF) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return;
    } else {
      st->status |= HAS_BOM_f;
      st->encoding = ENC_UTF16_BE;
      return;
    }
  }
  case 0xFF: {
    ch2 = st->stream_getc(sno);
    if (ch2 != 0xFE) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return;
    } else {
      ch3 = st->stream_getc(sno);
      if (ch3 != 0x00) {
        ungetc(ch1, st->file);
        ungetc(ch2, st->file);
        ungetc(ch3, st->file);
        return;
      } else {
        ch4 = st->stream_getc(sno);
        if (ch4 != 0x00) {
          ungetc(ch1, st->file);
          ungetc(ch2, st->file);
          ungetc(ch3, st->file);
          ungetc(ch4, st->file);
          return;
        } else {
          st->status |= HAS_BOM_f;
          st->encoding = ENC_ISO_UTF32_LE;
          return;
        }
      }
      st->status |= HAS_BOM_f;
      st->encoding = ENC_UTF16_LE;
      return;
    }
  }
  case 0xEF:
    ch2 = st->stream_getc(sno);
    if (ch2 != 0xBB) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return;
    } else {
      ch3 = st->stream_getc(sno);
      if (ch3 != 0xBF) {
        ungetc(ch1, st->file);
        ungetc(ch2, st->file);
        ungetc(ch3, st->file);
        return;
      } else {
        st->status |= HAS_BOM_f;
        st->encoding = ENC_ISO_UTF8;
        return;
      }
    }
  default:
    ungetc(ch1, st->file);
  }
}

static bool initStream(int sno, FILE *fd, const char *name, Term file_name,
                       encoding_t encoding, stream_flags_t flags,
                       Atom open_mode) {
  StreamDesc *st = &GLOBAL_Stream[sno];
  st->status = flags;

  st->charcount = 0;
  st->linecount = 1;
  if (flags & Binary_Stream_f) {
    st->encoding = ENC_OCTET;
  } else {
    st->encoding = encoding;
  }

  if (name == NULL) {
    char buf[YAP_FILENAME_MAX + 1];
    name = Yap_guessFileName(fileno(fd), sno, buf, YAP_FILENAME_MAX);
    st->name = Yap_LookupAtom(name);
  }
  st->user_name = file_name;
  st->file = fd;
  st->linepos = 0;
  if (flags & Pipe_Stream_f) {
    Yap_PipeOps(st);
    Yap_DefaultStreamOps(st);
  } else if (flags & Tty_Stream_f) {
    Yap_ConsoleOps(st);
    Yap_DefaultStreamOps(st);
  } else {
    st->stream_putc = FilePutc;
    st->stream_getc = PlGetc;
    unix_upd_stream_info(st);
    Yap_DefaultStreamOps(st);
  }
  st->stream_gets = PlGetsFunc();
  return true;
}

#define OPEN_DEFS()                                                            \
  PAR("alias", isatom, OPEN_ALIAS), PAR("bom", boolean, OPEN_BOM),             \
      PAR("buffer", isatom, OPEN_BUFFER),                                      \
      PAR("close_on_abort", boolean, OPEN_CLOSE_ON_ABORT),                     \
      PAR("create", isatom, OPEN_CREATE),                                      \
      PAR("encoding", isatom, OPEN_ENCODING),                                  \
      PAR("eof_action", isatom, OPEN_EOF_ACTION),                              \
      PAR("expand_filename", boolean, OPEN_EXPAND_FILENAME),                   \
      PAR("file_name", isatom, OPEN_FILE_NAME), PAR("input", ok, OPEN_INPUT),  \
      PAR("locale", isatom, OPEN_LOCALE), PAR("lock", isatom, OPEN_LOCK),      \
      PAR("mode", isatom, OPEN_MODE), PAR("output", ok, OPEN_OUTPUT),          \
      PAR("representation_errors", boolean, OPEN_REPRESENTATION_ERRORS),       \
      PAR("reposition", boolean, OPEN_REPOSITION),                             \
      PAR("type", isatom, OPEN_TYPE), PAR("wait", boolean, OPEN_WAIT),         \
      PAR(NULL, ok, OPEN_END)

#define PAR(x, y, z) z
typedef enum open_enum_choices { OPEN_DEFS() } open_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t open_defs[] = {OPEN_DEFS()};
#undef PAR

static Int
do_open(Term file_name, Term t2,
        Term tlist USES_REGS) { /* '$open'(+File,+Mode,?Stream,-ReturnCode) */
  Atom open_mode;
  int sno;
  SMALLUNSGN s;
  char io_mode[8];
  StreamDesc *st;
  bool avoid_bom = true, needs_bom = false, bin = false;
  char *fname;
  stream_flags_t flags;
  FILE *fd;
  encoding_t encoding;
  Term tenc;

  // original file name
  if (IsVarTerm(file_name)) {
    Yap_Error(INSTANTIATION_ERROR, file_name, "open/3");
    return FALSE;
  }
  if (!IsAtomTerm(file_name)) {
    if (IsStringTerm(file_name)) {
      fname = (char *)StringOfTerm(file_name);
    } else {
      Yap_Error(DOMAIN_ERROR_SOURCE_SINK, file_name, "open/3");
      return FALSE;
    }
  } else {
    fname = RepAtom(AtomOfTerm(file_name))->StrOfAE;
  }
  // open mode
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "open/3");
    return FALSE;
  }
  if (!IsAtomTerm(t2)) {
    if (IsStringTerm(t2)) {
      open_mode = Yap_LookupAtom(StringOfTerm(t2));
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t2, "open/3");
      return (FALSE);
    }
  } else {
    open_mode = AtomOfTerm(t2);
  }
  // read, write, append
  if (open_mode == AtomRead) {
    strncpy(io_mode, "rb", 8);
    s = Input_Stream_f;
  } else if (open_mode == AtomWrite) {
    strncpy(io_mode, "w", 8);
    s = Output_Stream_f;
  } else if (open_mode == AtomAppend) {
    strncpy(io_mode, "a", 8);
    s = Append_Stream_f | Output_Stream_f;
  } else {
    Yap_Error(DOMAIN_ERROR_IO_MODE, t2, "open/3");
    return (FALSE);
  }
  /* get options */
  xarg *args = Yap_ArgListToVector(tlist, open_defs, OPEN_END);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term,
                "option handling in open/3");
    return FALSE;
  }
  /* done */
  sno = GetFreeStreamD();
  if (sno < 0)
    return PlIOError(RESOURCE_ERROR_MAX_STREAMS, TermNil, "open/3");
  st = &GLOBAL_Stream[sno];
  st->user_name = file_name;
  flags = s;
  // user requested encoding?
  if (args[OPEN_ALIAS].used) {
    Atom al = AtomOfTerm(args[OPEN_ALIAS].tvalue);
    if (!Yap_AddAlias(al, sno))
      return false;
  }
  if (args[OPEN_ENCODING].used) {
    tenc = args[OPEN_ENCODING].tvalue;
    encoding = enc_id(RepAtom(AtomOfTerm(tenc))->StrOfAE);
  } else {
    encoding = LOCAL_encoding;
  }
  bool ok = (args[OPEN_EXPAND_FILENAME].used
                 ? args[OPEN_EXPAND_FILENAME].tvalue == TermTrue
                 : false) ||
            trueGlobalPrologFlag(OPEN_EXPANDS_FILENAME_FLAG);
  // expand file name?
  fname = Yap_AbsoluteFile(fname, LOCAL_FileNameBuf, ok);
  st->name = Yap_LookupAtom(fname);

  // binary type
  if (args[OPEN_TYPE].used) {
    Term t = args[OPEN_TYPE].tvalue;
    bool bin = (t == TermBinary);
    if (bin) {
#ifdef _WIN32
      strncat(io_mode, "b", 8);
#endif
      flags |= Binary_Stream_f;
      encoding = ENC_OCTET;
      avoid_bom = true;
    } else if (t == TermText) {
#ifdef _WIN32
      strncat(io_mode, "t", 8);
#endif
/* note that this matters for UNICODE style  conversions */
#if MAC
      if (open_mode == AtomWrite) {
        Yap_SetTextFile(RepAtom(AtomOfTerm(file_name))->StrOfAE);
      }
#endif
    } else {
      Yap_Error(DOMAIN_ERROR_STREAM, tlist,
                "type is ~a, must be one of binary or text", t);
    }
  }
  // BOM mess
  if ((encoding == ENC_OCTET ||
       encoding == ENC_ISO_ASCII ||
      encoding == ENC_ISO_LATIN1 ||
       encoding == ENC_ISO_UTF8 || bin))  {
    avoid_bom = true;
  }
  if (args[OPEN_BOM].used) {
    if (args[OPEN_BOM].tvalue == TermTrue) {
      needs_bom = true;
      if (avoid_bom) {
        return (PlIOError(SYSTEM_ERROR_INTERNAL, file_name,
                          "BOM not compatible with encoding"));
      }
    } else if (args[OPEN_BOM].tvalue == TermFalse) {
      needs_bom = false;
      avoid_bom = true;
    } else {
      Yap_Error(DOMAIN_ERROR_STREAM, tlist,
                "bom is ~a,  should be one of true or false",
                args[OPEN_BOM].tvalue);
    }
  } else if (st - GLOBAL_Stream < 3) {
    flags |= RepError_Prolog_f;
  }
  if ((fd = fopen(fname, io_mode)) == NULL ||
      (!(flags & Binary_Stream_f) && binary_file(fname))) {
    UNLOCK(st->streamlock);
    if (errno == ENOENT)
      return (PlIOError(EXISTENCE_ERROR_SOURCE_SINK, ARG6, "%s: %s", fname,
                        strerror(errno)));
    else
      return (PlIOError(PERMISSION_ERROR_OPEN_SOURCE_SINK, file_name, "%s: %s",
                        fname, strerror(errno)));
  }
#if MAC
  if (open_mode == AtomWrite) {
    Yap_SetTextFile(RepAtom(AtomOfTerm(file_name))->StrOfAE);
  }
#endif
  flags &= ~(Free_Stream_f);
  if (!initStream(sno, fd, fname, file_name, encoding, flags, open_mode))
    return false;
  if (open_mode == AtomWrite) {
    if (needs_bom && !write_bom(sno, st))
      return FALSE;
  } else if ((open_mode == AtomRead) && !avoid_bom &&
             (needs_bom || (flags & Seekable_Stream_f))) {
    check_bom(sno, st); // can change encoding
    if (st->encoding == ENC_ISO_UTF32_BE) {
      Yap_Error(DOMAIN_ERROR_STREAM_ENCODING, ARG1,
                "UTF-32 (BE) stream encoding unsupported");
      return FALSE;
    } else if (st->encoding == ENC_ISO_UTF32_LE) {
      Yap_Error(DOMAIN_ERROR_STREAM_ENCODING, ARG1,
                "UTF-32 (LE) stream encoding unsupported");
      return FALSE;
    }
  }

  UNLOCK(st->streamlock);
  {
    Term t = Yap_MkStream(sno);
    return (Yap_unify(ARG3, t));
  }
}

static Int open3(USES_REGS1) { /* '$open'(+File,+Mode,?Stream,-ReturnCode) */
  return do_open(Deref(ARG1), Deref(ARG2), TermNil PASS_REGS);
}

static Int open4(USES_REGS1) { /* '$open'(+File,+Mode,?Stream,-ReturnCode) */
  return do_open(Deref(ARG1), Deref(ARG2), Deref(ARG4) PASS_REGS);
}

static Int p_file_expansion(USES_REGS1) { /* '$file_expansion'(+File,-Name) */
  Term file_name = Deref(ARG1);

  /* we know file_name is bound */
  if (!IsAtomTerm(file_name)) {
    PlIOError(TYPE_ERROR_ATOM, file_name, "absolute_file_name/3");
    return (FALSE);
  }
  if (!Yap_TrueFileName(RepAtom(AtomOfTerm(file_name))->StrOfAE,
                        LOCAL_FileNameBuf, FALSE))
    return (PlIOError(EXISTENCE_ERROR_SOURCE_SINK, file_name,
                      "absolute_file_name/3"));
  return (Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(LOCAL_FileNameBuf))));
}

static Int p_open_null_stream(USES_REGS1) {
  Term t;
  StreamDesc *st;
  int sno = GetFreeStreamD();
  if (sno < 0)
    return (PlIOError(SYSTEM_ERROR_INTERNAL, TermNil,
                      "new stream not available for open_null_stream/1"));
  st = &GLOBAL_Stream[sno];
  st->status = Append_Stream_f | Output_Stream_f | Null_Stream_f;
#if _WIN32
  st->file = fopen("NUL", "w");
#else
  st->file = fopen("/dev/null", "w");
#endif
  if (st->file == NULL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "Could not open NULL stream (/dev/null,NUL)");
    return false;
  }
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = NullPutc;
  st->stream_wputc = put_wchar;
  st->stream_getc = PlGetc;
  st->stream_gets = PlGets;
  st->stream_wgetc = get_wchar;
  st->stream_wgetc_for_read = get_wchar;
  if (st->encoding == ENC_ISO_UTF8)
    st->stream_getc_for_utf8 = st->stream_getc;
  else
    st->stream_getc_for_utf8 = GetUTF8;
  st->user_name = MkAtomTerm(st->name = AtomDevNull);
  UNLOCK(st->streamlock);
  t = Yap_MkStream(sno);
  return (Yap_unify(ARG1, t));
}

int Yap_OpenStream(FILE *fd, char *name, Term file_name, int flags) {
  CACHE_REGS
  int sno;
  Atom at;

  sno = GetFreeStreamD();
  if (sno < 0)
    return (PlIOError(RESOURCE_ERROR_MAX_STREAMS, file_name,
                      "new stream not available for opening"));
  if (flags & Output_Stream_f) {
    if (flags & Append_Stream_f)
      at = AtomAppend;
    else
      at = AtomWrite;
  } else
    at = AtomRead;
  initStream(sno, fd, name, file_name, LOCAL_encoding, flags, at);
  return sno;
}

#define CheckStream(arg, kind, msg)                                            \
  CheckStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)

static int CheckStream__(const char *file, const char *f, int line, Term arg,
                         int kind, const char *msg) {
  int sno = -1;
  arg = Deref(arg);
  if (IsVarTerm(arg)) {
    Yap_Error(INSTANTIATION_ERROR, arg, msg);
    return -1;
  } else if (IsAtomTerm(arg)) {
    Atom sname = AtomOfTerm(arg);

    if (sname == AtomUser) {
      if (kind & Input_Stream_f) {
        if (kind & (Output_Stream_f | Append_Stream_f)) {
          PlIOError__(file, f, line, PERMISSION_ERROR_INPUT_STREAM, arg,
                      "ambiguous use of 'user' as a stream");
          return (-1);
        }
        sname = AtomUserIn;
      } else {
        sname = AtomUserOut;
      }
    }
    if ((sno = Yap_CheckAlias(sname)) < 0) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      PlIOError__(file, f, line, EXISTENCE_ERROR_STREAM, arg, msg);
      return -1;
    } else {
      LOCK(GLOBAL_Stream[sno].streamlock);
      return sno;
    }
  } else if (IsApplTerm(arg) && FunctorOfTerm(arg) == FunctorStream) {
    arg = ArgOfTerm(1, arg);
    if (!IsVarTerm(arg) && IsIntegerTerm(arg)) {
      sno = IntegerOfTerm(arg);
    }
  }
  if (sno < 0) {
    Yap_Error(DOMAIN_ERROR_STREAM_OR_ALIAS, arg, msg);
    return (-1);
  }
  if (GLOBAL_Stream[sno].status & Free_Stream_f) {
    PlIOError__(file, f, line, EXISTENCE_ERROR_STREAM, arg, msg);
    return (-1);
  }
  LOCK(GLOBAL_Stream[sno].streamlock);
  if ((GLOBAL_Stream[sno].status & Input_Stream_f) &&
      !(kind & Input_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    PlIOError__(file, f, line, PERMISSION_ERROR_INPUT_STREAM, arg, msg);
  }
  if ((GLOBAL_Stream[sno].status & (Append_Stream_f | Output_Stream_f)) &&
      !(kind & Output_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    PlIOError__(file, f, line, PERMISSION_ERROR_OUTPUT_STREAM, arg, msg);
  }
  return (sno);
}

int Yap_CheckStream__(const char *file, const char *f, int line, Term arg,
                      int kind, const char *msg) {
  return CheckStream__(file, f, line, arg, kind, msg);
}

int Yap_CheckTextStream__(const char *file, const char *f, int line, Term arg,
                          int kind, const char *msg) {
  int sno;
  if ((sno = CheckStream__(file, f, line, arg, kind, msg)) < 0)
    return -1;
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    PlIOError__(file, f, line, PERMISSION_ERROR_INPUT_BINARY_STREAM, arg, msg);
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return -1;
  }
  return sno;
}

/* used from C-interface */
int Yap_GetFreeStreamDForReading(void) {
  int sno = GetFreeStreamD();
  StreamDesc *s;

  if (sno < 0)
    return sno;
  s = GLOBAL_Stream + sno;
  s->status |= User_Stream_f | Input_Stream_f;
  s->charcount = 0;
  s->linecount = 1;
  s->linepos = 0;
  Yap_DefaultStreamOps(s);
  UNLOCK(s->streamlock);
  return sno;
}

static Int always_prompt_user(USES_REGS1) {
  StreamDesc *s = GLOBAL_Stream + StdInStream;

  s->status |= Promptable_Stream_f;
#if USE_SOCKET
  if (s->status & Socket_Stream_f) {
    Yap_ConsoleSocketOps(s);
  } else
#endif
      if (s->status & Pipe_Stream_f) {
    Yap_ConsolePipeOps(s);
  } else
    Yap_ConsoleOps(s);
  return (TRUE);
}

static Int close1(USES_REGS1) { /* '$close'(+GLOBAL_Stream) */
  Int sno = CheckStream(
      ARG1, (Input_Stream_f | Output_Stream_f | Socket_Stream_f), "close/2");
  if (sno < 0)
    return (FALSE);
  if (sno <= StdErrStream) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return TRUE;
  }
  Yap_CloseStream(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

#define CLOSE_DEFS()                                                           \
  PAR("force", boolean, CLOSE_FORCE), PAR(NULL, ok, CLOSE_END)

#define PAR(x, y, z) z

typedef enum close_enum_choices { CLOSE_DEFS() } close_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t close_defs[] = {CLOSE_DEFS()};
#undef PAR

static Int close2(USES_REGS1) { /* '$close'(+GLOBAL_Stream) */
  Int sno = CheckStream(
      ARG1, (Input_Stream_f | Output_Stream_f | Socket_Stream_f), "close/2");
  Term tlist;
  if (sno < 0)
    return (FALSE);
  if (sno <= StdErrStream) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return TRUE;
  }
  xarg *args =
      Yap_ArgListToVector((tlist = Deref(ARG2)), close_defs, CLOSE_END);
  if (args == NULL)
    return FALSE;
  // if (args[CLOSE_FORCE].used) {
  // }
  Yap_CloseStream(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

Term read_line(int sno) {
  CACHE_REGS
  Term tail;
  Int ch;

  if ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) == 10) {
    return (TermNil);
  }
  tail = read_line(sno);
  return (MkPairTerm(MkIntTerm(ch), tail));
}

#define ABSOLUTE_FILE_NAME_DEFS()                                              \
  PAR("access", isatom, ABSOLUTE_FILE_NAME_ACCESS),                            \
      PAR("expand", boolean, ABSOLUTE_FILE_NAME_EXPAND),                       \
      PAR("extensions", ok, ABSOLUTE_FILE_NAME_EXTENSIONS),                    \
      PAR("file_type", is_file_type, ABSOLUTE_FILE_NAME_FILE_TYPE),            \
      PAR("file_errors", is_file_errors, ABSOLUTE_FILE_NAME_FILE_ERRORS),      \
      PAR("glob", ok, ABSOLUTE_FILE_NAME_GLOB),                                \
      PAR("relative_to", isatom, ABSOLUTE_FILE_NAME_RELATIVE_TO),              \
      PAR("solutions", issolutions, ABSOLUTE_FILE_NAME_SOLUTIONS),             \
      PAR("verbose_file_search", boolean,                                      \
          ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH),                             \
      PAR(NULL, ok, ABSOLUTE_FILE_NAME_END)

#define PAR(x, y, z) z

typedef enum ABSOLUTE_FILE_NAME_enum_ {
  ABSOLUTE_FILE_NAME_DEFS()
} absolute_file_name_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t absolute_file_name_search_defs[] = {
    ABSOLUTE_FILE_NAME_DEFS()};
#undef PAR

static Int abs_file_parameters(USES_REGS1) {
  Term t[ABSOLUTE_FILE_NAME_END];
  Term tlist = Deref(ARG1), tf;
  /* get options */
  xarg *args = Yap_ArgListToVector(tlist, absolute_file_name_search_defs,
                                   ABSOLUTE_FILE_NAME_END);
  if (args == NULL)
    return FALSE;
  /* done */
  if (args[ABSOLUTE_FILE_NAME_EXTENSIONS].used)
    t[ABSOLUTE_FILE_NAME_EXTENSIONS] =
        args[ABSOLUTE_FILE_NAME_EXTENSIONS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_EXTENSIONS] = TermNil;
  if (args[ABSOLUTE_FILE_NAME_RELATIVE_TO].used)
    t[ABSOLUTE_FILE_NAME_RELATIVE_TO] =
        args[ABSOLUTE_FILE_NAME_RELATIVE_TO].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_RELATIVE_TO] = TermEmptyAtom;
  if (args[ABSOLUTE_FILE_NAME_FILE_TYPE].used)
    t[ABSOLUTE_FILE_NAME_FILE_TYPE] = args[ABSOLUTE_FILE_NAME_FILE_TYPE].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_FILE_TYPE] = TermTxt;
  if (args[ABSOLUTE_FILE_NAME_ACCESS].used)
    t[ABSOLUTE_FILE_NAME_ACCESS] = args[ABSOLUTE_FILE_NAME_ACCESS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_ACCESS] = TermNone;
  if (args[ABSOLUTE_FILE_NAME_FILE_ERRORS].used)
    t[ABSOLUTE_FILE_NAME_FILE_ERRORS] =
        args[ABSOLUTE_FILE_NAME_FILE_ERRORS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_FILE_ERRORS] = TermError;
  if (args[ABSOLUTE_FILE_NAME_SOLUTIONS].used)
    t[ABSOLUTE_FILE_NAME_SOLUTIONS] = args[ABSOLUTE_FILE_NAME_SOLUTIONS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_SOLUTIONS] = TermFirst;
  if (args[ABSOLUTE_FILE_NAME_EXPAND].used)
    t[ABSOLUTE_FILE_NAME_EXPAND] = args[ABSOLUTE_FILE_NAME_EXPAND].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_EXPAND] = TermFalse;
  if (args[ABSOLUTE_FILE_NAME_GLOB].used)
    t[ABSOLUTE_FILE_NAME_GLOB] = args[ABSOLUTE_FILE_NAME_GLOB].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_GLOB] = TermEmptyAtom;
  if (args[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH].used)
    t[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH] =
        args[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH] =
        (trueGlobalPrologFlag(VERBOSE_FILE_SEARCH_FLAG) ? TermTrue : TermFalse);
  tf = Yap_MkApplTerm(Yap_MkFunctor(AtomOpt, ABSOLUTE_FILE_NAME_END),
                      ABSOLUTE_FILE_NAME_END, t);
  return (Yap_unify(ARG2, tf));
}

static Int get_abs_file_parameter(USES_REGS1) {
  Term t = Deref(ARG1), topts = ARG2;
  /* get options */
  /* done */
  if (t == TermExtensions)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_EXTENSIONS + 1, topts));
  if (t == TermRelativeTo)
    return Yap_unify(ARG3,
                     ArgOfTerm(ABSOLUTE_FILE_NAME_RELATIVE_TO + 1, topts));
  if (t == TermFileType)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_FILE_TYPE + 1, topts));
  if (t == TermAccess)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_ACCESS + 1, topts));
  if (t == TermFileErrors)
    return Yap_unify(ARG3,
                     ArgOfTerm(ABSOLUTE_FILE_NAME_FILE_ERRORS + 1, topts));
  if (t == TermSolutions)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_SOLUTIONS + 1, topts));
  if (t == TermGlob)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_GLOB + 1, topts));
  if (t == TermExpand)
    return Yap_unify(ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_EXPAND + 1, topts));
  if (t == TermVerboseFileSearch)
    return Yap_unify(
        ARG3, ArgOfTerm(ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH + 1, topts));
  Yap_Error(DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION, ARG2, NULL);
  return false;
}

void Yap_InitPlIO(void) {
  Int i;

  Yap_stdin = stdin;
  Yap_stdout = stdout;
  Yap_stderr = stderr;
  GLOBAL_Stream =
      (StreamDesc *)Yap_AllocCodeSpace(sizeof(StreamDesc) * MaxStreams);
  for (i = 0; i < MaxStreams; ++i) {
    INIT_LOCK(GLOBAL_Stream[i].streamlock);
    GLOBAL_Stream[i].status = Free_Stream_f;
  }
  InitStdStreams();
}

void Yap_InitIOPreds(void) {
  /* here the Input/Output predicates */
  Yap_InitCPred("always_prompt_user", 0, always_prompt_user,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("close", 1, close1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("close", 2, close2, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("open", 4, open4, SyncPredFlag);
  Yap_InitCPred("open", 3, open3, SyncPredFlag);
  Yap_InitCPred("abs_file_parameters", 2, abs_file_parameters,
                SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("get_abs_file_parameter", 3, get_abs_file_parameter,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$file_expansion", 2, p_file_expansion,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$open_null_stream", 1, p_open_null_stream,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitIOStreams();
  Yap_InitCharsio();
  Yap_InitChtypes();
  Yap_InitConsole();
  Yap_InitReadUtil();
  Yap_InitMems();
  Yap_InitPipes();
  Yap_InitFiles();
  Yap_InitWriteTPreds();
  Yap_InitReadTPreds();
  Yap_InitFormat();
  Yap_InitRandomPreds();
  Yap_InitReadline();
  Yap_InitSockets();
  Yap_InitSignalPreds();
  Yap_InitSysPreds();
  Yap_InitTimePreds();
}
