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

/**260-1A
 A
 * @file   iopreds.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Jan 20 00:45:56 2016
 *
 * @brief  main open and close predicates over generic streams.
 *
 */
/*
 * This file includes the definition of a miscellania of standard predicates *
 *for yap refering to: Files and GLOBAL_1588
 *451ams, Simple Input/Output,
 *Rua do Orfeão do Po
 */

#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"
#include "sysbits.h"
#include <stdlib.h>

#if HAVE_UNISTD_H

#include <unistd.h>

#endif
#if HAVE_STDARG_H

#include <stdarg.h>

#endif
#if HAVE_CTYPE_H
#endif
#if HAVE_WCTYPE_Hab
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

#include "getw.h"

static int get_wchar_from_file(int);

FILE *Yap_stdin;
FILE *Yap_stdout;
FILE *Yap_stderr;


 char *Yap_VF(const char *path) {
    char *out;

    out = (char *)malloc(MAX_PATH + 1);
    if (GLOBAL_cwd == NULL || GLOBAL_cwd[0] == 0 || 
	!Yap_IsAbsolutePath(path, false)) {
        return (char *)path;
    }
    strcpy(out, GLOBAL_cwd);
    strcat(out, "/");
    strcat(out, path);
    return out;
}

char *Yap_VFAlloc(const char *path) {
    char *out;

    out = (char *)malloc(MAX_PATH + 1);
    if (GLOBAL_cwd == NULL || GLOBAL_cwd[0] == 0 ||
        !Yap_IsAbsolutePath(path, false)) {
        return (char *)path;
    }
    strcpy(out, GLOBAL_cwd);
    strcat(out, "/");
    strcat(out, path);
    return out;
}


int ResetEOF(StreamDesc *s) {
  if (s->status & Eof_Error_Stream_f) {
    Atom name = s->name;
    // Yap_CloseStream(s - GLOBAL_Stream);
    Yap_ThrowError(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, MkAtomTerm(name),
              "GetC");
    return FALSE;
  } else if (s->status & Reset_Eof_Stream_f) {
    s->status &= ~Push_Eof_Stream_f;
    /* reset the eof indicator on file */
    if (feof(s->file))
      clearerr(s->file);
    /* reset our function for reading input */
    Yap_DefaultStreamOps(s);
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
    Yap_DefaultStreamOps(s);
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
    Yap_DefaultStreamOps(s);
    return s->stream_getc(sno);
  }
  return EOF;
}

void Yap_stream_id(StreamDesc *s, Term user_name, Atom system_name) {
  Int sno = s-GLOBAL_Stream;
  int fd;
    char buf[MAX_PATH];
  if (system_name) {
    s->name = system_name;
  } else
#if _MSC_VER || defined(__MINGW32__)
#define ISATTY(F) _isatty(F)
#define FNO(F) _fileno(F)
#elif HAVE_ISATTY 
#define FNO(F) fileno(F)
#define ISATTY(F) isatty(F)
#endif
    if ((s->status & Null_Stream_f)) {
      system_name = AtomDevNull;
    } else if ((s->status & Socket_Stream_f) && s->file == NULL) {
      snprintf(buf, 255,"stream(%ld,sock(%d))", sno, s->u.socket.fd); 
    } else if ((s->status & Pipe_Stream_f) && s->file == NULL) {
      snprintf(buf, 255,"pipe(%ld,sock(%d))", sno, s->u.socket.fd); 
    } else if (s->file && ISATTY(FNO(s->file))) {
      /* standard error stream should never be buffered */
      s->status |= Tty_Stream_f | Reset_Eof_Stream_f | Promptable_Stream_f;
      /* make all console descriptors unbuffered */
    setvbuf(s->file, NULL, _IONBF, 0);
#if __simplescalar__
  /* isatty does not seem to work with simplescar. I'll assume the first
     three streams will probably be ttys (pipes are not thatg different) */
    if (s) {
      snprintf(buf, 255, "tty{file=%p}", s->file);
    } else
#endif
#if HAVE_TTYNAME
    int rc = ttyname_r(FNO(s->file),buf, MAX_PATH - 1);
    if (rc != 0) 
#endif
      {
	if (s-GLOBAL_Stream == 0) {
	  snprintf(buf, 255, "console(std_input)");
	}  else    if (s-GLOBAL_Stream == 1) {
	  snprintf(buf, 255, "console(std_output}"); 
	}  else    if (s-GLOBAL_Stream == 2) {
	  snprintf(buf, 255, "console{std_error}"); 
	} else if (s->file) {
	  snprintf(buf, 255, "tty{file=%p}", s->file);
	} else {
	  snprintf(buf, 255, "tty{fd=%ld}", s-GLOBAL_Stream); 
	}
      }
  }
#if HAVE_STAT
    else if (s->file && (fd=fileno(s->file))>=0) {
      struct SYSTEM_STAT ss;
  if (SYSTEM_FSTAT(fd, &ss) != 0) {
    /* ignore errors while checking a file */
    return;
  }
  if (S_ISREG(ss.st_mode)) {
    snprintf(buf, 255,"stream(%ld,regular_file(%d))", sno,fd); 
  }
  if (S_ISDIR(ss.st_mode)) {
    snprintf(buf, 255,"stream(%ld,directory(%d))", sno, fd);
  }
  if (S_ISFIFO(ss.st_mode)) {
    snprintf(buf, 255,"stream(%ld,pipe(%d))", sno, fd); 
  }
  if (S_ISSOCK(ss.st_mode)) {
    snprintf(buf, 255,"stream(%ld,sock(%d))", sno, fd); 
  }
  if (S_ISLNK(ss.st_mode)) {
    snprintf(buf, 255,"stream(%ld,symbolic_link(%d))", sno, fd); 
  }
    }
#endif
     else if (s->status & InMemory_Stream_f) {
      s->status |= Seekable_Stream_f;
      if (s->status & Output_Stream_f) {
	snprintf(buf, 255,"stream(%ld,string(\"%.10s\"))", sno, s->u.mem_string.buf);
    } else {
      snprintf(buf, 255, "stream(%ld,string)", sno);
    }
    } else {
       snprintf(buf, 255, "stream(%ld)", sno);
    }
    if (system_name)
      s->name = system_name;
    else
      s->name = Yap_LookupAtom(buf);
    if (user_name) {
      s->user_name = user_name;
    } else {
      s->user_name = MkAtomTerm(s->name);
    }
    
}

static void default_peek(StreamDesc *st) {
  {
    st->stream_peek = Yap_peekChar;
    st->stream_wpeek = Yap_peekWide;
  }
  if (st->status & Eof_Stream_f) {
    st->stream_peek = EOFPeek;
    st->stream_wpeek = EOFPeek;
    st->stream_getc = EOFGetc;
    st->stream_wgetc = EOFWGetc;
  }
  if (GLOBAL_CharConversionTable != NULL)
    st->stream_wgetc_for_read = ISOWGetc;
  else
    st->stream_wgetc_for_read = st->stream_wgetc;
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
void Yap_DefaultStreamOps(StreamDesc *st) {
  if (st->vfs && !st->file) {
    st->stream_putc = st->vfs->put_char;
    st->stream_wputc = st->vfs->put_wchar;
    st->stream_getc = st->vfs->get_char;
    st->stream_wgetc = st->vfs->get_wchar;
    default_peek(st);
    return;
  }
  st->stream_wputc = put_wchar;
  st->stream_wgetc = get_wchar;
  default_peek(st);
  if (st->encoding == ENC_ISO_UTF8) {
    st->stream_wgetc = get_wchar_UTF8;
  }
  
  if (st->status & Tty_Stream_f && st-GLOBAL_Stream <3) {
      st->stream_putc =ConsolePutc;
    if (st->status & Readline_Stream_f) {
      st->stream_getc =ReadlineGetc  ;
          st->stream_peek = Yap_ReadlinePeekChar;
    st->stream_wpeek = Yap_ReadlinePeekChar;
    } else {
      st->stream_getc =ConsoleGetc;
    }
      st->stream_getc =ReadlineGetc  ;
  } else if (st->status & Null_Stream_f) {
    st->stream_putc = NullPutc;
  } else if (st->status & Pipe_Stream_f) {
      Yap_PipeOps(st);
    } else if (st->status & InMemory_Stream_f) {
      Yap_MemOps(st);
#if HAVE_SETBUF
            setbuf(stdin, NULL);
#endif /* HAVE_SETBUF */
  } else if (st->status & Null_Stream_f) {
  st->stream_putc = NullPutc;
  st->stream_getc = PlGetc;
} else {
    st->stream_putc = FilePutc;
    st->stream_getc = PlGetc;
  }
#ifndef _WIN32
     if (st->file != NULL && 0 && !(st->status & InMemory_Stream_f)) {
      st->stream_wgetc = get_wchar_from_file;
    }
#endif
 #if USE_READLINE
  if (st->status & Readline_Stream_f) {
  }
  #endif

  /* else {
     st->stream_peek = Yap_peekWithGetc;
     st->stream_wpeek = Yap_peekWideWithGetwc;
     }
     } else if (st->status & Seekable_Stream_f) {
     st->stream_peek = Yap_peekWithSeek;
     st->stream_wpeek = Yap_peekWideWithSeek;
     } */

}


static void InitStdStream(int sno, SMALLUNSGN flags, FILE *file, VFS_t *vfsp) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  
  s->file = file;
  s->status = flags;
  s->linestart = 0;
  s->linecount = 1;
  s->charcount = 0;
  s->vfs = vfsp;
  s->buf.on = false;
  s->encoding = ENC_ISO_UTF8;
  INIT_LOCK(s->streamlock);
  if (vfsp == NULL) {
    Yap_stream_id(s, 0, NULL);
  }
  /* Getting streams to prompt is a mess because we need for cooperation
     between readers and writers to the stream :-(
  */
#if LIGHT
  s->status |= Tty_Stream_f | Promptable_Stream_f;
#endif
  s->buf.on = false;
  Yap_DefaultStreamOps(s);
#if HAVE_SETBUF
  if (s->status & Tty_Stream_f && sno == 0) {
    /* make sure input is unbuffered if it comes from stdin, this
       makes life simpler for interrupt handling */
    setbuf(stdin, NULL);
    //    fprintf(stderr,"here I am\n");
  }
#endif /* HAVE_SETBUF */
}

void Yap_InitStdStream(int sno, unsigned int flags, FILE *file, VFS_t *vfsp) {
  InitStdStream(sno, flags, file, vfsp);
}

static void InitStdStreams(void) {
  CACHE_REGS
  if (LOCAL_sockets_io) {
    InitStdStream(StdInStream, Input_Stream_f, NULL, NULL);
    InitStdStream(StdOutStream, Output_Stream_f, NULL, NULL);
    InitStdStream(StdErrStream, Output_Stream_f, NULL, NULL);
  } else {
    InitStdStream(StdInStream, Input_Stream_f, stdin, NULL);
    InitStdStream(StdOutStream, Output_Stream_f, stdout, NULL);
    InitStdStream(StdErrStream, Output_Stream_f, stderr, NULL);
  }
#if USE_READLINE
  if (GLOBAL_Stream[StdInStream].status & Tty_Stream_f &&
      GLOBAL_Stream[StdOutStream].status & Tty_Stream_f &&
      GLOBAL_Stream[StdErrStream].status & Tty_Stream_f && !Yap_Embedded) {
    Yap_InitReadline(TermTrue);
  }
#endif
  LOCAL_c_input_stream = StdInStream;
  LOCAL_c_output_stream = StdOutStream;
  LOCAL_c_error_stream = StdErrStream;
}

void Yap_InitStdStreams(void) { InitStdStreams(); }

Int PlIOError__(const char *file, const char *function, int lineno,
                yap_error_number type, Term culprit, ...) {
  CACHE_REGS
  if (FileErrors(PASS_REGS1)  ||
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
    Yap_ThrowError__(file, function, lineno, type, culprit, who);
    /* and fail */
    return false;
  } else {
    memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
    Yap_SetGlobalVal(AtomZip, MkVarTerm());
    return false;
  }
}

bool
 UnixIOError__(const char *file, const char *function, int lineno,
                int error, io_kind_t io_type, Term culprit, ...) {
  CACHE_REGS
  if (FileErrors(PASS_REGS1)  ) {
    va_list args;
    const char *format;
    char *who = Malloc(1024);
    yap_error_number e_type;

    va_start(args, culprit);
    format = va_arg(args, char *);
    if (format) {
      vsnprintf(who, 1023, format, args);
    } else {
      who[0] = '\0';
    }
    va_end(args);
    switch (error) {
      case EPERM: 
    case EROFS:
	// operation not permitted,
      switch (io_type) {
        case CREATE_DIRECTORY:
    e_type = PERMISSION_ERROR_CREATE_DIRECTORY;
    break;
  case CREATE_FILE:
   e_type = PERMISSION_ERROR_CREATE_DIRECTORY;
    break;
    default:
    e_type = SYSTEM_ERROR_INTERNAL;
           break;
      }
      case EEXIST:
             switch (io_type) {
  case CREATE_DIRECTORY:
    e_type = EXISTENCE_ERROR_DIRECTORY;
    break;
  case CREATE_FILE:
   e_type = EXISTENCE_ERROR_FILE;
    break;
    default:
    e_type = SYSTEM_ERROR_INTERNAL;
    }
    break;
       case EACCES:
             switch (io_type) {
  case CREATE_DIRECTORY:
    e_type = PERMISSION_ERROR_CREATE_DIRECTORY;
    break;
  case CREATE_FILE:
   e_type = PERMISSION_ERROR_CREATE_FILE;
    break;
    default:
    e_type = SYSTEM_ERROR_INTERNAL;
    }
    break;
    default:
        e_type = SYSTEM_ERROR_INTERNAL;
    }

   Yap_ThrowError__(file, function, lineno, e_type, culprit, who);
    /* aRgrownd fail */  
  } else {
    pop_text_stack(0);
    memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));

  }
    return false;
  }

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
  return (putc(ch, stderr));
}

int Yap_DebugPuts(FILE *s, const char *sch) {
  if (Yap_Option['l' - 96])
    (void)fputs(sch, Yap_logfile);
  return fputs(sch, stderr);
}

void Yap_DebugErrorPuts(const char *s) { Yap_DebugPuts(stderr, s); }

void Yap_DebugPlWrite(Term t) {
  CACHE_REGS
    
  int depths[3];
  if (t == 0)
    fprintf(stderr, "NULL");
  depths[0] = depths[1] = depths[2] = 10;
  Yap_plwrite(t, GLOBAL_Stream + 2, depths, HR, 0,0, NULL);
}

void
Yap_DebugPlWriteln(Term t) {
  CACHE_REGS
  if (t == 0)
    fprintf(stderr, "NULL");
     int depths[3];
     depths[0] = depths[1] = depths[2] = 100;
     Yap_plwrite(t, GLOBAL_Stream+LOCAL_c_error_stream , depths, HR, 0,Quote_illegal_f, NULL);
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

/* check if we read a LOCAL_newline or an EOF */
int console_post_process_eof(StreamDesc *s) {
  CACHE_REGS
  if (!ResetEOF(s)) {
    s->status |= Eof_Stream_f;
    s->stream_getc = EOFGetc;
    s->stream_wgetc = EOFWGetc;
    s->stream_wgetc_for_read = EOFWGetc;
    LOCAL_newline = true;
  }
  return EOFCHAR;
}

/* check if we read a newline or an EOF */
int post_process_read_wchar(int ch, size_t n, StreamDesc *s) {
  if (ch == EOF) {
    return post_process_weof(s);
  }
#if DEBUG
  if (GLOBAL_Option[1]) {
    static int v;
    fprintf(stderr, "%d %C\n", v, ch);
    v++;
  }
#endif
  s->charcount += n;
  if (ch == '\n') {
    ++s->linecount;
    s->linestart = s->charcount;
    /* don't convert if the stream is binary */
    if (!(s->status & Binary_Stream_f))
      ch = 10;
  }
  return ch;
}

int post_process_weof(StreamDesc *s) {
  if (!ResetEOF(s)) {
    s->status |= Eof_Stream_f;
    s->stream_wgetc = EOFWGetc;
    s->stream_getc = EOFGetc;
    s->stream_wgetc_for_read = EOFWGetc;
  }
  return EOFCHAR;
}

void *Yap_RepStreamFromId(int sno) { return GLOBAL_Stream + (sno); }

/**
 * caled after EOF found a peek, it just calls console_post_process to
 *conclude
 *the job.
 *
 * @param sno
 *
 * @return EOF
 */
int EOFPeek(int sno) { return EOFCHAR; }

int EOFWPeek(int sno) { return EOFCHAR; }

/* standard routine, it should read from anything pointed by a FILE *.
   It could be made more efficient by doing our own buffering and avoiding
   post_process_read_char, something to think about */
int PlGetc(int sno) {
  StreamDesc *s = &GLOBAL_Stream[sno];
  return fgetc(s->file);
}

// layered version
static inline int get_wchar_from_file(int sno) {
  return post_process_read_wchar(fgetwc(GLOBAL_Stream[sno].file), 1,
                                 GLOBAL_Stream + sno);
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
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, MkIntegerTerm(ch),
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
    mbstate_t mbstate;
    int n;

    memset((void *)&mbstate, 0, sizeof(mbstate_t));
    if ((n = wcrtomb(buf, ch, &mbstate)) < 0) {
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
      GLOBAL_Stream[sno].stream_putc(sno, ch);
    } else if (ch < 0x800) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xC0 | ch >> 6);
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else if (ch < 0x10000) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xE0 | ch >> 12);
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 6 & 0x3F));
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else if (ch < 0x200000) {
      GLOBAL_Stream[sno].stream_putc(sno, 0xF0 | ch >> 18);
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 12 & 0x3F));
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch >> 6 & 0x3F));
      GLOBAL_Stream[sno].stream_putc(sno, 0x80 | (ch & 0x3F));
    } else {
      /* should never happen */
      return -1;
    }
    return ch;
    break;
  case ENC_UTF16_LE: {
    if (ch < 0x10000) {
      GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
      GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
    } else {
      // computations
      uint16_t ich = ch;
      uint16_t lead = LEAD_OFFSET + (ich >> 10);
      uint16_t trail = 0xDC00 + (ich & 0x3FF);

      GLOBAL_Stream[sno].stream_putc(sno, (trail & 0xff));
      GLOBAL_Stream[sno].stream_putc(sno, (trail >> 8));
      GLOBAL_Stream[sno].stream_putc(sno, (lead & 0xff));
      GLOBAL_Stream[sno].stream_putc(sno, (lead >> 8));
    }
    return ch;
  }
  case ENC_UTF16_BE: {
    // computations
    if (ch < 0x10000) {
      GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
      GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
    } else {
      uint16_t lead = (uint16_t)LEAD_OFFSET + ((uint16_t)ch >> 10);
      uint16_t trail = 0xDC00 + ((uint16_t)ch & 0x3FF);

      GLOBAL_Stream[sno].stream_putc(sno, (lead >> 8));
      GLOBAL_Stream[sno].stream_putc(sno, (lead & 0xff));
      GLOBAL_Stream[sno].stream_putc(sno, (trail >> 8));
      GLOBAL_Stream[sno].stream_putc(sno, (trail & 0xff));
    }
    return ch;
  }
  case ENC_UCS2_LE: {
    if (ch >= 0x10000) {
      return 0;
    }
    GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
    return ch;
  }
  case ENC_UCS2_BE: {
    // computations
    if (ch < 0x10000) {
      GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8));
      GLOBAL_Stream[sno].stream_putc(sno, (ch & 0xff));
      return ch;
    } else {
      return 0;
    }
  }

  case ENC_ISO_UTF32_BE:
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 24) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 16) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, ch & 0xff);
    return ch;
  case ENC_ISO_UTF32_LE:
    GLOBAL_Stream[sno].stream_putc(sno, ch & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 8) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 16) & 0xff);
    GLOBAL_Stream[sno].stream_putc(sno, (ch >> 24) & 0xff);
    return ch;
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
    return (GLOBAL_Stream[sno].u.pipe.fd);
  } else if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
    return (-1);
  }
  return (fileno(GLOBAL_Stream[sno].file));
}

Int Yap_GetStreamFd(int sno) { return GetStreamFd(sno); }

static int binary_file(const char *file_name) {
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
    return false;
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
      return false;
    if (st->stream_putc(sno, 0xBB) < 0)
      return false;
    if (st->stream_putc(sno, 0xBF) < 0)
      return false;
    st->status |= HAS_BOM_f;
    return true;
  case ENC_UTF16_BE:
  case ENC_UCS2_BE:
    if (st->stream_putc(sno, 0xFE) < 0)
      return false;
    if (st->stream_putc(sno, 0xFF) < 0)
      return false;
    st->status |= HAS_BOM_f;
    return true;
  case ENC_UTF16_LE:
  case ENC_UCS2_LE:
    if (st->stream_putc(sno, 0xFF) < 0)
      return false;
    if (st->stream_putc(sno, 0xFE) < 0)
      return false;
    st->status |= HAS_BOM_f;
    return true;
  case ENC_ISO_UTF32_BE:
    if (st->stream_putc(sno, 0x00) < 0)
      return false;
    if (st->stream_putc(sno, 0x00) < 0)
      return false;
    if (st->stream_putc(sno, 0xFE) < 0)
      return false;
    if (st->stream_putc(sno, 0xFF) < 0)
      return false;
    st->status |= HAS_BOM_f;
    return true;
  case ENC_ISO_UTF32_LE:
    if (st->stream_putc(sno, 0xFF) < 0)
      return false;
    if (st->stream_putc(sno, 0xFE) < 0)
      return false;
    if (st->stream_putc(sno, 0x00) < 0)
      return false;
    if (st->stream_putc(sno, 0x00) < 0)
      return false;
    st->status |= HAS_BOM_f;
    return true;
  default:
    return true;
  }
}

static int check_bom(int sno, StreamDesc *st) {
  int ch1, ch2, ch3, ch4;
  if (st->file == NULL) {
    Yap_ThrowError(SYSTEM_ERROR_INTERNAL, Yap_MkStream(sno),
              "YAP does not support BOM n %x type of files", st->status);
    return -1;
  }
  ch1 = fgetc(st->file);
  switch (ch1) {
  case 0x00: {
    ch2 = fgetc(st->file);
    if (ch2 != 0x00) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return 0;
    } else {
      ch3 = fgetc(st->file);
      if (ch3 == EOFCHAR || ch3 != 0xFE) {
        ungetc(ch1, st->file);
        ungetc(ch2, st->file);
        ungetc(ch3, st->file);
        return 0;
      } else {
        ch4 = fgetc(st->file);
        if (ch4 == EOFCHAR || ch3 != 0xFF) {
          ungetc(ch1, st->file);
          ungetc(ch2, st->file);
          ungetc(ch3, st->file);
          ungetc(ch4, st->file);
          return 0;
        } else {
          st->status |= HAS_BOM_f;
          st->encoding = ENC_ISO_UTF32_BE;
          return 4;
        }
      }
    }
  }
  case 0xFE: {
    ch2 = fgetc(st->file);
    if (ch2 != 0xFF) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return 0;
    } else {
      st->status |= HAS_BOM_f;
      st->encoding = ENC_UTF16_BE;
      return 2;
    }
  }
  case 0xFF: {
    ch2 = fgetc(st->file);
    if (ch2 != 0xFE) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return 0;
    } else {
      ch3 = fgetc(st->file);
      if (ch3 != 0x00) {
        ungetc(ch3, st->file);
      } else {
        ch4 = fgetc(st->file);
        if (ch4 == 0x00) {
             st->charcount += 4;
         st->status |= HAS_BOM_f;
          st->encoding = ENC_ISO_UTF32_LE;
          return 4;
        } else {
          ungetc(ch3, st->file);
          ungetc(ch4, st->file);
        }
      }
    }
    st->status |= HAS_BOM_f;
    st->encoding = ENC_UTF16_LE;
    return 2;
  }
  case 0xEF:
    ch2 = fgetc(st->file);
    if (ch2 != 0xBB) {
      ungetc(ch1, st->file);
      ungetc(ch2, st->file);
      return 0;
    } else {
      ch3 = fgetc(st->file);
      if (ch3 != 0xBF) {
        ungetc(ch1, st->file);
        ungetc(ch2, st->file);
        ungetc(ch3, st->file);
        return 0;
      } else {
        st->status |= HAS_BOM_f;
        st->encoding = ENC_ISO_UTF8;
        return 3;
      }
    }
  default:
    ungetc(ch1, st->file);
    return 0;
  }
}

bool Yap_initStream__(const char *f, const char *func, int line, int sno, FILE *file, Atom name, const char *io_mode,
                    Term file_name, encoding_t encoding, stream_flags_t flags,
                    void *vfs) {
  CACHE_REGS
    extern void jmp_deb(int);
  StreamDesc *st = &GLOBAL_Stream[sno];
  __android_log_print(
      ANDROID_LOG_INFO, "YAPDroid", "init %s %s:%s  stream  <%d>", io_mode,
      CurrentModule == 0 ? "prolog"
                         : RepAtom(AtomOfTerm(CurrentModule))->StrOfAE,
      RepAtom(name)->StrOfAE, sno);
  if (io_mode == NULL)
    Yap_ThrowError(PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM, MkIntegerTerm(sno),
              "File opened with NULL Permissions");
  if (strchr(io_mode, 'a')) {
    st->status = Append_Stream_f | Output_Stream_f | flags;
  } else if (strchr(io_mode, 'w')) {
    st->status = Output_Stream_f | flags;
  }
  if (strchr(io_mode, 'r')) {
    st->status = Input_Stream_f | flags;
  }
  if (strchr(io_mode, 'b')) {
    st->status = Binary_Stream_f | flags;
  }
#if HAVE_STAT
  int fd;
  if (st->file && (fd=fileno(st->file))>=0) {
      struct SYSTEM_STAT ss;
      if (SYSTEM_FSTAT(fd, &ss) == 0 &&S_ISREG(ss.st_mode)) {
      st->status |= Seekable_Stream_f;
      }
  }
#endif

  // st->vfs = vfs;
  st->buf.on = false;
  st->charcount = 0;
  st->linecount = 1;
  st->linestart = 0;
  if (flags & Binary_Stream_f) {
    st->encoding = ENC_OCTET;
  } else {
    st->encoding = encoding;
  }
  Yap_stream_id(st, file_name, name);
  st->file = file;
  if (!st->name) {
    Yap_ThrowError(SYSTEM_ERROR_INTERNAL, file_name,
		   "Yap_guessFileName failed: opening a file without a name");
  }
  st->linestart = 0;
  Yap_DefaultStreamOps(st);
  return true;
}

static bool scan_failed(int sno, long pos, encoding_t onc) {
  FILE *file = GLOBAL_Stream[sno].file;
  fseek(file, pos, SEEK_SET);
    return false;
}


/*
 * scan_a_char gets the next character, assuming we may have NULLs inbetween characters
 */
static bool scan_encoding(int sno) {
    encoding_t onc = GLOBAL_Stream[sno].encoding;
    FILE *file = GLOBAL_Stream[sno].file;
    char txt[256];
    int ch=0,l=0;
    long pos =        ftell(file);
    while (ch != 10) {
      while ((ch=fgetc(file))=='\0');
      txt[l++] = ch;
      if (l == 32)
	return scan_failed(sno, pos, onc);
    }
    txt[l++] = '\0';
    if (strstr(txt,":-")!=txt)
      return scan_failed(sno, pos, onc);
    int i = 2;
    while (isspace(txt[i])) i++;
    if (strstr(txt+i,"encoding(")!=txt+i)
      return scan_failed(sno, pos, onc);
    char *s;
    if ((s=strstr(txt+i,")."))==NULL)
      return scan_failed(sno, l, onc);
    i += strlen("encoding(");
    while (isspace(txt[i])) i++;
    if (txt[i]=='\'') i++; 
    int j = (s-txt)-1;
    while (isspace(txt[j])) j--;
    if (txt[j]=='\'') j--; 
    txt[j+1]='\0';
    encoding_t enc=GLOBAL_Stream[sno].encoding = enc_id(txt+i, onc);
    long end = ftell(file);
    fseek(GLOBAL_Stream[sno].file, pos, SEEK_SET);
    while (pos++ < end)
      GLOBAL_Stream[sno].stream_getc(sno);
    if (enc == ENC_UTF16_LE || enc == ENC_ISO_UTF32_LE || enc==ENC_UCS2_LE) {
      GLOBAL_Stream[sno].stream_getc(sno);
      if (enc == ENC_ISO_UTF32_BE) {
	GLOBAL_Stream[sno].stream_getc(sno);
	GLOBAL_Stream[sno].stream_getc(sno);
      }
    }
      
    return true;
}

static bool open_header(int sno, Atom open_mode) {
  if (open_mode == AtomWrite) {
    const char *ptr;
    const char s[] = "#!";
    int ch;

    ptr = s;
    while ((ch = *ptr++))
      GLOBAL_Stream[sno].stream_wputc(sno, ch);
    const char *b = Yap_FindExecutable();
    ptr = b;
    while ((ch = *ptr++))
      GLOBAL_Stream[sno].stream_wputc(sno, ch);
    const char *l = " -L --\n\n YAP script\n#\n# .\n";
    ptr = l;
    while ((ch = *ptr++))
      GLOBAL_Stream[sno].stream_wputc(sno, ch);

  } else if (open_mode == AtomRead) {
      // skip header
      int ch;
      while ((ch = Yap_peek(sno)) == '#') {
          while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) != 10 && ch != -1);
      }
  }
      return true;
}

#undef PAR
#define OPEN_DEFS()                                                            \
  PAR("alias", isatom, OPEN_ALIAS)                                             \
  , PAR("bom", booleanFlag, OPEN_BOM), PAR("buffer", isatom, OPEN_BUFFER),     \
      PAR("close_on_abort", booleanFlag, OPEN_CLOSE_ON_ABORT),                 \
      PAR("create", isatom, OPEN_CREATE),                                      \
      PAR("encoding", isatom, OPEN_ENCODING),                                  \
      PAR("eof_action", isatom, OPEN_EOF_ACTION),                              \
      PAR("expand_filename", booleanFlag, OPEN_EXPAND_FILENAME),               \
      PAR("file_name", isatom, OPEN_FILE_NAME), PAR("input", ok, OPEN_INPUT),  \
      PAR("locale", isatom, OPEN_LOCALE), PAR("lock", isatom, OPEN_LOCK),      \
      PAR("mode", isatom, OPEN_MODE), PAR("output", ok, OPEN_OUTPUT),          \
      PAR("representation_errors", booleanFlag, OPEN_REPRESENTATION_ERRORS),   \
      PAR("reposition", booleanFlag, OPEN_REPOSITION),                         \
      PAR("script", booleanFlag, OPEN_SCRIPT), PAR("type", isatom, OPEN_TYPE), \
      PAR("wait", booleanFlag, OPEN_WAIT), PAR(NULL, ok, OPEN_END)

#define PAR(x, y, z) z
typedef enum open_enum_choices { OPEN_DEFS() } open_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t open_defs[] = {OPEN_DEFS()};
#undef PAR

static bool fill_stream(int sno, StreamDesc *st, Term tin, const char *io_mode,
                        Term user_name, bool *avoid_bomp, encoding_t enc) {
  CACHE_REGS
  struct vfs *vfsp = NULL;
  const char *fname;

  if (IsAtomTerm(tin))
    fname = RepAtom(AtomOfTerm(tin))->StrOfAE;
  else if (IsStringTerm(tin))
    fname = StringOfTerm(tin);
  else
    fname = NULL;

  st->file = NULL;
  if (fname) {
      if ((vfsp = vfs_owner(fname)) != NULL) {
          if (vfsp->open(vfsp, fname, io_mode, sno)) {
              // read, write, append
              user_name = st->user_name;
              st->vfs = vfsp;
              UNLOCK(st->streamlock);
              __android_log_print(
                      ANDROID_LOG_INFO, "YAPDroid", "got  %d ", sno);
          } else {
              UNLOCK(st->streamlock);
              __android_log_print(
                      ANDROID_LOG_INFO, "YAPDroid", "failed  %s ",fname);
return false;
          }
     } else {
          st->file = fopen(fname, io_mode);
          if (st->file == NULL) {
                 __android_log_print(
                     ANDROID_LOG_INFO, "YAPDroid", "failed  %s ",fname);
            UNLOCK(st->streamlock);
              if (errno == ENOENT && !strchr(io_mode, 'r')) {
                  PlIOError(EXISTENCE_ERROR_SOURCE_SINK, tin, "%s: %s", fname,
                            strerror(errno));
              } else {
                  PlIOError(PERMISSION_ERROR_OPEN_SOURCE_SINK, tin, "%s: %s", fname,
                            strerror(errno));
              }
              return false;
          }

      }
  } else if (IsApplTerm(tin)) {
            Functor f = FunctorOfTerm(tin);
            if (f == FunctorAtom || f == FunctorString || f == FunctorCodes1 ||
                f == FunctorCodes || f == FunctorChars1 || f == FunctorChars) {
                if (strchr(io_mode, 'w')) {
                    return Yap_OpenBufWriteStream(PASS_REGS1);
                } else {
                    int j = push_text_stack();
                    const char *buf;
		    //   encoding_t enc = ENC_ISO_UTF8;
		    //if (f == FunctorAtom || f == FunctorString)
		    //  enc = ENC_ISO_UTF8;
		    *avoid_bomp = true;
                    buf = Yap_TextTermToText(ArgOfTerm(1,tin) PASS_REGS);
                    if (!buf) {
                        pop_text_stack(j);
                        return false;
                    }
                    st->nbuf = pop_output_text_stack(j, buf);
                    Atom nat = Yap_LookupAtom(Yap_StrPrefix(buf, 32));
                    sno = Yap_open_buf_read_stream(st, buf, st->nsize=(strlen(buf) + 1), &LOCAL_encoding,
            	                                           MEM_BUF_MALLOC, nat,
                                                   MkAtomTerm(NameOfFunctor(f)));
                    pop_text_stack(j);
                }
            } else if (!strcmp(RepAtom(NameOfFunctor(f))->StrOfAE, "popen")) {
                const char *buf;
                int i = push_text_stack();
                buf = Yap_TextTermToText(ArgOfTerm(1, tin) PASS_REGS);
                if (buf == NULL) {
                    return false;
                }
#if _WIN32
                st->file = _popen(buf, io_mode);
#else
                st->file = popen(buf, io_mode);
#endif
		st->status |= Popen_Stream_f;
                pop_text_stack(i);
            } else {
                Yap_ThrowError(DOMAIN_ERROR_SOURCE_SINK, tin, "open");
            }
        }
  if (!strchr(io_mode, 'b') && binary_file(fname)) {
    st->status |= Binary_Stream_f;
  }
  Yap_initStream(sno, st->file, Yap_LookupAtom(fname), io_mode, tin,
                 LOCAL_encoding, st->status, vfsp);
  return true;
}

static Int do_open(Term file_name, Term t2, Term tlist USES_REGS) {
  // 
  Atom open_mode;
  bool avoid_bom = false, needs_bom = false;
  Term tenc;
  char io_mode[8];
  int sno = GetFreeStreamD(); // get locked stream
  if (sno < 0)
    return (PlIOError(RESOURCE_ERROR_MAX_STREAMS, file_name,
                      "new stream not available for opening"));
  StreamDesc *st = GLOBAL_Stream + sno;
  memset(st, 0, sizeof(*st));
  // user requested encoding?
  // BOM mess
  st->encoding = LOCAL_encoding;
  if (st->encoding == ENC_UTF16_BE || st->encoding == ENC_UTF16_LE ||
      st->encoding == ENC_UCS2_BE || st->encoding == ENC_UCS2_LE ||
      st->encoding == ENC_ISO_UTF32_BE || st->encoding == ENC_ISO_UTF32_LE) {
    st->status |= HAS_BOM_f;
  }
  file_name = Deref(file_name);
  if (IsVarTerm(file_name)) {
    Yap_ThrowError(INSTANTIATION_ERROR, file_name,
                   "while opening a list of options");
  }
  // open mode
  if (IsVarTerm(t2)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t2, "open/3");
    return false;
  }
  if (!IsAtomTerm(t2)) {
    if (IsStringTerm(t2)) {
      open_mode = Yap_LookupAtom(StringOfTerm(t2));
    } else {
      Yap_ThrowError(TYPE_ERROR_ATOM, t2, "open/3");
      return false;
    }
  } else {
    open_mode = AtomOfTerm(t2);
  }
  /* get options */
xarg *   args = Yap_ArgListToVector(tlist, open_defs, OPEN_END, NULL,DOMAIN_ERROR_STREAM_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
      Yap_ThrowError(LOCAL_Error_TYPE, tlist, "option handling in open/3");
    }
    return false;
  }
  /* done */
  st->status = 0;
  const char *s_encoding;
  if (args[OPEN_ENCODING].used) {
    tenc = args[OPEN_ENCODING].tvalue;
    s_encoding = RepAtom(AtomOfTerm(tenc))->StrOfAE;
  } else {
    s_encoding = "default";
  }
  // default encoding, no bom yet
  st->encoding = enc_id(s_encoding, ENC_OCTET);
  // only set encoding after getting BOM
  char const *fname0;
  bool ok = (args[OPEN_EXPAND_FILENAME].used
                 ? args[OPEN_EXPAND_FILENAME].tvalue == TermTrue
                 : false);
  if (ok) {
    if (((IsAtomTerm(file_name) &&
          (fname0 = RepAtom(AtomOfTerm(file_name))->StrOfAE))) ||
        (IsStringTerm(file_name) && (fname0 = StringOfTerm(file_name)))) {
      int lvl = push_text_stack();
      const char *fname = Yap_AbsoluteFile(fname0, ok);
      file_name = MkAtomTerm(Yap_LookupAtom(fname));
      if (!fname) {
        pop_text_stack(lvl);
        PlIOError(EXISTENCE_ERROR_SOURCE_SINK, ARG1, NULL);
      }
    }
  }
  // Skip scripts that start with !#/.. or similar

  if (open_mode == AtomRead) {
    strncpy(io_mode, "r", 8);
  } else if (open_mode == AtomWrite) {
    strncpy(io_mode, "w", 8);
  } else if (open_mode == AtomAppend) {
    strncpy(io_mode, "a", 8);
  } else {
    return false;
  }
  // binary type
  if (args[OPEN_TYPE].used) {
    Term t = args[OPEN_TYPE].tvalue;
    bool bin = (t == TermBinary);
    if (bin) {
#ifdef _WIN32
      strncat(io_mode, "b", 8);
#endif
      st->status |= Binary_Stream_f;
      st->encoding = ENC_OCTET;
      avoid_bom = true;
      needs_bom = false;
    } else if (t == TermText) {
#ifdef _WIN32
      strncat(io_mode, "t", 8);
#endif
      /* note that this matters for UNICODE style  conversions */
    } else {
      Yap_ThrowError(DOMAIN_ERROR_STREAM_OPTION, t,
                "type is ~a, must be one of binary or text", t);
    }
  }

  st = &GLOBAL_Stream[sno];

  if (!fill_stream(sno, st, file_name, io_mode, file_name, &avoid_bom, st->encoding)) {
    return false;
  }

  if (args[OPEN_BOM].used) {
    if (args[OPEN_BOM].tvalue == TermTrue) {
      avoid_bom = false;
      needs_bom = true;
    } else if (args[OPEN_BOM].tvalue == TermFalse) {
      avoid_bom = true;
      needs_bom = false;
    }
  }
  bool script =
      (args[OPEN_SCRIPT].used ? args[OPEN_SCRIPT].tvalue == TermTrue : false);

  if (args[OPEN_ALIAS].used) {
    Atom al = AtomOfTerm(args[OPEN_ALIAS].tvalue);
    if (!Yap_AddAlias(al, sno)) {
      free(args);
      return false;
    }
  }
  if (st - GLOBAL_Stream < 3) {
    st->status |= RepError_Prolog_f;
  }
#if MAC
  if (open_mode == AtomWrite) {
    Yap_SetTextFile(RepAtom(AtomOfTerm(file_name))->StrOfAE);
  }
#endif
  // interactive streams do not have a start, so they probably don't have
  // a BOM
  avoid_bom = avoid_bom || (st->status & Tty_Stream_f);
  if (needs_bom && !write_bom(sno, st)) {
    return false;
  } else if (open_mode == AtomRead || open_mode==AtomCsult) {
      if (!avoid_bom) {
          long bsz = check_bom(sno, st); // can change encoding
           // follow declaration unless there is v
          if (bsz>0) {
	    if (args[OPEN_BOM].used) {
	      st->encoding = enc_id(s_encoding, st->encoding);
	    }
          }
       }
      if (!(st->status & Tty_Stream_f) &&
	  st->file)
	scan_encoding(sno);
  }
  Yap_DefaultStreamOps(st);
  if (script) {
    open_header(sno, open_mode);
  }

  free(args);
  UNLOCK(st->streamlock);
  {
    Term t = Yap_MkStream(sno);
    return (Yap_unify(ARG3, t));
  }
}

/** @pred  open(+ _F_,+ _M_,- _S_) is iso


    Opens the file with name  _F_ in mode  _M_ (`read`, `write` or
    `append`), returning  _S_ unified with the stream name.

    Yap allows 64 streams opened at the same time. If you need more,
    redefine the MaxStreams constant.  Each stream is either an input or
    an output stream but not both. There are always 3 open streams:
    user_input for reading, user_output for writing and user_error for
    writing. If there is no ambiguity, the atoms user_input and
    user_output may be referred to as `user`.

    The `file_errors` flag controls whether errors are reported when in
    mode `read` or `append` the file  _F_ does not exist or is not
    readable, and whether in mode `write` or `append` the file is not
    writable.

*/

static Int open3(USES_REGS1) {
  /* '$open'(+File,+Mode,?Stream,-ReturnCode) */
  return do_open(Deref(ARG1), Deref(ARG2), TermNil PASS_REGS);
}

/** @pred open(+ _F_,+ _M_,- _S_,+ _Opts_) is iso

   Opens the file with name  _F_ in mode  _M_ (`read`,  `write` or
  `append`), returning  _S_ unified with the stream name, and following
   these options:



  + `type(+ _T_)` is iso

  Specify whether the stream is a `text` stream (default), or a
  `binary` stream.

  + `reposition(+ _Bool_)` is iso
  Specify whether it is possible to reposition the stream (`true`), or
  not (`false`). By default, YAP enables repositioning for all
  files, except terminal files and sockets.

  + `eof(+ _Action_)` is iso

  Specify the action to take if attempting to input characters from a
  stream where we have previously found an `end_of_file`. The possible
  actions are `error`, that raises an error, `reset`, that tries to
  reset the stream and is used for `tty` type files, and `eof_code`,
  which generates a new `end_of_file` (default for non-tty files).

  + `alias(+ _Name_)` is iso

  Specify an alias to the stream. The alias <tt>Name</tt> must be an atom.
  The
  alias can be used instead of the stream descriptor for every operation
  concerning the stream.

  The operation will fail and give an error if the alias name is already
  in use. YAP allows several aliases for the same file, but only
  one is returned by stream_property/2

  + `bom(+ _Bool_)`

  If present and `true`, a BOM (<em>Byte Order Mark</em>) was
  detected while opening the file for reading or a BOM was written while
  opening the stream. See BOM for details.

  + `encoding(+ _Encoding_)`

  Set the encoding used for text.  See Encoding for an overview of
  wide character and encoding issues.

  + `representation_errors(+ _Mode_)`

  Change the behaviour when writing characters to the stream that cannot
  be represented by the encoding.  The behaviour is one of `error`
  (throw and Input/Output error exception), `prolog` (write `\u...\`
  escape code or `xml` (write `\&#...;` XML character entity).
  The initial mode is `prolog` for the user streams and
  `error` for all other streams. See also Encoding.

  + `expand_filename(+ _Mode_)`

  If  _Mode_ is `true` then do filename expansion, then ask Prolog
  to do file name expansion before actually trying to opening the file:
  this includes processing `~` characters and processing `$`
  environment variables at the beginning of the file. Otherwise, just try
  to open the file using the given name.

  The default behavior is given by the Prolog flag
  open_expands_filename.

  + `script( + _Boolean_ )` YAP extension.

  The file may be a Prolog script. In `read` mode just check for
  initial lines if they start with the hash symbol, and skip them. In
  `write` mode output an header that can be used to launch the file by
  calling `yap -l file -- $*`. Note that YAP will not set file
  permissions as executable. In `append` mode ignore the flag.


*/
static Int open4(USES_REGS1) { /* '$open'(+File,+Mode,?Stream,-ReturnCode) */
  return do_open(Deref(ARG1), Deref(ARG2), Deref(ARG4) PASS_REGS);
}

static Int p_file_expansion(USES_REGS1) { /* '$file_expansion'(+File,-Name) */
  Term file_name = Deref(ARG1);

  /* we know file_name is bound */
  if (IsVarTerm(file_name)) {
    Yap_ThrowError(INSTANTIATION_ERROR, file_name, "absolute_file_name/3");
    return (FALSE);
  }
  int lvl = push_text_stack();
  const char *tmp;
  if ((tmp = Yap_AbsoluteFile(RepAtom(AtomOfTerm(file_name))->StrOfAE,
                              false)) == NULL) {
    pop_text_stack(lvl);
    return (PlIOError(EXISTENCE_ERROR_SOURCE_SINK, file_name,
                      "absolute_file_name/3"));
  }
  bool rc = (Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(tmp))));
  pop_text_stack(lvl);
  return rc;
}

static Int p_open_null_stream(USES_REGS1) {
  Term t;
  StreamDesc *st;
  int sno = GetFreeStreamD();
  if (sno < 0) {
   Yap_ThrowError(SYSTEM_ERROR_INTERNAL, TermNil,
		  "new stream not available for open_null_stream/1");
   
  return false;
}
  st = &GLOBAL_Stream[sno];
  st->status = Append_Stream_f | Output_Stream_f | Null_Stream_f;
#if _WIN32
  st->file = fopen("NUL", "w");
#else
  st->file = fopen("/dev/null", "w");
#endif
  if (st->file == NULL) {
    Yap_ThrowError(SYSTEM_ERROR_INTERNAL, TermNil,
              "Could not open NULL stream (/dev/null,NUL)");
    return false;
  }
  Yap_initStream(sno, st->file, NULL, "w", 0,
                 LOCAL_encoding, st->status, NULL);
  UNLOCK(st->streamlock);
  t = Yap_MkStream(sno);
  return (Yap_unify(ARG1, t));
}

int Yap_OpenStream(Term tin, const char *io_mode, Term user_name,
                   encoding_t enc) {
  int sno;
  StreamDesc *st;

  sno = GetFreeStreamD();
  if (sno < 0) {
    Yap_ThrowError(RESOURCE_ERROR_MAX_STREAMS, tin,
              "new stream not available for opening");
    return -1;
  }
  st = GLOBAL_Stream + sno;
  // fname = Yap_VF(fname);
  bool avoid_bom = false;
  if (fill_stream(sno, st, tin, io_mode, user_name, &avoid_bom, enc))
    return sno;
  return -1;
}

int Yap_FileStream(FILE *fd, Atom name, Term file_name, int flags,
                   VFS_t *vfsp) {
  CACHE_REGS
  int sno;
  const char *mode;

  sno = GetFreeStreamD();
  if (sno < 0) {
    Yap_ThrowError(RESOURCE_ERROR_MAX_STREAMS, file_name,
                      "new stream not available for opening");
  return false;
}
  if (flags & Output_Stream_f) {
    if (flags & Append_Stream_f)
      mode = "a";
    else
      mode = "w";
  } else {
    mode = "r";
  }
  Yap_initStream(sno, fd, name, mode, file_name, LOCAL_encoding, flags, vfsp);
  return sno;
}

#define CheckStream(arg, kind, msg)                                            \
  CheckStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)

static int CheckStream__(const char *file, const char *f, int line, Term arg,
                         int kind, const char *msg) {
  int sno = -1;
  arg = Deref(arg);

  if (msg == NULL)
    msg = "found unbound stream";
  if (IsVarTerm(arg)) {
    Yap_ThrowError__(file, f,line, INSTANTIATION_ERROR, arg, msg);
    return -1;
  } else if (IsAtomTerm(arg)) {
    Atom sname = AtomOfTerm(arg);

    if (sname == AtomUser) {
      if (kind & Input_Stream_f) {
        if (kind & (Output_Stream_f | Append_Stream_f)) {
          Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_STREAM, arg,
                      "ambiguous use of 'user' as <a stream");
          return (-1);
        }
        sname = AtomUserIn;
      } else {
        sname = AtomUserOut;
      }
    }
    if ((sno = Yap_CheckAlias(sname)) < 0) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      Yap_ThrowError__(file, f, line, EXISTENCE_ERROR_STREAM, arg, msg);
      return -1;
    } else {
      LOCK(GLOBAL_Stream[sno].streamlock);
    }
  } else if (IsApplTerm(arg) && FunctorOfTerm(arg) == FunctorStream) {
    arg = ArgOfTerm(1, arg);
    if (!IsVarTerm(arg) && IsIntegerTerm(arg)) {
      sno = IntegerOfTerm(arg);
    }
  } else {
        Yap_ThrowError__(file, f, line, TYPE_ERROR_STREAM, arg, msg);

  }
  if (sno < 0 || sno > MaxStreams) {
    Yap_ThrowError__(file, f, line, DOMAIN_ERROR_STREAM_OR_ALIAS, arg, msg);
    return -1;
  }
  if (GLOBAL_Stream[sno].status & Free_Stream_f) {
    Yap_ThrowError__(file, f, line, EXISTENCE_ERROR_STREAM, arg, msg);
    return -1;
  }
//  LOCK(GLOBAL_Stream[sno].streamlock);
  if ((GLOBAL_Stream[sno].status & Input_Stream_f) &&
      !(kind & Input_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_STREAM, arg, msg);
    return -1;
  }
  if ((GLOBAL_Stream[sno].status & (Append_Stream_f | Output_Stream_f)) &&
      !(kind & Output_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_ThrowError__(file, f, line, PERMISSION_ERROR_INPUT_STREAM, arg, msg);
    return -1;
  }
  return sno;
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
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    if (kind == Input_Stream_f)
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_INPUT_BINARY_STREAM, arg,
                  msg);
    else
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_BINARY_STREAM, arg,
                  msg);
    return -1;
  }
  return sno;
}

int Yap_CheckTextWriteStream__(const char *file, const char *f, int line,
                               Term arg, const char *msg) {
  int sno, kind = Output_Stream_f;
  if ((sno = CheckStream__(file, f, line, arg, kind, msg)) < 0)
    return -1;
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    if (kind & Output_Stream_f)
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_INPUT_BINARY_STREAM, arg,
                  msg);
    else
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_BINARY_STREAM, arg,
                  msg);
    return -1;
  }
  return sno;
}

int Yap_CheckTextReadStream__(const char *file, const char *f, int line,
                              Term arg, const char *msg) {
  int sno, kind = Input_Stream_f;
  if ((sno = CheckStream__(file, f, line, arg, kind, msg)) < 0)
    return -1;
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    if (kind & Input_Stream_f)
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_INPUT_BINARY_STREAM, arg,
                  msg);
    else
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_BINARY_STREAM, arg,
                  msg);
    return -1;
  }
  return sno;
}

int Yap_CheckBinaryStream__(const char *file, const char *f, int line, Term arg,
                            int kind, const char *msg) {
  int sno;
  if ((sno = CheckStream__(file, f, line, arg, kind, msg)) < 0)
    return -1;
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    if (kind == Input_Stream_f)
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_INPUT_TEXT_STREAM, arg, msg);
    else
      Yap_ThrowError__(file, f, line, PERMISSION_ERROR_OUTPUT_TEXT_STREAM, arg, msg);
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
  s->linestart = 0;
  Yap_DefaultStreamOps(s);
  UNLOCK(s->streamlock);
  return sno;
}

/**
 * @pred always_prompt_user
 *
 * Ensure that the stream always prompts before asking the standard input
 stream for data.

*/
static Int always_prompt_user(USES_REGS1) {
  StreamDesc *s = GLOBAL_Stream + StdInStream;

  s->status |= Promptable_Stream_f;
  Yap_DefaultStreamOps(s);
  return (TRUE);
}

/** @pred  close(+ _S_) is iso

Closes the stream  _S_. If  _S_ does not stand for a stream
currently opened an error is reported. The streams user_input,
user_output, and user_error can never be closed.
*/
static Int close1(USES_REGS1) { /* '$close'(+GLOBAL_Stream) */
  int sno = CheckStream(
      ARG1, (Input_Stream_f | Output_Stream_f | Socket_Stream_f), "close/2");
  if (sno < 0)
    return false;
  if (sno <= StdErrStream) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return true;
  }
  Yap_CloseStream(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

#define CLOSE_DEFS()                                                           \
  PAR("force", booleanFlag, CLOSE_FORCE), PAR(NULL, ok, CLOSE_END)

#define PAR(x, y, z) z

typedef enum close_enum_choices { CLOSE_DEFS() } close_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t close_defs[] = {CLOSE_DEFS()};
#undef PAR

/** @pred  close(+ _S_,+ _O_) is iso

    Closes the stream  _S_, following options  _O_.

    The only valid options are `force(true)` and `force(false)`.
    YAP currently ignores these options.


*/
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
  xarg *args = Yap_ArgListToVector((tlist = Deref(ARG2)), close_defs, CLOSE_END, NULL,
                                   DOMAIN_ERROR_CLOSE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
      Yap_ThrowError(LOCAL_Error_TYPE, tlist, NULL);
    }
    return false;
  }
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
void Yap_InitPlIO(struct yap_boot_params *argi) {
  Int i;
  if (argi->inp > 0)
    Yap_stdin = fdopen(argi->inp - 1, "r");
  else if (argi->inp < 0)
    Yap_stdin = NULL;
  else
    Yap_stdin = stdin;
  if (argi->out > 0)
    Yap_stdout = fdopen(argi->out - 1, "a");
  else if (argi->out < 0)
    Yap_stdout = NULL;
  else
    Yap_stdout = stdout;
  if (argi->err > 0)
    Yap_stderr = fdopen(argi->err - 1, "a");
  else if (argi->out)
    Yap_stdout = NULL;
  else
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
  Yap_InitCPred("$file_expansion", 2, p_file_expansion,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$open_null_stream", 1, p_open_null_stream,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitIOStreams();
  Yap_InitAliases();
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
#if USE_READLINE
  Yap_InitReadlinePreds();
#endif
  Yap_InitSockets();
  Yap_InitSignalPreds();
  Yap_InitSysPreds();
  Yap_InitTimePreds();
  Yap_InitAbsfPreds();
 }
