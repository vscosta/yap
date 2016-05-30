/**************************************************************************
 *									 *
 * File:		iopreds.h *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#ifndef IOPREDS_H
#define IOPREDS_H 1

#if _WIN32
#define USE_SOCKET 1
#define HAVE_SOCKET 1
#endif

#include "Atoms.h"
#include "Yap.h"
#include <stdlib.h>

/*
 * This file defines main data-structure for stream management,
 *
 */

extern size_t Yap_page_size;

#if defined(_MSC_VER) || defined(__MINGW32__)

#include <windows.h>

#endif

#include <wchar.h>

#define Yap_CheckStream(arg, kind, msg)                                        \
  Yap_CheckStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckStream__(const char *, const char *, int, Term, int,
                             const char *);
#define Yap_CheckTextStream(arg, kind, msg)                                    \
  Yap_CheckTextStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckTextStream__(const char *, const char *, int, Term, int,
                                 const char *);
#define Yap_CheckBinaryStream(arg, kind, msg)                                    \
  Yap_CheckBinaryStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckBinaryStream__(const char *, const char *, int, Term, int,
                                 const char *);

extern bool Yap_initStream(int sno, FILE *fd, const char *name, Term file_name,
                           encoding_t encoding, stream_flags_t flags,
                           Atom open_mode);

#if HAVE_SOCKET
extern int Yap_sockets_io;

/****************** defines for sockets *********************************/

typedef enum { /* in YAP, sockets may be in one of 4 possible status */
               new_socket,
               server_socket,
               client_socket,
               server_session_socket,
               closed_socket
} socket_info;

typedef enum { /* we accept two domains for the moment, IPV6 may follow */
               af_inet, /* IPV4 */
               af_unix  /* or AF_FILE */
} socket_domain;

extern Term Yap_InitSocketStream(int, socket_info, socket_domain);
extern int Yap_CheckSocketStream(Term, const char *);
extern socket_domain Yap_GetSocketDomain(int);
extern socket_info Yap_GetSocketStatus(int);
extern void Yap_UpdateSocketStream(int, socket_info, socket_domain);

/* routines in ypsocks.c */
Int Yap_CloseSocket(int, socket_info, socket_domain);

#endif /* USE_SOCKET */

/************ SWI compatible support for unicode representations  ************/
typedef struct yap_io_position {
  int64_t byteno;       /* byte-position in file */
  int64_t charno;       /* character position in file */
  long int lineno;      /* lineno in file */
  long int linepos;     /* position in line */
  intptr_t reserved[2]; /* future extensions */
} yapIOPOS;

#ifndef _PL_STREAM_H
typedef struct {
  Atom file;         /* current source file */
  yapIOPOS position; /* Line, line pos, char and byte */
} yapSourceLocation;
#endif

#define RD_MAGIC 0xefebe128

typedef struct vlist_struct_t {
  struct VARSTRUCT *ve;
  struct vlist_struct_t *next;
} vlist_t;

typedef struct qq_struct_t {
  unsigned char *text;
  yapIOPOS start, mid, end;
  vlist_t *vlist;
  struct qq_struct_t *next;
} qq_t;

typedef struct read_data_t {
  unsigned char *here;        /* current character */
  unsigned char *base;        /* base of clause */
  unsigned char *end;         /* end of the clause */
  unsigned char *token_start; /* start of most recent read token */

  int magic; /* RD_MAGIC */
  struct stream_desc *stream;
  FILE *f;       /* file. of known */
  Term position; /* Line, line pos, char and byte */
  void *posp;    /* position pointer */
  size_t posi;   /* position number */

  Term subtpos;                    /* Report Subterm positions */
  bool cycles;                     /* Re-establish cycles */
  yapSourceLocation start_of_term; /* Position of start of term */
  struct mod_entry *module;        /* Current source module */
  unsigned int flags;              /* Module syntax flags */
  int styleCheck;                  /* style-checking mask */
  bool backquoted_string;          /* Read `hello` as string */

  int *char_conversion_table; /* active conversion table */

  Atom on_error;     /* Handling of syntax errors */
  int has_exception; /* exception is raised */

  Term exception; /* raised exception */
  Term variables; /* report variables */
  Term singles;   /* Report singleton variables */
  Term varnames;  /* Report variables+names */
  int strictness; /* Strictness level */

#ifdef O_QUASIQUOTATIONS
  Term quasi_quotations; /* User option quasi_quotations(QQ) */
  Term qq;               /* Quasi quoted list */
  Term qq_tail;          /* Tail of the quoted stuff */
#endif

  Term comments; /* Report comments */

} read_data, *ReadData;

Term Yap_read_term(int inp_stream, Term opts, int nargs);
Term Yap_Parse(UInt prio, encoding_t enc, Term cmod);

void init_read_data(ReadData _PL_rd, struct stream_desc *s);

typedef int (*GetsFunc)(int, UInt, char *);

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#if __APPLE__
#include "fmemopen.h"
#define HAVE_FMEMOPEN 1
#define HAVE_OPEN_MEMSTREAM 1
FILE *open_memstream(char **buf, size_t *len);
#endif

#if __ANDROID__
#undef HAVE_FMEMOPEN
#undef HAVE_OPEN_MEMSTREAM
#endif

#if HAVE_FMEMOPEN
#define MAY_READ 1
#endif

#if HAVE_OPEN_MEMSTREAM
#define MAY_READ 1
#define MAY_WRITE 1
#endif

#if _WIN32
#undef MAY_WRITE
#undef MAY_READ
#endif

typedef struct mem_desc {
  char *buf;    /* where the file is being read from/written to */
  int src;      /* where the space comes from, 0 code space, 1 malloc */
  Int max_size; /* maximum buffer size (may be changed dynamically) */
  UInt pos;     /* cursor */
  volatile void *error_handler;
} memHandle;

typedef struct stream_desc {
  Atom name;
  Term user_name;
  FILE *file;
  // useful in memory streams
  char *nbuf;
  size_t nsize;
  union {
    struct {
#define PLGETC_BUF_SIZE 4096
      unsigned char *buf, *ptr;
      int left;
    } file;
    memHandle mem_string;
    struct {
      int fd;
    } pipe;
#if HAVE_SOCKET
    struct {
      socket_domain domain;
      socket_info flags;
      int fd;
    } socket;
#endif
    struct {
      const unsigned char *buf, *ptr;
    } irl;
  } u;
  Int charcount, linecount, linepos;
  stream_flags_t status;
#if defined(YAPOR) || defined(THREADS)
  lockvar streamlock; /* protect stream access */
#endif
  int (*stream_putc)(
      int, int); /** function the stream uses for writing a single octet */
  int (*stream_wputc)(
      int, wchar_t); /** function the stream uses for writing a character */
  int (*stream_getc)(int); /** function the stream uses for reading an octet. */
  int (*stream_wgetc)(
      int); /** function the stream uses for reading a character. */

  int (*stream_wgetc_for_read)(
      int); /* function the stream uses for parser. It may be different
               from above if the ISO  character conversion is on */
  encoding_t encoding; /** current encoding for stream */
} StreamDesc;

static inline bool IsStreamTerm(Term t) {
  return !IsVarTerm(t) &&
         (IsAtomTerm(t) ||
          (IsApplTerm(t) && (FunctorOfTerm(t) == FunctorStream)));
}

static inline StreamDesc *Yap_GetStreamHandle(Term t) {
  int sno = Yap_CheckStream(t, 0, "stream search");
  if (sno < 0)
    return NULL;
  return GLOBAL_Stream + sno;
}

#define YAP_ERROR NIL

#define MaxStreams 64

#define EXPAND_FILENAME 0x000080

#define StdInStream 0
#define StdOutStream 1
#define StdErrStream 2

#define ALIASES_BLOCK_SIZE 8

void Yap_InitStdStreams(void);
Term Yap_StreamPosition(int);

static inline Int GetCurInpPos(StreamDesc *inp_stream) {
  return (inp_stream->linecount);
}

#define PlIOError(type, culprit, ...)                                          \
  PlIOError__(__FILE__, __FUNCTION__, __LINE__, type, culprit, __VA_ARGS__)

Int PlIOError__(const char *, const char *, int, yap_error_number, Term, ...);

int GetFreeStreamD(void);
Term Yap_MkStream(int n);

bool Yap_PrintWarning(Term twarning);

void Yap_plwrite(Term, struct stream_desc *, int, int, int);
void Yap_WriteAtom(struct stream_desc *s, Atom atom);

Term Yap_scan_num(struct stream_desc *);

void Yap_DefaultStreamOps(StreamDesc *st);
void Yap_PipeOps(StreamDesc *st);
void Yap_MemOps(StreamDesc *st);
bool Yap_CloseMemoryStream(int sno);
void Yap_ConsolePipeOps(StreamDesc *st);
void Yap_SocketOps(StreamDesc *st);
void Yap_ConsoleSocketOps(StreamDesc *st);
bool Yap_ReadlineOps(StreamDesc *st);
int Yap_OpenBufWriteStream(USES_REGS1);
void Yap_ConsoleOps(StreamDesc *s);

void Yap_InitRandomPreds(void);
void Yap_InitSignalPreds(void);
void Yap_InitTimePreds(void);

void Yap_init_socks(char *host, long interface_port);
void Yap_InitPipes(void);
void Yap_InitMem(void);
void Yap_InitSockets(void);
void Yap_InitSocketLayer(void);
void Yap_InitMems(void);
void Yap_InitConsole(void);
void Yap_InitReadlinePreds(void);
bool Yap_InitReadline(Term);
void Yap_InitChtypes(void);
void Yap_InitCharsio(void);
void Yap_InitFormat(void);
void Yap_InitFiles(void);
void Yap_InitIOStreams(void);
void Yap_InitWriteTPreds(void);
void Yap_InitReadTPreds(void);
void Yap_socketStream(StreamDesc *s);
void Yap_ReadlineFlush(int sno);
Int Yap_ReadlinePeekChar(int sno);
int Yap_ReadlineForSIGINT(void);
bool Yap_ReadlinePrompt(StreamDesc *s);

Int Yap_peek(int sno);

Term Yap_syntax_error(TokEntry *tokptr, int sno);

int console_post_process_read_char(int, StreamDesc *);
int console_post_process_eof(StreamDesc *);
int post_process_read_wchar(int, size_t, StreamDesc *);
int post_process_weof(StreamDesc *);

bool is_same_tty(FILE *f1, FILE *f2);

int ISOWGetc(int sno);
int GetUTF8(int sno);
Term read_line(int sno);
int PlGets(int sno, UInt size, char *buf);
GetsFunc PlGetsFunc(void);
int PlGetc(int sno);
int FilePutc(int sno, int c);
int DefaultGets(int, UInt, char *);
int put_wchar(int sno, wchar_t ch);
Int GetStreamFd(int sno);
int ResetEOF(StreamDesc *s);
int EOFPeek(int sno);
int EOFWPeek(int sno);

void Yap_SetAlias(Atom arg, int sno);
bool Yap_AddAlias(Atom arg, int sno);
int Yap_CheckAlias(Atom arg);
int Yap_RemoveAlias(Atom arg, int snoinline);
void Yap_SetAlias(Atom arg, int sno);
void Yap_InitAliases(void);
void Yap_DeleteAliases(int sno);
bool Yap_FindStreamForAlias(Atom al);
bool Yap_FetchStreamAlias(int sno, Term t2 USES_REGS);

INLINE_ONLY inline EXTERN void count_output_char(int ch, StreamDesc *s);

Term Yap_StreamUserName(int sno);

INLINE_ONLY inline EXTERN void count_output_char(int ch, StreamDesc *s) {
  if (ch == '\n') {
#if MPWSHELL
    if (mpwshell && (sno == StdOutStream || sno == StdErrStream) &&
        !(s->status & Null_Stream_f)) {
      putc(MPWSEP, s->file);
      if (!(GLOBAL_Stream[LOCAL_output_stream].status & Null_Stream_f))
        fflush(stdout);
    }
#endif
    /* Inform that we have written a newline */
    ++s->charcount;
    ++s->linecount;
    s->linepos = 0;
  } else {
#if MAC
    if ((sno == StdOutStream || sno == StdErrStream) && s->linepos > 200)
      sno->stream_putc(sno, '\n');
#endif
    ++s->charcount;
    ++s->linepos;
  }
}

inline static Term StreamName(int i) { return (GLOBAL_Stream[i].user_name); }

inline static Atom StreamFullName(int i) { return (GLOBAL_Stream[i].name); }

inline static void console_count_output_char(int ch, StreamDesc *s) {
  CACHE_REGS
  if (ch == '\n') {
#if MPWSHELL
    if (mpwshell && (sno == StdOutStream || sno == StdErrStream) &&
        !(s->status & Null_Stream_f)) {
      putc(MPWSEP, s->file);
      if (!(GLOBAL_Stream[LOCAL_output_stream].status & Null_Stream_f))
        fflush(stdout);
    }
#endif
    ++s->charcount;
    ++s->linecount;
    s->linepos = 0;
    LOCAL_newline = TRUE;
    /* Inform we are not at the start of a newline */
  } else {
    LOCAL_newline = FALSE;
#if MAC
    if ((sno == StdOutStream || sno == StdErrStream) && s->linepos > 200)
      sno->stream_putc(sno, '\n');
#endif
    ++s->charcount;
    ++s->linepos;
  }
}

inline static Term StreamPosition(int sno) {
  CACHE_REGS
  Term sargs[5];
  Int cpos;
  cpos = GLOBAL_Stream[sno].charcount;
  sargs[0] = MkIntegerTerm(LOCAL_StartCharCount = cpos);
  sargs[1] = MkIntegerTerm(LOCAL_StartLineCount = GLOBAL_Stream[sno].linecount);
  sargs[2] = MkIntegerTerm(LOCAL_StartLinePos = GLOBAL_Stream[sno].linepos);
  sargs[3] = sargs[4] = MkIntTerm(0);
  return Yap_MkApplTerm(FunctorStreamPos, 5, sargs);
}

inline static Term CurrentPositionToTerm(void) {
  CACHE_REGS
  Term sargs[5];
  sargs[0] = MkIntegerTerm(LOCAL_StartCharCount);
  sargs[1] = MkIntegerTerm(LOCAL_StartLineCount);
  sargs[2] = MkIntegerTerm(LOCAL_StartLinePos);
  sargs[3] = sargs[4] = MkIntTerm(0);
  return Yap_MkApplTerm(FunctorStreamPos, 5, sargs);
}

extern FILE *Yap_stdin;
extern FILE *Yap_stdout;
extern FILE *Yap_stderr;

char *Yap_MemExportStreamPtr(int sno);

bool Yap_Exists(const char *f);

static inline void freeBuffer(const void *ptr) {
  CACHE_REGS
  if (ptr == NULL || ptr == LOCAL_FileNameBuf || ptr == LOCAL_FileNameBuf2)
    return;
  free((void *)ptr);
}

#endif
