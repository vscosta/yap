/**************************************************************************
 *									 *
 * File:		iopreds.h *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/

#ifndef IOPREDS_H
#define IOPREDS_H 1

#include "Atoms.h"
#include "Yap.h"
#include <stdlib.h>

#if defined(_MSC_VER) || defined(__MINGW32__)

#include <windows.h>

#endif

#include <wchar.h>

#include "YapStreams.h"

static inline bool IsStreamTerm(Term t) {
  return !IsVarTerm(t) &&
         (IsAtomTerm(t) ||
          (IsApplTerm(t) && (FunctorOfTerm(t) == FunctorStream)));
}

extern bool Yap_initStream(int sno, FILE *fd, const char *name, Term file_name,
                           encoding_t encoding, stream_flags_t flags,
                           Atom open_mode);

#
#define Yap_CheckStream(arg, kind, msg)                                        \
  Yap_CheckStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckStream__(const char *, const char *, int, Term, int,
                             const char *);
#define Yap_CheckTextStream(arg, kind, msg)                                    \
  Yap_CheckTextStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckTextStream__(const char *, const char *, int, Term, int,
                                 const char *);

#define Yap_CheckBinaryStream(arg, kind, msg)                                  \
  Yap_CheckBinaryStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckBinaryStream__(const char *, const char *, int, Term, int,
                                   const char *);

static inline StreamDesc *Yap_GetStreamHandle(Term t) {
  int sno = Yap_CheckStream(t, 0, "stream search");
  if (sno < 0)
    return NULL;
  return GLOBAL_Stream + sno;
}

#include "VFS.h"

/*
 * This file defines main data-structure for stream management,
 *
 */

extern size_t Yap_page_size;

#if HAVE_SOCKET
extern int Yap_sockets_io;

extern Term Yap_InitSocketStream(int, socket_info, socket_domain);
extern int Yap_CheckSocketStream(Term, const char *);
extern socket_domain Yap_GetSocketDomain(int);
extern socket_info Yap_GetSocketStatus(int);
extern void Yap_UpdateSocketStream(int, socket_info, socket_domain);

/* routines in ypsocks.c */
Int Yap_CloseSocket(int, socket_info, socket_domain);

#endif /* USE_SOCKET */

extern Term Yap_read_term(int inp_stream, Term opts, int nargs);
extern Term Yap_Parse(UInt prio, encoding_t enc, Term cmod);

extern void init_read_data(ReadData _PL_rd, struct stream_desc *s);

typedef int (*GetsFunc)(int, UInt, char *);

void Yap_InitStdStreams(void);
Term Yap_StreamPosition(int);

static inline Int GetCurInpPos(StreamDesc *inp_stream) {
  return (inp_stream->linecount);
}

#define PlIOError(type, culprit, ...)                                          \
  PlIOError__(__FILE__, __FUNCTION__, __LINE__, type, culprit, __VA_ARGS__)

extern Int PlIOError__(const char *, const char *, int, yap_error_number, Term, ...);

extern int GetFreeStreamD(void);
extern  Term Yap_MkStream(int n);

extern bool Yap_PrintWarning(Term twarning);

extern void Yap_plwrite(Term, struct stream_desc *, int, int, int);
extern void Yap_WriteAtom(struct stream_desc *s, Atom atom);
extern bool Yap_WriteTerm( int output_stream, Term t, Term opts USES_REGS);

extern Term Yap_scan_num(struct stream_desc *);

extern void Yap_DefaultStreamOps(StreamDesc *st);
extern void Yap_PipeOps(StreamDesc *st);
extern void Yap_MemOps(StreamDesc *st);
extern bool Yap_CloseMemoryStream(int sno);
extern void Yap_ConsolePipeOps(StreamDesc *st);
extern void Yap_SocketOps(StreamDesc *st);
extern void Yap_ConsoleSocketOps(StreamDesc *st);
extern bool Yap_ReadlineOps(StreamDesc *st);
extern int Yap_OpenBufWriteStream(USES_REGS1);
extern void Yap_ConsoleOps(StreamDesc *s);

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
bool Yap_DoPrompt(StreamDesc *s);

Int Yap_peek(int sno);
int Yap_MemPeekc(int sno);

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

/** VFS handling */

VFS_t *Yap_InitAssetManager(void);

#endif
