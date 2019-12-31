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

INLINE_ONLY UInt PRED_HASH(FunctorEntry *, Term, UInt);
INLINE_ONLY bool IsStreamTerm(Term t) {
  return !IsVarTerm(t) &&
         (IsAtomTerm(t) ||
          (IsApplTerm(t) && (FunctorOfTerm(t) == FunctorStream)));
}

extern bool Yap_initStream(int sno, FILE *fd, Atom name, const char *io_mode, Term file_name, encoding_t encoding,
                           stream_flags_t flags, void *vfs);

#define Yap_CheckStream(arg, kind, msg)                                        \
  Yap_CheckStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckStream__(const char *, const char *, int, Term, int,
                             const char *);
#define Yap_CheckTextStream(arg, kind, msg)                                    \
  Yap_CheckTextStream__(__FILE__, __FUNCTION__, __LINE__, arg, kind, msg)
extern int Yap_CheckTextStream__(const char *, const char *, int, Term, int,
                                 const char *);

#define Yap_CheckTextReadStream(arg, msg)    \
  Yap_CheckTextReadStream__(__FILE__, __FUNCTION__, __LINE__, arg, msg)
extern int Yap_CheckTextReadStream__(const char *, const char *, int, Term,
                                 const char *);
#define Yap_CheckTextWriteStream(arg, msg) \
 Yap_CheckTextWriteStream__(__FILE__, __FUNCTION__, __LINE__, arg, msg)
extern int Yap_CheckTextWriteStream__(const char *, const char *, int, Term,
                                 const char *);

#define Yap_CheckBinaryStream(arg, kind, msg)     \
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

extern bool Yap_clearInput(int sno);
extern Term Yap_read_term(int inp_stream, Term opts, bool clause);
extern Term Yap_Parse(UInt prio, encoding_t enc, Term cmod);

extern void init_read_data(ReadData _PL_rd, struct stream_desc *s);

typedef int (*GetsFunc)(int, UInt, char *);

extern void Yap_InitStdStreams(void);
extern Term Yap_StreamPosition(int);
extern void Yap_CloseStream(int sno);

static inline Int GetCurInpLine(StreamDesc *inp_stream) {
    return (inp_stream->linecount);
}

static inline Int GetCurInpPos(StreamDesc *inp_stream) {
    return (inp_stream->charcount);
}
extern bool Yap_SetCurInpPos(int sno,  Int pos USES_REGS);

typedef enum {
  CREATE_DIRECTORY,
  CREATE_FILE
}io_kind_t;

#define PlIOError(type, culprit, ...)                                          \
  PlIOError__(__FILE__, __FUNCTION__, __LINE__, type, culprit, __VA_ARGS__)

extern Int PlIOError__(const char *, const char *, int, yap_error_number, Term, 
                       ...);


#define UnixIOError(errorno, io_kind, culprit, ...)   \
  UnixIOError__(__FILE__, __FUNCTION__, __LINE__, errorno, io_kind, culprit, __VA_ARGS__)

extern bool UnixIOError__(const char *, const char *, int, int, io_kind_t, Term, ...);

extern int GetFreeStreamD(void);
extern Term Yap_MkStream(int n);

extern bool Yap_PrintWarning(Term twarning);

extern void Yap_plwrite(Term, struct stream_desc *, int, int, xarg *);
extern void Yap_WriteAtom(struct stream_desc *s, Atom atom);
extern bool Yap_WriteTerm(int output_stream, Term t, Term opts USES_REGS);
extern Term Yap_scan_num(struct stream_desc *, bool);

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

extern void Yap_InitRandomPreds(void);
extern void Yap_InitSignalPreds(void);
extern void Yap_InitTimePreds(void);

extern void Yap_init_socks(char *host, long interface_port);
extern void Yap_InitPipes(void);
extern void Yap_InitMem(void);
extern void Yap_InitSockets(void);
extern void Yap_InitSocketLayer(void);
extern void Yap_InitMems(void);
extern void Yap_InitConsole(void);
extern void Yap_InitReadlinePreds(void);
extern bool Yap_InitReadline(Term);
extern bool Yap_readline_clear_pending_input(StreamDesc *s);
extern void Yap_InitChtypes(void);
extern void Yap_InitCharsio(void);
extern void Yap_InitFormat(void);
extern void Yap_InitFiles(void);
extern void Yap_InitIOStreams(void);
extern void Yap_InitWriteTPreds(void);
extern void Yap_InitReadTPreds(void);
extern void Yap_socketStream(StreamDesc *s);
extern void Yap_ReadlineFlush(int sno);
extern int Yap_ReadlinePeekChar(int sno);
extern int Yap_ReadlineForSIGINT(void);
extern bool Yap_DoPrompt(StreamDesc *s);

extern int Yap_peek(int sno);
extern int Yap_MemPeekc(int sno);

extern int Yap_popChar(int sno);
extern int Yap_popWide(int sno);
extern int Yap_peekWithGetc(int sno);
extern int Yap_peekWideWithGetwc(int sno);
extern int Yap_peekWideWithSeek(int sno);
    extern int Yap_peekWithSeek(int sno);
extern int Yap_peekWide(int sno);
extern int Yap_peekChar(int sno);


extern Term Yap_syntax_error(TokEntry *tokptr, int sno, const char *msg);

extern int console_post_process_read_char(int, StreamDesc *);
extern int console_post_process_eof(StreamDesc *);
extern int post_process_read_wchar(int, size_t, StreamDesc *);
extern int post_process_weof(StreamDesc *);

extern bool is_same_tty(FILE *f1, FILE *f2);

extern int ISOWGetc(int sno);
extern int GetUTF8(int sno);
extern Term read_line(int sno);
extern int PlGets(int sno, UInt size, char *buf);
extern GetsFunc PlGetsFunc(void);
extern int PlGetc(int sno);
extern int FilePutc(int sno, int c);
extern int DefaultGets(int, UInt, char *);
extern int put_wchar(int sno, wchar_t ch);
extern Int GetStreamFd(int sno);
extern int ResetEOF(StreamDesc *s);
extern int EOFPeek(int sno);
extern int EOFWPeek(int sno);

extern void Yap_SetAlias(Atom arg, int sno);
extern bool Yap_AddAlias(Atom arg, int sno);
extern int Yap_CheckAlias(Atom arg);
extern int Yap_RemoveAlias(Atom arg, int snoinline);
extern void Yap_SetAlias(Atom arg, int sno);
extern void Yap_InitAliases(void);
extern void Yap_DeleteAliases(int sno);
extern bool Yap_FindStreamForAlias(Atom al);
extern bool Yap_FetchStreamAlias(int sno, Term t2 USES_REGS);

INLINE_ONLY void count_output_char(int ch, StreamDesc *s);

extern Term Yap_StreamUserName(int sno);

INLINE_ONLY void count_output_char(int ch, StreamDesc *s) {
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

inline static Atom StreamFullName(int i) { return (Atom)(GLOBAL_Stream[i].name); }

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

char *Yap_MemExportStreamPtr(int sno);


static inline void freeBuffer(const void *ptr) {
  CACHE_REGS
  if (ptr == NULL || ptr == LOCAL_FileNameBuf || ptr == LOCAL_FileNameBuf2 ||
      ptr == AuxBase)
    return;
  free((void *)ptr);
}

#endif
