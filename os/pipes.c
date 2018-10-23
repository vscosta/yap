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
* File:		pipes.c							 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 *
 * @file pipes.c
 *
 * This file includes the definition of a pipe related IO.
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
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include "iopreds.h"

static int PipePutc( int, int);
static int ConsolePipePutc( int, int);
static int PipeGetc( int);
static int ConsolePipeGetc( int);

/* static */
static int
ConsolePipePutc (int sno, int ch)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
  {
    int out = 0;
    while (!out) {
      out = write(s->u.pipe.fd,  &c, sizeof(c));
      if (out <0) {
#if HAVE_STRERROR
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream pipe: %s", strerror(errno));
#else
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream pipe");
#endif	
      }
    }
  }
  count_output_char(ch,s);
  return ((int) ch);
}

static int
PipePutc (int sno, int ch)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
  {
    int out = 0;
    while (!out) {
      out = write(s->u.pipe.fd,  &c, sizeof(c));
      if (out <0) {
#if HAVE_STRERROR
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream pipe: %s", strerror(errno));
#else
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream pipe");
#endif	
      }
    }
  }
  console_count_output_char(ch,s);
  return ((int) ch);
}


/*
  Basically, the same as console but also sends a prompt and takes care of
  finding out whether we are at the start of a LOCAL_newline.
*/
static int
ConsolePipeGetc(int sno)
{
    CACHE_REGS
  StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;
  char c;
#if _MSC_VER || defined(__MINGW32__) 
  DWORD count;
#else
  int count;
#endif

  /* send the prompt away */
  if (LOCAL_newline) {
    char *cptr = LOCAL_Prompt, ch;
    /* use the default routine */
    while ((ch = *cptr++) != '\0') {
      GLOBAL_Stream[StdErrStream].stream_putc(StdErrStream, ch);
    }
    strncpy(LOCAL_Prompt, RepAtom (LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
    LOCAL_newline = false;
  }
  /* should be able to use a buffer */
  LOCAL_PrologMode |= ConsoleGetcMode;
  count = read(s->u.pipe.fd, &c, sizeof(char));
  LOCAL_PrologMode &= ~ConsoleGetcMode;
  if (count == 0) {
    return console_post_process_eof(s);
  } else if (count > 0) {
    ch = c;
  } else {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "read");
    return console_post_process_eof(s);
  }
  return console_post_process_read_char(ch, s);
}


static int
PipeGetc(int sno)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
  Int ch;
  char c;
  
  /* should be able to use a buffer */
  int count;
  count = read(s->u.pipe.fd, &c, sizeof(char));
  if (count == 0) {
    return EOF;
  } else if (count > 0) {
    ch = c;
  } else {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "at pipe getc: %s", strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "at pipe getc");
#endif
    return EOF;
  }
  return ch;
}

void
Yap_PipeOps( StreamDesc *st )
{
  st->stream_putc = PipePutc;
  st->stream_getc = PipeGetc;
}

void
Yap_ConsolePipeOps( StreamDesc *st )
{
  st->stream_putc = ConsolePipePutc;
  st->stream_getc = ConsolePipeGetc;
}

static Int
open_pipe_stream (USES_REGS1)
{
  Term t1, t2;
  StreamDesc *st;
  int sno;
  int filedes[2];

  if (
#if   _MSC_VER || defined(__MINGW32__)
      // assume for now only text streams...
      _pipe(filedes, 1024, O_TEXT)
#else
      pipe(filedes)
#endif      
      != 0)
    {
      return (PlIOError (SYSTEM_ERROR_INTERNAL,TermNil, "error %s",  strerror(errno)) );
    }
  sno = GetFreeStreamD();
  if (sno < 0)
    return (PlIOError (RESOURCE_ERROR_MAX_STREAMS,TermNil, "new stream not available for open_pipe_stream/2"));
  t1 = Yap_MkStream (sno);
  st = &GLOBAL_Stream[sno];
  st->status = Input_Stream_f | Pipe_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->stream_putc = PipePutc;
  st->stream_getc = PipeGetc;
  st->vfs = NULL;
  Yap_DefaultStreamOps( st );
  st->u.pipe.fd = filedes[0];
  st->file = fdopen( filedes[0], "r");
  UNLOCK(st->streamlock);
  sno = GetFreeStreamD();
  if (sno < 0)
    return (PlIOError (RESOURCE_ERROR_MAX_STREAMS,TermNil, "new stream not available for open_pipe_stream/2"));
  st = &GLOBAL_Stream[sno];
  st->status = Output_Stream_f | Pipe_Stream_f;
  st->linepos = 0;
  st->charcount = 0;
  st->linecount = 1;
  st->vfs = NULL;
  st->stream_putc = PipePutc;
  st->stream_getc = PipeGetc;
  Yap_DefaultStreamOps( st );
  st->u.pipe.fd = filedes[1];
  st->file = fdopen( filedes[1], "w");
  UNLOCK(st->streamlock);
  t2 = Yap_MkStream (sno);
  return
    Yap_unify (ARG1, t1) &&
    Yap_unify (ARG2, t2);
}

void
Yap_InitPipes( void )
{
  Yap_InitCPred ("open_pipe_stream", 2, open_pipe_stream, SafePredFlag);
}
