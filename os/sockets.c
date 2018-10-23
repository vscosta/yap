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
* File:		sockets.c							 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a socket related IO.
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

#if HAVE_SOCKET
static int SocketPutc( int, int);
static int ConsoleSocketPutc( int, int);
static int SocketGetc( int);
static int ConsoleSocketGetc( int);

void
Yap_SocketOps( StreamDesc *st )
{
  st->stream_putc = SocketPutc;
  st->stream_getc = SocketGetc;
}

void
Yap_ConsoleSocketOps( StreamDesc *st )
{
  st->stream_putc = ConsoleSocketPutc;
  st->stream_getc = ConsoleSocketGetc;
}

void
Yap_socketStream( StreamDesc *s )
{
    CACHE_REGS
  if (LOCAL_sockets_io &&
      s->file == NULL)
    {
      s->status |= Socket_Stream_f;
      s->u.socket.domain = af_inet;
      s->u.socket.flags = client_socket;
      s->u.socket.fd = 0;
      return;
    }
}

/*
   sockets cannot use standard FILE *, we have to go through fds, and in the
   case of VC++, we have to use the receive routines...
*/
static int
SocketGetc(int sno)
{
  register StreamDesc *s = &GLOBAL_Stream[sno];
  register Int ch;
  char c;
  int count;
  /* should be able to use a buffer */
#if _MSC_VER || defined(__MINGW32__)
  count = recv(s->u.socket.fd, &c, sizeof(char), 0);
#else
  count = read(s->u.socket.fd, &c, sizeof(char));
#endif
  if (count == 0) {
    s->u.socket.flags = closed_socket;
    return EOF;
  } else if (count > 0) {
    ch = c;
  } else {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
	    "( socket_getc: %s)", strerror(errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
	    "(socket_getc)");
#endif
    return EOF;
  }
  return ch;
}

/*
  Basically, the same as console but also sends a prompt and takes care of
  finding out whether we are at the start of a newline.
*/
static int
ConsoleSocketGetc(int sno)
{
    CACHE_REGS
  register StreamDesc *s = &GLOBAL_Stream[sno];
  int ch;
  Int c;
  int count;

  /* send the prompt away */
  if (LOCAL_newline) {
    char *cptr = LOCAL_Prompt, ch;
    /* use the default routine */
    while ((ch = *cptr++) != '\0') {
      GLOBAL_Stream[StdErrStream].stream_putc(StdErrStream, ch);
    }
    strncpy(LOCAL_Prompt, RepAtom (LOCAL_AtPrompt)->StrOfAE, MAX_PROMPT);
    LOCAL_newline = FALSE;
  }
  /* should be able to use a buffer */
  LOCAL_PrologMode |= ConsoleGetcMode;
#if _MSC_VER || defined(__MINGW32__)
  count = recv(s->u.socket.fd, (void *)&c, sizeof(char), 0);
#else
  count = read(s->u.socket.fd, &c, sizeof(char));
#endif
  LOCAL_PrologMode &= ~ConsoleGetcMode;
  if (count == 0) {
    return console_post_process_eof(s);
  } else if (count > 0) {
    ch = c;
  } else {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "read");
    return console_post_process_eof(s);
  }
  return ch;
}

/* static */
static int
ConsoleSocketPutc (int sno, int ch)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__)
  send(s->u.socket.fd,  &c, sizeof(c), 0);
#else
  if (write(s->u.socket.fd,  &c, sizeof(c)) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "no access to console: %s", strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_FATAL, TermNil, "no access to console");
#endif
  }
#endif
  count_output_char(ch,s);
  return ((int) ch);
}

static int
SocketPutc (int sno, int ch)
{
  StreamDesc *s = &GLOBAL_Stream[sno];
  char c = ch;
#if MAC || _MSC_VER
  if (ch == 10)
    {
      ch = '\n';
    }
#endif
#if _MSC_VER || defined(__MINGW32__)
  send(s->u.socket.fd,  &c, sizeof(c), 0);
#else
  {
    int out = 0;
    while (!out) {
      out = write(s->u.socket.fd,  &c, sizeof(c));
      if (out <0) {
#if HAVE_STRERROR
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream socket: %s", strerror(errno));
#else
	Yap_Error(PERMISSION_ERROR_INPUT_STREAM, TermNil, "error writing stream socket");
#endif
      }
    }
  }
#endif
  return (int) ch;
}


/* given a socket file descriptor, get the corresponding stream descripor */
int
Yap_CheckIOStream(Term stream, char * error)
{
  int sno = Yap_CheckStream(stream, Input_Stream_f|Output_Stream_f|Socket_Stream_f, error);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return(sno);
}

Term
Yap_InitSocketStream(int fd, socket_info flags, socket_domain domain) {
  StreamDesc *st;
  int sno;

  sno = GetFreeStreamD();
  if (sno < 0) {
    PlIOError (SYSTEM_ERROR_INTERNAL,TermNil, "new stream not available for socket/4");
    return(TermNil);
  }
  st = &GLOBAL_Stream[sno];
  st->u.socket.domain = domain;
  st->u.socket.flags = flags;
  st->vfs = NULL;
  if (flags & (client_socket|server_session_socket)) {
    /* I can read and write from these sockets */
    st->status = (Socket_Stream_f|Input_Stream_f|Output_Stream_f);
  } else {
    /* oops, I cannot */
    st->status = Socket_Stream_f;
  }
  st->u.socket.fd = fd;
  // use dup and have two streams?
  st->file = fdopen( fd, "rw");
  st->charcount = 0;
  st->linecount = 1;
  st->linepos = 0;
  st->vfs = NULL;
  st->buf.on = false;
  st->stream_putc = SocketPutc;
  st->stream_getc = SocketGetc;
  Yap_DefaultStreamOps( st );
  UNLOCK(st->streamlock);
  return(Yap_MkStream(sno));
}

/* given a socket file descriptor, get the corresponding stream descripor */
int
Yap_CheckSocketStream(Term stream, const char * error)
{
  int sno = Yap_CheckStream(stream, Socket_Stream_f, error);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return sno;
}

/* given a stream index, get the corresponding domain */
socket_domain
Yap_GetSocketDomain(int sno)
{
  return(GLOBAL_Stream[sno].u.socket.domain);
}

/* given a stream index, get the corresponding status */
socket_info
Yap_GetSocketStatus(int sno)
{
  return(GLOBAL_Stream[sno].u.socket.flags);
}

/* update info on a socket, eg, new->server or new->client */
void
Yap_UpdateSocketStream(int sno, socket_info flags, socket_domain domain) {
  StreamDesc *st;

  st = &GLOBAL_Stream[sno];
  st->u.socket.domain = domain;
  st->u.socket.flags = flags;
  if (flags & (client_socket|server_session_socket)) {
    /* I can read and write from these sockets */
    st->status = (Socket_Stream_f|Input_Stream_f|Output_Stream_f);
  } else {
    /* oops, I cannot */
    st->status = Socket_Stream_f;
  }
}


void
Yap_InitSockets( void )
{
  Yap_InitSocketLayer(  );
}


#endif
