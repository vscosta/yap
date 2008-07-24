
#ifndef _PL_STREAM_H
#define _PL_STREAM_H

#ifndef X_API
#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif
#endif

/* This appears to make the wide-character support compile and work
   on HPUX 11.23.  There really should be a cleaner way ...
*/
#if defined(__hpux)
#include <sys/_mbstate_t.h>
#endif

#if defined(_MSC_VER) && !defined(__WINDOWS__)
#define __WINDOWS__ 1
#endif

#include <stdarg.h>
#include <wchar.h>
#include <stddef.h>
#ifdef __WINDOWS__
typedef __int64 int64_t;
#if (_MSC_VER < 1300)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif
typedef intptr_t ssize_t;		/* signed version of size_t */
#else
#include <unistd.h>
#include <inttypes.h>			/* more portable than stdint.h */
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif

		 /*******************************
		 *	    CONSTANTS		*
		 *******************************/

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#if defined(__WINDOWS__) && !defined(EWOULDBLOCK)
#define EWOULDBLOCK	1000		/* Needed for socket handling */
#endif
#define EPLEXCEPTION	1001		/* errno: pending Prolog exception */

#define SIO_BUFSIZE	(4096)		/* buffering buffer-size */
#define SIO_LINESIZE	(1024)		/* Sgets() default buffer size */
#define SIO_MAGIC	(7212676)	/* magic number */
#define SIO_CMAGIC	(42)		/* we are close (and thus illegal!) */

typedef ssize_t (*Sread_function)(void *handle, char *buf, size_t bufsize);
typedef ssize_t (*Swrite_function)(void *handle, char*buf, size_t bufsize);
typedef long  (*Sseek_function)(void *handle, long pos, int whence);
typedef int64_t (*Sseek64_function)(void *handle, int64_t pos, int whence);
typedef int   (*Sclose_function)(void *handle);
typedef int   (*Scontrol_function)(void *handle, int action, void *arg);

#if defined(O_PLMT) && defined(PL_KERNEL)
#include "pl-mutex.h"
#define IOLOCK recursiveMutex
#else
typedef void *		IOLOCK;		/* Definition for external use */
#endif

typedef struct io_functions
{ Sread_function	read;		/* fill the buffer */
  Swrite_function	write;		/* empty the buffer */
  Sseek_function	seek;		/* seek to position */
  Sclose_function	close;		/* close stream */
  Scontrol_function	control;	/* Info/control */
  Sseek64_function	seek64;		/* seek to position (intptr_t files) */
} IOFUNCTIONS;

typedef struct io_position
{ int64_t		byteno;		/* byte-position in file */
  int64_t		charno;		/* character position in file */
  int			lineno;		/* lineno in file */
  int			linepos;	/* position in line */
  intptr_t		reserved[2];	/* future extensions */
} IOPOS;

					/* NOTE: check with encoding_names */
					/* in pl-file.c */
typedef enum
{ ENC_UNKNOWN = 0,			/* invalid/unknown */
  ENC_OCTET,				/* raw 8 bit input */
  ENC_ASCII,				/* US-ASCII (0..127) */
  ENC_ISO_LATIN_1,			/* ISO Latin-1 (0..256) */
  ENC_ANSI,				/* default (multibyte) codepage */
  ENC_UTF8,
  ENC_UNICODE_BE,			/* big endian unicode file */
  ENC_UNICODE_LE,			/* little endian unicode file */
  ENC_WCHAR				/* pl_wchar_t */
} IOENC;

#define SIO_NL_POSIX  0			/* newline as \n */
#define SIO_NL_DOS    1			/* newline as \r\n */
#define SIO_NL_DETECT 3			/* detect processing mode */

typedef struct io_stream
{ char		       *bufp;		/* `here' */
  char		       *limitp;		/* read/write limit */
  char		       *buffer;		/* the buffer */
  char		       *unbuffer;	/* Sungetc buffer */
  int			lastc;		/* last character written */
  int			magic;		/* magic number SIO_MAGIC */
  int  			bufsize;	/* size of the buffer */
  int			flags;		/* Status flags */
  IOPOS			posbuf;		/* location in file */
  IOPOS *		position;	/* pointer to above */
  void		       *handle;		/* function's handle */
  IOFUNCTIONS	       *functions;	/* open/close/read/write/seek */
  int		        locks;		/* lock/unlock count */
  IOLOCK *		mutex;		/* stream mutex */
					/* SWI-Prolog 4.0.7 */
  void			(*close_hook)(void* closure);
  void *		closure;
					/* SWI-Prolog 5.1.3 */
  int			timeout;	/* timeout (milliseconds) */
					/* SWI-Prolog 5.4.4 */
  char *		message;	/* error/warning message */
  IOENC			encoding;	/* character encoding used */
  struct io_stream *	tee;		/* copy data to this stream */
  mbstate_t *		mbstate;	/* ENC_ANSI decoding */
  struct io_stream *	upstream;	/* stream providing our input */
  struct io_stream *	downstream;	/* stream providing our output */
  unsigned		newline : 2;	/* Newline mode */
  intptr_t		reserved[3];	/* reserved for extension */
} IOSTREAM;

#define PL_EXPORT(type)		extern X_API type

extern X_API int	PL_unify_stream(term_t t, IOSTREAM *s);

#endif /*_PL_STREAM_H*/
