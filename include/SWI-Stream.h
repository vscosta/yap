
#ifndef _PL_STREAM_H
#define _PL_STREAM_H

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE

#if (defined(__WINDOWS__) || defined(__CYGWIN__)) && !defined(__LCC__)
#define HAVE_DECLSPEC
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) type
#define install_t	 	void
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)	 	type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#  endif
#define install_t	 	__declspec(dllexport) void
# endif
#else /*HAVE_DECLSPEC*/
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	extern type
#define install_t	 	void
#endif /*HAVE_DECLSPEC*/
#endif /*_PL_EXPORT_DONE*/

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

#define SmakeFlag(n)	(1<<(n-1))

#define SIO_FBUF	SmakeFlag(1)	/* full buffering */
#define SIO_LBUF	SmakeFlag(2)	/* line buffering */
#define SIO_NBUF	SmakeFlag(3)	/* no buffering */
#define SIO_FEOF	SmakeFlag(4)	/* end-of-file */
#define SIO_FERR	SmakeFlag(5)	/* error ocurred */
#define SIO_USERBUF	SmakeFlag(6)	/* buffer is from user */
#define SIO_INPUT	SmakeFlag(7)	/* input stream */
#define SIO_OUTPUT	SmakeFlag(8)	/* output stream */
#define SIO_NOLINENO	SmakeFlag(9)	/* line no. info is void */
#define SIO_NOLINEPOS	SmakeFlag(10)	/* line pos is void */
#define SIO_STATIC	SmakeFlag(11)	/* Stream in static memory */
#define SIO_RECORDPOS	SmakeFlag(12)	/* Maintain position */
#define SIO_FILE	SmakeFlag(13)	/* Stream refers to an OS file */
#define SIO_PIPE	SmakeFlag(14)	/* Stream refers to an OS pipe */
#define SIO_NOFEOF	SmakeFlag(15)	/* don't set SIO_FEOF flag */
#define SIO_TEXT	SmakeFlag(16)	/* text-mode operation */
#define SIO_FEOF2	SmakeFlag(17)	/* attempt to read past eof */
#define SIO_FEOF2ERR	SmakeFlag(18)	/* Sfpasteof() */
#define SIO_NOCLOSE     SmakeFlag(19)	/* Do not close on abort */
#define SIO_APPEND	SmakeFlag(20)	/* opened in append-mode */
#define SIO_UPDATE	SmakeFlag(21)	/* opened in update-mode */
#define SIO_ISATTY	SmakeFlag(22)	/* Stream is a tty */
#define SIO_CLOSING	SmakeFlag(23)	/* We are closing the stream */
#define SIO_TIMEOUT	SmakeFlag(24)	/* We had a timeout */
#define SIO_NOMUTEX	SmakeFlag(25)	/* Do not allow multi-thread access */
#define SIO_ADVLOCK	SmakeFlag(26)	/* File locked with advisory lock */
#define SIO_WARN	SmakeFlag(27)	/* Pending warning */
#define SIO_CLEARERR	SmakeFlag(28)	/* Clear error after reporting */
#define SIO_REPXML	SmakeFlag(29)	/* Bad char --> XML entity */
#define SIO_REPPL	SmakeFlag(30)	/* Bad char --> Prolog \hex\ */
#define SIO_BOM		SmakeFlag(31)	/* BOM was detected/written */

#define	SIO_SEEK_SET	0	/* From beginning of file.  */
#define	SIO_SEEK_CUR	1	/* From current position.  */
#define	SIO_SEEK_END	2	/* From end of file.  */

#define Sinput  (&S__iob[0])		/* Stream Sinput */
#define Soutput (&S__iob[1])		/* Stream Soutput */
#define Serror  (&S__iob[2])		/* Stream Serror */

#define Sgetchar()	Sgetc(Sinput)
#define Sputchar(c)	Sputc((c), Soutput)

#define S__updatefilepos_getc(s, c) \
	((s)->position ? S__fupdatefilepos_getc((s), (c)) \
		       : (c))

#define Snpgetc(s) ((s)->bufp < (s)->limitp ? (int)(*(s)->bufp++)&0xff \
					    : S__fillbuf(s))
#define Sgetc(s) S__updatefilepos_getc((s), Snpgetc(s))

/* Control-operations */
#define SIO_GETSIZE	(1)		/* get size of underlying object */
#define SIO_GETFILENO	(2)		/* get underlying file (if any) */
#define SIO_SETENCODING	(3)		/* modify encoding of stream */

/* Sread_pending() */
#define SIO_RP_BLOCK 0x1		/* wait for new input */

PL_EXPORT(void)		Sseterr(IOSTREAM *s, int which, const char *message);
PL_EXPORT(int)		S__fillbuf(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	Snew(void *handle, int flags, IOFUNCTIONS *functions);
PL_EXPORT(int)	   	Sfileno(IOSTREAM *s);
PL_EXPORT(int)		Sgetcode(IOSTREAM *s);
PL_EXPORT(int)		Sungetc(int c, IOSTREAM *s);
PL_EXPORT(int)		Sputcode(int c, IOSTREAM *s);
PL_EXPORT(int)		Sfeof(IOSTREAM *s);
PL_EXPORT(int)		Sfpasteof(IOSTREAM *s);
PL_EXPORT(int)		Sferror(IOSTREAM *s);
PL_EXPORT(void)		Sclearerr(IOSTREAM *s);
PL_EXPORT(void)		Sseterr(IOSTREAM *s, int which, const char *message);

#endif /*_PL_STREAM_H*/
