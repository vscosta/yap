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
* File:		iopreds.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file defines main data-structure for stream management, 
 *
 */

#if defined(_MSC_VER) || defined(__MINGW32__)

#include <windows.h>

#endif

#include <wchar.h>

#if HAVE_LIBREADLINE

#if defined(_MSC_VER) || defined(__MINGW32__)

FILE *rl_instream, *rl_outstream;
#endif

#endif

#define MEM_BUF_CODE   0
#define MEM_BUF_MALLOC 1

typedef int (*GetsFunc)(int, UInt, char *);

typedef struct stream_desc
  {
    union {
      struct {
	struct io_stream *swi_ptr;
      } swi_stream;
      struct {
	Atom name;
	Term user_name;
#if defined(__MINGW32__) || defined(_MSC_VER)
#define PLGETC_BUF_SIZE 4096
	char *buf, *ptr;
	int left;
#endif
	YP_File file;
      } file;
      struct {
	char *buf;         /* where the file is being read from/written to */
	int src;           /* where the space comes from, 0 code space, 1 malloc */
	Int max_size;	   /* maximum buffer size (may be changed dynamically) */
	UInt pos;
	volatile void *error_handler;
      } mem_string;
      struct {
#if defined(__MINGW32__) || defined(_MSC_VER)
	HANDLE hdl;
#else
	int fd;
#endif
      } pipe;
#if USE_SOCKET
      struct {
	socket_domain domain;
	socket_info flags;
	int fd;
      } socket;
#endif
    } u;
    Int charcount, linecount, linepos;
    Int status;
    int och;
#if defined(YAPOR) || defined(THREADS)
    lockvar  streamlock;        /* protect stream access */
#endif
    int (* stream_putc)(int, int);  /* function the stream uses for writing */
    int (* stream_getc)(int);       /* function the stream uses for reading */
    GetsFunc stream_gets;           /* function the stream uses for reading a sequence of characters */
    /* function the stream uses for parser. It may be different if the ISO
       character conversion is on */
    int (* stream_wgetc_for_read)(int);
    int (* stream_wgetc)(int);
    int (* stream_wputc)(int,wchar_t);
    encoding_t encoding;
    mbstate_t mbstate;
  }
StreamDesc;

#define YAP_ERROR NIL

#define MaxStreams 64

#define	Free_Stream_f		0x000001
#define Output_Stream_f		0x000002
#define Input_Stream_f		0x000004
#define Append_Stream_f		0x000008
#define Eof_Stream_f		0x000010
#define Null_Stream_f		0x000020
#define Tty_Stream_f		0x000040
#define Socket_Stream_f		0x000080
#define Binary_Stream_f		0x000100
#define Eof_Error_Stream_f	0x000200
#define Reset_Eof_Stream_f	0x000400
#define Past_Eof_Stream_f	0x000800
#define Push_Eof_Stream_f	0x001000
#define Seekable_Stream_f	0x002000
#define Promptable_Stream_f	0x004000
#if USE_SOCKET
#define Client_Socket_Stream_f	0x008000
#define Server_Socket_Stream_f	0x010000
#endif
#define InMemory_Stream_f	0x020000
#define Pipe_Stream_f		0x040000
#define Popen_Stream_f		0x080000
#define User_Stream_f		0x100000
#define HAS_BOM_f		0x200000
#define RepError_Prolog_f	0x400000
#define RepError_Xml_f		0x800000
#define SWI_Stream_f		0x1000000

#define EXPAND_FILENAME		0x000080

#define StdInStream	0
#define StdOutStream	1
#define	StdErrStream	2

#define ALIASES_BLOCK_SIZE 8

void STD_PROTO (Yap_InitStdStreams, (void));
Term STD_PROTO (Yap_StreamPosition, (int));

EXTERN inline int
GetCurInpPos (int inp_stream)
{
  return (Stream[inp_stream].linecount);
}

