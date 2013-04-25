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
#define Tty_Stream_f		0x000040
#define Binary_Stream_f		0x000100
#define Eof_Error_Stream_f	0x000200
#define Reset_Eof_Stream_f	0x000400
#define Past_Eof_Stream_f	0x000800
#define Push_Eof_Stream_f	0x001000
#define Seekable_Stream_f	0x002000
#define Promptable_Stream_f	0x004000
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

void Yap_InitStdStreams(void);
Term Yap_StreamPosition(struct io_stream *);
void Yap_InitPlIO(void);

