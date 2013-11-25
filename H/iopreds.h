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

#define StdInStream	0
#define StdOutStream	1
#define	StdErrStream	2

#define ALIASES_BLOCK_SIZE 8

void Yap_InitStdStreams(void);
Term Yap_StreamPosition(struct io_stream *);
void Yap_InitPlIO(void);
