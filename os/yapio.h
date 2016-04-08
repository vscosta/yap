/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2003	 *
*									 *
**************************************************************************
*									 *
* File:		yapio.h							 *
* Last rev:	22/1/03							 *
* mods:									 *
* comments:	Input/Output information				 *
*									 *
*************************************************************************/

#ifndef YAPIO_H

#define YAPIO_H 1

#ifdef SIMICS
#undef HAVE_LIBREADLINE
#endif

#include <stdio.h>
#include <wchar.h>

#include "os/YapIOConfig.h"
#include <Yatom.h>

#ifndef _PL_WRITE_

#define EOFCHAR EOF

#endif

/* info on aliases */
typedef struct AliasDescS {
  Atom name;
  int alias_stream;
} * AliasDesc;

#define MAX_ISO_LATIN1 255

/* parser stack, used to be AuxSp, now is ASP */
#define ParserAuxSp LOCAL_ScannerStack

/* routines in parser.c */
VarEntry *Yap_LookupVar(const char *);
Term Yap_VarNames(VarEntry *, Term);
Term Yap_Variables(VarEntry *, Term);
Term Yap_Singletons(VarEntry *, Term);

/* routines in scanner.c */
TokEntry *Yap_tokenizer(struct stream_desc *, bool, Term *d);
void Yap_clean_tokenizer(TokEntry *, VarEntry *, VarEntry *);
char *Yap_AllocScannerMemory(unsigned int);

/* routines in iopreds.c */
FILE *Yap_FileDescriptorFromStream(Term);
Int Yap_FirstLineInParse(void);
int Yap_CheckIOStream(Term, char *);
#if defined(YAPOR) || defined(THREADS)
void Yap_LockStream(void *);
void Yap_UnLockStream(void *);
#else
#define Yap_LockStream(X)
#define Yap_UnLockStream(X)
#endif
Int Yap_GetStreamFd(int);
void Yap_CloseStreams(int);
void Yap_FlushStreams(void);
void Yap_ReleaseStream(int);
void Yap_CloseStream(int);
int Yap_PlGetchar(void);
int Yap_PlGetWchar(void);
int Yap_PlFGetchar(void);
int Yap_GetCharForSIGINT(void);
Int Yap_StreamToFileNo(Term);
int Yap_OpenStream(FILE *, char *, Term, int);
char *Yap_TermToString(Term t, char *s, size_t sz, size_t *length,
                       encoding_t *encoding, int flags);
char *Yap_HandleToString(yhandle_t l, size_t sz, size_t *length,
                         encoding_t *encoding, int flags);
int Yap_GetFreeStreamD(void);
int Yap_GetFreeStreamDForReading(void);

Term Yap_WStringToList(wchar_t *);
Term Yap_WStringToListOfAtoms(wchar_t *);
Atom Yap_LookupWideAtom(const wchar_t *);

#define Quote_illegal_f 0x01
#define Ignore_ops_f 0x02
#define Handle_vars_f 0x04
#define Use_portray_f 0x08
#define To_heap_f 0x10
#define Unfold_cyclics_f 0x20
#define Use_SWI_Stream_f 0x40
#define BackQuote_String_f 0x80
#define AttVar_None_f 0x100
#define AttVar_Dots_f 0x200
#define AttVar_Portray_f 0x400
#define Blob_Portray_f 0x800
#define No_Escapes_f 0x1000
#define No_Brace_Terms_f 0x2000
#define Fullstop_f 0x4000
#define New_Line_f 0x8000

/* grow.c */
int Yap_growheap_in_parser(tr_fr_ptr *, TokEntry **, VarEntry **);
int Yap_growstack_in_parser(tr_fr_ptr *, TokEntry **, VarEntry **);
int Yap_growtrail_in_parser(tr_fr_ptr *, TokEntry **, VarEntry **);

bool Yap_IsAbsolutePath(const char *p);
Atom Yap_TemporaryFile(const char *prefix, int *fd);
const char *Yap_AbsoluteFile(const char *spec, bool expand);

typedef enum mem_buf_source {
  MEM_BUF_CODE = 1,
  MEM_BUF_MALLOC = 2,
  MEM_BUF_USER = 4
} memBufSource;

char *Yap_MemStreamBuf(int sno);

extern Term Yap_StringToTerm(const char *s, size_t len, encoding_t *encp,
                             int prio, Term *bindings_p);
extern Term Yap_StringToNumberTerm(char *s, encoding_t *encp);
int Yap_FormatFloat(Float f, char **s, size_t sz);
int Yap_open_buf_read_stream(const char *nbuf, size_t nchars, encoding_t *encp,
                             memBufSource src);
int Yap_open_buf_write_stream(char *nbuf, size_t nchars, encoding_t *encp,
                              memBufSource src);
Term Yap_ReadFromAtom(Atom a, Term opts);
FILE *Yap_GetInputStream(Term t, const char *m);
FILE *Yap_GetOutputStream(Term t, const char *m);
char *Yap_guessFileName(FILE *f, int sno, char *nameb, size_t max);
void Yap_plwrite(Term t, struct stream_desc *mywrite, int max_depth, int flags,
                 int priority);

int Yap_CheckSocketStream(Term stream, const char *error);
void Yap_init_socks(char *host, long interface_port);

#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif

uint64_t HashFunction(const unsigned char *);
uint64_t WideHashFunction(wchar_t *);

INLINE_ONLY inline EXTERN Term MkCharTerm(Int c);

/**
 * MkCharTerm: convert a character into a single atom.
 *
 * @param c the character code
 *
 * @return the term.
 */
INLINE_ONLY inline EXTERN Term MkCharTerm(Int c) {
  wchar_t cs[2];
  if (c < 0)
    return TermEof;
  cs[0] = c;
  cs[1] = '\0';
  return MkAtomTerm(Yap_LookupMaybeWideAtom(cs));
}

/// UT when yap started
uint64_t Yap_StartOfWTimes;

#endif
