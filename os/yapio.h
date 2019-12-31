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
 * File:		yapio.h * Last
 *rev:	22/1/03							 * mods:
 ** comments:	Input/Output information				 *
 *									 *
 *************************************************************************/

#ifndef YAPIO_H

#define YAPIO_H 1

#ifdef SIMICS
#undef HAVE_LIBREADLINE
#endif

#include <stdio.h>
#include <wchar.h>

#include "YapIOConfig.h"
#include <VFS.h>
#include <Yatom.h>


#define WRITE_DEFS()                                                           \
  PAR("module", isatom, WRITE_MODULE)                                          \
  , PAR("attributes", isatom, WRITE_ATTRIBUTES),                               \
      PAR("cycles", booleanFlag, WRITE_CYCLES),                                \
      PAR("quoted", booleanFlag, WRITE_QUOTED),                                \
      PAR("ignore_ops", booleanFlag, WRITE_IGNORE_OPS),                        \
      PAR("max_depth", nat, WRITE_MAX_DEPTH),                                  \
      PAR("numbervars", booleanFlag, WRITE_NUMBERVARS),                        \
      PAR("singletons", booleanFlag, WRITE_SINGLETONS),                        \
      PAR("portrayed", booleanFlag, WRITE_PORTRAYED),                          \
      PAR("portray", booleanFlag, WRITE_PORTRAY),                              \
      PAR("priority", nat, WRITE_PRIORITY),                                    \
      PAR("character_escapes", booleanFlag, WRITE_CHARACTER_ESCAPES),          \
      PAR("backquotes", booleanFlag, WRITE_BACKQUOTES),                        \
      PAR("brace_terms", booleanFlag, WRITE_BRACE_TERMS),                      \
      PAR("fullstop", booleanFlag, WRITE_FULLSTOP),                            \
      PAR("nl", booleanFlag, WRITE_NL),                                        \
      PAR("variable_names", ok, WRITE_VARIABLE_NAMES),                         \
      PAR(NULL, ok, WRITE_END)
#define PAR(x, y, z) z
typedef enum write_enum_choices { WRITE_DEFS() } write_choices_t;


#ifdef BEAM
int beam_write(USES_REGS1) {
  Yap_StartSlots();
  Yap_plwrite(ARG1, GLOBAL_Stream + LOCAL_c_output_stream, LOCAL_max_depth, 0,
              NULL);
  Yap_CloseSlots();
  Yap_RaiseException();
  return (TRUE);
}
#endif

#ifndef _PL_WRITE_

#define EOFCHAR EOF

#endif

/* info on aliases */
typedef struct AliasDescS {
  Atom name;
  int alias_stream;
} * AliasDesc;

#define MAX_ISO_LATIN1 255

typedef struct scanner_extra_params {
  Term tposINPUT, tposOUTPUT;
  Term backquotes, singlequotes, doublequotes;
  bool ce, vprefix, vn_asfl;
    Term tcomms;       /// Access to comments
    Term cmod;         /// Access to commen
  bool store_comments; //
  bool get_eot_blank;
} scanner_params;

/**
 *
 * @return a new VFS that will support /assets
 */

extern struct vfs *Yap_InitAssetManager(void);

/* routines in parser.c */
extern VarEntry *Yap_LookupVar(const char *);
extern Term Yap_VarNames(VarEntry *, Term);
extern Term Yap_Variables(VarEntry *, Term);
extern Term Yap_Singletons(VarEntry *, Term);

/* routines in scanner.c */
extern TokEntry *Yap_tokenizer(struct stream_desc *, scanner_params *sp);
extern void Yap_clean_tokenizer(void);
extern char *Yap_AllocScannerMemory(unsigned int);

/* routines in iopreds.c */
extern FILE *Yap_FileDescriptorFromStream(Term);
extern Int Yap_FirstLineInParse(void);
extern int Yap_CheckIOStream(Term, char *);
#if defined(YAPOR) || defined(THREADS)
extern void Yap_LockStream(void *);
extern void Yap_UnLockStream(void *);
#else
#define Yap_LockStream(X)
#define Yap_UnLockStream(X)
#endif
extern Int Yap_GetStreamFd(int);
extern void Yap_CloseStreams(void);
extern void Yap_CloseTemporaryStreams(void);
extern void Yap_FlushStreams(void);
extern void Yap_ReleaseStream(int);
extern int Yap_PlGetchar(void);
extern int Yap_PlGetWchar(void);
extern int Yap_PlFGetchar(void);
extern int Yap_GetCharForSIGINT(void);
extern Int Yap_StreamToFileNo(Term);
extern int Yap_OpenStream(Term tin, const char *io_mode, Term user_name,
                          encoding_t enc);
extern int Yap_FileStream(FILE *, Atom, Term, int, VFS_t *);
extern char *Yap_TermToBuffer(Term t, int flags);
extern char *Yap_HandleToString(yhandle_t l, size_t sz, size_t *length,
                                encoding_t *encoding, int flags);
extern int Yap_GetFreeStreamD(void);
extern int Yap_GetFreeStreamDForReading(void);

extern Term Yap_BufferToTerm(const char *s, Term opts);
extern Term Yap_UBufferToTerm(const unsigned char *s, Term opts);

extern Term Yap_WStringToList(wchar_t *);
extern Term Yap_WStringToListOfAtoms(wchar_t *);
extern Atom Yap_LookupWideAtom(const wchar_t *);

/* grow.c */
extern int Yap_growheap_in_parser(void);
extern int Yap_growstack_in_parser(void);
extern int Yap_growtrail_in_parser(void);

typedef enum mem_buf_source {
  MEM_BUF_MALLOC = 1,
  MEM_BUF_USER = 2
} memBufSource;

extern char *Yap_MemStreamBuf(int sno);

extern char *Yap_StrPrefix(const char *buf, size_t n);

extern Term Yap_StringToNumberTerm(const char *s, encoding_t *encp,
                                   bool error_on);
extern int Yap_FormatFloat(Float f, char **s, size_t sz);
extern int Yap_open_buf_read_stream(const char *buf, size_t nchars,
                                    encoding_t *encp, memBufSource src,
                                    Atom name, Term uname);
extern int Yap_open_buf_write_stream(encoding_t enc, memBufSource src);
extern Term Yap_BufferToTerm(const char *s, Term opts);

extern X_API Term Yap_BufferToTermWithPrioBindings(const char *s, Term opts,
                                                   Term bindings, size_t sz,
                                                   int prio);
extern FILE *Yap_GetInputStream(Term t, const char *m);
extern FILE *Yap_GetOutputStream(Term t, const char *m);
extern Atom Yap_guessFileName(FILE *f, int sno, size_t max);

extern int Yap_CheckSocketStream(Term stream, const char *error);
extern void Yap_init_socks(char *host, long interface_port);

extern bool Yap_flush(int sno);

extern uint64_t HashFunction(const unsigned char *);
extern uint64_t WideHashFunction(wchar_t *);

extern void Yap_InitAbsfPreds(void);

INLINE_ONLY Term MkCharTerm(Int c);

/**
 * MkCharTerm: convert a character into a single atom.
 *
 * @param c the character code
 *
 * @return the term.
 */
INLINE_ONLY Term MkCharTerm(Int c) {
  unsigned char cs[10];
  if (c < 0)
    return TermEof;
  size_t n = put_utf8(cs, c);
  cs[n] = '\0';
  return MkAtomTerm(Yap_ULookupAtom(cs));
}

extern char *GLOBAL_cwd;

INLINE_ONLY char *Yap_VF(const char *path) {
  char *out;

  out = (char *)malloc(YAP_FILENAME_MAX + 1);
  if (GLOBAL_cwd == NULL || GLOBAL_cwd[0] == 0 ||
      !Yap_IsAbsolutePath(path, false)) {
    return (char *)path;
  }
  strcpy(out, GLOBAL_cwd);
  strcat(out, "/");
  strcat(out, path);
  return out;
}

INLINE_ONLY char *Yap_VFAlloc(const char *path) {
  char *out;

  out = (char *)malloc(YAP_FILENAME_MAX + 1);
  if (GLOBAL_cwd == NULL || GLOBAL_cwd[0] == 0 ||
      !Yap_IsAbsolutePath(path, false)) {
    return (char *)path;
  }
  strcpy(out, GLOBAL_cwd);
  strcat(out, "/");
  strcat(out, path);
  return out;
}

/// UT when yap started
extern uint64_t Yap_StartOfWTimes;

extern bool Yap_HandleSIGINT(void);

#endif
