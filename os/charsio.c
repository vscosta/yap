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
* File:		charcodes.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Character codes and character conversion		 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
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
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#include "iopreds.h"

static Int get_code(USES_REGS1);
static Int get_byte(USES_REGS1);
// static Int past_eof( USES_REGS1);
static Int put_code(USES_REGS1);
static Int put_byte(USES_REGS1);
static Int skip(USES_REGS1);
static Int flush_output(USES_REGS1);
static Int flush_all_streams(USES_REGS1);

/**
 * CharOfAtom: convert an atom  into a single character.
 *
 * @param at the atom
 *
 * @return the char .
 */
INLINE_ONLY inline EXTERN Int CharOfAtom(Atom at) {
  if (IsWideAtom(at)) {
    return at->WStrOfAE[0];
  } else {
    return at->StrOfAE[0];
  }
}

static int plUnGetc(int sno, int ch) {
  return ungetc(ch, GLOBAL_Stream[sno].file);
}

Int Yap_peek(int sno) {
  CACHE_REGS
  Int ocharcount, olinecount, olinepos;
  StreamDesc *s;
  Int ch;

  s = GLOBAL_Stream + sno;
  ocharcount = s->charcount;
  olinecount = s->linecount;
  olinepos = s->linepos;
  ch = s->stream_wgetc(sno);
  s->charcount = ocharcount;
  s->linecount = olinecount;
  s->linepos = olinepos;
  /* buffer the character */
  if (s->encoding == LOCAL_encoding) {
    ungetwc(ch, s->file);
  } else {
    /* do the ungetc as if a write .. */
    int (*f)(int, int) = s->stream_putc;
    s->stream_putc = plUnGetc;
    put_wchar(sno, ch);
    s->stream_putc = f;
  }
  return ch;
}

static Int dopeek_byte(int sno) {
  Int ocharcount, olinecount, olinepos;
  StreamDesc *s;
  Int ch;

  s = GLOBAL_Stream + sno;
  ocharcount = s->charcount;
  olinecount = s->linecount;
  olinepos = s->linepos;
  ch = GLOBAL_Stream[sno].stream_getc(sno);
  s->charcount = ocharcount;
  s->linecount = olinecount;
  s->linepos = olinepos;
  /* buffer the character */
  ungetc(ch, s->file);
  return ch;
}

static Int at_end_of_stream(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, NULL);
  Int out;

  if (sno < 0)
    return (FALSE);
  out = GLOBAL_Stream[sno].status & Eof_Stream_f;
  if (!out) {
    if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
      out = (dopeek_byte(sno) < 0);
    } else {
      out = (Yap_peek(sno) < 0);
    }
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return out;
}

static Int at_end_of_stream_0(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  Int out;

  int sno = LOCAL_c_input_stream;
  out = GLOBAL_Stream[sno].status & Eof_Stream_f;
  if (!out) {
    out = (Yap_peek(sno) < 0);
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return out;
}

static int yap_fflush(int sno) {
  Yap_ReadlineFlush(sno);
  if ((GLOBAL_Stream[sno].status & Output_Stream_f) &&
      !(GLOBAL_Stream[sno].status &
        (Null_Stream_f | InMemory_Stream_f | Socket_Stream_f | Pipe_Stream_f |
         Free_Stream_f))) {
    return (fflush(GLOBAL_Stream[sno].file));
  } else
    return (0);
}

static Int get(USES_REGS1) { /* '$get'(Stream,-N)                     */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get/2");
  int ch;
  // Int status;

  if (sno < 0)
    return FALSE;
  // status = GLOBAL_Stream[sno].status;
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) <= 32 && ch >= 0)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntegerTerm(ch)));
}

static Int get_char(USES_REGS1) { /* '$get'(Stream,-N)                     */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get/2");
  int ch;
  //  Int status;

  if (sno < 0)
    return FALSE;
  // status = GLOBAL_Stream[sno].status;
  ch = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkCharTerm(ch)));
}

static Int get_code(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get0/2");
  // Int status;
  Int out;

  if (sno < 0)
    return (FALSE);
  // status = GLOBAL_Stream[sno].status;
  out = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntegerTerm(out)));
}

static Int get_1(USES_REGS1) { /* get_code1(Stream,-N)                     */
  int sno = LOCAL_c_input_stream;
  int ch;
  // Int status;

  LOCK(GLOBAL_Stream[sno].streamlock);
  // status = GLOBAL_Stream[sno].status;
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) <= 32 && ch >= 0)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntegerTerm(ch)));
}

static Int getcode_1(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = LOCAL_c_input_stream;
  // Int status;
  Int out;

  // status = GLOBAL_Stream[sno].status;
  LOCK(GLOBAL_Stream[sno].streamlock);
  out = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG1, MkIntegerTerm(out)));
}

static Int getchar_1(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = LOCAL_c_input_stream;
  // Int status;
  Int out;

  LOCK(GLOBAL_Stream[sno].streamlock);
  // status = GLOBAL_Stream[sno].status;
  out = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG1, MkCharTerm(out)));
}

static Int get0_line_codes(USES_REGS1) { /* '$get0'(Stream,-N) */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get0/2");
  // Int status;
  Term out;
  Int ch = '\0';
  int rewind;

  if (sno < 0)
    return (FALSE);
  rewind = FALSE;
  // status = GLOBAL_Stream[sno].status;
  out = read_line(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  if (rewind)
    return Yap_unify(MkPairTerm(MkIntegerTerm(ch), out), ARG2);
  else
    return Yap_unify(out, ARG2);
}

static Int get_byte(USES_REGS1) { /* '$get_byte'(Stream,-N) */
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, "get_byte/2");
  Int status;
  Term out;

  if (sno < 0)
    return (FALSE);
  status = GLOBAL_Stream[sno].status;
  if (!(status & Binary_Stream_f)
      //&& strictISOFlag()
      ) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "get_byte/2");
    return (FALSE);
  }
  out = MkIntTerm(GLOBAL_Stream[sno].stream_getc(sno));
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return Yap_unify_constant(ARG2, out);
}

static Int get_byte_1(USES_REGS1) { /* '$get_byte'(Stream,-N) */
  int sno = LOCAL_c_input_stream;
  Int status;
  Term out;

  LOCK(GLOBAL_Stream[sno].streamlock);
  status = GLOBAL_Stream[sno].status;
  if (!(status & Binary_Stream_f)
      // &&strictISOFlag()
      ) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "get_byte/1");
    return (FALSE);
  }
  out = MkIntTerm(GLOBAL_Stream[sno].stream_getc(sno));
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return Yap_unify_constant(ARG1, out);
}

static Int put_code_1(USES_REGS1) { /* '$put'(,N)                      */
  int sno = LOCAL_c_output_stream, ch;
  Term t2;

  if (IsVarTerm(t2 = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_code/1");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_code/1");
    return FALSE;
  } else if ((ch = IntegerOfTerm(t2)) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_code/1");
    return FALSE;
  }
  LOCK(GLOBAL_Stream[sno].streamlock);
  GLOBAL_Stream[sno].stream_wputc(sno, (int)IntegerOfTerm(Deref(ARG2)));
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int put_code(USES_REGS1) { /* '$put'(Stream,N)                      */
  int ch;
  Term t2;
  int sno;

  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_code/1");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_code/1");
    return FALSE;
  } else if ((ch = IntegerOfTerm(t2)) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_code/1");
    return FALSE;
  }
  sno = Yap_CheckTextStream(ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "put/2");
    return (FALSE);
  }

  GLOBAL_Stream[sno].stream_wputc(sno, ch);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int put_char_1(USES_REGS1) { /* '$put'(,N)                      */
  int sno = LOCAL_c_output_stream;
  Term t2;
  int ch;

  if (IsVarTerm(t2 = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_char/1");
    return FALSE;
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_char/1");
    return FALSE;
  } else if ((ch = CharOfAtom(AtomOfTerm(t2))) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_char/1");
    return FALSE;
  }
  LOCK(GLOBAL_Stream[sno].streamlock);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "put/2");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_wputc(sno, ch);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int put_char(USES_REGS1) { /* '$put'(Stream,N)                      */
  Term t2;
  int ch;
  int sno;

  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_char/1");
    return FALSE;
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_char/1");
    return FALSE;
  } else if ((ch = CharOfAtom(AtomOfTerm(t2))) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_char/1");
    return FALSE;
  }
  sno = Yap_CheckTextStream(ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "put/2");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_wputc(sno, (int)IntegerOfTerm(Deref(ARG2)));
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int tab_1(USES_REGS1) { /* nl                      */
  int sno = LOCAL_c_output_stream;
  Term t2;
  Int tabs, i;
  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_char/1");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_char/1");
    return FALSE;
  } else if ((tabs = IntegerOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "tab/1");
    return FALSE;
  }

  LOCK(GLOBAL_Stream[sno].streamlock);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "nl/0");
    return (FALSE);
  }

  for (i = 0; i < tabs; i++)
    GLOBAL_Stream[sno].stream_wputc(sno, ' ');
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int tab(USES_REGS1) { /* nl(Stream)                      */
  int sno;
  Term t2;
  Int tabs, i;
  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_char/1");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "put_char/1");
    return FALSE;
  } else if ((tabs = IntegerOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "tab/1");
    return FALSE;
  }
  sno = Yap_CheckTextStream(ARG1, Output_Stream_f, "nl/1");
  if (sno < 0)
    return (FALSE);

  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "nl/0");
    return (FALSE);
  }

  for (i = 0; i < tabs; i++)
    GLOBAL_Stream[sno].stream_wputc(sno, ' ');
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int nl_1(USES_REGS1) { /* nl                      */
  int sno = LOCAL_c_output_stream;
  LOCK(GLOBAL_Stream[sno].streamlock);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "nl/0");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_wputc(sno, 10);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int nl(USES_REGS1) { /* nl(Stream)                      */
  int sno = Yap_CheckTextStream(ARG1, Output_Stream_f, "nl/1");
  if (sno < 0)
    return (FALSE);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "put/2");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_wputc(sno, 10);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int put_byte(USES_REGS1) { /* '$put_byte'(Stream,N)                 */
  Term t2;
  Int ch;
  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_code/2");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_BYTE, t2, "put_code/2");
    return FALSE;
  } else if ((ch = IntegerOfTerm(t2)) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_code/1");
    return FALSE;
  }
  int sno = Yap_CheckStream(ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)
      // && strictISOFlag()
      ) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_TEXT_STREAM, ARG1, NULL);
    return false;
  }
  GLOBAL_Stream[sno].stream_putc(sno, ch);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int put_byte_1(USES_REGS1) { /* '$put_byte'(Stream,N)                 */
  Term t2;
  Int ch;
  int sno = LOCAL_c_output_stream;
  if (IsVarTerm(t2 = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "put_code/1");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_BYTE, t2, "put_code/1");
    return FALSE;
  } else if ((ch = IntegerOfTerm(t2)) < -1) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "put_code/1");
    return FALSE;
  }
  LOCK(GLOBAL_Stream[sno].streamlock);
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)
      //&& strictISOFlag()
      ) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_TEXT_STREAM, ARG1, "get0/2");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_putc(sno, ch);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int skip_1(USES_REGS1) { /* '$skip'(Stream,N)                     */
  Int n;
  Term t2;
  int sno;
  int ch;

  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "skip/2");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "skip/2");
    return FALSE;
  } else if ((n = IntegerOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "skip/2");
    return FALSE;
  }
  sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "skip/2");
  if (sno < 0)
    return (FALSE);
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) != n && ch != -1)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

static Int skip(USES_REGS1) { /* '$skip'(Stream,N)                     */
  Int n;
  Term t2;
  int sno;
  int ch;

  if (IsVarTerm(t2 = Deref(ARG2))) {
    Yap_Error(INSTANTIATION_ERROR, t2, "skip/2");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "skip/2");
    return FALSE;
  } else if ((n = IntegerOfTerm(t2)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "skip/2");
    return FALSE;
  }
  sno = LOCAL_c_input_stream;
  LOCK(GLOBAL_Stream[sno].streamlock);
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) != n && ch != -1)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

/**
 * @pred flush_output(+Stream)
 *
 * Flush the stream _Stream_, that is, make sure all pending output is committed
 * before any further execution.
 *
 * @param +_Stream_
 *
 */
static Int flush_output(USES_REGS1) { /* flush_output(Stream)          */
  int sno = Yap_CheckStream(ARG1, Output_Stream_f, "flush_output/1");
  if (sno < 0)
    return (FALSE);
  yap_fflush(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

/**
 * @pred flush_output
 *
 * Flush the current output stream, that is, make sure all pending output is
 *committed
 * before any further execution. By default this is user_output, but it may be
 * changed by current_output/1.
 *
 */
static Int flush_output0(USES_REGS1) { /* flush_output          */
  yap_fflush(LOCAL_c_output_stream);
  return (TRUE);
}

static Int flush_all_streams(USES_REGS1) { /* $flush_all_streams          */
#if BROKEN_FFLUSH_NULL
  int i;
  for (i = 0; i < MaxStreams; ++i) {
    LOCK(GLOBAL_Stream[i].streamlock);
    yap_fflush(i);
    UNLOCK(GLOBAL_Stream[i].streamlock);
  }
#else
  fflush(NULL);
#endif

  return TRUE;
}

/** @pred  peek(+ _S_, - _C_) is deprecated


If  _C_ is unbound, or is the code for a character, and
the  stream _S_ is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

*/

/** @pred  peek_code(+ _S_, - _C_) is iso


If  _C_ is unbound, or is the code for a character, and
the  stream _S_ is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

*/
static Int peek_code(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "peek/2");
  Int ch;

  if (sno < 0)
    return FALSE;
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "peek_code/2");
    return FALSE;
  }
  if ((ch = Yap_peek(sno)) < 0) {
#ifdef PEEK_EOF
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
#endif
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntTerm(ch)));
}

/** @pred  peek_code( - _C_) is iso


If  _C_ is unbound, or is the code for a character, and
the  current input stream is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

*/
static Int peek_code_1(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = LOCAL_c_input_stream;
  Int ch;

  LOCK(GLOBAL_Stream[sno].streamlock);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "peek_code/2");
    return FALSE;
  }
  if ((ch = Yap_peek(sno)) < 0) {
#ifdef PEEK_EOF
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
#endif
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG1, MkIntTerm(ch)));
}

/** @pred  peek_byte(+Stream, - _C_) is iso


If  _C_ is unbound, or is a character code, and _Stream_ is a
binary stream, read the next byte from the current stream and unify its
code with  _C_, while leaving the current stream position unaltered.
*/
static Int peek_byte(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, "peek_byte/2");
  Int ch;

  if (sno < 0)
    return (FALSE);
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_STREAM, ARG1, "peek_byte/2");
    return (FALSE);
  }
  if ((ch = dopeek_byte(sno)) < 0) {
#ifdef PEEK_EOF
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
#endif
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntTerm(ch)));
}

/** @pred  peek_byte( - _C_) is iso


If  _C_ is unbound, or is a character code, and _Stream_ is a
binary stream, read the next byte from the current stream and unify its
code with  _C_, while leaving the current stream position unaltered.
*/
static Int peek_byte_1(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = LOCAL_c_input_stream;
  Int ch;

  if (sno < 0)
    return (FALSE);
  LOCK(GLOBAL_Stream[sno].streamlock);
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "peek_byte/2");
    return (FALSE);
  }
  if ((ch = dopeek_byte(sno)) < 0) {
#ifdef PEEK_EOF
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
#endif
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG2, MkIntTerm(ch)));
}

/** @pred  peek_char(+_S_, - _C_) is iso


If  _C_ is unbound, or is a character code, and the  stream _S_ is a
binary stream, read the next byte from the current stream and unify the
atom with  _C_, while leaving the  stream position unaltered.
*/
static Int peek_char(USES_REGS1) {
  /* the next character is a EOF */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "peek/2");
  wchar_t wsinp[2];
  Int ch;

  if (sno < 0)
    return false;
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "peek_byte/2");
    return (FALSE);
  }
  if ((ch = Yap_peek(sno)) < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  wsinp[1] = '\0';
  wsinp[0] = ch;
  return Yap_unify_constant(ARG2, MkAtomTerm(Yap_LookupMaybeWideAtom(wsinp)));
}

/** @pred  peek_char( - _C_) is iso


If  _C_ is unbound, or is a character code, and the current input stream is a
binary stream, read the next byte from the current stream and unify the
atom with  _C_, while leaving the  stream position unaltered.
*/
static Int peek_char_1(USES_REGS1) {
  /* the next character is a EOF */
  int sno = LOCAL_c_input_stream;
  wchar_t wsinp[2];
  Int ch;

  LOCK(GLOBAL_Stream[sno].streamlock);
  if ((ch = Yap_peek(sno)) < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
    // return false;
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  wsinp[1] = '\0';
  wsinp[0] = ch;
  return Yap_unify_constant(ARG2, MkAtomTerm(Yap_LookupMaybeWideAtom(wsinp)));
}

/** @pred  peek(+ _S_, - _C_) is deprecated


If  _C_ is unbound, or is the code for a character, and
the  stream _S_ is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

Please use the ISO built-in peek_code/2.
*/

/** @pred  peek( - _C_) is iso


If  _C_ is unbound, or is the code for a character, and
the  currrent input stream  is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

*/

void Yap_flush(void) { CACHE_REGS(void)flush_all_streams(PASS_REGS1); }

void Yap_FlushStreams(void) { CACHE_REGS(void)flush_all_streams(PASS_REGS1); }

void Yap_InitCharsio(void) {
  Yap_InitCPred("get", 2, get, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_code", 2, get_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_char", 2, get_char, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get0", 2, get_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get", 1, get_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_code", 1, getcode_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_char", 1, getchar_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get0", 1, getcode_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$get0_line_codes", 2, get0_line_codes,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("get_byte", 2, get_byte, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_byte", 1, get_byte_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put", 1, put_code_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put", 2, put_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_code", 1, put_code_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_code", 2, put_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_char", 1, put_char_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_char", 2, put_char, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_byte", 2, put_byte, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_byte", 1, put_byte_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_char", 2, put_char, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("put_char1", 1, put_char_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab", 2, tab, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab1", 1, tab_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("nl", 0, nl_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("nl", 1, nl, SafePredFlag | SyncPredFlag);

  Yap_InitCPred("$flush_all_streams", 0, flush_all_streams,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("flush_output", 1, flush_output, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("flush_output", 0, flush_output0, SafePredFlag | SyncPredFlag);

  Yap_InitCPred("at_end_of_stream", 1, at_end_of_stream,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("at_end_of_stream_0", 0, at_end_of_stream_0,
                SafePredFlag | SyncPredFlag);
  // Yap_InitCPred ("$past_eof", 1, past_eof, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("peek", 2, peek_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_code", 2, peek_code, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_char", 2, peek_char, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_byte", 2, peek_byte, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek", 1, peek_code_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_code", 1, peek_code_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_char", 1, peek_char_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("peek_byte", 1, peek_byte_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("skip", 2, skip, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("skip1", 1, skip_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab", 2, tab, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab1", 1, tab_1, SafePredFlag | SyncPredFlag);
}
