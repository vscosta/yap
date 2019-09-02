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
 * File:		charcodes.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Character codes and character conversion		 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file   charsio.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 11:37:16 2015
 * @brief This file includes the definition of character-by-character related
 *IO.
 *
 *
 */

/**
 * @defgroup CharIO Character-Based Input/Output
 * @ingroup  InputOutput
 *
 * @{
 * YAP implements most of the ISO-Prolog built-ins. Input/Output may be
 *performed on
 * the current stream or on a specified stream, and it may involve a:
 * + byte
 * + character code
 * + character atom
 *
 * Old-style operations, like get0/2 are still implemented.
 *
 *
 */

#include "Yap.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
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

/**
 * CharOfAtom: convert an atom  into a single character.
 *
 * @param at the atom
 *
 * @return the char .
 */
INLINE_ONLY Int CharOfAtom(Atom at) {
  int32_t val;

  get_utf8(at->UStrOfAE, 1, &val);
  return val;
}


static int oops_c_from_w(int sno)
{
//    StreamDesc *s = GLOBAL_Stream + sno;
    fprintf(stderr, "oops_c_from_w\n" );
    return 0;

}

 int Yap_popWide(int sno)
{
    StreamDesc *s = GLOBAL_Stream + sno;
    s->buf.on = false;
 Yap_DefaultStreamOps(s);
 return s->buf.ch;

}

int Yap_peekWide(int sno) {
  StreamDesc *s = GLOBAL_Stream + sno;
  int ch;
      Int pos = s->charcount;
      Int line = s->linecount;
      Int lpos = s->linepos;

  if (s->file&&fileno(s->file)>=0) {
      ch = fgetwc(s->file);
      if (ch == WEOF) {
          clearerr(s->file);
          s->status &= ~Eof_Error_Stream_f;
      } else {
          // do not try doing error processing
          ungetwc(ch, s->file);
      }
  }else {
      ch = s->stream_wgetc(sno);
     if (ch == EOF) {
          s->status &= ~Eof_Error_Stream_f;
      } else if (s->status & Seekable_Stream_f) {
        Yap_SetCurInpPos(sno, pos);
      } else {
        s->buf.on = true;
        s->buf.ch = ch;
         s->stream_wgetc = Yap_popWide;
         s->stream_getc = oops_c_from_w;
      }
    }
      s->charcount = pos;
    s->linecount = line;
    s->linepos = lpos;
  return ch;
}


static int oops_w_from_c(int sno)
{
//    StreamDesc *s = GLOBAL_Stream + sno;
    fprintf(stderr, "oops_w_from_c\n" );
    return 0;

}


int Yap_popChar(int sno)
{
    StreamDesc *s = GLOBAL_Stream + sno;
    s->buf.on = false;
Yap_DefaultStreamOps(s);
return s->buf.ch;

}

int Yap_peekChar(int sno) {
    StreamDesc *s = GLOBAL_Stream + sno;
    int ch;
    if (s->file) {
        ch = fgetc(s->file);
        if (ch == EOF) {
            clearerr(s->file);
            s->status &= ~Eof_Error_Stream_f;
        } else {
            // do not try doing error processing
            ungetc(ch, s->file);
        }
    }else {
        Int pos = s->charcount;
        Int line = s->linecount;
        Int lpos = s->linepos;

	ch = s->stream_wgetc(sno);
        s->charcount = pos;
        s->linecount = line;
        s->linepos = lpos;
        if (ch == EOF) {
            s->status &= ~Eof_Error_Stream_f;
            return ch;
        } else {
            s->buf.on = true;
            s->buf.ch = ch;
            s->stream_wgetc = oops_w_from_c;
            s->stream_getc =  Yap_popChar;
        }
        //  Yap_SetCurInpPos(sno, pos);
    }
    return ch;
}

int Yap_peek(int sno) { return GLOBAL_Stream[sno].stream_wpeek(sno); }

static int dopeek_byte(int sno) { return GLOBAL_Stream[sno].stream_peek(sno); }

bool store_code(int ch, Term t USES_REGS) {
  Term t2 = Deref(t);
  bool rc = Yap_unify_constant(t2, MkIntegerTerm(ch));
  if (!rc && !IsVarTerm(t2)) {
    if (!IsIntegerTerm(t2)) {
      Yap_Error(TYPE_ERROR_INTEGER, t, "in output argument");
    } else if (IntegerOfTerm(t2) < 0) {
      Yap_Error(REPRESENTATION_ERROR_IN_CHARACTER_CODE, t,
                "in output argument");
    }
  }
  return rc;
}

/** @pred  at_end_of_stream(+ _S_) is iso

Succeed if the stream  _S_ has stream position end-of-stream or
past-end-of-stream. Note that  _S_ must be a readable stream.


*/
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

/** @pred  at_end_of_stream is iso


Succeed if the current stream has stream position end-of-stream or
past-end-of-stream.


*/
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
#if USE_READLINE
  Yap_ReadlineFlush(sno);
#endif
  if ((GLOBAL_Stream[sno].status & Output_Stream_f) &&
      !(GLOBAL_Stream[sno].status &
        (Null_Stream_f | InMemory_Stream_f | Socket_Stream_f | Pipe_Stream_f |
         Free_Stream_f))) {
    return (fflush(GLOBAL_Stream[sno].file));
  } else
    return (0);
}

/** @pred  get(_S_, - _C_)


The next non-blank character from the  stream _S_ is unified
with  _C_. Blank characters are the ones whose ASCII codes are not
greater than 32. If there are no more non-blank characters in the
stream,  _C_ is unified with -1. If `end_of_stream` has already
been reached in the previous reading, this call will give an error message.
*/
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
  return store_code(ch, ARG2 PASS_REGS);
}

/** @pred  get_char(+ _S_,- _C_) is iso

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_.
*/
static Int get_char(USES_REGS1) { /* '$get'(Stream,-N)                     */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get/2");
  int ch;
  //  Int status;

  if (sno < 0)
    return false;
  // status = GLOBAL_Stream[sno].status;
  ch = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  Term t2 = Deref(ARG2);
  bool rc = Yap_unify_constant(t2, MkCharTerm(ch));
  if (!rc) {
    if (!IsAtomTerm(t2)) {
      Yap_Error(TYPE_ERROR_IN_CHARACTER, ARG2, "in input argument");
    }
  }
  return rc;
}

/** @pred  get_code(+ _S_,- _C_) is iso

If  _C_ is unbound, or is a character code, and the stream  _S_ is a
text stream, read the next character from that stream and unify its
code with  _C_.


*/
static Int get_code(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "get0/2");
  // Int status;
  Int out;

  if (sno < 0)
    return (FALSE);
  // status = GLOBAL_Stream[sno].status;
  out = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return store_code(out, ARG2 PASS_REGS);
}

/** @pred  get(- _C_)


The next non-blank character from the current input stream is unified
with  _C_. Blank characters are the ones whose ASCII codes are not
greater than 32. If there are no more non-blank characters in the
stream,  _C_ is unified with -1. If `end_of_stream` has already
been reached in the previous reading, this call will give an error message.


*/
static Int get_1(USES_REGS1) { /* get_code1(Stream,-N)                     */
  int sno = LOCAL_c_input_stream;
  int ch;
  // Int status;

  LOCK(GLOBAL_Stream[sno].streamlock);
  // status = GLOBAL_Stream[sno].status;
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    PlIOError(PERMISSION_ERROR_INPUT_BINARY_STREAM, TermUserIn,
              "while getting code");
    return false;
  }
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) <= 32 && ch >= 0)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return store_code(ch, ARG1 PASS_REGS);
}

/** @pred  get_code(- _C_) is iso


If  _C_ is unbound, or is the code for a character, and
the current stream is a text stream, read the next character from the
current stream and unify its code with  _C_.


*/
static Int getcode_1(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = LOCAL_c_input_stream;
  // Int status;
  Int out;

  // status = GLOBAL_Stream[sno].status;
  LOCK(GLOBAL_Stream[sno].streamlock);
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    PlIOError(PERMISSION_ERROR_INPUT_BINARY_STREAM, TermUserIn,
              "while getting code");
    return false;
  }
  out = GLOBAL_Stream[sno].stream_wgetc(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return store_code(out, ARG1 PASS_REGS);
}

/** @pred  get_char(- _C_) is iso


If  _C_ is unbound, or is an atom representation of a character, and
the current stream is a text stream, read the next character from the
current stream and unify its atom representation with  _C_.


*/
static Int getchar_1(USES_REGS1) { /* get0(Stream,-N)                    */
  int sno = LOCAL_c_input_stream;
  // Int status;
  Int ch;

  LOCK(GLOBAL_Stream[sno].streamlock);
  // status = GLOBAL_Stream[sno].status;
  ch = GLOBAL_Stream[sno].stream_wgetc(sno);
  if ((GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    PlIOError(PERMISSION_ERROR_INPUT_BINARY_STREAM, TermUserIn,
              "while getting code");
    return false;
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  bool rc = Yap_unify_constant(ARG1, MkCharTerm(ch));
  if (!rc) {
    Term t2 = Deref(ARG1);
    if (!IsAtomTerm(t2)) {
      Yap_Error(TYPE_ERROR_IN_CHARACTER, ARG1, "in input argument");
    }
  }
  return rc;
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

/** @pred  get_byte(+ _S_,- _C_) is iso

If  _C_ is unbound, or is a byte, and the stream  _S_ is a
binary stream, read the next byte from that stream and unify its
code with  _C_. A byte is represented as either a number between 1 and 255, or as -1 for EOF.


*/
static Int get_byte(USES_REGS) { /* '$get_byte'(Stream,-N) */
  Term out = Deref(ARG2);

  if (!IsVarTerm(out)) {
    if (!IsIntegerTerm(out)) Yap_ThrowError(TYPE_ERROR_IN_BYTE, ARG1, " bad type");
    Int ch = IntegerOfTerm(out);
    if (ch < -1 || ch > 255) Yap_ThrowError(TYPE_ERROR_IN_BYTE, ARG1, " bad type");
  }

  int sno = Yap_CheckBinaryStream(ARG1, Input_Stream_f, "get_byte/2");
  if (sno < 0)
    return (FALSE);
  out = MkIntTerm(GLOBAL_Stream[sno].stream_getc(sno));
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return Yap_unify_constant(ARG2, out);
}

/** @pred  get_byte(- _C_) is iso


If  _C_ is unbound, or is a character code, and the current stream is a
binary stream, read the next byte from the current stream and unify its
code with  _C_.


*/
static Int get_byte_1(USES_REGS1) { /* '$get_byte'(Stream,-N) */
  int sno = LOCAL_c_input_stream;
  Int status;
  Term out = Deref(ARG1);

  if (!IsVarTerm(out)) {
    if (!IsIntegerTerm(out)) Yap_ThrowError(TYPE_ERROR_IN_BYTE, ARG2, " bad type");
    Int ch = IntegerOfTerm(out);
    if (ch < -1 || ch > 255) Yap_ThrowError(TYPE_ERROR_IN_BYTE, ARG2, " bad type");
  }
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

/** @pred  put_code(+ _N_) is iso

Outputs to the current output stream the character whose ASCII code is
 _N_. The current output stream must be a text stream. The character
 _N_ must be a legal ASCII character code, an expression yielding such
a code, or a list in which case only the first element is used.


*/
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
  GLOBAL_Stream[sno].stream_wputc(sno, ch);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

/** @pred  put_code(+ _S_,+ _N_) is iso

As `put_code(N)`, but to text stream  _S_.


*/
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

/** @pred  put_char(+ _N_) is iso


Outputs to the current output stream the character who is used to build
the representation of atom `A`. The current output stream must be a
text stream.


*/
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

/** @pred  put_char(+ _S_,+ _A_) is iso

As `put_char(A)`, but to text stream  _S_.


*/
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

/** @pred  tab(+ _N_)

Outputs  _N_ spaces to the current output stream.
*/
static Int tab_1(USES_REGS1) { /* nl                      */
  int sno = LOCAL_c_output_stream;
  Term t1;
  Int tabs, i;
  if (IsVarTerm(t1 = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t1, "first argument");
    return FALSE;
  } else if (!IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "first argument");
    return FALSE;
  } else if ((tabs = IntegerOfTerm(t1)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t1, "first argument");
    return FALSE;
  }

  LOCK(GLOBAL_Stream[sno].streamlock);
  if (GLOBAL_Stream[sno].status & Binary_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "user_output");
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

/** @pred  tab(+ _S_,+ _N_)

The same as tab/1, but using stream  _S_.


*/
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

/** @pred  nl(+ _S_) is iso

Outputs a new line to stream  _S_.
 */
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

/** @pred  nl is iso


Outputs a new line to the current output stream.




 */
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

/** @pred  put_byte(+ _S_,+ _N_) is iso

As `put_byte(N)`, but to binary stream  _S_.


*/
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
  int sno = Yap_CheckBinaryStream(ARG1, Output_Stream_f, "put/2");
  if (sno < 0)
    return (FALSE);
  GLOBAL_Stream[sno].stream_putc(sno, ch);
  /*
   * if (!(GLOBAL_Stream[sno].status & Null_Stream_f))
   * yap_fflush(GLOBAL_Stream[sno].file);
   */
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

/** @pred  put_byte(+ _N_) is iso


Outputs to the current output stream the character whose code is
 _N_. The current output stream must be a binary stream.


*/
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
    Yap_Error(PERMISSION_ERROR_OUTPUT_BINARY_STREAM, ARG1, "get0/2");
    return (FALSE);
  }
  GLOBAL_Stream[sno].stream_putc(sno, ch);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (TRUE);
}

/** @pred  skip(+ _N_)


Skips input characters until the next occurrence of the character with
ASCII code  _N_. The argument to this predicate can take the same forms
as those for `put` (see 6.11).


*/
static Int skip_1(USES_REGS1) { /* 'skip'(N)                     */
  Int n;
  Term t1;
  int sno = LOCAL_c_output_stream;
  int ch;

  if (IsVarTerm(t1 = Deref(ARG1))) {
    Yap_Error(INSTANTIATION_ERROR, t1, "skip/1");
    return FALSE;
  } else if (!IsIntegerTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "skip/1");
    return FALSE;
  } else if ((n = IntegerOfTerm(t1)) < 0) {
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t1, "skip/1");
    return false;
  }
  while ((ch = GLOBAL_Stream[sno].stream_wgetc(sno)) != n && ch != -1)
    ;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return true;
}

/** @pred  skip(+ _S_,- _C_)

Like skip/1, but using stream  _S_ instead of the current
input stream.


*/
static Int skip(USES_REGS1) { /* '$skip'(Stream,N)                     */
  Int n;
  Term t2;
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "skip/2");
  int ch;

  if (sno < 0)
    return (FALSE);
  if (IsVarTerm(t2 = Deref(ARG2))) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(INSTANTIATION_ERROR, t2, "skip/2");
    return FALSE;
  } else if (!IsIntegerTerm(t2)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(TYPE_ERROR_INTEGER, t2, "skip/2");
    return FALSE;
  } else if ((n = IntegerOfTerm(t2)) < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, t2, "skip/2");
    return FALSE;
  }
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
  if (sno < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return (FALSE);
  }
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

/** @pred  peek_code(+ _S_, - _C_) is iso

If  _C_ is unbound, or is the code for a character, and
the  stream _S_ is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

*/
static Int peek_code(USES_REGS1) { /* at_end_of_stream */
  /* the next character is a EOF */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "peek_code/2");
  Int ch;

  if (sno < 0)
    return FALSE;
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
    Yap_Error(PERMISSION_ERROR_INPUT_TEXT_STREAM, ARG1, "peek_code/2");
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
  int sno = Yap_CheckBinaryStream(ARG1, Input_Stream_f, "peek_byte/2");
  Int ch;

  if (sno < 0)
    return false;
  if (!(GLOBAL_Stream[sno].status & Binary_Stream_f)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "peek_byte/2");
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
    Yap_Error(PERMISSION_ERROR_INPUT_BINARY_STREAM, ARG1, "peek_byte/2");
    return (FALSE);
  }
  if ((ch = dopeek_byte(sno)) < 0) {
#ifdef PEEK_EOF
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
#endif
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return (Yap_unify_constant(ARG1, MkIntTerm(ch)));
}

/** @pred  peek_char(+_S_, - _C_) is iso


If  _C_ is unbound, or is a character code, and the  stream _S_ is a
binary stream, read the next byte from the current stream and unify the
atom with  _C_, while leaving the  stream position unaltered.
*/
static Int peek_char(USES_REGS1) {
  /* the next character is a EOF */
  int sno = Yap_CheckTextStream(ARG1, Input_Stream_f, "peek/2");
  unsigned char sinp[16];
  Int ch;

  if (sno < 0)
    return false;
  ch = Yap_peek(sno);
  if (ch < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  int off = put_utf8(sinp, ch);
  if (off < 0) {
    return false;
  }
  sinp[off] = '\0';
  return Yap_unify_constant(ARG2, MkAtomTerm(Yap_ULookupAtom(sinp)));
}

/** @pred  peek_char( - _C_) is iso


If  _C_ is unbound, or is a character code, and the current input stream is a
binary stream, read the next byte from the current stream and unify the
atom with  _C_, while leaving the  stream position unaltered.
*/
static Int peek_char_1(USES_REGS1) {
  /* the next character is a EOF */
  int sno = LOCAL_c_input_stream;
  unsigned char sinp[10];
  Int ch;

  LOCK(GLOBAL_Stream[sno].streamlock);
  if ((ch = Yap_peek(sno)) < 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
    // return false;
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  int off = put_utf8(sinp, ch);
  sinp[off] = '\0';
  return Yap_unify_constant(ARG2, MkAtomTerm(Yap_ULookupAtom(sinp)));
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

void Yap_flush_all(void) { CACHE_REGS(void) flush_all_streams(PASS_REGS1); }

void Yap_FlushStreams(void) { CACHE_REGS(void) flush_all_streams(PASS_REGS1); }

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
  Yap_InitCPred("tab", 1, tab_1, SafePredFlag | SyncPredFlag);
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
  Yap_InitCPred("skip", 1, skip_1, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab", 2, tab, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tab", 1, tab_1, SafePredFlag | SyncPredFlag);
}

/// @}
