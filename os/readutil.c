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
* File:		readutil.c						 *
* Last rev:	2/8/06							 *
* mods:									 *
* comments:	readutil library support				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "YapEncoding.h"
#include "iopreds.h"
#include "yapio.h"

/**
* @defgroup readutil Reading Lines and Files
* @addtogroup library
* @{
* @brief Read full lines and a full file in a single call.
*/

static Int rl_to_codes(Term TEnd, int do_as_binary, bool codes USES_REGS) {
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, "read_line_to_codes/2");
  StreamDesc *st = GLOBAL_Stream + sno;
  Int status;
  size_t  buf_sz, sz;
  unsigned char *buf;
  bool binary_stream;
  utf8proc_int32_t ch;

  if (sno < 0)
    return false;
  status = GLOBAL_Stream[sno].status;
  binary_stream = GLOBAL_Stream[sno].status & Binary_Stream_f;
  if (status & Eof_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
  }
          buf = Malloc(4096);
  buf_sz = 4096;
  while (true) {
    if (do_as_binary && !binary_stream) {
      GLOBAL_Stream[sno].status |= Binary_Stream_f;
    }
    if (st->status & Binary_Stream_f) {
      sz = fread(buf, 1, buf_sz, GLOBAL_Stream[sno].file);
    } else {
      unsigned char *pt = buf;
      do {
        ch = st->stream_wgetc_for_read(sno);
        if (ch < 127) {

          if (ch < 0) {
            break;
          }
          *pt++ = ch;
        } else {
	  
            pt += get_utf8(pt, 4, &ch);
            if (pt + 4 == buf + buf_sz)
            break;
         }
      } while (ch != '\n');
      sz = pt - buf;
    }
    if (do_as_binary && !binary_stream)
      GLOBAL_Stream[sno].status &= ~Binary_Stream_f;
    if (sz == -1 || sz == 0) {
      if (GLOBAL_Stream[sno].status & Eof_Stream_f) {
        UNLOCK(GLOBAL_Stream[sno].streamlock);
        return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
      }
      UNLOCK(GLOBAL_Stream[sno].streamlock);
    }
    if (GLOBAL_Stream[sno].status & Eof_Stream_f || buf[sz - 1] == 10) {
      /* we're done */
      if (!(do_as_binary || GLOBAL_Stream[sno].status & Eof_Stream_f)) {
        UNLOCK(GLOBAL_Stream[sno].streamlock);
        /* handle CR before NL */
        if ((Int)sz - 2 >= 0 && buf[sz - 2] == 13)
          buf[sz - 2] = '\0';
        else
          buf[sz - 1] = '\0';
      } else {
        UNLOCK(GLOBAL_Stream[sno].streamlock);
      }
      if (codes)
      return Yap_unify(
          ARG2, Yap_UTF8ToDiffListOfCodes(buf, TEnd PASS_REGS));
      else
      return Yap_unify(
          ARG2, Yap_UTF8ToDiffListOfChars(buf, TEnd PASS_REGS));
     }

   }
}
/**
   read_line_to_codes( +_Stream_, -_Codes_)

   If _Stream_ is a readable text stream, unify _Codes_ with
   the sequence of character codes forming the first line of the stream.
   */
static Int read_line_to_codes2(USES_REGS1) {
  return rl_to_codes(TermNil, FALSE, true PASS_REGS);
}
/**
   read_line_to_chars( +_Stream_, -_Codes_)

   If _Stream_ is a readable text stream, unify _Codes_ with
   the sequence of character atoms forming the first line of the stream.
   */
static Int read_line_to_chars2(USES_REGS1) {
  return rl_to_codes(TermNil, FALSE,false PASS_REGS);
}


/**
   read_line_to_codes( +_Stream_, -_Codes_-_Tail_)

   If _Stream_ is a readable text stream, unify _Codes_ with
   the sequence of character codes available from the first line.

   If the stream is exhausted, unify _Codes_ with `end_of_file`.
   */
static Int read_line_to_codes(USES_REGS1) {
  return rl_to_codes(ARG3, false, true PASS_REGS);
}

/**
   read_line_to_chars( +_Stream_, -_Codes_-_Tail_)

   If _Stream_ is a readable text stream, unify _Codes_ with
   the sequence of characters available from the stream.

   If the stream is exhausted, unify _Codes_ with `end_of_file`.
   */
static Int read_line_to_chars(USES_REGS1) {
  return rl_to_codes(ARG3, false, false PASS_REGS);
}

/**
   @pred read_line_to_string( +_Stream_, -_String_)

   If _Stream_ is a readable text stream, unify _String_ with
   the Prolog string storing the codes forming the first line of the stream.

     If the stream is exhausted, unify _Codes_ with `end_of_file`.
 */
static Int read_line_to_string(USES_REGS1) {
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, "read_line_to_codes/2");
  Int status;
  UInt max_inp, buf_sz;
  unsigned char *buf;
  size_t sz;
  StreamDesc *st = GLOBAL_Stream + sno;
  utf8proc_int32_t ch;

  if (sno < 0)
    return false;
  status = GLOBAL_Stream[sno].status;
  if (status & Eof_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
  }
  max_inp = (ASP - HR) / 2 - 1024;
  buf = (unsigned char *)TR;
  buf_sz = (unsigned char *)LOCAL_TrailTop - buf;
 
    if (buf_sz > max_inp) {
      buf_sz = max_inp;
    }
    if (st->status & Binary_Stream_f) {
      char *b = (char *)TR;
      sz = fread(b, 1, buf_sz, GLOBAL_Stream[sno].file);
    } else {
      unsigned char *pt = buf;
      do {
         ch = st->stream_wgetc_for_read(sno);
        if (ch < 127) {
          if (ch < 0) {
            break;
          }
          *pt++ = ch;
        } else {
          pt += get_utf8(pt, 4, &ch);
          if (pt + 4 == buf + buf_sz)
            break;
        }
      } while (ch != '\n');
      sz = pt - buf;
    }
  if (sz == -1 || sz == 0) {
    if (GLOBAL_Stream[sno].status & Eof_Stream_f) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      return Yap_unify_constant(ARG2, MkAtomTerm(AtomEof));
    }
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return false;
  }
  if (GLOBAL_Stream[sno].status & Eof_Stream_f || buf[sz - 1] == 10) {
    /* we're done */

    if (!(GLOBAL_Stream[sno].status & Eof_Stream_f)) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      /* handle CR before NL */
      if ((Int)sz - 2 >= 0 && buf[sz - 2] == 13)
        buf[sz - 2] = '\0';
      else {
        buf[sz - 1] = '\0';
      }
    } else {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
    }
  }
  if (GLOBAL_Stream[sno].encoding == ENC_ISO_UTF8) {
    return Yap_unify(ARG2, Yap_UTF8ToString((const char *)TR PASS_REGS));
  } else if (GLOBAL_Stream[sno].encoding == ENC_WCHAR) {
    return Yap_unify(ARG2, Yap_WCharsToString((const wchar_t *)TR PASS_REGS));
  } else {
    return Yap_unify(
        ARG2, Yap_CharsToString((const char *)TR, ENC_ISO_LATIN1 PASS_REGS));
  }
  buf += (buf_sz - 1);
  max_inp -= (buf_sz - 1);
  if (max_inp <= 0) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    Yap_Error(RESOURCE_ERROR_STACK, ARG1, NULL);
    return FALSE;
  }
}

/**
   @pred read_stream_to_codes( +_Stream_, -Codes, ?_Tail_)

   If _Stream_ is a readable text stream, unify _String_ with
   the difference list  storing the codes forming the first line of the stream.

     If the stream is exhausted, unify _Codes_ with `end_of_file`.
 */
static Int read_stream_to_codes(USES_REGS1) {
  int sno = Yap_CheckStream(ARG1, Input_Stream_f,
                            "reaMkAtomTerm (AtomEofd_line_to_codes/2");
  CELL *HBASE = HR;
  CELL *h0 = &ARG4;

  if (sno < 0)
    return FALSE;
  while (!(GLOBAL_Stream[sno].status & Eof_Stream_f)) {
    /* skip errors */
    Int ch = GLOBAL_Stream[sno].stream_getc(sno);
    Term t;
    if (ch == EOFCHAR)
      break;
    t = MkIntegerTerm(ch);
    h0[0] = AbsPair(HR);
    *HR = t;
    HR += 2;
    h0 = HR - 1;
    yhandle_t news, news1, st = Yap_StartSlots();
    if (HR >= ASP - 1024) {
      RESET_VARIABLE(h0);
      news = Yap_InitSlot(AbsPair(HBASE));
      news1 = Yap_InitSlot((CELL)(h0));
      if (!Yap_dogc(PASS_REGS1)) {
        Yap_Error(RESOURCE_ERROR_STACK, ARG1, "read_stream_to_codes/3");
        return false;
      }
      /* build a legal term again */
      h0 = (CELL *)(Yap_GetFromSlot(news1));
      HBASE = RepPair(Yap_GetFromSlot(news));
    }
    Yap_CloseSlots(st);
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  if (HR == HBASE)
    return Yap_unify(ARG2, ARG3);
  RESET_VARIABLE(HR - 1);
  Yap_unify(HR[-1], ARG3);
  return Yap_unify(AbsPair(HBASE), ARG2);
}


/**
   @pred read_stream_to_terms( +_Stream_, -Terms, ?_Tail_)

   If _Stream_ is a readable text stream, unify _String_ with
   the difference list  storing the Prolog terms in the stream.

     If the stream is exhausted, unify _Codes_ with `end_of_file`.
 */
static Int read_stream_to_terms(USES_REGS1) {
  int sno = Yap_CheckStream(ARG1, Input_Stream_f, "read_line_to_codes/2");
  Term t, r;
  yhandle_t hdl, hd3;

  if (sno < 0)
    return FALSE;

  hd3 =  Yap_InitSlot((ARG3));
 Term td = TermDec10;
 Term opts =  MkPairTerm(Yap_MkApplTerm(FunctorSyntaxErrors,1,&td),TermNil);
  hdl = Yap_InitSlot(ARG2);
  while (!(GLOBAL_Stream[sno].status & Eof_Stream_f)) {
    r = Yap_read_term(sno, opts, 2);
      ;
      Yap_DebugPlWriteln(r);
    // just ignore failure
      t = Deref(Yap_GetFromHandle(hdl));
    if (Deref(r) == TermEOfCode) {
      break;
    } else {
      
      if (IsVarTerm(t)) {
	Yap_unify(t, Yap_MkNewPairTerm());
	t = Deref(t);
      } else if (!IsPairTerm(t))
	return false;
      Term h = HeadOfTerm(t);
	if (!Yap_unify(h,r))
	    return false;
          Yap_DebugPlWriteln(t);
  Yap_PutInHandle(hdl,TailOfTerm(t));
    }
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return Yap_unify( Yap_GetFromHandle(hdl), Yap_GetFromHandle(hd3));
}

void Yap_InitReadUtil(void) {
  CACHE_REGS

  Term cm = CurrentModule;
  CurrentModule = READUTIL_MODULE;
  Yap_InitCPred("read_line_to_string", 2, read_line_to_string, SyncPredFlag);
  Yap_InitCPred("read_line_to_codes", 3, read_line_to_codes, SyncPredFlag);
  Yap_InitCPred("read_line_to_codes", 2, read_line_to_codes2, SyncPredFlag);
  Yap_InitCPred("read_line_to_chars", 3, read_line_to_chars, SyncPredFlag);
  Yap_InitCPred("read_line_to_chars", 2, read_line_to_chars2, SyncPredFlag);
  Yap_InitCPred("read_stream_to_codes", 3, read_stream_to_codes, SyncPredFlag);
  Yap_InitCPred("read_stream_to_terms", 3, read_stream_to_terms, SyncPredFlag);
  CurrentModule = cm;
}

/// @}
