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
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "iopreds.h"

static Int
rl_to_codes(Term TEnd, int do_as_binary, int arity)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  Int status;
  UInt max_inp, buf_sz, sz;
  char *buf;
  int  binary_stream;

  if (sno < 0)
    return FALSE;
  status = Stream[sno].status;
  binary_stream = Stream[sno].status & Binary_Stream_f;
  if (status & Eof_Stream_f) {
    UNLOCK(Stream[sno].streamlock);
    Yap_Error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM, ARG1, "read_line_to_codes/%d", arity);
    return FALSE;
  }
  max_inp = (ASP-H)/2-1024;
  buf = (char *)TR;
  buf_sz = (char *)Yap_TrailTop-buf;
  while (TRUE) {
    if ( buf_sz > max_inp ) {
      buf_sz = max_inp;
    }
    if (do_as_binary && !binary_stream)
      Stream[sno].status |= Binary_Stream_f;
    sz = Stream[sno].stream_gets(sno, buf_sz, buf);
    if (do_as_binary && !binary_stream)
      Stream[sno].status &= ~Binary_Stream_f;
    if (sz == -1 || sz == 0) {
      UNLOCK(Stream[sno].streamlock);
      if (Stream[sno].status & Eof_Stream_f) {
	return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof));
      }
      return FALSE;
    }
    if (Stream[sno].status & Eof_Stream_f || buf[sz-1] == 10) {
      /* we're done */
      Term end;
      UNLOCK(Stream[sno].streamlock);
      if (!(do_as_binary || Stream[sno].status & Eof_Stream_f)) {
	buf[sz-1] = '\0';
      }
      if (arity == 2)
	end = TermNil;
      else
	end = Deref(XREGS[arity]);
      return Yap_unify(ARG2, Yap_StringToDiffList((char *)TR, end)) ;
    }
    buf += (buf_sz-1);
    max_inp -= (buf_sz-1);
    if (max_inp <= 0) {
      UNLOCK(Stream[sno].streamlock);
      Yap_Error(OUT_OF_STACK_ERROR, ARG1, "read_line_to_codes/%d", arity);
      return FALSE;      
    }
  }
}

static Int
p_rl_to_codes(void)
{
  return rl_to_codes(TermNil, FALSE, 2);
}

static Int
p_rl_to_codes2(void)
{
  return rl_to_codes(TermNil, TRUE, 3);
}

static Int
p_stream_to_codes(void)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  CELL *HBASE = H;

  if (sno < 0)
    return FALSE;
  while (!(Stream[sno].status & Eof_Stream_f)) {
    /* skip errors */
    Int ch = Stream[sno].stream_getc(sno);
    if (ch == EOFCHAR)
      break;
    *H++ = MkIntTerm(ch);
    *H = AbsPair(H+1);
    H++;
    if (H >= ASP) {
      Yap_Error(OUT_OF_STACK_ERROR, ARG1, "read_stream_to_codes/3");
      return FALSE;      
    }
  }
  if (H == HBASE)
    return Yap_unify(ARG2,ARG3);
  H[-1] = Deref(ARG3);
  return Yap_unify(AbsPair(HBASE),ARG2);
}

static Int
p_stream_to_terms(void)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  Term t = Deref(ARG3);

  if (sno < 0)
    return FALSE;
  while (!(Stream[sno].status & Eof_Stream_f)) {
    /* skip errors */
    TokEntry *tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(sno);
    if (!Yap_ErrorMessage)
    {
      Term th = Yap_Parse();
      if (H >= ASP-1024) {
	Yap_Error(OUT_OF_STACK_ERROR, ARG1, "read_stream_to_terms/3");
	return FALSE;      
      }
      if (!th || Yap_ErrorMessage)
	break;
      if (th == MkAtomTerm (AtomEof)) {
	Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	return Yap_unify(t,ARG2);
      } else {
	t = MkPairTerm(th,t);
      } 
    }
    Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
  }
  return Yap_unify(t,ARG2);
}

void
Yap_InitReadUtil(void)
{
  Term cm = CurrentModule;
  CurrentModule = READUTIL_MODULE;
  Yap_InitCPred("read_line_to_codes", 2, p_rl_to_codes, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("read_line_to_codes", 3, p_rl_to_codes2, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("read_stream_to_codes", 3, p_stream_to_codes, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("read_stream_to_terms", 3, p_stream_to_terms, SafePredFlag|SyncPredFlag);
  CurrentModule = cm;
}

