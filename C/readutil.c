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
#include "YapHeap.h"
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
    return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof));
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
      if (Stream[sno].status & Eof_Stream_f) {
	UNLOCK(Stream[sno].streamlock);
	return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof));
      }
      UNLOCK(Stream[sno].streamlock);
      return FALSE;
    }
    if (Stream[sno].status & Eof_Stream_f || buf[sz-1] == 10) {
      /* we're done */
      Term end;
      if (!(do_as_binary || Stream[sno].status & Eof_Stream_f)) {
	UNLOCK(Stream[sno].streamlock);
	/* handle CR before NL */
	if (sz-2 >= 0 && buf[sz-2] == 13)
	  buf[sz-2] = '\0';
	else
	  buf[sz-1] = '\0';
      } else {
	UNLOCK(Stream[sno].streamlock);
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
  CELL *h0 = &ARG4;

  if (sno < 0)
    return FALSE;
  while (!(Stream[sno].status & Eof_Stream_f)) {
    /* skip errors */
    Int ch = Stream[sno].stream_getc(sno);
    Term t;
    if (ch == EOFCHAR)
      break;
    t = MkIntegerTerm(ch);
    h0[0] = AbsPair(H);
    *H = t;
    H+=2;
    h0 = H-1;
    if (H >= ASP-1024) {
      RESET_VARIABLE(h0);
      ARG4 = AbsPair(HBASE);
      ARG5 = (CELL)h0;
      if (!Yap_gcl((ASP-HBASE)*sizeof(CELL), 5, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, ARG1, "read_stream_to_codes/3");
	return FALSE;
      }
      /* build a legal term again */
      h0 = (CELL *)ARG5;
      HBASE = RepPair(ARG4);
    }
  }
  UNLOCK(Stream[sno].streamlock);
  if (H == HBASE)
    return Yap_unify(ARG2,ARG3);
  RESET_VARIABLE(H-1);
  Yap_unify(H[-1],ARG3);
  return Yap_unify(AbsPair(HBASE),ARG2);
    
}

static Int
p_stream_to_terms(void)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  Term t = Deref(ARG3), tpos = TermNil;

  if (sno < 0)
    return FALSE;
  while (!(Stream[sno].status & Eof_Stream_f)) {
    /* skip errors */
    TokEntry *tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(sno, &tpos);
    if (!Yap_ErrorMessage)
    {
      Term th = Yap_Parse();
      if (H >= ASP-1024) {
	UNLOCK(Stream[sno].streamlock);
	Yap_Error(OUT_OF_STACK_ERROR, ARG1, "read_stream_to_terms/3");
	return FALSE;      
      }
      if (!th || Yap_ErrorMessage)
	break;
      if (th == MkAtomTerm (AtomEof)) {
	UNLOCK(Stream[sno].streamlock);
	Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
	return Yap_unify(t,ARG2);
      } else {
	t = MkPairTerm(th,t);
      } 
    }
    Yap_clean_tokenizer(tokstart, Yap_VarTable, Yap_AnonVarTable);
  }
  UNLOCK(Stream[sno].streamlock);
  return Yap_unify(t,ARG2);
}

void
Yap_InitReadUtil(void)
{
  Term cm = CurrentModule;
  CurrentModule = READUTIL_MODULE;
  Yap_InitCPred("read_line_to_codes", 2, p_rl_to_codes, SyncPredFlag);
  Yap_InitCPred("read_line_to_codes", 3, p_rl_to_codes2, SyncPredFlag);
  Yap_InitCPred("read_stream_to_codes", 3, p_stream_to_codes, SyncPredFlag);
  Yap_InitCPred("read_stream_to_terms", 3, p_stream_to_terms, SyncPredFlag);
  CurrentModule = cm;
}

