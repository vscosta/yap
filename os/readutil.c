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
#include "YapText.h"
#include "encoding.h"


static Int
rl_to_codes(Term TEnd, int do_as_binary, int arity USES_REGS)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  Int status;
  UInt max_inp, buf_sz, sz;
  char *buf;
  int  binary_stream;

  if (sno < 0)
    return FALSE;
  status = GLOBAL_Stream[sno].status;
  binary_stream = GLOBAL_Stream[sno].status & Binary_Stream_f;
  if (status & Eof_Stream_f) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof));
  }
  max_inp = (ASP-HR)/2-1024;
  buf = (char *)TR;
  buf_sz = (char *)LOCAL_TrailTop-buf;
  while (TRUE) {
    if ( buf_sz > max_inp ) {
      buf_sz = max_inp;
    }
    if (do_as_binary && !binary_stream)
      GLOBAL_Stream[sno].status |= Binary_Stream_f;
    sz = GLOBAL_Stream[sno].stream_gets(sno, buf_sz, buf);
    if (do_as_binary && !binary_stream)
      GLOBAL_Stream[sno].status &= ~Binary_Stream_f;
    if (sz == -1 || sz == 0) {
      if (GLOBAL_Stream[sno].status & Eof_Stream_f) {
	UNLOCK(GLOBAL_Stream[sno].streamlock);
	return Yap_unify_constant(ARG2, MkAtomTerm (AtomEof));
      }
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      return FALSE;
    }
    if (GLOBAL_Stream[sno].status & Eof_Stream_f || buf[sz-1] == 10) {
      /* we're done */
      Term end;
      if (!(do_as_binary || GLOBAL_Stream[sno].status & Eof_Stream_f)) {
	UNLOCK(GLOBAL_Stream[sno].streamlock);
	/* handle CR before NL */
	if ((Int)sz-2 >= 0 && buf[sz-2] == 13)
	  buf[sz-2] = '\0';
	else
	  buf[sz-1] = '\0';
      } else {
	UNLOCK(GLOBAL_Stream[sno].streamlock);
      }
      if (arity == 2)
	end = TermNil;
      else
	end = Deref(XREGS[arity]);
      if (GLOBAL_Stream[sno].encoding == ENC_ISO_UTF8)
	return Yap_unify(ARG2, Yap_UTF8ToDiffListOfCodes((const char *)TR, end PASS_REGS)) ;
      else if (GLOBAL_Stream[sno].encoding == ENC_WCHAR)
	return Yap_unify(ARG2, Yap_WCharsToDiffListOfCodes((const wchar_t *)TR, end PASS_REGS)) ;
      return Yap_unify(ARG2, Yap_CharsToDiffListOfCodes((const char *)TR, end, ENC_ISO_LATIN1 PASS_REGS)) ;
    }
    buf += (buf_sz-1);
    max_inp -= (buf_sz-1);
    if (max_inp <= 0) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      Yap_Error(RESOURCE_ERROR_STACK, ARG1, "read_line_to_codes/%d", arity);
      return FALSE;      
    }
  }
}

static Int
read_line_to_codes(USES_REGS1)
{
  return rl_to_codes(TermNil, FALSE, 2 PASS_REGS);
}

static Int
read_line_to_codes2(USES_REGS1)
{
  return rl_to_codes(TermNil, TRUE, 3 PASS_REGS);
}

static Int
read_stream_to_codes(USES_REGS1)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "reaMkAtomTerm (AtomEofd_line_to_codes/2");
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
    HR+=2;
    h0 = HR-1;
    if (HR >= ASP-1024) {
      RESET_VARIABLE(h0);
      ARG4 = AbsPair(HBASE);
      ARG5 = (CELL)h0;
      if (!Yap_gcl((ASP-HBASE)*sizeof(CELL), 5, ENV, Yap_gcP())) {
	Yap_Error(RESOURCE_ERROR_STACK, ARG1, "read_stream_to_codes/3");
	return FALSE;
      }
      /* build a legal term again */
      h0 = (CELL *)ARG5;
      HBASE = RepPair(ARG4);
    }
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  if (HR == HBASE)
    return Yap_unify(ARG2,ARG3);
  RESET_VARIABLE(HR-1);
  Yap_unify(HR[-1],ARG3);
  return Yap_unify(AbsPair(HBASE),ARG2);
    
}

static Int
read_stream_to_terms(USES_REGS1)
{
  int sno = Yap_CheckStream (ARG1, Input_Stream_f, "read_line_to_codes/2");
  Term t, hd;
  yhandle_t tails, news;

  if (sno < 0)
    return FALSE;

  t = AbsPair(HR);
  RESET_VARIABLE(HR);
  Yap_InitSlot( (CELL)(HR) );
  tails = Yap_InitSlot( (CELL)(HR) );
  news = Yap_InitSlot( (CELL)(HR) );
  HR++;
    
  while (!(GLOBAL_Stream[sno].status & Eof_Stream_f)) {
    RESET_VARIABLE(HR);
    RESET_VARIABLE(HR+1);
    hd = (CELL)HR;
    Yap_PutInSlot(news, (CELL)(HR+1) PASS_REGS);
    HR += 2;
    while ((hd=Yap_read_term(sno, TermNil, 2)) == 0L)
      ;
    // just ignore failure
    CELL *pt = VarOfTerm(Yap_GetFromSlot(tails));
    if (Deref(hd) == TermEOfCode) {
      *pt = Deref(ARG3);
      break;
    } else {
      CELL *newpt = (CELL*)Yap_GetFromSlot(news);
      *pt =AbsPair(newpt-1);
     Yap_PutInSlot(tails, (CELL)newpt PASS_REGS);
    }
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return Yap_unify(t,ARG2);
}

void
Yap_InitReadUtil(void)
{
    CACHE_REGS

  Term cm = CurrentModule;
  CurrentModule = READUTIL_MODULE;
  Yap_InitCPred("read_line_to_codes", 2, read_line_to_codes, SyncPredFlag);
  Yap_InitCPred("read_line_to_codes", 3, read_line_to_codes2, SyncPredFlag);
  Yap_InitCPred("read_stream_to_codes", 3, read_stream_to_codes, SyncPredFlag);
  Yap_InitCPred("read_stream_to_terms", 3, read_stream_to_terms, SyncPredFlag);
  CurrentModule = cm;
}

