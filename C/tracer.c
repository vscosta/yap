/*************************************************************************
*									 *
*	 YAP Prolog    @(#)amidefs.h	1.3 3/15/90
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		tracer.h						 *
* Last rev:								 *
* mods:									 *
* comments:	definitions for low level tracer			 *
*									 *
*************************************************************************/

#include "Yap.h"

#ifdef LOW_LEVEL_TRACER

#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "tracer.h"

STATIC_PROTO(int  TracePutchar, (int, int));
STATIC_PROTO(void  send_tracer_message, (char *, char *, Int, char *, CELL *));

static int do_trace_primitives = TRUE;

static int
TracePutchar(int sno, int ch)
{
  return(putc(ch, _YAP_stderr)); /* use standard error stream, which is supposed to be 2*/
}

static void
send_tracer_message(char *start, char *name, Int arity, char *mname, CELL *args)
{
  if (name == NULL) {
#ifdef  YAPOR
    fprintf(_YAP_stderr, "(%d)%s", worker_id, start);
#else
    fprintf(_YAP_stderr, "%s", start);
#endif
  } else {
    int i;

    if (arity) {
      fprintf(_YAP_stderr, "%s %s:%s(", start, mname, name);
    } else {
      fprintf(_YAP_stderr, "%s %s:%s", start, mname, name);
    }
    for (i= 0; i < arity; i++) {
      if (i > 0) fprintf(_YAP_stderr, ",");
#if DEBUG
#if COROUTINING
      _YAP_Portray_delays = TRUE;
#endif
#endif
      _YAP_plwrite(args[i], TracePutchar, Handle_vars_f);
#if DEBUG
#if COROUTINING
      _YAP_Portray_delays = FALSE;
#endif
#endif
    }
    if (arity) fprintf(_YAP_stderr, ")");
  }
  fprintf(_YAP_stderr, "\n");
}

#if defined(__GNUC__)
unsigned long long vsc_count;
#else
unsigned long vsc_count;
#endif

/*
static int
check_trail_consistency(void) {
  tr_fr_ptr ptr = TR;
  while (ptr > (CELL *)_YAP_TrailBase) {
    ptr = --ptr;
    if (!IsVarTerm(TrailTerm(ptr))) {
      if (IsApplTerm(TrailTerm(ptr))) {
	CELL *cptr = (CELL *)ptr;
	ptr = (tr_fr_ptr)(cptr-1);
      } else {
	if (IsPairTerm(TrailTerm(ptr))) {
	  CELL *p = RepPair(TrailTerm(ptr));
	  if (p < H0) continue;
	}
	printf("Oops at call %ld, B->cp(%p) TR(%p)  pt(%p)\n", vsc_count,B->cp_tr, TR, ptr);
	return(FALSE);
      }
    }
  }
  return(TRUE);
}
*/


void
low_level_trace(yap_low_level_port port, PredEntry *pred, CELL *args)
{
  char *s;
  char *mname;
  Int arity;
  /*  extern int gc_calls; */

  vsc_count++;
  /* return;*/
  /*  if (vsc_count < 38372100LL) return;*/
  /* if (vsc_count == 80) {
     printf("Here I go\n");
     } */
  /* if (vsc_count > 500000) exit(0); */
  /* if (gc_calls < 1) return; */
#if defined(__GNUC__)
  fprintf(_YAP_stderr,"%llu ", vsc_count);
#endif
  /* check_trail_consistency(); */
  if (pred == NULL) {
    return;
  }
  if (pred->ModuleOfPred == 0 && !do_trace_primitives) {
    return;
  }
  switch (port) {
  case enter_pred:
    mname = RepAtom(AtomOfTerm(_YAP_Module_Name((CODEADDR)pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0)
      s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      else
    s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    /*    if ((pred->ModuleOfPred == 0) && (s[0] == '$'))
	  return;       */
    send_tracer_message("CALL: ", s, arity, mname, args);
    break;
  case try_or:
    send_tracer_message("TRY_OR ", NULL, 0, NULL, args);
    break;
  case retry_or:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    send_tracer_message("RETRY_OR ", NULL, 0, NULL, args);
    break;
  case retry_table_producer:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    /* HANDLE METACALLS */
    if (pred == NULL) {
      send_tracer_message("RETRY TABLE: ", NULL, 0, NULL, args);
    } else {
      mname = RepAtom(AtomOfTerm(_YAP_Module_Name((CODEADDR)pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (arity == 0)
	s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      else
	s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      /*    if ((pred->ModuleOfPred == 0) && (s[0] == '$'))
	    return;      */
      send_tracer_message("RETRY PRODUCER: ", s, 0, mname, NULL);
    }
    break;
  case retry_table_consumer:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    /* HANDLE METACALLS */
    if (pred == NULL) {
      send_tracer_message("RETRY TABLE: ", NULL, 0, NULL, args);
    } else {
      mname = RepAtom(AtomOfTerm(_YAP_Module_Name((CODEADDR)pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (arity == 0)
	s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      else
	s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      /*    if ((pred->ModuleOfPred == 0) && (s[0] == '$'))
	    return;      */
      send_tracer_message("RETRY CONSUMER: ", s, 0, mname, NULL);
    }
    break;
  case retry_pred:
    mname = RepAtom(AtomOfTerm(_YAP_Module_Name((CODEADDR)pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0)
      s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
    else
      s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    /*    if ((pred->ModuleOfPred == 0) && (s[0] == '$'))
	  return;      */
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    send_tracer_message("RETRY: ", s, arity, mname, args);
    break;
  }
}

void
toggle_low_level_trace(void)
{
  _YAP_do_low_level_trace = !_YAP_do_low_level_trace;
}

static Int p_start_low_level_trace(void)
{
  _YAP_do_low_level_trace = TRUE;
  return(TRUE);
}

static Int p_stop_low_level_trace(void)
{
  _YAP_do_low_level_trace = FALSE;
  do_trace_primitives = TRUE;
  return(TRUE);
}

void
_YAP_InitLowLevelTrace(void)
{
  _YAP_InitCPred("start_low_level_trace", 0, p_start_low_level_trace, SafePredFlag);
  _YAP_InitCPred("stop_low_level_trace", 0, p_stop_low_level_trace, SafePredFlag);
}

#endif

