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
  return(putc(ch, Yap_stderr)); /* use standard error stream, which is supposed to be 2*/
}

static void
send_tracer_message(char *start, char *name, Int arity, char *mname, CELL *args)
{
  UInt omax_depth, omax_list, omax_write_args;

  if (name == NULL) {
#ifdef  YAPOR
    fprintf(Yap_stderr, "(%d)%s", worker_id, start);
#else
    fprintf(Yap_stderr, "%s", start);
#endif
  } else {
    int i;

    if (arity) {
      fprintf(Yap_stderr, "%s %s:%s(", start, mname, name);
    } else {
      fprintf(Yap_stderr, "%s %s:%s", start, mname, name);
    }
    for (i= 0; i < arity; i++) {
      if (i > 0) fprintf(Yap_stderr, ",");
#if DEBUG
#if COROUTINING
      /* Yap_Portray_delays = TRUE; */
#endif
#endif
  omax_depth = max_depth;
  omax_list = max_list;
  omax_write_args = max_write_args;
  max_depth = 5;
  max_list = 5;
  max_write_args = 5;
  Yap_plwrite(args[i], TracePutchar, Handle_vars_f);
  max_depth = omax_depth;
  max_list = omax_list;
  max_write_args = omax_write_args;
#if DEBUG
#if COROUTINING
      Yap_Portray_delays = FALSE;
#endif
#endif
    }
    if (arity) fprintf(Yap_stderr, ")");
  }
  fprintf(Yap_stderr, "\n");
}

#if defined(__GNUC__)
unsigned long long vsc_count;
#else
unsigned long vsc_count;
#endif

#if THREADS
static int thread_trace;
#endif

/*
static int
check_trail_consistency(void) {
  tr_fr_ptr ptr = TR;
  while (ptr > (CELL *)Yap_TrailBase) {
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


CELL old_value = 0L, old_value2 = 0L;

void jmp_deb(int);

void
jmp_deb(int i) {if (i) printf("Here we go\n"); else jmp_deb(0);}

struct various_codes *sc;

void
low_level_trace(yap_low_level_port port, PredEntry *pred, CELL *args)
{
  char *s;
  char *mname;
  Int arity;
  /*  extern int gc_calls; */

  LOCK(Yap_heap_regs->low_level_trace_lock);
  sc = Yap_heap_regs;
  vsc_count++;
  //  if (vsc_count < 13600) {
  //    UNLOCK(Yap_heap_regs->low_level_trace_lock);
  //    return;
  //  }
#ifdef COMMENTED
  //  if (vsc_count == 218280)
  //    vsc_xstop = 1;
  if (vsc_count < 1468068888) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (port != enter_pred ||
      !pred ||
      pred->ArityOfPE != 4 ||
      strcmp(RepAtom(NameOfFunctor(pred->FunctorOfPred))->StrOfAE,"in_between_target_phrases")) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count < 1246949400LL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count == 1246949493LL)
    vsc_xstop = TRUE;
  if (vsc_count < 5646100000LL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count == 5646100441LL)
    vsc_xstop = TRUE;
  if (vsc_count < 2923351500LL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count == 123536441LL) vsc_xstop = 1;
  if (vsc_count < 5530257LL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count == 9414280LL) {
    vsc_xstop = TRUE;
  }
  if (vsc_count < 3399741LL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count == 51021) {
    printf("Here I go\n");
  }
  if (vsc_count < 52000) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count > 52000) exit(0);
  UNLOCK(Yap_heap_regs->low_level_trace_lock);
  return;
  if (vsc_count == 837074) {
    printf("Here I go\n");
  } 
  if (gc_calls < 1) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  {
    CELL *env_ptr = ENV;
    PredEntry *p;

    while (env_ptr) {
      PredEntry *pe = EnvPreg(env_ptr[E_CP]);

      printf("%p->",env_ptr,pe);
      if (vsc_count == 52LL) printf("\n");
      if (p == pe) {
	UNLOCK(Yap_heap_regs->low_level_trace_lock);
	return;
      }
      if (env_ptr != NULL)
	env_ptr = (CELL *)(env_ptr[E_E]);
      }
      printf("\n");
 }
#endif
  fprintf(Yap_stderr,"%lld %x ", vsc_count,ActiveSignals);
#if defined(THREADS) || defined(YAPOR)
  fprintf(Yap_stderr,"(%d)", worker_id);
#endif
  /* check_trail_consistency(); */
  if (pred == NULL) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (pred->ModuleOfPred == 0 && !do_trace_primitives) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  switch (port) {
  case enter_pred:
    mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
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
  case retry_table_generator:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0)
      s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
    else
      s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    send_tracer_message("RETRY GENERATOR: ", s, arity, mname, args);
    break;
  case retry_table_consumer:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0)
      s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
    else
      s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    send_tracer_message("RETRY CONSUMER: ", s, 0, mname, NULL);
    break;
  case retry_table_loader:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    if (pred == UndefCode) {
      send_tracer_message("RETRY LOADER ", NULL, 0, NULL, NULL);
    } else {
      mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (arity == 0)
	s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      else
	s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      send_tracer_message("RETRY LOADER: ", s, 0, mname, NULL);
    }
    break;
  case retry_pred:
    send_tracer_message("FAIL ", NULL, 0, NULL, args);
    if (pred != NULL) {
      mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (pred->ModuleOfPred == IDB_MODULE) {
	s = "recorded";
	arity = 3;
      } else if (arity == 0) {
	s = RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      } else {
	s = RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      }
      send_tracer_message("RETRY: ", s, arity, mname, args);
    }
    break;
  }
  UNLOCK(Yap_heap_regs->low_level_trace_lock);
}

void
toggle_low_level_trace(void)
{
  Yap_do_low_level_trace = !Yap_do_low_level_trace;
}

static Int p_start_low_level_trace(void)
{
  Yap_do_low_level_trace = TRUE;
  return(TRUE);
}

#ifdef THREADS
static Int p_start_low_level_trace2(void)
{
  thread_trace = IntegerOfTerm(Deref(ARG1))+1;
  Yap_do_low_level_trace = TRUE;
  return(TRUE);
}
#endif

static Int p_stop_low_level_trace(void)
{
  Yap_do_low_level_trace = FALSE;
  do_trace_primitives = TRUE;
  return(TRUE);
}

void
Yap_InitLowLevelTrace(void)
{
  Yap_InitCPred("start_low_level_trace", 0, p_start_low_level_trace, SafePredFlag);
#if THREADS
  Yap_InitCPred("start_low_level_trace", 1, p_start_low_level_trace2, SafePredFlag);
#endif
  Yap_InitCPred("stop_low_level_trace", 0, p_stop_low_level_trace, SafePredFlag);
}

#endif



