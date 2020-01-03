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
 * File:		tracer.h *
 * Last rev:								 *
 * mods: *
 * comments:	definitions for low level tracer			 *
 *									 *
 *************************************************************************/

#include "Yap.h"

#ifdef LOW_LEVEL_TRACER

#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "attvar.h"
#include "clause.h"
#include "iopreds.h"
#include "tracer.h"
#include "yapio.h"

static char *send_tracer_message(char *start, char *name, arity_t arity,
                                 char *mname, CELL *args, char **s0, char *s,
                                 char **top) {
  bool expand = false;
  size_t max = *top - (s + 1);
  int d, min = 1024;
  char *s1 = s;
  do {
    if (expand || max < 32) {
      Int cbeg = s1 - *s0;
      max = *top - *s0;
      max += min;
      *s0 = Realloc(*s0, max);

      *top = *s0 + max;
      max--;
      s1 = *s0 + cbeg;
      s = s1;
      expand = false;
    }
    min = 1024;
    if (name == NULL) {
#ifdef YAPOR
      d = snprintf(s, max, "(%d)%s", worker_id, start);
#else
      d = snprintf(s, max, "%s", start);
#endif
    } else {

      if (arity) {
        if (args)
          d = snprintf(s, max, "%s %s:%s(", start, mname, name);
        else
          d = snprintf(s, max, "%s %s:%s/%lu", start, mname, name,
                       (unsigned long int)arity);
      } else {
        d = snprintf(s, max, "%s %s:%s", start, mname, name);
      }
    }
    if (d >= max) {
      expand = true;
      min = d + 1024;
      continue;
    }
    max -= d;
    s += d;
    if (args) {
      int i;
      for (i = 0; i < arity; i++) {
        if (i > 0) {
          if (max > 16) {
            *s++ = ',';
            *s++ = ' ';
            max -= 2;
          } else {
            expand = true;
            continue;
          }
        }
	//Int md = LOCAL_max_depth, ml = LOCAL_max_list, ma = LOCAL_max_write_args;
	//LOCAL_max_depth=6, LOCAL_max_list=6, LOCAL_max_write_args = 6;
        const char *sn = Yap_TermToBuffer(args[i],
                                          Handle_cyclics_f|Quote_illegal_f | Handle_vars_f|Singleton_vars_f);
//	LOCAL_max_depth=md, LOCAL_max_list=ml, LOCAL_max_write_args = ma;
        size_t sz;
        if (sn == NULL) {
	  sn = Malloc(strlen("<* error *>")+1);
	  strcpy((char*)sn, "<* error *>");
        }
        sz = strlen(sn);
        if (max <= sz) {
          min = sz + 1024;
          expand = true;
          continue;
        }
        strcpy(s, sn);
	sn = NULL;
        s += sz;
        max -= sz;
      }
      if (arity) {
        *s++ = ' ';
        *s++ = ')';
        max -= 2;
      }
    }
  } while (expand);
  s[0] = '\0';
  return s;
}

#if defined(__GNUC__) || defined(__clang__)
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
  while (ptr > (CELL *)LOCAL_TrailBase) {
    ptr = --ptr;
    if (!IsVarTerm(TrailTerm(ptr))) {
      if (IsApplTerm(TrailTerm(ptr))) {
        CELL *cptr = (CELL *)ptr;
        ptr = (tr_fr_ptr)(cptr-1);
      } else {
        if (IsPairTerm(TrailTerm(ptr))) {
          CELL *p = RepPair(TrailTerm(ptr));
          if IsAttVar(p) continue;
        }
        printf("Oops at call %ld, B->cp(%p) TR(%p)  pt(%p)\n",
vsc_count,B->cp_tr, TR, ptr);
        return(FALSE);
      }
    }
  }
  return(TRUE);
}
*/

volatile int v;

CELL old_value = 0L, old_value2 = 0L;

void jmp_deb(int), jmp_deb2(void);

void jmp_deb2(void) { fprintf(stderr, "Here\n"); }

void jmp_deb(int i) {
  if (i)
    printf("Here we go " Int_FORMAT "\n", old_value++);
  if (old_value == 716)
    jmp_deb2();
}

struct various_codes *sc;

/*
CELL array[332];

int found = FALSE;

static void
check_area(void)
{
  int i, first = -1;
  for (i= 0; i < 332; i++) {
    if (array[i] !=((CELL *)0x187a800)[i]) {
      if (first != -1) {
        first = i;
        found = TRUE;
      }
      fprintf(stderr,"%lld changed %d\n",vsc_count,i);
    }
    array[i] = ((CELL *)0x187a800)[i];
  }
  if (first != -1)
    jmp_deb(i);
}
*/
// PredEntry *old_p[10000];
// Term old_x1[10000], old_x2[10000], old_x3[10000];

// static CELL oldv = 0;

bool low_level_trace__(yap_low_level_port port, PredEntry *pred, CELL *args) {
  CACHE_REGS
  char *s;
  char *mname;
  Int arity;
  CELL *H1 =HR;
  int l = push_text_stack();
  /*  extern int gc_calls; */
  vsc_count++;
  //fprintf(stderr,"%p-%p\n",B->cp_tr,TR);
    if (vsc_count >= 2615LL)
      jmp_deb(1);
  // if (HR < ASP ) return;
  // fif (vsc_count == 12534) jmp_deb( 2 );
  char *buf = Malloc(512), *top = buf + 511, *b = buf;
      // if (!worker_id) return;
  LOCK(Yap_low_level_trace_lock);
  sc = Yap_heap_regs;
// if (vsc_count == 161862) jmp_deb(1);
#ifdef THREADS
  LOCAL_ThreadHandle.thread_inst_count++;
#endif
#ifdef COMMENTED
  b = snprintf(b, top - b, "in %p\n");
  CELL *gc_ENV = ENV;
  while (gc_ENV != NULL) { /* no more environments */
    b = snprintf(b, top - b, "%ld\n", LCL0 - gc_ENV);
    gc_ENV = (CELL *)gc_ENV[E_E]; /* link to prev
                                   * environment */
  }
  return;
  {
    choiceptr b_p = B;
    while (b_p) {
      b = snprintf(b, top - b, "%p %ld\n", b_p,
                   Yap_op_from_opcode(b_p->cp_ap->opc));
      b_p = b_p->cp_b;
    }
  }
  {
    choiceptr myB = B;
    while (myB)
      myB = myB->cp_b;
  }
  //*(H0+(0xb65f2850-0xb64b2008)/sizeof(CELL))==0xc ||
  // 0x4fd4d
  if (vsc_count > 1388060LL && vsc_count < 1388070LL) {
    if (vsc_count % 1LL == 0) {
      UInt sz = Yap_regp->H0_[17];
      UInt end = sizeof(MP_INT) / sizeof(CELL) + sz + 1;
      b = snprintf(b, top - b, "VAL %lld %d %x/%x\n", vsc_count, sz, H0[16],
                   H0[16 + end]);
    }
  } else
    return;
  {
    tr_fr_ptr pt = (tr_fr_ptr)LOCAL_TrailBase;
    if (pt[140].term == 0 && pt[140].value != 0)
      jmp_deb(1);
  }
  if (worker_id != 04 || worker_id != 03)
    return;
  //  if (vsc_count == 218280)
  //    vsc_xstop = 1;
  if (vsc_count < 1468068888) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (port != enter_pred || !pred || pred->ArityOfPE != 4 ||
      strcmp(RepAtom(NameOfFunctor(pred->FunctorOfPred))->StrOfAE,
             "in_between_target_phrases")) {
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
  if (vsc_count == 123536441LL)
    vsc_xstop = 1;
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
  if (TR_FZ > TR)
    jmp_deb(1);
  {
    tr_fr_ptr pt = (tr_fr_ptr)LOCAL_TrailBase;
    if (pt[153].term == 0 && pt[153].value == 0 && pt[154].term != 0 &&
        pt[154].value != 0 && (TR > pt + 154 || TR_FZ > pt + 154))
      jmp_deb(2);
    if (pt[635].term == 0 && pt[635].value == 0 && pt[636].term != 0 &&
        pt[636].value != 0 && (TR > pt + 636 || TR_FZ > pt + 636))
      jmp_deb(3);
    if (pt[138].term == 0 && pt[138].value == 0 && pt[139].term != 0 &&
        pt[139].value != 0 && (TR > pt + 138 || TR_FZ > pt + 138))
      jmp_deb(4);
  }
  if (vsc_count == 287939LL)
    jmp_deb(1);
  if (vsc_count == 173118LL)
    jmp_deb(1);
  if (!(vsc_count >= 287934LL && vsc_count <= 287939LL) &&
      !(vsc_count >= 173100LL && vsc_count <= 173239LL) && vsc_count != -1) {
    return;
  }
  if (vsc_count == 51021) {
    printf("Here I go\n");
  }
  if (vsc_count < 52000) {
    UNLOCK(Yap_heap_regs->low_level_trace_lock);
    return;
  }
  if (vsc_count > 52000)
    exit(0);
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

      if (p == pe) {
        UNLOCK(Yap_heap_regs->low_level_trace_lock);
	HR 
        pop_text_stack(l);
        return (true);
      }
      if (env_ptr != NULL)
        env_ptr = (CELL *)(env_ptr[E_E]);
    }
    printf("\n");
  }
#endif
  b += snprintf(b, top - b, "%llud " UInt_FORMAT " ", vsc_count,
                LCL0 - (CELL *)B);
  b += snprintf(b, top - b, Int_FORMAT " ", LOCAL_CurHandle);
#if defined(THREADS) || defined(YAPOR)
  b += snprintf(b, top - b, "(%d)", worker_id);
#endif
  /* check_trail_consistency(); */
  if (pred == NULL) {
    //HR = HI;
    UNLOCK(Yap_low_level_trace_lock);
    pop_text_stack(l);
    return (true);
  }
  if (pred->ModuleOfPred == PROLOG_MODULE) {
    if (!LOCAL_do_trace_primitives) {
      // HR = HI;
    UNLOCK(Yap_low_level_trace_lock);
      pop_text_stack(l);
      return (true);
    }
    mname = "prolog";
  } else {
    mname = RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
  }
  switch (port) {
  case enter_pred:
    arity = pred->ArityOfPE;
    if (arity == 0) {
      s = (char *)RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
    } else {
      s = (char *)RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    }
    /*    if ((pred->ModuleOfPred == 0) && (s[0] == '$'))
          return;       */
    b = send_tracer_message("CALL: ", s, arity, mname, args, &buf, b, &top);
    break;
  case try_or:
    b = send_tracer_message("TRY_OR ", NULL, 0, NULL, args, &buf, b, &top);
    break;
  case retry_or:
    b = send_tracer_message("FAIL ", NULL, 0, NULL, args, &buf, b, &top);
    b = send_tracer_message("RETRY_OR ", NULL, 0, NULL, args, &buf, b, &top);
    break;
  case retry_table_generator:
    b = send_tracer_message("FAIL ", NULL, 0, NULL, args, &buf, b, &top);
    mname = (char *)RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0)
      s = (char *)RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
    else
      s = (char *)RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
    b = send_tracer_message("RETRY GENERATOR: ", s, arity, mname, args, &buf, b,
                            &top);
    break;
  case retry_table_consumer:
    b = send_tracer_message("FAIL ", NULL, 0, NULL, args, &buf, b, &top);
    mname = (char *)RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
    arity = pred->ArityOfPE;
    if (arity == 0) {
      s = (char *)RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      b = send_tracer_message("RETRY CONSUMER: ", s, 0, mname, NULL, &buf, b,
                              &top);
    } else {
      s = (char *)RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      b = send_tracer_message("RETRY CONSUMER: ", s, pred->ArityOfPE, mname,
                              NULL, &buf, b, &top);
    }
    break;
  case retry_table_loader:
    b = send_tracer_message("FAIL ", NULL, 0, NULL, args, &buf, b, &top);
    if (pred == UndefCode) {
      b = send_tracer_message("RETRY LOADER ", NULL, 0, NULL, NULL, &buf, b,
                              &top);
    } else {
      mname = (char *)RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (arity == 0)
        s = (char *)RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      else
        s = (char *)RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      b = send_tracer_message("RETRY LOADER: ", s, 0, mname, NULL, &buf, b,
                              &top);
    }
    break;
  case retry_pred:
    b = send_tracer_message("FAIL ", NULL, 0, NULL, args, &buf, b, &top);
    if (pred != NULL) {
      mname = (char *)RepAtom(AtomOfTerm(Yap_Module_Name(pred)))->StrOfAE;
      arity = pred->ArityOfPE;
      if (pred->ModuleOfPred == IDB_MODULE) {
        s = "recorded";
        arity = 3;
      } else if (arity == 0) {
        s = (char *)RepAtom((Atom)pred->FunctorOfPred)->StrOfAE;
      } else {
        s = (char *)RepAtom(NameOfFunctor((pred->FunctorOfPred)))->StrOfAE;
      }
      b = send_tracer_message("RETRY: ", s, arity, mname, args, &buf, b, &top);
    }
    break;
  }
  UNLOCK(Yap_low_level_trace_lock);
#if __ANDROID__
  __android_log_print(ANDROID_LOG_ERROR, "YAPDroid", "%s\n", buf);
#else
  *b++ = '\n';
  *b = '\0';
  fputs(buf, stderr);
#endif
  //  if (vsc_count==1271) jmp_deb(1);
  pop_text_stack(l);
  HR = H1;
  return (true);
}

void toggle_low_level_trace(void) {
  Yap_do_low_level_trace = !Yap_do_low_level_trace;
}

static Int start_low_level_trace(USES_REGS1) {
  Yap_do_low_level_trace = TRUE;
  return (TRUE);
}

static Int total_choicepoints(USES_REGS1) {
  return Yap_unify(MkIntegerTerm(LOCAL_total_choicepoints), ARG1);
}

static Int reset_total_choicepoints(USES_REGS1) {
  LOCAL_total_choicepoints = 0;
  return TRUE;
}

static Int show_low_level_trace(USES_REGS1) {
  fprintf(stderr, "Call counter=%lld\n", vsc_count);
  return (TRUE);
}

#ifdef THREADS
static Int start_low_level_trace2(USES_REGS1) {
  thread_trace = IntegerOfTerm(Deref(ARG1)) + 1;
  Yap_do_low_level_trace = TRUE;
  return (TRUE);
}
#endif

#include <stdio.h>

/** @pred stop_low_level_trace

Stop displaying messages at procedure entry and retry.

Note that using this compile-time option will slow down execution, even if
messages  are
not being output.

 */
static Int stop_low_level_trace(USES_REGS1) {
  Yap_do_low_level_trace = FALSE;
  LOCAL_do_trace_primitives = TRUE;
#if DEBUG_LOCKS////
  debug_locks = TRUE;
#endif
  return (TRUE);
}

volatile int v_wait;

static Int vsc_wait(USES_REGS1) {
  while (!v_wait)
    ;
  return true;
}

static Int vsc_go(USES_REGS1) {
  v_wait = 1;
  return true;
}

void Yap_InitLowLevelTrace(void) {
  Yap_InitCPred("start_low_level_trace", 0, start_low_level_trace,
                SafePredFlag);
  Yap_InitCPred("$start_low_level_trace", 0, start_low_level_trace,
                SafePredFlag);
/** @pred start_low_level_trace


Begin display of messages at procedure entry and retry.

<
*/
#if THREADS
  Yap_InitCPred("start_low_level_trace", 1, start_low_level_trace2,
                SafePredFlag);
#endif
  Yap_InitCPred("stop_low_level_trace", 0, stop_low_level_trace, SafePredFlag);
  Yap_InitCPred("show_low_level_trace", 0, show_low_level_trace, SafePredFlag);
  Yap_InitCPred("$stop_low_level_trace", 0, stop_low_level_trace, SafePredFlag);
  Yap_InitCPred("total_choicepoints", 1, total_choicepoints, SafePredFlag);
  Yap_InitCPred("reset_total_choicepoints", 0, reset_total_choicepoints,
                SafePredFlag);
  Yap_InitCPred("vsc_wait", 0, vsc_wait, SafePredFlag);
  Yap_InitCPred("vsc_go", 0, vsc_go, SafePredFlag);
}

#else

static null(USES_REGS1) { return true; }

void Yap_InitLowLevelTrace(void) {
  Yap_InitCPred("$start_low_level_trace", 0, null,
                SafePredFlag | HiddenPredFlag);
  Yap_InitCPred("$stop_low_level_trace", 0, null,
                SafePredFlag | HiddenPredFlag);
}
#endif
