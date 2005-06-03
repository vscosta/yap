/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.preds.c
  version:     $Id: opt.preds.c,v 1.17 2005-06-03 18:28:11 ricroc Exp $   
                                                                     
**********************************************************************/

/* ----------------------------------------------- **
**      Includes, defines and local variables      **
** ----------------------------------------------- */

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#ifdef YAPOR
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */

#ifdef YAPOR
#define TIME_RESOLUTION 1000000
#define NO_ANSWER   0
#define YES_ANSWER -1
static int length_answer;
static qg_ans_fr_ptr actual_answer;
#endif /* YAPOR */



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

#ifdef YAPOR
static realtime current_time(void);
static int p_yapor_on(void);
static int p_start_yapor(void);
static int p_sequential(void);
static int p_default_sequential(void);
static int p_execution_mode(void);
static int p_performance(void);
static int p_parallel_new_answer(void);
static int p_parallel_yes_answer(void);
static int parallel_new_answer_putchar(int sno, int ch);
static void show_answers(void);
static void answer_to_stdout(char *answer);
#endif /* YAPOR */
#ifdef TABLING
static int p_do_table(void);
static int p_do_tabling_mode(void);
static int p_do_abolish_trie(void);
static int p_do_show_trie(void);
static int p_do_show_trie_stats(void);
#endif /* TABLING */
#ifdef STATISTICS
static int p_show_frames_stats(void);
#endif /* STATISTICS */
#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
static int p_debug_prolog(void);
#endif /* YAPOR_ERRORS || TABLING_ERRORS */



/* -------------------------- **
**      Global functions      **
** -------------------------- */

void Yap_init_optyap_preds(void) {
#ifdef YAPOR
  Yap_InitCPred("$yapor_on", 0, p_yapor_on, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$start_yapor", 0, p_start_yapor, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$sequential", 1, p_sequential, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$default_sequential", 1, p_default_sequential, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("execution_mode", 1, p_execution_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("performance", 1, p_performance, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$parallel_new_answer", 1, p_parallel_new_answer, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$parallel_yes_answer", 0, p_parallel_yes_answer, SafePredFlag|SyncPredFlag|HiddenPredFlag);
#endif /* YAPOR */
#ifdef TABLING
  Yap_InitCPred("$do_table", 2, p_do_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$do_tabling_mode", 3, p_do_tabling_mode, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$do_abolish_trie", 2, p_do_abolish_trie, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$do_show_trie", 2, p_do_show_trie, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$do_show_trie_stats", 2, p_do_show_trie_stats, SafePredFlag|SyncPredFlag|HiddenPredFlag);
#endif /* TABLING */
#ifdef STATISTICS
  Yap_InitCPred("show_frames_stats", 0, p_show_frames_stats, SafePredFlag|SyncPredFlag);
#endif /* STATISTICS */
#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
  Yap_InitCPred("debug_prolog", 1, p_debug_prolog, SafePredFlag|SyncPredFlag);
#endif /* YAPOR_ERRORS || TABLING_ERRORS */
}


#ifdef YAPOR
void finish_yapor(void) {
  GLOBAL_execution_time = current_time() - GLOBAL_execution_time;
  show_answers();
  return;
}
#endif /* YAPOR */



/* ------------------------- **
**      Local functions      **
** ------------------------- */

#ifdef YAPOR
static
realtime current_time(void) {
  /* to get time as Yap */
  /*
  double now, interval;
  Yap_cputime_interval(&now, &interval);
  return ((realtime)now);
  */
  struct timeval tempo;
  gettimeofday(&tempo, NULL);
  return ((realtime)tempo.tv_sec + (realtime)tempo.tv_usec / TIME_RESOLUTION);
}


static
int p_yapor_on(void) {
  return (PARALLEL_EXECUTION_MODE);
}


static
int p_start_yapor(void) {
#ifdef TIMESTAMP_CHECK
  GLOBAL_timestamp = 0;
#endif /* TIMESTAMP_CHECK */
  GLOBAL_answers = NO_ANSWER;
  BITMAP_delete(GLOBAL_bm_idle_workers, 0);
  BITMAP_clear(GLOBAL_bm_invisible_workers);
  BITMAP_clear(GLOBAL_bm_requestable_workers);
#ifdef TABLING_INNER_CUTS
  BITMAP_clear(GLOBAL_bm_pruning_workers);
#endif /* TABLING_INNER_CUTS */
  make_root_choice_point();
  GLOBAL_execution_time = current_time();
  BITMAP_clear(GLOBAL_bm_finished_workers);
  PUT_IN_EXECUTING(worker_id);
  return (TRUE);
}


static
int p_sequential(void) {
  Term t, mod;
  PredEntry *pe;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return(FALSE);
  }
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor func = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(func, mod));
  } else {
    return(FALSE);
  }
  pe->PredFlags |= SequentialPredFlag;
  return (TRUE);
}


static
int p_default_sequential(void) {
  Term t;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Term ta;
    if (SEQUENTIAL_IS_DEFAULT)
      ta = MkAtomTerm(Yap_LookupAtom("on"));
    else
      ta = MkAtomTerm(Yap_LookupAtom("off"));
    Bind((CELL *)t, ta);
    return(TRUE);
  } 
  if (IsAtomTerm(t)) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
    if (strcmp(s, "on") == 0) {
      SEQUENTIAL_IS_DEFAULT = TRUE;
      return(TRUE);
    } 
    if (strcmp(s,"off") == 0) {
      SEQUENTIAL_IS_DEFAULT = FALSE;
      return(TRUE);
    }
  }
  return(FALSE);
}


static
int p_execution_mode(void) {
  Term t;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Term ta;
    if (PARALLEL_EXECUTION_MODE) 
      ta = MkAtomTerm(Yap_LookupAtom("parallel"));
    else 
      ta = MkAtomTerm(Yap_LookupAtom("sequential"));
    Bind((CELL *)t, ta);
    return(TRUE);
  }
  if (IsAtomTerm(t)) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
    if (strcmp(s,"parallel") == 0) {
      PARALLEL_EXECUTION_MODE = TRUE;
      return(TRUE);
    } 
    if (strcmp(s,"sequential") == 0) {
      PARALLEL_EXECUTION_MODE = FALSE;
      return(TRUE);
    }
  }
  return(FALSE);
}


static
int p_performance(void) {
  Term t;
  realtime one_worker_execution_time = 0;
  int i;

  GLOBAL_performance_mode |= PERFORMANCE_IN_EXECUTION;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Term ta;
    if (GLOBAL_performance_mode & PERFORMANCE_ON) {
      ta = MkAtomTerm(Yap_LookupAtom("on"));
    } else { 
      ta = MkAtomTerm(Yap_LookupAtom("off"));
    }
    Bind((CELL *)t, ta);
    return(TRUE);
  }
  if (IsAtomTerm(t)) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
    if (strcmp(s, "on") == 0) {
      GLOBAL_performance_mode |= PERFORMANCE_ON;
      return(TRUE);
    } 
    if (strcmp(s,"off") == 0) {
      GLOBAL_performance_mode &= ~PERFORMANCE_ON;
      return(TRUE);
    }
    if (strcmp(s,"clear") == 0) {
      GLOBAL_number_goals = 0;
      GLOBAL_best_times(0) = 0;
      return(TRUE);
    }
  }
  if (IsIntTerm(t))
    one_worker_execution_time = IntOfTerm(t);
  else if (IsFloatTerm(t))
    one_worker_execution_time = FloatOfTerm(t);
  else 
    return(FALSE);

  if (GLOBAL_number_goals) {
    fprintf(stdout, "[\n  Best execution times:\n");
    for (i = 1; i <= GLOBAL_number_goals; i++) {
      fprintf(stdout, "    %d. time: %f seconds", i, GLOBAL_best_times(i));  
      if (one_worker_execution_time != 0)
        fprintf(stdout, " --> speedup %f (%6.2f %% )\n",
                one_worker_execution_time / GLOBAL_best_times(i),
                one_worker_execution_time / GLOBAL_best_times(i) / number_workers * 100 );
      else fprintf(stdout, "\n");
    }

    fprintf(stdout, "  Average             : %f seconds",
            GLOBAL_best_times(0) / GLOBAL_number_goals);
    if (one_worker_execution_time != 0)
      fprintf(stdout, " --> speedup %f (%6.2f %% )",
              one_worker_execution_time * GLOBAL_number_goals / GLOBAL_best_times(0),
              one_worker_execution_time * GLOBAL_number_goals / GLOBAL_best_times(0) / number_workers * 100 );

    if (GLOBAL_number_goals >= 3) {
      fprintf(stdout, "\n  Average (best three): %f seconds",
              (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)) / 3);
      if (one_worker_execution_time != 0)
        fprintf(stdout, " --> speedup %f (%6.2f %% ) ]\n\n",
                one_worker_execution_time * 3 / (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)),
                one_worker_execution_time * 3 / (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)) / number_workers * 100 );
      else fprintf(stdout, "\n]\n\n");
    } else fprintf(stdout, "\n]\n\n");
    return (TRUE);
  }
  return (FALSE);
}


static
int p_parallel_new_answer(void) {
  or_fr_ptr leftmost_or_fr;

  length_answer = 0;
  ALLOC_QG_ANSWER_FRAME(actual_answer);
  Yap_plwrite(ARG1, parallel_new_answer_putchar, 4);
  AnsFr_answer(actual_answer)[length_answer] = 0;
  AnsFr_next(actual_answer) = NULL;
  leftmost_or_fr = CUT_leftmost_or_frame();
  LOCK_OR_FRAME(leftmost_or_fr);
  if (LOCAL_prune_request) {
    UNLOCK_OR_FRAME(leftmost_or_fr);
    FREE_QG_ANSWER_FRAME(actual_answer);
  } else {
    CUT_store_answer(leftmost_or_fr, actual_answer);
    UNLOCK_OR_FRAME(leftmost_or_fr);
  }
  return (TRUE);
}


static
int p_parallel_yes_answer(void) {
  GLOBAL_answers = YES_ANSWER;
  return (TRUE);
}


static
int parallel_new_answer_putchar(int sno, int ch) {
  AnsFr_answer(actual_answer)[length_answer++] = ch;
  return ch;
}


static
void show_answers(void) {
  int i;
  if (OrFr_qg_solutions(LOCAL_top_or_fr)) {
    qg_ans_fr_ptr aux_answer1, aux_answer2;
    aux_answer1 = SolFr_first(OrFr_qg_solutions(LOCAL_top_or_fr));
    while (aux_answer1) {
      answer_to_stdout(AnsFr_answer(aux_answer1));
      aux_answer2 = aux_answer1;
      aux_answer1 = AnsFr_next(aux_answer1);
      FREE_QG_ANSWER_FRAME(aux_answer2);
      GLOBAL_answers++;
    }
    FREE_QG_SOLUTION_FRAME(OrFr_qg_solutions(LOCAL_top_or_fr));
    OrFr_qg_solutions(LOCAL_top_or_fr) = NULL;
  }
  switch(GLOBAL_answers) {
    case YES_ANSWER:
      fprintf(stdout, "[ yes");
      break;
    case NO_ANSWER:  
      fprintf(stdout, "[ no");
      break;
    case 1:
      fprintf(stdout, "[ 1 answer found");
      break;
    default:
         fprintf(stdout, "[ %d answers found", GLOBAL_answers);
      break;
  }
  fprintf(stdout, " (in %f seconds) ]\n\n", GLOBAL_execution_time);

  if (GLOBAL_performance_mode & PERFORMANCE_IN_EXECUTION) {
    GLOBAL_performance_mode &= ~PERFORMANCE_IN_EXECUTION;
  } else if (GLOBAL_performance_mode == PERFORMANCE_ON) {
    for (i = GLOBAL_number_goals; i > 0; i--) {
      if (GLOBAL_best_times(i) > GLOBAL_execution_time) {
        if (i + 1 < MAX_BEST_TIMES)
          GLOBAL_best_times(i + 1) = GLOBAL_best_times(i);
        else {
          GLOBAL_best_times(0) -= GLOBAL_best_times(i);
        }
      }
      else break;
    }
    if (i + 1 < MAX_BEST_TIMES) {
      GLOBAL_best_times(0) += GLOBAL_execution_time;
      GLOBAL_best_times(i + 1) = GLOBAL_execution_time;
      if (GLOBAL_number_goals + 1 < MAX_BEST_TIMES)
        GLOBAL_number_goals++;
    }
  }

  return;
}


static
void answer_to_stdout(char *answer) {
  int length_answer = 0, length_output = 0, caracter, list, par_rectos;
  char output[MAX_LENGTH_ANSWER];
  while (1) {
    length_answer += 2;
    while (answer[length_answer] != ']') {
      length_answer++;
      caracter = 0;
      while (answer[length_answer] != ',' && answer[length_answer] != ']')
	caracter = caracter * 10 + answer[length_answer++] - '0';
      output[length_output++] = caracter;
    }
    length_answer++;
    output[length_output++] = ' ';
    output[length_output++] = '=';
    output[length_output++] = ' '; 
    if (answer[length_answer++] == ',') {
      list = 1;
      output[length_output++] = '[';
    } else list = 0;
    par_rectos = 1;
    while (1) {
      if (answer[length_answer] == '[') par_rectos++;
      else if (answer[length_answer] == ']' && --par_rectos == 0) break;
      output[length_output++] = answer[length_answer++];
    }
    if (list) output[length_output++] = ']';
    if (answer[++length_answer] != ']') {
      output[length_output++] = ' ';
      output[length_output++] = ';';
      output[length_output++] = ' ';
    }
    else break;
  }
  output[length_output] = 0;
  fprintf(stdout, "  %s\n", output);
  return;
}
#endif /* YAPOR */


#ifdef TABLING
static
int p_do_table(void) {
  Term t, mod;
  PredEntry *pe;
  tab_ent_ptr te;
  sg_node_ptr sg_node;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor func = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(func, mod));
  } else {
    return (FALSE);
  }
  if (pe->PredFlags & TabledPredFlag) {
    return (TRUE);
  }
  pe->PredFlags |= TabledPredFlag;
  new_subgoal_trie_node(sg_node, 0, NULL, NULL, NULL);
  new_table_entry(te, sg_node);
  pe->TableOfPred = te;
  return (TRUE);
}


static
int p_do_tabling_mode(void) {
  Term t, mod, s;
  PredEntry *pe;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  }
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor func = FunctorOfTerm(t);
    pe = RepPredProp(PredPropByFunc(func, mod));
  } else {
    return (FALSE);
  }
  s = Deref(ARG3);
  if (IsVarTerm(s)) {
    Term sa;
    if (pe->PredFlags & LocalSchedPredFlag) {
      sa = MkAtomTerm(Yap_LookupAtom("local"));
    } else { 
      sa = MkAtomTerm(Yap_LookupAtom("batched"));
    }
    Bind((CELL *)s, sa);
    return(TRUE);
  }
  if (IsAtomTerm(s)) {
    char *sa;
    sa = RepAtom(AtomOfTerm(s))->StrOfAE;
    if (strcmp(sa, "local") == 0) {
      pe->PredFlags |= LocalSchedPredFlag;
      return(TRUE);
    } 
    if (strcmp(sa,"batched") == 0) {
      pe->PredFlags &= ~LocalSchedPredFlag;
      return(TRUE);
    }
  }
  return (FALSE);
}


static
int p_do_abolish_trie(void) {
  Term t, mod;
  tab_ent_ptr tab_ent;
  sg_hash_ptr hash;
  sg_node_ptr sg_node;
  UInt arity;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  } 
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    tab_ent = RepPredProp(PredPropByAtom(at, mod))->TableOfPred;
    arity = 0;
  } else if (IsApplTerm(t)) {
    Functor func = FunctorOfTerm(t);
    tab_ent = RepPredProp(PredPropByFunc(func, mod))->TableOfPred;
    arity = ArityOfFunctor(func);
  } else {
    return (FALSE);
  }
  hash = TabEnt_hash_chain(tab_ent);
  TabEnt_hash_chain(tab_ent) = NULL;
  free_subgoal_hash_chain(hash);
  sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));
  if (sg_node) {
    TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
    free_subgoal_trie_branch(sg_node, arity);
  }
  return (TRUE);
}


static
int p_do_show_trie(void) {
  Term t1, mod;
  PredEntry *pe;
  Atom at;
  UInt arity;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  } 
  t1 = Deref(ARG1);
  if (IsAtomTerm(t1)) {
    at = AtomOfTerm(t1);
    pe = RepPredProp(PredPropByAtom(at, mod));
    arity = 0;
  } else if (IsApplTerm(t1)) {
    Functor func = FunctorOfTerm(t1);
    at = NameOfFunctor(func);
    pe = RepPredProp(PredPropByFunc(func, mod));
    arity = ArityOfFunctor(func);
  } else {
    return (FALSE);
  }
  traverse_trie(TrNode_child(TabEnt_subgoal_trie(pe->TableOfPred)), arity, at, TRUE);
  return (TRUE);
}


static
int p_do_show_trie_stats(void) {
  Term t, mod;
  PredEntry *pe;
  Atom at;
  UInt arity;

  mod = Deref(ARG2);
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return (FALSE);
  } 
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
    arity = 0;
  } else if (IsApplTerm(t)) {
    Functor func = FunctorOfTerm(t);
    at = NameOfFunctor(func);
    pe = RepPredProp(PredPropByFunc(func, mod));
    arity = ArityOfFunctor(func);
  } else {
    return(FALSE);
  }
  traverse_trie(TrNode_child(TabEnt_subgoal_trie(pe->TableOfPred)), arity, at, FALSE);
  return (TRUE);
}
#endif /* TABLING */


#ifdef STATISTICS
static
int p_show_frames_stats(void) {
  long cont, pages;
  pg_hd_ptr pg_hd;
  void *str_ptr;

  fprintf(stdout, "[\n");
  pages = 0;

#ifdef YAPOR
  /* show or frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_or_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_or_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) OrFr_next((or_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Or frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_or_fr) - Pg_str_in_use(GLOBAL_PAGES_or_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_or_fr) == 1) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_or_fr), Pg_str_alloc(GLOBAL_PAGES_or_fr), 
          Pg_str_in_use(GLOBAL_PAGES_or_fr), cont, Pg_requests(GLOBAL_PAGES_or_fr));

  /* show query goal solution frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_qg_sol_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_sol_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) SolFr_next((qg_sol_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Query goal solution frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_qg_sol_fr) - Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) == 0) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_qg_sol_fr), Pg_str_alloc(GLOBAL_PAGES_qg_sol_fr), 
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr), cont, Pg_requests(GLOBAL_PAGES_qg_sol_fr));

  /* show query goal answer frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_qg_ans_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_ans_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) AnsFr_next((qg_ans_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Query goal answer frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_qg_ans_fr) - Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) == 0) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_qg_ans_fr), Pg_str_alloc(GLOBAL_PAGES_qg_ans_fr), 
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr), cont, Pg_requests(GLOBAL_PAGES_qg_ans_fr));
#endif /* YAPOR */

#ifdef TABLING_INNER_CUTS
  /* show table subgoal solution frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_tg_sol_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_sol_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) SolFr_next((tg_sol_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Table subgoal solution frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_tg_sol_fr) - Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) == 0) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tg_sol_fr), Pg_str_alloc(GLOBAL_PAGES_tg_sol_fr), 
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr), cont, Pg_requests(GLOBAL_PAGES_tg_sol_fr));

  /* show table subgoal answer frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_tg_ans_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_ans_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) AnsFr_next((tg_ans_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Table subgoal answer frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_tg_ans_fr) - Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) == 0) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tg_ans_fr), Pg_str_alloc(GLOBAL_PAGES_tg_ans_fr), 
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr), cont, Pg_requests(GLOBAL_PAGES_tg_ans_fr));
#endif /* TABLING_INNER_CUTS */

#ifdef TABLING
  /* show table entries */
  pages += Pg_pg_alloc(GLOBAL_PAGES_tab_ent);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_tab_ent);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) TabEnt_next((tab_ent_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Table entries: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_tab_ent) - Pg_str_in_use(GLOBAL_PAGES_tab_ent) == cont) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tab_ent), Pg_str_alloc(GLOBAL_PAGES_tab_ent), 
          Pg_str_in_use(GLOBAL_PAGES_tab_ent), cont, Pg_requests(GLOBAL_PAGES_tab_ent));

  /* show subgoal frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_sg_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) SgFr_next((sg_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Subgoal frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_sg_fr) - Pg_str_in_use(GLOBAL_PAGES_sg_fr) == cont) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_fr), Pg_str_alloc(GLOBAL_PAGES_sg_fr), 
          Pg_str_in_use(GLOBAL_PAGES_sg_fr), cont, Pg_requests(GLOBAL_PAGES_sg_fr));

  /* show subgoal trie nodes */
  pages += Pg_pg_alloc(GLOBAL_PAGES_sg_node);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_node);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) TrNode_next((sg_node_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Subgoal trie nodes: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_sg_node) - Pg_str_in_use(GLOBAL_PAGES_sg_node) == cont) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_node), Pg_str_alloc(GLOBAL_PAGES_sg_node), 
          Pg_str_in_use(GLOBAL_PAGES_sg_node), cont, Pg_requests(GLOBAL_PAGES_sg_node));

  /* show answer trie nodes */
  pages += Pg_pg_alloc(GLOBAL_PAGES_ans_node);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_node);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) TrNode_next((ans_node_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Answer trie nodes: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_ans_node) - Pg_str_in_use(GLOBAL_PAGES_ans_node) == cont) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_ans_node), Pg_str_alloc(GLOBAL_PAGES_ans_node), 
          Pg_str_in_use(GLOBAL_PAGES_ans_node), cont, Pg_requests(GLOBAL_PAGES_ans_node));

  /* show subgoal hashes */
  pages += Pg_pg_alloc(GLOBAL_PAGES_sg_hash);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_hash);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) Hash_next((sg_hash_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Subgoal hashes: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_sg_hash) - Pg_str_in_use(GLOBAL_PAGES_sg_hash) == cont) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_hash), Pg_str_alloc(GLOBAL_PAGES_sg_hash), 
          Pg_str_in_use(GLOBAL_PAGES_sg_hash), cont, Pg_requests(GLOBAL_PAGES_sg_hash));

  /* show answer hashes */
  pages += Pg_pg_alloc(GLOBAL_PAGES_ans_hash);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_hash);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) Hash_next((ans_hash_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Answer hashes: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_ans_hash) - Pg_str_in_use(GLOBAL_PAGES_ans_hash) == cont &&
          Pg_pg_alloc(GLOBAL_PAGES_ans_hash) == 0) ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_ans_hash), Pg_str_alloc(GLOBAL_PAGES_ans_hash), 
          Pg_str_in_use(GLOBAL_PAGES_ans_hash), cont, Pg_requests(GLOBAL_PAGES_ans_hash));

  /* show dependency frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_dep_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_dep_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) DepFr_next((dep_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Dependency frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_dep_fr) - Pg_str_in_use(GLOBAL_PAGES_dep_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) == 1) ? " ": "*", 
          Pg_pg_alloc(GLOBAL_PAGES_dep_fr), Pg_str_alloc(GLOBAL_PAGES_dep_fr), 
          Pg_str_in_use(GLOBAL_PAGES_dep_fr), cont, Pg_requests(GLOBAL_PAGES_dep_fr));
#endif /* TABLING */

#if defined(YAPOR) && defined(TABLING)
  /* show suspension frames */
  pages += Pg_pg_alloc(GLOBAL_PAGES_susp_fr);
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_susp_fr);
  while (pg_hd) {
    str_ptr = PgHd_free_str(pg_hd);
    while (str_ptr) {
      cont++;
      str_ptr = (void *) SuspFr_next((susp_fr_ptr)str_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Suspension frames: Alloc %ld - In Use %ld - Free %ld (%ld Accesses)\n",
          (Pg_str_alloc(GLOBAL_PAGES_susp_fr) - Pg_str_in_use(GLOBAL_PAGES_susp_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_susp_fr) == 0) ? " ": "*",  
          Pg_pg_alloc(GLOBAL_PAGES_susp_fr), Pg_str_alloc(GLOBAL_PAGES_susp_fr), 
          Pg_str_in_use(GLOBAL_PAGES_susp_fr), cont, Pg_requests(GLOBAL_PAGES_susp_fr));
#endif /* YAPOR && TABLING */

  /* show pages */
  cont = 0;
  pg_hd = Pg_free_pg(GLOBAL_PAGES_void);
  while (pg_hd) {
    cont++;
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(stdout, " %s[%ld] Pages: In Use %ld - Free %ld (%ld Accesses)\n]\n\n",
          (Pg_str_alloc(GLOBAL_PAGES_void) - Pg_str_in_use(GLOBAL_PAGES_void) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_void) == pages) ? " ": "*", 
          Pg_str_alloc(GLOBAL_PAGES_void), 
          Pg_str_in_use(GLOBAL_PAGES_void), cont, Pg_requests(GLOBAL_PAGES_void));

  return (TRUE);
}
#endif /* STATISTICS */


#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
static
int p_debug_prolog(void) {
  Term t;
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
#ifdef YAPOR_ERRORS
    fprintf(stdout, "W%d: %s\n", worker_id, s);
#else /* TABLING_ERRORS */
    fprintf(stdout, "%s\n", s);
#endif /* YAPOR_ERRORS */
    return(TRUE);
  } else {
    return (FALSE);
  }
}
#endif /* YAPOR_ERRORS || TABLING_ERRORS */
#endif /* YAPOR || TABLING */
