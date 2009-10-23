/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.preds.c
  version:     $Id: opt.preds.c,v 1.29 2008/04/11 16:26:19 ricroc Exp $   
                                                                     
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
#include "YapHeap.h"
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
static Int p_yapor_on(void);
static Int p_start_yapor(void);
static Int p_sequential(void);
static Int p_default_sequential(void);
static Int p_execution_mode(void);
static Int p_performance(void);
static Int p_parallel_new_answer(void);
static Int p_parallel_yes_answer(void);
static int parallel_new_answer_putchar(int sno, int ch);
static void show_answers(void);
static void answer_to_stdout(char *answer);
static Int p_or_statistics(void);
#endif /* YAPOR */

#ifdef TABLING
static Int p_freeze(void);
static Int p_wake(void);
static Int p_table(void);
static Int p_tabling_mode(void);
static Int p_abolish_table(void);
static Int p_abolish_all_tables(void);
static Int p_show_tabled_predicates(void);
static Int p_show_table(void);
static Int p_show_all_tables(void);
#ifdef GLOBAL_TRIE
static Int p_show_global_trie(void);
#endif /* GLOBAL_TRIE */
static Int p_table_statistics(void);
static Int p_tabling_statistics(void);
#endif /* TABLING */

#if defined(YAPOR) && defined(TABLING)
static int p_opt_statistics(void);
#endif /* YAPOR && TABLING */

#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
static int p_debug_prolog(void);
#endif /* YAPOR_ERRORS || TABLING_ERRORS */

#ifdef SHM_MEMORY_ALLOC_SCHEME
static void shm_pages(long pages_in_use, long bytes_in_use);
#ifdef YAPOR
static void shm_or_frames(long *pages_in_use, long *bytes_in_use);
static void shm_query_goal_solution_frames(long *pages_in_use, long *bytes_in_use);
static void shm_query_goal_answer_frames(long *pages_in_use, long *bytes_in_use);
#endif /* YAPOR */
#ifdef TABLING_INNER_CUTS
static void shm_table_subgoal_solution_frames(long *pages_in_use, long *bytes_in_use);
static void shm_table_subgoal_answer_frames(long *pages_in_use, long *bytes_in_use);
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
static void shm_table_entries(long *pages_in_use, long *bytes_in_use);
static void shm_subgoal_frames(long *pages_in_use, long *bytes_in_use);
static void shm_subgoal_trie_nodes(long *pages_in_use, long *bytes_in_use);
static void shm_answer_trie_nodes(long *pages_in_use, long *bytes_in_use);
static void shm_subgoal_trie_hashes(long *pages_in_use, long *bytes_in_use);
static void shm_answer_trie_hashes(long *pages_in_use, long *bytes_in_use);
static void shm_dependency_frames(long *pages_in_use, long *bytes_in_use);
#ifdef GLOBAL_TRIE
static void shm_global_trie_nodes(long *pages_in_use, long *bytes_in_use);
static void shm_global_trie_hashes(long *pages_in_use, long *bytes_in_use);
#endif /* GLOBAL_TRIE */
#endif /* TABLING */
#if defined(YAPOR) && defined(TABLING)
static void shm_suspension_frames(long *pages_in_use, long *bytes_in_use);
#endif /* YAPOR && TABLING */
#endif /* SHM_MEMORY_ALLOC_SCHEME */



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
  Yap_InitCPred("or_statistics", 0, p_or_statistics, SafePredFlag|SyncPredFlag);
#endif /* YAPOR */
#ifdef TABLING
  Yap_InitCPred("freeze_choice_point", 1, p_freeze, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("wake_choice_point", 1, p_wake, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_table", 2, p_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_tabling_mode", 3, p_tabling_mode, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_abolish_table", 2, p_abolish_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("abolish_all_tables", 0, p_abolish_all_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_tabled_predicates", 0, p_show_tabled_predicates, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_show_table", 2, p_show_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("show_all_tables", 0, p_show_all_tables, SafePredFlag|SyncPredFlag);
#ifdef GLOBAL_TRIE
  Yap_InitCPred("show_global_trie", 0, p_show_global_trie, SafePredFlag|SyncPredFlag);
#endif /* GLOBAL_TRIE */
  Yap_InitCPred("$c_table_statistics", 2, p_table_statistics, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("tabling_statistics", 0, p_tabling_statistics, SafePredFlag|SyncPredFlag);
#endif /* TABLING */
#if defined(YAPOR) && defined(TABLING)
  Yap_InitCPred("opt_statistics", 0, p_opt_statistics, SafePredFlag|SyncPredFlag);
#endif /* YAPOR && TABLING */
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
Int p_yapor_on(void) {
  return (PARALLEL_EXECUTION_MODE);
}


static
Int p_start_yapor(void) {
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
  GLOBAL_performance_mode &= ~PERFORMANCE_IN_EXECUTION;
  GLOBAL_execution_time = current_time();
  BITMAP_clear(GLOBAL_bm_finished_workers);
  PUT_IN_EXECUTING(worker_id);
  return (TRUE);
}


static
Int p_sequential(void) {
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
Int p_default_sequential(void) {
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
Int p_execution_mode(void) {
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
Int p_performance(void) {
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
    fprintf(Yap_stdout, "[\n  Best execution times:\n");
    for (i = 1; i <= GLOBAL_number_goals; i++) {
      fprintf(Yap_stdout, "    %d. time: %f seconds", i, GLOBAL_best_times(i));  
      if (one_worker_execution_time != 0)
        fprintf(Yap_stdout, " --> speedup %f (%6.2f %% )\n",
                one_worker_execution_time / GLOBAL_best_times(i),
                one_worker_execution_time / GLOBAL_best_times(i) / number_workers * 100 );
      else fprintf(Yap_stdout, "\n");
    }

    fprintf(Yap_stdout, "  Average             : %f seconds",
            GLOBAL_best_times(0) / GLOBAL_number_goals);
    if (one_worker_execution_time != 0)
      fprintf(Yap_stdout, " --> speedup %f (%6.2f %% )",
              one_worker_execution_time * GLOBAL_number_goals / GLOBAL_best_times(0),
              one_worker_execution_time * GLOBAL_number_goals / GLOBAL_best_times(0) / number_workers * 100 );

    if (GLOBAL_number_goals >= 3) {
      fprintf(Yap_stdout, "\n  Average (best three): %f seconds",
              (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)) / 3);
      if (one_worker_execution_time != 0)
        fprintf(Yap_stdout, " --> speedup %f (%6.2f %% ) ]\n\n",
                one_worker_execution_time * 3 / (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)),
                one_worker_execution_time * 3 / (GLOBAL_best_times(1) + GLOBAL_best_times(2) + GLOBAL_best_times(3)) / number_workers * 100 );
      else fprintf(Yap_stdout, "\n]\n\n");
    } else fprintf(Yap_stdout, "\n]\n\n");
    return (TRUE);
  }
  return (FALSE);
}


static
Int p_parallel_new_answer(void) {
  or_fr_ptr leftmost_or_fr;

  length_answer = 0;
  ALLOC_QG_ANSWER_FRAME(actual_answer);
  Yap_plwrite(ARG1, parallel_new_answer_putchar, 4, 1200);
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
Int p_parallel_yes_answer(void) {
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
      fprintf(Yap_stderr, "[ yes");
      break;
    case NO_ANSWER:  
      fprintf(Yap_stderr, "[ no");
      break;
    case 1:
      fprintf(Yap_stderr, "[ 1 answer found");
      break;
    default:
         fprintf(Yap_stderr, "[ %d answers found", GLOBAL_answers);
      break;
  }
  fprintf(Yap_stderr, " (in %f seconds) ]\n\n", GLOBAL_execution_time);

  if (GLOBAL_performance_mode == PERFORMANCE_ON) {
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
  fprintf(Yap_stderr, "  %s\n", output);
  return;
}


static
Int p_or_statistics(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
  long pages_in_use = 0, bytes_in_use = 0;

  shm_or_frames(&pages_in_use, &bytes_in_use);
  shm_query_goal_solution_frames(&pages_in_use, &bytes_in_use);
  shm_query_goal_answer_frames(&pages_in_use, &bytes_in_use);
  shm_pages(pages_in_use, bytes_in_use);
#else
  long bytes_in_use = 0;

  fprintf(Yap_stdout, "%s Or-frames:                     %10ld structs in use\n", 
          Pg_str_in_use(GLOBAL_PAGES_or_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_or_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
  fprintf(Yap_stdout, "%s Query goal solution frames:    %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) == 0 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
  fprintf(Yap_stdout, "%s Query goal answer frames:      %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) == 0 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
  fprintf(Yap_stdout, "  Total memory in use:                    %10ld bytes\n", bytes_in_use);
#endif /* MEMORY_ALLOC_SCHEME */
  return (TRUE);
}
#endif /* YAPOR */


#ifdef TABLING
static Int p_freeze(void) {
  Term term_arg, term_cp;

  term_arg = Deref(ARG1);
  if (IsVarTerm(term_arg)) {
    choiceptr cp = freeze_current_cp();
    term_cp = MkIntegerTerm((Int) cp);
    return Yap_unify(ARG1, term_cp);
  }
  return (FALSE);
}


static Int p_wake(void) {
  Term term_arg;

  term_arg = Deref(ARG1);
  if (IsIntegerTerm(term_arg)) {
    choiceptr cp = (choiceptr) IntegerOfTerm(term_arg);
    resume_frozen_cp(cp);
  }
  return (FALSE);
}


static
Int p_table(void) {
  Term mod, t;
  PredEntry *pe;
  Atom at;
  int arity;
  tab_ent_ptr tab_ent;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t)) {
    at = AtomOfTerm(t);
    pe = RepPredProp(PredPropByAtom(at, mod));
    arity = 0;
  } else if (IsApplTerm(t)) {
    at = NameOfFunctor(FunctorOfTerm(t));
    pe = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod));
    arity = ArityOfFunctor(FunctorOfTerm(t));
  } else
    return (FALSE);
  if (pe->PredFlags & TabledPredFlag)
    return (TRUE);  /* predicate already tabled */
  if (pe->cs.p_code.FirstClause)
    return (FALSE);  /* predicate already compiled */
  pe->PredFlags |= TabledPredFlag;
  new_table_entry(tab_ent, pe, at, arity);
  if (IsMode_Local(yap_flags[TABLING_MODE_FLAG]))
    SetMode_Local(TabEnt_mode(tab_ent));
  if (IsMode_LoadAnswers(yap_flags[TABLING_MODE_FLAG]))
    SetMode_LoadAnswers(TabEnt_mode(tab_ent));
  pe->TableOfPred = tab_ent;
  return (TRUE);
}


static
Int p_tabling_mode(void) {
  Term mod, t, val;
  tab_ent_ptr tab_ent;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else
    return (FALSE);
  val = Deref(ARG3);
  if (IsVarTerm(val)) {
    Term mode;
    t = MkAtomTerm(AtomNil);
    if (IsDefaultMode_LoadAnswers(TabEnt_mode(tab_ent)))
      mode = MkAtomTerm(Yap_LookupAtom("load_answers"));
    else
      mode = MkAtomTerm(Yap_LookupAtom("exec_answers"));
    t = MkPairTerm(mode, t);
    if (IsDefaultMode_Local(TabEnt_mode(tab_ent)))
      mode = MkAtomTerm(Yap_LookupAtom("local"));
    else
      mode = MkAtomTerm(Yap_LookupAtom("batched"));
    t = MkPairTerm(mode, t);
    mode = MkAtomTerm(Yap_LookupAtom("default"));
    t = MkPairTerm(mode, t);
    t = MkPairTerm(t, MkAtomTerm(AtomNil));
    if (IsMode_LoadAnswers(TabEnt_mode(tab_ent)))
      mode = MkAtomTerm(Yap_LookupAtom("load_answers"));
    else
      mode = MkAtomTerm(Yap_LookupAtom("exec_answers"));
    t = MkPairTerm(mode, t);
    if (IsMode_Local(TabEnt_mode(tab_ent)))
      mode = MkAtomTerm(Yap_LookupAtom("local"));
    else
      mode = MkAtomTerm(Yap_LookupAtom("batched"));
    t = MkPairTerm(mode, t);
    Bind((CELL *)val, t);
    return(TRUE);
  }
  if (IsAtomTerm(val)) {
    char *str_val = RepAtom(AtomOfTerm(val))->StrOfAE;
    if (strcmp(str_val,"batched") == 0) {
      SetDefaultMode_Batched(TabEnt_mode(tab_ent));
      if (IsMode_SchedulingOff(yap_flags[TABLING_MODE_FLAG]))
	SetMode_Batched(TabEnt_mode(tab_ent));
      return(TRUE);
    } 
    if (strcmp(str_val,"local") == 0) {
      SetDefaultMode_Local(TabEnt_mode(tab_ent));
      if (IsMode_SchedulingOff(yap_flags[TABLING_MODE_FLAG]))
	SetMode_Local(TabEnt_mode(tab_ent));
      return(TRUE);
    }
    if (strcmp(str_val,"exec_answers") == 0) {
      SetDefaultMode_ExecAnswers(TabEnt_mode(tab_ent));
      if (IsMode_CompletedOff(yap_flags[TABLING_MODE_FLAG]))
	SetMode_ExecAnswers(TabEnt_mode(tab_ent));
      return(TRUE);
    } 
    if (strcmp(str_val,"load_answers") == 0) {
      SetDefaultMode_LoadAnswers(TabEnt_mode(tab_ent));
      if (IsMode_CompletedOff(yap_flags[TABLING_MODE_FLAG]))
	SetMode_LoadAnswers(TabEnt_mode(tab_ent));
      return(TRUE);
    }
  }
  return (FALSE);
}


static
Int p_abolish_table(void) {
  Term mod, t;
  tab_ent_ptr tab_ent;
  sg_hash_ptr hash;
  sg_node_ptr sg_node;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else
    return (FALSE);
  hash = TabEnt_hash_chain(tab_ent);
  TabEnt_hash_chain(tab_ent) = NULL;
  free_subgoal_trie_hash_chain(hash);
  sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));
  if (sg_node) {
    TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
#ifdef GLOBAL_TRIE
    free_subgoal_trie_branch(sg_node, TabEnt_arity(tab_ent), TRAVERSE_POSITION_FIRST);
#else
    free_subgoal_trie_branch(sg_node, TabEnt_arity(tab_ent), 0, TRAVERSE_POSITION_FIRST);
#endif /* GLOBAL_TRIE */
  }
  return (TRUE);
}


static
Int p_abolish_all_tables(void) {
  tab_ent_ptr tab_ent;
  sg_hash_ptr hash;
  sg_node_ptr sg_node;

  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    hash = TabEnt_hash_chain(tab_ent);
    TabEnt_hash_chain(tab_ent) = NULL;
    free_subgoal_trie_hash_chain(hash);
    sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));
    if (sg_node) {
      TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
#ifdef GLOBAL_TRIE
      free_subgoal_trie_branch(sg_node, TabEnt_arity(tab_ent), TRAVERSE_POSITION_FIRST);
#else
      free_subgoal_trie_branch(sg_node, TabEnt_arity(tab_ent), 0, TRAVERSE_POSITION_FIRST);
#endif /* GLOBAL_TRIE */
    }
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}


static
Int p_show_tabled_predicates(void) {
  tab_ent_ptr tab_ent;

  tab_ent = GLOBAL_root_tab_ent;
  fprintf(Yap_stdout, "Tabled predicates\n");
  if (tab_ent == NULL)
    fprintf(Yap_stdout, "  none\n");
  else
    while(tab_ent) {
      fprintf(Yap_stdout, "  %s/%d\n", AtomName(TabEnt_atom(tab_ent)), TabEnt_arity(tab_ent));
      tab_ent = TabEnt_next(tab_ent);
    }
  return (TRUE);
}


static
Int p_show_table(void) {
  Term mod, t;
  tab_ent_ptr tab_ent;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else
    return (FALSE);
  show_table(tab_ent, SHOW_MODE_STRUCTURE);
  return (TRUE);
}


static
Int p_show_all_tables(void) {
  tab_ent_ptr tab_ent;

  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    show_table(tab_ent, SHOW_MODE_STRUCTURE);
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}


#ifdef GLOBAL_TRIE
static
Int p_show_global_trie(void) {
  show_global_trie();
  return (TRUE);
}
#endif /* GLOBAL_TRIE */


static
Int p_table_statistics(void) {
  Term mod, t;
  tab_ent_ptr tab_ent;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else
    return (FALSE);
  show_table(tab_ent, SHOW_MODE_STATISTICS);
  return (TRUE);
}


static
Int p_tabling_statistics(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
  long pages_in_use = 0, bytes_in_use = 0;

  shm_table_entries(&pages_in_use, &bytes_in_use);
  shm_subgoal_frames(&pages_in_use, &bytes_in_use);
  shm_subgoal_trie_nodes(&pages_in_use, &bytes_in_use);
  shm_answer_trie_nodes(&pages_in_use, &bytes_in_use);
#ifdef GLOBAL_TRIE
  shm_global_trie_nodes(&pages_in_use, &bytes_in_use);
#endif /* GLOBAL_TRIE */
  shm_subgoal_trie_hashes(&pages_in_use, &bytes_in_use);
  shm_answer_trie_hashes(&pages_in_use, &bytes_in_use);
#ifdef GLOBAL_TRIE
  shm_global_trie_hashes(&pages_in_use, &bytes_in_use);
#endif /* GLOBAL_TRIE */
  shm_dependency_frames(&pages_in_use, &bytes_in_use);
  shm_pages(pages_in_use, bytes_in_use);
#else
  long bytes_in_use = 0;

  fprintf(Yap_stdout, "  Table entries:                 %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_tab_ent));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
  fprintf(Yap_stdout, "  Subgoal frames:                %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
  fprintf(Yap_stdout, "  Subgoal trie nodes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
  fprintf(Yap_stdout, "  Answer trie nodes:             %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_ans_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
#ifdef GLOBAL_TRIE
  fprintf(Yap_stderr, "  Global trie nodes:             %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_gt_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
#endif /* GLOBAL_TRIE */
  fprintf(Yap_stdout, "  Subgoal trie hashes:           %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
  fprintf(Yap_stdout, "  Answer trie hashes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_ans_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
#ifdef GLOBAL_TRIE
  fprintf(Yap_stderr, "  Global trie hashes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_gt_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
#endif /* GLOBAL_TRIE */
  fprintf(Yap_stdout, "%s Dependency frames:             %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_dep_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
  fprintf(Yap_stdout, "  Total memory in use:                    %10ld bytes\n", bytes_in_use);
#endif /* MEMORY_ALLOC_SCHEME */
  return (TRUE);
}
#endif /* TABLING */


#if defined(YAPOR) && defined(TABLING)
static
int p_opt_statistics(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
  long pages_in_use = 0, bytes_in_use = 0;

  shm_or_frames(&pages_in_use, &bytes_in_use);
  shm_query_goal_solution_frames(&pages_in_use, &bytes_in_use);
  shm_query_goal_answer_frames(&pages_in_use, &bytes_in_use);
#ifdef TABLING_INNER_CUTS
  shm_table_subgoal_solution_frames(&pages_in_use, &bytes_in_use);
  shm_table_subgoal_answer_frames(&pages_in_use, &bytes_in_use);
#endif /* TABLING_INNER_CUTS */
  shm_table_entries(&pages_in_use, &bytes_in_use);
  shm_subgoal_frames(&pages_in_use, &bytes_in_use);
  shm_subgoal_trie_nodes(&pages_in_use, &bytes_in_use);
  shm_answer_trie_nodes(&pages_in_use, &bytes_in_use);
#ifdef GLOBAL_TRIE
  shm_global_trie_nodes(&pages_in_use, &bytes_in_use);
#endif /* GLOBAL_TRIE */
  shm_subgoal_trie_hashes(&pages_in_use, &bytes_in_use);
  shm_answer_trie_hashes(&pages_in_use, &bytes_in_use);
#ifdef GLOBAL_TRIE
  shm_global_trie_hashes(&pages_in_use, &bytes_in_use);
#endif /* GLOBAL_TRIE */
  shm_dependency_frames(&pages_in_use, &bytes_in_use);
  shm_show_suspension_frames(&pages_in_use, &bytes_in_use);
  shm_pages(pages_in_use, bytes_in_use);
#else
  long bytes_in_use = 0;

  fprintf(Yap_stdout, "%s Or-frames:                     %10ld structs in use\n", 
          Pg_str_in_use(GLOBAL_PAGES_or_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_or_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
  fprintf(Yap_stdout, "%s Query goal solution frames:    %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
  fprintf(Yap_stdout, "%s Query goal answer frames:      %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
#ifdef TABLING_INNER_CUTS
  fprintf(Yap_stdout, "%s Table subgoal solution frames: %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) == 0 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
  fprintf(Yap_stdout, "%s Table subgoal answer frames:   %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) == 0 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
#endif /* TABLING_INNER_CUTS */
  fprintf(Yap_stdout, "  Table entries:                 %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_tab_ent));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
  fprintf(Yap_stdout, "  Subgoal frames:                %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
  fprintf(Yap_stdout, "  Subgoal trie nodes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
  fprintf(Yap_stdout, "  Answer trie nodes:             %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_ans_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
#ifdef GLOBAL_TRIE
  fprintf(Yap_stderr, "  Global trie nodes:             %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_gt_node));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
#endif /* GLOBAL_TRIE */
  fprintf(Yap_stdout, "  Subgoal trie hashes:           %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_sg_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
  fprintf(Yap_stdout, "  Answer trie hashes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_ans_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
#ifdef GLOBAL_TRIE
  fprintf(Yap_stderr, "  Global trie hashes:            %10ld structs in use\n", Pg_str_in_use(GLOBAL_PAGES_gt_hash));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
#endif /* GLOBAL_TRIE */
  fprintf(Yap_stdout, "%s Dependency frames:             %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) == 1 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_dep_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
  fprintf(Yap_stdout, "%s Suspension frames:             %10ld structs in use\n",
          Pg_str_in_use(GLOBAL_PAGES_susp_fr) == 0 ? " ": "*", Pg_str_in_use(GLOBAL_PAGES_susp_fr));
  bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame);
  fprintf(Yap_stdout, "  Total memory in use:                    %10ld bytes\n", bytes_in_use);
#endif /* MEMORY_ALLOC_SCHEME */
  return (TRUE);
}
#endif /* YAPOR && TABLING */


#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
static
int p_debug_prolog(void) {
  Term t;
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
#ifdef YAPOR_ERRORS
    fprintf(Yap_stderr, "W%d: %s\n", worker_id, s);
#else /* TABLING_ERRORS */
    fprintf(Yap_stderr, "%s\n", s);
#endif /* YAPOR_ERRORS */
    return(TRUE);
  } else {
    return (FALSE);
  }
}
#endif /* YAPOR_ERRORS || TABLING_ERRORS */



/* ----------------------------- **
**      Auxiliary functions      **
** ----------------------------- */

#ifdef SHM_MEMORY_ALLOC_SCHEME
static
void shm_pages(long pages_in_use, long bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_void);
  while (pg_hd) {
    cont++;
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Total memory in use:           %8ld pages          %10ld bytes\n",
          Pg_str_in_use(GLOBAL_PAGES_void) == pages_in_use && 
          Pg_pg_alloc(GLOBAL_PAGES_void) - pages_in_use == cont ? " ": "*", 
          Pg_str_in_use(GLOBAL_PAGES_void), bytes_in_use);
  fprintf(Yap_stdout, "  Total memory allocated:        %8ld pages          %10ld bytes\n",
          Pg_pg_alloc(GLOBAL_PAGES_void), Pg_pg_alloc(GLOBAL_PAGES_void) * Yap_page_size);
  return;
}


#ifdef YAPOR
static
void shm_or_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  or_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_or_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = OrFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Or-frames:                     %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_or_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_or_fr) == 1 ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_or_fr), Pg_str_in_use(GLOBAL_PAGES_or_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_or_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
  return;
}


static
void shm_query_goal_solution_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  qg_sol_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_sol_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Query goal solution frames:    %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_qg_sol_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) == 0 ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_qg_sol_fr), Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_qg_sol_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
  return;
}


static
void shm_query_goal_answer_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  qg_ans_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Query goal answer frames:      %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_qg_ans_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) == 0 ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_qg_ans_fr), Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_qg_ans_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
  return;
}
#endif /* YAPOR */


#ifdef TABLING_INNER_CUTS
static
void shm_table_subgoal_solution_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  tg_sol_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_sol_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Table subgoal solution frames: %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_tg_sol_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) == 0 ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tg_sol_fr), Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_tg_sol_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
  return;
}


static
void shm_table_subgoal_answer_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  tg_ans_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Table subgoal answer frames:   %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_tg_ans_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) == 0 ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tg_ans_fr), Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_tg_ans_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
  return;
}
#endif /* TABLING_INNER_CUTS */


#ifdef TABLING
static
void shm_table_entries(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  tab_ent_ptr aux_ptr;

  aux_ptr = GLOBAL_root_tab_ent;
  while(aux_ptr) {
    cont++;
    aux_ptr = TabEnt_next(aux_ptr);
  }
  pg_hd = Pg_free_pg(GLOBAL_PAGES_tab_ent);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TabEnt_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Table entries:                 %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_tab_ent) + Pg_str_in_use(GLOBAL_PAGES_tab_ent) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_tab_ent), Pg_str_in_use(GLOBAL_PAGES_tab_ent));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_tab_ent);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
  return;
}


static
void shm_subgoal_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  sg_fr_ptr aux_ptr;

#ifdef LIMIT_TABLING
  aux_ptr = GLOBAL_first_sg_fr;
  while(aux_ptr) {
    cont++;
    aux_ptr = SgFr_next(aux_ptr);
  }
#endif /* LIMIT_TABLING */
  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SgFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Subgoal frames:                %8ld pages %10ld structs in use\n",
#ifdef LIMIT_TABLING
          Pg_str_in_use(GLOBAL_PAGES_sg_fr) +
#endif /* LIMIT_TABLING */
          Pg_str_free(GLOBAL_PAGES_sg_fr) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_fr), Pg_str_in_use(GLOBAL_PAGES_sg_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_sg_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
  return;
}


static
void shm_subgoal_trie_nodes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  sg_node_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Subgoal trie nodes:            %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_sg_node) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_node), Pg_str_in_use(GLOBAL_PAGES_sg_node));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_sg_node);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
  return;
}


static
void shm_answer_trie_nodes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  ans_node_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Answer trie nodes:             %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_ans_node) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_ans_node), Pg_str_in_use(GLOBAL_PAGES_ans_node));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_ans_node);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
  return;
}


static
void shm_subgoal_trie_hashes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  sg_hash_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Subgoal trie hashes:           %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_sg_hash) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_sg_hash), Pg_str_in_use(GLOBAL_PAGES_sg_hash));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_sg_hash);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
  return;
}


static
void shm_answer_trie_hashes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  ans_hash_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Answer trie hashes:            %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_ans_hash) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_ans_hash), Pg_str_in_use(GLOBAL_PAGES_ans_hash));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_ans_hash);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
  return;
}


static
void shm_dependency_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  dep_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_dep_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = DepFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Dependency frames:             %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_dep_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) == 1 ? " ": "*", 
          Pg_pg_alloc(GLOBAL_PAGES_dep_fr), Pg_str_in_use(GLOBAL_PAGES_dep_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_dep_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
  return;
}


#ifdef GLOBAL_TRIE
static
void shm_global_trie_nodes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  gt_node_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_gt_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stderr, "%s Global trie nodes:             %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_gt_node) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_gt_node), Pg_str_in_use(GLOBAL_PAGES_gt_node));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_gt_node);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
  return;
}


static
void shm_global_trie_hashes(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  gt_hash_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_gt_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stderr, "%s Global trie hashes:            %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_gt_hash) == cont ? " ": "*",
          Pg_pg_alloc(GLOBAL_PAGES_gt_hash), Pg_str_in_use(GLOBAL_PAGES_gt_hash));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_gt_hash);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
  return;
}
#endif /* GLOBAL_TRIE */
#endif /* TABLING */


#if defined(YAPOR) && defined(TABLING)
static
void shm_suspension_frames(long *pages_in_use, long *bytes_in_use) {
  long cont = 0;
  pg_hd_ptr pg_hd;
  susp_fr_ptr aux_ptr;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_susp_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SuspFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  fprintf(Yap_stdout, "%s Suspension frames:             %8ld pages %10ld structs in use\n",
          Pg_str_free(GLOBAL_PAGES_susp_fr) == cont &&
          Pg_str_in_use(GLOBAL_PAGES_susp_fr) == 0 ? " ": "*",  
          Pg_pg_alloc(GLOBAL_PAGES_susp_fr), Pg_str_in_use(GLOBAL_PAGES_susp_fr));
  *pages_in_use += Pg_pg_alloc(GLOBAL_PAGES_susp_fr);
  *bytes_in_use += Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame);
  return;
}
#endif /* YAPOR && TABLING */
#endif /* SHM_MEMORY_ALLOC_SCHEME */
#endif /* YAPOR || TABLING */
