/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/************************************
**      Includes & Prototypes      **
************************************/

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

#ifdef TABLING
static Int p_freeze_choice_point(void);
static Int p_wake_choice_point(void);
static Int p_reset_frozen_choice_points(void);
static Int p_abolish_frozen_choice_points_until(void);
static Int p_abolish_frozen_choice_points_all(void);
static Int p_table(void);
static Int p_tabling_mode(void);
static Int p_abolish_table(void);
static Int p_abolish_all_tables(void);
static Int p_show_tabled_predicates(void);
static Int p_show_table(void);
static Int p_show_all_tables(void);
static Int p_show_global_trie(void);
static Int p_show_statistics_table(void);
static Int p_show_statistics_tabling(void);
static Int p_show_statistics_global_trie(void);
#endif /* TABLING */
static Int p_yapor_threads(void);
#ifdef YAPOR
static Int p_worker(void);
static Int p_yapor_on(void);
static Int p_start_yapor(void);
static Int p_default_sequential(void);
static Int p_execution_mode(void);
static Int p_performance(void);
static Int p_parallel_new_answer(void);
static Int p_parallel_yes_answer(void);
static Int p_show_statistics_or(void);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt(void);
#endif /* YAPOR && TABLING */
static Int p_get_optyap_statistics(void);

#ifdef YAPOR
static inline realtime current_time(void);
static inline int parallel_new_answer_putchar(int sno, int ch);
static inline void show_answers(void);
static inline void answer_to_stdout(char *answer);
#endif /* YAPOR */

#ifdef TABLING
static inline long show_statistics_table_entries(void);
static inline long show_statistics_subgoal_frames(void);
static inline long show_statistics_dependency_frames(void);
static inline long show_statistics_subgoal_trie_nodes(void);
static inline long show_statistics_answer_trie_nodes(void);
static inline long show_statistics_subgoal_trie_hashes(void);
static inline long show_statistics_answer_trie_hashes(void);
static inline long show_statistics_global_trie_nodes(void);
static inline long show_statistics_global_trie_hashes(void);
#endif /* TABLING */
#ifdef YAPOR
static inline long show_statistics_or_frames(void);
static inline long show_statistics_query_goal_solution_frames(void);
static inline long show_statistics_query_goal_answer_frames(void);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static inline long show_statistics_suspension_frames(void);
#ifdef TABLING_INNER_CUTS
static inline long show_statistics_table_subgoal_solution_frames(void);
static inline long show_statistics_table_subgoal_answer_frames(void);
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */



/************************************
**      Macros & Declarations      **
************************************/

#ifdef YAPOR
#define TIME_RESOLUTION 1000000
#define NO_ANSWER   0
#define YES_ANSWER -1
static int length_answer;
static qg_ans_fr_ptr actual_answer;
#endif /* YAPOR */



/*******************************
**      Global functions      **
*******************************/

void Yap_init_optyap_preds(void) {
#ifdef TABLING
  Yap_InitCPred("freeze_choice_point", 1, p_freeze_choice_point, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("wake_choice_point", 1, p_wake_choice_point, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("abolish_frozen_choice_points", 1, p_abolish_frozen_choice_points_until, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("abolish_frozen_choice_points", 0, p_abolish_frozen_choice_points_all, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_table", 2, p_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_tabling_mode", 3, p_tabling_mode, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_abolish_table", 2, p_abolish_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("abolish_all_tables", 0, p_abolish_all_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_tabled_predicates", 0, p_show_tabled_predicates, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_show_table", 2, p_show_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("show_all_tables", 0, p_show_all_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_global_trie", 0, p_show_global_trie, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_table_statistics", 2, p_show_statistics_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("tabling_statistics", 0, p_show_statistics_tabling, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("global_trie_statistics", 0, p_show_statistics_global_trie, SafePredFlag|SyncPredFlag);
#endif /* TABLING */
  Yap_InitCPred("$c_yapor_threads", 1, p_yapor_threads, SafePredFlag|SyncPredFlag|HiddenPredFlag);
#ifdef YAPOR
  Yap_InitCPred("$c_worker", 0, p_worker, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_yapor_on", 0, p_yapor_on, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_start_yapor", 0, p_start_yapor, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_default_sequential", 1, p_default_sequential, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("execution_mode", 1, p_execution_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("performance", 1, p_performance, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_parallel_new_answer", 1, p_parallel_new_answer, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_parallel_yes_answer", 0, p_parallel_yes_answer, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("or_statistics", 0, p_show_statistics_or, SafePredFlag|SyncPredFlag);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  Yap_InitCPred("opt_statistics", 0, p_show_statistics_opt, SafePredFlag|SyncPredFlag);
#endif /* YAPOR && TABLING */
  Yap_InitCPred("$c_get_optyap_statistics", 3, p_get_optyap_statistics, SafePredFlag|SyncPredFlag|HiddenPredFlag);
}


#ifdef YAPOR
void finish_yapor(void) {
  GLOBAL_execution_time = current_time() - GLOBAL_execution_time;
  show_answers();
  return;
}
#endif /* YAPOR */



/***********************************
**      Tabling C Predicates      **
***********************************/

#ifdef TABLING
static Int p_freeze_choice_point(void) {
  if (IsVarTerm(Deref(ARG1))) {
    Int offset = freeze_current_cp();
    return Yap_unify(ARG1, MkIntegerTerm(offset));
  }
  return (FALSE);
}


static Int p_wake_choice_point(void) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    wake_frozen_cp(IntegerOfTerm(term_offset));
  return (FALSE);
}


static Int p_abolish_frozen_choice_points_until(void) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    abolish_frozen_cps_until(IntegerOfTerm(term_offset));
  return (TRUE);
}


static Int p_abolish_frozen_choice_points_all(void) {
  abolish_frozen_cps_all();
  return (TRUE);
}


static Int p_table(void) {
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
  pe->TableOfPred = tab_ent;
  return (TRUE);
}


static Int p_tabling_mode(void) {
  Term mod, t, tvalue;
  tab_ent_ptr tab_ent;

  mod = Deref(ARG1);
  t = Deref(ARG2);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else
    return (FALSE);
  tvalue = Deref(ARG3);
  if (IsVarTerm(tvalue)) {
    t = TermNil;
    if (IsMode_LocalTrie(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLocalTrie), t);
    else if (IsMode_GlobalTrie(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomGlobalTrie), t);
    if (IsMode_ExecAnswers(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomExecAnswers), t);
    else if (IsMode_LoadAnswers(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLoadAnswers), t);
    if (IsMode_Batched(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomBatched), t);
    else if (IsMode_Local(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLocal), t);
    t = MkPairTerm(MkAtomTerm(AtomDefault), t);
    t = MkPairTerm(t, TermNil);
    if (IsMode_LocalTrie(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLocalTrie), t);
    else if (IsMode_GlobalTrie(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomGlobalTrie), t);
    if (IsMode_ExecAnswers(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomExecAnswers), t);
    else if (IsMode_LoadAnswers(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLoadAnswers), t);
    if (IsMode_Batched(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomBatched), t);
    else if (IsMode_Local(TabEnt_mode(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomLocal), t);
    Bind((CELL *) tvalue, t);
    return(TRUE);
  } else if (IsIntTerm(tvalue)) {
    Int value = IntOfTerm(tvalue);
    if (value == 1) {  /* batched */
      SetMode_Batched(TabEnt_flags(tab_ent));
      if (! IsMode_Local(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_Batched(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    } else if (value == 2) {  /* local */
      SetMode_Local(TabEnt_flags(tab_ent));
      if (! IsMode_Batched(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_Local(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    } else if (value == 3) {  /* exec_answers */
      SetMode_ExecAnswers(TabEnt_flags(tab_ent));
      if (! IsMode_LoadAnswers(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_ExecAnswers(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    } else if (value == 4) {  /* load_answers */
      SetMode_LoadAnswers(TabEnt_flags(tab_ent));
      if (! IsMode_ExecAnswers(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_LoadAnswers(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    } else if (value == 5) {  /* local_trie */
      SetMode_LocalTrie(TabEnt_flags(tab_ent));
      if (! IsMode_GlobalTrie(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_LocalTrie(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    } else if (value == 6) {  /* global_trie */
      SetMode_GlobalTrie(TabEnt_flags(tab_ent));
      if (! IsMode_LocalTrie(yap_flags[TABLING_MODE_FLAG])) {
	SetMode_GlobalTrie(TabEnt_mode(tab_ent));
	return(TRUE);
      }
    }    
  }
  return (FALSE);
}


static Int p_abolish_table(void) {
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
  free_subgoal_hash_chain(hash);
  sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));
  if (sg_node) {
    if (TabEnt_arity(tab_ent)) {
      TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
      free_subgoal_trie(sg_node, TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);
    } else {
      sg_fr_ptr sg_fr = UNTAG_SUBGOAL_LEAF_NODE(sg_node);
      FREE_ANSWER_TRIE_NODE(SgFr_answer_trie(sg_fr));
#ifdef LIMIT_TABLING
      remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
      FREE_SUBGOAL_FRAME(sg_fr);
    }
  }
  return (TRUE);
}


static Int p_abolish_all_tables(void) {
  tab_ent_ptr tab_ent;
  sg_hash_ptr hash;
  sg_node_ptr sg_node;

  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    hash = TabEnt_hash_chain(tab_ent);
    TabEnt_hash_chain(tab_ent) = NULL;
    free_subgoal_hash_chain(hash);
    sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));
    if (sg_node) {
      if (TabEnt_arity(tab_ent)) {
	TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
	free_subgoal_trie(sg_node, TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);
      } else {
	sg_fr_ptr sg_fr = UNTAG_SUBGOAL_LEAF_NODE(sg_node);
	FREE_ANSWER_TRIE_NODE(SgFr_answer_trie(sg_fr));
#ifdef LIMIT_TABLING
	remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
	FREE_SUBGOAL_FRAME(sg_fr);
      }
    }
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}


static Int p_show_tabled_predicates(void) {
  tab_ent_ptr tab_ent;

  tab_ent = GLOBAL_root_tab_ent;
  fprintf(Yap_stdout, "Tabled predicates\n");
  if (tab_ent == NULL)
    fprintf(Yap_stdout, "  NONE\n");
  else
    while(tab_ent) {
      fprintf(Yap_stdout, "  %s/%d\n", AtomName(TabEnt_atom(tab_ent)), TabEnt_arity(tab_ent));
      tab_ent = TabEnt_next(tab_ent);
    }
  return (TRUE);
}


static Int p_show_table(void) {
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


static Int p_show_all_tables(void) {
  tab_ent_ptr tab_ent;

  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    show_table(tab_ent, SHOW_MODE_STRUCTURE);
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}


static Int p_show_global_trie(void) {
  show_global_trie(SHOW_MODE_STRUCTURE);
  return (TRUE);
}


static Int p_show_statistics_table(void) {
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


static Int p_show_statistics_tabling(void) {
  long total_bytes = 0, aux_bytes;

  aux_bytes = 0;
  fprintf(Yap_stdout, "Execution data structures\n");
  aux_bytes += show_statistics_table_entries();
  aux_bytes += show_statistics_subgoal_frames();
  aux_bytes += show_statistics_dependency_frames();
  fprintf(Yap_stdout, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Local trie data structures\n");
  aux_bytes += show_statistics_subgoal_trie_nodes();
  aux_bytes += show_statistics_answer_trie_nodes();
  aux_bytes += show_statistics_subgoal_trie_hashes();
  aux_bytes += show_statistics_answer_trie_hashes();
  fprintf(Yap_stdout, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Global trie data structures\n");
  aux_bytes += show_statistics_global_trie_nodes();
  aux_bytes += show_statistics_global_trie_hashes();
  fprintf(Yap_stdout, "  Memory in use (III):             %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef SHM_MEMORY_ALLOC_SCHEME
  fprintf(Yap_stdout, "Total memory in use (I+II+III):    %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_PAGES_void));
  fprintf(Yap_stdout, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_PAGES_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_PAGES_void));
#else 
  fprintf(Yap_stdout, "Total memory in use (I+II+III):    %10ld bytes\n", total_bytes);
#endif /* SHM_MEMORY_ALLOC_SCHEME */

  return (TRUE);
}

static Int p_show_statistics_global_trie(void) {
  show_global_trie(SHOW_MODE_STATISTICS);
  return (TRUE);
}
#endif /* TABLING */


/*********************************
**      YapOr C Predicates      **
*********************************/

static Int p_yapor_threads(void) {
#if defined(YAPOR) && defined(THREADS)
  return Yap_unify(MkIntegerTerm(number_workers),ARG1);
#else
  return FALSE;
#endif
}


#ifdef YAPOR
static Int p_worker(void) {
  CurrentModule = USER_MODULE;
  P = GETWORK_FIRST_TIME;
  return TRUE;
}


static Int p_yapor_on(void) {
  return (PARALLEL_EXECUTION_MODE);
}


static Int p_start_yapor(void) {
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


static Int p_default_sequential(void) {
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


static Int p_execution_mode(void) {
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


static Int p_performance(void) {
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


static Int p_parallel_new_answer(void) {
  or_fr_ptr leftmost_or_fr;

  length_answer = 0;
  ALLOC_QG_ANSWER_FRAME(actual_answer);
  Yap_plwrite(ARG1, parallel_new_answer_putchar, 4, 1200);
  AnsFr_answer(actual_answer)[length_answer] = 0;
  AnsFr_next(actual_answer) = NULL;
  leftmost_or_fr = CUT_leftmost_or_frame();
  LOCK_OR_FRAME(leftmost_or_fr);
  if (Get_LOCAL_prune_request()) {
    UNLOCK_OR_FRAME(leftmost_or_fr);
    FREE_QG_ANSWER_FRAME(actual_answer);
  } else {
    CUT_store_answer(leftmost_or_fr, actual_answer);
    UNLOCK_OR_FRAME(leftmost_or_fr);
  }
  return (TRUE);
}


static Int p_parallel_yes_answer(void) {
  GLOBAL_answers = YES_ANSWER;
  return (TRUE);
}


static Int p_show_statistics_or(void) {
  long total_bytes = 0, aux_bytes;

  aux_bytes = 0;
  fprintf(Yap_stdout, "Execution data structures\n");
  aux_bytes += show_statistics_or_frames();
  fprintf(Yap_stdout, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Cut support data structures\n");
  aux_bytes += show_statistics_query_goal_solution_frames();
  aux_bytes += show_statistics_query_goal_answer_frames();
  fprintf(Yap_stdout, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef SHM_MEMORY_ALLOC_SCHEME
  fprintf(Yap_stdout, "Total memory in use (I+II+III):    %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_PAGES_void));
  fprintf(Yap_stdout, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_PAGES_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_PAGES_void));
#else 
  fprintf(Yap_stdout, "Total memory in use (I+II+III):    %10ld bytes\n", total_bytes);
#endif /* SHM_MEMORY_ALLOC_SCHEME */

  return (TRUE);
}
#endif /* YAPOR */



/**********************************
**      OPTYap C Predicates      **
**********************************/

#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt(void) {
  long total_bytes = 0, aux_bytes;

  aux_bytes = 0;
  fprintf(Yap_stdout, "Execution data structures\n");
  aux_bytes += show_statistics_table_entries();
  aux_bytes += show_statistics_subgoal_frames();
  aux_bytes += show_statistics_dependency_frames();
  aux_bytes += show_statistics_or_frames();
  aux_bytes += show_statistics_suspension_frames();
  fprintf(Yap_stdout, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Local trie data structures\n");
  aux_bytes += show_statistics_subgoal_trie_nodes();
  aux_bytes += show_statistics_answer_trie_nodes();
  aux_bytes += show_statistics_subgoal_trie_hashes();
  aux_bytes += show_statistics_answer_trie_hashes();
  fprintf(Yap_stdout, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Global trie data structures\n");
  aux_bytes += show_statistics_global_trie_nodes();
  aux_bytes += show_statistics_global_trie_hashes();
  fprintf(Yap_stdout, "  Memory in use (III):             %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  fprintf(Yap_stdout, "Cut support data structures\n");
  aux_bytes += show_statistics_query_goal_solution_frames();
  aux_bytes += show_statistics_query_goal_answer_frames();
#ifdef TABLING_INNER_CUTS
  aux_bytes += show_statistics_table_subgoal_solution_frames();
  aux_bytes += show_statistics_table_subgoal_answer_frames();
#endif /* TABLING_INNER_CUTS */
  fprintf(Yap_stdout, "  Memory in use (IV):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef SHM_MEMORY_ALLOC_SCHEME
  fprintf(Yap_stdout, "Total memory in use (I+II+III+IV): %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_PAGES_void));
  fprintf(Yap_stdout, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_PAGES_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_PAGES_void));
#else 
  fprintf(Yap_stdout, "Total memory in use (I+II+III+IV): %10ld bytes\n", total_bytes);
#endif /* SHM_MEMORY_ALLOC_SCHEME */

  return (TRUE);
}
#endif /* YAPOR && TABLING */


static Int p_get_optyap_statistics(void) {
  Term tbytes, tstructs;
  Int value, bytes = -1, structs = -1;

  value = IntOfTerm(Deref(ARG1));
  if (value == 0) {  /* total_memory */
    bytes = 0;
#ifdef TABLING
    bytes += Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
    bytes += Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
    bytes += Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
    bytes += Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
    bytes += Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
    bytes += Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
    bytes += Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
    bytes += Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
    bytes += Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
#endif /* TABLING */
#ifdef YAPOR
    bytes += Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
    bytes += Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
    bytes += Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
    bytes += Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame);
#ifdef TABLING_INNER_CUTS
    bytes += Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
    bytes += Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
#ifdef SHM_MEMORY_ALLOC_SCHEME
    structs = Pg_pg_alloc(GLOBAL_PAGES_void) * Yap_page_size;
#else
    structs = bytes;
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  }
#ifdef TABLING
  if (value == 1) {  /* table_entries */
    bytes = Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
    structs = Pg_str_in_use(GLOBAL_PAGES_tab_ent);
  }
  if (value == 2) {  /* subgoal_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_sg_fr);
  }
  if (value == 3) {  /* dependency_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_dep_fr);
  }
  if (value == 6) {  /* subgoal_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
    structs = Pg_str_in_use(GLOBAL_PAGES_sg_node);
  }
  if (value == 7) {  /* answer_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
    structs = Pg_str_in_use(GLOBAL_PAGES_ans_node);
  }
  if (value == 8) {  /* subgoal_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
    structs = Pg_str_in_use(GLOBAL_PAGES_sg_hash);
  }
  if (value == 9) {  /* answer_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
    structs = Pg_str_in_use(GLOBAL_PAGES_ans_hash);
  }
  if (value == 10) {  /* global_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
    structs = Pg_str_in_use(GLOBAL_PAGES_gt_node);
  }
  if (value == 11) {  /* global_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
    structs = Pg_str_in_use(GLOBAL_PAGES_gt_hash);
  }
#endif /* TABLING */
#ifdef YAPOR
  if (value == 4) {  /* or_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_or_fr);
  }
  if (value == 12) {  /* query_goal_solution_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr);
  }
  if (value == 13) {  /* query_goal_answer_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr);
  }
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  if (value == 5) {  /* suspension_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_susp_fr);
  }
#ifdef TABLING_INNER_CUTS
  if (value == 14) {  /* table_subgoal_solution_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr);
  }
  if (value == 15) {  /* table_subgoal_answer_frames */
    bytes = Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
    structs = Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr);
  }
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
  if (bytes == -1)
    return (FALSE);
  tbytes = Deref(ARG2);
  tstructs = Deref(ARG3);
  if (IsVarTerm(tbytes)) {
    Bind((CELL *) tbytes, MkIntTerm(bytes));
  } else if (IsIntTerm(tbytes) &&  IntOfTerm(tbytes) != bytes)
    return (FALSE);
  if (IsVarTerm(tstructs)) {
    Bind((CELL *) tstructs, MkIntTerm(structs));
  } else if (IsIntTerm(tstructs) &&  IntOfTerm(tstructs) != structs)
    return (FALSE);
  return (TRUE);
}



/******************************
**      Local functions      **
******************************/

#ifdef YAPOR
static inline realtime current_time(void) {
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

static inline int parallel_new_answer_putchar(int sno, int ch) {
  AnsFr_answer(actual_answer)[length_answer++] = ch;
  return ch;
}


static inline void show_answers(void) {
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


static inline void answer_to_stdout(char *answer) {
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
#endif /* YAPOR */


#ifdef TABLING
static inline long show_statistics_table_entries(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  tab_ent_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_tab_ent);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TabEnt_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_table_entries, Pg_str_free(GLOBAL_PAGES_tab_ent) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Table entries:                   %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry), Pg_pg_alloc(GLOBAL_PAGES_tab_ent), Pg_str_in_use(GLOBAL_PAGES_tab_ent));
#else
  fprintf(Yap_stdout, "  Table entries:                   %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry), Pg_str_in_use(GLOBAL_PAGES_tab_ent));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_tab_ent) * sizeof(struct table_entry);
}


static inline long show_statistics_subgoal_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SgFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_frames, Pg_str_free(GLOBAL_PAGES_sg_fr) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Subgoal frames:                  %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame), Pg_pg_alloc(GLOBAL_PAGES_sg_fr), Pg_str_in_use(GLOBAL_PAGES_sg_fr));
#else
  fprintf(Yap_stdout, "  Subgoal frames:                  %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame), Pg_str_in_use(GLOBAL_PAGES_sg_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_sg_fr) * sizeof(struct subgoal_frame);
}


static inline long show_statistics_dependency_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  dep_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_dep_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = DepFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_dependency_frames, Pg_str_free(GLOBAL_PAGES_dep_fr) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Dependency frames:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame), Pg_pg_alloc(GLOBAL_PAGES_dep_fr), Pg_str_in_use(GLOBAL_PAGES_dep_fr));
#else
  fprintf(Yap_stdout, "  Dependency frames:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame), Pg_str_in_use(GLOBAL_PAGES_dep_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_dep_fr) * sizeof(struct dependency_frame);
}


static inline long show_statistics_subgoal_trie_nodes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_trie_nodes, Pg_str_free(GLOBAL_PAGES_sg_node) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Subgoal trie nodes:              %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node), Pg_pg_alloc(GLOBAL_PAGES_sg_node), Pg_str_in_use(GLOBAL_PAGES_sg_node));
#else
  fprintf(Yap_stdout, "  Subgoal trie nodes:              %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node), Pg_str_in_use(GLOBAL_PAGES_sg_node));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_sg_node) * sizeof(struct subgoal_trie_node);
}


static inline long show_statistics_answer_trie_nodes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  ans_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_answer_trie_nodes, Pg_str_free(GLOBAL_PAGES_ans_node) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Answer trie nodes:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node), Pg_pg_alloc(GLOBAL_PAGES_ans_node), Pg_str_in_use(GLOBAL_PAGES_ans_node));
#else
  fprintf(Yap_stdout, "  Answer trie nodes:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node), Pg_str_in_use(GLOBAL_PAGES_ans_node));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_ans_node) * sizeof(struct answer_trie_node);
}


static inline long show_statistics_subgoal_trie_hashes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_sg_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_trie_hashes, Pg_str_free(GLOBAL_PAGES_sg_hash) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Subgoal trie hashes:             %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash), Pg_pg_alloc(GLOBAL_PAGES_sg_hash), Pg_str_in_use(GLOBAL_PAGES_sg_hash));
#else
  fprintf(Yap_stdout, "  Subgoal trie hashes:             %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash), Pg_str_in_use(GLOBAL_PAGES_sg_hash));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_sg_hash) * sizeof(struct subgoal_trie_hash);
}


static inline long show_statistics_answer_trie_hashes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  ans_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_ans_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_answer_trie_hashes, Pg_str_free(GLOBAL_PAGES_ans_hash) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Answer trie hashes:              %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash), Pg_pg_alloc(GLOBAL_PAGES_ans_hash), Pg_str_in_use(GLOBAL_PAGES_ans_hash));
#else
  fprintf(Yap_stdout, "  Answer trie hashes:              %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash), Pg_str_in_use(GLOBAL_PAGES_ans_hash));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_ans_hash) * sizeof(struct answer_trie_hash);
}


static inline long show_statistics_global_trie_nodes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  gt_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_gt_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_global_trie_nodes, Pg_str_free(GLOBAL_PAGES_gt_node) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Global trie nodes:               %10ld bytes (%ld pages and %ld structs in use)\n", 
	  Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node), Pg_pg_alloc(GLOBAL_PAGES_gt_node), Pg_str_in_use(GLOBAL_PAGES_gt_node));
#else
  fprintf(Yap_stdout, "  Global trie nodes:               %10ld bytes (%ld structs in use)\n", 
	  Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node), Pg_str_in_use(GLOBAL_PAGES_gt_node));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_gt_node) * sizeof(struct global_trie_node);
}


static inline long show_statistics_global_trie_hashes(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  gt_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_gt_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_global_trie_hashes, Pg_str_free(GLOBAL_PAGES_gt_hash) != cont);
#endif /* DEBUG_TABLING */
  fprintf(Yap_stdout, "  Global trie hashes:              %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash), Pg_pg_alloc(GLOBAL_PAGES_gt_hash), Pg_str_in_use(GLOBAL_PAGES_gt_hash));
#else
  fprintf(Yap_stdout, "  Global trie hashes:              %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash), Pg_str_in_use(GLOBAL_PAGES_gt_hash));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_gt_hash) * sizeof(struct global_trie_hash);
}
#endif /* TABLING */


#ifdef YAPOR
static inline long show_statistics_or_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  or_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_or_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = OrFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_or_frames, Pg_str_free(GLOBAL_PAGES_or_fr) != cont);
#endif /* DEBUG_YAPOR */
  fprintf(Yap_stdout, "  Or-frames:                       %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame), Pg_pg_alloc(GLOBAL_PAGES_or_fr), Pg_str_in_use(GLOBAL_PAGES_or_fr));
#else
  fprintf(Yap_stdout, "  Or-frames:                       %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame), Pg_str_in_use(GLOBAL_PAGES_or_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_or_fr) * sizeof(struct or_frame);
}


static inline long show_statistics_query_goal_solution_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  qg_sol_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_sol_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_query_goal_solution_frames, Pg_str_free(GLOBAL_PAGES_qg_sol_fr) != cont);
#endif /* DEBUG_YAPOR */
  fprintf(Yap_stdout, "  Query goal solution frames:      %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame), Pg_pg_alloc(GLOBAL_PAGES_qg_sol_fr), Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr));
#else
  fprintf(Yap_stdout, "  Query goal solution frames:      %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame), Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_qg_sol_fr) * sizeof(struct query_goal_solution_frame);
}


static inline long show_statistics_query_goal_answer_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  qg_ans_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_qg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_query_goal_answer_frames, Pg_str_free(GLOBAL_PAGES_qg_ans_fr) != cont);
#endif /* DEBUG_YAPOR */
  fprintf(Yap_stdout, "  Query goal answer frames:        %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame), Pg_pg_alloc(GLOBAL_PAGES_qg_ans__fr), Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr));
#else
  fprintf(Yap_stdout, "  Query goal answer frames:        %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame), Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
}
#endif /* YAPOR */


#if defined(YAPOR) && defined(TABLING)
static inline long show_statistics_suspension_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  susp_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_susp_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SuspFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_suspension_frames, Pg_str_free(GLOBAL_PAGES_susp_fr) != cont);
#endif /* DEBUG_OPTYAP */
  fprintf(Yap_stdout, "  Suspension frames:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame), Pg_pg_alloc(GLOBAL_PAGES_susp_fr), Pg_str_in_use(GLOBAL_PAGES_susp_fr));
#else
  fprintf(Yap_stdout, "  Suspension frames:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame), Pg_str_in_use(GLOBAL_PAGES_susp_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_susp_fr) * sizeof(struct suspension_frame);
}


#ifdef TABLING_INNER_CUTS
static inline long show_statistics_table_subgoal_solution_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  tg_sol_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_sol_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_table_subgoal_solution_frames, Pg_str_free(GLOBAL_PAGES_tg_sol_fr) != cont);
#endif /* DEBUG_OPTYAP */
  fprintf(Yap_stdout, "  Table subgoal solution frames:   %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame), Pg_pg_alloc(GLOBAL_PAGES_tg_sol_fr), Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr));
#else
  fprintf(Yap_stdout, "  Table subgoal solution frames:   %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame), Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
}


static inline long show_statistics_table_subgoal_answer_frames(void) {
#ifdef SHM_MEMORY_ALLOC_SCHEME
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  tg_ans_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_PAGES_tg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_table_subgoal_answer_frames, Pg_str_free(GLOBAL_PAGES_tg_ans_fr) != cont);
#endif /* DEBUG_OPTYAP */
  fprintf(Yap_stdout, "  Table subgoal answer frames:     %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame), Pg_pg_alloc(GLOBAL_PAGES_tg_ans_fr), Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr));
#else
  fprintf(Yap_stdout, "  Table subgoal answer frames:     %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame), Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr));
#endif /* SHM_MEMORY_ALLOC_SCHEME */
  return Pg_str_in_use(GLOBAL_PAGES_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
}
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
#endif /* YAPOR || TABLING */
