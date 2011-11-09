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
#include "Yatom.h"
#include "YapHeap.h"
#include "SWI-Prolog.h"
#ifdef YAPOR
#if HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */

#ifdef TABLING
static Int p_freeze_choice_point( USES_REGS1 );
static Int p_wake_choice_point( USES_REGS1 );
static Int p_abolish_frozen_choice_points_until( USES_REGS1 );
static Int p_abolish_frozen_choice_points_all( USES_REGS1 );
static Int p_table( USES_REGS1 );
static Int p_tabling_mode( USES_REGS1 );
static Int p_abolish_table( USES_REGS1 );
static Int p_abolish_all_tables( USES_REGS1 );
static Int p_abolish_all_local_tables( USES_REGS1 );
static Int p_show_tabled_predicates( USES_REGS1 );
static Int p_show_table( USES_REGS1 );
static Int p_show_all_tables( USES_REGS1 );
static Int p_show_all_local_tables( USES_REGS1 );
static Int p_show_global_trie( USES_REGS1 );
static Int p_show_statistics_table( USES_REGS1 );
static Int p_show_statistics_tabling( USES_REGS1 );
static Int p_show_statistics_global_trie( USES_REGS1 );
#endif /* TABLING */

#ifdef YAPOR
static Int p_parallel_mode( USES_REGS1 );
static Int p_yapor_start( USES_REGS1 );
static Int p_yapor_workers( USES_REGS1 );
static Int p_worker( USES_REGS1 );
static Int p_parallel_new_answer( USES_REGS1 );
static Int p_show_statistics_or( USES_REGS1 );
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt( USES_REGS1 );
#endif /* YAPOR && TABLING */
static Int p_get_optyap_statistics( USES_REGS1 );

#ifdef YAPOR
static inline realtime current_time(void);
static inline int parallel_new_answer_putchar(int sno, int ch);
static inline void show_answers(void);
static inline void answer_to_stdout(char *answer);
#endif /* YAPOR */

#ifdef TABLING
static inline long show_statistics_table_entries(IOSTREAM *out);
static inline long show_statistics_subgoal_frames(IOSTREAM *out);
static inline long show_statistics_dependency_frames(IOSTREAM *out);
static inline long show_statistics_subgoal_trie_nodes(IOSTREAM *out);
static inline long show_statistics_answer_trie_nodes(IOSTREAM *out);
static inline long show_statistics_subgoal_trie_hashes(IOSTREAM *out);
static inline long show_statistics_answer_trie_hashes(IOSTREAM *out);
static inline long show_statistics_global_trie_nodes(IOSTREAM *out);
static inline long show_statistics_global_trie_hashes(IOSTREAM *out);
#endif /* TABLING */
#ifdef YAPOR
static inline long show_statistics_or_frames(IOSTREAM *out);
static inline long show_statistics_query_goal_solution_frames(IOSTREAM *out);
static inline long show_statistics_query_goal_answer_frames(IOSTREAM *out);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static inline long show_statistics_suspension_frames(IOSTREAM *out);
#ifdef TABLING_INNER_CUTS
static inline long show_statistics_table_subgoal_solution_frames(IOSTREAM *out);
static inline long show_statistics_table_subgoal_answer_frames(IOSTREAM *out);
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */



/************************************
**      Macros & Declarations      **
************************************/

#ifdef YAPOR
#define TIME_RESOLUTION 1000000
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
  Yap_InitCPred("$c_table", 3, p_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_tabling_mode", 3, p_tabling_mode, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_abolish_table", 2, p_abolish_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("abolish_all_tables", 0, p_abolish_all_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("abolish_all_local_tables", 0, p_abolish_all_local_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_tabled_predicates", 1, p_show_tabled_predicates, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_show_table", 3, p_show_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("show_all_tables", 1, p_show_all_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_all_local_tables", 1, p_show_all_local_tables, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("show_global_trie", 1, p_show_global_trie, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_table_statistics", 3, p_show_statistics_table, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("tabling_statistics", 1, p_show_statistics_tabling, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("global_trie_statistics", 1, p_show_statistics_global_trie, SafePredFlag|SyncPredFlag);
#endif /* TABLING */
#ifdef YAPOR
  Yap_InitCPred("parallel_mode", 1, p_parallel_mode, SafePredFlag|SyncPredFlag);
  Yap_InitCPred("$c_yapor_start", 0, p_yapor_start, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_yapor_workers", 1, p_yapor_workers, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_worker", 0, p_worker, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$c_parallel_new_answer", 1, p_parallel_new_answer, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("or_statistics", 1, p_show_statistics_or, SafePredFlag|SyncPredFlag);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  Yap_InitCPred("opt_statistics", 1, p_show_statistics_opt, SafePredFlag|SyncPredFlag);
#endif /* YAPOR && TABLING */
  Yap_InitCPred("$c_get_optyap_statistics", 3, p_get_optyap_statistics, SafePredFlag|SyncPredFlag|HiddenPredFlag);
}


#ifdef YAPOR
void finish_yapor(void) {
  GLOBAL_execution_time = current_time() - GLOBAL_execution_time;
  GLOBAL_parallel_mode = PARALLEL_MODE_ON;
  /*  show_answers(); */
  return;
}
#endif /* YAPOR */



/***********************************
**      Tabling C Predicates      **
***********************************/

#ifdef TABLING
static Int p_freeze_choice_point( USES_REGS1 ) {
  if (IsVarTerm(Deref(ARG1))) {
    Int offset = freeze_current_cp();
    return Yap_unify(ARG1, MkIntegerTerm(offset));
  }
  return (FALSE);
}


static Int p_wake_choice_point( USES_REGS1 ) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    wake_frozen_cp(IntegerOfTerm(term_offset));
  return (FALSE);
}


static Int p_abolish_frozen_choice_points_until( USES_REGS1 ) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    abolish_frozen_cps_until(IntegerOfTerm(term_offset));
  return (TRUE);
}


static Int p_abolish_frozen_choice_points_all( USES_REGS1 ) {
  abolish_frozen_cps_all();
  return (TRUE);
}


static Int p_table( USES_REGS1 ) {
  Term mod, t, list;
  PredEntry *pe;
  Atom at;
  int arity;
  tab_ent_ptr tab_ent;
#ifdef MODE_DIRECTED_TABLING
  int* mode_directed = NULL;
#endif /* MODE_DIRECTED_TABLING */
  
  mod = Deref(ARG1);
  t = Deref(ARG2);
  list = Deref(ARG3);

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
  if (list != TermNil) {  /* non-empty list */
#ifndef MODE_DIRECTED_TABLING
    Yap_Error(INTERNAL_COMPILER_ERROR, TermNil, "invalid tabling declaration for %s/%d (mode directed tabling not enabled)", AtomName(at), arity);
    return(FALSE);
#else 
    int pos_index = 0;
    int pos_agreg = 0;  /* min/max */
    int pos_first = 0;
    int pos_all = 0;
    int pos_last = 0;
    int i;
    int *aux_mode_directed;

    aux_mode_directed = malloc(arity * sizeof(int));
    ALLOC_BLOCK(mode_directed, arity * sizeof(int), int);
    for (i = 0; i < arity; i++) {
      int mode = IntOfTerm(HeadOfTerm(list));
      if (mode == MODE_DIRECTED_INDEX)
	pos_index++;
      else if (mode == MODE_DIRECTED_ALL)
	pos_all++;
      else if (mode == MODE_DIRECTED_LAST)
	pos_last++;
      else if (mode == MODE_DIRECTED_MIN || mode == MODE_DIRECTED_MAX)
	pos_agreg++;
      aux_mode_directed[i] = mode;
      list = TailOfTerm(list);
    }    
    pos_first = pos_index + pos_agreg + pos_all + pos_last;
    pos_last = pos_index + pos_agreg + pos_all;
    pos_all = pos_index + pos_agreg;
    pos_agreg = pos_index;
    pos_index = 0;
    for (i = 0; i < arity; i++) {
      int aux_pos;
      if (aux_mode_directed[i] == MODE_DIRECTED_MAX)
	aux_pos = pos_agreg++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_MIN)
	aux_pos = pos_agreg++;	
      else if (aux_mode_directed[i] == MODE_DIRECTED_INDEX)
	aux_pos = pos_index++;	
      else if(aux_mode_directed[i] == MODE_DIRECTED_FIRST)
	aux_pos = pos_first++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_ALL)
	aux_pos = pos_all++;		
      else if (aux_mode_directed[i] == MODE_DIRECTED_LAST)
	aux_pos = pos_last++;	
      mode_directed[aux_pos] = MODE_DIRECTED_SET(i, aux_mode_directed[i]);
    }
    free(aux_mode_directed);
#endif /*MODE_DIRECTED_TABLING*/
  }
  if (pe->PredFlags & TabledPredFlag)
    return (TRUE);  /* predicate already tabled */
  if (pe->cs.p_code.FirstClause)
    return (FALSE);  /* predicate already compiled */
  pe->PredFlags |= TabledPredFlag;
  new_table_entry(tab_ent, pe, at, arity, mode_directed);
  pe->TableOfPred = tab_ent;
  return (TRUE);
}


static Int p_tabling_mode( USES_REGS1 ) {
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


static Int p_abolish_table( USES_REGS1 ) {
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
      free_subgoal_trie(sg_node, TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);
    } else {
      sg_fr_ptr sg_fr = UNTAG_SUBGOAL_LEAF_NODE(sg_node);
      FREE_ANSWER_TRIE_NODE(SgFr_answer_trie(sg_fr));
#ifdef LIMIT_TABLING
      remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
      FREE_SUBGOAL_FRAME(sg_fr);
    }
    TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
  }
  return (TRUE);
}


static Int p_abolish_all_tables( USES_REGS1 ) {
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
	free_subgoal_trie(sg_node, TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);
      } else {
	sg_fr_ptr sg_fr = UNTAG_SUBGOAL_LEAF_NODE(sg_node);
	FREE_ANSWER_TRIE_NODE(SgFr_answer_trie(sg_fr));
#ifdef LIMIT_TABLING
	remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
	FREE_SUBGOAL_FRAME(sg_fr);
      }
      TrNode_child(TabEnt_subgoal_trie(tab_ent)) = NULL;
    }
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}


static Int p_abolish_all_local_tables( USES_REGS1 ) {
#ifdef THREADS

#else
  p_abolish_all_tables();
#endif /* THREADS */
  return (TRUE);
}


static Int p_show_tabled_predicates( USES_REGS1 ) {
  IOSTREAM *out;
  tab_ent_ptr tab_ent;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  tab_ent = GLOBAL_root_tab_ent;
  Sfprintf(out, "Tabled predicates\n");
  if (tab_ent == NULL)
    Sfprintf(out, "  NONE\n");
  else while(tab_ent) {
    Sfprintf(out, "  %s/%d\n", AtomName(TabEnt_atom(tab_ent)), TabEnt_arity(tab_ent));
    tab_ent = TabEnt_next(tab_ent);
  }
  PL_release_stream(out);
  return (TRUE);
}


static Int p_show_table( USES_REGS1 ) {
  IOSTREAM *out;
  Term mod, t;
  tab_ent_ptr tab_ent;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  mod = Deref(ARG2);
  t = Deref(ARG3);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else {
    PL_release_stream(out);
    return (FALSE);
  }
  show_table(tab_ent, SHOW_MODE_STRUCTURE, out);
  PL_release_stream(out);
  return (TRUE);
}


static Int p_show_all_tables( USES_REGS1 ) {
  IOSTREAM *out;
  tab_ent_ptr tab_ent;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    show_table(tab_ent, SHOW_MODE_STRUCTURE, out);
    tab_ent = TabEnt_next(tab_ent);
  }
  PL_release_stream(out);
  return (TRUE);
}


static Int p_show_all_local_tables( USES_REGS1 ) {
#ifdef THREADS
  IOSTREAM *out;
  tab_ent_ptr tab_ent;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);  
  tab_ent = GLOBAL_root_tab_ent;
  while(tab_ent) {
    show_table(tab_ent, SHOW_MODE_STRUCTURE, out);
    tab_ent = TabEnt_next(tab_ent);
  }
  PL_release_stream(out);
#else
  p_show_all_tables();
#endif /* THREADS */
  return (TRUE);
}


static Int p_show_global_trie( USES_REGS1 ) {
  IOSTREAM *out;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  show_global_trie(SHOW_MODE_STRUCTURE, out);
  PL_release_stream(out);
  return (TRUE);
}


static Int p_show_statistics_table( USES_REGS1 ) {
  IOSTREAM *out;
  Term mod, t;
  tab_ent_ptr tab_ent;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  mod = Deref(ARG2);
  t = Deref(ARG3);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else {
    PL_release_stream(out);
    return (FALSE);
  }
  show_table(tab_ent, SHOW_MODE_STATISTICS, out);
  PL_release_stream(out);
  return (TRUE);
}


static Int p_show_statistics_tabling( USES_REGS1 ) {
  IOSTREAM *out;
  long total_bytes = 0, aux_bytes;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  aux_bytes = 0;
  Sfprintf(out, "Execution data structures\n");
  aux_bytes += show_statistics_table_entries(out);
  aux_bytes += show_statistics_subgoal_frames(out);
  aux_bytes += show_statistics_dependency_frames(out);
  Sfprintf(out, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Local trie data structures\n");
  aux_bytes += show_statistics_subgoal_trie_nodes(out);
  aux_bytes += show_statistics_answer_trie_nodes(out);
  aux_bytes += show_statistics_subgoal_trie_hashes(out);
  aux_bytes += show_statistics_answer_trie_hashes(out);
  Sfprintf(out, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Global trie data structures\n");
  aux_bytes += show_statistics_global_trie_nodes(out);
  aux_bytes += show_statistics_global_trie_hashes(out);
  Sfprintf(out, "  Memory in use (III):             %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef USE_PAGES_MALLOC
  Sfprintf(out, "Total memory in use (I+II+III):    %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_pages_void));
  Sfprintf(out, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_pages_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_pages_void));
#else 
  Sfprintf(out, "Total memory in use (I+II+III):    %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  PL_release_stream(out);
  return (TRUE);
}

static Int p_show_statistics_global_trie( USES_REGS1 ) {
  IOSTREAM *out;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  show_global_trie(SHOW_MODE_STATISTICS, out);
  PL_release_stream(out);
  return (TRUE);
}
#endif /* TABLING */


/*********************************
**      YapOr C Predicates      **
*********************************/

#ifdef YAPOR
static Int p_parallel_mode( USES_REGS1 ) {
  Term t;
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Term ta;
    if (GLOBAL_parallel_mode == PARALLEL_MODE_OFF) 
      ta = MkAtomTerm(Yap_LookupAtom("off"));
    else if (GLOBAL_parallel_mode == PARALLEL_MODE_ON) 
      ta = MkAtomTerm(Yap_LookupAtom("on"));
    else /* PARALLEL_MODE_RUNNING */
      ta = MkAtomTerm(Yap_LookupAtom("running"));
    Bind((CELL *)t, ta);
    return(TRUE);
  }
  if (IsAtomTerm(t) && GLOBAL_parallel_mode != PARALLEL_MODE_RUNNING) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
    if (strcmp(s,"on") == 0) {
      GLOBAL_parallel_mode = PARALLEL_MODE_ON;
      return(TRUE);
    }
    if (strcmp(s,"off") == 0) {
      GLOBAL_parallel_mode = PARALLEL_MODE_OFF;
      return(TRUE);
    }
    return(FALSE); /* PARALLEL_MODE_RUNNING */
  }
  return(FALSE);
}


static Int p_yapor_start( USES_REGS1 ) {
#ifdef TIMESTAMP_CHECK
  GLOBAL_timestamp = 0;
#endif /* TIMESTAMP_CHECK */
  BITMAP_delete(GLOBAL_bm_idle_workers, 0);
  BITMAP_clear(GLOBAL_bm_invisible_workers);
  BITMAP_clear(GLOBAL_bm_requestable_workers);
#ifdef TABLING_INNER_CUTS
  BITMAP_clear(GLOBAL_bm_pruning_workers);
#endif /* TABLING_INNER_CUTS */
  make_root_choice_point();
  GLOBAL_parallel_mode = PARALLEL_MODE_RUNNING;
  GLOBAL_execution_time = current_time();
  BITMAP_clear(GLOBAL_bm_finished_workers);
  PUT_IN_EXECUTING(worker_id);
  return (TRUE);
}


static Int p_yapor_workers( USES_REGS1 ) {
#ifdef YAPOR_THREADS
  return Yap_unify(MkIntegerTerm(GLOBAL_number_workers),ARG1);
#else
  return FALSE;
#endif /* YAPOR_THREADS */
}


static Int p_worker( USES_REGS1 ) {
  CurrentModule = USER_MODULE;
  P = GETWORK_FIRST_TIME;
  return TRUE;
}


static Int p_parallel_new_answer( USES_REGS1 ) {
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


static Int p_show_statistics_or( USES_REGS1 ) {
  IOSTREAM *out;
  long total_bytes = 0, aux_bytes;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  aux_bytes = 0;
  Sfprintf(out, "Execution data structures\n");
  aux_bytes += show_statistics_or_frames(out);
  Sfprintf(out, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Cut support data structures\n");
  aux_bytes += show_statistics_query_goal_solution_frames(out);
  aux_bytes += show_statistics_query_goal_answer_frames(out);
  Sfprintf(out, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef USE_PAGES_MALLOC
  Sfprintf(out, "Total memory in use (I+II):        %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_pages_void));
  Sfprintf(out, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_pages_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_pages_void));
#else 
  Sfprintf(out, "Total memory in use (I+II):        %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  PL_release_stream(out);
  return (TRUE);
}
#endif /* YAPOR */



/**********************************
**      OPTYap C Predicates      **
**********************************/

#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt( USES_REGS1 ) {
  IOSTREAM *out;
  long total_bytes = 0, aux_bytes;

  if (!PL_get_stream_handle(Yap_InitSlot(Deref(ARG1) PASS_REGS), &out))
    return (FALSE);
  aux_bytes = 0;
  Sfprintf(out, "Execution data structures\n");
  aux_bytes += show_statistics_table_entries(out);
  aux_bytes += show_statistics_subgoal_frames(out);
  aux_bytes += show_statistics_dependency_frames(out);
  aux_bytes += show_statistics_or_frames(out);
  aux_bytes += show_statistics_suspension_frames(out);
  Sfprintf(out, "  Memory in use (I):               %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Local trie data structures\n");
  aux_bytes += show_statistics_subgoal_trie_nodes(out);
  aux_bytes += show_statistics_answer_trie_nodes(out);
  aux_bytes += show_statistics_subgoal_trie_hashes(out);
  aux_bytes += show_statistics_answer_trie_hashes(out);
  Sfprintf(out, "  Memory in use (II):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Global trie data structures\n");
  aux_bytes += show_statistics_global_trie_nodes(out);
  aux_bytes += show_statistics_global_trie_hashes(out);
  Sfprintf(out, "  Memory in use (III):             %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
  aux_bytes = 0;
  Sfprintf(out, "Cut support data structures\n");
  aux_bytes += show_statistics_query_goal_solution_frames(out);
  aux_bytes += show_statistics_query_goal_answer_frames(out);
#ifdef TABLING_INNER_CUTS
  aux_bytes += show_statistics_table_subgoal_solution_frames(out);
  aux_bytes += show_statistics_table_subgoal_answer_frames(out);
#endif /* TABLING_INNER_CUTS */
  Sfprintf(out, "  Memory in use (IV):              %10ld bytes\n\n", aux_bytes);
  total_bytes += aux_bytes;
#ifdef USE_PAGES_MALLOC
  Sfprintf(out, "Total memory in use (I+II+III+IV): %10ld bytes (%ld pages in use)\n",
          total_bytes, Pg_str_in_use(GLOBAL_pages_void));
  Sfprintf(out, "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
          Pg_pg_alloc(GLOBAL_pages_void) * Yap_page_size, Pg_pg_alloc(GLOBAL_pages_void));
#else 
  Sfprintf(out, "Total memory in use (I+II+III+IV): %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  PL_release_stream(out);
  return (TRUE);
}
#endif /* YAPOR && TABLING */


static Int p_get_optyap_statistics( USES_REGS1 ) {
  Term tbytes, tstructs;
  Int value, bytes = -1, structs = -1;

  value = IntOfTerm(Deref(ARG1));
  if (value == 0) {  /* total_memory */
    bytes = 0;
#ifdef TABLING
    bytes += Pg_str_in_use(GLOBAL_pages_tab_ent) * sizeof(struct table_entry);
    bytes += Pg_str_in_use(GLOBAL_pages_sg_fr) * sizeof(struct subgoal_frame);
    bytes += Pg_str_in_use(GLOBAL_pages_dep_fr) * sizeof(struct dependency_frame);
    bytes += Pg_str_in_use(GLOBAL_pages_sg_node) * sizeof(struct subgoal_trie_node);
    bytes += Pg_str_in_use(GLOBAL_pages_ans_node) * sizeof(struct answer_trie_node);
    bytes += Pg_str_in_use(GLOBAL_pages_sg_hash) * sizeof(struct subgoal_trie_hash);
    bytes += Pg_str_in_use(GLOBAL_pages_ans_hash) * sizeof(struct answer_trie_hash);
    bytes += Pg_str_in_use(GLOBAL_pages_gt_node) * sizeof(struct global_trie_node);
    bytes += Pg_str_in_use(GLOBAL_pages_gt_hash) * sizeof(struct global_trie_hash);
#endif /* TABLING */
#ifdef YAPOR
    bytes += Pg_str_in_use(GLOBAL_pages_or_fr ) * sizeof(struct or_frame);
    bytes += Pg_str_in_use(GLOBAL_pages_qg_sol_fr ) * sizeof(struct query_goal_solution_frame);
    bytes += Pg_str_in_use(GLOBAL_pages_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
    bytes += Pg_str_in_use(GLOBAL_pages_susp_fr) * sizeof(struct suspension_frame);
#ifdef TABLING_INNER_CUTS
    bytes += Pg_str_in_use(GLOBAL_pages_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
    bytes += Pg_str_in_use(GLOBAL_pages_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
#ifdef USE_PAGES_MALLOC
    structs = Pg_pg_alloc(GLOBAL_pages_void) * Yap_page_size;
#else
    structs = bytes;
#endif /* USE_PAGES_MALLOC */
  }
#ifdef TABLING
  if (value == 1) {  /* table_entries */
    bytes = Pg_str_in_use(GLOBAL_pages_tab_ent) * sizeof(struct table_entry);
    structs = Pg_str_in_use(GLOBAL_pages_tab_ent);
  }
  if (value == 2) {  /* subgoal_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_sg_fr) * sizeof(struct subgoal_frame);
    structs = Pg_str_in_use(GLOBAL_pages_sg_fr);
  }
  if (value == 3) {  /* dependency_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_dep_fr) * sizeof(struct dependency_frame);
    structs = Pg_str_in_use(GLOBAL_pages_dep_fr);
  }
  if (value == 6) {  /* subgoal_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_pages_sg_node) * sizeof(struct subgoal_trie_node);
    structs = Pg_str_in_use(GLOBAL_pages_sg_node);
  }
  if (value == 7) {  /* answer_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_pages_ans_node) * sizeof(struct answer_trie_node);
    structs = Pg_str_in_use(GLOBAL_pages_ans_node);
  }
  if (value == 8) {  /* subgoal_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_pages_sg_hash) * sizeof(struct subgoal_trie_hash);
    structs = Pg_str_in_use(GLOBAL_pages_sg_hash);
  }
  if (value == 9) {  /* answer_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_pages_ans_hash) * sizeof(struct answer_trie_hash);
    structs = Pg_str_in_use(GLOBAL_pages_ans_hash);
  }
  if (value == 10) {  /* global_trie_nodes */
    bytes = Pg_str_in_use(GLOBAL_pages_gt_node) * sizeof(struct global_trie_node);
    structs = Pg_str_in_use(GLOBAL_pages_gt_node);
  }
  if (value == 11) {  /* global_trie_hashes */
    bytes = Pg_str_in_use(GLOBAL_pages_gt_hash) * sizeof(struct global_trie_hash);
    structs = Pg_str_in_use(GLOBAL_pages_gt_hash);
  }
#endif /* TABLING */
#ifdef YAPOR
  if (value == 4) {  /* or_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_or_fr ) * sizeof(struct or_frame);
    structs = Pg_str_in_use(GLOBAL_pages_or_fr );
  }
  if (value == 12) {  /* query_goal_solution_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_qg_sol_fr ) * sizeof(struct query_goal_solution_frame);
    structs = Pg_str_in_use(GLOBAL_pages_qg_sol_fr );
  }
  if (value == 13) {  /* query_goal_answer_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
    structs = Pg_str_in_use(GLOBAL_pages_qg_ans_fr);
  }
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  if (value == 5) {  /* suspension_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_susp_fr) * sizeof(struct suspension_frame);
    structs = Pg_str_in_use(GLOBAL_pages_susp_fr);
  }
#ifdef TABLING_INNER_CUTS
  if (value == 14) {  /* table_subgoal_solution_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
    structs = Pg_str_in_use(GLOBAL_pages_tg_sol_fr);
  }
  if (value == 15) {  /* table_subgoal_answer_frames */
    bytes = Pg_str_in_use(GLOBAL_pages_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
    structs = Pg_str_in_use(GLOBAL_pages_tg_ans_fr);
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
  CACHE_REGS
  int i, answers = 0;
  if (OrFr_qg_solutions(LOCAL_top_or_fr)) {
    qg_ans_fr_ptr aux_answer1, aux_answer2;
    aux_answer1 = SolFr_first(OrFr_qg_solutions(LOCAL_top_or_fr));
    while (aux_answer1) {
      answer_to_stdout(AnsFr_answer(aux_answer1));
      aux_answer2 = aux_answer1;
      aux_answer1 = AnsFr_next(aux_answer1);
      FREE_QG_ANSWER_FRAME(aux_answer2);
      answers++;
    }
    FREE_QG_SOLUTION_FRAME(OrFr_qg_solutions(LOCAL_top_or_fr));
    OrFr_qg_solutions(LOCAL_top_or_fr) = NULL;
  }
  switch(answers) {
    case 0:  
      Sfprintf(Serror, "[ no answers found");
      break;
    case 1:
      Sfprintf(Serror, "[ 1 answer found");
      break;
    default:
         Sfprintf(Serror, "[ %d answers found", answers);
      break;
  }
  Sfprintf(Serror, " (in %f seconds) ]\n\n", GLOBAL_execution_time);
  Sflush(Serror);

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
  Sfprintf(Serror, "  %s\n", output);
  Sflush(Serror);
  return;
}
#endif /* YAPOR */


#ifdef TABLING
static inline long show_statistics_table_entries(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  tab_ent_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_tab_ent);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TabEnt_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_table_entries, Pg_str_free(GLOBAL_pages_tab_ent) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Table entries:                   %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_tab_ent) * sizeof(struct table_entry), Pg_pg_alloc(GLOBAL_pages_tab_ent), Pg_str_in_use(GLOBAL_pages_tab_ent));
#else
  Sfprintf(out, "  Table entries:                   %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_tab_ent) * sizeof(struct table_entry), Pg_str_in_use(GLOBAL_pages_tab_ent));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_tab_ent) * sizeof(struct table_entry);
}


static inline long show_statistics_subgoal_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_sg_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SgFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_frames, Pg_str_free(GLOBAL_pages_sg_fr) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Subgoal frames:                  %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_fr) * sizeof(struct subgoal_frame), Pg_pg_alloc(GLOBAL_pages_sg_fr), Pg_str_in_use(GLOBAL_pages_sg_fr));
#else
  Sfprintf(out, "  Subgoal frames:                  %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_fr) * sizeof(struct subgoal_frame), Pg_str_in_use(GLOBAL_pages_sg_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_sg_fr) * sizeof(struct subgoal_frame);
}


static inline long show_statistics_dependency_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  dep_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_dep_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = DepFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_dependency_frames, Pg_str_free(GLOBAL_pages_dep_fr) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Dependency frames:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_dep_fr) * sizeof(struct dependency_frame), Pg_pg_alloc(GLOBAL_pages_dep_fr), Pg_str_in_use(GLOBAL_pages_dep_fr));
#else
  Sfprintf(out, "  Dependency frames:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_dep_fr) * sizeof(struct dependency_frame), Pg_str_in_use(GLOBAL_pages_dep_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_dep_fr) * sizeof(struct dependency_frame);
}


static inline long show_statistics_subgoal_trie_nodes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_sg_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_trie_nodes, Pg_str_free(GLOBAL_pages_sg_node) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Subgoal trie nodes:              %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_node) * sizeof(struct subgoal_trie_node), Pg_pg_alloc(GLOBAL_pages_sg_node), Pg_str_in_use(GLOBAL_pages_sg_node));
#else
  Sfprintf(out, "  Subgoal trie nodes:              %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_node) * sizeof(struct subgoal_trie_node), Pg_str_in_use(GLOBAL_pages_sg_node));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_sg_node) * sizeof(struct subgoal_trie_node);
}


static inline long show_statistics_answer_trie_nodes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  ans_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_ans_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_answer_trie_nodes, Pg_str_free(GLOBAL_pages_ans_node) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Answer trie nodes:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_ans_node) * sizeof(struct answer_trie_node), Pg_pg_alloc(GLOBAL_pages_ans_node), Pg_str_in_use(GLOBAL_pages_ans_node));
#else
  Sfprintf(out, "  Answer trie nodes:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_ans_node) * sizeof(struct answer_trie_node), Pg_str_in_use(GLOBAL_pages_ans_node));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_ans_node) * sizeof(struct answer_trie_node);
}


static inline long show_statistics_subgoal_trie_hashes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  sg_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_sg_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_subgoal_trie_hashes, Pg_str_free(GLOBAL_pages_sg_hash) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Subgoal trie hashes:             %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_hash) * sizeof(struct subgoal_trie_hash), Pg_pg_alloc(GLOBAL_pages_sg_hash), Pg_str_in_use(GLOBAL_pages_sg_hash));
#else
  Sfprintf(out, "  Subgoal trie hashes:             %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_sg_hash) * sizeof(struct subgoal_trie_hash), Pg_str_in_use(GLOBAL_pages_sg_hash));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_sg_hash) * sizeof(struct subgoal_trie_hash);
}


static inline long show_statistics_answer_trie_hashes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  ans_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_ans_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_answer_trie_hashes, Pg_str_free(GLOBAL_pages_ans_hash) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Answer trie hashes:              %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_ans_hash) * sizeof(struct answer_trie_hash), Pg_pg_alloc(GLOBAL_pages_ans_hash), Pg_str_in_use(GLOBAL_pages_ans_hash));
#else
  Sfprintf(out, "  Answer trie hashes:              %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_ans_hash) * sizeof(struct answer_trie_hash), Pg_str_in_use(GLOBAL_pages_ans_hash));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_ans_hash) * sizeof(struct answer_trie_hash);
}


static inline long show_statistics_global_trie_nodes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  pg_hd_ptr pg_hd;
  gt_node_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_gt_node);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = TrNode_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_global_trie_nodes, Pg_str_free(GLOBAL_pages_gt_node) != cont);
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Global trie nodes:               %10ld bytes (%ld pages and %ld structs in use)\n", 
	  Pg_str_in_use(GLOBAL_pages_gt_node) * sizeof(struct global_trie_node), Pg_pg_alloc(GLOBAL_pages_gt_node), Pg_str_in_use(GLOBAL_pages_gt_node));
#else
  Sfprintf(out, "  Global trie nodes:               %10ld bytes (%ld structs in use)\n", 
	  Pg_str_in_use(GLOBAL_pages_gt_node) * sizeof(struct global_trie_node), Pg_str_in_use(GLOBAL_pages_gt_node));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_gt_node) * sizeof(struct global_trie_node);
}


static inline long show_statistics_global_trie_hashes(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
  /* suport not yet implemented :(
  pg_hd_ptr pg_hd;
  gt_hash_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_gt_hash);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = Hash_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  TABLING_ERROR_CHECKING(statistics_global_trie_hashes, Pg_str_free(GLOBAL_pages_gt_hash) != cont);
  */
#endif /* DEBUG_TABLING */
  Sfprintf(out, "  Global trie hashes:              %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_gt_hash) * sizeof(struct global_trie_hash), Pg_pg_alloc(GLOBAL_pages_gt_hash), Pg_str_in_use(GLOBAL_pages_gt_hash));
#else
  Sfprintf(out, "  Global trie hashes:              %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_gt_hash) * sizeof(struct global_trie_hash), Pg_str_in_use(GLOBAL_pages_gt_hash));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_gt_hash) * sizeof(struct global_trie_hash);
}
#endif /* TABLING */


#ifdef YAPOR
static inline long show_statistics_or_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  or_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_or_fr );
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = OrFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_or_frames, Pg_str_free(GLOBAL_pages_or_fr ) != cont);
#endif /* DEBUG_YAPOR */
  Sfprintf(out, "  Or-frames:                       %10ld bytes (%ld pages and %ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_or_fr ) * sizeof(struct or_frame), Pg_pg_alloc(GLOBAL_pages_or_fr ), Pg_str_in_use(GLOBAL_pages_or_fr ));
#else
  Sfprintf(out, "  Or-frames:                       %10ld bytes (%ld structs in use)\n", 
          Pg_str_in_use(GLOBAL_pages_or_fr ) * sizeof(struct or_frame), Pg_str_in_use(GLOBAL_pages_or_fr ));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_or_fr ) * sizeof(struct or_frame);
}


static inline long show_statistics_query_goal_solution_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  qg_sol_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_qg_sol_fr );
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_query_goal_solution_frames, Pg_str_free(GLOBAL_pages_qg_sol_fr ) != cont);
#endif /* DEBUG_YAPOR */
  Sfprintf(out, "  Query goal solution frames:      %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_qg_sol_fr ) * sizeof(struct query_goal_solution_frame), Pg_pg_alloc(GLOBAL_pages_qg_sol_fr ), Pg_str_in_use(GLOBAL_pages_qg_sol_fr ));
#else
  Sfprintf(out, "  Query goal solution frames:      %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_qg_sol_fr ) * sizeof(struct query_goal_solution_frame), Pg_str_in_use(GLOBAL_pages_qg_sol_fr ));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_qg_sol_fr ) * sizeof(struct query_goal_solution_frame);
}


static inline long show_statistics_query_goal_answer_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_YAPOR
  pg_hd_ptr pg_hd;
  qg_ans_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_qg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  YAPOR_ERROR_CHECKING(statistics_query_goal_answer_frames, Pg_str_free(GLOBAL_pages_qg_ans_fr) != cont);
#endif /* DEBUG_YAPOR */
  Sfprintf(out, "  Query goal answer frames:        %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_qg_ans_fr) * sizeof(struct query_goal_answer_frame), Pg_pg_alloc(GLOBAL_pages_qg_ans_fr), Pg_str_in_use(GLOBAL_pages_qg_ans_fr));
#else
  Sfprintf(out, "  Query goal answer frames:        %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_qg_ans_fr) * sizeof(struct query_goal_answer_frame), Pg_str_in_use(GLOBAL_pages_qg_ans_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_qg_ans_fr) * sizeof(struct query_goal_answer_frame);
}
#endif /* YAPOR */


#if defined(YAPOR) && defined(TABLING)
static inline long show_statistics_suspension_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  susp_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_susp_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SuspFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_suspension_frames, Pg_str_free(GLOBAL_pages_susp_fr) != cont);
#endif /* DEBUG_OPTYAP */
  Sfprintf(out, "  Suspension frames:               %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_susp_fr) * sizeof(struct suspension_frame), Pg_pg_alloc(GLOBAL_pages_susp_fr), Pg_str_in_use(GLOBAL_pages_susp_fr));
#else
  Sfprintf(out, "  Suspension frames:               %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_susp_fr) * sizeof(struct suspension_frame), Pg_str_in_use(GLOBAL_pages_susp_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_susp_fr) * sizeof(struct suspension_frame);
}


#ifdef TABLING_INNER_CUTS
static inline long show_statistics_table_subgoal_solution_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  tg_sol_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_tg_sol_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = SolFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_table_subgoal_solution_frames, Pg_str_free(GLOBAL_pages_tg_sol_fr) != cont);
#endif /* DEBUG_OPTYAP */
  Sfprintf(out, "  Table subgoal solution frames:   %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame), Pg_pg_alloc(GLOBAL_pages_tg_sol_fr), Pg_str_in_use(GLOBAL_pages_tg_sol_fr));
#else
  Sfprintf(out, "  Table subgoal solution frames:   %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame), Pg_str_in_use(GLOBAL_pages_tg_sol_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_tg_sol_fr) * sizeof(struct table_subgoal_solution_frame);
}


static inline long show_statistics_table_subgoal_answer_frames(IOSTREAM *out) {
#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_OPTYAP
  pg_hd_ptr pg_hd;
  tg_ans_fr_ptr aux_ptr;
  long cont = 0;

  pg_hd = Pg_free_pg(GLOBAL_pages_tg_ans_fr);
  while (pg_hd) {
    aux_ptr = PgHd_free_str(pg_hd);
    while (aux_ptr) {
      cont++;
      aux_ptr = AnsFr_next(aux_ptr);
    }
    pg_hd = PgHd_next(pg_hd);
  }
  OPTYAP_ERROR_CHECKING(statistics_table_subgoal_answer_frames, Pg_str_free(GLOBAL_pages_tg_ans_fr) != cont);
#endif /* DEBUG_OPTYAP */
  Sfprintf(out, "  Table subgoal answer frames:     %10ld bytes (%ld pages and %ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame), Pg_pg_alloc(GLOBAL_pages_tg_ans_fr), Pg_str_in_use(GLOBAL_pages_tg_ans_fr));
#else
  Sfprintf(out, "  Table subgoal answer frames:     %10ld bytes (%ld structs in use)\n",
          Pg_str_in_use(GLOBAL_pages_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame), Pg_str_in_use(GLOBAL_pages_tg_ans_fr));
#endif /* USE_PAGES_MALLOC */
  return Pg_str_in_use(GLOBAL_pages_tg_ans_fr) * sizeof(struct table_subgoal_answer_frame);
}
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
#endif /* YAPOR || TABLING */
