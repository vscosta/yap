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


/// @file opt.preds.c
///
/// @namespace prolog

/************************************
**      Includes & Prototypes      **
************************************/

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#include "YapHeap.h"
#include "Yatom.h"
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
#include "iopreds.h"

#ifdef TABLING
static Int p_freeze_choice_point(USES_REGS1);
static Int p_wake_choice_point(USES_REGS1);
static Int p_abolish_frozen_choice_points_until(USES_REGS1);
static Int p_abolish_frozen_choice_points_all(USES_REGS1);
static Int p_table(USES_REGS1);
static Int p_tabling_mode(USES_REGS1);
static Int p_abolish_table(USES_REGS1);
static Int p_abolish_all_tables(USES_REGS1);
static Int p_show_tabled_predicates(USES_REGS1);
static Int p_show_table(USES_REGS1);
static Int p_show_all_tables(USES_REGS1);
static Int p_show_global_trie(USES_REGS1);
static Int p_show_statistics_table(USES_REGS1);
static Int p_show_statistics_tabling(USES_REGS1);
static Int p_show_statistics_global_trie(USES_REGS1);
#endif /* TABLING */

static Int p_yapor_workers(USES_REGS1);
#ifdef YAPOR
static Int p_parallel_mode(USES_REGS1);
static Int p_yapor_start(USES_REGS1);
static Int p_worker(USES_REGS1);
static Int p_parallel_new_answer(USES_REGS1);
static Int p_parallel_get_answers(USES_REGS1);
static Int p_show_statistics_or(USES_REGS1);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt(USES_REGS1);
#endif /* YAPOR && TABLING */
static Int p_get_optyap_statistics(USES_REGS1);

#ifdef YAPOR
static inline realtime current_time(void);
#endif /* YAPOR */

#ifdef TABLING
static inline struct page_statistics show_statistics_table_entries(FILE *out);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
static inline struct page_statistics show_statistics_subgoal_entries(FILE *out);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
static inline struct page_statistics show_statistics_subgoal_frames(FILE *out);
static inline struct page_statistics
show_statistics_dependency_frames(FILE *out);
static inline struct page_statistics
show_statistics_subgoal_trie_nodes(FILE *out);
static inline struct page_statistics
show_statistics_subgoal_trie_hashes(FILE *out);
static inline struct page_statistics
show_statistics_answer_trie_nodes(FILE *out);
static inline struct page_statistics
show_statistics_answer_trie_hashes(FILE *out);
#if defined(THREADS_FULL_SHARING)
static inline struct page_statistics
show_statistics_answer_ref_nodes(FILE *out);
#endif /* THREADS_FULL_SHARING */
static inline struct page_statistics
show_statistics_global_trie_nodes(FILE *out);
static inline struct page_statistics
show_statistics_global_trie_hashes(FILE *out);
#endif /* TABLING */
#ifdef YAPOR
static inline struct page_statistics show_statistics_or_frames(FILE *out);
static inline struct page_statistics
show_statistics_query_goal_solution_frames(FILE *out);
static inline struct page_statistics
show_statistics_query_goal_answer_frames(FILE *out);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
static inline struct page_statistics
show_statistics_suspension_frames(FILE *out);
#ifdef TABLING_INNER_CUTS
static inline struct page_statistics
show_statistics_table_subgoal_solution_frames(FILE *out);
static inline struct page_statistics
show_statistics_table_subgoal_answer_frames(FILE *out);
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */

/************************************
**      Macros & Declarations      **
************************************/

struct page_statistics {
#ifdef USE_PAGES_MALLOC
  long pages_in_use;   /* same as struct pages (opt.structs.h) */
#endif                 /* USE_PAGES_MALLOC */
  size_t structs_in_use; /* same as struct pages (opt.structs.h) */
  long bytes_in_use;
};

#define PgEnt_bytes_in_use(STATS) STATS.bytes_in_use

#ifdef USE_PAGES_MALLOC
#ifdef DEBUG_TABLING
#define CHECK_PAGE_FREE_STRUCTS(STR_TYPE, PAGE)                                \
  {                                                                            \
    pg_hd_ptr pg_hd;                                                           \
    STR_TYPE *aux_ptr;                                                         \
    long cont = 0;                                                             \
    pg_hd = PgEnt_first(PAGE);                                                 \
    while (pg_hd) {                                                            \
      aux_ptr = PgHd_first_str(pg_hd);                                         \
      while (aux_ptr) {                                                        \
        cont++;                                                                \
        aux_ptr = aux_ptr->next;                                               \
      }                                                                        \
      pg_hd = PgHd_next(pg_hd);                                                \
    }                                                                          \
    TABLING_ERROR_CHECKING(CHECK_PAGE_FREE_STRUCTS,                            \
                           PgEnt_strs_free(PAGE) != cont);                     \
  }
#else
#define CHECK_PAGE_FREE_STRUCTS(STR_TYPE, PAGE)
#endif /* DEBUG_TABLING */
#define INIT_PAGE_STATS(STATS)                                                 \
  PgEnt_pages_in_use(STATS) = 0;                                               \
  PgEnt_strs_in_use(STATS) = 0
#define INCREMENT_PAGE_STATS(STATS, PAGE)                                      \
  PgEnt_pages_in_use(STATS) += PgEnt_pages_in_use(PAGE);                       \
  PgEnt_strs_in_use(STATS) += PgEnt_strs_in_use(PAGE)
#define INCREMENT_AUX_STATS(STATS, BYTES, PAGES)                               \
  BYTES += PgEnt_bytes_in_use(STATS);                                          \
  PAGES += PgEnt_pages_in_use(STATS)
#define SHOW_PAGE_STATS_MSG(STR_NAME)                                          \
  "  " STR_NAME "   %10" Int_F " bytes (%ld pages and %ld structs in use)\n"
#define SHOW_PAGE_STATS_ARGS(STATS, STR_TYPE)                                  \
  PgEnt_strs_in_use(STATS) * sizeof(STR_TYPE), PgEnt_pages_in_use(STATS),      \
      PgEnt_strs_in_use(STATS)
#else /* !USE_PAGES_MALLOC */
#define CHECK_PAGE_FREE_STRUCTS(STR_TYPE, PAGE)
#define INIT_PAGE_STATS(STATS) PgEnt_strs_in_use(STATS) = 0
#define INCREMENT_PAGE_STATS(STATS, PAGE)                                      \
  PgEnt_strs_in_use(STATS) += PgEnt_strs_in_use(PAGE)
#define INCREMENT_AUX_STATS(STATS, BYTES, PAGES)                               \
  BYTES += PgEnt_bytes_in_use(STATS)
#define SHOW_PAGE_STATS_MSG(STR_NAME)                                          \
  "  %s %10" Int_F "s bytes (%" Sizet_F " ; structs in use)\n", STR_NAME
#define SHOW_PAGE_STATS_ARGS(STATS, STR_TYPE)                                  \
  PgEnt_strs_in_use(STATS) * sizeof(STR_TYPE), PgEnt_strs_in_use(STATS)
#endif /* USE_PAGES_MALLOC */

#if defined(THREADS) && defined(TABLING)
#define GET_ALL_PAGE_STATS(STATS, STR_TYPE, _PAGES)                            \
  LOCK(GLOBAL_ThreadHandlesLock);                                              \
  CHECK_PAGE_FREE_STRUCTS(STR_TYPE, GLOBAL##_PAGES);                           \
  INCREMENT_PAGE_STATS(STATS, GLOBAL##_PAGES);                                 \
  {                                                                            \
    int wid;                                                                   \
    for (wid = 0; wid < MAX_THREADS; wid++) {                                  \
      if (!Yap_local[wid])                                                     \
        break;                                                                 \
      if (REMOTE_ThreadHandle(wid).in_use) {                                   \
        CHECK_PAGE_FREE_STRUCTS(STR_TYPE, REMOTE##_PAGES(wid));                \
        INCREMENT_PAGE_STATS(STATS, REMOTE##_PAGES(wid));                      \
      }                                                                        \
    }                                                                          \
  }                                                                            \
  UNLOCK(GLOBAL_ThreadHandlesLock)
#else
#define GET_ALL_PAGE_STATS(STATS, STR_TYPE, _PAGES)                            \
  CHECK_PAGE_FREE_STRUCTS(STR_TYPE, GLOBAL##_PAGES);                           \
  INCREMENT_PAGE_STATS(STATS, GLOBAL##_PAGES)
#endif

#define GET_PAGE_STATS(STATS, STR_TYPE, _PAGES)                                \
  INIT_PAGE_STATS(STATS);                                                      \
  GET_ALL_PAGE_STATS(STATS, STR_TYPE, _PAGES);                                 \
  PgEnt_bytes_in_use(STATS) = PgEnt_strs_in_use(STATS) * sizeof(STR_TYPE)
#define SHOW_PAGE_STATS(OUT_STREAM, STR_TYPE, _PAGES, STR_NAME)                \
  {                                                                            \
    struct page_statistics stats;                                              \
    GET_PAGE_STATS(stats, STR_TYPE, _PAGES);                                   \
    fprintf(OUT_STREAM, SHOW_PAGE_STATS_MSG(STR_NAME),                         \
            SHOW_PAGE_STATS_ARGS(stats, STR_TYPE));                            \
    return stats;                                                              \
  }

/*******************************
**      Global functions      **
*******************************/

void Yap_init_optyap_preds(void) {
  Yap_InitCPred("$c_yapor_workers", 1, p_yapor_workers,
                SafePredFlag | SyncPredFlag);
#ifdef TABLING
  Yap_InitCPred("freeze_choice_point", 1, p_freeze_choice_point,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("wake_choice_point", 1, p_wake_choice_point,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("abolish_frozen_choice_points", 1,
                p_abolish_frozen_choice_points_until,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("abolish_frozen_choice_points", 0,
                p_abolish_frozen_choice_points_all,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_table", 3, p_table, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_tabling_mode", 3, p_tabling_mode,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_abolish_table", 2, p_abolish_table,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("abolish_all_tables", 0, p_abolish_all_tables,
                SafePredFlag | SyncPredFlag);
  /** @pred abolish_all_tables


  Removes all the entries from the table space for all tabled
  predicates. The predicates remain as tabled predicates.


  */
  Yap_InitCPred("show_tabled_predicates", 1, p_show_tabled_predicates,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_show_table", 3, p_show_table, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("show_all_tables", 1, p_show_all_tables,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("show_global_trie", 1, p_show_global_trie,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_table_statistics", 3, p_show_statistics_table,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("tabling_statistics", 1, p_show_statistics_tabling,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("global_trie_statistics", 1, p_show_statistics_global_trie,
                SafePredFlag | SyncPredFlag);
#endif /* TABLING */
#ifdef YAPOR
  Yap_InitCPred("parallel_mode", 1, p_parallel_mode,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_yapor_start", 0, p_yapor_start,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_worker", 0, p_worker, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_parallel_new_answer", 1, p_parallel_new_answer,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$c_parallel_get_answers", 1, p_parallel_get_answers,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("or_statistics", 1, p_show_statistics_or,
                SafePredFlag | SyncPredFlag);
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  Yap_InitCPred("opt_statistics", 1, p_show_statistics_opt,
                SafePredFlag | SyncPredFlag);
#endif /* YAPOR && TABLING */
  Yap_InitCPred("$c_get_optyap_statistics", 3, p_get_optyap_statistics,
                SafePredFlag | SyncPredFlag);
}

#ifdef YAPOR
void finish_yapor(void) {
  GLOBAL_execution_time = current_time() - GLOBAL_execution_time;
  GLOBAL_parallel_mode = PARALLEL_MODE_ON;
  return;
}
#endif /* YAPOR */

/***********************************
**      Tabling C Predicates      **
***********************************/

#ifdef TABLING
static Int p_freeze_choice_point(USES_REGS1) {
  if (IsVarTerm(Deref(ARG1))) {
    Int offset = freeze_current_cp();
    return Yap_unify(ARG1, MkIntegerTerm(offset));
  }
  return (FALSE);
}

static Int p_wake_choice_point(USES_REGS1) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    wake_frozen_cp(IntegerOfTerm(term_offset));
  return (FALSE);
}

static Int p_abolish_frozen_choice_points_until(USES_REGS1) {
  Term term_offset = Deref(ARG1);
  if (IsIntegerTerm(term_offset))
    abolish_frozen_cps_until(IntegerOfTerm(term_offset));
  return (TRUE);
}

static Int p_abolish_frozen_choice_points_all(USES_REGS1) {
  abolish_frozen_cps_all();
  return (TRUE);
}

static Int p_table(USES_REGS1) {
  Term mod, t, list;
  PredEntry *pe;
  Atom at;
  int arity;
  tab_ent_ptr tab_ent;
#ifdef MODE_DIRECTED_TABLING
  int *mode_directed = NULL;
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
  if (list != TermNil) { /* non-empty list */
#ifndef MODE_DIRECTED_TABLING
    Yap_Error(SYSTEM_ERROR_COMPILER, TermNil, "invalid tabling declaration for "
                                              "%s/%d (mode directed tabling "
                                              "not enabled)",
              AtomName(at), arity);
    return (FALSE);
#else
    /*************************************************************************************
      The mode operator declaration is reordered as follows:
         1. arguments with mode 'index'         (any number)
         2. arguments with mode 'min' and 'max' (any number, following the
    original order)
         3. arguments with mode 'all'           (any number)
         4. arguments with mode 'sum' or 'last' (only one of the two is allowed)
         5. arguments with mode 'first'         (any number)
    *************************************************************************************/
    int pos_index = 0;
    int pos_min_max = 0;
    int pos_all = 0;
    int pos_sum_last = 0;
    int pos_first = 0;
    int i;
    int *aux_mode_directed;

    aux_mode_directed = malloc(arity * sizeof(int));
    for (i = 0; i < arity; i++) {
      int mode = IntOfTerm(HeadOfTerm(list));
      if (mode == MODE_DIRECTED_INDEX)
        pos_index++;
      else if (mode == MODE_DIRECTED_MIN || mode == MODE_DIRECTED_MAX)
        pos_min_max++;
      else if (mode == MODE_DIRECTED_ALL)
        pos_all++;
      else if (mode == MODE_DIRECTED_SUM || mode == MODE_DIRECTED_LAST) {
        if (pos_sum_last) {
          free(aux_mode_directed);
          Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                    "invalid tabling declaration for %s/%d (more than one "
                    "argument with modes 'sum' and/or 'last')",
                    AtomName(at), arity);
          return (FALSE);
        } else
          pos_sum_last = 1;
      }
      aux_mode_directed[i] = mode;
      list = TailOfTerm(list);
    }
    pos_first = pos_index + pos_min_max + pos_all + pos_sum_last;
    pos_sum_last = pos_index + pos_min_max + pos_all;
    pos_all = pos_index + pos_min_max;
    pos_min_max = pos_index;
    pos_index = 0;
    ALLOC_BLOCK(mode_directed, arity * sizeof(int), int);
    for (i = 0; i < arity; i++) {
      int aux_pos = 0;
      if (aux_mode_directed[i] == MODE_DIRECTED_INDEX)
        aux_pos = pos_index++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_MIN ||
               aux_mode_directed[i] == MODE_DIRECTED_MAX)
        aux_pos = pos_min_max++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_ALL)
        aux_pos = pos_all++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_SUM ||
               aux_mode_directed[i] == MODE_DIRECTED_LAST)
        aux_pos = pos_sum_last++;
      else if (aux_mode_directed[i] == MODE_DIRECTED_FIRST)
        aux_pos = pos_first++;
      mode_directed[aux_pos] = MODE_DIRECTED_SET(i, aux_mode_directed[i]);
    }
    free(aux_mode_directed);
#endif /* MODE_DIRECTED_TABLING */
  }
  if (pe->PredFlags & TabledPredFlag)
    return (TRUE); /* predicate already tabled */
  if (pe->FirstClause)
    return (FALSE); /* predicate already compiled */
  if (!(pe->PredFlags & TabledPredFlag)) {
    pe->PredFlags |= TabledPredFlag;
    new_table_entry(tab_ent, pe, at, arity, mode_directed);
    pe->TableOfPred = tab_ent;
  }
  return (TRUE);
}

static Int p_tabling_mode(USES_REGS1) {
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
    if (IsMode_CoInductive(TabEnt_flags(tab_ent)))
      t = MkPairTerm(MkAtomTerm(AtomCoInductive), t);
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
    YapBind((CELL *)tvalue, t);
    return (TRUE);
  } else if (IsIntTerm(tvalue)) {
    Int value = IntOfTerm(tvalue);
    if (value == 1) { /* batched */
      SetMode_Batched(TabEnt_flags(tab_ent));
      if (!IsMode_Local(LOCAL_TabMode)) {
        SetMode_Batched(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 2) { /* local */
      SetMode_Local(TabEnt_flags(tab_ent));
      if (!IsMode_Batched(LOCAL_TabMode)) {
        SetMode_Local(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 3) { /* exec_answers */
      SetMode_ExecAnswers(TabEnt_flags(tab_ent));
      if (!IsMode_LoadAnswers(LOCAL_TabMode)) {
        SetMode_ExecAnswers(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 4) { /* load_answers */
      SetMode_LoadAnswers(TabEnt_flags(tab_ent));
      if (!IsMode_ExecAnswers(LOCAL_TabMode)) {
        SetMode_LoadAnswers(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 5) { /* local_trie */
      SetMode_LocalTrie(TabEnt_flags(tab_ent));
      if (!IsMode_GlobalTrie(LOCAL_TabMode)) {
        SetMode_LocalTrie(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 6) { /* global_trie */
      SetMode_GlobalTrie(TabEnt_flags(tab_ent));
      if (!IsMode_LocalTrie(LOCAL_TabMode)) {
        SetMode_GlobalTrie(TabEnt_mode(tab_ent));
        return (TRUE);
      }
    } else if (value == 7) {
      /* coinductive */ // only affect the predicate flag. Also it cant be unset
      SetMode_CoInductive(TabEnt_flags(tab_ent));
      return (TRUE);
    }
  }
  return (FALSE);
}

static Int p_abolish_table(USES_REGS1) {
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
  abolish_table(tab_ent);
  return (TRUE);
}

static Int p_abolish_all_tables(USES_REGS1) {
  tab_ent_ptr tab_ent;

  tab_ent = GLOBAL_root_tab_ent;
  while (tab_ent) {
    abolish_table(tab_ent);
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}

static Int p_show_tabled_predicates(USES_REGS1) {
  FILE *out;
  tab_ent_ptr tab_ent;
  Term t = Deref(ARG1);

  if (!IsStreamTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t)->file))
    return FALSE;
  tab_ent = GLOBAL_root_tab_ent;
  fprintf(out, "Tabled predicates\n");
  if (tab_ent == NULL)
    fprintf(out, "  NONE\n");
  else
    while (tab_ent) {
      fprintf(out, "  %s/%d\n", AtomName(TabEnt_atom(tab_ent)),
              TabEnt_arity(tab_ent));
      tab_ent = TabEnt_next(tab_ent);
    }
  // PL_release_stream(out);
  return (TRUE);
}

static Int p_show_table(USES_REGS1) {
  Term mod, t;
  tab_ent_ptr tab_ent;
  Term t1 = Deref(ARG1);
  FILE *out;

  if (!IsStreamTerm(t1))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t1)->file))
    return FALSE;
  mod = Deref(ARG2);
  t = Deref(ARG3);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else {
    return (FALSE);
  }
  showTable(tab_ent, SHOW_MODE_STRUCTURE, out);
  return (TRUE);
}

static Int p_show_all_tables(USES_REGS1) {
  tab_ent_ptr tab_ent;
  Term t = Deref(ARG1);
  FILE *out;

  if (!IsStreamTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t)->file))
    return FALSE;
  tab_ent = GLOBAL_root_tab_ent;
  while (tab_ent) {
    showTable(tab_ent, SHOW_MODE_STRUCTURE, out);
    tab_ent = TabEnt_next(tab_ent);
  }
  return (TRUE);
}

static Int p_show_global_trie(USES_REGS1) {
  Term t = Deref(ARG1);
  FILE *out;

  if (!IsStreamTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t)->file))
    return FALSE;
  showGlobalTrie(SHOW_MODE_STRUCTURE, out);
  return (TRUE);
}

static Int p_show_statistics_table(USES_REGS1) {
  Term mod, t;
  tab_ent_ptr tab_ent;
  Term t1 = Deref(ARG1);
  FILE *out;

  if (!IsStreamTerm(t1))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t1)->file))
    return FALSE;
  mod = Deref(ARG2);
  t = Deref(ARG3);
  if (IsAtomTerm(t))
    tab_ent = RepPredProp(PredPropByAtom(AtomOfTerm(t), mod))->TableOfPred;
  else if (IsApplTerm(t))
    tab_ent = RepPredProp(PredPropByFunc(FunctorOfTerm(t), mod))->TableOfPred;
  else {
    // PL_release_stream(out);
    return (FALSE);
  }
  showTable(tab_ent, SHOW_MODE_STATISTICS, out);
  return (TRUE);
}

static Int p_show_statistics_tabling(USES_REGS1) {
  struct page_statistics stats;
  long bytes, total_bytes = 0;
#ifdef USE_PAGES_MALLOC
  long total_pages = 0;
#endif /* USE_PAGES_MALLOC */
  FILE *out;
  Term t = Deref(ARG1);

  if (!IsStreamTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t)->file))
    return FALSE;
  bytes = 0;
  fprintf(out, "Execution data structures\n");
  stats = show_statistics_table_entries(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  stats = show_statistics_subgoal_entries(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
  stats = show_statistics_subgoal_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_dependency_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (I):               %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Local trie data structures\n");
  stats = show_statistics_subgoal_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_answer_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_subgoal_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_answer_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#if defined(THREADS_FULL_SHARING)
  stats = show_statistics_answer_ref_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#endif /* THREADS_FULL_SHARING */
  fprintf(out, "  Memory in use (II):              %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Global trie data structures\n");
  stats = show_statistics_global_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_global_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (III):             %10ld bytes\n\n", bytes);
  total_bytes += bytes;
#ifdef USE_PAGES_MALLOC
  fprintf(out,
          "Total memory in use (I+II+III):    %10ld bytes (%ld pages in use)\n",
          total_bytes, total_pages);
  fprintf(
      out,
      "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
      PgEnt_pages_in_use(GLOBAL_pages_alloc) * Yap_page_size,
      PgEnt_pages_in_use(GLOBAL_pages_alloc));
#else
  fprintf(out, "Total memory in use (I+II+III):    %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  // PL_release_stream(out);
  return (TRUE);
}

static Int p_show_statistics_global_trie(USES_REGS1) {
  Term t = Deref(ARG1);
  FILE *out;

  if (!IsStreamTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(t)->file))
    return FALSE;
  showGlobalTrie(SHOW_MODE_STATISTICS, out);
  return (TRUE);
}
#endif /* TABLING */

/*********************************
**      YapOr C Predicates      **
*********************************/

#ifdef YAPOR
static Int p_parallel_mode(USES_REGS1) {
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
    YapBind((CELL *)t, ta);
    return (TRUE);
  }
  if (IsAtomTerm(t) && GLOBAL_parallel_mode != PARALLEL_MODE_RUNNING) {
    char *s;
    s = RepAtom(AtomOfTerm(t))->StrOfAE;
    if (strcmp(s, "on") == 0) {
      GLOBAL_parallel_mode = PARALLEL_MODE_ON;
      return (TRUE);
    }
    if (strcmp(s, "off") == 0) {
      GLOBAL_parallel_mode = PARALLEL_MODE_OFF;
      return (TRUE);
    }
    return (FALSE); /* PARALLEL_MODE_RUNNING */
  }
  return (FALSE);
}

static Int p_yapor_start(USES_REGS1) {
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
  return (TRUE);
}

static Int p_yapor_workers(USES_REGS1) {
#ifdef YAPOR_THREADS
  return Yap_unify(MkIntegerTerm(GLOBAL_number_workers), ARG1);
#else
  return FALSE;
#endif /* YAPOR_THREADS */
}

static Int p_worker(USES_REGS1) {
  CurrentModule = USER_MODULE;
  P = GETWORK_FIRST_TIME;
  return TRUE;
}

static Int p_parallel_new_answer(USES_REGS1) {
  qg_ans_fr_ptr actual_answer;
  or_fr_ptr leftmost_or_fr;

  ALLOC_QG_ANSWER_FRAME(actual_answer);
  AnsFr_answer(actual_answer) = Deref(ARG1);
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

static Int p_parallel_get_answers(USES_REGS1) {
  Term t = TermNil;

  if (OrFr_qg_solutions(LOCAL_top_or_fr)) {
    qg_ans_fr_ptr aux_answer1, aux_answer2;
    aux_answer1 = SolFr_first(OrFr_qg_solutions(LOCAL_top_or_fr));
    while (aux_answer1) {
      t = MkPairTerm(AnsFr_answer(aux_answer1), t);
      aux_answer2 = aux_answer1;
      aux_answer1 = AnsFr_next(aux_answer1);
      FREE_QG_ANSWER_FRAME(aux_answer2);
    }
    FREE_QG_SOLUTION_FRAME(OrFr_qg_solutions(LOCAL_top_or_fr));
    OrFr_qg_solutions(LOCAL_top_or_fr) = NULL;
  }
  Yap_unify(ARG1, t);
  return (TRUE);
}

static Int p_show_statistics_or(USES_REGS1) {
  struct page_statistics stats;
  long bytes, total_bytes = 0;
#ifdef USE_PAGES_MALLOC
  long total_pages = 0;
#endif /* USE_PAGES_MALLOC */
  Term t = Deref(ARG1);

  if (!IsStreamTerm(t))
    return FALSE;
  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ bytes =
      0;
  fprintf(out, "Execution data structures\n");
  stats = show_statistics_or_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (I):               %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Cut support data structures\n");
  stats = show_statistics_query_goal_solution_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_query_goal_answer_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (II):              %10ld bytes\n\n", bytes);
  total_bytes += bytes;
#ifdef USE_PAGES_MALLOC
  fprintf(out,
          "Total memory in use (I+II):        %10ld bytes (%ld pages in use)\n",
          total_bytes, total_pages);
  fprintf(
      out,
      "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
      PgEnt_pages_in_use(GLOBAL_pages_alloc) * Yap_page_size,
      PgEnt_pages_in_use(GLOBAL_pages_alloc));
#else
  fprintf(out, "Total memory in use (I+II):        %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  PL_release_stream(out);
  return (TRUE);
}

#else

static Int p_yapor_workers(USES_REGS1) { return FALSE; }
#endif /* YAPOR */

/**********************************
 **      OPTYap C Predicates      **
**********************************/

#if defined(YAPOR) && defined(TABLING)
static Int p_show_statistics_opt(USES_REGS1) {
  struct page_statistics stats;
  long bytes, total_bytes = 0;
#ifdef USE_PAGES_MALLOC
  long total_pages = 0;
#endif /* USE_PAGES_MALLOC */
  FILE *out;
  Term t = Deref(ARG1);

  if (IsVarTerm(t) || !IsAtomTerm(t))
    return FALSE;
  if (!(out = Yap_GetStreamHandle(AtomOfTerm(t))))
    return FALSE;
  bytes = 0;
  fprintf(out, "Execution data structures\n");
  stats = show_statistics_table_entries(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  stats = show_statistics_subgoal_entries(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
  stats = show_statistics_subgoal_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_dependency_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_or_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_suspension_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (I):               %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Local trie data structures\n");
  stats = show_statistics_subgoal_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_answer_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_subgoal_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_answer_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#if defined(THREADS_FULL_SHARING)
  stats = show_statistics_answer_ref_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#endif /* THREADS_FULL_SHARING */
  fprintf(out, "  Memory in use (II):              %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Global trie data structures\n");
  stats = show_statistics_global_trie_nodes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_global_trie_hashes(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  fprintf(out, "  Memory in use (III):             %10ld bytes\n\n", bytes);
  total_bytes += bytes;
  bytes = 0;
  fprintf(out, "Cut support data structures\n");
  stats = show_statistics_query_goal_solution_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_query_goal_answer_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#ifdef TABLING_INNER_CUTS
  stats = show_statistics_table_subgoal_solution_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
  stats = show_statistics_table_subgoal_answer_frames(out);
  INCREMENT_AUX_STATS(stats, bytes, total_pages);
#endif /* TABLING_INNER_CUTS */
  fprintf(out, "  Memory in use (IV):              %10ld bytes\n\n", bytes);
  total_bytes += bytes;
#ifdef USE_PAGES_MALLOC
  fprintf(out,
          "Total memory in use (I+II+III+IV): %10ld bytes (%ld pages in use)\n",
          total_bytes, total_pages);
  fprintf(
      out,
      "Total memory allocated:            %10ld bytes (%ld pages in total)\n",
      PgEnt_pages_in_use(GLOBAL_pages_alloc) * Yap_page_size,
      PgEnt_pages_in_use(GLOBAL_pages_alloc));
#else
  fprintf(out, "Total memory in use (I+II+III+IV): %10ld bytes\n", total_bytes);
#endif /* USE_PAGES_MALLOC */
  PL_release_stream(out);
  return (TRUE);
}
#endif /* YAPOR && TABLING */

static Int p_get_optyap_statistics(USES_REGS1) {
  struct page_statistics stats;
  Int value, bytes = 0, structs = -1;
  Term tbytes, tstructs;

  value = IntOfTerm(Deref(ARG1));
#ifdef TABLING
  if (value == 0 || value == 1) { /* table_entries */
    GET_PAGE_STATS(stats, struct table_entry, _pages_tab_ent);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  if (value == 0 || value == 16) { /* subgoal_entries */
    GET_PAGE_STATS(stats, struct subgoal_entry, _pages_sg_ent);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#endif
  if (value == 0 || value == 2) { /* subgoal_frames */
    GET_PAGE_STATS(stats, struct subgoal_frame, _pages_sg_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 3) { /* dependency_frames */
    GET_PAGE_STATS(stats, struct dependency_frame, _pages_dep_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 6) { /* subgoal_trie_nodes */
    GET_PAGE_STATS(stats, struct subgoal_trie_node, _pages_sg_node);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 8) { /* subgoal_trie_hashes */
    GET_PAGE_STATS(stats, struct subgoal_trie_hash, _pages_sg_hash);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 7) { /* answer_trie_nodes */
    GET_PAGE_STATS(stats, struct answer_trie_node, _pages_ans_node);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 9) { /* answer_trie_hashes */
    GET_PAGE_STATS(stats, struct answer_trie_hash, _pages_ans_hash);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#if defined(THREADS_FULL_SHARING)
  if (value == 0 || value == 17) { /* answer_ref_nodes */
    GET_PAGE_STATS(stats, struct answer_ref_node, _pages_ans_ref_node);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#endif
  if (value == 0 || value == 10) { /* global_trie_nodes */
    GET_PAGE_STATS(stats, struct global_trie_node, _pages_gt_node);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 11) { /* global_trie_hashes */
    GET_PAGE_STATS(stats, struct global_trie_hash, _pages_gt_hash);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#endif /* TABLING */
#ifdef YAPOR
  if (value == 0 || value == 4) { /* or_frames */
    GET_PAGE_STATS(stats, struct or_frame, _pages_or_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 12) { /* query_goal_solution_frames */
    GET_PAGE_STATS(stats, struct query_goal_solution_frame, _pages_qg_sol_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 13) { /* query_goal_answer_frames */
    GET_PAGE_STATS(stats, struct query_goal_answer_frame, _pages_qg_ans_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#endif /* YAPOR */
#if defined(YAPOR) && defined(TABLING)
  if (value == 0 || value == 5) { /* suspension_frames */
    GET_PAGE_STATS(stats, struct suspension_frame, _pages_susp_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#ifdef TABLING_INNER_CUTS
  if (value == 0 || value == 14) { /* table_subgoal_solution_frames */
    GET_PAGE_STATS(stats, struct table_subgoal_solution_frame,
                   _pages_tg_sol_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
  if (value == 0 || value == 15) { /* table_subgoal_answer_frames */
    GET_PAGE_STATS(stats, struct table_subgoal_answer_frame, _pages_tg_ans_fr);
    bytes += PgEnt_bytes_in_use(stats);
    if (value != 0)
      structs = PgEnt_strs_in_use(stats);
  }
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */

  if (value == 0) { /* total_memory */
#ifdef USE_PAGES_MALLOC
    structs = PgEnt_pages_in_use(GLOBAL_pages_alloc) * Yap_page_size;
#else
    structs = bytes;
#endif /* USE_PAGES_MALLOC */
  }
  if (structs == -1)
    return (FALSE);
  tbytes = Deref(ARG2);
  tstructs = Deref(ARG3);
  if (IsVarTerm(tbytes)) {
    YapBind((CELL *)tbytes, MkIntTerm(bytes));
  } else if (IsIntTerm(tbytes) && IntOfTerm(tbytes) != bytes)
    return (FALSE);
  if (IsVarTerm(tstructs)) {
    YapBind((CELL *)tstructs, MkIntTerm(structs));
  } else if (IsIntTerm(tstructs) && IntOfTerm(tstructs) != structs)
    return (FALSE);
  return (TRUE);
}

/******************************
**      Local functions      **
******************************/

#ifdef YAPOR
static inline realtime current_time(void) {
#define TIME_RESOLUTION 1000000
  struct timeval tempo;
  gettimeofday(&tempo, NULL);
  return ((realtime)tempo.tv_sec + (realtime)tempo.tv_usec / TIME_RESOLUTION);
  /* to get time as Yap */
  /*
  double now, interval;
  Yap_cputime_interval(&now, &interval);
  return ((realtime)now);
  */
}
#endif /* YAPOR */

#ifdef TABLING
static inline struct page_statistics show_statistics_table_entries(FILE *out) {
  SHOW_PAGE_STATS(out, struct table_entry, _pages_tab_ent,
                  "Table entries:                ");
}

#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
static inline struct page_statistics
show_statistics_subgoal_entries(FILE *out) {
  SHOW_PAGE_STATS(out, struct subgoal_entry, _pages_sg_ent,
                  "Subgoal entries:              ");
}
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */

static inline struct page_statistics show_statistics_subgoal_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct subgoal_frame, _pages_sg_fr,
                  "Subgoal frames:               ");
}

static inline struct page_statistics
show_statistics_dependency_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct dependency_frame, _pages_dep_fr,
                  "Dependency frames:            ");
}

static inline struct page_statistics
show_statistics_subgoal_trie_nodes(FILE *out) {
  SHOW_PAGE_STATS(out, struct subgoal_trie_node, _pages_sg_node,
                  "Subgoal trie nodes:           ");
}

static inline struct page_statistics
show_statistics_subgoal_trie_hashes(FILE *out) {
  SHOW_PAGE_STATS(out, struct subgoal_trie_hash, _pages_sg_hash,
                  "Subgoal trie hashes:          ");
}

static inline struct page_statistics
show_statistics_answer_trie_nodes(FILE *out) {
  SHOW_PAGE_STATS(out, struct answer_trie_node, _pages_ans_node,
                  "Answer trie nodes:            ");
}

static inline struct page_statistics
show_statistics_answer_trie_hashes(FILE *out) {
  SHOW_PAGE_STATS(out, struct answer_trie_hash, _pages_ans_hash,
                  "Answer trie hashes:           ");
}

#if defined(THREADS_FULL_SHARING)
static inline struct page_statistics
show_statistics_answer_ref_nodes(FILE *out) {
  SHOW_PAGE_STATS(out, struct answer_ref_node, _pages_ans_ref_node,
                  "Answer ref nodes:             ");
}
#endif /* THREADS_FULL_SHARING */

static inline struct page_statistics
show_statistics_global_trie_nodes(FILE *out) {
  SHOW_PAGE_STATS(out, struct global_trie_node, _pages_gt_node,
                  "Global trie nodes:            ");
}

static inline struct page_statistics
show_statistics_global_trie_hashes(FILE *out) {
  SHOW_PAGE_STATS(out, struct global_trie_hash, _pages_gt_hash,
                  "Global trie hashes:           ");
}
#endif /* TABLING */

#ifdef YAPOR
static inline struct page_statistics show_statistics_or_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct or_frame, _pages_or_fr,
                  "Or-frames:                    ");
}

static inline struct page_statistics
show_statistics_query_goal_solution_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct query_goal_solution_frame, _pages_qg_sol_fr,
                  "Query goal solution frames:   ");
}

static inline struct page_statistics
show_statistics_query_goal_answer_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct query_goal_answer_frame, _pages_qg_ans_fr,
                  "Query goal answer frames:     ");
}
#endif /* YAPOR */

#if defined(YAPOR) && defined(TABLING)
static inline struct page_statistics
show_statistics_suspension_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct suspension_frame, _pages_susp_fr,
                  "Suspension frames:            ");
}

#ifdef TABLING_INNER_CUTS
static inline struct page_statistics
show_statistics_table_subgoal_solution_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct table_subgoal_solution_frame, _pages_tg_sol_fr,
                  "Table subgoal solution frames:");
}

static inline struct page_statistics
show_statistics_table_subgoal_answer_frames(FILE *out) {
  SHOW_PAGE_STATS(out, struct table_subgoal_answer_frame, _pages_tg_ans_fr,
                  "Table subgoal answer frames:  ");
}
#endif /* TABLING_INNER_CUTS */
#endif /* YAPOR && TABLING */
#endif /* YAPOR || TABLING */
