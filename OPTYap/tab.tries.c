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

#ifndef TAB_TRiES_H
#define TAB_TRIES_H 1

#include "Yap.h"
#ifdef TABLING
#include "Yatom.h"
#include "YapHeap.h"
#include "YapEval.h"
#include "tab.macros.h"

static inline sg_node_ptr
subgoal_trie_check_insert_entry(tab_ent_ptr, sg_node_ptr, Term USES_REGS);
static inline sg_node_ptr
subgoal_trie_check_insert_gt_entry(tab_ent_ptr, sg_node_ptr, Term USES_REGS);
static inline ans_node_ptr
answer_trie_check_insert_entry(sg_fr_ptr, ans_node_ptr, Term, int USES_REGS);
static inline ans_node_ptr
answer_trie_check_insert_gt_entry(sg_fr_ptr, ans_node_ptr, Term, int USES_REGS);
static inline gt_node_ptr global_trie_check_insert_entry(gt_node_ptr,
                                                         Term USES_REGS);
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static inline gt_node_ptr global_trie_check_insert_gt_entry(gt_node_ptr,
                                                            Term USES_REGS);
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
static inline sg_node_ptr subgoal_search_loop(tab_ent_ptr, sg_node_ptr, Term,
                                              int *, CELL **USES_REGS);
static inline sg_node_ptr subgoal_search_terms_loop(tab_ent_ptr, sg_node_ptr,
                                                    Term, int *,
                                                    CELL **USES_REGS);
static inline ans_node_ptr answer_search_loop(sg_fr_ptr, ans_node_ptr, Term,
                                              int *USES_REGS);
static inline ans_node_ptr answer_search_terms_loop(sg_fr_ptr, ans_node_ptr,
                                                    Term, int *USES_REGS);
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static inline gt_node_ptr
subgoal_search_global_trie_terms_loop(Term, int *, CELL **, CELL *USES_REGS);
static inline gt_node_ptr answer_search_global_trie_terms_loop(Term, int *,
                                                               CELL *USES_REGS);
#else
static inline gt_node_ptr subgoal_search_global_trie_loop(Term, int *,
                                                          CELL **USES_REGS);
static inline gt_node_ptr answer_search_global_trie_loop(Term, int *USES_REGS);
#endif /* GLOBAL_TRIE_MODE */
static inline CELL *load_answer_loop(ans_node_ptr USES_REGS);
static inline CELL *load_substitution_loop(gt_node_ptr, int *, CELL *USES_REGS);
static inline CELL *exec_substitution_loop(gt_node_ptr, CELL **,
                                           CELL *USES_REGS);
#ifdef MODE_DIRECTED_TABLING
static inline ans_node_ptr answer_search_min_max(sg_fr_ptr, ans_node_ptr, Term,
                                                 int USES_REGS);
static inline ans_node_ptr answer_search_sum(sg_fr_ptr, ans_node_ptr,
                                             Term USES_REGS);
static void invalidate_answer_trie(ans_node_ptr, sg_fr_ptr, int USES_REGS);
#endif /* MODE_DIRECTED_TABLING */

#ifdef YAPOR
#ifdef TABLING_INNER_CUTS
static int update_answer_trie_branch(ans_node_ptr, ans_node_ptr);
#else /* YAPOR && ! TABLING_INNER_CUTS */
static int update_answer_trie_branch(ans_node_ptr);
#endif
#else /* ! YAPOR */
static void update_answer_trie_branch(ans_node_ptr, int);
#endif
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static void free_global_trie_branch(gt_node_ptr, int USES_REGS);
#else
static void free_global_trie_branch(gt_node_ptr USES_REGS);
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
static void traverse_subgoal_trie(sg_node_ptr, char *, int, int *, int,
                                  int USES_REGS);
static void traverse_answer_trie(ans_node_ptr, char *, int, int *, int, int,
                                 int USES_REGS);
static void traverse_global_trie(gt_node_ptr, char *, int, int *, int,
                                 int USES_REGS);
static void traverse_global_trie_for_term(gt_node_ptr, char *, int *, int *,
                                          int *, int USES_REGS);
static inline void traverse_trie_node(Term, char *, int *, int *, int *,
                                      int USES_REGS);
static inline void traverse_update_arity(char *, int *, int *);

/*******************************
**      Structs & Macros      **
*******************************/

static struct trie_statistics {
  FILE *out;
  int show;
  long subgoals;
  long subgoals_incomplete;
  long subgoal_trie_nodes;
  long answers;
#ifdef TABLING_INNER_CUTS
  long answers_pruned;
#endif /* TABLING_INNER_CUTS */
  long answers_true;
  long answers_no;
  long answer_trie_nodes;
  long global_trie_terms;
  long global_trie_nodes;
  long global_trie_references;
}
#ifdef THREADS
trie_stats[MAX_THREADS];

#define TrStat_out trie_stats[worker_id].out
#define TrStat_show trie_stats[worker_id].show
#define TrStat_subgoals trie_stats[worker_id].subgoals
#define TrStat_sg_incomplete trie_stats[worker_id].subgoals_incomplete
#define TrStat_sg_nodes trie_stats[worker_id].subgoal_trie_nodes
#define TrStat_answers trie_stats[worker_id].answers
#define TrStat_answers_true trie_stats[worker_id].answers_true
#define TrStat_answers_no trie_stats[worker_id].answers_no
#define TrStat_answers_pruned trie_stats[worker_id].answers_pruned
#define TrStat_ans_nodes trie_stats[worker_id].answer_trie_nodes
#define TrStat_gt_terms trie_stats[worker_id].global_trie_terms
#define TrStat_gt_nodes trie_stats[worker_id].global_trie_nodes
#define TrStat_gt_refs trie_stats[worker_id].global_trie_references
#else /*!THREADS */
trie_stats;

#define TrStat_out trie_stats.out
#define TrStat_show trie_stats.show
#define TrStat_subgoals trie_stats.subgoals
#define TrStat_sg_incomplete trie_stats.subgoals_incomplete
#define TrStat_sg_nodes trie_stats.subgoal_trie_nodes
#define TrStat_answers trie_stats.answers
#define TrStat_answers_true trie_stats.answers_true
#define TrStat_answers_no trie_stats.answers_no
#define TrStat_answers_pruned trie_stats.answers_pruned
#define TrStat_ans_nodes trie_stats.answer_trie_nodes
#define TrStat_gt_terms trie_stats.global_trie_terms
#define TrStat_gt_nodes trie_stats.global_trie_nodes
#define TrStat_gt_refs trie_stats.global_trie_references
#endif /*THREADS */

#if defined(THREADS_SUBGOAL_SHARING) || defined(THREADS_FULL_SHARING) ||       \
    defined(THREADS_CONSUMER_SHARING)
#define IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES                         \
  if (GLOBAL_NOfThreads == 1)
#else
#define IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES
#endif /* THREADS_SUBGOAL_SHARING || THREADS_FULL_SHARING ||                   \
          THREADS_CONSUMER_SHARING */

#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES                          \
  if (GLOBAL_NOfThreads == 1)
#else
#define IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */

#define SHOW_TABLE_STR_ARRAY_SIZE 100000
#define SHOW_TABLE_ARITY_ARRAY_SIZE 10000
#define SHOW_TABLE_STRUCTURE( ...)                                    \
  if (TrStat_show == SHOW_MODE_STRUCTURE)                                      \
  fprintf(TrStat_out, __VA_ARGS__ )

#define CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(REF, MODE)                       \
  if (MODE == TRAVERSE_MODE_NORMAL && IsVarTerm(REF) &&                        \
      REF > VarIndexOfTableTerm(MAX_TABLE_VARS)) {                             \
    register gt_node_ptr gt_node = (gt_node_ptr)(REF);                         \
    TrNode_child(gt_node) =                                                    \
        (gt_node_ptr)((uintptr_t)TrNode_child(gt_node) - 1);                   \
    if (TrNode_child(gt_node) == 0)                                            \
      FREE_GLOBAL_TRIE_BRANCH(gt_node, TRAVERSE_MODE_NORMAL);                  \
  }
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
#define CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(REF, MODE)          \
  CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(REF, MODE)
#define FREE_GLOBAL_TRIE_BRANCH(NODE, MODE)                                    \
  free_global_trie_branch(NODE, MODE PASS_REGS)
#else
#define CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(REF, MODE)
#define FREE_GLOBAL_TRIE_BRANCH(NODE, MODE)                                    \
  free_global_trie_branch(NODE PASS_REGS)
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */

/******************************
** Rational Terms Support    **
******************************/
#ifdef TRIE_RATIONAL_TERMS
#include "tab.rational.h"
#endif /* RATIONAL TERM SUPPORT FOR TRIES */

/******************************
**      Local functions      **
******************************/

#define INCLUDE_SUBGOAL_TRIE_CHECK_INSERT /* subgoal_trie_check_insert_entry   \
                                             */
#define INCLUDE_ANSWER_TRIE_CHECK_INSERT  /* answer_trie_check_insert_entry */
#define INCLUDE_GLOBAL_TRIE_CHECK_INSERT  /* global_trie_check_insert_entry */
#include "tab.tries.h"
#undef INCLUDE_GLOBAL_TRIE_CHECK_INSERT
#undef INCLUDE_ANSWER_TRIE_CHECK_INSERT
#undef INCLUDE_SUBGOAL_TRIE_CHECK_INSERT

#define MODE_GLOBAL_TRIE_ENTRY
#define INCLUDE_SUBGOAL_TRIE_CHECK_INSERT /* subgoal_trie_check_insert_gt_entry \
                                             */
#define INCLUDE_ANSWER_TRIE_CHECK_INSERT  /* answer_trie_check_insert_gt_entry \
                                             */
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
#define INCLUDE_GLOBAL_TRIE_CHECK_INSERT /* global_trie_check_insert_gt_entry  \
                                            */
#endif                                   /* GLOBAL_TRIE_FOR_SUBTERMS */
#include "tab.tries.h"
#undef INCLUDE_GLOBAL_TRIE_CHECK_INSERT
#undef INCLUDE_ANSWER_TRIE_CHECK_INSERT
#undef INCLUDE_SUBGOAL_TRIE_CHECK_INSERT
#undef MODE_GLOBAL_TRIE_ENTRY

#define INCLUDE_SUBGOAL_SEARCH_LOOP /* subgoal_search_loop */
#define INCLUDE_ANSWER_SEARCH_LOOP  /* answer_search_loop */
#define INCLUDE_LOAD_ANSWER_LOOP    /* load_answer_loop */
#include "tab.tries.h"
#undef INCLUDE_LOAD_ANSWER_LOOP
#undef INCLUDE_ANSWER_SEARCH_LOOP
#undef INCLUDE_SUBGOAL_SEARCH_LOOP

#define MODE_TERMS_LOOP
#define INCLUDE_SUBGOAL_SEARCH_LOOP /* subgoal_search_terms_loop */
#define INCLUDE_ANSWER_SEARCH_LOOP  /* answer_search_terms_loop */
#ifdef TRIE_RATIONAL_TERMS
#undef TRIE_RATIONAL_TERMS
#include "tab.tries.h"
#define TRIE_RATIONAL_TERMS
#else
#include "tab.tries.h"
#endif
#undef INCLUDE_ANSWER_SEARCH_LOOP
#undef INCLUDE_SUBGOAL_SEARCH_LOOP
#undef MODE_TERMS_LOOP

#define MODE_GLOBAL_TRIE_LOOP
#define INCLUDE_SUBGOAL_SEARCH_LOOP /* subgoal_search_global_trie_(terms)_loop \
                                       */
#define INCLUDE_ANSWER_SEARCH_LOOP  /* answer_search_global_trie_(terms)_loop  \
                                       */
#define INCLUDE_LOAD_ANSWER_LOOP    /* load_substitution_loop */
#ifdef TRIE_RATIONAL_TERMS
#undef TRIE_RATIONAL_TERMS
#include "tab.tries.h"
#define TRIE_RATIONAL_TERMS
#else
#include "tab.tries.h"
#endif
#undef INCLUDE_LOAD_ANSWER_LOOP
#undef INCLUDE_ANSWER_SEARCH_LOOP
#undef INCLUDE_SUBGOAL_SEARCH_LOOP
#undef MODE_GLOBAL_TRIE_LOOP

#ifdef MODE_DIRECTED_TABLING
#define INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
#include "tab.tries.h" /* answer_search_min_max + answer_search_sum + invalidate_answer_trie */
#undef INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
#endif /* MODE_DIRECTED_TABLING */

static inline CELL *exec_substitution_loop(gt_node_ptr current_node,
                                           CELL **stack_vars_ptr,
                                           CELL *stack_terms USES_REGS) {
  /************************************************************************
                       ===========
                       |         |
                       |   ...   |
                       |         |
                       -----------
             YENV -->  |   N+1   |  <-- stack_vars
                       -----------
                       |  VAR_N  |
                       -----------
                       |   ...   |
                       -----------
                       |  VAR_0  |
                       -----------
                       |         |
                       |   ...   |
                       |         |
                       ===========
                       |         |
                       |   ...   |
                       |         |
                       -----------
               TR -->  |         |  <-- stack_terms_limit
                       -----------
                       |         |
                       |   ...   |
                       |         |
                       ----------|
                       |  TERM_N |  <-- stack_terms
                       ----------|           *
                       |   ...   |          /|\
                       ----------|           |  stack_terms_pair_offset
  (TRIE_COMPACT_PAIRS)
                       |  TERM_1 |          \|/
                       ===========           *
   LOCAL_TrailTop -->  |         |  <-- stack_terms_base (TRIE_COMPACT_PAIRS)
                       -----------
  ************************************************************************/
  CELL *stack_vars = *stack_vars_ptr;
  CELL *stack_terms_limit = (CELL *)TR;
#ifdef TRIE_COMPACT_PAIRS
#define stack_terms_base ((CELL *)LOCAL_TrailTop)
  int stack_terms_pair_offset = 0;
#endif /* TRIE_COMPACT_PAIRS */
  Term t = TrNode_entry(current_node);
  current_node = TrNode_parent(current_node);

  do {
    if (IsVarTerm(t)) {
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
      if (t > VarIndexOfTableTerm(MAX_TABLE_VARS)) {
        stack_terms = exec_substitution_loop((gt_node_ptr)t, &stack_vars,
                                             stack_terms PASS_REGS);
      } else
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
      {
        int var_index = VarIndexOfTableTerm(t);
        int vars_arity = *stack_vars;
        t = MkVarTerm();
        if (var_index >= vars_arity) {
          while (vars_arity < var_index) {
            *stack_vars-- = 0;
            vars_arity++;
          }
          *stack_vars-- = t;
          vars_arity++;
          *stack_vars = vars_arity;
        } else {
          /* do the same as in macro stack_trie_val_instr() */
          CELL aux_sub, aux_var, *vars_ptr;
          vars_ptr = stack_vars + vars_arity - var_index;
          aux_sub = *((CELL *)t);
          aux_var = *vars_ptr;
          if (aux_var == 0) {
            *vars_ptr = t;
          } else {
            if (aux_sub > aux_var) {
              if ((CELL *)aux_sub <= HR) {
                Bind_Global((CELL *)aux_sub, aux_var);
              } else if ((CELL *)aux_var <= HR) {
                Bind_Local((CELL *)aux_sub, aux_var);
              } else {
                Bind_Local((CELL *)aux_var, aux_sub);
                *vars_ptr = aux_sub;
              }
            } else {
              if ((CELL *)aux_var <= HR) {
                Bind_Global((CELL *)aux_var, aux_sub);
                *vars_ptr = aux_sub;
              } else if ((CELL *)aux_sub <= HR) {
                Bind_Local((CELL *)aux_var, aux_sub);
                *vars_ptr = aux_sub;
              } else {
                Bind_Local((CELL *)aux_sub, aux_var);
              }
            }
          }
        }
        AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
        STACK_PUSH_UP(t, stack_terms);
      }
    } else if (IsAtomOrIntTerm(t)) {
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
      STACK_PUSH_UP(t, stack_terms);
    } else if (IsPairTerm(t)) {
#ifdef TRIE_COMPACT_PAIRS
      if (t == CompactPairInit) {
        Term *stack_aux = stack_terms_base - stack_terms_pair_offset;
        Term head, tail = STACK_POP_UP(stack_aux);
        while (STACK_NOT_EMPTY(stack_aux, stack_terms)) {
          head = STACK_POP_UP(stack_aux);
          tail = MkPairTerm(head, tail);
        }
        stack_terms = stack_terms_base - stack_terms_pair_offset;
        stack_terms_pair_offset = (int)STACK_POP_DOWN(stack_terms);
        STACK_PUSH_UP(tail, stack_terms);
      } else { /* CompactPairEndList / CompactPairEndTerm */
        Term last;
        AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
        last = STACK_POP_DOWN(stack_terms);
        STACK_PUSH_UP(stack_terms_pair_offset, stack_terms);
        stack_terms_pair_offset = (int)(stack_terms_base - stack_terms);
        if (t == CompactPairEndList)
          STACK_PUSH_UP(TermNil, stack_terms);
        STACK_PUSH_UP(last, stack_terms);
      }
#else
      Term head = STACK_POP_DOWN(stack_terms);
      Term tail = STACK_POP_DOWN(stack_terms);
      t = MkPairTerm(head, tail);
      STACK_PUSH_UP(t, stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = (Functor)RepAppl(t);
      if (f == FunctorDouble) {
        union {
          Term t_dbl[sizeof(Float) / sizeof(Term)];
          Float dbl;
        } u;
        t = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        u.t_dbl[0] = t;
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
        t = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        u.t_dbl[1] = t;
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
        current_node = TrNode_parent(current_node);
        t = MkFloatTerm(u.dbl);
      } else if (f == FunctorLongInt) {
        Int li = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        current_node = TrNode_parent(current_node);
        t = MkLongIntTerm(li);
      } else {
        int f_arity = ArityOfFunctor(f);
        t = Yap_MkApplTerm(f, f_arity, stack_terms);
        stack_terms += f_arity;
      }
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
      STACK_PUSH_UP(t, stack_terms);
    }
    t = TrNode_entry(current_node);
    current_node = TrNode_parent(current_node);
  } while (current_node);

  *stack_vars_ptr = stack_vars;
  return stack_terms;

#ifdef TRIE_COMPACT_PAIRS
#undef stack_terms_base
#endif /* TRIE_COMPACT_PAIRS */
}

#ifdef YAPOR
#ifdef TABLING_INNER_CUTS
static int update_answer_trie_branch(ans_node_ptr previous_node,
                                     ans_node_ptr current_node) {
  int ltt;
  if (!IS_ANSWER_LEAF_NODE(current_node)) {
    if (TrNode_child(current_node)) {
      TrNode_instr(TrNode_child(current_node)) -= 1; /* retry --> try */
      update_answer_trie_branch(NULL, TrNode_child(current_node));
      if (TrNode_child(current_node))
        goto update_next_trie_branch;
    }
    /* node belonging to a pruned answer */
    if (previous_node) {
      TrNode_next(previous_node) = TrNode_next(current_node);
      FREE_ANSWER_TRIE_NODE(current_node);
      if (TrNode_next(previous_node)) {
        return update_answer_trie_branch(previous_node,
                                         TrNode_next(previous_node));
      } else {
        TrNode_instr(previous_node) -= 2; /* retry --> trust : try --> do */
        return 0;
      }
    } else {
      TrNode_child(TrNode_parent(current_node)) = TrNode_next(current_node);
      if (TrNode_next(current_node)) {
        TrNode_instr(TrNode_next(current_node)) -= 1; /* retry --> try */
        update_answer_trie_branch(NULL, TrNode_next(current_node));
      }
      FREE_ANSWER_TRIE_NODE(current_node);
      return 0;
    }
  }
update_next_trie_branch:
  if (TrNode_next(current_node)) {
    ltt =
        1 + update_answer_trie_branch(current_node, TrNode_next(current_node));
  } else {
    TrNode_instr(current_node) -= 2; /* retry --> trust : try --> do */
    ltt = 1;
  }

  TrNode_or_arg(current_node) = ltt;
  TrNode_instr(current_node) = Yap_opcode(TrNode_instr(current_node));
  return ltt;
}
#else /* YAPOR && ! TABLING_INNER_CUTS */
static int update_answer_trie_branch(ans_node_ptr current_node) {
  int ltt;
  if (!IS_ANSWER_LEAF_NODE(current_node)) {
    TrNode_instr(TrNode_child(current_node)) -= 1; /* retry --> try */
    update_answer_trie_branch(TrNode_child(current_node));
  }
  if (TrNode_next(current_node)) {
    ltt = 1 + update_answer_trie_branch(TrNode_next(current_node));
  } else {
    TrNode_instr(current_node) -= 2; /* retry --> trust : try --> do */
    ltt = 1;
  }
  TrNode_or_arg(current_node) = ltt;
  TrNode_instr(current_node) = Yap_opcode(TrNode_instr(current_node));
  return ltt;
}
#endif
#else /* ! YAPOR */
static void update_answer_trie_branch(ans_node_ptr current_node, int position) {
  if (!IS_ANSWER_LEAF_NODE(current_node))
    update_answer_trie_branch(TrNode_child(current_node),
                              TRAVERSE_POSITION_FIRST); /* retry --> try */
  if (position == TRAVERSE_POSITION_FIRST) {
    ans_node_ptr next = TrNode_next(current_node);
    if (next) {
      while (TrNode_next(next)) {
        update_answer_trie_branch(next,
                                  TRAVERSE_POSITION_NEXT); /* retry --> retry */
        next = TrNode_next(next);
      }
      update_answer_trie_branch(next,
                                TRAVERSE_POSITION_LAST); /* retry --> trust */
    } else
      position += TRAVERSE_POSITION_LAST; /* try --> do */
  }
  TrNode_instr(current_node) =
      Yap_opcode(TrNode_instr(current_node) - position);
  return;
}

#endif /* YAPOR */

#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static void free_global_trie_branch(gt_node_ptr current_node,
                                    int mode USES_REGS) {
  Term t = TrNode_entry(current_node);
#else
static void free_global_trie_branch(gt_node_ptr current_node USES_REGS) {
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
  gt_node_ptr parent_node, child_node;

  parent_node = TrNode_parent(current_node);
  child_node = TrNode_child(parent_node);
  if (IS_GLOBAL_TRIE_HASH(child_node)) {
    gt_hash_ptr hash = (gt_hash_ptr)child_node;
    gt_node_ptr *bucket =
        Hash_buckets(hash) +
        HASH_ENTRY(TrNode_entry(current_node), Hash_num_buckets(hash));
    int num_nodes = --Hash_num_nodes(hash);
    child_node = *bucket;
    if (child_node != current_node) {
      while (TrNode_next(child_node) != current_node)
        child_node = TrNode_next(child_node);
      TrNode_next(child_node) = TrNode_next(current_node);
      CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(t, mode);
      FREE_GLOBAL_TRIE_NODE(current_node);
    } else {
      *bucket = TrNode_next(current_node);
      CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(t, mode);
      FREE_GLOBAL_TRIE_NODE(current_node);
      if (num_nodes == 0) {
        FREE_BUCKETS(Hash_buckets(hash));
        FREE_GLOBAL_TRIE_HASH(hash);
        if (parent_node != GLOBAL_root_gt) {
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
          if (mode == TRAVERSE_MODE_NORMAL) {
            if (IsApplTerm(t)) {
              Functor f = (Functor)RepAppl(t);
              if (f == FunctorDouble)
                mode = TRAVERSE_MODE_DOUBLE;
              else if (f == FunctorLongInt)
                mode = TRAVERSE_MODE_LONGINT;
              else if (f == FunctorBigInt || f == FunctorString)
                mode = TRAVERSE_MODE_BIGINT_OR_STRING;
              else
                mode = TRAVERSE_MODE_NORMAL;
            } else
              mode = TRAVERSE_MODE_NORMAL;
          } else if (mode == TRAVERSE_MODE_LONGINT)
            mode = TRAVERSE_MODE_LONGINT_END;
        } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING)
          mode = TRAVERSE_MODE_BIGINT_OR_STRING_END;
        else if (mode == TRAVERSE_MODE_DOUBLE)
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
          mode = TRAVERSE_MODE_DOUBLE2;
        else if (mode == TRAVERSE_MODE_DOUBLE2)
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
          mode = TRAVERSE_MODE_DOUBLE_END;
        else
          mode = TRAVERSE_MODE_NORMAL;
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
        FREE_GLOBAL_TRIE_BRANCH(parent_node, mode);
      } else
        TrNode_child(parent_node) = NULL;
    }
  }
}
else if (child_node != current_node) {
  while (TrNode_next(child_node) != current_node)
    child_node = TrNode_next(child_node);
  TrNode_next(child_node) = TrNode_next(current_node);
  CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(t, mode);
  FREE_GLOBAL_TRIE_NODE(current_node);
}
else if (TrNode_next(current_node) == NULL) {
  CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(t, mode);
  FREE_GLOBAL_TRIE_NODE(current_node);
  if (parent_node != GLOBAL_root_gt) {
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
    if (mode == TRAVERSE_MODE_NORMAL) {
      if (IsApplTerm(t)) {
        Functor f = (Functor)RepAppl(t);
        if (f == FunctorDouble)
          mode = TRAVERSE_MODE_DOUBLE;
        else if (f == FunctorLongInt)
          mode = TRAVERSE_MODE_LONGINT;
        else if (f == FunctorBigInt || f == FunctorString)
          mode = TRAVERSE_MODE_BIGINT_OR_STRING;
        else
          mode = TRAVERSE_MODE_NORMAL;
      } else
        mode = TRAVERSE_MODE_NORMAL;
    } else if (mode == TRAVERSE_MODE_LONGINT) {
      mode = TRAVERSE_MODE_LONGINT_END;
    } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING) {
      mode = TRAVERSE_MODE_BIGINT_OR_STRING_END;
    } else if (mode == TRAVERSE_MODE_DOUBLE)
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      mode = TRAVERSE_MODE_DOUBLE2;
    else if (mode == TRAVERSE_MODE_DOUBLE2)
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      mode = TRAVERSE_MODE_DOUBLE_END;
    else
      mode = TRAVERSE_MODE_NORMAL;
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
    FREE_GLOBAL_TRIE_BRANCH(parent_node, mode);
  } else
    TrNode_child(parent_node) = NULL;
}
else {
  TrNode_child(parent_node) = TrNode_next(current_node);
  CHECK_DECREMENT_GLOBAL_TRIE_FOR_SUBTERMS_REFERENCE(t, mode);
  FREE_GLOBAL_TRIE_NODE(current_node);
}
return;
}

static void traverse_subgoal_trie(sg_node_ptr current_node, char *str,
                                  int str_index, int *arity, int mode,
                                  int position USES_REGS) {
  int *current_arity = NULL, current_str_index = 0, current_mode = 0;

  /* test if hashing */
  if (IS_SUBGOAL_TRIE_HASH(current_node)) {
    sg_node_ptr *bucket, *last_bucket;
    sg_hash_ptr hash;
    hash = (sg_hash_ptr)current_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    do {
      if (*bucket) {
        traverse_subgoal_trie(*bucket, str, str_index, arity, mode,
                              TRAVERSE_POSITION_FIRST PASS_REGS);
        memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
        if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
          str[str_index - 1] = ',';
#else
        if (arity[arity[0]] == -1)
          str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      }
    } while (++bucket != last_bucket);
    free(current_arity);
    return;
  }

  /* save current state if first sibling node */
  if (position == TRAVERSE_POSITION_FIRST) {
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    current_str_index = str_index;
    current_mode = mode;
  }

  /* process current trie node */
  TrStat_sg_nodes++;
  traverse_trie_node(TrNode_entry(current_node), str, &str_index, arity, &mode,
                     TRAVERSE_TYPE_SUBGOAL PASS_REGS);

  /* show answers ... */
  if (IS_SUBGOAL_LEAF_NODE(current_node)) {
    sg_fr_ptr sg_fr = get_subgoal_frame(current_node);
    if (sg_fr) {
      TrStat_subgoals++;
      str[str_index] = 0;
      SHOW_TABLE_STRUCTURE("%s.\n", str);
      TrStat_ans_nodes++;
      if (SgFr_first_answer(sg_fr) == NULL) {
        if (SgFr_state(sg_fr) < complete) {
          TrStat_sg_incomplete++;
          SHOW_TABLE_STRUCTURE("    ---> INCOMPLETE\n");
        } else {
          TrStat_answers_no++;
          SHOW_TABLE_STRUCTURE("    NO\n");
        }
      } else if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
        TrStat_answers_true++;
        SHOW_TABLE_STRUCTURE("    TRUE\n");
      } else {
        arity[0] = 0;
        traverse_answer_trie(TrNode_child(SgFr_answer_trie(sg_fr)),
                             &str[str_index], 0, arity, 0, TRAVERSE_MODE_NORMAL,
                             TRAVERSE_POSITION_FIRST PASS_REGS);
        if (SgFr_state(sg_fr) < complete) {
          TrStat_sg_incomplete++;
          SHOW_TABLE_STRUCTURE("    ---> INCOMPLETE\n");
        }
      }
    }
  } else
    /* ... or continue with child node */
    traverse_subgoal_trie(TrNode_child(current_node), str, str_index, arity,
                          mode, TRAVERSE_POSITION_FIRST PASS_REGS);
  /* restore the initial state and continue with sibling nodes */
  if (position == TRAVERSE_POSITION_FIRST) {
    str_index = current_str_index;
    mode = current_mode;
    current_node = TrNode_next(current_node);
    while (current_node) {
      memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
      if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
        str[str_index - 1] = ',';
#else
      if (arity[arity[0]] == -1)
        str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      traverse_subgoal_trie(current_node, str, str_index, arity, mode,
                            TRAVERSE_POSITION_NEXT PASS_REGS);
      current_node = TrNode_next(current_node);
    }
    free(current_arity);
  }
  return;
}

static void traverse_answer_trie(ans_node_ptr current_node, char *str,
                                 int str_index, int *arity, int var_index,
                                 int mode, int position USES_REGS) {
  int *current_arity = NULL, current_str_index = 0, current_var_index = 0,
      current_mode = 0;

  /* test if hashing */
  if (IS_ANSWER_TRIE_HASH(current_node)) {
    ans_node_ptr *bucket, *last_bucket;
    ans_hash_ptr hash;
    hash = (ans_hash_ptr)current_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    do {
      if (*bucket) {
        traverse_answer_trie(*bucket, str, str_index, arity, var_index, mode,
                             TRAVERSE_POSITION_FIRST PASS_REGS);
        memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
        if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
          str[str_index - 1] = ',';
#else
        if (arity[arity[0]] == -1)
          str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      }
    } while (++bucket != last_bucket);
    free(current_arity);
    return;
  }

  /* save current state if first sibling node */
  if (position == TRAVERSE_POSITION_FIRST) {
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    current_str_index = str_index;
    current_var_index = var_index;
    current_mode = mode;
  }

  /* print VAR if starting a term */
  if (arity[0] == 0 && mode == TRAVERSE_MODE_NORMAL) {
    str_index += sprintf(&str[str_index], "    VAR%d: ", var_index);
    var_index++;
  }

  /* process current trie node */
  TrStat_ans_nodes++;
  traverse_trie_node(TrNode_entry(current_node), str, &str_index, arity, &mode,
                     TRAVERSE_TYPE_ANSWER PASS_REGS);

  /* show answer .... */
  if (IS_ANSWER_LEAF_NODE(current_node)) {
    TrStat_answers++;
    str[str_index] = 0;
    SHOW_TABLE_STRUCTURE("%s\n", str);
  }
#ifdef TABLING_INNER_CUTS
  /* ... or continue with pruned node */
  else if (TrNode_child(current_node) == NULL) {
    TrStat_answers++;
    TrStat_answers_pruned++;
  }
#endif /* TABLING_INNER_CUTS */
  /* ... or continue with child node */
  else
    traverse_answer_trie(TrNode_child(current_node), str, str_index, arity,
                         var_index, mode, TRAVERSE_POSITION_FIRST PASS_REGS);

  /* restore the initial state and continue with sibling nodes */
  if (position == TRAVERSE_POSITION_FIRST) {
    str_index = current_str_index;
    var_index = current_var_index;
    mode = current_mode;
    current_node = TrNode_next(current_node);
    while (current_node) {
      memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
      if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
        str[str_index - 1] = ',';
#else
      if (arity[arity[0]] == -1)
        str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      traverse_answer_trie(current_node, str, str_index, arity, var_index, mode,
                           TRAVERSE_POSITION_NEXT PASS_REGS);
      current_node = TrNode_next(current_node);
    }
    free(current_arity);
  }

  return;
}

static void traverse_global_trie(gt_node_ptr current_node, char *str,
                                 int str_index, int *arity, int mode,
                                 int position USES_REGS) {
  int *current_arity = NULL, current_str_index = 0, current_mode = 0;

  /* test if hashing */
  if (IS_GLOBAL_TRIE_HASH(current_node)) {
    gt_node_ptr *bucket, *last_bucket;
    gt_hash_ptr hash;
    hash = (gt_hash_ptr)current_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    do {
      if (*bucket) {
        traverse_global_trie(*bucket, str, str_index, arity, mode,
                             TRAVERSE_POSITION_FIRST PASS_REGS);
        memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
        if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
          str[str_index - 1] = ',';
#else
        if (arity[arity[0]] == -1)
          str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      }
    } while (++bucket != last_bucket);
    free(current_arity);
    return;
  }

  /* save current state if first sibling node */
  if (position == TRAVERSE_POSITION_FIRST) {
    current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    current_str_index = str_index;
    current_mode = mode;
  }

  /* process current trie node */
  TrStat_gt_nodes++;
  traverse_trie_node(TrNode_entry(current_node), str, &str_index, arity, &mode,
                     TRAVERSE_TYPE_GT_SUBGOAL PASS_REGS);

  /* continue with child node ... */
  if (arity[0] != 0 || mode != TRAVERSE_MODE_NORMAL)
    traverse_global_trie(TrNode_child(current_node), str, str_index, arity,
                         mode, TRAVERSE_POSITION_FIRST PASS_REGS);
  /* ... or show term */
  else {
    TrStat_gt_terms++;
    str[str_index] = 0;
    SHOW_TABLE_STRUCTURE("  TERMx" UInt_FORMAT ": %s\n",
                         (CELL)TrNode_child(current_node), str);
  }

  /* restore the initial state and continue with sibling nodes */
  if (position == TRAVERSE_POSITION_FIRST) {
    str_index = current_str_index;
    mode = current_mode;
    current_node = TrNode_next(current_node);
    while (current_node) {
      memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
#ifdef TRIE_COMPACT_PAIRS
      if (arity[arity[0]] == -2 && str[str_index - 1] != '[')
        str[str_index - 1] = ',';
#else
      if (arity[arity[0]] == -1)
        str[str_index - 1] = '|';
#endif /* TRIE_COMPACT_PAIRS */
      traverse_global_trie(current_node, str, str_index, arity, mode,
                           TRAVERSE_POSITION_NEXT PASS_REGS);
      current_node = TrNode_next(current_node);
    }
    free(current_arity);
  }

  return;
}

static void traverse_global_trie_for_term(gt_node_ptr current_node, char *str,
                                          int *str_index, int *arity, int *mode,
                                          int type USES_REGS) {
  if (TrNode_parent(current_node) != GLOBAL_root_gt)
    traverse_global_trie_for_term(TrNode_parent(current_node), str, str_index,
                                  arity, mode, type PASS_REGS);
  traverse_trie_node(TrNode_entry(current_node), str, str_index, arity, mode,
                     type PASS_REGS);
  return;
}

static inline void traverse_trie_node(Term t, char *str, int *str_index_ptr,
                                      int *arity, int *mode_ptr,
                                      int type USES_REGS) {
  int mode = *mode_ptr;
  int str_index = *str_index_ptr;

  /* test the node type */
  if (mode == TRAVERSE_MODE_DOUBLE) {
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    arity[0]++;
    arity[arity[0]] = (int)t;
    mode = TRAVERSE_MODE_DOUBLE2;
  } else if (mode == TRAVERSE_MODE_DOUBLE2) {
    union {
      Term t_dbl[sizeof(Float) / sizeof(Term)];
      Float dbl;
    } u;
    u.dbl = 0.0;
    u.t_dbl[0] = t;
    u.t_dbl[1] = (Term)arity[arity[0]];
    arity[0]--;
#else  /* SIZEOF_DOUBLE == SIZEOF_INT_P */
    union {
      Term t_dbl[sizeof(Float) / sizeof(Term)];
      Float dbl;
    } u;
    u.t_dbl[0] = t;
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
    str_index += sprintf(&str[str_index], "%.15g", u.dbl);
    traverse_update_arity(str, &str_index, arity);
    if (type == TRAVERSE_TYPE_SUBGOAL)
      mode = TRAVERSE_MODE_NORMAL;
    else /* TRAVERSE_TYPE_ANSWER || TRAVERSE_TYPE_GT_SUBGOAL ||
            TRAVERSE_TYPE_GT_ANSWER */
      mode = TRAVERSE_MODE_DOUBLE_END;
  } else if (mode == TRAVERSE_MODE_DOUBLE_END) {
    mode = TRAVERSE_MODE_NORMAL;
  } else if (mode == TRAVERSE_MODE_LONGINT) {
    Int li = (Int)t;
    str_index += sprintf(&str[str_index], Int_FORMAT, li);
    traverse_update_arity(str, &str_index, arity);
    if (type == TRAVERSE_TYPE_SUBGOAL)
      mode = TRAVERSE_MODE_NORMAL;
    else /* TRAVERSE_TYPE_ANSWER || TRAVERSE_TYPE_GT_SUBGOAL ||
            TRAVERSE_TYPE_GT_ANSWER */
      mode = TRAVERSE_MODE_LONGINT_END;
  } else if (mode == TRAVERSE_MODE_LONGINT_END) {
    mode = TRAVERSE_MODE_NORMAL;
  } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING) {
    str_index += Yap_OpaqueTermToString(AbsAppl((CELL *)t), str + str_index, 0);
    traverse_update_arity(str, &str_index, arity);
    if (type == TRAVERSE_TYPE_SUBGOAL)
      mode = TRAVERSE_MODE_NORMAL;
    else /* TRAVERSE_TYPE_ANSWER || TRAVERSE_TYPE_GT_SUBGOAL ||
            TRAVERSE_TYPE_GT_ANSWER */
      mode = TRAVERSE_MODE_BIGINT_OR_STRING_END;
  } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING_END) {
    mode = TRAVERSE_MODE_NORMAL;
  } else if (IsVarTerm(t)) {
#ifdef TRIE_RATIONAL_TERMS
    if (t > VarIndexOfTableTerm(MAX_TABLE_VARS) &&
        TrNode_child((gt_node_ptr)t) !=
            (gt_node_ptr)1) { // TODO: substitute the != 1 test to something
                              // more appropriate
      /* Rational term */
      str_index += sprintf(&str[str_index], "**");
      traverse_update_arity(str, &str_index, arity);
    } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        if (t > VarIndexOfTableTerm(MAX_TABLE_VARS)) {
      TrStat_gt_refs++;
      /* (type % 2 + 2): TRAVERSE_TYPE_ANSWER  --> TRAVERSE_TYPE_GT_ANSWER  */
      /* (type % 2 + 2): TRAVERSE_TYPE_SUBGOAL --> TRAVERSE_TYPE_GT_SUBGOAL */
      traverse_global_trie_for_term((gt_node_ptr)t, str, &str_index, arity,
                                    &mode, type % 2 + 2 PASS_REGS);
    } else {
      if (type == TRAVERSE_TYPE_SUBGOAL || type == TRAVERSE_TYPE_GT_SUBGOAL)
        str_index += sprintf(&str[str_index], "VAR%d", VarIndexOfTableTerm(t));
      else /* TRAVERSE_TYPE_ANSWER || TRAVERSE_TYPE_GT_ANSWER */
        str_index +=
            sprintf(&str[str_index], "ANSVAR%d", VarIndexOfTableTerm(t));
      traverse_update_arity(str, &str_index, arity);
    }
  } else if (IsIntTerm(t)) {
    str_index += sprintf(&str[str_index], Int_FORMAT, IntOfTerm(t));
    traverse_update_arity(str, &str_index, arity);
  } else if (IsAtomTerm(t)) {
#ifndef TRIE_COMPACT_PAIRS
    if (arity[arity[0]] == -1 && t == TermNil) {
      str[str_index - 1] = ']';
      arity[0]--;
    } else
#endif /* TRIE_COMPACT_PAIRS */
      str_index += sprintf(&str[str_index], "%s", AtomName(AtomOfTerm(t)));
    traverse_update_arity(str, &str_index, arity);
  } else if (IsPairTerm(t)) {
#ifdef TRIE_COMPACT_PAIRS
    if (t == CompactPairEndList)
      arity[arity[0]] = -1;
    else if (t == CompactPairEndTerm) {
      str[str_index - 1] = '|';
      arity[arity[0]] = -1;
#else
    if (arity[arity[0]] == -1) {
      str[str_index - 1] = ',';
      arity[arity[0]] = -2;
#endif /* TRIE_COMPACT_PAIRS */
    } else {
      str_index += sprintf(&str[str_index], "[");
      arity[0]++;
      arity[arity[0]] = -2;
    }
  } else if (IsApplTerm(t)) {
    Functor f = (Functor)RepAppl(t);
    if (f == FunctorDouble) {
      mode = TRAVERSE_MODE_DOUBLE;
    } else if (f == FunctorLongInt) {
      mode = TRAVERSE_MODE_LONGINT;
    } else if (f == FunctorBigInt || f == FunctorString) {
      mode = TRAVERSE_MODE_BIGINT_OR_STRING;
    } else if (f == FunctorComma) {
      if (arity[arity[0]] != -3) {
        str_index += sprintf(&str[str_index], "(");
        arity[0]++;
      }
      arity[arity[0]] = -4;
    } else {
      str_index += sprintf(&str[str_index], "%s(", AtomName(NameOfFunctor(f)));
      arity[0]++;
      arity[arity[0]] = ArityOfFunctor(f);
    }
  }

  *mode_ptr = mode;
  *str_index_ptr = str_index;
  return;
}

static inline void traverse_update_arity(char *str, int *str_index_ptr,
                                         int *arity) {
  int str_index = *str_index_ptr;
  while (arity[0]) {
    if (arity[arity[0]] > 0) {
      arity[arity[0]]--;
      if (arity[arity[0]] == 0) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else {
        str_index += sprintf(&str[str_index], ",");
        break;
      }
    } else {
      if (arity[arity[0]] == -4) {
        str_index += sprintf(&str[str_index], ",");
        arity[arity[0]] = -3;
        break;
      } else if (arity[arity[0]] == -3) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else if (arity[arity[0]] == -2) {
#ifdef TRIE_COMPACT_PAIRS
        str_index += sprintf(&str[str_index], ",");
#else
        str_index += sprintf(&str[str_index], "|");
        arity[arity[0]] = -1;
#endif /* TRIE_COMPACT_PAIRS */
        break;
      } else if (arity[arity[0]] == -1) {
        str_index += sprintf(&str[str_index], "]");
        arity[0]--;
      }
    }
  }
  *str_index_ptr = str_index;
}

/*******************************
**      Global functions      **
*******************************/

sg_fr_ptr subgoal_search(yamop *preg, CELL **Yaddr) {
  CACHE_REGS
  CELL *stack_vars;
  int i, subs_arity, pred_arity;
  tab_ent_ptr tab_ent;
  sg_fr_ptr sg_fr;
  sg_node_ptr current_sg_node;
#ifdef MODE_DIRECTED_TABLING
  int *mode_directed, aux_mode_directed[MAX_TABLE_VARS];
  int subs_pos = 0;
#endif /* MODE_DIRECTED_TABLING */

  stack_vars = *Yaddr;
  subs_arity = 0;
  pred_arity = preg->y_u.Otapl.s;
  tab_ent = preg->y_u.Otapl.te;
  current_sg_node = get_insert_subgoal_trie(tab_ent PASS_REGS);
  LOCK_SUBGOAL_TRIE(tab_ent);

#ifdef MODE_DIRECTED_TABLING
  mode_directed = TabEnt_mode_directed(tab_ent);
  if (mode_directed) {
    int old_subs_arity = subs_arity;
    for (i = 1; i <= pred_arity; i++) {
      int j = MODE_DIRECTED_GET_ARG(mode_directed[i - 1]) + 1;
      current_sg_node =
          subgoal_search_loop(tab_ent, current_sg_node, Deref(XREGS[j]),
                              &subs_arity, &stack_vars PASS_REGS);
      if (subs_arity != old_subs_arity) {
        if (subs_pos &&
            MODE_DIRECTED_GET_MODE(aux_mode_directed[subs_pos - 1]) ==
                MODE_DIRECTED_GET_MODE(mode_directed[i - 1])) {
          /* same mode as before -> use the current entry in the
           * aux_mode_directed[] array */
          aux_mode_directed[subs_pos - 1] +=
              MODE_DIRECTED_SET(subs_arity - old_subs_arity, 0);
        } else {
          /* new mode -> init a new entry in the aux_mode_directed[] array */
          aux_mode_directed[subs_pos] =
              MODE_DIRECTED_SET(subs_arity - old_subs_arity,
                                MODE_DIRECTED_GET_MODE(mode_directed[i - 1]));
          subs_pos++;
        }
        old_subs_arity = subs_arity;
      }
    }
  } else
#endif /* MODE_DIRECTED_TABLING */
      if (IsMode_GlobalTrie(TabEnt_mode(tab_ent))) {
    for (i = 1; i <= pred_arity; i++)
      current_sg_node =
          subgoal_search_terms_loop(tab_ent, current_sg_node, Deref(XREGS[i]),
                                    &subs_arity, &stack_vars PASS_REGS);
  } else {
    for (i = 1; i <= pred_arity; i++)
      current_sg_node =
          subgoal_search_loop(tab_ent, current_sg_node, Deref(XREGS[i]),
                              &subs_arity, &stack_vars PASS_REGS);
  }

  STACK_PUSH_UP(subs_arity, stack_vars);
  *Yaddr = stack_vars++;
  /* reset variables */
  while (subs_arity--) {
    Term t = STACK_POP_DOWN(stack_vars);
    RESET_VARIABLE(t);
  }

  sg_fr_ptr *sg_fr_end =
      get_insert_subgoal_frame_addr(current_sg_node PASS_REGS);
#ifndef THREADS
  LOCK_SUBGOAL_NODE(current_sg_node);
#endif /* !THREADS */
  if (*sg_fr_end == NULL) {
/* new tabled subgoal */
#ifdef MODE_DIRECTED_TABLING
    if (subs_pos) {
      ALLOC_BLOCK(mode_directed, subs_pos * sizeof(int), int);
      memmove((void *)mode_directed, (void *)aux_mode_directed,
             subs_pos * sizeof(int));
    } else
      mode_directed = NULL;
#endif /* MODE_DIRECTED_TABLING */
#if !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
    new_subgoal_frame(sg_fr, preg, mode_directed);
    *sg_fr_end = sg_fr;
#ifndef _MSC_VER
    __sync_synchronize();
#endif
    TAG_AS_SUBGOAL_LEAF_NODE(current_sg_node);
    UNLOCK_SUBGOAL_NODE(current_sg_node);
#else /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    sg_ent_ptr sg_ent =
        (sg_ent_ptr)UNTAG_SUBGOAL_NODE(TrNode_sg_ent(current_sg_node));
    new_subgoal_frame(sg_fr, sg_ent);
#ifdef THREADS_CONSUMER_SHARING
    SgFr_state(sg_fr) = ready_external;
#else
    SgFr_state(sg_fr) = ready;
#endif /* THREADS_CONSUMER_SHARING */
    if (SgEnt_sg_ent_state(sg_ent) == ready) {
      LOCK(SgEnt_lock(sg_ent));
      if (SgEnt_sg_ent_state(sg_ent) == ready) {
        SgEnt_code(sg_ent) = preg;
        SgEnt_init_mode_directed_fields(sg_ent, mode_directed);
        SgEnt_sg_ent_state(sg_ent) = evaluating;
#ifdef THREADS_CONSUMER_SHARING
        SgEnt_gen_worker(sg_ent) = worker_id;
        SgFr_state(sg_fr) = ready;
#endif /* THREADS_CONSUMER_SHARING */
      }
      UNLOCK(SgEnt_lock(sg_ent));
    }
    *sg_fr_end = sg_fr;
#endif /* !THREADS_FULL_SHARING && !THREADS_CONSUMER_SHARING */
  } else {
/* repeated tabled subgoal */
#ifndef THREADS
    UNLOCK_SUBGOAL_NODE(current_sg_node);
#endif /* !THREADS */
    sg_fr = (sg_fr_ptr)UNTAG_SUBGOAL_NODE(*sg_fr_end);
#ifdef LIMIT_TABLING
    if (SgFr_state(sg_fr) <= ready) { /* incomplete or ready */
      remove_from_global_sg_fr_list(sg_fr);
    }
#endif /* LIMIT_TABLING */
  }
  UNLOCK_SUBGOAL_TRIE(tab_ent);
  return sg_fr;
}

ans_node_ptr answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr) {
#define subs_arity *subs_ptr
  CACHE_REGS
  CELL *stack_vars;
  int i, vars_arity;
  ans_node_ptr current_ans_node;

  vars_arity = 0;
  current_ans_node = SgFr_answer_trie(sg_fr);

  if (IsMode_GlobalTrie(TabEnt_mode(SgFr_tab_ent(sg_fr)))) {
    for (i = subs_arity; i >= 1; i--) {
      TABLING_ERROR_CHECKING(answer_search, IsNonVarTerm(subs_ptr[i]));
      current_ans_node = answer_search_terms_loop(
          sg_fr, current_ans_node, Deref(subs_ptr[i]), &vars_arity PASS_REGS);
    }
  } else {
    for (i = subs_arity; i >= 1; i--) {
      TABLING_ERROR_CHECKING(answer_search, IsNonVarTerm(subs_ptr[i]));
      current_ans_node = answer_search_loop(
          sg_fr, current_ans_node, Deref(subs_ptr[i]), &vars_arity PASS_REGS);
    }
  }

  /* reset variables */
  stack_vars = (CELL *)TR;
  while (vars_arity--) {
    Term t = STACK_POP_DOWN(stack_vars);
    RESET_VARIABLE(t);
  }

  return current_ans_node;
#undef subs_arity
}

#ifdef MODE_DIRECTED_TABLING
ans_node_ptr mode_directed_answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr) {
#define subs_arity *subs_ptr
  CACHE_REGS
  CELL *stack_vars;
  int i, j, vars_arity;
  ans_node_ptr current_ans_node, invalid_ans_node;
  int *mode_directed;

  vars_arity = 0;
  current_ans_node = SgFr_answer_trie(sg_fr);
  invalid_ans_node = NULL;
  mode_directed = SgFr_mode_directed(sg_fr);
  j = 0;
  i = subs_arity;
  while (i) {
    int mode = MODE_DIRECTED_GET_MODE(mode_directed[j]);
    int n_subs = MODE_DIRECTED_GET_ARG(mode_directed[j]);
    do {
      TABLING_ERROR_CHECKING(answer_search, IsNonVarTerm(subs_ptr[i]));
      if (mode == MODE_DIRECTED_INDEX || mode == MODE_DIRECTED_ALL) {
        current_ans_node = answer_search_loop(
            sg_fr, current_ans_node, Deref(subs_ptr[i]), &vars_arity PASS_REGS);
      } else {
        LOCK_ANSWER_NODE(current_ans_node);
        if (TrNode_child(current_ans_node) == NULL) {
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
          struct answer_trie_node
              virtual_ans_node; /* necessary because the answer_search_loop()
                                   procedure also locks the parent node */
          ans_node_ptr parent_ans_node = current_ans_node;
          AnsNode_init_lock_field(&virtual_ans_node);
          TrNode_parent(&virtual_ans_node) = NULL;
          TrNode_child(&virtual_ans_node) = NULL;
          current_ans_node =
              answer_search_loop(sg_fr, &virtual_ans_node, Deref(subs_ptr[i]),
                                 &vars_arity PASS_REGS);
          TrNode_child(parent_ans_node) = TrNode_child(&virtual_ans_node);
          TrNode_parent(TrNode_child(&virtual_ans_node)) = parent_ans_node;
#else
          current_ans_node =
              answer_search_loop(sg_fr, current_ans_node, Deref(subs_ptr[i]),
                                 &vars_arity PASS_REGS);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
        } else if (mode == MODE_DIRECTED_MIN || mode == MODE_DIRECTED_MAX) {
          ans_node_ptr parent_ans_node = current_ans_node;
          invalid_ans_node = TrNode_child(
              parent_ans_node); /* by default, assume a better answer */
          current_ans_node = answer_search_min_max(
              sg_fr, current_ans_node, Deref(subs_ptr[i]), mode PASS_REGS);
          if (invalid_ans_node ==
              TrNode_child(parent_ans_node)) /* worse or equal answer */
            invalid_ans_node = NULL;
        } else if (mode == MODE_DIRECTED_SUM) {
          invalid_ans_node = TrNode_child(current_ans_node);
          current_ans_node = answer_search_sum(sg_fr, current_ans_node,
                                               Deref(subs_ptr[i]) PASS_REGS);
        } else if (mode == MODE_DIRECTED_LAST) {
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
          struct answer_trie_node
              virtual_ans_node; /* necessary because the answer_search_loop()
                                   procedure also locks the parent node */
          ans_node_ptr parent_ans_node = current_ans_node;
          invalid_ans_node = TrNode_child(parent_ans_node);
          AnsNode_init_lock_field(&virtual_ans_node);
          TrNode_parent(&virtual_ans_node) = NULL;
          TrNode_child(&virtual_ans_node) = NULL;
          current_ans_node =
              answer_search_loop(sg_fr, &virtual_ans_node, Deref(subs_ptr[i]),
                                 &vars_arity PASS_REGS);
          TrNode_child(parent_ans_node) = TrNode_child(&virtual_ans_node);
          TrNode_parent(TrNode_child(&virtual_ans_node)) = parent_ans_node;
#else
          invalid_ans_node = TrNode_child(current_ans_node);
          TrNode_child(current_ans_node) = NULL;
          current_ans_node =
              answer_search_loop(sg_fr, current_ans_node, Deref(subs_ptr[i]),
                                 &vars_arity PASS_REGS);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
        } else if (mode == MODE_DIRECTED_FIRST) {
          current_ans_node = NULL;
        } else
          Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                    "mode_directed_answer_search: unknown mode");
        UNLOCK_ANSWER_NODE(current_ans_node);
      }
      n_subs--;
      i--;
    } while (n_subs && current_ans_node);
    if (current_ans_node == NULL) /* no answer inserted */
      break;
    j++;
  }
  if (invalid_ans_node)
    invalidate_answer_trie(invalid_ans_node, sg_fr,
                           TRAVERSE_POSITION_FIRST PASS_REGS);

  /* reset variables */
  stack_vars = (CELL *)TR;
  while (vars_arity--) {
    Term t = STACK_POP_DOWN(stack_vars);
    RESET_VARIABLE(t);
  }

  return current_ans_node;
#undef subs_arity
}
#endif /* MODE_DIRECTED_TABLING */

void load_answer(ans_node_ptr current_ans_node, CELL *subs_ptr) {
  CACHE_REGS
#define subs_arity *subs_ptr
  CELL *stack_terms;
  int i;

  TABLING_ERROR_CHECKING(load_answer, H < H_FZ);
  if (subs_arity == 0)
    return;

  stack_terms = load_answer_loop(current_ans_node PASS_REGS);

  for (i = subs_arity; i >= 1; i--) {
    Term t = STACK_POP_DOWN(stack_terms);
    YapBind((CELL *)subs_ptr[i], t);
  }
  TABLING_ERROR_CHECKING(load_answer, stack_terms != (CELL *)LOCAL_TrailTop);

  return;
#undef subs_arity
}

CELL *exec_substitution(gt_node_ptr current_node, CELL *aux_stack) {
  CACHE_REGS
#define subs_arity *subs_ptr
  CELL *stack_terms, *subs_ptr;
  Term t;

  ++aux_stack; /* skip the heap_arity entry */
  stack_terms = exec_substitution_loop(current_node, &aux_stack,
                                       (CELL *)LOCAL_TrailTop PASS_REGS);
  *--aux_stack = 0; /* restore the heap_arity entry */

  subs_ptr = aux_stack + aux_stack[1] + 2;
  t = STACK_POP_DOWN(stack_terms);
  YapBind((CELL *)subs_ptr[subs_arity], t);
  TABLING_ERROR_CHECKING(exec_substitution,
                         stack_terms != (CELL *)LOCAL_TrailTop);
  *subs_ptr = subs_arity - 1;

  return aux_stack;
#undef subs_arity
}

void update_answer_trie(sg_fr_ptr sg_fr) {
  ans_node_ptr current_node;

  free_answer_hash_chain(SgFr_hash_chain(sg_fr));
  SgFr_hash_chain(sg_fr) = NULL;
  SgFr_state(sg_fr) +=
      2; /* complete --> compiled : complete_in_use --> compiled_in_use */

#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  SgFr_sg_ent_state(sg_fr) += 2; /* complete --> compiled */
#ifdef THREADS_FULL_SHARING
  if (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(sg_fr)))) {
    /* cleaning bits used by batched mode and shifting the instruction back to
     * the original place */
    ans_node_ptr leaf_ans_trie_node = SgFr_first_answer(sg_fr);
    while (TrNode_child(leaf_ans_trie_node) != NULL) {
      ANSWER_LEAF_NODE_INSTR_ABSOLUTE(leaf_ans_trie_node);
      leaf_ans_trie_node = TrNode_child(leaf_ans_trie_node);
    }
    ANSWER_LEAF_NODE_INSTR_ABSOLUTE(leaf_ans_trie_node);
  }
#endif /* THREADS_FULL_SHARING */
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
  current_node = TrNode_child(SgFr_answer_trie(sg_fr));
  if (current_node) {
#ifdef YAPOR
    TrNode_instr(current_node) -= 1;
#ifdef TABLING_INNER_CUTS
    update_answer_trie_branch(NULL, current_node);
#else
    update_answer_trie_branch(current_node);
#endif /* TABLING_INNER_CUTS */
#else  /* TABLING */
    update_answer_trie_branch(current_node, TRAVERSE_POSITION_FIRST);
#endif /* YAPOR */
  }
  return;
}

void free_subgoal_trie(sg_node_ptr current_node, int mode, int position) {
  CACHE_REGS

  if (IS_SUBGOAL_TRIE_HASH(current_node)) {
    sg_node_ptr *bucket, *last_bucket;
    sg_hash_ptr hash;
    hash = (sg_hash_ptr)current_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    do {
      if (*bucket) {
        sg_node_ptr next_node = *bucket;
        do {
          current_node = next_node;
          next_node = TrNode_next(current_node);
          free_subgoal_trie(current_node, mode, TRAVERSE_POSITION_NEXT);
        } while (next_node);
      }
    } while (++bucket != last_bucket);
    IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES {
      FREE_BUCKETS(Hash_buckets(hash));
      FREE_SUBGOAL_TRIE_HASH(hash);
    }
    return;
  }
  if (!IS_SUBGOAL_LEAF_NODE(current_node)) {
    int child_mode;
    if (mode == TRAVERSE_MODE_NORMAL) {
      Term t = TrNode_entry(current_node);
      if (IsApplTerm(t)) {
        Functor f = (Functor)RepAppl(t);
        if (f == FunctorDouble)
          child_mode = TRAVERSE_MODE_DOUBLE;
        else if (f == FunctorLongInt)
          child_mode = TRAVERSE_MODE_LONGINT;
        else if (f == FunctorBigInt || f == FunctorString)
          child_mode = TRAVERSE_MODE_BIGINT_OR_STRING;
        else
          child_mode = TRAVERSE_MODE_NORMAL;
      } else
        child_mode = TRAVERSE_MODE_NORMAL;
    } else if (mode == TRAVERSE_MODE_LONGINT) {
      child_mode = TRAVERSE_MODE_LONGINT_END;
    } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING) {
      Yap_FreeCodeSpace((char *)TrNode_entry(current_node));
      child_mode = TRAVERSE_MODE_BIGINT_OR_STRING_END;
    } else if (mode == TRAVERSE_MODE_DOUBLE) {
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      child_mode = TRAVERSE_MODE_DOUBLE2;
    } else if (mode == TRAVERSE_MODE_DOUBLE2) {
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      child_mode = TRAVERSE_MODE_DOUBLE_END;
    } else {
      child_mode = TRAVERSE_MODE_NORMAL;
    }
    free_subgoal_trie(TrNode_child(current_node), child_mode,
                      TRAVERSE_POSITION_FIRST);
  } else {
    sg_fr_ptr sg_fr = get_subgoal_frame_for_abolish(current_node PASS_REGS);
    if (sg_fr) {
      ans_node_ptr ans_node;
      free_answer_hash_chain(SgFr_hash_chain(sg_fr));
      ans_node = SgFr_answer_trie(sg_fr);
      if (TrNode_child(ans_node))
        free_answer_trie(TrNode_child(ans_node), TRAVERSE_MODE_NORMAL,
                         TRAVERSE_POSITION_FIRST);
      IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES {
        FREE_ANSWER_TRIE_NODE(ans_node);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#ifdef MODE_DIRECTED_TABLING
        if (SgEnt_mode_directed(SgFr_sg_ent(sg_fr)))
          FREE_BLOCK(SgEnt_mode_directed(SgFr_sg_ent(sg_fr)));
        if (SgFr_invalid_chain(sg_fr)) {
          ans_node_ptr current_node, next_node;
          /* free invalid answer nodes */
          current_node = SgFr_invalid_chain(sg_fr);
          SgFr_invalid_chain(sg_fr) = NULL;
          while (current_node) {
            next_node = TrNode_next(current_node);
            FREE_ANSWER_TRIE_NODE(current_node);
            current_node = next_node;
          }
        }
#endif /* MODE_DIRECTED_TABLING */
        FREE_SUBGOAL_ENTRY(SgFr_sg_ent(sg_fr));
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
      }
#ifdef LIMIT_TABLING
      remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
#if defined(MODE_DIRECTED_TABLING) && !defined(THREADS_FULL_SHARING) &&        \
    !defined(THREADS_CONSUMER_SHARING)
      if (SgFr_mode_directed(sg_fr))
        FREE_BLOCK(SgFr_mode_directed(sg_fr));
#endif /* MODE_DIRECTED_TABLING && !THREADS_FULL_SHARING &&                    \
          !THREADS_CONSUMER_SHARING */
      FREE_SUBGOAL_FRAME(sg_fr);
    }
  }
  if (position == TRAVERSE_POSITION_FIRST) {
    sg_node_ptr next_node = TrNode_next(current_node);
    IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES {
      CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(TrNode_entry(current_node), mode);
      FREE_SUBGOAL_TRIE_NODE(current_node);
    }
    while (next_node) {
      current_node = next_node;
      next_node = TrNode_next(current_node);
      free_subgoal_trie(current_node, mode, TRAVERSE_POSITION_NEXT);
    }
  } else {
    IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES {
      CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(TrNode_entry(current_node), mode);
      FREE_SUBGOAL_TRIE_NODE(current_node);
    }
  }
  return;
}

void free_answer_trie(ans_node_ptr current_node, int mode, int position) {
  CACHE_REGS

#ifdef TABLING_INNER_CUTS
  if (!IS_ANSWER_LEAF_NODE(current_node) && TrNode_child(current_node)) {
#else
  if (!IS_ANSWER_LEAF_NODE(current_node)) {
#endif /* TABLING_INNER_CUTS */
    int child_mode;
    if (mode == TRAVERSE_MODE_NORMAL) {
      Term t = TrNode_entry(current_node);
      if (IsApplTerm(t)) {
        Functor f = (Functor)RepAppl(t);
        if (f == FunctorDouble)
          child_mode = TRAVERSE_MODE_DOUBLE;
        else if (f == FunctorLongInt)
          child_mode = TRAVERSE_MODE_LONGINT;
        else if (f == FunctorBigInt || f == FunctorString)
          child_mode = TRAVERSE_MODE_BIGINT_OR_STRING;
        else
          child_mode = TRAVERSE_MODE_NORMAL;
      } else
        child_mode = TRAVERSE_MODE_NORMAL;
    } else if (mode == TRAVERSE_MODE_LONGINT) {
      child_mode = TRAVERSE_MODE_LONGINT_END;
    } else if (mode == TRAVERSE_MODE_BIGINT_OR_STRING) {
      Yap_FreeCodeSpace((char *)TrNode_entry(current_node));
      child_mode = TRAVERSE_MODE_BIGINT_OR_STRING_END;
    } else if (mode == TRAVERSE_MODE_DOUBLE) {
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      child_mode = TRAVERSE_MODE_DOUBLE2;
    } else if (mode == TRAVERSE_MODE_DOUBLE2) {
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      child_mode = TRAVERSE_MODE_DOUBLE_END;
    } else {
      child_mode = TRAVERSE_MODE_NORMAL;
    }
    free_answer_trie(TrNode_child(current_node), child_mode,
                     TRAVERSE_POSITION_FIRST);
  }
  if (position == TRAVERSE_POSITION_FIRST) {
    ans_node_ptr next_node = TrNode_next(current_node);
    IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES {
      CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(TrNode_entry(current_node), mode);
      FREE_ANSWER_TRIE_NODE(current_node);
    }
    while (next_node) {
      current_node = next_node;
      next_node = TrNode_next(current_node);
      free_answer_trie(current_node, mode, TRAVERSE_POSITION_NEXT);
    }
  } else {
    IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES {
      CHECK_DECREMENT_GLOBAL_TRIE_REFERENCE(TrNode_entry(current_node), mode);
      FREE_ANSWER_TRIE_NODE(current_node);
    }
  }
  return;
}

void free_answer_hash_chain(ans_hash_ptr hash) {
#if defined(THREADS_NO_SHARING) || defined(THREADS_SUBGOAL_SHARING)
  CACHE_REGS
#endif /* THREADS_NO_SHARING || THREADS_SUBGOAL_SHARING */

  while (hash) {
    ans_node_ptr chain_node, *bucket, *last_bucket;
    ans_hash_ptr next_hash;

    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    while (!*bucket)
      bucket++;
    chain_node = *bucket;
    TrNode_child((ans_node_ptr)UNTAG_ANSWER_NODE(TrNode_parent(chain_node))) =
        chain_node;
    while (++bucket != last_bucket) {
      if (*bucket) {
        while (TrNode_next(chain_node))
          chain_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        chain_node = *bucket;
      }
    }
    next_hash = Hash_next(hash);
    FREE_BUCKETS(Hash_buckets(hash));
    FREE_ANSWER_TRIE_HASH(hash);
    hash = next_hash;
  }
  return;
}

/*****************************************************************************************
** all threads abolish their local data structures, and the main thread also
*abolishes  **
** all shared data structures, if no other thread is running (GLOBAL_NOfThreads
*== 1).  **
*****************************************************************************************/
void abolish_table(tab_ent_ptr tab_ent) {
  CACHE_REGS
  sg_node_ptr sg_node;

#ifdef THREADS
  if (GLOBAL_NOfThreads == 1) {
    ATTACH_PAGES(_pages_tab_ent);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
    ATTACH_PAGES(_pages_sg_ent);
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
    ATTACH_PAGES(_pages_sg_fr);
    ATTACH_PAGES(_pages_dep_fr);
    ATTACH_PAGES(_pages_sg_node);
    ATTACH_PAGES(_pages_sg_hash);
    ATTACH_PAGES(_pages_ans_node);
    ATTACH_PAGES(_pages_ans_hash);
#if defined(THREADS_FULL_SHARING)
    ATTACH_PAGES(_pages_ans_ref_node);
#endif /* THREADS_FULL_SHARING */
    ATTACH_PAGES(_pages_gt_node);
    ATTACH_PAGES(_pages_gt_hash);
  }
#endif /* THREADS */
  sg_node = get_subgoal_trie_for_abolish(tab_ent PASS_REGS);
  if (sg_node) {
    if (TrNode_child(sg_node)) {
      if (TabEnt_arity(tab_ent)) {
        free_subgoal_trie(TrNode_child(sg_node), TRAVERSE_MODE_NORMAL,
                          TRAVERSE_POSITION_FIRST);
      } else {
        sg_fr_ptr sg_fr = get_subgoal_frame_for_abolish(sg_node PASS_REGS);
        if (sg_fr) {
          IF_ABOLISH_ANSWER_TRIE_SHARED_DATA_STRUCTURES {
            FREE_ANSWER_TRIE_NODE(SgFr_answer_trie(sg_fr));
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
            FREE_SUBGOAL_ENTRY(SgFr_sg_ent(sg_fr));
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
          }
#ifdef LIMIT_TABLING
          remove_from_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
          FREE_SUBGOAL_FRAME(sg_fr);
        }
      }
      IF_ABOLISH_SUBGOAL_TRIE_SHARED_DATA_STRUCTURES
      TrNode_child(sg_node) = NULL;
    }
#ifdef THREADS_NO_SHARING
    FREE_SUBGOAL_TRIE_NODE(sg_node);
#endif /* THREADS_NO_SHARING */
  }
  return;
}

void showTable(tab_ent_ptr tab_ent, int show_mode, FILE *out) {
  CACHE_REGS
  sg_node_ptr sg_node;

  TrStat_out = out;
  TrStat_show = show_mode;
  TrStat_subgoals = 0;
  TrStat_sg_incomplete = 0;
  TrStat_sg_nodes = 1;
  TrStat_answers = 0;
  TrStat_answers_true = 0;
  TrStat_answers_no = 0;
#ifdef TABLING_INNER_CUTS
  TrStat_answers_pruned = 0;
#endif /* TABLING_INNER_CUTS */
  TrStat_ans_nodes = 0;
  TrStat_gt_refs = 0;
  if (show_mode == SHOW_MODE_STATISTICS)
    fprintf(TrStat_out, "Table statistics for predicate '%s",
            AtomName(TabEnt_atom(tab_ent)));
  else /* SHOW_MODE_STRUCTURE */
    fprintf(TrStat_out, "Table structure for predicate '%s",
            AtomName(TabEnt_atom(tab_ent)));
#ifdef MODE_DIRECTED_TABLING
  if (TabEnt_mode_directed(tab_ent)) {
    int i, *mode_directed = TabEnt_mode_directed(tab_ent);
    fprintf(TrStat_out, "(");
    for (i = 0; i < TabEnt_arity(tab_ent); i++) {
      int mode = MODE_DIRECTED_GET_MODE(mode_directed[i]);
      if (mode == MODE_DIRECTED_INDEX) {
        fprintf(TrStat_out, "index");
      } else if (mode == MODE_DIRECTED_MIN) {
        fprintf(TrStat_out, "min");
      } else if (mode == MODE_DIRECTED_MAX) {
        fprintf(TrStat_out, "max");
      } else if (mode == MODE_DIRECTED_ALL) {
        fprintf(TrStat_out, "all");
      } else if (mode == MODE_DIRECTED_SUM) {
        fprintf(TrStat_out, "sum");
      } else if (mode == MODE_DIRECTED_LAST) {
        fprintf(TrStat_out, "last");
      } else if (mode == MODE_DIRECTED_FIRST) {
        fprintf(TrStat_out, "first");
      } else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "show_table: unknown mode");
      if (i != MODE_DIRECTED_GET_ARG(mode_directed[i]))
        fprintf(TrStat_out, "(ARG%d)",
                MODE_DIRECTED_GET_ARG(mode_directed[i]) + 1);
      if (i + 1 != TabEnt_arity(tab_ent))
        fprintf(TrStat_out, ",");
    }
    fprintf(TrStat_out, ")'\n");
  } else
#endif /* MODE_DIRECTED_TABLING */
    fprintf(TrStat_out, "/%d'\n", TabEnt_arity(tab_ent));
  sg_node = get_subgoal_trie(tab_ent);
  if (sg_node) {
    if (TrNode_child(sg_node)) {
      if (TabEnt_arity(tab_ent)) {
        char *str = (char *)malloc(sizeof(char) * SHOW_TABLE_STR_ARRAY_SIZE);
        int *arity = (int *)malloc(sizeof(int) * SHOW_TABLE_ARITY_ARRAY_SIZE);
        arity[0] = 1;
        arity[1] = TabEnt_arity(tab_ent);
        int str_index =
            sprintf(str, "  ?- %s(", AtomName(TabEnt_atom(tab_ent)));
        traverse_subgoal_trie(TrNode_child(sg_node), str, str_index, arity,
                              TRAVERSE_MODE_NORMAL,
                              TRAVERSE_POSITION_FIRST PASS_REGS);
        free(str);
        free(arity);
      } else {
        sg_fr_ptr sg_fr = get_subgoal_frame(sg_node);
        if (sg_fr) {
          TrStat_subgoals++;
          SHOW_TABLE_STRUCTURE("  ?- %s.\n", AtomName(TabEnt_atom(tab_ent)));
          TrStat_ans_nodes++;
          if (SgFr_first_answer(sg_fr) == NULL) {
            if (SgFr_state(sg_fr) < complete) {
              TrStat_sg_incomplete++;
              SHOW_TABLE_STRUCTURE("    ---> INCOMPLETE\n");
            } else {
              TrStat_answers_no++;
              SHOW_TABLE_STRUCTURE("    NO\n");
            }
          } else { /* SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr) */
            TrStat_answers_true++;
            SHOW_TABLE_STRUCTURE("    TRUE\n");
          }
        }
      }
    }
  }
  if (TrStat_subgoals == 0)
    SHOW_TABLE_STRUCTURE("  EMPTY\n");
  if (show_mode == SHOW_MODE_STATISTICS) {
    fprintf(TrStat_out, "  Subgoal trie structure\n");
    fprintf(TrStat_out, "    Subgoals: %ld (%ld incomplete)\n", TrStat_subgoals,
            TrStat_sg_incomplete);
    fprintf(TrStat_out, "    Subgoal trie nodes: %ld\n", TrStat_sg_nodes);
    fprintf(TrStat_out, "  Answer trie structure(s)\n");
#ifdef TABLING_INNER_CUTS
    fprintf(TrStat_out, "    Answers: %ld (%ld pruned)\n", TrStat_answers,
            TrStat_answers_pruned);
#else
    fprintf(TrStat_out, "    Answers: %ld\n", TrStat_answers);
#endif /* TABLING_INNER_CUTS */
    fprintf(TrStat_out, "    Answers 'TRUE': %ld\n", TrStat_answers_true);
    fprintf(TrStat_out, "    Answers 'NO': %ld\n", TrStat_answers_no);
    fprintf(TrStat_out, "    Answer trie nodes: %ld\n", TrStat_ans_nodes);
    fprintf(TrStat_out, "  Global trie references: %ld\n", TrStat_gt_refs);
  }
  return;
}

void showGlobalTrie(int show_mode, FILE *out) {
  CACHE_REGS

  TrStat_out = out;
  TrStat_show = show_mode;
  TrStat_gt_terms = 0;
  TrStat_gt_nodes = 1;
  TrStat_gt_refs = 0;
  if (show_mode == SHOW_MODE_STATISTICS)
    fprintf(TrStat_out, "Global trie statistics\n");
  else /* SHOW_MODE_STRUCTURE */
    fprintf(TrStat_out, "Global trie structure\n");
  if (TrNode_child(GLOBAL_root_gt)) {
    char *str = (char *)malloc(sizeof(char) * SHOW_TABLE_STR_ARRAY_SIZE);
    int *arity = (int *)malloc(sizeof(int) * SHOW_TABLE_ARITY_ARRAY_SIZE);
    arity[0] = 0;
    traverse_global_trie(TrNode_child(GLOBAL_root_gt), str, 0, arity,
                         TRAVERSE_MODE_NORMAL,
                         TRAVERSE_POSITION_FIRST PASS_REGS);
    free(str);
    free(arity);
  } else
    SHOW_TABLE_STRUCTURE("  EMPTY\n");
  if (show_mode == SHOW_MODE_STATISTICS) {
    fprintf(TrStat_out, "  Terms: %ld\n", TrStat_gt_terms);
    fprintf(TrStat_out, "  Global trie nodes: %ld\n", TrStat_gt_nodes);
    fprintf(TrStat_out, "  Global trie auto references: %ld\n", TrStat_gt_refs);
  }
  return;
}
#endif /* TABLING */

#endif
