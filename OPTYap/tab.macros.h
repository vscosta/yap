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

#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#include "opt.mavar.h"
#ifdef YAPOR
#include "or.macros.h"
#endif

#ifdef THREADS
static inline void **__get_insert_thread_bucket(void **, lockvar * USES_REGS);
static inline void **__get_thread_bucket(void ** USES_REGS);
static inline void abolish_thread_buckets(void **);
#endif /* THREADS */
static inline sg_node_ptr get_insert_subgoal_trie(tab_ent_ptr USES_REGS);
static inline sg_node_ptr __get_subgoal_trie(tab_ent_ptr USES_REGS);
static inline sg_node_ptr get_subgoal_trie_for_abolish(tab_ent_ptr USES_REGS);
static inline sg_fr_ptr *get_insert_subgoal_frame_addr(sg_node_ptr USES_REGS);
static inline sg_fr_ptr get_subgoal_frame(sg_node_ptr);
static inline sg_fr_ptr get_subgoal_frame_for_abolish(sg_node_ptr USES_REGS);
#ifdef THREADS_FULL_SHARING
static inline void __SgFr_batched_cached_answers_check_insert(sg_fr_ptr, ans_node_ptr USES_REGS);
static inline int SgFr_batched_cached_answers_check_remove(sg_fr_ptr, ans_node_ptr);
#endif /* THREADS_FULL_SHARING */
#ifdef THREADS_CONSUMER_SHARING
static inline void __add_to_tdv(int, int USES_REGS);
static inline void __check_for_deadlock(sg_fr_ptr USES_REGS);
static inline sg_fr_ptr __deadlock_detection(sg_fr_ptr USES_REGS);
#endif /* THREADS_CONSUMER_SHARING */
static inline Int __freeze_current_cp( USES_REGS1 );
static inline void __wake_frozen_cp(Int USES_REGS);
static inline void __abolish_frozen_cps_until(Int USES_REGS);
static inline void __abolish_frozen_cps_all( USES_REGS1 );
static inline void __adjust_freeze_registers( USES_REGS1 );
static inline void __mark_as_completed(sg_fr_ptr USES_REGS);
static inline void __unbind_variables(tr_fr_ptr, tr_fr_ptr USES_REGS);
static inline void __rebind_variables(tr_fr_ptr, tr_fr_ptr USES_REGS);
static inline void __restore_bindings(tr_fr_ptr, tr_fr_ptr USES_REGS);
static inline CELL *__expand_auxiliary_stack(CELL * USES_REGS);
static inline void __abolish_incomplete_subgoals(choiceptr USES_REGS);
#ifdef YAPOR
static inline void pruning_over_tabling_data_structures(void);
static inline void __collect_suspension_frames(or_fr_ptr USES_REGS);
#ifdef TIMESTAMP_CHECK
static inline susp_fr_ptr suspension_frame_to_resume(or_fr_ptr, long);
#else
static inline susp_fr_ptr suspension_frame_to_resume(or_fr_ptr);
#endif /* TIMESTAMP_CHECK */
#endif /* YAPOR */
#ifdef TABLING_INNER_CUTS
static inline void CUT_store_tg_answer(or_fr_ptr, ans_node_ptr, choiceptr, int);
static inline tg_sol_fr_ptr CUT_store_tg_answers(or_fr_ptr, tg_sol_fr_ptr, int);
static inline void CUT_validate_tg_answers(tg_sol_fr_ptr);
static inline void CUT_join_tg_solutions(tg_sol_fr_ptr *, tg_sol_fr_ptr);
static inline void CUT_join_solution_frame_tg_answers(tg_sol_fr_ptr);
static inline void CUT_join_solution_frames_tg_answers(tg_sol_fr_ptr);
static inline void CUT_free_tg_solution_frame(tg_sol_fr_ptr);
static inline void CUT_free_tg_solution_frames(tg_sol_fr_ptr);
static inline tg_sol_fr_ptr CUT_prune_tg_solution_frames(tg_sol_fr_ptr, int);
#endif /* TABLING_INNER_CUTS */



/*********************************
**      Tabling mode flags      **
*********************************/

#define Flag_Batched            0x001
#define Flag_Local              0x002
#define Flags_SchedulingMode    (Flag_Batched | Flag_Local)
#define Flag_ExecAnswers        0x010
#define Flag_LoadAnswers        0x020
#define Flags_AnswersMode       (Flag_ExecAnswers | Flag_LoadAnswers)
#define Flag_LocalTrie          0x100
#define Flag_GlobalTrie         0x200
#define Flags_TrieMode          (Flag_LocalTrie | Flag_GlobalTrie)
#define Flag_CoInductive        0x008

#define SetMode_Batched(X)      (X) = ((X) & ~Flags_SchedulingMode) | Flag_Batched
#define SetMode_Local(X)        (X) = ((X) & ~Flags_SchedulingMode) | Flag_Local
#define SetMode_ExecAnswers(X)  (X) = ((X) & ~Flags_AnswersMode) | Flag_ExecAnswers
#define SetMode_LoadAnswers(X)  (X) = ((X) & ~Flags_AnswersMode) | Flag_LoadAnswers
#define SetMode_LocalTrie(X)    (X) = ((X) & ~Flags_TrieMode) | Flag_LocalTrie
#define SetMode_GlobalTrie(X)   (X) = ((X) & ~Flags_TrieMode) | Flag_GlobalTrie
#define SetMode_CoInductive(X)  (X) = (X) | Flag_CoInductive
#define IsMode_Batched(X)       ((X) & Flag_Batched)
#define IsMode_Local(X)         ((X) & Flag_Local)
#define IsMode_ExecAnswers(X)   ((X) & Flag_ExecAnswers)
#define IsMode_LoadAnswers(X)   ((X) & Flag_LoadAnswers)
#define IsMode_LocalTrie(X)     ((X) & Flag_LocalTrie)
#define IsMode_GlobalTrie(X)    ((X) & Flag_GlobalTrie)
#define IsMode_CoInductive(X)   ((X) & Flag_CoInductive)



/******************************
**      Tabling defines      **
******************************/

/* traverse macros */
#define SHOW_MODE_STRUCTURE        0
#define SHOW_MODE_STATISTICS       1
typedef enum {
  TRAVERSE_MODE_NORMAL =       0,
  TRAVERSE_MODE_DOUBLE =       1,
  TRAVERSE_MODE_DOUBLE2 =      2,
  TRAVERSE_MODE_DOUBLE_END =   3,
  TRAVERSE_MODE_BIGINT_OR_STRING =      4,
  TRAVERSE_MODE_BIGINT_OR_STRING_END =  5,
  TRAVERSE_MODE_LONGINT =      6,
  TRAVERSE_MODE_LONGINT_END =  7
} traverse_mode_t;
/* do not change order !!! */
#define TRAVERSE_TYPE_SUBGOAL      0
#define TRAVERSE_TYPE_ANSWER       1
#define TRAVERSE_TYPE_GT_SUBGOAL   2
#define TRAVERSE_TYPE_GT_ANSWER    3
/* do not change order !!! */
#define TRAVERSE_POSITION_NEXT     0
#define TRAVERSE_POSITION_FIRST    1
#define TRAVERSE_POSITION_LAST     2

/* mode directed tabling */
#define MODE_DIRECTED_TAGBITS         0xF
#define MODE_DIRECTED_NUMBER_TAGBITS  4
#define MODE_DIRECTED_INDEX           1
#define MODE_DIRECTED_MIN             2
#define MODE_DIRECTED_MAX             3
#define MODE_DIRECTED_ALL             4
#define MODE_DIRECTED_SUM             5
#define MODE_DIRECTED_LAST            6
#define MODE_DIRECTED_FIRST           7
#define MODE_DIRECTED_SET(ARG,MODE)   (((ARG) << MODE_DIRECTED_NUMBER_TAGBITS) + MODE)
#define MODE_DIRECTED_GET_ARG(X)      ((X) >> MODE_DIRECTED_NUMBER_TAGBITS)
#define MODE_DIRECTED_GET_MODE(X)     ((X) & MODE_DIRECTED_TAGBITS)

/* LowTagBits is 3 for 32 bit-machines and 7 for 64 bit-machines */
#define NumberOfLowTagBits         (LowTagBits == 3 ? 2 : 3)
#define MakeTableVarTerm(INDEX)    ((INDEX) << NumberOfLowTagBits)
#define VarIndexOfTableTerm(TERM)  (((unsigned int) (TERM)) >> NumberOfLowTagBits)
#define VarIndexOfTerm(TERM)                                                     \
        ((((CELL) (TERM)) - GLOBAL_table_var_enumerator(0)) / sizeof(CELL))
#define IsTableVarTerm(TERM)                                                     \
        ((CELL) (TERM)) >= GLOBAL_table_var_enumerator(0) &&		 	 \
        ((CELL) (TERM)) <= GLOBAL_table_var_enumerator(MAX_TABLE_VARS - 1)
#ifdef TRIE_COMPACT_PAIRS
#define PairTermMark        NULL
#define CompactPairInit     AbsPair((Term *) 0)
#define CompactPairEndTerm  AbsPair((Term *) (LowTagBits + 1))
#define CompactPairEndList  AbsPair((Term *) (2*(LowTagBits + 1)))
#endif /* TRIE_COMPACT_PAIRS */

/* threads */
#if (_trie_retry_gterm - _trie_do_var + 1) + 1 <= 64  /* 60 (trie instructions) + 1 (ANSWER_TRIE_HASH_MARK) <= 64 */
#define ANSWER_LEAF_NODE_INSTR_BITS   6               /* 2^6 = 64 */
#define ANSWER_LEAF_NODE_INSTR_MASK   0x3F
#endif
#if SIZEOF_INT_P == 4
#define ANSWER_LEAF_NODE_MAX_THREADS  (32 - ANSWER_LEAF_NODE_INSTR_BITS)
#elif SIZEOF_INT_P == 8
#define ANSWER_LEAF_NODE_MAX_THREADS  (64 - ANSWER_LEAF_NODE_INSTR_BITS)
#else
#define ANSWER_LEAF_NODE_MAX_THREADS  OOOOPPS!!! Unknown Pointer Sizeof
#endif /* SIZEOF_INT_P */
#define ANSWER_LEAF_NODE_INSTR_RELATIVE(NODE)  (TrNode_instr(NODE) = TrNode_instr(NODE) - _trie_do_var + 1)
#define ANSWER_LEAF_NODE_INSTR_ABSOLUTE(NODE)  (TrNode_instr(NODE) = (TrNode_instr(NODE) & ANSWER_LEAF_NODE_INSTR_MASK) + _trie_do_var - 1)
#define ANSWER_LEAF_NODE_SET_WID(NODE,WID)     BITMAP_insert(TrNode_instr(NODE), WID + ANSWER_LEAF_NODE_INSTR_BITS)
#define ANSWER_LEAF_NODE_DEL_WID(NODE,WID)     BITMAP_delete(TrNode_instr(NODE), WID + ANSWER_LEAF_NODE_INSTR_BITS)
#define ANSWER_LEAF_NODE_CHECK_WID(NODE,WID)   BITMAP_member(TrNode_instr(NODE), WID + ANSWER_LEAF_NODE_INSTR_BITS)

/* choice points */
#define NORM_CP(CP)                 ((choiceptr)(CP))
#define GEN_CP(CP)                  ((struct generator_choicept *)(CP))
#define CONS_CP(CP)                 ((struct consumer_choicept *)(CP))
#define LOAD_CP(CP)                 ((struct loader_choicept *)(CP))
#ifdef DETERMINISTIC_TABLING
#define DET_GEN_CP(CP)              ((struct deterministic_generator_choicept *)(CP))
#define IS_DET_GEN_CP(CP)           (*(CELL*)(DET_GEN_CP(CP) + 1) <= MAX_TABLE_VARS)
#define IS_BATCHED_NORM_GEN_CP(CP)  (GEN_CP(CP)->cp_dep_fr == NULL)
#define IS_BATCHED_GEN_CP(CP)       (IS_DET_GEN_CP(CP) || IS_BATCHED_NORM_GEN_CP(CP))
#else
#ifdef THREADS_CONSUMER_SHARING
#define IS_BATCHED_GEN_CP(CP)       (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(GEN_CP(CP)->cp_sg_fr))))
#else
#define IS_BATCHED_GEN_CP(CP)       (GEN_CP(CP)->cp_dep_fr == NULL)
#endif /* THREADS_CONSUMER_SHARING */
#endif /* DETERMINISTIC_TABLING */

/* tagging nodes */
#define TAG_AS_SUBGOAL_LEAF_NODE(NODE)       TrNode_child(NODE) = (sg_node_ptr)((CELL) TrNode_child(NODE) | 0x1)
#define IS_SUBGOAL_LEAF_NODE(NODE)           ((CELL) TrNode_child(NODE) & 0x1)
#define TAG_AS_ANSWER_LEAF_NODE(NODE)        TrNode_parent(NODE) = (ans_node_ptr)((CELL) TrNode_parent(NODE) | 0x1)
#define IS_ANSWER_LEAF_NODE(NODE)            ((CELL) TrNode_parent(NODE) & 0x1)
#define TAG_AS_ANSWER_INVALID_NODE(NODE)     TrNode_parent(NODE) = (ans_node_ptr)((CELL) TrNode_parent(NODE) | 0x2)
#define IS_ANSWER_INVALID_NODE(NODE)         ((CELL) TrNode_parent(NODE) & 0x2)
#define UNTAG_SUBGOAL_NODE(NODE)             ((CELL) (NODE) & ~(0x1))
#define UNTAG_ANSWER_NODE(NODE)              ((CELL) (NODE) & ~(0x3))

/* trie hashes */
#define MAX_NODES_PER_TRIE_LEVEL        8
#define MAX_NODES_PER_BUCKET            (MAX_NODES_PER_TRIE_LEVEL / 2)
#define BASE_HASH_BUCKETS               64
#define HASH_ENTRY(ENTRY, NUM_BUCKETS)  ((((CELL) ENTRY) >> NumberOfLowTagBits) & (NUM_BUCKETS - 1))
#define SUBGOAL_TRIE_HASH_MARK          ((Term) MakeTableVarTerm(MAX_TABLE_VARS))
#define IS_SUBGOAL_TRIE_HASH(NODE)      (TrNode_entry(NODE) == SUBGOAL_TRIE_HASH_MARK)
#define ANSWER_TRIE_HASH_MARK           0
#define IS_ANSWER_TRIE_HASH(NODE)       (TrNode_instr(NODE) == ANSWER_TRIE_HASH_MARK)
#define GLOBAL_TRIE_HASH_MARK           ((Term) MakeTableVarTerm(MAX_TABLE_VARS))
#define IS_GLOBAL_TRIE_HASH(NODE)       (TrNode_entry(NODE) == GLOBAL_TRIE_HASH_MARK)
#define HASH_TRIE_LOCK(NODE)            GLOBAL_trie_locks((((CELL) (NODE)) >> 5) & (TRIE_LOCK_BUCKETS - 1))

/* auxiliary stack */
#define STACK_PUSH_UP(ITEM, STACK)          *--(STACK) = (CELL)(ITEM)
#define STACK_POP_UP(STACK)                 *--(STACK)
#define STACK_PUSH_DOWN(ITEM, STACK)        *(STACK)++ = (CELL)(ITEM)
#define STACK_POP_DOWN(STACK)               *(STACK)++
#define STACK_NOT_EMPTY(STACK, STACK_BASE)  (STACK) != (STACK_BASE)
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
#define AUX_STACK_CHECK_EXPAND(STACK, STACK_LIMIT)                                  \
        if ((STACK_LIMIT) >= (STACK))                                               \
          Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "stack full (AUX_STACK_CHECK_EXPAND)")
#else /* YAPOR_THREADS */
#define AUX_STACK_CHECK_EXPAND(STACK, STACK_LIMIT)                                  \
        if ((STACK_LIMIT) >= (STACK))                                               \
          STACK = expand_auxiliary_stack(STACK)
#endif /* YAPOR */
#define STACK_CHECK_EXPAND(STACK, STACK_LIMIT)                                      \
        if ((STACK_LIMIT) >= (STACK) + 4096)                                        \
          Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "stack full (STACK_CHECK_EXPAND)")



/*************************************
**      Data structures macros      **
*************************************/

#ifdef YAPOR
#define frame_with_suspensions_not_collected(OR_FR)                               \
        (OrFr_nearest_suspnode(OR_FR) == NULL)
#define find_dependency_node(SG_FR, LEADER_CP, DEP_ON_STACK)                      \
        if (SgFr_gen_worker(SG_FR) == worker_id) {                                \
          LEADER_CP = SgFr_gen_cp(SG_FR);                                         \
          DEP_ON_STACK = TRUE;                                                    \
        } else {                                                                  \
          or_fr_ptr aux_or_fr = SgFr_gen_top_or_fr(SG_FR);                        \
          while (! BITMAP_member(OrFr_members(aux_or_fr), worker_id))             \
            aux_or_fr = OrFr_next(aux_or_fr);                                     \
          LEADER_CP = GetOrFr_node(aux_or_fr);                                    \
          DEP_ON_STACK = (LEADER_CP == SgFr_gen_cp(SG_FR));                       \
        }
#define find_leader_node(LEADER_CP, DEP_ON_STACK)                                 \
        { dep_fr_ptr chain_dep_fr = LOCAL_top_dep_fr;                             \
          while (YOUNGER_CP(DepFr_cons_cp(chain_dep_fr), LEADER_CP)) {            \
            if (LEADER_CP == DepFr_leader_cp(chain_dep_fr)) {                     \
              DEP_ON_STACK |= DepFr_leader_dep_is_on_stack(chain_dep_fr);         \
              break;                                                              \
            } else if (YOUNGER_CP(LEADER_CP, DepFr_leader_cp(chain_dep_fr))) {    \
              LEADER_CP = DepFr_leader_cp(chain_dep_fr);                          \
              DEP_ON_STACK = DepFr_leader_dep_is_on_stack(chain_dep_fr);          \
              break;                                                              \
            }                                                                     \
            chain_dep_fr = DepFr_next(chain_dep_fr);                              \
          }                                                                       \
	}
#ifdef TIMESTAMP
#define DepFr_init_timestamp_field(DEP_FR)  DepFr_timestamp(DEP_FR) = 0
#else
#define DepFr_init_timestamp_field(DEP_FR)
#endif /* TIMESTAMP */
#define YAPOR_SET_LOAD(CP_PTR)  SCH_set_load(CP_PTR)
#define SgFr_init_yapor_fields(SG_FR)                                             \
        SgFr_gen_worker(SG_FR) = worker_id;                                       \
        SgFr_gen_top_or_fr(SG_FR) = LOCAL_top_or_fr
#define DepFr_init_yapor_fields(DEP_FR, DEP_ON_STACK, TOP_OR_FR)                  \
        INIT_LOCK_DEP_FR(DEP_FR);                                                 \
        DepFr_leader_dep_is_on_stack(DEP_FR) = DEP_ON_STACK;                      \
        DepFr_top_or_fr(DEP_FR) = TOP_OR_FR;                                      \
        DepFr_init_timestamp_field(DEP_FR)
#else
#define find_dependency_node(SG_FR, LEADER_CP, DEP_ON_STACK)                      \
        LEADER_CP = SgFr_gen_cp(SG_FR)
#define find_leader_node(LEADER_CP, DEP_ON_STACK)                                 \
        { dep_fr_ptr chain_dep_fr = LOCAL_top_dep_fr;                             \
          while (YOUNGER_CP(DepFr_cons_cp(chain_dep_fr), LEADER_CP)) {            \
            if (EQUAL_OR_YOUNGER_CP(LEADER_CP, DepFr_leader_cp(chain_dep_fr))) {  \
              LEADER_CP = DepFr_leader_cp(chain_dep_fr);                          \
              break;                                                              \
            }                                                                     \
            chain_dep_fr = DepFr_next(chain_dep_fr);                              \
          }                                                                       \
	}
#define YAPOR_SET_LOAD(CP_PTR)
#define SgFr_init_yapor_fields(SG_FR)
#define DepFr_init_yapor_fields(DEP_FR, DEP_ON_STACK, TOP_OR_FR)
#endif /* YAPOR */

#ifdef MODE_DIRECTED_TABLING
#define TabEnt_init_mode_directed_field(TAB_ENT, MODE_ARRAY)  \
        TabEnt_mode_directed(TAB_ENT) = MODE_ARRAY
#define SgEnt_init_mode_directed_fields(SG_ENT, MODE_ARRAY)   \
        SgEnt_invalid_chain(SG_ENT) = NULL;                   \
        SgEnt_mode_directed(SG_ENT) = MODE_ARRAY
#define SgFr_init_mode_directed_fields(SG_FR, MODE_ARRAY)     \
        SgFr_invalid_chain(SG_FR) = NULL;                     \
        SgFr_mode_directed(SG_FR) = MODE_ARRAY
#define AnsHash_init_previous_field(HASH, SG_FR)              \
        if (SgFr_hash_chain(SG_FR))                           \
          Hash_previous(SgFr_hash_chain(SG_FR)) = HASH;       \
        Hash_previous(HASH) = NULL
#else
#define TabEnt_init_mode_directed_field(TAB_ENT, MODE_ARRAY)
#define SgEnt_init_mode_directed_fields(SG_ENT, MODE_ARRAY)
#define SgFr_init_mode_directed_fields(SG_FR, MODE_ARRAY)
#define AnsHash_init_previous_field(HASH, SG_FR)
#endif /* MODE_DIRECTED_TABLING */

#if defined(YAPOR) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define INIT_LOCK_SG_FR(SG_FR)  INIT_LOCK(SgFr_lock(SG_FR))
#define LOCK_SG_FR(SG_FR)       LOCK(SgFr_lock(SG_FR))
#define UNLOCK_SG_FR(SG_FR)     UNLOCK(SgFr_lock(SG_FR))
#else
#define INIT_LOCK_SG_FR(SG_FR)
#define LOCK_SG_FR(SG_FR)
#define UNLOCK_SG_FR(SG_FR)
#endif /* YAPOR || THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */

#ifdef YAPOR
#define INIT_LOCK_DEP_FR(DEP_FR)    INIT_LOCK(DepFr_lock(DEP_FR))
#define LOCK_DEP_FR(DEP_FR)         LOCK(DepFr_lock(DEP_FR))
#define UNLOCK_DEP_FR(DEP_FR)       UNLOCK(DepFr_lock(DEP_FR))
#define IS_UNLOCKED_DEP_FR(DEP_FR)  IS_UNLOCKED(DepFr_lock(DEP_FR))
#else
#define INIT_LOCK_DEP_FR(DEF_FR)
#define LOCK_DEP_FR(DEP_FR)
#define UNLOCK_DEP_FR(DEP_FR)
#define IS_UNLOCKED_DEP_FR(DEP_FR)
#endif /* YAPOR */

#if defined(SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL) || defined(THREADS_NO_SHARING)
#define INIT_LOCK_TAB_ENT(TAB_ENT)  INIT_LOCK(TabEnt_lock(TAB_ENT))
#else
#define INIT_LOCK_TAB_ENT(TAB_ENT)
#endif /* SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL || THREADS_NO_SHARING */

#ifdef SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL
#define LOCK_SUBGOAL_TRIE(TAB_ENT)    LOCK(TabEnt_lock(TAB_ENT))
#define UNLOCK_SUBGOAL_TRIE(TAB_ENT)  UNLOCK(TabEnt_lock(TAB_ENT))
#else
#define LOCK_SUBGOAL_TRIE(TAB_ENT)
#define UNLOCK_SUBGOAL_TRIE(TAB_ENT)
#endif /* SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL */

#ifdef ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL
#define LOCK_ANSWER_TRIE(SG_FR)    LOCK_SG_FR(SG_FR)
#define UNLOCK_ANSWER_TRIE(SG_FR)  UNLOCK_SG_FR(SG_FR)
#define AnsHash_init_chain_fields(HASH, SG_FR)     \
        AnsHash_init_previous_field(HASH, SG_FR);  \
        Hash_next(HASH) = SgFr_hash_chain(SG_FR);  \
	SgFr_hash_chain(SG_FR) = HASH
#else
#define LOCK_ANSWER_TRIE(SG_FR)
#define UNLOCK_ANSWER_TRIE(SG_FR)
#define AnsHash_init_chain_fields(HASH, SG_FR)     \
        LOCK_SG_FR(SG_FR);                         \
        AnsHash_init_previous_field(HASH, SG_FR);  \
        Hash_next(HASH) = SgFr_hash_chain(SG_FR);  \
        SgFr_hash_chain(SG_FR) = HASH;             \
        UNLOCK_SG_FR(SG_FR)
#endif /* ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL */

#ifdef SUBGOAL_TRIE_LOCK_USING_NODE_FIELD
#define LOCK_SUBGOAL_NODE(NODE)       LOCK(TrNode_lock(NODE))
#define UNLOCK_SUBGOAL_NODE(NODE)     UNLOCK(TrNode_lock(NODE))
#define SgNode_init_lock_field(NODE)  INIT_LOCK(TrNode_lock(NODE))
#elif defined(SUBGOAL_TRIE_LOCK_USING_GLOBAL_ARRAY)
#define LOCK_SUBGOAL_NODE(NODE)       LOCK(HASH_TRIE_LOCK(NODE))
#define UNLOCK_SUBGOAL_NODE(NODE)     UNLOCK(HASH_TRIE_LOCK(NODE))
#define SgNode_init_lock_field(NODE)
#else
#define LOCK_SUBGOAL_NODE(NODE)
#define UNLOCK_SUBGOAL_NODE(NODE)
#define SgNode_init_lock_field(NODE)
#endif /* SUBGOAL_TRIE_LOCK_LEVEL */

#ifdef ANSWER_TRIE_LOCK_USING_NODE_FIELD
#define LOCK_ANSWER_NODE(NODE)         LOCK(TrNode_lock(NODE))
#define UNLOCK_ANSWER_NODE(NODE)       UNLOCK(TrNode_lock(NODE))
#define AnsNode_init_lock_field(NODE)  INIT_LOCK(TrNode_lock(NODE))
#elif defined(ANSWER_TRIE_LOCK_USING_GLOBAL_ARRAY)
#define LOCK_ANSWER_NODE(NODE)         LOCK(HASH_TRIE_LOCK(NODE))
#define UNLOCK_ANSWER_NODE(NODE)       UNLOCK(HASH_TRIE_LOCK(NODE))
#define AnsNode_init_lock_field(NODE)
#else
#define LOCK_ANSWER_NODE(NODE)
#define UNLOCK_ANSWER_NODE(NODE)
#define AnsNode_init_lock_field(NODE)
#endif /* ANSWER_TRIE_LOCK_LEVEL */

#ifdef GLOBAL_TRIE_LOCK_USING_NODE_FIELD
#define LOCK_GLOBAL_NODE(NODE)        LOCK(TrNode_lock(NODE))
#define UNLOCK_GLOBAL_NODE(NODE)      UNLOCK(TrNode_lock(NODE))
#define GtNode_init_lock_field(NODE)  INIT_LOCK(TrNode_lock(NODE))
#elif defined(GLOBAL_TRIE_LOCK_USING_GLOBAL_ARRAY)
#define LOCK_GLOBAL_NODE(NODE)        LOCK(HASH_TRIE_LOCK(NODE))
#define UNLOCK_GLOBAL_NODE(NODE)      UNLOCK(HASH_TRIE_LOCK(NODE))
#define GtNode_init_lock_field(NODE)
#else
#define LOCK_GLOBAL_NODE(NODE)
#define UNLOCK_GLOBAL_NODE(NODE)
#define GtNode_init_lock_field(NODE)
#endif /* GLOBAL_TRIE_LOCK_LEVEL */

#ifdef THREADS_NO_SHARING
#define TabEnt_init_subgoal_trie_field(TAB_ENT)                           \
        INIT_BUCKETS(&TabEnt_subgoal_trie(TAB_ENT), THREADS_NUM_BUCKETS)
#else
#define TabEnt_init_subgoal_trie_field(TAB_ENT)                           \
        { register sg_node_ptr sg_node;                                   \
          new_subgoal_trie_node(sg_node, 0, NULL, NULL, NULL);            \
          TabEnt_subgoal_trie(TAB_ENT) = sg_node;                         \
	}
#endif /* THREADS_NO_SHARING */

#if defined(THREADS_FULL_SHARING)
#define SgFr_init_batched_fields(SG_FR)             \
        SgFr_batched_last_answer(SG_FR) = NULL;     \
        SgFr_batched_cached_answers(SG_FR) = NULL;
#else
#define SgFr_init_batched_fields(SG_FR)
#endif /* THREADS_FULL_SHARING */

#ifdef THREADS_CONSUMER_SHARING
#define DepFr_init_external_field(DEP_FR, IS_EXTERNAL)          \
        DepFr_external(DEP_FR) = IS_EXTERNAL
#else
#define DepFr_init_external_field(DEP_FR, IS_EXTERNAL)
#endif /* THREADS_CONSUMER_SHARING */

#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define DepFr_init_last_answer_field(DEP_FR, SG_FR)                                               \
        /* start with TrNode_child(DepFr_last_answer(DEP_FR)) ... */                              \
        /* ... pointing to SgEnt_first_answer(SgFr_sg_ent(SG_FR)) */	                          \
        if (SG_FR)                                                                                \
          DepFr_last_answer(DEP_FR) = (ans_node_ptr) (                                            \
                                 (CELL) (SgFr_sg_ent((sg_fr_ptr)SG_FR)) +            \
 	                         (CELL) (&SgEnt_first_answer((sg_ent_ptr)DEP_FR)) -  \
				 (CELL) (&TrNode_child((ans_node_ptr)DEP_FR)));      \
        else                                                                                      \
          DepFr_last_answer(DEP_FR) = NULL
#else
#define DepFr_init_last_answer_field(DEP_FR, SG_FR)                                               \
        /* start with TrNode_child(DepFr_last_answer(DEP_FR)) ... */                              \
        /* ... pointing to SgFr_first_answer(SG_FR) */                                            \
        if (SG_FR)                                                                                \
          DepFr_last_answer(DEP_FR) = (ans_node_ptr) (                                            \
                                 (CELL) (SG_FR) +                                    \
                                 (CELL) (&SgFr_first_answer((sg_fr_ptr)DEP_FR)) -    \
				 (CELL) (&TrNode_child((ans_node_ptr)DEP_FR)));      \
        else                                                                                      \
          DepFr_last_answer(DEP_FR) = NULL
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */


#define new_table_entry(TAB_ENT, PRED_ENTRY, ATOM, ARITY, MODE_ARRAY)  \
        ALLOC_TABLE_ENTRY(TAB_ENT);                                    \
        INIT_LOCK_TAB_ENT(TAB_ENT);                                    \
        TabEnt_pe(TAB_ENT) = PRED_ENTRY;                               \
        TabEnt_atom(TAB_ENT) = ATOM;                                   \
        TabEnt_arity(TAB_ENT) = ARITY;                                 \
        TabEnt_flags(TAB_ENT) = 0;                                     \
        SetMode_Batched(TabEnt_flags(TAB_ENT));                        \
        SetMode_ExecAnswers(TabEnt_flags(TAB_ENT));                    \
        SetMode_LocalTrie(TabEnt_flags(TAB_ENT));                      \
        TabEnt_mode(TAB_ENT) = TabEnt_flags(TAB_ENT);                  \
        if (IsMode_Local(LOCAL_TabMode))                \
          SetMode_Local(TabEnt_mode(TAB_ENT));                         \
        if (IsMode_LoadAnswers(LOCAL_TabMode))				       \
          SetMode_LoadAnswers(TabEnt_mode(TAB_ENT));                   \
        if (IsMode_GlobalTrie(LOCAL_TabMode))				       \
          SetMode_GlobalTrie(TabEnt_mode(TAB_ENT));                    \
        TabEnt_init_mode_directed_field(TAB_ENT, MODE_ARRAY);          \
        TabEnt_init_subgoal_trie_field(TAB_ENT);                       \
        TabEnt_next(TAB_ENT) = GLOBAL_root_tab_ent;                    \
        GLOBAL_root_tab_ent = TAB_ENT

#define new_subgoal_entry(SG_ENT)                                   \
        { register ans_node_ptr ans_node;                           \
          new_answer_trie_node(ans_node, 0, 0, NULL, NULL, NULL);   \
          ALLOC_SUBGOAL_ENTRY(SG_ENT);                              \
          INIT_LOCK(SgEnt_lock(SG_ENT));		            \
          SgEnt_hash_chain(SG_ENT) = NULL;		 	    \
          SgEnt_answer_trie(SG_ENT) = ans_node;                     \
          SgEnt_first_answer(SG_ENT) = NULL;                        \
          SgEnt_last_answer(SG_ENT) = NULL;		            \
          SgEnt_sg_ent_state(SG_ENT) = ready;		 	    \
          SgEnt_active_workers(SG_ENT) = 0;                         \
          INIT_BUCKETS(&SgEnt_sg_fr(SG_ENT), THREADS_NUM_BUCKETS);  \
        }

#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define new_subgoal_frame(SG_FR, SG_ENT)		           \
        { ALLOC_SUBGOAL_FRAME(SG_FR);    	     	           \
          SgFr_sg_ent(SG_FR) = SG_ENT ; 		           \
          SgFr_init_batched_fields(SG_FR);		           \
        }

#define init_subgoal_frame(SG_FR)                                  \
        { SgFr_state(SG_FR) = evaluating;			   \
          SgFr_next(SG_FR) = LOCAL_top_sg_fr;                      \
          LOCAL_top_sg_fr = SG_FR;                                 \
	}
#else
#define new_subgoal_frame(SG_FR, CODE, MODE_ARRAY)		   \
        { register ans_node_ptr ans_node;                          \
          new_answer_trie_node(ans_node, 0, 0, NULL, NULL, NULL);  \
          ALLOC_SUBGOAL_FRAME(SG_FR);                              \
          INIT_LOCK_SG_FR(SG_FR);                                  \
          SgFr_code(SG_FR) = CODE;                                 \
          SgFr_hash_chain(SG_FR) = NULL;                           \
          SgFr_answer_trie(SG_FR) = ans_node;                      \
          SgFr_first_answer(SG_FR) = NULL;                         \
          SgFr_last_answer(SG_FR) = NULL;                          \
	  SgFr_init_mode_directed_fields(SG_FR, MODE_ARRAY);	   \
          SgFr_state(SG_FR) = ready;                               \
	}

#define init_subgoal_frame(SG_FR)                                  \
        { SgFr_init_yapor_fields(SG_FR);                           \
          SgFr_state(SG_FR) = evaluating;                          \
          SgFr_next(SG_FR) = LOCAL_top_sg_fr;                      \
          LOCAL_top_sg_fr = SG_FR;                                 \
	}
#endif /* THREADS_FULL_SHARING) || THREADS_CONSUMER_SHARING */

#define new_dependency_frame(DEP_FR, DEP_ON_STACK, TOP_OR_FR, LEADER_CP, CONS_CP, SG_FR, IS_EXTERNAL, NEXT)  \
        ALLOC_DEPENDENCY_FRAME(DEP_FR);                                                                      \
        DepFr_init_yapor_fields(DEP_FR, DEP_ON_STACK, TOP_OR_FR);                                            \
        DepFr_init_external_field(DEP_FR, IS_EXTERNAL);                                                      \
        DepFr_backchain_cp(DEP_FR) = NULL;                                                                   \
        DepFr_leader_cp(DEP_FR) = NORM_CP(LEADER_CP);                                                        \
        DepFr_cons_cp(DEP_FR) = NORM_CP(CONS_CP);                                                            \
        DepFr_init_last_answer_field(DEP_FR, SG_FR);                                                         \
        DepFr_next(DEP_FR) = NEXT

#define new_suspension_frame(SUSP_FR, TOP_OR_FR_ON_STACK, TOP_DEP, TOP_SG,             \
                             H_REG, B_REG, TR_REG, H_SIZE, B_SIZE, TR_SIZE)            \
        ALLOC_SUSPENSION_FRAME(SUSP_FR);                                               \
        SuspFr_top_or_fr_on_stack(SUSP_FR) = TOP_OR_FR_ON_STACK;                       \
        SuspFr_top_dep_fr(SUSP_FR) = TOP_DEP;                                          \
        SuspFr_top_sg_fr(SUSP_FR) = TOP_SG;                                            \
        SuspFr_global_reg(SUSP_FR) = (void *) (H_REG);                                 \
        SuspFr_local_reg(SUSP_FR) = (void *) (B_REG);                                  \
        SuspFr_trail_reg(SUSP_FR) = (void *) (TR_REG);                                 \
        ALLOC_BLOCK(SuspFr_global_start(SUSP_FR), H_SIZE + B_SIZE + TR_SIZE, void *);  \
        SuspFr_local_start(SUSP_FR) = SuspFr_global_start(SUSP_FR) + H_SIZE;           \
        SuspFr_trail_start(SUSP_FR) = SuspFr_local_start(SUSP_FR) + B_SIZE;            \
        SuspFr_global_size(SUSP_FR) = H_SIZE;                                          \
        SuspFr_local_size(SUSP_FR) = B_SIZE;                                           \
        SuspFr_trail_size(SUSP_FR) = TR_SIZE;                                          \
        memmove(SuspFr_global_start(SUSP_FR), SuspFr_global_reg(SUSP_FR), H_SIZE);      \
        memmove(SuspFr_local_start(SUSP_FR), SuspFr_local_reg(SUSP_FR), B_SIZE);        \
        memmove(SuspFr_trail_start(SUSP_FR), SuspFr_trail_reg(SUSP_FR), TR_SIZE)

#define new_subgoal_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)  \
        ALLOC_SUBGOAL_TRIE_NODE(NODE);                           \
        TrNode_entry(NODE) = ENTRY;				 \
	TrNode_child(NODE) = CHILD;	                         \
        TrNode_parent(NODE) = PARENT;                            \
        TrNode_next(NODE) = NEXT;                                \
        SgNode_init_lock_field(NODE)

#define new_answer_trie_node(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)  \
        ALLOC_ANSWER_TRIE_NODE(NODE);				       \
        TrNode_instr(NODE) = INSTR;                                    \
        TrNode_entry(NODE) = ENTRY;                                    \
        TrNode_child(NODE) = CHILD;                                    \
        TrNode_parent(NODE) = PARENT;                                  \
        TrNode_next(NODE) = NEXT;                                      \
        AnsNode_init_lock_field(NODE)

#define new_global_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)  \
        ALLOC_GLOBAL_TRIE_NODE(NODE);                           \
        TrNode_entry(NODE) = ENTRY;                             \
        TrNode_child(NODE) = CHILD;                             \
        TrNode_parent(NODE) = PARENT;                           \
        TrNode_next(NODE) = NEXT;                               \
        GtNode_init_lock_field(NODE)

#define new_answer_ref_node(NODE, ANSWER, NEXT, PREVIOUS)  \
        ALLOC_ANSWER_REF_NODE(NODE);			   \
        RefNode_answer(NODE) = ANSWER;	                   \
        RefNode_next(NODE) = NEXT;                         \
        RefNode_previous(NODE) = PREVIOUS

#define new_subgoal_trie_hash(HASH, NUM_NODES, TAB_ENT)         \
        ALLOC_SUBGOAL_TRIE_HASH(HASH);                          \
        Hash_mark(HASH) = SUBGOAL_TRIE_HASH_MARK;               \
        Hash_num_buckets(HASH) = BASE_HASH_BUCKETS;             \
        ALLOC_BUCKETS(Hash_buckets(HASH), BASE_HASH_BUCKETS);   \
        Hash_num_nodes(HASH) = NUM_NODES

#define new_answer_trie_hash(HASH, NUM_NODES, SG_FR)            \
        ALLOC_ANSWER_TRIE_HASH(HASH);                           \
        Hash_mark(HASH) = ANSWER_TRIE_HASH_MARK;                \
        Hash_num_buckets(HASH) = BASE_HASH_BUCKETS;             \
        ALLOC_BUCKETS(Hash_buckets(HASH), BASE_HASH_BUCKETS);   \
        Hash_num_nodes(HASH) = NUM_NODES;                       \
        AnsHash_init_chain_fields(HASH, SG_FR)

#define new_global_trie_hash(HASH, NUM_NODES)                   \
        ALLOC_GLOBAL_TRIE_HASH(HASH);                           \
        Hash_mark(HASH) = GLOBAL_TRIE_HASH_MARK;                \
        Hash_num_buckets(HASH) = BASE_HASH_BUCKETS;             \
        ALLOC_BUCKETS(Hash_buckets(HASH), BASE_HASH_BUCKETS);   \
	Hash_num_nodes(HASH) = NUM_NODES

#ifdef LIMIT_TABLING
#define insert_into_global_sg_fr_list(SG_FR)                                 \
        SgFr_previous(SG_FR) = GLOBAL_last_sg_fr;                            \
        SgFr_next(SG_FR) = NULL;                                             \
        if (GLOBAL_first_sg_fr == NULL)                                      \
          GLOBAL_first_sg_fr = SG_FR;                                        \
        else                                                                 \
          SgFr_next(GLOBAL_last_sg_fr) = SG_FR;                              \
        GLOBAL_last_sg_fr = SG_FR
#define remove_from_global_sg_fr_list(SG_FR)                                 \
        if (SgFr_previous(SG_FR)) {                                          \
          if ((SgFr_next(SgFr_previous(SG_FR)) = SgFr_next(SG_FR)) != NULL)  \
            SgFr_previous(SgFr_next(SG_FR)) = SgFr_previous(SG_FR);          \
          else                                                               \
            GLOBAL_last_sg_fr = SgFr_previous(SG_FR);                        \
        } else {                                                             \
          if ((GLOBAL_first_sg_fr = SgFr_next(SG_FR)) != NULL)               \
            SgFr_previous(SgFr_next(SG_FR)) = NULL;                          \
          else                                                               \
            GLOBAL_last_sg_fr = NULL;                                        \
	}                                                                    \
        if (GLOBAL_check_sg_fr == SG_FR)                                     \
          GLOBAL_check_sg_fr = SgFr_previous(SG_FR)
#else
#define insert_into_global_sg_fr_list(SG_FR)
#define remove_from_global_sg_fr_list(SG_FR)
#endif /* LIMIT_TABLING */



/******************************
**      Inline funcions      **
******************************/

#ifdef THREADS
#define get_insert_thread_bucket(b, bl) __get_insert_thread_bucket((b), (bl) PASS_REGS)

static inline void **__get_insert_thread_bucket(void **buckets, lockvar *buckets_lock USES_REGS) {

  /* direct bucket */
  if (worker_id < THREADS_DIRECT_BUCKETS)
    return buckets + worker_id;

  /* indirect bucket */
  buckets = buckets + THREADS_DIRECT_BUCKETS + (worker_id - THREADS_DIRECT_BUCKETS) / THREADS_DIRECT_BUCKETS;
  if (*buckets)
    return (void **)((char *)(*buckets) + (worker_id - THREADS_DIRECT_BUCKETS) % THREADS_DIRECT_BUCKETS);

  /* insert indirect bucket */
  LOCK(*buckets_lock);
  if (*buckets == NULL)
    ALLOC_BUCKETS(*buckets, THREADS_DIRECT_BUCKETS);
  UNLOCK(*buckets_lock);
  return (void **)((char *)(*buckets) + (worker_id - THREADS_DIRECT_BUCKETS) % THREADS_DIRECT_BUCKETS);
}

#define get_thread_bucket(b) __get_thread_bucket((b) PASS_REGS)

static inline void **__get_thread_bucket(void **buckets USES_REGS) {

  /* direct bucket */
  if (worker_id < THREADS_DIRECT_BUCKETS)
    return buckets + worker_id;

  /* indirect bucket */
  buckets = buckets + THREADS_DIRECT_BUCKETS + (worker_id - THREADS_DIRECT_BUCKETS) / THREADS_DIRECT_BUCKETS;
  if (*buckets)
    return (void **)((char *)(buckets) + (worker_id - THREADS_DIRECT_BUCKETS) % THREADS_DIRECT_BUCKETS);

  /* empty indirect bucket */
  return buckets;
}


static inline void abolish_thread_buckets(void **buckets) {
  int i;

  /* abolish indirect buckets */
  buckets += THREADS_NUM_BUCKETS;
  for (i = 0; i < THREADS_INDIRECT_BUCKETS; i++) {
    if (*--buckets) {
      FREE_BUCKETS(*buckets);
      *buckets = NULL;
    }
  }

#if defined(THREADS_SUBGOAL_SHARING)
  /* abolish direct buckets */
  buckets -= THREADS_DIRECT_BUCKETS;
  FREE_BUCKETS(buckets);
#endif /* THREADS_SUBGOAL_SHARING */
}
#endif /* THREADS */


static inline sg_node_ptr get_insert_subgoal_trie(tab_ent_ptr tab_ent USES_REGS) {
#ifdef THREADS_NO_SHARING
  sg_node_ptr *sg_node_addr = (sg_node_ptr *) get_insert_thread_bucket((void **) &TabEnt_subgoal_trie(tab_ent), &TabEnt_lock(tab_ent));
  if (*sg_node_addr == NULL) {
    new_subgoal_trie_node(*sg_node_addr, 0, NULL, NULL, NULL);
  }
  return *sg_node_addr;
#else
  return TabEnt_subgoal_trie(tab_ent);
#endif /* THREADS_NO_SHARING */
}

#define get_subgoal_trie(te) __get_subgoal_trie((te) PASS_REGS)

static inline sg_node_ptr __get_subgoal_trie(tab_ent_ptr tab_ent USES_REGS) {
#ifdef THREADS_NO_SHARING
  sg_node_ptr *sg_node_addr = (sg_node_ptr *) get_thread_bucket((void **) &TabEnt_subgoal_trie(tab_ent));
  return *sg_node_addr;
#else
  return TabEnt_subgoal_trie(tab_ent);
#endif /* THREADS_NO_SHARING */
}


static inline sg_node_ptr get_subgoal_trie_for_abolish(tab_ent_ptr tab_ent USES_REGS) {
#ifdef THREADS_NO_SHARING
  sg_node_ptr *sg_node_addr = (sg_node_ptr *) get_thread_bucket((void **) &TabEnt_subgoal_trie(tab_ent));
  sg_node_ptr sg_node = *sg_node_addr;
  *sg_node_addr = NULL;
  if (GLOBAL_NOfThreads == 1)
    abolish_thread_buckets((void **) &TabEnt_subgoal_trie(tab_ent));
  return sg_node;
#else
  return TabEnt_subgoal_trie(tab_ent);
#endif /* THREADS_NO_SHARING */
}


static inline sg_fr_ptr *get_insert_subgoal_frame_addr(sg_node_ptr sg_node USES_REGS) {
  sg_fr_ptr *sg_fr_addr = (sg_fr_ptr *) &TrNode_sg_fr(sg_node);
#if defined(THREADS_SUBGOAL_SHARING) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  if (*sg_fr_addr == NULL) {
    LOCK_SUBGOAL_NODE(sg_node);
    if (*sg_fr_addr == NULL) {
#if defined(THREADS_SUBGOAL_SHARING)
      ALLOC_BUCKETS(TrNode_sg_fr(sg_node), THREADS_NUM_BUCKETS);
#elif defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
      sg_ent_ptr sg_ent;
      new_subgoal_entry(sg_ent);
      TrNode_sg_ent(sg_node) = (sg_node_ptr) sg_ent;
#endif
      TAG_AS_SUBGOAL_LEAF_NODE(sg_node);
    }
    UNLOCK_SUBGOAL_NODE(sg_node);
  }
  sg_fr_addr = (sg_fr_ptr *) get_insert_thread_bucket(
#if defined(THREADS_SUBGOAL_SHARING)
                                (void **) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node)),
#elif defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
                                (void **) &SgEnt_sg_fr((sg_ent_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node))),
#endif
#ifdef SUBGOAL_TRIE_LOCK_USING_NODE_FIELD
                                   &TrNode_lock(sg_node)
#elif defined(SUBGOAL_TRIE_LOCK_USING_GLOBAL_ARRAY)
                                   &HASH_TRIE_LOCK(sg_node)
#endif
                             );
#endif /* THREADS_SUBGOAL_SHARING || THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
  return sg_fr_addr;
}


static inline sg_fr_ptr get_subgoal_frame(sg_node_ptr sg_node) {
#if defined(THREADS_SUBGOAL_SHARING)
  sg_fr_ptr *sg_fr_addr = (sg_fr_ptr *) get_thread_bucket((void **) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node)));
  return *sg_fr_addr;
#elif defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  sg_fr_ptr *sg_fr_addr = (sg_fr_ptr *) get_thread_bucket((void **) &SgEnt_sg_fr((sg_ent_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node))));
  return *sg_fr_addr;
#else
  return (sg_fr_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node));
#endif
}


static inline sg_fr_ptr get_subgoal_frame_for_abolish(sg_node_ptr sg_node USES_REGS) {
#if defined(THREADS_SUBGOAL_SHARING)
  sg_fr_ptr *sg_fr_addr = (sg_fr_ptr *) get_thread_bucket((void **) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node)));
  sg_fr_ptr sg_fr = *sg_fr_addr;
  if (GLOBAL_NOfThreads == 1)
    abolish_thread_buckets((void **) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node)));
  else
    *sg_fr_addr = NULL;
  return sg_fr;
#elif defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  sg_fr_ptr *sg_fr_addr = (sg_fr_ptr *) get_thread_bucket((void **) &SgEnt_sg_fr((sg_ent_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node))));
  sg_fr_ptr sg_fr = *sg_fr_addr;
  if (GLOBAL_NOfThreads == 1)
    abolish_thread_buckets((void **) &SgEnt_sg_fr((sg_ent_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node))));
  else
    *sg_fr_addr = NULL;
  return sg_fr;
#else
  return (sg_fr_ptr) UNTAG_SUBGOAL_NODE(TrNode_sg_fr(sg_node));
#endif
}


#ifdef THREADS_FULL_SHARING
#define SgFr_batched_cached_answers_check_insert(s, a) __SgFr_batched_cached_answers_check_insert((s), (a) PASS_REGS)
static inline void SgFr_batched_cached_answers_check_insert(sg_fr_ptr sg_fr, ans_node_ptr ans_node USES_REGS) {

  if (SgFr_batched_last_answer(sg_fr) == NULL)
    SgFr_batched_last_answer(sg_fr) = SgFr_first_answer(sg_fr);
  else
    SgFr_batched_last_answer(sg_fr) = TrNode_child(SgFr_batched_last_answer(sg_fr)) ;

  while (SgFr_batched_last_answer(sg_fr) != ans_node) {
    struct answer_ref_node *ref_node = NULL;
    if (SgFr_batched_cached_answers(sg_fr) == NULL) {
      new_answer_ref_node(ref_node, SgFr_batched_last_answer(sg_fr), NULL, NULL);
    } else {
      new_answer_ref_node(ref_node, SgFr_batched_last_answer(sg_fr), SgFr_batched_cached_answers(sg_fr), NULL);
      RefNode_previous(SgFr_batched_cached_answers(sg_fr)) = ref_node;
    }
    SgFr_batched_cached_answers(sg_fr) = ref_node;
    SgFr_batched_last_answer(sg_fr) = TrNode_child(SgFr_batched_last_answer(sg_fr)) ;			
  }
  if (ans_node != NULL)
    /* new answer */
    SgFr_batched_last_answer(sg_fr) = ans_node;
  else
    /* repeated answer */
    SgFr_batched_last_answer(sg_fr) = SgFr_last_answer(sg_fr);

  return;
}

#define SgFr_batched_cached_answers_check_remove(s, a) __SgFr_batched_cached_answers_check_remove((s), (a) PASS_REGS)

static inline int __SgFr_batched_cached_answers_check_remove(sg_fr_ptr sg_fr, ans_node_ptr ans_node USES_REgS) {
  struct answer_ref_node *local_uncons_ans;

  local_uncons_ans = SgFr_batched_cached_answers(sg_fr) ; 
  while ( local_uncons_ans != NULL ) {				 	                
    if ( RefNode_answer(local_uncons_ans) == ans_node )			                
      break;								                
    local_uncons_ans = RefNode_next(local_uncons_ans) ;			                
  }									                
  if ( local_uncons_ans == NULL )                                                       
    return 1; 

  /*remove node from buffer */		
  if (RefNode_previous(local_uncons_ans) == NULL)	{
    SgFr_batched_cached_answers(sg_fr) = RefNode_next(local_uncons_ans) ; 
    if (SgFr_batched_cached_answers(sg_fr) != NULL) 
      RefNode_previous(SgFr_batched_cached_answers(sg_fr)) = NULL;
  } else{
    RefNode_next(RefNode_previous(local_uncons_ans)) = RefNode_next(local_uncons_ans);
    if (RefNode_next(local_uncons_ans) != NULL) 
      RefNode_previous(RefNode_next(local_uncons_ans)) = RefNode_previous(local_uncons_ans);
  }
  FREE_ANSWER_REF_NODE(local_uncons_ans);
  return 0;
}
#endif /* THREADS_FULL_SHARING */


#ifdef THREADS_CONSUMER_SHARING

#define add_to_tdv(w, wd) __add_to_tdv((w), (wd) PASS_REGS)

static inline void __add_to_tdv(int wid, int wid_dep USES_REGS) {
  // thread wid next of thread wid_dep
  /* check before insert */
  int c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
  do {
    if (c_wid == wid_dep)
      break;
    c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
  } while( c_wid != wid );
  if (c_wid == wid_dep)
    return;

  if (wid < wid_dep) {
    LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid)));
    LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid_dep)));
  } else {
    LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid_dep)));
    LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid)));
  }
  
  c_wid = ThDepFr_next(GLOBAL_th_dep_fr(wid));
  do {
    if (c_wid == wid_dep)
      break;
    c_wid = ThDepFr_next(GLOBAL_th_dep_fr(c_wid));
  } while( c_wid != wid );
  if ( c_wid == wid ){
    // joining the two different SCCs
    int t = ThDepFr_next(GLOBAL_th_dep_fr(wid));
    ThDepFr_next(GLOBAL_th_dep_fr(wid)) = ThDepFr_next(GLOBAL_th_dep_fr(wid_dep));
    ThDepFr_next(GLOBAL_th_dep_fr(wid_dep)) = t;
  }

  INFO_THREADS("add_to_tdv (2) :tdv_next[%d] = %d tdv_next[%d]= %d", wid, ThDepFr_next(GLOBAL_th_dep_fr(wid)), wid_dep, ThDepFr_next(GLOBAL_th_dep_fr(wid_dep)));

  UNLOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid)));
  UNLOCK(ThDepFr_lock(GLOBAL_th_dep_fr(wid_dep)));
  return;
}

#define check_for_deadlock(s) __check_for_deadlock((s) PASS_REGS)

static inline void __check_for_deadlock(sg_fr_ptr sg_fr USES_REGS) {
  sg_fr_ptr local_sg_fr = deadlock_detection(sg_fr);

  if (local_sg_fr){
    LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(worker_id)));
    if (YOUNGER_CP(DepFr_leader_cp(LOCAL_top_dep_fr),SgFr_gen_cp(local_sg_fr)))
      DepFr_leader_cp(LOCAL_top_dep_fr) = SgFr_gen_cp(local_sg_fr);

    UNLOCK(ThDepFr_lock(GLOBAL_th_dep_fr(worker_id)));
  }
  return; 
}

#define deadlock_detection(s) __deadlock_detection((s) PASS_REGS)

static inline sg_fr_ptr __deadlock_detection(sg_fr_ptr sg_fr USES_REGS) {
  sg_fr_ptr remote_sg_fr = REMOTE_top_sg_fr(SgFr_gen_worker(sg_fr));

  while( SgFr_sg_ent(remote_sg_fr) != SgFr_sg_ent(sg_fr)){
    sg_fr_ptr local_sg_fr = SgFr_next(LOCAL_top_sg_fr);
    while(local_sg_fr){
      if (      SgFr_sg_ent(local_sg_fr)   == SgFr_sg_ent(remote_sg_fr) || 
         (      SgFr_gen_worker(remote_sg_fr) != SgFr_gen_worker(sg_fr)       &&  /* jump to other chain */
                SgFr_gen_worker(remote_sg_fr) != worker_id                      &&  /* not jumping to the chain of the thread that is detecting the deadlock */
	        deadlock_detection(remote_sg_fr))){
	
	sg_fr_ptr leader_remote_sg_fr;
	do 
	  leader_remote_sg_fr = SgFr_next(remote_sg_fr);
	while(SgFr_sg_ent(leader_remote_sg_fr) != SgFr_sg_ent(sg_fr));
	LOCK(ThDepFr_lock(GLOBAL_th_dep_fr(SgFr_gen_worker(leader_remote_sg_fr)))); 

	if (YOUNGER_CP(DepFr_leader_cp(REMOTE_top_dep_fr(SgFr_gen_worker(leader_remote_sg_fr))),SgFr_gen_cp(leader_remote_sg_fr)))
	  DepFr_leader_cp(REMOTE_top_dep_fr(SgFr_gen_worker(leader_remote_sg_fr))) = SgFr_gen_cp(leader_remote_sg_fr);
	UNLOCK(ThDepFr_lock(GLOBAL_th_dep_fr(SgFr_gen_worker(leader_remote_sg_fr))));
	
	add_to_tdv(SgFr_gen_worker(local_sg_fr),SgFr_gen_worker(leader_remote_sg_fr));

      	return local_sg_fr;
      }
      local_sg_fr = SgFr_next(local_sg_fr);
    }
    remote_sg_fr = SgFr_next(remote_sg_fr);
  }
  return NULL;
}
#endif /* THREADS_CONSUMER_SHARING */

#define freeze_current_cp() __freeze_current_cp( PASS_REGS1 )

static inline Int __freeze_current_cp(USES_REGS1) {
  choiceptr freeze_cp = B;

  B_FZ  = freeze_cp;
  H_FZ  = freeze_cp->cp_h;
  TR_FZ = freeze_cp->cp_tr;
  B = B->cp_b;
  HB = B->cp_h;
  return (LOCAL_LocalBase - (ADDR)freeze_cp);
}


#define wake_frozen_cp(f) __wake_frozen_cp((f) PASS_REGS)

#define restore_bindings(u, r) __restore_bindings((u), (r) PASS_REGS)

static inline void __wake_frozen_cp(Int frozen_offset USES_REGS) {
  choiceptr frozen_cp = (choiceptr)(LOCAL_LocalBase - frozen_offset);

  restore_bindings(TR, frozen_cp->cp_tr);
  B = frozen_cp;
  TR = TR_FZ;
  TRAIL_LINK(B->cp_tr);
  return;
}


#define abolish_frozen_cps_until(f) __abolish_frozen_cps_until((f) PASS_REGS )

static inline void __abolish_frozen_cps_until(Int frozen_offset USES_REGS) {
  choiceptr frozen_cp = (choiceptr)(LOCAL_LocalBase - frozen_offset);

  B_FZ  = frozen_cp;
  H_FZ  = frozen_cp->cp_h;
  TR_FZ = frozen_cp->cp_tr;
  return;
}

#define abolish_frozen_cps_all() __abolish_frozen_cps_all( PASS_REGS1 )

static inline void __abolish_frozen_cps_all( USES_REGS1 ) {
  B_FZ  = (choiceptr) LOCAL_LocalBase;
  H_FZ  = (CELL *) LOCAL_GlobalBase;
  TR_FZ = (tr_fr_ptr) LOCAL_TrailBase;
  return;
}

#define adjust_freeze_registers() __adjust_freeze_registers( PASS_REGS1 )

static inline void __adjust_freeze_registers( USES_REGS1 ) {
  B_FZ  = DepFr_cons_cp(LOCAL_top_dep_fr);
  H_FZ  = B_FZ->cp_h;
  TR_FZ = B_FZ->cp_tr;
  return;
}

#define mark_as_completed(sg) __mark_as_completed((sg) PASS_REGS )

static inline void __mark_as_completed(sg_fr_ptr sg_fr USES_REGS) {
#if defined(MODE_DIRECTED_TABLING) && !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
#endif /* MODE_DIRECTED_TABLING && !THREADS_FULL_SHARING && !THREADS_CONSUMER_SHARING */

  LOCK_SG_FR(sg_fr);
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
  INFO_THREADS(" mark_as_completed sgfr=%p ", SgFr_sg_ent(sg_fr));
  TABLING_ERROR_CHECKING(mark_as_completed, SgFr_sg_ent_state(sg_fr) > complete);
  SgFr_active_workers(sg_fr)--;
  SgFr_sg_ent_state(sg_fr) = complete;
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
  SgFr_state(sg_fr) = complete;
  UNLOCK_SG_FR(sg_fr);
#ifdef MODE_DIRECTED_TABLING
  if (SgFr_invalid_chain(sg_fr)) {
    ans_node_ptr current_node, next_node;
    /* find first valid answer */
    current_node = SgFr_first_answer(sg_fr);
    while (IS_ANSWER_INVALID_NODE(current_node))
      current_node = TrNode_child(current_node);
    SgFr_first_answer(sg_fr) = current_node;
    /* chain next valid answers */
    next_node = TrNode_child(current_node);
    while (next_node) {
      if (! IS_ANSWER_INVALID_NODE(next_node)) {
	TrNode_child(current_node) = next_node;
	current_node = next_node;   
      }
      next_node = TrNode_child(next_node);
    }
    SgFr_last_answer(sg_fr) = current_node;
#if !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
    /* free invalid answer nodes */
    current_node = SgFr_invalid_chain(sg_fr);
    SgFr_invalid_chain(sg_fr) = NULL;
    while (current_node) {
      next_node = TrNode_next(current_node);	
      FREE_ANSWER_TRIE_NODE(current_node);
      current_node = next_node;
    }
#endif /* !THREADS_FULL_SHARING && !THREADS_CONSUMER_SHARING */
  }
#endif /* MODE_DIRECTED_TABLING */
  return;
}

#define unbind_variables(u, e) __unbind_variables((u), (e) PASS_REGS)

static inline void __unbind_variables(tr_fr_ptr unbind_tr, tr_fr_ptr end_tr USES_REGS) {
  TABLING_ERROR_CHECKING(unbind_variables, unbind_tr < end_tr);
  /* unbind loop */
  while (unbind_tr != end_tr) {
    CELL ref = (CELL) TrailTerm(--unbind_tr);
    /* check for global or local variables */
    if (IsVarTerm(ref)) {
      /* unbind variable */
      RESET_VARIABLE(ref);
    } else if (IsPairTerm(ref)) {
      ref = (CELL) RepPair(ref);
      if (IN_BETWEEN(LOCAL_TrailBase, ref, LOCAL_TrailTop)) {
        /* avoid frozen segments */
        unbind_tr = (tr_fr_ptr) ref;
	TABLING_ERROR_CHECKING(unbind_variables, unbind_tr > (tr_fr_ptr) LOCAL_TrailTop);
	TABLING_ERROR_CHECKING(unbind_variables, unbind_tr < end_tr);
      }
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *aux_ptr = RepAppl(ref);
      --unbind_tr;
      Term aux_val = TrailVal(unbind_tr);
      *aux_ptr = aux_val;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
    }
  }
  return;
}


#define rebind_variables(u, e) __rebind_variables(u, e PASS_REGS)

static inline void __rebind_variables(tr_fr_ptr rebind_tr, tr_fr_ptr end_tr USES_REGS) {
  TABLING_ERROR_CHECKING(rebind_variables, rebind_tr < end_tr);
  /* rebind loop */
  Yap_NEW_MAHASH((ma_h_inner_struct *)HR PASS_REGS);
  while (rebind_tr != end_tr) {
    CELL ref = (CELL) TrailTerm(--rebind_tr);
    /* check for global or local variables */
    if (IsVarTerm(ref)) {
      /* rebind variable */
      *((CELL *)ref) = TrailVal(rebind_tr);
    } else if (IsPairTerm(ref)) {
      ref = (CELL) RepPair(ref);
      if (IN_BETWEEN(LOCAL_TrailBase, ref, LOCAL_TrailTop)) {
        /* avoid frozen segments */
  	rebind_tr = (tr_fr_ptr) ref;
	TABLING_ERROR_CHECKING(rebind_variables, rebind_tr > (tr_fr_ptr) LOCAL_TrailTop);
	TABLING_ERROR_CHECKING(rebind_variables, rebind_tr < end_tr);
      }
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *cell_ptr = RepAppl(ref);
      if (!Yap_lookup_ma_var(cell_ptr PASS_REGS)) {
	/* first time we found the variable, let's put the new value */
	*cell_ptr = TrailVal(rebind_tr);
      }
      --rebind_tr;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
    }
  }
  return;
}

static inline void __restore_bindings(tr_fr_ptr unbind_tr, tr_fr_ptr rebind_tr USES_REGS) {
  CELL ref;
  tr_fr_ptr end_tr;

  TABLING_ERROR_CHECKING(restore_variables, unbind_tr < rebind_tr);
  end_tr = rebind_tr;
  Yap_NEW_MAHASH((ma_h_inner_struct *)HR PASS_REGS);
  while (unbind_tr != end_tr) {
    /* unbind loop */
    while (unbind_tr > end_tr) {
      ref = (CELL) TrailTerm(--unbind_tr);
      if (IsVarTerm(ref)) {
        RESET_VARIABLE(ref);
      } else if (IsPairTerm(ref)) {
        ref = (CELL) RepPair(ref);
	if (IN_BETWEEN(LOCAL_TrailBase, ref, LOCAL_TrailTop)) {
	  /* avoid frozen segments */
          unbind_tr = (tr_fr_ptr) ref;
	  TABLING_ERROR_CHECKING(restore_variables, unbind_tr > (tr_fr_ptr) LOCAL_TrailTop);
        }
#ifdef MULTI_ASSIGNMENT_VARIABLES
      }	else if (IsApplTerm(ref)) {
	CELL *pt = RepAppl(ref);

	/* AbsAppl means */
	/* multi-assignment variable */
	/* so that the upper cell is the old value */ 
	--unbind_tr;
	if (!Yap_lookup_ma_var(pt PASS_REGS)) {
	  pt[0] = TrailVal(unbind_tr);
	}
#endif /* MULTI_ASSIGNMENT_VARIABLES */
      }
    }
    /* look for end */
    while (unbind_tr < end_tr) {
      ref = (CELL) TrailTerm(--end_tr);
      if (IsPairTerm(ref)) {
        ref = (CELL) RepPair(ref);
	if (IN_BETWEEN(LOCAL_TrailBase, ref, LOCAL_TrailTop)) {
	  /* avoid frozen segments */
  	  end_tr = (tr_fr_ptr) ref;
	  TABLING_ERROR_CHECKING(restore_variables, end_tr > (tr_fr_ptr) LOCAL_TrailTop);
        }
      }
    }
  }
  /* rebind loop */
  while (rebind_tr != end_tr) {
    ref = (CELL) TrailTerm(--rebind_tr);
    if (IsVarTerm(ref)) {
      *((CELL *)ref) = TrailVal(rebind_tr);
    } else if (IsPairTerm(ref)) {
      ref = (CELL) RepPair(ref);
      if (IN_BETWEEN(LOCAL_TrailBase, ref, LOCAL_TrailTop)) {
	/* avoid frozen segments */
        rebind_tr = (tr_fr_ptr) ref;
	TABLING_ERROR_CHECKING(restore_variables, rebind_tr > (tr_fr_ptr) LOCAL_TrailTop);
	TABLING_ERROR_CHECKING(restore_variables, rebind_tr < end_tr);
      }
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *cell_ptr = RepAppl(ref);
      /* first time we found the variable, let's put the new value */
      *cell_ptr = TrailVal(rebind_tr);
      --rebind_tr;
#endif /* MULTI_ASSIGNMENT_VARIABLES */
    }
  }
  return;
}

#define expand_auxiliary_stack(s) __expand_auxiliary_stack((s) PASS_REGS)

static inline CELL *__expand_auxiliary_stack(CELL *stack USES_REGS) {
  char *old_top = (char *)LOCAL_TrailTop;
  INFORMATION_MESSAGE("Expanding trail in " UInt_FORMAT " bytes", K64);
  if (! Yap_growtrail(K64, TRUE)) {  /* TRUE means 'contiguous_only' */
    Yap_Error(RESOURCE_ERROR_TRAIL, TermNil, "stack full (STACK_CHECK_EXPAND)");
    return NULL;
  } else {
    UInt diff = (char *)LOCAL_TrailTop - old_top;
    CELL *new_stack = (CELL *)((char *)stack + diff);
    memmove((void *)new_stack, stack, old_top - (char *)stack);
    return new_stack;
  }
}

#define abolish_incomplete_subgoals(p) __abolish_incomplete_subgoals((p) PASS_REGS)


static inline void __abolish_incomplete_subgoals(choiceptr prune_cp USES_REGS) {

#ifdef YAPOR
  if (EQUAL_OR_YOUNGER_CP(GetOrFr_node(LOCAL_top_susp_or_fr), prune_cp))
    pruning_over_tabling_data_structures();
#endif /* YAPOR */

  if (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), prune_cp)) {
#ifdef YAPOR
    if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING)
      pruning_over_tabling_data_structures();
#endif /* YAPOR */
    do {
      dep_fr_ptr dep_fr = LOCAL_top_dep_fr;
      LOCAL_top_dep_fr = DepFr_next(dep_fr);
      FREE_DEPENDENCY_FRAME(dep_fr);
    } while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), prune_cp));
    adjust_freeze_registers();
  }

  while (LOCAL_top_sg_fr && EQUAL_OR_YOUNGER_CP(SgFr_gen_cp(LOCAL_top_sg_fr), prune_cp)) {
    sg_fr_ptr sg_fr;
#ifdef YAPOR
    if (GLOBAL_parallel_mode == PARALLEL_MODE_RUNNING)
      pruning_over_tabling_data_structures();
#endif /* YAPOR */
    sg_fr = LOCAL_top_sg_fr;
    LOCAL_top_sg_fr = SgFr_next(sg_fr);
    LOCK_SG_FR(sg_fr);
    if (SgFr_first_answer(sg_fr) == NULL) {
      /* no answers --> ready */
      SgFr_state(sg_fr) = ready;
      UNLOCK_SG_FR(sg_fr);
    } else if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
      /* yes answer --> complete */
#ifndef TABLING_EARLY_COMPLETION
      /* with early completion, at this point the subgoal should be already completed */
      SgFr_state(sg_fr) = complete;
#endif /* TABLING_EARLY_COMPLETION */
      UNLOCK_SG_FR(sg_fr);
    } else {
      /* answers --> incomplete/ready */
#ifdef INCOMPLETE_TABLING
      SgFr_state(sg_fr) = incomplete;
      UNLOCK_SG_FR(sg_fr);
#ifdef MODE_DIRECTED_TABLING
      if (SgFr_invalid_chain(sg_fr)) {
	ans_node_ptr current_node, next_node;
	/* find first valid answer */
	current_node = SgFr_first_answer(sg_fr);
	while (IS_ANSWER_INVALID_NODE(current_node))
	  current_node = TrNode_child(current_node);
	SgFr_first_answer(sg_fr) = current_node;
	/* chain next valid answers */
	next_node = TrNode_child(current_node);
	while (next_node) {
	  if (! IS_ANSWER_INVALID_NODE(next_node)) {
	    TrNode_child(current_node) = next_node;
	    current_node = next_node;   
	  }
	  next_node = TrNode_child(next_node);
	}
	SgFr_last_answer(sg_fr) = current_node;
#if !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
	/* free invalid answer nodes */
	current_node = SgFr_invalid_chain(sg_fr);
	SgFr_invalid_chain(sg_fr) = NULL;
	while (current_node) {
	  next_node = TrNode_next(current_node);	
	  FREE_ANSWER_TRIE_NODE(current_node);
	  current_node = next_node;
	}
#endif /* !THREADS_FULL_SHARING && !THREADS_CONSUMER_SHARING */
      }
#endif /* MODE_DIRECTED_TABLING */
#else
      ans_node_ptr node;
#if defined(MODE_DIRECTED_TABLING) && !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
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
#endif /* MODE_DIRECTED_TABLING && !THREADS_FULL_SHARING && !THREADS_CONSUMER_SHARING */
      SgFr_state(sg_fr) = ready;
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
      if (SgFr_active_workers(sg_fr) == 0) {
	SgFr_sg_ent_state(sg_fr) = ready;
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
	free_answer_hash_chain(SgFr_hash_chain(sg_fr));
	SgFr_hash_chain(sg_fr) = NULL;
	SgFr_first_answer(sg_fr) = NULL;
	SgFr_last_answer(sg_fr) = NULL;
	node = TrNode_child(SgFr_answer_trie(sg_fr));
	TrNode_child(SgFr_answer_trie(sg_fr)) = NULL;
	UNLOCK_SG_FR(sg_fr);
	free_answer_trie(node, TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);
#ifdef THREADS_FULL_SHARING
        if (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(sg_fr)))) {
	  SgFr_batched_last_answer(sg_fr) = NULL;
	  struct answer_ref_node *local_uncons_ans = SgFr_batched_cached_answers(sg_fr) ; 
	  while ( local_uncons_ans ) {
	    SgFr_batched_cached_answers(sg_fr) = RefNode_next(SgFr_batched_cached_answers(sg_fr));
	    FREE_ANSWER_REF_NODE(local_uncons_ans);
	    local_uncons_ans = SgFr_batched_cached_answers(sg_fr);
	  }
	}
      } else { /* SgFr_active_workers(sg_fr) != 0 */
	if (IsMode_Batched(TabEnt_mode(SgFr_tab_ent(sg_fr)))){
	  SgFr_batched_last_answer(sg_fr) = NULL;
	  if ( worker_id >= ANSWER_LEAF_NODE_MAX_THREADS ) {
	    struct answer_ref_node *local_uncons_ans = SgFr_batched_cached_answers(sg_fr) ; 
	    while ( local_uncons_ans ) {
	      SgFr_batched_cached_answers(sg_fr) = RefNode_next(SgFr_batched_cached_answers(sg_fr));
	      FREE_ANSWER_REF_NODE(local_uncons_ans);
	      local_uncons_ans = SgFr_batched_cached_answers(sg_fr);
	    }	
	  } else { /* worker_id < ANSWER_LEAF_NODE_MAX_THREADS */
	    ans_node_ptr leaf_ans_trie_node = SgFr_first_answer(sg_fr);
	    while( leaf_ans_trie_node ){
	      ANSWER_LEAF_NODE_DEL_WID(leaf_ans_trie_node,worker_id);
	      leaf_ans_trie_node = TrNode_child(leaf_ans_trie_node);
	    }
	  }
	}
#endif /* THREADS_FULL_SHARING */
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
      }
#endif /* THREADS_FULL_SHARING || THREADS_CONSUMER_SHARING */
#endif /* INCOMPLETE_TABLING */
    }
#ifdef LIMIT_TABLING
    insert_into_global_sg_fr_list(sg_fr);
#endif /* LIMIT_TABLING */
  }

  return;
}


#ifdef YAPOR
static inline void pruning_over_tabling_data_structures(void) {
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "pruning over tabling data structures");
  return;
}


#define collect_suspension_frames(o) __collect_suspension_frames((o) PASS_REGS)

static inline void __collect_suspension_frames(or_fr_ptr or_fr USES_REGS) {  
  int depth;
  or_fr_ptr *susp_ptr;

  OPTYAP_ERROR_CHECKING(collect_suspension_frames, IS_UNLOCKED(or_fr));
  OPTYAP_ERROR_CHECKING(collect_suspension_frames, OrFr_suspensions(or_fr) == NULL);

  /* order collected suspension frames by depth */
  depth = OrFr_depth(or_fr);
  susp_ptr = & LOCAL_top_susp_or_fr;
  while (OrFr_depth(*susp_ptr) > depth)
    susp_ptr = & OrFr_nearest_suspnode(*susp_ptr);
  OrFr_nearest_suspnode(or_fr) = *susp_ptr;
  *susp_ptr = or_fr;
  return;
}


static inline
#ifdef TIMESTAMP_CHECK
susp_fr_ptr suspension_frame_to_resume(or_fr_ptr susp_or_fr, long timestamp) {
#else
susp_fr_ptr suspension_frame_to_resume(or_fr_ptr susp_or_fr) {
#endif /* TIMESTAMP_CHECK */
  choiceptr top_cp;
  susp_fr_ptr *susp_ptr, susp_fr;
  dep_fr_ptr dep_fr;

  top_cp = GetOrFr_node(susp_or_fr);
  susp_ptr = & OrFr_suspensions(susp_or_fr);
  susp_fr = *susp_ptr;
  while (susp_fr) {
    dep_fr = SuspFr_top_dep_fr(susp_fr);
    do {
      if (TrNode_child(DepFr_last_answer(dep_fr))) {
        /* unconsumed answers in susp_fr */
        *susp_ptr = SuspFr_next(susp_fr);
        return susp_fr;
      }
#ifdef TIMESTAMP_CHECK
      DepFr_timestamp(dep_fr) = timestamp;
#endif /* TIMESTAMP_CHECK */
      dep_fr = DepFr_next(dep_fr);
#ifdef TIMESTAMP_CHECK
    } while (timestamp > DepFr_timestamp(dep_fr) && YOUNGER_CP(DepFr_cons_cp(dep_fr), top_cp));
#else
    } while (YOUNGER_CP(DepFr_cons_cp(dep_fr), top_cp));
#endif /* TIMESTAMP_CHECK */
    susp_ptr = & SuspFr_next(susp_fr);
    susp_fr = *susp_ptr;
  }
  /* no suspension frame with unconsumed answers */
  return NULL;
}
#endif /* YAPOR */


#ifdef TABLING_INNER_CUTS
static inline void CUT_store_tg_answer(or_fr_ptr or_frame, ans_node_ptr ans_node, choiceptr gen_cp, int ltt) {
  tg_sol_fr_ptr tg_sol_fr, *solution_ptr, next, ltt_next;
  tg_ans_fr_ptr tg_ans_fr;

  solution_ptr = & OrFr_tg_solutions(or_frame);
  while (*solution_ptr && YOUNGER_CP(gen_cp, TgSolFr_gen_cp(*solution_ptr))) {
    solution_ptr = & TgSolFr_next(*solution_ptr);
  }
  if (*solution_ptr && gen_cp == TgSolFr_gen_cp(*solution_ptr)) {
    if (ltt >= TgSolFr_ltt(*solution_ptr)) {
      while (*solution_ptr && ltt > TgSolFr_ltt(*solution_ptr)) {
        solution_ptr = & TgSolFr_ltt_next(*solution_ptr);
      }
      if (*solution_ptr && ltt == TgSolFr_ltt(*solution_ptr)) {
        tg_ans_fr = TgSolFr_first(*solution_ptr);
        if (TgAnsFr_free_slot(tg_ans_fr) == TG_ANSWER_SLOTS) {
          ALLOC_TG_ANSWER_FRAME(tg_ans_fr);
          TgAnsFr_free_slot(tg_ans_fr) = 1;
          TgAnsFr_answer(tg_ans_fr, 0) = ans_node;
          TgAnsFr_next(tg_ans_fr) = TgSolFr_first(*solution_ptr);
          TgSolFr_first(*solution_ptr) = tg_ans_fr;
        } else {
          TgAnsFr_answer(tg_ans_fr, TgAnsFr_free_slot(tg_ans_fr)) = ans_node;
          TgAnsFr_free_slot(tg_ans_fr)++;
        }
        return;
      }
      ltt_next = *solution_ptr;
      next = NULL;
    } else {
      ltt_next = *solution_ptr;
      next = TgSolFr_next(*solution_ptr);
    }
  } else {
    ltt_next = NULL;
    next = *solution_ptr;
  }
  ALLOC_TG_ANSWER_FRAME(tg_ans_fr);
  TgAnsFr_free_slot(tg_ans_fr) = 1;
  TgAnsFr_answer(tg_ans_fr, 0) = ans_node;
  TgAnsFr_next(tg_ans_fr) = NULL;
  ALLOC_TG_SOLUTION_FRAME(tg_sol_fr);
  TgSolFr_gen_cp(tg_sol_fr) = gen_cp;
  TgSolFr_ltt(tg_sol_fr) = ltt;
  TgSolFr_first(tg_sol_fr) = tg_ans_fr;
  TgSolFr_last(tg_sol_fr) = tg_ans_fr;
  TgSolFr_ltt_next(tg_sol_fr) = ltt_next;
  TgSolFr_next(tg_sol_fr) = next;
  *solution_ptr = tg_sol_fr;
  return;
}


static inline tg_sol_fr_ptr CUT_store_tg_answers(or_fr_ptr or_frame, tg_sol_fr_ptr new_solution, int ltt) {
  tg_sol_fr_ptr *old_solution_ptr, next_new_solution;
  choiceptr node, gen_cp;

  old_solution_ptr = & OrFr_tg_solutions(or_frame);
  node = GetOrFr_node(or_frame);
  while (new_solution && YOUNGER_CP(node, TgSolFr_gen_cp(new_solution))) {
    next_new_solution = TgSolFr_next(new_solution);
    gen_cp = TgSolFr_gen_cp(new_solution);
    while (*old_solution_ptr && YOUNGER_CP(gen_cp, TgSolFr_gen_cp(*old_solution_ptr))) {
      old_solution_ptr = & TgSolFr_next(*old_solution_ptr);
    }
    if (*old_solution_ptr && gen_cp == TgSolFr_gen_cp(*old_solution_ptr)) {
      if (ltt >= TgSolFr_ltt(*old_solution_ptr)) {
        tg_sol_fr_ptr *ltt_next_old_solution_ptr;
        ltt_next_old_solution_ptr = old_solution_ptr;
        while (*ltt_next_old_solution_ptr && ltt > TgSolFr_ltt(*ltt_next_old_solution_ptr)) {
          ltt_next_old_solution_ptr = & TgSolFr_ltt_next(*ltt_next_old_solution_ptr);
        }
        if (*ltt_next_old_solution_ptr && ltt == TgSolFr_ltt(*ltt_next_old_solution_ptr)) {
          TgAnsFr_next(TgSolFr_last(*ltt_next_old_solution_ptr)) = TgSolFr_first(new_solution);
          TgSolFr_last(*ltt_next_old_solution_ptr) = TgSolFr_last(new_solution);
          FREE_TG_SOLUTION_FRAME(new_solution);
        } else {
          TgSolFr_ltt(new_solution) = ltt;
          TgSolFr_ltt_next(new_solution) = *ltt_next_old_solution_ptr;
          TgSolFr_next(new_solution) = NULL;
          *ltt_next_old_solution_ptr = new_solution;
	}
      } else {
        TgSolFr_ltt(new_solution) = ltt;
        TgSolFr_ltt_next(new_solution) = *old_solution_ptr;
        TgSolFr_next(new_solution) = TgSolFr_next(*old_solution_ptr);
        *old_solution_ptr = new_solution;
      }
    } else {
      TgSolFr_ltt(new_solution) = ltt;
      TgSolFr_ltt_next(new_solution) = NULL;
      TgSolFr_next(new_solution) = *old_solution_ptr;
      *old_solution_ptr = new_solution;
    }
    old_solution_ptr = & TgSolFr_next(*old_solution_ptr);
    new_solution = next_new_solution;
  }
  return new_solution;
}


static inline void CUT_validate_tg_answers(tg_sol_fr_ptr valid_solutions) {
  tg_ans_fr_ptr valid_answers, free_answer;
  tg_sol_fr_ptr ltt_valid_solutions, free_solution;
  ans_node_ptr first_answer, last_answer, ans_node;
  sg_fr_ptr sg_fr;
  int slots;

  while (valid_solutions) {
    first_answer = last_answer = NULL;
#ifdef DETERMINISTIC_TABLING
    if (IS_DET_GEN_CP(TgSolFr_gen_cp(valid_solutions)))
      sg_fr = DET_GEN_CP(TgSolFr_gen_cp(valid_solutions))->cp_sg_fr;
    else
#endif /* DETERMINISTIC_TABLING */
      sg_fr = GEN_CP(TgSolFr_gen_cp(valid_solutions))->cp_sg_fr;
    ltt_valid_solutions = valid_solutions;
    valid_solutions = TgSolFr_next(valid_solutions);
    do {
      valid_answers = TgSolFr_first(ltt_valid_solutions);
      do {
        slots = TgAnsFr_free_slot(valid_answers);
        do {
          ans_node = TgAnsFr_answer(valid_answers, --slots);
	  LOCK_ANSWER_TRIE(sg_fr);
	  LOCK_ANSWER_NODE(ans_node);
          if (! IS_ANSWER_LEAF_NODE(ans_node)) {
            TAG_AS_ANSWER_LEAF_NODE(ans_node);
            if (first_answer == NULL)
	      first_answer = ans_node;
	    else
              TrNode_child(last_answer) = ans_node;
	    last_answer = ans_node;
	  }
          UNLOCK_ANSWER_NODE(ans_node);	  
	  UNLOCK_ANSWER_TRIE(sg_fr);
        } while (slots);
        free_answer = valid_answers;
        valid_answers = TgAnsFr_next(valid_answers);
        FREE_TG_ANSWER_FRAME(free_answer);
      } while (valid_answers);
      free_solution = ltt_valid_solutions;
      ltt_valid_solutions = TgSolFr_ltt_next(ltt_valid_solutions);
      FREE_TG_SOLUTION_FRAME(free_solution);
    } while (ltt_valid_solutions);
    if (first_answer) {
      LOCK_SG_FR(sg_fr);
      if (SgFr_first_answer(sg_fr) == NULL) {
        SgFr_first_answer(sg_fr) = first_answer;
      } else {
        TrNode_child(SgFr_last_answer(sg_fr)) = first_answer;
      }
      SgFr_last_answer(sg_fr) = last_answer;
      UNLOCK_SG_FR(sg_fr);
    }
  }
  return;
}


static inline void CUT_join_tg_solutions(tg_sol_fr_ptr *old_solution_ptr, tg_sol_fr_ptr new_solution) {
  tg_sol_fr_ptr next_old_solution, next_new_solution;
  choiceptr gen_cp;

  do {
    gen_cp = TgSolFr_gen_cp(new_solution);
    while (*old_solution_ptr && YOUNGER_CP(gen_cp, TgSolFr_gen_cp(*old_solution_ptr))) {
      old_solution_ptr = & TgSolFr_next(*old_solution_ptr);
    }
    if (*old_solution_ptr) {
      next_old_solution = *old_solution_ptr;
      *old_solution_ptr = new_solution;
      CUT_join_solution_frame_tg_answers(new_solution);
      if (gen_cp == TgSolFr_gen_cp(next_old_solution)) {
        tg_sol_fr_ptr free_solution;
        TgAnsFr_next(TgSolFr_last(new_solution)) = TgSolFr_first(next_old_solution);
        TgSolFr_last(new_solution) = TgSolFr_last(next_old_solution);
        free_solution = next_old_solution;
        next_old_solution = TgSolFr_next(next_old_solution);
        FREE_TG_SOLUTION_FRAME(free_solution);
        if (! next_old_solution) {
          if ((next_new_solution = TgSolFr_next(new_solution))) {
            CUT_join_solution_frames_tg_answers(next_new_solution);
	  }
          return;
	}
      }
      gen_cp = TgSolFr_gen_cp(next_old_solution);
      next_new_solution = TgSolFr_next(new_solution);
      while (next_new_solution && YOUNGER_CP(gen_cp, TgSolFr_gen_cp(next_new_solution))) {
        new_solution = next_new_solution;
        next_new_solution = TgSolFr_next(new_solution);
        CUT_join_solution_frame_tg_answers(new_solution);
      }
      old_solution_ptr = & TgSolFr_next(new_solution);
      TgSolFr_next(new_solution) = next_old_solution;
      new_solution = next_new_solution;
    } else {
      *old_solution_ptr = new_solution;
      CUT_join_solution_frames_tg_answers(new_solution);
      return;
    }
  } while (new_solution);
  return;
}


static inline void CUT_join_solution_frame_tg_answers(tg_sol_fr_ptr join_solution) {
  tg_sol_fr_ptr next_solution;

  while ((next_solution = TgSolFr_ltt_next(join_solution))) {
    TgAnsFr_next(TgSolFr_last(join_solution)) = TgSolFr_first(next_solution);
    TgSolFr_last(join_solution) = TgSolFr_last(next_solution);
    TgSolFr_ltt_next(join_solution) = TgSolFr_ltt_next(next_solution);
    FREE_TG_SOLUTION_FRAME(next_solution);
  }
  return;
}


static inline void CUT_join_solution_frames_tg_answers(tg_sol_fr_ptr join_solution) {
  do {
    CUT_join_solution_frame_tg_answers(join_solution);
    join_solution = TgSolFr_next(join_solution);
  } while (join_solution);
  return;
}


static inline void CUT_free_tg_solution_frame(tg_sol_fr_ptr solution) {
  tg_ans_fr_ptr current_answer, next_answer;

  current_answer = TgSolFr_first(solution);
  do {
    next_answer = TgAnsFr_next(current_answer);
    FREE_TG_ANSWER_FRAME(current_answer);
    current_answer = next_answer;
  } while (current_answer);
  FREE_TG_SOLUTION_FRAME(solution);
  return;
}


static inline void CUT_free_tg_solution_frames(tg_sol_fr_ptr current_solution) {
  tg_sol_fr_ptr ltt_solution, next_solution;

  while (current_solution) {
    ltt_solution = TgSolFr_ltt_next(current_solution);
    while (ltt_solution) {
      next_solution = TgSolFr_ltt_next(ltt_solution);
      CUT_free_tg_solution_frame(ltt_solution);
      ltt_solution = next_solution;
    }
    next_solution = TgSolFr_next(current_solution);
    CUT_free_tg_solution_frame(current_solution);
    current_solution = next_solution;
  }
  return;
}


static inline tg_sol_fr_ptr CUT_prune_tg_solution_frames(tg_sol_fr_ptr solutions, int ltt) {
  tg_sol_fr_ptr ltt_next_solution, return_solution;

  if (! solutions) return NULL;
  return_solution = CUT_prune_tg_solution_frames(TgSolFr_next(solutions), ltt);
  while (solutions && ltt > TgSolFr_ltt(solutions)) {
    ltt_next_solution = TgSolFr_ltt_next(solutions);
    CUT_free_tg_solution_frame(solutions);
    solutions = ltt_next_solution;
  }
  if (solutions) {
    TgSolFr_next(solutions) = return_solution;
    return solutions;
  } else {
    return return_solution;
  }
}
#endif /* TABLING_INNER_CUTS */
