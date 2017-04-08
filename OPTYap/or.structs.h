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


/**

@defgroup Parallelism Parallelism in YAP
@ingroup extensions
@{

There has been a sizeable amount of work on an or-parallel
implementation for YAP, called  *YAPOr*. Most of this work has
been performed by Ricardo Rocha. In this system parallelism is exploited
implicitly by running several alternatives in or-parallel. This option
can be enabled from the `configure` script or by checking the
system's `Makefile`.

 *YAPOr* is still a very experimental system, going through rapid
development. The following restrictions are of note:

+ *YAPOr* currently only supports the Linux/X86 and SPARC/Solaris
platforms. Porting to other Unix-like platforms should be straightforward.

+ *YAPOr* does not support parallel updates to the
data-base.

+ *YAPOr* does not support opening or closing of streams during
parallel execution.

+ Garbage collection and stack shifting are not supported in
 *YAPOr*.

+ Built-ins that cause side-effects can only be executed when
left-most in the search-tree. There are no primitives to provide
asynchronous or cavalier execution of these built-ins, as in Aurora or
Muse.

+ YAP does not support voluntary suspension of work.


We expect that some of these restrictions will be removed in future
releases.


 */

/* ------------------------- **
**      Struct or_frame      **
** ------------------------- */

typedef struct or_frame {
  lockvar lock;
  yamop *alternative;
  volatile bitmap members;
#ifdef YAPOR_THREADS
  Int node_offset;
#else
  choiceptr node;
#endif
  struct or_frame *nearest_livenode;
  /* cut support */
  int depth;
#ifdef YAPOR_THREADS
  Int pending_prune_cp_offset;
#else
  choiceptr pending_prune_cp;
#endif
  volatile int pending_prune_ltt;
  struct or_frame *nearest_leftnode;
  struct query_goal_solution_frame *query_solutions;
#ifdef TABLING_INNER_CUTS
  struct table_subgoal_solution_frame *table_solutions;
#endif /* TABLING_INNER_CUTS */
#ifdef TABLING
  /* tabling support */
  volatile int number_owners;
  struct or_frame *next_on_stack;
  struct suspension_frame *suspensions;
  struct or_frame *nearest_suspension_node;
#endif /* TABLING */
  struct or_frame *next;
} *or_fr_ptr;

#define OrFr_lock(X)              ((X)->lock)
#define OrFr_alternative(X)       ((X)->alternative)
#define OrFr_members(X)           ((X)->members)
#ifdef YAPOR_THREADS
#define GetOrFr_node(X)           offset_to_cptr((X)->node_offset)
#define SetOrFr_node(X,V)         ((X)->node_offset = cptr_to_offset(V))
#else
#define OrFr_node(X)              ((X)->node)
#define GetOrFr_node(X)           ((X)->node)
#define SetOrFr_node(X,V)         ((X)->node = V)
#endif
#define OrFr_nearest_livenode(X)  ((X)->nearest_livenode)
#define OrFr_depth(X)             ((X)->depth)
#ifdef YAPOR_THREADS
#define Get_OrFr_pend_prune_cp(X) offset_to_cptr_with_null((X)->pending_prune_cp_offset)
#define Set_OrFr_pend_prune_cp(X,V)  ((X)->pending_prune_cp_offset = cptr_to_offset_with_null(V))
#else
#define OrFr_pend_prune_cp(X)     ((X)->pending_prune_cp)
#define Get_OrFr_pend_prune_cp(X) ((X)->pending_prune_cp)
#define Set_OrFr_pend_prune_cp(X,V)  ((X)->pending_prune_cp = (V))
#endif
#define OrFr_pend_prune_ltt(X)    ((X)->pending_prune_ltt)
#define OrFr_nearest_leftnode(X)  ((X)->nearest_leftnode)
#define OrFr_qg_solutions(X)      ((X)->query_solutions)
#define OrFr_tg_solutions(X)      ((X)->table_solutions)
#define OrFr_owners(X)            ((X)->number_owners)
#ifdef TABLING
#define OrFr_next_on_stack(X)     ((X)->next_on_stack)
#else
#define OrFr_next_on_stack(X)     ((X)->next)
#endif /* TABLING */
#define OrFr_suspensions(X)       ((X)->suspensions)
#define OrFr_nearest_suspnode(X)  ((X)->nearest_suspension_node)
#define OrFr_next(X)              ((X)->next)



/* ------------------------------------------ **
**      Struct query_goal_solution_frame      **
** ------------------------------------------ */

typedef struct query_goal_solution_frame{
  volatile int ltt;
  struct query_goal_answer_frame *first;
  struct query_goal_answer_frame *last;
  struct query_goal_solution_frame *next;
} *qg_sol_fr_ptr;

#define SolFr_ltt(X)    ((X)->ltt)
#define SolFr_first(X)  ((X)->first)
#define SolFr_last(X)   ((X)->last)
#define SolFr_next(X)   ((X)->next)



/* ---------------------------------------- **
**      Struct query_goal_answer_frame      **
** ---------------------------------------- */

typedef struct query_goal_answer_frame{
  Term answer;
  struct query_goal_answer_frame *next;
} *qg_ans_fr_ptr;

#define AnsFr_answer(X)  ((X)->answer)
#define AnsFr_next(X)    ((X)->next)



#ifdef TABLING_INNER_CUTS
/* --------------------------------------------- **
**      Struct table_subgoal_solution_frame      **
** --------------------------------------------- */

typedef struct table_subgoal_solution_frame{
  choiceptr generator_choice_point;
  volatile int ltt;
  struct table_subgoal_answer_frame *first_answer_frame;
  struct table_subgoal_answer_frame *last_answer_frame;
  struct table_subgoal_solution_frame *ltt_next;
  struct table_subgoal_solution_frame *next;
} *tg_sol_fr_ptr;

#define TgSolFr_gen_cp(X)    ((X)->generator_choice_point)
#define TgSolFr_ltt(X)       ((X)->ltt)
#define TgSolFr_first(X)     ((X)->first_answer_frame)
#define TgSolFr_last(X)      ((X)->last_answer_frame)
#define TgSolFr_ltt_next(X)  ((X)->ltt_next)
#define TgSolFr_next(X)      ((X)->next)



/* ------------------------------------------- **
**      Struct table_subgoal_answer_frame      **
** ------------------------------------------- */

typedef struct table_subgoal_answer_frame{
  volatile int next_free_slot;
  struct answer_trie_node *answer[TG_ANSWER_SLOTS];
  struct table_subgoal_answer_frame *next;
} *tg_ans_fr_ptr;

#define TgAnsFr_free_slot(X)  ((X)->next_free_slot)
#define TgAnsFr_answer(X,N)   ((X)->answer[N])
#define TgAnsFr_next(X)       ((X)->next)
#endif /* TABLING_INNER_CUTS */

///@}
