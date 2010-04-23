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

/************************************************************************
**                     Table Space Data Structures                     **
************************************************************************/

/**************************
**      table_entry      **
**************************/

typedef struct table_entry {
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;
#endif /* YAPOR || THREADS */
  struct pred_entry *pred_entry;
  Atom pred_atom;
  int pred_arity;
  short pred_flags;
  short execution_mode;  /* combines yap_flags with pred_flags */
  struct subgoal_trie_node *subgoal_trie;
  struct subgoal_trie_hash *hash_chain;
  struct table_entry *next;
} *tab_ent_ptr;

#define TabEnt_lock(X)          ((X)->lock)
#define TabEnt_pe(X)            ((X)->pred_entry)
#define TabEnt_atom(X)          ((X)->pred_atom)
#define TabEnt_arity(X)         ((X)->pred_arity)
#define TabEnt_flags(X)         ((X)->pred_flags)
#define TabEnt_mode(X)          ((X)->execution_mode)
#define TabEnt_subgoal_trie(X)  ((X)->subgoal_trie)
#define TabEnt_hash_chain(X)    ((X)->hash_chain)
#define TabEnt_next(X)          ((X)->next)



/***********************************************************************
**      global_trie_node, subgoal_trie_node and answer_trie_node      **
***********************************************************************/

typedef struct subgoal_trie_node {
  Term entry;
  struct subgoal_trie_node *parent;
  struct subgoal_trie_node *child;
  struct subgoal_trie_node *next;
#ifdef TABLE_LOCK_AT_NODE_LEVEL
  lockvar lock;
#endif /* TABLE_LOCK_AT_NODE_LEVEL */
} *sg_node_ptr;

typedef struct answer_trie_node {
  OPCODE trie_instruction;  /* u.opc */
#ifdef YAPOR
  int or_arg;               /* u.Otapl.or_arg */
#endif /* YAPOR */
  Term entry;
  struct answer_trie_node *parent;
  struct answer_trie_node *child;
  struct answer_trie_node *next;
#ifdef TABLE_LOCK_AT_NODE_LEVEL
  lockvar lock;
#endif /* TABLE_LOCK_AT_NODE_LEVEL */
} *ans_node_ptr;

typedef struct global_trie_node {
  Term entry;
  struct global_trie_node *parent;
  struct global_trie_node *child;
  struct global_trie_node *next;
#ifdef TABLE_LOCK_AT_NODE_LEVEL
  lockvar lock;
#endif /* TABLE_LOCK_AT_NODE_LEVEL */
} *gt_node_ptr;

#define TrNode_instr(X)   ((X)->trie_instruction)
#define TrNode_or_arg(X)  ((X)->or_arg)
#define TrNode_entry(X)   ((X)->entry)
#define TrNode_parent(X)  ((X)->parent)
#define TrNode_child(X)   ((X)->child)
#define TrNode_sg_fr(X)   ((X)->child)
#define TrNode_next(X)    ((X)->next)
#define TrNode_lock(X)    ((X)->lock)



/***********************************************************************
**      global_trie_hash, subgoal_trie_hash and answer_trie_hash      **
***********************************************************************/

typedef struct subgoal_trie_hash {
  /* the first field is used for compatibility **
  ** with the subgoal_trie_node data structure */
  Term mark;    
  int number_of_buckets;
  struct subgoal_trie_node **buckets;
  int number_of_nodes;
  struct subgoal_trie_hash *next;
} *sg_hash_ptr;

typedef struct answer_trie_hash {
  /* the first field is used for compatibility **
  ** with the answer_trie_node data structure  */
  OPCODE mark;
  int number_of_buckets;
  struct answer_trie_node **buckets;
  int number_of_nodes;
  struct answer_trie_hash *next;
} *ans_hash_ptr;

typedef struct global_trie_hash {
  /* the first field is used for compatibility **
  ** with the global_trie_node data structure  */
  Term mark;
  int number_of_buckets;
  struct global_trie_node **buckets;
  int number_of_nodes;
} *gt_hash_ptr;

#define Hash_mark(X)         ((X)->mark)
#define Hash_num_buckets(X)  ((X)->number_of_buckets)
#define Hash_seed(X)         ((X)->number_of_buckets - 1)
#define Hash_buckets(X)      ((X)->buckets)
#define Hash_bucket(X,N)     ((X)->buckets + N)
#define Hash_num_nodes(X)    ((X)->number_of_nodes)
#define Hash_next(X)         ((X)->next)



/************************************************************************
**                      Execution Data Structures                      **
************************************************************************/

/****************************
**      choice points      **
****************************/

struct generator_choicept {
  struct choicept cp;
  struct dependency_frame *cp_dep_fr;  /* always NULL if batched scheduling */
  struct subgoal_frame *cp_sg_fr;
#ifdef LOW_LEVEL_TRACER
  struct pred_entry *cp_pred_entry;
#endif /* LOW_LEVEL_TRACER */
};

#ifdef DETERMINISTIC_TABLING
struct deterministic_generator_choicept {
  struct deterministic_choicept cp;
  struct subgoal_frame *cp_sg_fr;
#ifdef LOW_LEVEL_TRACER
  struct pred_entry *cp_pred_entry;
#endif /* LOW_LEVEL_TRACER */
};
#endif /* DETERMINISTIC_TABLING */

struct consumer_choicept {
  struct choicept cp;
  struct dependency_frame *cp_dep_fr;
#ifdef LOW_LEVEL_TRACER
  struct pred_entry *cp_pred_entry;
#endif /* LOW_LEVEL_TRACER */
};

struct loader_choicept {
  struct choicept cp;
  struct answer_trie_node *cp_last_answer;
#ifdef LOW_LEVEL_TRACER
  struct pred_entry *cp_pred_entry;
#endif /* LOW_LEVEL_TRACER */
};



/****************************
**      subgoal_frame      **
****************************/

typedef struct subgoal_frame {
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;
#endif /* YAPOR || THREADS */
#ifdef YAPOR
  int generator_worker;
  struct or_frame *top_or_frame_on_generator_branch;
#endif /* YAPOR */
  yamop *code_of_subgoal;
  enum {  /* do not change order !!! */
    incomplete      = 0,  /* INCOMPLETE_TABLING */
    ready           = 1,
    evaluating      = 2,
    complete        = 3,
    complete_in_use = 4,  /* LIMIT_TABLING */
    compiled        = 5,
    compiled_in_use = 6   /* LIMIT_TABLING */
  } state_flag;
  choiceptr generator_choice_point;
  struct answer_trie_hash *hash_chain;
  struct answer_trie_node *answer_trie;
  struct answer_trie_node *first_answer;
  struct answer_trie_node *last_answer;
#ifdef INCOMPLETE_TABLING
  struct answer_trie_node *try_answer;
#endif /* INCOMPLETE_TABLING */
#ifdef LIMIT_TABLING
  struct subgoal_frame *previous;
#endif /* LIMIT_TABLING */
  struct subgoal_frame *next;
} *sg_fr_ptr;

#define SgFr_lock(X)           ((X)->lock)
#define SgFr_gen_worker(X)     ((X)->generator_worker)
#define SgFr_gen_top_or_fr(X)  ((X)->top_or_frame_on_generator_branch)
#define SgFr_code(X)           ((X)->code_of_subgoal)
#define SgFr_tab_ent(X)        (((X)->code_of_subgoal)->u.Otapl.te)
#define SgFr_arity(X)          (((X)->code_of_subgoal)->u.Otapl.s)
#define SgFr_state(X)          ((X)->state_flag)
#define SgFr_gen_cp(X)         ((X)->generator_choice_point)
#define SgFr_hash_chain(X)     ((X)->hash_chain)
#define SgFr_answer_trie(X)    ((X)->answer_trie)
#define SgFr_first_answer(X)   ((X)->first_answer)
#define SgFr_last_answer(X)    ((X)->last_answer)
#define SgFr_try_answer(X)     ((X)->try_answer)
#define SgFr_previous(X)       ((X)->previous)
#define SgFr_next(X)           ((X)->next)

/**************************************************************************************************

  SgFr_lock:          spin-lock to modify the frame fields.
  SgFr_gen_worker:    the id of the worker that had allocated the frame.
  SgFr_gen_top_or_fr: a pointer to the top or-frame in the generator choice point branch. 
                      When the generator choice point is shared the pointer is updated 
                      to its or-frame. It is used to find the direct dependency node for 
                      consumer nodes in other workers branches.
  SgFr_code           initial instruction of the subgoal's compiled code.
  SgFr_tab_ent        a pointer to the correspondent table entry.
  SgFr_arity          the arity of the subgoal.
  SgFr_state:         a flag that indicates the subgoal state.
  SgFr_gen_cp:        a pointer to the correspondent generator choice point.
  SgFr_hash_chain:    a pointer to the first answer_trie_hash struct for the subgoal in hand.
  SgFr_answer_trie:   a pointer to the top answer trie node.
                      It is used to check for/insert new answers.
  SgFr_first_answer:  a pointer to the bottom answer trie node of the first available answer.
  SgFr_last_answer:   a pointer to the bottom answer trie node of the last available answer.
  SgFr_try_answer:    a pointer to the bottom answer trie node of the last tried answer.
                      It is used when a subgoal was not completed during the previous evaluation.
                      Not completed subgoals start by trying the answers already found.
  SgFr_previous:      a pointer to the previous subgoal frame on the chain.
  SgFr_next:          a pointer to the next subgoal frame on the chain.

**************************************************************************************************/



/*******************************
**      dependency_frame      **
*******************************/

typedef struct dependency_frame {
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;
#endif /* YAPOR || THREADS */
#ifdef YAPOR
  int leader_dependency_is_on_stack;
  struct or_frame *top_or_frame;
#ifdef TIMESTAMP_CHECK
  long timestamp;
#endif /* TIMESTAMP_CHECK */
#endif /* YAPOR */
  choiceptr backchain_choice_point;
  choiceptr leader_choice_point;
  choiceptr consumer_choice_point;
  struct answer_trie_node *last_consumed_answer;
  struct dependency_frame *next;
} *dep_fr_ptr;

#define DepFr_lock(X)                    ((X)->lock)
#define DepFr_leader_dep_is_on_stack(X)  ((X)->leader_dependency_is_on_stack)
#define DepFr_top_or_fr(X)               ((X)->top_or_frame)
#define DepFr_timestamp(X)               ((X)->timestamp)
#define DepFr_backchain_cp(X)            ((X)->backchain_choice_point)
#define DepFr_leader_cp(X)               ((X)->leader_choice_point)
#define DepFr_cons_cp(X)                 ((X)->consumer_choice_point)
#define DepFr_last_answer(X)             ((X)->last_consumed_answer)
#define DepFr_next(X)                    ((X)->next)

/*******************************************************************************************************

  DepFr_lock:                   lock variable to modify the frame fields.
  DepFr_leader_dep_is_on_stack: the generator choice point for the correspondent consumer choice point 
                                is on the worker's stack (FALSE/TRUE).
  DepFr_top_or_fr:              a pointer to the top or-frame in the consumer choice point branch. 
                                When the consumer choice point is shared the pointer is updated to 
                                its or-frame. It is used to update the LOCAL_top_or_fr when a worker 
                                backtracks through answers.
  DepFr_timestamp:              a timestamp used to optimize the search for suspension frames to be 
                                resumed.
  DepFr_backchain_cp:           a pointer to the nearest choice point with untried alternatives.
                                It is used to efficiently return (backtrack) to the leader node where 
                                we perform the last backtracking through answers operation.
  DepFr_leader_cp:              a pointer to the leader choice point.
  DepFr_cons_cp:                a pointer to the correspondent consumer choice point.
  DepFr_last_answer:            a pointer to the last consumed answer.
  DepFr_next:                   a pointer to the next dependency frame on the chain.  

*******************************************************************************************************/



/*******************************
**      suspension_frame      **
*******************************/

#ifdef YAPOR
typedef struct suspension_frame {
  struct or_frame *top_or_frame_on_stack;
  struct dependency_frame *top_dependency_frame;
  struct subgoal_frame *top_subgoal_frame;
  struct suspended_block {
    void *resume_register;
    void *block_start;
    long block_size;
  } global_block, local_block, trail_block;
  struct suspension_frame *next;
} *susp_fr_ptr;
#endif /* YAPOR */

#define SuspFr_top_or_fr_on_stack(X)  ((X)->top_or_frame_on_stack)
#define SuspFr_top_dep_fr(X)          ((X)->top_dependency_frame)
#define SuspFr_top_sg_fr(X)           ((X)->top_subgoal_frame)
#define SuspFr_global_reg(X)          ((X)->global_block.resume_register)
#define SuspFr_global_start(X)        ((X)->global_block.block_start)
#define SuspFr_global_size(X)         ((X)->global_block.block_size)
#define SuspFr_local_reg(X)           ((X)->local_block.resume_register)
#define SuspFr_local_start(X)         ((X)->local_block.block_start)
#define SuspFr_local_size(X)          ((X)->local_block.block_size)
#define SuspFr_trail_reg(X)           ((X)->trail_block.resume_register)
#define SuspFr_trail_start(X)         ((X)->trail_block.block_start)
#define SuspFr_trail_size(X)          ((X)->trail_block.block_size)
#define SuspFr_next(X)                ((X)->next)
