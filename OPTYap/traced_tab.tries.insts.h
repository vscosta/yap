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
 **           Trie instructions: auxiliary stack organization           **
 *************************************************************************
               -------------------
               | ha = heap_arity | 
               -------------------  --
               |   heap ptr 1    |    |
               -------------------    |
               |       ...       |    -- heap_arity (0 if in global trie)
               -------------------    |
               |   heap ptr ha   |    |
               -------------------  --
               | va = vars_arity |
               -------------------  --
               |    var ptr va   |    |
               -------------------    |
               |       ...       |    -- vars_arity
               -------------------    |
               |    var ptr 1    |    |
               -------------------  -- 
               | sa = subs_arity |
               -------------------  --
               |   subs ptr sa   |    |
               -------------------    |
               |       ...       |    -- subs_arity 
               -------------------    |
               |   subs ptr 1    |    |
               -------------------  --
************************************************************************/



/************************************************************************
 **                      Trie instructions: macros                      **
 ************************************************************************/

#define TOP_STACK          YENV

#define HEAP_ARITY_ENTRY   (0)
#define VARS_ARITY_ENTRY   (1 + heap_arity)
#define SUBS_ARITY_ENTRY   (1 + heap_arity + 1 + vars_arity)

/* macros 'HEAP_ENTRY', 'VARS_ENTRY' and 'SUBS_ENTRY' **
** assume that INDEX starts at 1 (and not at 0 !!!)   */
#define HEAP_ENTRY(INDEX)  (HEAP_ARITY_ENTRY + (INDEX))
#define VARS_ENTRY(INDEX)  (VARS_ARITY_ENTRY + 1 + vars_arity - (INDEX))
#define SUBS_ENTRY(INDEX)  (SUBS_ARITY_ENTRY + 1 + subs_arity - (INDEX))

#define next_trie_instruction(NODE)		\
  PREG = (yamop *) TrNode_child(NODE);		\
  PREFETCH_OP(PREG);				\
  GONext()

#define next_instruction(CONDITION, NODE)	\
  if (CONDITION) {				\
    PREG = (yamop *) TrNode_child(NODE);	\
  } else {  /* procceed */			\
    PREG = (yamop *) CPREG;			\
    TOP_STACK = ENV;				\
  }						\
  PREFETCH_OP(PREG);				\
  GONext()

#define copy_aux_stack()                                                \
  { int size = 3 + heap_arity + subs_arity + vars_arity;		\
  TOP_STACK -= size;							\
  memmove(TOP_STACK, aux_stack, size * sizeof(CELL *));			\
  aux_stack = TOP_STACK;						\
  }/* macros 'store_trie_node', 'restore_trie_node' and 'pop_trie_node'   **
   ** do not include 'set_cut' because trie instructions are cut safe     */

#define store_trie_node(AP)				\
  { register choiceptr cp;				\
    TOP_STACK = (CELL *) (NORM_CP(TOP_STACK) - 1);	\
    cp = NORM_CP(TOP_STACK);				\
    HBREG = HR;						\
    store_yaam_reg_cpdepth(cp);				\
    cp->cp_tr = TR;					\
    cp->cp_h  = HR;					\
    cp->cp_b  = B;					\
    cp->cp_cp = CPREG;					\
    cp->cp_ap = (yamop *) AP;				\
    cp->cp_env= ENV;					\
    B = cp;						\
    YAPOR_SET_LOAD(B);					\
    SET_BB(B);						\
    TABLING_ERROR_CHECKING_STACK;			\
  }							\
    copy_aux_stack()

#define restore_trie_node(AP)			\
  HR = HBREG = PROTECT_FROZEN_H(B);		\
  restore_yaam_reg_cpdepth(B);			\
  CPREG = B->cp_cp;				\
  ENV = B->cp_env;				\
  YAPOR_update_alternative(PREG, (yamop *) AP)	\
  B->cp_ap = (yamop *) AP;			\
  TOP_STACK = (CELL *) PROTECT_FROZEN_B(B);	\
  SET_BB(NORM_CP(TOP_STACK));			\
  copy_aux_stack()

#define really_pop_trie_node()				\
  TOP_STACK = (CELL *) PROTECT_FROZEN_B((B + 1));	\
  HR = PROTECT_FROZEN_H(B);				\
  pop_yaam_reg_cpdepth(B);				\
  CPREG = B->cp_cp;					\
  TABLING_close_alt(B);					\
  ENV = B->cp_env;					\
  B = B->cp_b;						\
  HBREG = PROTECT_FROZEN_H(B);				\
  SET_BB(PROTECT_FROZEN_B(B));				\
  if ((choiceptr) TOP_STACK == B_FZ) {			\
    copy_aux_stack();					\
  }

#ifdef YAPOR
#define pop_trie_node()				\
  if (SCH_top_shared_cp(B)) {			\
    restore_trie_node(NULL);			\
  } else {					\
    really_pop_trie_node();			\
  }
#else
#define pop_trie_node()				\
  really_pop_trie_node()
#endif /* YAPOR */



/************************************************************************
 **                         aux_stack_null_instr                        **
 ************************************************************************/

#define aux_stack_null_instr()			\
  next_trie_instruction(node)



/************************************************************************
 **                      aux_stack_extension_instr                      **
 ************************************************************************/

#define aux_stack_extension_instr()                                     \
  TOP_STACK = &aux_stack[-2];						\
  TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity + 2;				\
  TOP_STACK[HEAP_ENTRY(1)] = TrNode_entry(node);			\
  TOP_STACK[HEAP_ENTRY(2)] = 0;             /* extension mark */	\
  next_trie_instruction(node)



/************************************************************************
 **                    aux_stack_term_(in_pair_)instr                   **
 ************************************************************************/

#define aux_stack_term_instr()					\
  if (heap_arity) {						\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], t);		\
    TOP_STACK = &aux_stack[1];					\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity - 1;		\
    next_instruction(heap_arity - 1 || subs_arity, node);	\
  } else {							\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], t);		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;		\
    next_instruction(subs_arity - 1, node);			\
  }

#define aux_stack_term_in_pair_instr()                                  \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-1];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = 1;					\
  }									\
  Bind_Global(HR, TrNode_entry(node));					\
  TOP_STACK[HEAP_ENTRY(1)] = (CELL) (HR + 1);				\
  HR += 2;								\
  next_trie_instruction(node)



/************************************************************************
 **                      aux_stack_(new_)pair_instr                     **
 ************************************************************************/

#define aux_stack_new_pair_instr()    /* for term 'CompactPairInit' */  \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
    TOP_STACK = &aux_stack[-1];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity + 1;			\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-2];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = 2;					\
  }									\
  TOP_STACK[HEAP_ENTRY(1)] = (CELL) HR;					\
  TOP_STACK[HEAP_ENTRY(2)] = (CELL) (HR + 1);				\
  HR += 2;								\
  next_trie_instruction(node)

#ifdef TRIE_COMPACT_PAIRS
#define aux_stack_pair_instr()	   /* for term 'CompactPairEndList' */  \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-1];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = 1;					\
  }									\
  TOP_STACK[HEAP_ENTRY(1)] = (CELL) HR;					\
  Bind_Global(HR + 1, TermNil);						\
  HR += 2;								\
  next_trie_instruction(node)
#else
#define aux_stack_pair_instr()			\
  aux_stack_new_pair_instr()
#endif /* TRIE_COMPACT_PAIRS */



/************************************************************************
 **                    aux_stack_appl_(in_pair_)instr                   **
 ************************************************************************/

#define aux_stack_appl_instr()                                          \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsAppl(HR));	\
    TOP_STACK = &aux_stack[-func_arity + 1];				\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity + func_arity - 1;		\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsAppl(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-func_arity];				\
    TOP_STACK[HEAP_ARITY_ENTRY] = func_arity;				\
  }									\
  *HR = (CELL) func;							\
  { int i;								\
    for (i = 1; i <= func_arity; i++)					\
      TOP_STACK[HEAP_ENTRY(i)] = (CELL) (HR + i);			\
  }									\
  HR += 1 + func_arity;							\
  next_trie_instruction(node)

#define aux_stack_appl_in_pair_instr()	                                \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
    TOP_STACK = &aux_stack[-func_arity];				\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity + func_arity;		\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-func_arity - 1];				\
    TOP_STACK[HEAP_ARITY_ENTRY] = func_arity + 1;			\
  }									\
  TOP_STACK[HEAP_ENTRY(func_arity + 1)] = (CELL) (HR + 1);		\
  Bind_Global(HR, AbsAppl(HR + 2));					\
  HR += 2;								\
  *HR = (CELL) func;							\
  { int i;								\
    for (i = 1; i <= func_arity; i++)					\
      TOP_STACK[HEAP_ENTRY(i)] = (CELL) (HR + i);			\
  }									\
  HR += 1 + func_arity;							\
  next_trie_instruction(node)



/************************************************************************
 **                    aux_stack_var_(in_pair_)instr                    **
 ************************************************************************/

#define aux_stack_var_instr()					\
  if (heap_arity) {						\
    int i;							\
    CELL var = aux_stack[HEAP_ENTRY(1)];			\
    RESET_VARIABLE(var);					\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity - 1;		\
    for (i = 2; i <= heap_arity; i++)				\
      TOP_STACK[HEAP_ENTRY(i - 1)] = aux_stack[HEAP_ENTRY(i)];	\
    aux_stack[VARS_ARITY_ENTRY - 1] = vars_arity + 1;		\
    aux_stack[VARS_ENTRY(vars_arity + 1)] = var;		\
    next_instruction(heap_arity - 1 || subs_arity, node);	\
  } else {				                        \
    CELL var = aux_stack[SUBS_ENTRY(1)];			\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;		\
    TOP_STACK = &aux_stack[-1];					\
    TOP_STACK[HEAP_ARITY_ENTRY] = 0;				\
    aux_stack[VARS_ARITY_ENTRY - 1] = vars_arity + 1;		\
    aux_stack[VARS_ENTRY(vars_arity + 1)] = var;		\
    next_instruction(subs_arity - 1, node);	                \
  }							 

#define aux_stack_var_in_pair_instr()                                   \
  if (heap_arity) {							\
    int i;								\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
    TOP_STACK = &aux_stack[-1];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity;				\
    TOP_STACK[HEAP_ENTRY(1)] = (CELL) (HR + 1);				\
    for (i = 2; i <= heap_arity; i++)					\
      TOP_STACK[HEAP_ENTRY(i)] = aux_stack[HEAP_ENTRY(i)];		\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-2];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = 1;					\
    TOP_STACK[HEAP_ENTRY(1)] = (CELL) (HR + 1);				\
  }									\
  aux_stack[VARS_ARITY_ENTRY - 1] = vars_arity + 1;			\
  aux_stack[VARS_ENTRY(vars_arity + 1)] = (CELL) HR;			\
  RESET_VARIABLE((CELL) HR);						\
  HR += 2;								\
  next_trie_instruction(node)



/************************************************************************
 **                    aux_stack_val_(in_pair_)instr                    **
 ************************************************************************/

#define aux_stack_val_instr()					\
  if (heap_arity) {						\
    CELL aux_sub, aux_var;				        \
    aux_sub = aux_stack[HEAP_ENTRY(1)];				\
    aux_var = aux_stack[VARS_ENTRY(var_index + 1)];		\
    if (aux_sub > aux_var) {					\
      Bind_Global((CELL *) aux_sub, aux_var);			\
    } else {							\
      RESET_VARIABLE(aux_sub);					\
      Bind_Local((CELL *) aux_var, aux_sub);			\
      aux_stack[VARS_ENTRY(var_index + 1)] = aux_sub;		\
    }								\
    TOP_STACK = &aux_stack[1];					\
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity - 1;		\
    next_instruction(heap_arity - 1 || subs_arity, node);	\
  } else {							\
    CELL aux_sub, aux_var;					\
    aux_sub = aux_stack[SUBS_ENTRY(1)];				\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;		\
    aux_var = aux_stack[VARS_ENTRY(var_index + 1)];		\
    if (aux_sub > aux_var) {					\
      if ((CELL *) aux_sub <= HR) {				\
	Bind_Global((CELL *) aux_sub, aux_var);			\
      } else if ((CELL *) aux_var <= HR) {			\
	Bind_Local((CELL *) aux_sub, aux_var);			\
      } else {							\
	Bind_Local((CELL *) aux_var, aux_sub);			\
	aux_stack[VARS_ENTRY(var_index + 1)] = aux_sub;		\
      }								\
    } else {							\
      if ((CELL *) aux_var <= HR) {				\
	Bind_Global((CELL *) aux_var, aux_sub);			\
	aux_stack[VARS_ENTRY(var_index + 1)] = aux_sub;		\
      } else if ((CELL *) aux_sub <= HR) {			\
	Bind_Local((CELL *) aux_var, aux_sub);			\
	aux_stack[VARS_ENTRY(var_index + 1)] = aux_sub;		\
      } else {							\
	Bind_Local((CELL *) aux_sub, aux_var);			\
      }								\
    }								\
    next_instruction(subs_arity - 1, node);			\
  }

#define aux_stack_val_in_pair_instr()                                   \
  if (heap_arity) {							\
    Bind_Global((CELL *) aux_stack[HEAP_ENTRY(1)], AbsPair(HR));	\
  } else {								\
    YapBind((CELL *) aux_stack[SUBS_ENTRY(1)], AbsPair(HR));		\
    aux_stack[SUBS_ARITY_ENTRY] = subs_arity - 1;			\
    TOP_STACK = &aux_stack[-1];						\
    TOP_STACK[HEAP_ARITY_ENTRY] = 1;					\
  }									\
        { CELL aux_sub, aux_var;                                        \
          aux_sub = (CELL) HR;                                          \
          aux_var = aux_stack[VARS_ENTRY(var_index + 1)];               \
          if (aux_sub > aux_var) {                                      \
            Bind_Global((CELL *) aux_sub, aux_var);                     \
          } else {                                                      \
            RESET_VARIABLE(aux_sub);                                    \
	    Bind_Local((CELL *) aux_var, aux_sub);                      \
            aux_stack[VARS_ENTRY(var_index + 1)] = aux_sub;             \
          }                                                             \
        }                                                               \
        TOP_STACK[HEAP_ENTRY(1)] = (CELL) (HR + 1);			\
        HR += 2;							\
        next_trie_instruction(node)


/************************************************************************
**                          Trie instructions                          **
************************************************************************/

PBOp(trie_do_var, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }

ENDPBOp();


PBOp(trie_trust_var, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


PBOp(trie_try_var, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();
  


PBOp(trie_retry_var, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_var_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_var_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_var_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_var_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_val, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_val, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_val, e)
   { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_val, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_val_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_val_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_val_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_val_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_atom, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_atom, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_atom, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_atom, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_atom_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_atom_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_atom_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_atom_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_null, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_null, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_null, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_null, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_null_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_null_in_pair, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


PBOp(trie_try_null_in_pair, e)
 { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


 PBOp(trie_retry_null_in_pair, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


PBOp(trie_do_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_appl, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_appl, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_appl, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_appl, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_appl_in_pair, e)
{ printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_appl_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_appl_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_appl_in_pair, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_extension, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_extension, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_extension, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_extension, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_do_double, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack = TOP_STACK;
    int heap_arity = aux_stack[HEAP_ARITY_ENTRY];
    int vars_arity = aux_stack[VARS_ARITY_ENTRY];
    int subs_arity = aux_stack[SUBS_ARITY_ENTRY];
    volatile union {
      Float dbl;
      Term ts[SIZEOF_DOUBLE/SIZEOF_INT_P];
    } td;
    Term t;

#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    td.ts[0] = aux_stack[HEAP_ENTRY(1)];
    td.ts[1] = aux_stack[HEAP_ENTRY(3)];  /* jump the first extension mark */
    heap_arity -= 4;
    TOP_STACK = aux_stack = &aux_stack[4];  /* jump until the second extension mark */
#else /* SIZEOF_DOUBLE == SIZEOF_INT_P */
    td.ts[0] = aux_stack[HEAP_ENTRY(1)];
    heap_arity -= 2;
    TOP_STACK = aux_stack = &aux_stack[2];  /* jump until the extension mark */
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity;
    t = MkFloatTerm(td.dbl);
    aux_stack_term_instr();
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  BOp(trie_trust_double, e)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "trie_trust_double: invalid instruction");
  ENDBOp();


  BOp(trie_try_double, e)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "trie_try_double: invalid instruction");
  ENDBOp();


  BOp(trie_retry_double, e)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "trie_retry_double: invalid instruction");
  ENDBOp();


  PBOp(trie_do_longint, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack = TOP_STACK;
    int heap_arity = aux_stack[HEAP_ARITY_ENTRY];
    int vars_arity = aux_stack[VARS_ARITY_ENTRY];
    int subs_arity = aux_stack[SUBS_ARITY_ENTRY];
    Term t = MkLongIntTerm(aux_stack[HEAP_ENTRY(1)]);

    heap_arity -= 2;
    TOP_STACK = aux_stack = &aux_stack[2];  /* jump until the extension mark */
    TOP_STACK[HEAP_ARITY_ENTRY] = heap_arity;
    aux_stack_term_instr();
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  BOp(trie_trust_longint, e)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "trie_trust_longint: invalid instruction");
  ENDBOp();


  BOp(trie_try_longint, e)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "trie_try_longint: invalid instruction");
  ENDBOp();


  BOp(trie_retry_longint, e)
  ENDBOp();


  PBOp(trie_do_bigint, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  BOp(trie_trust_bigint, e)
  ENDBOp();


  BOp(trie_try_bigint, e)
  ENDBOp();


  BOp(trie_retry_bigint, e)
  ENDBOp();



  PBOp(trie_do_gterm, e)

  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_trust_gterm, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_try_gterm, e)
   { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();


  PBOp(trie_retry_gterm, e)
  { printf("Tries not supported by JIT!!\n"); exit(1); }
ENDPBOp();
