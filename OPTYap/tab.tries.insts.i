/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        tab.tries.insts.i
  version:     $Id: tab.tries.insts.i,v 1.12 2007-04-26 14:11:08 ricroc Exp $   
                                                                     
**********************************************************************/

/* --------------------------------------------------------- **
**      Trie instructions: auxiliary stack organization      **
** --------------------------------------------------------- **
                 STANDARD_TRIE
              -------------------
              | ha = heap_arity | 
              -------------------  --
              |   heap ptr 1    |    |
              -------------------    |
              |       ...       |    -- heap_arity
              -------------------    |
              |   heap ptr ha   |    |
              -------------------  --
              | va = vars_arity |
              -------------------
              | sa = subs_arity |
              -------------------  --
              |   subs ptr sa   |    |
              -------------------    |
              |       ...       |    -- subs_arity 
              -------------------    |
              |   subs ptr 1    |    |
              -------------------  --
              |    var ptr va   |    |
              -------------------    |
              |       ...       |    -- vars_arity
              -------------------    |
              |    var ptr 1    |    |
              -------------------  -- 


                  GLOBAL_TRIE
              -------------------
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
** --------------------------------------------------------- */



/* --------------------------------------------- **
**      Trie instructions: auxiliary macros      **
** --------------------------------------------- */

#ifdef GLOBAL_TRIE
#define copy_arity_stack()                                      \
        { int size = subs_arity + vars_arity + 2;               \
          YENV -= size;                                         \
          memcpy(YENV, aux_stack_ptr, size * sizeof(CELL *));   \
          aux_stack_ptr = YENV;                                 \
	}
#else
#define copy_arity_stack()                                      \
        { int size = heap_arity + subs_arity + vars_arity + 3;  \
          YENV -= size;                                         \
          memcpy(YENV, aux_stack_ptr, size * sizeof(CELL *));   \
          aux_stack_ptr = YENV;                                 \
	}
#endif /* GLOBAL_TRIE */

#define next_trie_instruction(NODE)                             \
        PREG = (yamop *) TrNode_child(NODE);                    \
        PREFETCH_OP(PREG);                                      \
        GONext()

#define next_instruction(CONDITION, NODE)                       \
        if (CONDITION) {                                        \
          PREG = (yamop *) TrNode_child(NODE);                  \
        } else {                                                \
          /* procceed */                                        \
	  PREG = (yamop *) CPREG;                               \
	  YENV = ENV;                                           \
        }                                                       \
        PREFETCH_OP(PREG);                                      \
        GONext()



/* ---------------------------------------------------------------------------- **
** the 'store_trie_node', 'restore_trie_node' and 'pop_trie_node' macros do not **
** include the 'set_cut' macro because there are no cuts in trie instructions.  **
** ---------------------------------------------------------------------------- */

#define store_trie_node(AP)                           \
        { register choiceptr cp;                      \
          YENV = (CELL *) (NORM_CP(YENV) - 1);        \
          cp = NORM_CP(YENV);                         \
          HBREG = H;                                  \
          store_yaam_reg_cpdepth(cp);                 \
          cp->cp_tr = TR;                             \
          cp->cp_h  = H;                              \
          cp->cp_b  = B;                              \
          cp->cp_cp = CPREG;                          \
          cp->cp_ap = (yamop *) AP;                   \
          cp->cp_env= ENV;                            \
          B = cp;                                     \
          YAPOR_SET_LOAD(B);                          \
          SET_BB(B);                                  \
          TABLING_ERRORS_check_stack;                 \
	}                                             \
        copy_arity_stack()

#define restore_trie_node(AP)                         \
        H = HBREG = PROTECT_FROZEN_H(B);              \
        restore_yaam_reg_cpdepth(B);                  \
        CPREG = B->cp_cp;                             \
        ENV = B->cp_env;                              \
        YAPOR_update_alternative(PREG, (yamop *) AP)  \
        B->cp_ap = (yamop *) AP;                      \
        YENV = (CELL *) PROTECT_FROZEN_B(B);          \
        SET_BB(NORM_CP(YENV));                        \
        copy_arity_stack()

#define really_pop_trie_node()                        \
        YENV = (CELL *) PROTECT_FROZEN_B((B + 1));    \
        H = PROTECT_FROZEN_H(B);                      \
        pop_yaam_reg_cpdepth(B);                      \
	CPREG = B->cp_cp;                             \
        TABLING_close_alt(B);                         \
        ENV = B->cp_env;                              \
	B = B->cp_b;                                  \
        HBREG = PROTECT_FROZEN_H(B);                  \
        SET_BB(PROTECT_FROZEN_B(B));                  \
        if ((choiceptr) YENV == B_FZ) {               \
          copy_arity_stack();                         \
        }

#ifdef YAPOR
#define pop_trie_node()                               \
        if (SCH_top_shared_cp(B)) {                   \
          restore_trie_node(NULL);                    \
        } else {                                      \
          really_pop_trie_node();                     \
        }
#else
#define pop_trie_node()  really_pop_trie_node()
#endif /* YAPOR */



/* ------------------- **
**      trie_null      **
** ------------------- */

#define stack_trie_null_instr()                              \
        next_trie_instruction(node)

#ifdef TRIE_COMPACT_PAIRS
/* trie compiled code for term 'CompactPairInit' */
#define stack_trie_null_in_new_pair_instr()                  \
        if (heap_arity) {                                    \
          aux_stack_ptr++;                                   \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));  \
          *aux_stack_ptr-- = (CELL) (H + 1);                 \
          *aux_stack_ptr-- = (CELL) H;                       \
          *aux_stack_ptr = heap_arity - 1 + 2;               \
          YENV = aux_stack_ptr;                              \
        } else {                                             \
          int i;                                             \
          *aux_stack_ptr-- = (CELL) (H + 1);                 \
          *aux_stack_ptr-- = (CELL) H;                       \
          *aux_stack_ptr = 2;                                \
          YENV = aux_stack_ptr;                              \
          aux_stack_ptr += 2 + 2;                            \
          *aux_stack_ptr = subs_arity - 1;                   \
          aux_stack_ptr += subs_arity;                       \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));         \
          for (i = 0; i < vars_arity; i++) {                 \
            *aux_stack_ptr = *(aux_stack_ptr + 1);           \
            aux_stack_ptr++;                                 \
          }                                                  \
        }                                                    \
        H += 2;                                              \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* ------------------ **
**      trie_var      **
** ------------------ */

#define stack_trie_var_instr()                                   \
        if (heap_arity) {                                        \
          CELL var;                                              \
          int i;                                                 \
          *aux_stack_ptr = heap_arity - 1;                       \
          var = *++aux_stack_ptr;                                \
          RESET_VARIABLE(var);                                   \
          for (i = 0; i < heap_arity - 1; i++) {                 \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
          *aux_stack_ptr++ = vars_arity + 1;                     \
          *aux_stack_ptr++ = subs_arity;                         \
          for (i = 0; i < subs_arity; i++) {                     \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
          *aux_stack_ptr = var;                                  \
          next_instruction(heap_arity - 1 || subs_arity, node);  \
        } else {                                                 \
          *++aux_stack_ptr = vars_arity + 1;                     \
          *++aux_stack_ptr = subs_arity - 1;                     \
          next_instruction(subs_arity - 1, node);                \
        }

#ifdef TRIE_COMPACT_PAIRS
#define stack_trie_var_in_new_pair_instr()                       \
        if (heap_arity) {                                        \
          int i;                                                 \
          *aux_stack_ptr-- = (CELL) (H + 1);                     \
          *aux_stack_ptr = heap_arity - 1 + 1;                   \
          YENV = aux_stack_ptr;                                  \
          aux_stack_ptr += 2;                                    \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));      \
          for (i = 0; i < heap_arity - 1; i++) {                 \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
          *aux_stack_ptr++ = vars_arity + 1;                     \
          *aux_stack_ptr++ = subs_arity;                         \
          for (i = 0; i < subs_arity; i++) {                     \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
          *aux_stack_ptr = (CELL) H;                             \
        } else {                                                 \
          *aux_stack_ptr-- = (CELL) (H + 1);                     \
          *aux_stack_ptr = 1;                                    \
          YENV = aux_stack_ptr;                                  \
          aux_stack_ptr += 2;                                    \
          *aux_stack_ptr++ = vars_arity + 1;                     \
          *aux_stack_ptr = subs_arity - 1;                       \
          aux_stack_ptr += subs_arity;                           \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));             \
          *aux_stack_ptr = (CELL) H;                             \
        }                                                        \
        RESET_VARIABLE((CELL) H);                                \
        H += 2;                                                  \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* ------------------ **
**      trie_val      **
** ------------------ */

#define stack_trie_val_instr()                                                              \
        if (heap_arity) {                                                                   \
          CELL aux_sub, aux_var, *vars_ptr;				                    \
          YENV = ++aux_stack_ptr;                                                           \
          vars_ptr = aux_stack_ptr + heap_arity + 1 + subs_arity + vars_arity - var_index;  \
          aux_sub = *aux_stack_ptr;                                                         \
          aux_var = *vars_ptr;                                                              \
          if (aux_sub > aux_var) {                                                          \
            Bind_Global((CELL *) aux_sub, aux_var);                                         \
          } else {                                                                          \
            RESET_VARIABLE(aux_sub);                                                        \
	    Bind_Local((CELL *) aux_var, aux_sub);                                          \
            *vars_ptr = aux_sub;                                                            \
          }                                                                                 \
          *aux_stack_ptr = heap_arity - 1;                                                  \
          next_instruction(heap_arity - 1 || subs_arity, node);                             \
        } else {                                                                            \
          CELL aux_sub, aux_var, *vars_ptr;                                                 \
          int i;                                                                            \
          aux_stack_ptr += 2;                                                               \
          *aux_stack_ptr = subs_arity - 1;                                                  \
          aux_stack_ptr += subs_arity;                                                      \
          vars_ptr = aux_stack_ptr + vars_arity - var_index;                                \
          aux_sub = *aux_stack_ptr;                                                         \
          aux_var = *vars_ptr;                                                              \
          if (aux_sub > aux_var) {                                                          \
	    if ((CELL *) aux_sub <= H) {                                                    \
              Bind_Global((CELL *) aux_sub, aux_var);                                       \
            } else if ((CELL *) aux_var <= H) {                                             \
              Bind_Local((CELL *) aux_sub, aux_var);                                        \
            } else {                                                                        \
              Bind_Local((CELL *) aux_var, aux_sub);                                        \
              *vars_ptr = aux_sub;                                                          \
            }                                                                               \
          } else {                                                                          \
	    if ((CELL *) aux_var <= H) {                                                    \
              Bind_Global((CELL *) aux_var, aux_sub);                                       \
              *vars_ptr = aux_sub;                                                          \
            } else if ((CELL *) aux_sub <= H) {                                             \
              Bind_Local((CELL *) aux_var, aux_sub);                                        \
              *vars_ptr = aux_sub;                                                          \
            } else {                                                                        \
              Bind_Local((CELL *) aux_sub, aux_var);                                        \
            }                                                                               \
          }                                                                                 \
          for (i = 0; i < vars_arity; i++) {                                                \
            *aux_stack_ptr = *(aux_stack_ptr + 1);                                          \
            aux_stack_ptr++;                                                                \
          }                                                                                 \
          next_instruction(subs_arity - 1, node);                                           \
        }

#ifdef TRIE_COMPACT_PAIRS      
#define stack_trie_val_in_new_pair_instr()                                                  \
        if (heap_arity) {                                                                   \
          CELL aux_sub, aux_var, *vars_ptr;	      	               		            \
          aux_stack_ptr++;				                                    \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));                                 \
          *aux_stack_ptr = (CELL) (H + 1);                                                  \
          aux_sub = (CELL) H;                                                               \
          vars_ptr = aux_stack_ptr + heap_arity + 1 + subs_arity + vars_arity - var_index;  \
          aux_var = *vars_ptr;                                                              \
          if (aux_sub > aux_var) {                                                          \
            Bind_Global((CELL *) aux_sub, aux_var);                                         \
          } else {                                                                          \
            RESET_VARIABLE(aux_sub);                                                        \
  	    Bind_Local((CELL *) aux_var, aux_sub);                                          \
            *vars_ptr = aux_sub;                                                            \
          }                                                                                 \
        } else {                                                                            \
          CELL aux_sub, aux_var, *vars_ptr;                                                 \
          int i;                                                                            \
          *aux_stack_ptr-- = (CELL) (H + 1);                                                \
          *aux_stack_ptr = 1;                                                               \
          YENV = aux_stack_ptr;                                                             \
          aux_stack_ptr += 1 + 2;                                                           \
          aux_sub = (CELL) H;                                                               \
          vars_ptr = aux_stack_ptr + subs_arity + vars_arity - var_index;                   \
          aux_var = *vars_ptr;                                                              \
          if (aux_sub > aux_var) {                                                          \
            Bind_Global((CELL *) aux_sub, aux_var);                                         \
          } else {                                                                          \
            RESET_VARIABLE(aux_sub);                                                        \
	    Bind_Local((CELL *) aux_var, aux_sub);                                          \
            *vars_ptr = aux_sub;                                                            \
          }                                                                                 \
          *aux_stack_ptr = subs_arity - 1;                                                  \
          aux_stack_ptr += subs_arity;                                                      \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));                                        \
          for (i = 0; i < vars_arity; i++) {                                                \
            *aux_stack_ptr = *(aux_stack_ptr + 1);                                          \
            aux_stack_ptr++;                                                                \
          }                                                                                 \
        }                                                                                   \
        H += 2;                                                                             \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* ------------------- **
**      trie_atom      **
** ------------------- */

#define stack_trie_atom_instr()                                      \
        if (heap_arity) {                                            \
          YENV = ++aux_stack_ptr;                                    \
          Bind_Global((CELL *) *aux_stack_ptr, TrNode_entry(node));  \
          *aux_stack_ptr = heap_arity - 1;                           \
          next_instruction(heap_arity - 1 || subs_arity, node);      \
        } else {                                                     \
          int i;                                                     \
          aux_stack_ptr += 2;                                        \
          *aux_stack_ptr = subs_arity - 1;                           \
          aux_stack_ptr += subs_arity;                               \
          Bind((CELL *) *aux_stack_ptr, TrNode_entry(node));         \
          for (i = 0; i < vars_arity; i++) {                         \
            *aux_stack_ptr = *(aux_stack_ptr + 1);                   \
            aux_stack_ptr++;                                         \
          }                                                          \
          next_instruction(subs_arity - 1, node);                    \
        }

#ifdef TRIE_COMPACT_PAIRS
#define stack_trie_atom_in_new_pair_instr()                          \
        if (heap_arity) {                                            \
          aux_stack_ptr++;		                             \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));          \
          *aux_stack_ptr = (CELL) (H + 1);                           \
        } else {                                                     \
          int i;                                                     \
          *aux_stack_ptr-- = (CELL) (H + 1);                         \
          *aux_stack_ptr = 1;                                        \
          YENV = aux_stack_ptr;                                      \
          aux_stack_ptr += 1 + 2;                                    \
          *aux_stack_ptr = subs_arity - 1;                           \
          aux_stack_ptr += subs_arity;                               \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));                 \
          for (i = 0; i < vars_arity; i++) {                         \
            *aux_stack_ptr = *(aux_stack_ptr + 1);                   \
            aux_stack_ptr++;                                         \
          }                                                          \
        }                                                            \
        Bind_Global(H, TrNode_entry(node));                          \
        H += 2;                                                      \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* ------------------- **
**      trie_pair      **
** ------------------- */

#ifdef TRIE_COMPACT_PAIRS
/* trie compiled code for term 'CompactPairEndList' */
#define stack_trie_pair_instr()		                     \
        if (heap_arity) {                                    \
          aux_stack_ptr++;                                   \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));  \
          *aux_stack_ptr = (CELL) H;                         \
	} else {                                             \
          int i;                                             \
          *aux_stack_ptr-- = (CELL) H;                       \
          *aux_stack_ptr = 1;                                \
          YENV = aux_stack_ptr;                              \
          aux_stack_ptr += 1 + 2;                            \
          *aux_stack_ptr = subs_arity - 1;                   \
          aux_stack_ptr += subs_arity;                       \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));         \
          for (i = 0; i < vars_arity; i++) {                 \
            *aux_stack_ptr = *(aux_stack_ptr + 1);           \
            aux_stack_ptr++;                                 \
          }                                                  \
	}                                                    \
        Bind_Global(H + 1, TermNil);                         \
        H += 2;                                              \
        next_trie_instruction(node)
#else
#define stack_trie_pair_instr()                              \
        if (heap_arity) {                                    \
          aux_stack_ptr++;                                   \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));  \
          *aux_stack_ptr-- = (CELL) (H + 1);                 \
          *aux_stack_ptr-- = (CELL) H;                       \
          *aux_stack_ptr = heap_arity - 1 + 2;               \
          YENV = aux_stack_ptr;                              \
        } else {                                             \
          int i;                                             \
          *aux_stack_ptr-- = (CELL) (H + 1);                 \
          *aux_stack_ptr-- = (CELL) H;                       \
          *aux_stack_ptr = 2;                                \
          YENV = aux_stack_ptr;                              \
          aux_stack_ptr += 2 + 2;                            \
          *aux_stack_ptr = subs_arity - 1;                   \
          aux_stack_ptr += subs_arity;                       \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));         \
          for (i = 0; i < vars_arity; i++) {                 \
            *aux_stack_ptr = *(aux_stack_ptr + 1);           \
            aux_stack_ptr++;                                 \
          }                                                  \
        }                                                    \
        H += 2;                                              \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* --------------------- **
**      trie_struct      **
** --------------------- */

#define stack_trie_struct_instr()                                \
        if (heap_arity) {                                        \
          int i;                                                 \
          aux_stack_ptr++;                                       \
          Bind_Global((CELL *) *aux_stack_ptr, AbsAppl(H));      \
          for (i = 0; i < func_arity; i++)                       \
            *aux_stack_ptr-- = (CELL) (H + func_arity - i);      \
          *aux_stack_ptr = heap_arity - 1 + func_arity;          \
          YENV = aux_stack_ptr;                                  \
        } else {                                                 \
          int i;                                                 \
          for (i = 0; i < func_arity; i++)                       \
            *aux_stack_ptr-- = (CELL) (H + func_arity - i);      \
          *aux_stack_ptr = func_arity;                           \
          YENV = aux_stack_ptr;                                  \
          aux_stack_ptr += func_arity + 2;                       \
          *aux_stack_ptr = subs_arity - 1;                       \
          aux_stack_ptr += subs_arity;                           \
          Bind((CELL *) *aux_stack_ptr, AbsAppl(H));             \
          for (i = 0; i < vars_arity; i++) {                     \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
        }                                                        \
        *H = (CELL) func;                                        \
        H += 1 + func_arity;                                     \
        next_trie_instruction(node)

#ifdef TRIE_COMPACT_PAIRS
#define stack_trie_struct_in_new_pair_instr()	                 \
        if (heap_arity) {                                        \
          int i;                                                 \
          aux_stack_ptr++;		                         \
          Bind_Global((CELL *) *aux_stack_ptr, AbsPair(H));      \
          *aux_stack_ptr-- = (CELL) (H + 1);                     \
          for (i = 0; i < func_arity; i++)                       \
            *aux_stack_ptr-- = (CELL) (H + 2 + func_arity - i);  \
          *aux_stack_ptr = heap_arity - 1 + 1 + func_arity;      \
          YENV = aux_stack_ptr;                                  \
        } else {                                                 \
          int i;                                                 \
          *aux_stack_ptr-- = (CELL) (H + 1);                     \
          for (i = 0; i < func_arity; i++)                       \
            *aux_stack_ptr-- = (CELL) (H + 2 + func_arity - i);  \
          *aux_stack_ptr = 1 + func_arity;                       \
          YENV = aux_stack_ptr;                                  \
          aux_stack_ptr += 1 + func_arity + 2;                   \
          *aux_stack_ptr = subs_arity - 1;                       \
          aux_stack_ptr += subs_arity;                           \
          Bind((CELL *) *aux_stack_ptr, AbsPair(H));             \
          for (i = 0; i < vars_arity; i++) {                     \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
        }                                                        \
        Bind_Global(H, AbsAppl(H + 2));                          \
        H += 2;                                                  \
        *H = (CELL) func;                                        \
        H += 1 + func_arity;                                     \
        next_trie_instruction(node)
#endif /* TRIE_COMPACT_PAIRS */



/* ------------------------ **
**      trie_extension      **
** ------------------------ */

#define stack_trie_extension_instr()                               \
        *aux_stack_ptr-- = 0;  /* float/longint extension mark */  \
        *aux_stack_ptr-- = TrNode_entry(node);                     \
        *aux_stack_ptr = heap_arity + 2;                           \
        YENV = aux_stack_ptr;                                      \
        next_trie_instruction(node)



/* ---------------------------- **
**      trie_float_longint      **
** ---------------------------- */

#define stack_trie_float_longint_instr()                         \
        if (heap_arity) {                                        \
          YENV = ++aux_stack_ptr;                                \
          Bind_Global((CELL *) *aux_stack_ptr, t);               \
          *aux_stack_ptr = heap_arity - 1;                       \
          next_instruction(heap_arity - 1 || subs_arity, node);  \
        } else {                                                 \
          int i;                                                 \
          YENV = aux_stack_ptr;                                  \
          *aux_stack_ptr = 0;                                    \
          aux_stack_ptr += 2;                                    \
          *aux_stack_ptr = subs_arity - 1;                       \
          aux_stack_ptr += subs_arity;                           \
          Bind((CELL *) *aux_stack_ptr, t);                      \
          for (i = 0; i < vars_arity; i++) {                     \
            *aux_stack_ptr = *(aux_stack_ptr + 1);               \
            aux_stack_ptr++;                                     \
          }                                                      \
	  next_instruction(subs_arity - 1, node);                \
        }



/* --------------------------- **
**      Trie instructions      **
** --------------------------- */

  PBOp(trie_do_null, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;

    stack_trie_null_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_null)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_null, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_null_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_null)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_null, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_null_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_null)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_null, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_null_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_null)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_null_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_null_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_null_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_null_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_null_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_null_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_null_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_null_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_null_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_null_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_null_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_null_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_var, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_var_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_var)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_var, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_var_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_var)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_var, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_var_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_var)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_var, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_var_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_var)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_var_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_var_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_var_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_var_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_var_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_var_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_var_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_var_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_var_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_var_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_var_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_var_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_val, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    stack_trie_val_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_val)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_val, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    pop_trie_node();
    stack_trie_val_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_val)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_val, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    store_trie_node(TrNode_next(node));
    stack_trie_val_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_val)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_val, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    restore_trie_node(TrNode_next(node));
    stack_trie_val_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_val)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_val_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    stack_trie_val_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_val_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_val_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    pop_trie_node();
    stack_trie_val_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_val_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_val_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    store_trie_node(TrNode_next(node));
    stack_trie_val_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_val_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_val_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));

    restore_trie_node(TrNode_next(node));
    stack_trie_val_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_val_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
#ifdef GLOBAL_TRIE
    int subs_arity = *(aux_stack_ptr + *aux_stack_ptr + 1);
    YENV = aux_stack_ptr = load_substitution_variable(TrNode_entry(node), aux_stack_ptr);
    next_instruction(subs_arity - 1 , node);
#else
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_atom_instr();
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
#ifdef GLOBAL_TRIE
    int vars_arity = *(aux_stack_ptr);
    int subs_arity = *(aux_stack_ptr + vars_arity + 1);
#else
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
#endif /* GLOBAL_TRIE */
    pop_trie_node();
#ifdef GLOBAL_TRIE
    YENV = aux_stack_ptr = load_substitution_variable(TrNode_entry(node), aux_stack_ptr);
    next_instruction(subs_arity - 1 , node);
#else
    stack_trie_atom_instr();
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
#ifdef GLOBAL_TRIE
    int vars_arity = *(aux_stack_ptr);
    int subs_arity = *(aux_stack_ptr + vars_arity + 1);
#else
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
#endif /* GLOBAL_TRIE */
    store_trie_node(TrNode_next(node));
#ifdef GLOBAL_TRIE
    YENV = aux_stack_ptr = load_substitution_variable(TrNode_entry(node), aux_stack_ptr);
    next_instruction(subs_arity - 1, node); 
#else
    stack_trie_atom_instr();
#endif /* GLOBAL_TRIE */    
  ENDPBOp();


  PBOp(trie_retry_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
#ifdef GLOBAL_TRIE
    int vars_arity = *(aux_stack_ptr);
    int subs_arity = *(aux_stack_ptr + vars_arity + 1);
#else
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
#endif /* GLOBAL_TRIE */
    restore_trie_node(TrNode_next(node));
#ifdef GLOBAL_TRIE
    YENV = aux_stack_ptr = load_substitution_variable(TrNode_entry(node), aux_stack_ptr);
    next_instruction(subs_arity - 1, node); 
#else
    stack_trie_atom_instr();
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_atom_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_atom_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_atom_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_atom_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_atom_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_atom_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_atom_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_atom_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_atom_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_atom_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_atom_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_atom_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_pair, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    stack_trie_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_pair)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_pair, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_pair)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_pair, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_pair)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_pair, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_pair)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_struct, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    stack_trie_struct_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_struct)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_struct, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    pop_trie_node();
    stack_trie_struct_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_struct)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_struct, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    store_trie_node(TrNode_next(node));
    stack_trie_struct_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_struct)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_struct, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    restore_trie_node(TrNode_next(node));
    stack_trie_struct_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_struct)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_struct_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    stack_trie_struct_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_struct_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_struct_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    pop_trie_node();
    stack_trie_struct_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_struct_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_struct_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    store_trie_node(TrNode_next(node));
    stack_trie_struct_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_struct_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_struct_in_new_pair, e)
#if defined(TRIE_COMPACT_PAIRS) && !defined(GLOBAL_TRIE)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);

    restore_trie_node(TrNode_next(node));
    stack_trie_struct_in_new_pair_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_struct_in_new_pair)");
#endif /* TRIE_COMPACT_PAIRS && GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_extension, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;

    stack_trie_extension_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_extension)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_trust_extension, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    pop_trie_node();
    stack_trie_extension_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_extension)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_try_extension, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    store_trie_node(TrNode_next(node));
    stack_trie_extension_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_extension)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_retry_extension, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);

    restore_trie_node(TrNode_next(node));
    stack_trie_extension_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_extension)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  PBOp(trie_do_float, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    volatile Float dbl;
    volatile Term *t_dbl = (Term *)((void *) &dbl);
    Term t;

#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    heap_arity -= 4;
    *t_dbl = *++aux_stack_ptr;
    ++aux_stack_ptr;  /* jump the float/longint extension mark */
    *(t_dbl + 1) = *++aux_stack_ptr;
#else /* SIZEOF_DOUBLE == SIZEOF_INT_P */
    heap_arity -= 2;
    *t_dbl = *++aux_stack_ptr;
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
    ++aux_stack_ptr;  /* jump the float/longint extension mark */
    t = MkFloatTerm(dbl);
    stack_trie_float_longint_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_float)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  BOp(trie_trust_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_float)");
  ENDBOp();


  BOp(trie_try_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_float)");
  ENDBOp();


  BOp(trie_retry_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_float)");
  ENDBOp();


  PBOp(trie_do_long, e)
#ifndef GLOBAL_TRIE
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_stack_ptr = YENV;
    int heap_arity = *aux_stack_ptr;
    int vars_arity = *(aux_stack_ptr + heap_arity + 1);
    int subs_arity = *(aux_stack_ptr + heap_arity + 2);
    Term t;

    heap_arity -= 2;
    t = MkLongIntTerm(*++aux_stack_ptr);
    ++aux_stack_ptr;  /* jump the float/longint extension mark */
    stack_trie_float_longint_instr();
#else
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_do_long)");
#endif /* GLOBAL_TRIE */
  ENDPBOp();


  BOp(trie_trust_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_long)");
  ENDBOp();


  BOp(trie_try_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_long)");
  ENDBOp();


  BOp(trie_retry_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_long)");
  ENDBOp();
