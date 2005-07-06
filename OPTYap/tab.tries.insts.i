/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        tab.tries.insts.i
  version:     $Id: tab.tries.insts.i,v 1.10 2005-07-06 19:34:11 ricroc Exp $   
                                                                     
**********************************************************************/

/* ----------------------------------------------- **
**      Trie instructions: stack organization      **
** ----------------------------------------------- **
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
** ----------------------------------------------- */



/* --------------------------------------------- **
**      Trie instructions: auxiliary macros      **
** --------------------------------------------- */

#define next_trie_instruction(NODE)             \
        PREG = (yamop *) TrNode_child(NODE);    \
        PREFETCH_OP(PREG);                      \
        GONext()

#define next_instruction(CONDITION, NODE)       \
        if (CONDITION) {                        \
          PREG = (yamop *) TrNode_child(NODE);  \
        } else {                                \
          /* procceed */                        \
	  PREG = (yamop *) CPREG;               \
	  YENV = ENV;                           \
        }                                       \
        PREFETCH_OP(PREG);                      \
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
	}

#define restore_trie_node(AP)                         \
        H = HBREG = PROTECT_FROZEN_H(B);              \
        restore_yaam_reg_cpdepth(B);                  \
        CPREG = B->cp_cp;                             \
        ENV = B->cp_env;                              \
        YAPOR_update_alternative(PREG, (yamop *) AP)  \
        B->cp_ap = (yamop *) AP;                      \
        YENV = (CELL *) PROTECT_FROZEN_B(B);          \
        SET_BB(NORM_CP(YENV)) 

#define pop_trie_node()                               \
        YENV = (CELL *) PROTECT_FROZEN_B((B+1));      \
        H = PROTECT_FROZEN_H(B);                      \
        pop_yaam_reg_cpdepth(B);                      \
	CPREG = B->cp_cp;                             \
        TABLING_close_alt(B);	                      \
        ENV = B->cp_env;                              \
	B = B->cp_b;	                              \
        HBREG = PROTECT_FROZEN_H(B);                  \
        SET_BB(PROTECT_FROZEN_B(B))



/* ------------------- **
**      trie_null      **
** ------------------- */

#define no_cp_trie_null_instr()                                         \
        *aux_ptr = 0;                                                   \
        *--aux_ptr = heap_arity + 1;                                    \
        YENV = aux_ptr;                                                 \
        next_trie_instruction(node)

#define cp_trie_null_instr()                                            \
        aux_ptr += heap_arity + subs_arity + vars_arity + 2;            \
        for (i = 0; i < heap_arity + subs_arity + vars_arity + 2; i++)  \
          *--YENV = *aux_ptr--;                                         \
        *--YENV = 0;                                                    \
        *--YENV = heap_arity + 1;                                       \
        next_trie_instruction(node)



/* ------------------ **
**      trie_var      **
** ------------------ */

#define no_cp_trie_var_instr()                                   \
        if (heap_arity) {                                        \
          *aux_ptr = heap_arity - 1;                             \
          var_ptr = *++aux_ptr;                                  \
          RESET_VARIABLE(var_ptr);                               \
          for (i = 0; i < heap_arity - 1; i++) {                 \
            *aux_ptr = *(aux_ptr + 1);                           \
            aux_ptr++;                                           \
          }                                                      \
          *aux_ptr++ = vars_arity + 1;                           \
          *aux_ptr++ = subs_arity;                               \
          for (i = 0; i < subs_arity; i++) {                     \
            *aux_ptr = *(aux_ptr + 1);                           \
            aux_ptr++;                                           \
          }                                                      \
          *aux_ptr = var_ptr;                                    \
          next_instruction(heap_arity - 1 || subs_arity, node);  \
        } else {                                                 \
          *++aux_ptr = vars_arity + 1;                           \
          *++aux_ptr = subs_arity - 1;                           \
          next_instruction(subs_arity - 1, node);                \
        }

#define cp_trie_var_instr()                                      \
        if (heap_arity) {                                        \
          var_ptr = *++aux_ptr;                                  \
          RESET_VARIABLE(var_ptr);                               \
          aux_ptr += heap_arity + subs_arity + vars_arity + 1;   \
          for (i = 0; i < vars_arity; i++)                       \
            *--YENV = *aux_ptr--;                                \
          *--YENV = var_ptr;                                     \
          for (i = 0; i < subs_arity; i++)                       \
            *--YENV = *aux_ptr--;                                \
          *--YENV = subs_arity;                                  \
          *--YENV = vars_arity + 1;                              \
          aux_ptr--;                                             \
          for (i = 1; i < heap_arity; i++)                       \
            *--YENV = *--aux_ptr;                                \
          *--YENV = heap_arity - 1;                              \
          next_instruction(heap_arity - 1 || subs_arity, node);  \
        } else {                                                 \
          aux_ptr += 2 + subs_arity + vars_arity;                \
          for (i = 0; i < subs_arity + vars_arity; i++)          \
            *--YENV = *aux_ptr--;                                \
          *--YENV = subs_arity - 1;                              \
          *--YENV = vars_arity + 1;                              \
          *--YENV = 0;                                           \
          next_instruction(subs_arity - 1, node);                \
        }



/* ------------------ **
**      trie_val      **
** ------------------ */

#define no_cp_trie_val_instr()                                                        \
        if (heap_arity) {                                                             \
          YENV = ++aux_ptr;                                                           \
          subs_ptr = aux_ptr + heap_arity + 1 + subs_arity + vars_arity - var_index;  \
          aux = *aux_ptr;                                                             \
          subs = *subs_ptr;                                                           \
          if (aux > subs) {                                                           \
            Bind_Global((CELL *) aux, subs);                                          \
            /* *((CELL *) aux) = subs;  -->  avoids trail test (always fails?) */     \
          } else {                                                                    \
            RESET_VARIABLE(aux);                                                      \
	    Bind_Local((CELL *) subs, aux);                                           \
            *subs_ptr = aux;                                                          \
          }                                                                           \
          *aux_ptr = heap_arity - 1;                                                  \
          next_instruction(heap_arity - 1 || subs_arity, node);                       \
        } else {                                                                      \
          aux_ptr += 2;                                                               \
          *aux_ptr = subs_arity - 1;                                                  \
          aux_ptr += subs_arity;                                                      \
          subs_ptr = aux_ptr + vars_arity - var_index;                                \
          aux = *aux_ptr;                                                             \
          subs = *subs_ptr;                                                           \
          if (aux > subs) {                                                           \
	    if ((CELL *) aux <= H) {                                                  \
              Bind_Global((CELL *) aux, subs);                                        \
            } else if ((CELL *) subs <= H) {                                          \
              Bind_Local((CELL *) aux, subs);                                         \
            } else {                                                                  \
              Bind_Local((CELL *) subs, aux);                                         \
              *subs_ptr = aux;                                                        \
            }                                                                         \
          } else {                                                                    \
	    if ((CELL *) subs <= H) {                                                 \
              Bind_Global((CELL *) subs, aux);                                        \
              *subs_ptr = aux;                                                        \
            } else if ((CELL *) aux <= H) {                                           \
              Bind_Local((CELL *) subs, aux);                                         \
              *subs_ptr = aux;                                                        \
            } else {                                                                  \
              Bind_Local((CELL *) aux, subs);                                         \
            }                                                                         \
          }                                                                           \
          for (i = 0; i < vars_arity; i++) {                                          \
            *aux_ptr = *(aux_ptr + 1);                                                \
            aux_ptr++;                                                                \
          }                                                                           \
          next_instruction(subs_arity - 1, node);                                     \
        }

#define cp_trie_val_instr()                                                           \
        if (heap_arity) {                                                             \
          aux_ptr++;                                                                  \
          subs_ptr = aux_ptr + heap_arity + 1 + subs_arity + vars_arity - var_index;  \
          aux = *aux_ptr;                                                             \
          subs = *subs_ptr;                                                           \
          if (aux > subs) {                                                           \
            Bind_Global((CELL *) aux, subs);                                          \
            /* *((CELL *) aux) = subs;  -->  avoids trail test (always fails?) */     \
          } else {                                                                    \
            RESET_VARIABLE(aux);                                                      \
	    Bind_Local((CELL *) subs, aux);                                           \
            *subs_ptr = aux;                                                          \
          }                                                                           \
          aux_ptr += heap_arity + subs_arity + vars_arity + 1;                        \
          for (i = 0; i < heap_arity + subs_arity + vars_arity + 1; i++)              \
            *--YENV = *aux_ptr--;                                                     \
          *--YENV = heap_arity - 1;                                                   \
          next_instruction(heap_arity - 1 || subs_arity, node);                       \
        } else {                                                                      \
          aux_ptr += 2 + subs_arity;                                                  \
          subs_ptr = aux_ptr + vars_arity - var_index;                                \
          aux = *aux_ptr;                                                             \
          subs = *subs_ptr;                                                           \
          if (aux > subs) {                                                           \
	    if ((CELL *) aux <= H) {                                                  \
              Bind_Global((CELL *) aux, subs);                                        \
            } else if ((CELL *) subs <= H) {                                          \
              Bind_Local((CELL *) aux, subs);                                         \
            } else {                                                                  \
              Bind_Local((CELL *) subs, aux);                                         \
              *subs_ptr = aux;                                                        \
            }                                                                         \
          } else {                                                                    \
	    if ((CELL *) subs <= H) {                                                 \
              Bind_Global((CELL *) subs, aux);                                        \
              *subs_ptr = aux;                                                        \
            } else if ((CELL *) aux <= H) {                                           \
              Bind_Local((CELL *) subs, aux);                                         \
              *subs_ptr = aux;                                                        \
            } else {                                                                  \
              Bind_Local((CELL *) aux, subs);                                         \
            }                                                                         \
          }                                                                           \
          aux_ptr += vars_arity;                                                      \
          for (i = 0; i < vars_arity; i++)                                            \
            *--YENV = *aux_ptr--;                                                     \
          for (i = 1; i < subs_arity; i++)                                            \
            *--YENV = *--aux_ptr;                                                     \
          *--YENV = subs_arity - 1;                                                   \
          *--YENV = vars_arity;                                                       \
          *--YENV = 0;                                                                \
          next_instruction(subs_arity - 1, node);                                     \
        }



/* ------------------- **
**      trie_atom      **
** ------------------- */

#define no_cp_trie_atom_instr()                                           \
        if (heap_arity) {                                                 \
          YENV = ++aux_ptr;                                               \
          Bind_Global((CELL *) *aux_ptr, TrNode_entry(node));             \
          *aux_ptr = heap_arity - 1;                                      \
          next_instruction(heap_arity - 1 || subs_arity, node);           \
        } else {                                                          \
          aux_ptr += 2;                                                   \
          *aux_ptr = subs_arity - 1;                                      \
          aux_ptr += subs_arity;                                          \
          Bind((CELL *) *aux_ptr, TrNode_entry(node));                    \
          for (i = 0; i < vars_arity; i++) {                              \
            *aux_ptr = *(aux_ptr + 1);                                    \
            aux_ptr++;                                                    \
          }                                                               \
          next_instruction(subs_arity - 1, node);                         \
        }

#define cp_trie_atom_instr()                                              \
        if (heap_arity) {                                                 \
          aux_ptr++;                                                      \
          Bind_Global((CELL *) *aux_ptr, TrNode_entry(node));             \
          aux_ptr += heap_arity + subs_arity + vars_arity + 1;            \
          for (i = 0; i < heap_arity + subs_arity + vars_arity + 1; i++)  \
            *--YENV = *aux_ptr--;                                         \
          *--YENV = heap_arity - 1;                                       \
          next_instruction(heap_arity - 1 || subs_arity, node);           \
        } else {                                                          \
          aux_ptr += 2 + subs_arity;                                      \
          Bind((CELL *) *aux_ptr, TrNode_entry(node));                    \
          aux_ptr += vars_arity;                                          \
          for (i = 0; i < vars_arity; i++)                                \
            *--YENV = *aux_ptr--;                                         \
          for (i = 1; i < subs_arity; i++)                                \
            *--YENV = *--aux_ptr;                                         \
          *--YENV = subs_arity - 1;                                       \
          *--YENV = vars_arity;                                           \
          *--YENV = 0;                                                    \
          next_instruction(subs_arity - 1, node);                         \
        }



/* ------------------- **
**      trie_list      **
** ------------------- */

#define no_cp_trie_list_instr()                                           \
        if (heap_arity) {                                                 \
          aux_ptr++;                                                      \
          Bind_Global((CELL *) *aux_ptr, AbsPair(H));                     \
          H += 2;                                                         \
          *aux_ptr-- = (CELL) (H - 1);                                    \
          *aux_ptr-- = (CELL) (H - 2);                                    \
          *aux_ptr = heap_arity - 1 + 2;                                  \
          YENV = aux_ptr;                                                 \
        } else {                                                          \
          H += 2;                                                         \
          *aux_ptr-- = (CELL) (H - 1);                                    \
          *aux_ptr-- = (CELL) (H - 2);                                    \
          *aux_ptr = 2;                                                   \
          YENV = aux_ptr;                                                 \
          aux_ptr += 2 + 2;                                               \
          *aux_ptr = subs_arity - 1;                                      \
          aux_ptr += subs_arity;                                          \
          Bind((CELL *) *aux_ptr, AbsPair(H - 2));                        \
          for (i = 0; i < vars_arity; i++) {                              \
            *aux_ptr = *(aux_ptr + 1);                                    \
            aux_ptr++;                                                    \
          }                                                               \
        }                                                                 \
        next_trie_instruction(node)

#define cp_trie_list_instr()                                              \
        if (heap_arity) {                                                 \
          aux_ptr++;                                                      \
          Bind_Global((CELL *) *aux_ptr, AbsPair(H));                     \
          aux_ptr += heap_arity + subs_arity + vars_arity + 1;            \
          for (i = 0; i < vars_arity + subs_arity + heap_arity + 1; i++)  \
            *--YENV = *aux_ptr--;                                         \
          H += 2;                                                         \
          *--YENV = (CELL) (H - 1);                                       \
          *--YENV = (CELL) (H - 2);                                       \
          *--YENV = heap_arity + 1;                                       \
        } else {                                                          \
          aux_ptr += 2 + subs_arity;                                      \
          Bind((CELL *) *aux_ptr, AbsPair(H));                            \
          aux_ptr += vars_arity;                                          \
          for (i = 0; i < vars_arity; i++)                                \
            *--YENV = *aux_ptr--;                                         \
          for (i = 1; i < subs_arity; i++)                                \
            *--YENV = *--aux_ptr;                                         \
          *--YENV = subs_arity - 1;                                       \
          *--YENV = vars_arity;                                           \
          H += 2;                                                         \
          *--YENV = (CELL) (H - 1);                                       \
          *--YENV = (CELL) (H - 2);                                       \
          *--YENV = 2;                                                    \
        }                                                                 \
        next_trie_instruction(node)



/* --------------------- **
**      trie_struct      **
** --------------------- */

#define no_cp_trie_struct_instr()                                         \
        if (heap_arity) {                                                 \
          aux_ptr++;                                                      \
          Bind_Global((CELL *) *aux_ptr, AbsAppl(H));                     \
          *H++ = (CELL) func;                                             \
          H += func_arity;                                                \
          for (i = 1; i <= func_arity; i++)                               \
            *aux_ptr-- = (CELL) (H - i);                                  \
          *aux_ptr = heap_arity - 1 + func_arity;                         \
          YENV = aux_ptr;                                                 \
        } else {                                                          \
          *H++ = (CELL) func;                                             \
          H += func_arity;                                                \
          for (i = 1; i <= func_arity; i++)                               \
            *aux_ptr-- = (CELL) (H - i);                                  \
          *aux_ptr = func_arity;                                          \
          YENV = aux_ptr;                                                 \
          aux_ptr += func_arity + 2;                                      \
          *aux_ptr = subs_arity - 1;                                      \
          aux_ptr += subs_arity;                                          \
          Bind((CELL *) *aux_ptr, AbsAppl(H - func_arity - 1));           \
          for (i = 0; i < vars_arity; i++) {                              \
            *aux_ptr = *(aux_ptr + 1);                                    \
            aux_ptr++;                                                    \
          }                                                               \
        }                                                                 \
        next_trie_instruction(node)

#define cp_trie_struct_instr()                                            \
        if (heap_arity) {                                                 \
          aux_ptr++;                                                      \
          Bind_Global((CELL *) *aux_ptr, AbsAppl(H));                     \
          aux_ptr += heap_arity + subs_arity + vars_arity + 1;            \
          for (i = 0; i < vars_arity + subs_arity + heap_arity + 1; i++)  \
            *--YENV = *aux_ptr--;                                         \
          *H++ = (CELL) func;                                             \
          H += func_arity;                                                \
          for (i = 1; i <= func_arity; i++)                               \
            *--YENV = (CELL) (H - i);                                     \
          *--YENV = heap_arity + func_arity - 1;                          \
        } else {                                                          \
          aux_ptr += 2 + subs_arity;                                      \
          Bind((CELL *) *aux_ptr, AbsAppl(H));                            \
          aux_ptr += vars_arity;                                          \
          for (i = 0; i < vars_arity; i++)                                \
            *--YENV = *aux_ptr--;                                         \
          for (i = 1; i < subs_arity; i++)                                \
            *--YENV = *--aux_ptr;                                         \
          *--YENV = subs_arity - 1;                                       \
          *--YENV = vars_arity;                                           \
          *H++ = (CELL) func;                                             \
          H += func_arity;                                                \
          for (i = 1; i <= func_arity; i++)                               \
            *--YENV = (CELL) (H - i);                                     \
          *--YENV = func_arity;                                           \
        }                                                                 \
        next_trie_instruction(node)



/* ------------------------ **
**      trie_extension      **
** ------------------------ */

#define no_cp_trie_extension_instr()                                    \
        *aux_ptr = TrNode_entry(node);                                  \
        *--aux_ptr = heap_arity + 1;                                    \
        YENV = aux_ptr;                                                 \
        next_trie_instruction(node)

#define cp_trie_extension_instr()                                       \
        aux_ptr += heap_arity + subs_arity + vars_arity + 2;            \
        for (i = 0; i < heap_arity + subs_arity + vars_arity + 2; i++)  \
          *--YENV = *aux_ptr--;                                         \
        *--YENV = TrNode_entry(node);                                   \
        *--YENV = heap_arity + 1;                                       \
        next_trie_instruction(node)



/* ---------------------------- **
**      trie_float_longint      **
** ---------------------------- */

#define no_cp_trie_float_longint_instr()                         \
        if (heap_arity) {                                        \
          aux_ptr++;                                             \
          YENV = ++aux_ptr;                                      \
          Bind_Global((CELL *) *aux_ptr, t);                     \
          *aux_ptr = heap_arity - 1;                             \
          next_instruction(heap_arity - 1 || subs_arity, node);  \
        } else {                                                 \
          YENV = ++aux_ptr;                                      \
          *aux_ptr = 0;                                          \
          aux_ptr += 2;                                          \
          *aux_ptr = subs_arity - 1;                             \
          aux_ptr += subs_arity;                                 \
          Bind((CELL *) *aux_ptr, t);                            \
          for (i = 0; i < vars_arity; i++) {                     \
            *aux_ptr = *(aux_ptr + 1);                           \
            aux_ptr++;                                           \
          }                                                      \
	  next_instruction(subs_arity - 1, node);                \
        }                                                        \



/* --------------------------- **
**      Trie instructions      **
** --------------------------- */

  PBOp(trie_do_null, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;

    no_cp_trie_null_instr();
  ENDPBOp();


  PBOp(trie_try_null, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_null_instr();
  ENDPBOp();


  PBOp(trie_retry_null, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_null_instr();
  ENDPBOp();


  PBOp(trie_trust_null, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;
 
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_null_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_null_instr();
      } else {
        no_cp_trie_null_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_var, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    register CELL var_ptr;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    no_cp_trie_var_instr();
  ENDPBOp();


  PBOp(trie_try_var, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    register CELL var_ptr;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_var_instr();
  ENDPBOp();


  PBOp(trie_retry_var, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    register CELL var_ptr;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_var_instr();
  ENDPBOp();


  PBOp(trie_trust_var, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    register CELL var_ptr;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_var_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_var_instr();
      } else {
        no_cp_trie_var_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_val, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV, *subs_ptr;
    register CELL aux, subs;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));
    int i;

    no_cp_trie_val_instr();
  ENDPBOp();


  PBOp(trie_try_val, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV, *subs_ptr;
    register CELL aux, subs;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_val_instr();
  ENDPBOp();


  PBOp(trie_retry_val, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1), *subs_ptr;
    register CELL aux, subs;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_val_instr();
  ENDPBOp();


  PBOp(trie_trust_val, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1), *subs_ptr;
    register CELL aux, subs;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int var_index = VarIndexOfTableTerm(TrNode_entry(node));
    int i;

#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_val_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_val_instr();
      } else {
        no_cp_trie_val_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    no_cp_trie_atom_instr();
  ENDPBOp();


  PBOp(trie_try_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_atom_instr();
  ENDPBOp();


  PBOp(trie_retry_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_atom_instr();
  ENDPBOp();


  PBOp(trie_trust_atom, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;
 
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_atom_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_atom_instr();
      } else {
        no_cp_trie_atom_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_list, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    no_cp_trie_list_instr();
  ENDPBOp();


  PBOp(trie_try_list, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_list_instr();
  ENDPBOp();


  PBOp(trie_retry_list, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_list_instr();
  ENDPBOp();


  PBOp(trie_trust_list, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_list_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_list_instr();
      } else {
        no_cp_trie_list_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_struct, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);
    int i;

    no_cp_trie_struct_instr();
  ENDPBOp();


  PBOp(trie_try_struct, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_struct_instr();
  ENDPBOp();


  PBOp(trie_retry_struct, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_struct_instr();
  ENDPBOp();


  PBOp(trie_trust_struct, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    Functor func = (Functor) RepAppl(TrNode_entry(node));
    int func_arity = ArityOfFunctor(func);
    int i;

#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_struct_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_struct_instr();
      } else {
        no_cp_trie_struct_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_extension, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;

    no_cp_trie_extension_instr();
  ENDPBOp();


  PBOp(trie_try_extension, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    store_trie_node(TrNode_next(node));
    cp_trie_extension_instr();
  ENDPBOp();


  PBOp(trie_retry_extension, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;

    restore_trie_node(TrNode_next(node));
    cp_trie_extension_instr();
  ENDPBOp();


  PBOp(trie_trust_extension, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = (CELL *) (B + 1);
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;
 
#ifdef YAPOR
    if (SCH_top_shared_cp(B)) {
      restore_trie_node(NULL);
      cp_trie_extension_instr();
    } else
#endif /* YAPOR */
    {
      pop_trie_node();
      if ((choiceptr) YENV == B_FZ) {
        cp_trie_extension_instr();
      } else {
        no_cp_trie_extension_instr();
      }
    }
  ENDPBOp();


  PBOp(trie_do_float, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;
    Term t;
    volatile Float dbl;
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
    volatile Term *t_dbl = (Term *)((void *) &dbl);
    *t_dbl = *++aux_ptr;
    *(t_dbl + 1) = *++aux_ptr;
    heap_arity -= 3;
#else /* SIZEOF_DOUBLE == SIZEOF_LONG_INT */
    dbl = (Float) *++aux_ptr;
    heap_arity -= 2;
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
    t = MkFloatTerm(dbl);
    no_cp_trie_float_longint_instr();
  ENDPBOp();


  PBOp(trie_try_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_float)");
  ENDPBOp();


  PBOp(trie_retry_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_float)");
  ENDPBOp();


  PBOp(trie_trust_float, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_float)");
  ENDPBOp();


  PBOp(trie_do_long, e)
    register ans_node_ptr node = (ans_node_ptr) PREG;
    register CELL *aux_ptr = YENV;
    int heap_arity = *aux_ptr;
    int vars_arity = *(aux_ptr + heap_arity + 1);
    int subs_arity = *(aux_ptr + heap_arity + 2);
    int i;
    Term t = MkLongIntTerm(*++aux_ptr);
    heap_arity -= 2;
    no_cp_trie_float_longint_instr();
  ENDPBOp();


  PBOp(trie_try_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_try_long)");
  ENDPBOp();


  PBOp(trie_retry_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_retry_long)");
  ENDPBOp();


  PBOp(trie_trust_long, e)
    Yap_Error(INTERNAL_ERROR, TermNil, "invalid instruction (trie_trust_long)");
  ENDPBOp();
