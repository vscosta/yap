/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#ifdef TABLING
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include "Yatom.h"
#include "Heap.h"
#include "tab.macros.h"



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

static int traverse_subgoal_trie(FILE *stream, sg_node_ptr sg_node, char *str, int str_index, int *arity, int depth);
static int traverse_answer_trie(FILE *stream, ans_node_ptr ans_node, char *str, int str_index, int *arity, int var_index, int depth);
static void free_answer_trie_branch(ans_node_ptr node);
#ifdef YAPOR
#ifdef TABLING_INNER_CUTS
static int update_answer_trie_branch(ans_node_ptr previous_node, ans_node_ptr node);
#else
static int update_answer_trie_branch(ans_node_ptr node);
#endif /* TABLING_INNER_CUTS */
#else
static void update_answer_trie_branch(ans_node_ptr node);
#endif /* YAPOR */



/* ----------------------- **
**      Local inlines      **
** ----------------------- */

STD_PROTO(static inline sg_node_ptr subgoal_trie_node_check_insert, (tab_ent_ptr, sg_node_ptr, Term));
STD_PROTO(static inline ans_node_ptr answer_trie_node_check_insert, (sg_fr_ptr, ans_node_ptr, Term, int));



#ifdef TABLE_LOCK_AT_WRITE_LEVEL

static inline
sg_node_ptr subgoal_trie_node_check_insert(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
  sg_node_ptr chain_node, new_node;
  sg_hash_ptr hash;


  chain_node = TrNode_child(parent_node);


  if (chain_node == NULL) {
#ifdef ALLOC_BEFORE_CHECK
    new_subgoal_trie_node(new_node, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (TrNode_child(parent_node)) {
      chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_TABLE(parent_node);
        hash = (sg_hash_ptr) chain_node;
        goto subgoal_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = TrNode_child(parent_node);
#else
      new_subgoal_trie_node(new_node, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      new_subgoal_trie_node(new_node, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = new_node;
    UNLOCK_TABLE(parent_node);
    return new_node;
  } 


  if (! IS_SUBGOAL_HASH(chain_node)) {
    sg_node_ptr first_node;
    int count_nodes;

    first_node = chain_node;
    count_nodes = 0;
    do {
      if (TrNode_entry(chain_node) == t) {
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
    new_subgoal_trie_node(new_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_TABLE(parent_node);
        hash = (sg_hash_ptr) chain_node;
        goto subgoal_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = TrNode_child(parent_node);
#else
      new_subgoal_trie_node(new_node, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      new_subgoal_trie_node(new_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    if (count_nodes > MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_node_ptr next_node, *bucket;
      new_subgoal_hash(hash, count_nodes, tab_ent);
      chain_node = new_node;
      do {
        bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (sg_node_ptr) hash;
    } else {
      TrNode_child(parent_node) = new_node;
    }
    UNLOCK_TABLE(parent_node);
    return new_node;
  }


  hash = (sg_hash_ptr) chain_node; 
subgoal_hash:
  { /* trie nodes with hashing */
    sg_node_ptr *bucket, first_node;
    int seed, count_nodes;

    seed = Hash_seed(hash);
    bucket = Hash_bucket(hash, HASH_TERM(t, seed));
    first_node = chain_node = *bucket;
    count_nodes = 0;
    while (chain_node) {
      if (TrNode_entry(chain_node) == t) {
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    }
#ifdef ALLOC_BEFORE_CHECK
    new_subgoal_trie_node(new_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (seed != Hash_seed(hash)) {
      /* the hash has been expanded */ 
#ifdef ALLOC_BEFORE_CHECK
      FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
      UNLOCK_TABLE(parent_node);
      goto subgoal_hash;
    }
    if (first_node != *bucket) {
      chain_node = *bucket;
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = *bucket;
#else
      new_subgoal_trie_node(new_node, t, NULL, parent_node, *bucket);
    } else {
      new_subgoal_trie_node(new_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    *bucket = new_node;
    Hash_num_nodes(hash)++;
    if (count_nodes > MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      sg_node_ptr next_node, *first_old_bucket, *old_bucket;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      seed = Hash_num_buckets(hash) * 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), seed);
      seed--;
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), seed));
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != first_old_bucket);
      Hash_num_buckets(hash) = seed + 1;
      FREE_HASH_BUCKETS(first_old_bucket);
    }
    UNLOCK_TABLE(parent_node);
    return new_node;
  }
}


static inline 
ans_node_ptr answer_trie_node_check_insert(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
  ans_node_ptr chain_node, new_node;
  ans_hash_ptr hash;

#ifdef TABLING_ERRORS
  if (IS_ANSWER_LEAF_NODE(parent_node))
    TABLING_ERROR_MESSAGE("IS_ANSWER_LEAF_NODE(parent_node) (answer_trie_node_check_insert)");
#endif /* TABLING_ERRORS */


  chain_node = TrNode_child(parent_node);


  if (chain_node == NULL) {
#ifdef ALLOC_BEFORE_CHECK
    new_answer_trie_node(new_node, instr, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (TrNode_child(parent_node)) {
      chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_TABLE(parent_node);
        hash = (ans_hash_ptr) chain_node;
        goto answer_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = TrNode_child(parent_node);
#else
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = new_node;
    UNLOCK_TABLE(parent_node);
    return new_node;
  } 


  if (! IS_ANSWER_HASH(chain_node)) {
    ans_node_ptr first_node;
    int count_nodes;

    first_node = chain_node;
    count_nodes = 0;
    do {
      if (TrNode_entry(chain_node) == t) {
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
    new_answer_trie_node(new_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_TABLE(parent_node);
        hash = (ans_hash_ptr) chain_node; 
        goto answer_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = TrNode_child(parent_node);
#else
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    if (count_nodes > MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_node_ptr next_node, *bucket;
      new_answer_hash(hash, count_nodes, sg_fr);
      chain_node = new_node;
      do {
        bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (ans_node_ptr) hash;
    } else {
      TrNode_child(parent_node) = new_node;
    }
    UNLOCK_TABLE(parent_node);
    return new_node;
  }


  hash = (ans_hash_ptr) chain_node; 
answer_hash:
  { /* trie nodes with hashing */
    ans_node_ptr *bucket, first_node;
    int seed, count_nodes;

    seed = Hash_seed(hash);
    bucket = Hash_bucket(hash, HASH_TERM(t, seed));
    first_node = chain_node = *bucket;
    count_nodes = 0;
    while (chain_node) {
      if (TrNode_entry(chain_node) == t) {
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    }
#ifdef ALLOC_BEFORE_CHECK
    new_answer_trie_node(new_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_TABLE(parent_node);
    if (seed != Hash_seed(hash)) {
      /* the hash has been expanded */ 
#ifdef ALLOC_BEFORE_CHECK
      FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
      UNLOCK_TABLE(parent_node);
      goto answer_hash;
    }
    if (first_node != *bucket) {
      chain_node = *bucket;
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(new_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_TABLE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(new_node) = *bucket;
#else
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, *bucket);
    } else {
      new_answer_trie_node(new_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    *bucket = new_node;
    Hash_num_nodes(hash)++;
    if (count_nodes > MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      ans_node_ptr next_node, *first_old_bucket, *old_bucket;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      seed = Hash_num_buckets(hash) * 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), seed);
      seed--;
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), seed));
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != first_old_bucket);
      Hash_num_buckets(hash) = seed + 1;
      FREE_HASH_BUCKETS(first_old_bucket);
    }
    UNLOCK_TABLE(parent_node);
    return new_node;
  }
}
#else  /* TABLE_LOCK_AT_ENTRY_LEVEL || TABLE_LOCK_AT_NODE_LEVEL || ! YAPOR */


#ifdef TABLE_LOCK_AT_NODE_LEVEL
#define LOCK_NODE(NODE)  TRIE_LOCK(TrNode_lock(NODE))
#define UNLOCK_NODE(NODE)   UNLOCK(TrNode_lock(NODE))
#else
#define LOCK_NODE(NODE)
#define UNLOCK_NODE(NODE)
#endif /* TABLE_LOCK_AT_NODE_LEVEL */


static inline
sg_node_ptr subgoal_trie_node_check_insert(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
  sg_node_ptr child_node;

  LOCK_NODE(parent_node);
  child_node = TrNode_child(parent_node);

  if (child_node == NULL) {
    new_subgoal_trie_node(child_node, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  if (! IS_SUBGOAL_HASH(child_node)) {
    int count_nodes;
    count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    new_subgoal_trie_node(child_node, t, NULL, parent_node, TrNode_child(parent_node));
    if (count_nodes > MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_hash_ptr hash;
      sg_node_ptr chain_node, next_node, *bucket;
      new_subgoal_hash(hash, count_nodes, tab_ent);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (sg_node_ptr) hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    sg_hash_ptr hash;
    sg_node_ptr *bucket;
    int count_nodes;
    hash = (sg_hash_ptr) child_node; 
    bucket = Hash_bucket(hash, HASH_TERM(t, Hash_seed(hash)));
    child_node = *bucket;
    count_nodes = 0;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    Hash_num_nodes(hash)++;
    new_subgoal_trie_node(child_node, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    if (count_nodes > MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      sg_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      int seed;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash)); 
      seed = Hash_num_buckets(hash) - 1;
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), seed));
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != first_old_bucket);
      FREE_HASH_BUCKETS(first_old_bucket);
    }
    UNLOCK_NODE(parent_node);
    return child_node;
  }
}


static inline 
ans_node_ptr answer_trie_node_check_insert(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
  ans_node_ptr child_node;

#ifdef TABLING_ERRORS
  if (IS_ANSWER_LEAF_NODE(parent_node))
    TABLING_ERROR_MESSAGE("IS_ANSWER_LEAF_NODE(parent_node) (answer_trie_node_check_insert)");
#endif /* TABLING_ERRORS */

  LOCK_NODE(parent_node);
  child_node = TrNode_child(parent_node);

  if (child_node == NULL) {
    new_answer_trie_node(child_node, instr, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  if (! IS_ANSWER_HASH(child_node)) {
    int count_nodes;
    count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    new_answer_trie_node(child_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    if (count_nodes > MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_hash_ptr hash;
      ans_node_ptr chain_node, next_node, *bucket;
      new_answer_hash(hash, count_nodes, sg_fr);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (ans_node_ptr) hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    ans_hash_ptr hash;
    ans_node_ptr *bucket;
    int count_nodes;
    hash = (ans_hash_ptr) child_node; 
    bucket = Hash_bucket(hash, HASH_TERM(t, Hash_seed(hash)));
    child_node = *bucket;
    count_nodes = 0;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    Hash_num_nodes(hash)++;
    new_answer_trie_node(child_node, instr, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    if (count_nodes > MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      ans_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      int seed;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash)); 
      seed = Hash_num_buckets(hash) - 1;
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_TERM(TrNode_entry(chain_node), seed));
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != first_old_bucket);
      FREE_HASH_BUCKETS(first_old_bucket);
    }
    UNLOCK_NODE(parent_node);
    return child_node;
  }
}
#endif /* TABLE_LOCK_LEVEL */



/* -------------------------- **
**      Global functions      **
** -------------------------- */

sg_node_ptr subgoal_search(tab_ent_ptr tab_ent, OPREG arity, CELL **Yaddr) {
  int i, j, count_vars;
  CELL *stack_vars, *stack_terms_top, *stack_terms_base, *stack_terms;
  sg_node_ptr current_sg_node;
  
  count_vars = 0;
  stack_vars = *Yaddr;
#ifdef YAPOR
  stack_terms_top = (CELL *)TrailTop;
  stack_terms_base = stack_terms = AuxSp;
#else
  stack_terms_top = (CELL *)TR;
  stack_terms_base = stack_terms = (CELL *)TrailTop;
#endif
  current_sg_node = TabEnt_subgoal_trie(tab_ent);

  for (i = 1; i <= arity; i++) {
    STACK_PUSH(XREGS[i], stack_terms, stack_terms_top, stack_terms_base);
    do {
      Term t = Deref(STACK_POP(stack_terms));
      int tag = t & TabTagBits;
      switch (tag) {
        case TabVarTagBits:
          if (IsTableVarTerm(t)) {
	    t = MakeTableVarTerm(VarIndexOfTerm(t));
            current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
  	  } else {
            if (count_vars == MAX_TABLE_VARS)
              Error(SYSTEM_ERROR,TermNil,"MAX_TABLE_VARS exceeded in function subgoal_search (%d)", count_vars);
            FREE_STACK_PUSH(t, stack_vars);
	    *((CELL *)t) = GLOBAL_table_var_enumerator(count_vars);
            t = MakeTableVarTerm(count_vars);
            count_vars++;
            current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
	  }
          break;
        case TabAtomTagBits:
        case TabNumberTagBits:
          current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
          break;
        case TabPairTagBits:
          current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, TabPairTagBits);
          STACK_PUSH(*(RepPair(t) + 1), stack_terms, stack_terms_top, stack_terms_base);
          STACK_PUSH(*(RepPair(t)), stack_terms, stack_terms_top, stack_terms_base);
          break;
        case TabApplTagBits:
          current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node,
                                                           TAGGEDA(TabApplTagBits, FunctorOfTerm(t)));
          for (j = ArityOfFunctor(FunctorOfTerm(t)); j >= 1; j--)
            STACK_PUSH(*(RepAppl(t) + j), stack_terms, stack_terms_top, stack_terms_base);
          break;
        default:
          abort_optyap("unknown type tag in function subgoal_search");
      }
    } while (STACK_NOT_EMPTY(stack_terms, stack_terms_base));
  }

  FREE_STACK_PUSH(count_vars, stack_vars);
  *Yaddr = stack_vars++;
  /* reset variables */
  while (count_vars--) {
    Term t = STACK_POP(stack_vars);
    RESET_VARIABLE(t);
  }

  return current_sg_node;
}


ans_node_ptr answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr) {
  int i, j, count_vars, subs_arity;
  CELL *stack_vars, *stack_terms_base, *stack_terms_top, *stack_terms;
  ans_node_ptr current_ans_node;

  count_vars = 0;
  subs_arity = *subs_ptr;
  stack_vars = AuxSp;
#ifdef YAPOR
  stack_terms_top = (CELL *)TrailTop;
  stack_terms_base = stack_terms = stack_vars - MAX_TABLE_VARS;
#else
  stack_terms_top = (CELL *)TR;
  stack_terms_base = stack_terms = (CELL *)TrailTop;
#endif
  current_ans_node = SgFr_answer_trie(sg_fr);

  for (i = subs_arity; i >= 1; i--) {
    STACK_PUSH(*(subs_ptr + i), stack_terms, stack_terms_top, stack_terms_base);
#ifdef TABLING_ERRORS
    if ((*stack_terms & TabTagBits) != TabVarTagBits)
      TABLING_ERROR_MESSAGE("*stack_terms & TabTagBits != TabVarTagBits (answer_search)");
#endif /* TABLING_ERRORS */
    do {
      Term t = Deref(STACK_POP(stack_terms));
      int tag = t & TabTagBits;
      switch (tag) {
        case TabVarTagBits:
          if (IsTableVarTerm(t)) {
	    t = MakeTableVarTerm(VarIndexOfTerm(t));
            current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_val);
  	  } else {
            if (count_vars == MAX_TABLE_VARS)
              Error(SYSTEM_ERROR,TermNil,"MAX_TABLE_VARS exceeded in function answer_search (%d)", count_vars);
            FREE_STACK_PUSH(t, stack_vars);
	    *((CELL *)t) = GLOBAL_table_var_enumerator(count_vars);
            t = MakeTableVarTerm(count_vars);
            count_vars++;
            current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_var);
	  }
          break;
        case TabAtomTagBits:
        case TabNumberTagBits:
          current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_atom);
          break;
        case TabPairTagBits:
          current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, TabPairTagBits, _trie_retry_list);
          STACK_PUSH(*(RepPair(t) + 1), stack_terms, stack_terms_top, stack_terms_base);
          STACK_PUSH(*(RepPair(t)), stack_terms, stack_terms_top, stack_terms_base);
          break;
        case TabApplTagBits:
          current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, TAGGEDA(TabApplTagBits,
                                                           FunctorOfTerm(t)), _trie_retry_struct);
          for (j = ArityOfFunctor(FunctorOfTerm(t)); j >= 1; j--)
            STACK_PUSH(*(RepAppl(t) + j), stack_terms, stack_terms_top, stack_terms_base);
          break;
        default:
          abort_optyap("unknown type tag in function answer_search");
      }
    } while (STACK_NOT_EMPTY(stack_terms, stack_terms_base));
  }

  /* reset variables */
  while (count_vars--) {
    Term t = STACK_POP(stack_vars);
    RESET_VARIABLE(t);
  }

  return current_ans_node;
}


void load_answer_trie(ans_node_ptr ans_node, CELL *subs_ptr) {
  int subs_arity;
  subs_arity = *subs_ptr;
  if (subs_arity) {
    int i, n_vars = 0;
    CELL *stack_vars, *stack_terms, *stack_refs, *stack_refs_base, *stack_top;
    ans_node_ptr aux_parent_node;
#ifdef YAPOR
    stack_top = (CELL *)TrailTop;
    stack_vars = stack_terms = AuxSp - MAX_TABLE_VARS;
#else
    stack_top = (CELL *)TR;
    stack_vars = stack_terms = ((CELL *)TrailTop)-MAX_TABLE_VARS;
#endif

    /* load the new answer from the answer trie to the stack_terms */
    aux_parent_node = UNTAG_ANSWER_LEAF_NODE(TrNode_parent(ans_node));
    do {
      STACK_PUSH(TrNode_entry(ans_node), stack_terms, stack_top, stack_vars);
      ans_node = aux_parent_node;
      aux_parent_node = TrNode_parent(aux_parent_node);
    } while (aux_parent_node);
    stack_refs_base = stack_refs = stack_terms;

#ifdef TABLING_ERRORS
    if (H < H_FZ)
      TABLING_ERROR_MESSAGE("H < H_FZ (load_answer_trie)");
#endif /* TABLING_ERRORS */
    for (i = subs_arity; i >= 1; i--) {
      /* bind the substitution variables with the answer loaded in stack_terms */
      CELL *subs_var = (CELL *) *(subs_ptr + i);
      Term t = STACK_POP(stack_terms);
      int tag = t & TabTagBits;
#ifdef TABLING_ERRORS
      if ((CELL)subs_var != *subs_var)
        TABLING_ERROR_MESSAGE("subs_var != *subs_var (load_answer_trie)");
#endif /* TABLING_ERRORS */
      switch (tag) {
        case TabVarTagBits:
        { int var_index = VarIndexOfTableTerm(t);
          if (var_index == n_vars) {
            stack_vars[n_vars++] = (CELL) subs_var;
	  } else {
            Bind(subs_var, stack_vars[var_index]);
	  }
        } break;
        case TabNumberTagBits:
        case TabAtomTagBits:
	  Bind(subs_var, t);
	  break;
        case TabPairTagBits:
          /* build a pair term as in function MkPairTerm */
          Bind(subs_var, AbsPair(H));
#ifdef TABLING_ERRORS
          if ((*subs_var & TabTagBits) != TabPairTagBits)
            TABLING_ERROR_MESSAGE("*subs_var & TabTagBits != TabPairTagBits (load_answer_trie)");
#endif /* TABLING_ERRORS */
          H += 2;
          STACK_PUSH(H - 1, stack_refs, stack_top, stack_refs_base);
          STACK_PUSH(H - 2, stack_refs, stack_top, stack_refs_base);
          break;
        case TabApplTagBits:
        { /* build a pair term as in function MkApplTerm */
          Functor f = (Functor)NonTagPart(t);
          int j, f_arity = ArityOfFunctor(f);
          Bind(subs_var, AbsAppl(H));
#ifdef TABLING_ERRORS
          if ((*subs_var & TabTagBits) != TabApplTagBits)
            TABLING_ERROR_MESSAGE("*subs_var & TabTagBits != TabApplTagBits (load_answer_trie)");
#endif /* TABLING_ERRORS */
          *H++ = (CELL) f;
          H += f_arity;
          for (j = 1; j <= f_arity; j++)
            STACK_PUSH(H - j, stack_refs, stack_top, stack_refs_base);
        } break;
        default:
          abort_optyap("unknown type tag in macro load_answer_trie");
      }
      while (STACK_NOT_EMPTY(stack_refs, stack_refs_base)) {
        CELL *ref = (CELL *) STACK_POP(stack_refs);
        Term t = STACK_POP(stack_terms);
        int tag = t & TabTagBits;
        switch (tag) {
          case TabVarTagBits:
	  { int var_index = VarIndexOfTableTerm(t);
            if (var_index == n_vars) {
              stack_vars[n_vars++] = (CELL) ref;
  	    }
            *ref = stack_vars[var_index];
          } break;
          case TabNumberTagBits:
          case TabAtomTagBits:
            *ref = t;
            break;
          case TabPairTagBits:
            /* build a pair term as in function MkPairTerm */
            *ref = AbsPair(H);
#ifdef TABLING_ERRORS
            if ((*ref & TabTagBits) != TabPairTagBits)
              TABLING_ERROR_MESSAGE("*ref & TabTagBits != TabPairTagBits (load_answer_trie)");
#endif /* TABLING_ERRORS */
            H += 2;
            STACK_PUSH(H - 1, stack_refs, stack_top, stack_refs_base);
            STACK_PUSH(H - 2, stack_refs, stack_top, stack_refs_base);
            break;
          case TabApplTagBits:
          { /* build a pair term as in function MkApplTerm */
            Functor f = (Functor)NonTagPart(t);
            int j, f_arity = ArityOfFunctor(f);
            *ref = AbsAppl(H);
#ifdef TABLING_ERRORS
            if ((*ref & TabTagBits) != TabApplTagBits)
              TABLING_ERROR_MESSAGE("*ref & TabTagBits != TabApplTagBits (load_answer_trie)");
#endif /* TABLING_ERRORS */
            *H++ = (CELL) f;
            H += f_arity;
            for (j = 1; j <= f_arity; j++)
              STACK_PUSH(H - j, stack_refs, stack_top, stack_refs_base);
          } break;
          default:
            abort_optyap("unknown type tag in macro load_answer_trie");
        }
      }
    }
#ifdef TABLING_ERRORS
    if (stack_terms != AuxSp - MAX_TABLE_VARS)
      TABLING_ERROR_MESSAGE("stack_terms != AuxSp - MAX_TABLE_VARS (load_answer_trie)");
#endif /* TABLING_ERRORS */
  }
  return;
}


void private_completion(sg_fr_ptr sg_fr) {
  /* complete subgoals */
  mark_as_completed(LOCAL_top_sg_fr);
  while (LOCAL_top_sg_fr != sg_fr) {
    LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);
    mark_as_completed(LOCAL_top_sg_fr);
  }
  LOCAL_top_sg_fr = SgFr_next(LOCAL_top_sg_fr);

  /* release dependency frames */
#ifdef TABLING_BATCHED_SCHEDULING
  while (YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), B)) {
#else /* TABLING_LOCAL_SCHEDULING */
  while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), B)) {
#endif /* TABLING_SCHEDULING */
    dep_fr_ptr dep_fr = DepFr_next(LOCAL_top_dep_fr);
    FREE_DEPENDENCY_FRAME(LOCAL_top_dep_fr);
    LOCAL_top_dep_fr = dep_fr;
  }

  /* adjust freeze registers */
  adjust_freeze_registers();

  return;
}


void free_subgoal_trie_branch(sg_node_ptr node, int missing_nodes) {
  int tag;
  Term t;

  if (TrNode_next(node))
    free_subgoal_trie_branch(TrNode_next(node), missing_nodes);

  missing_nodes -= 1;
  t = TrNode_entry(node);
  tag = t & TabTagBits;
  switch (tag) {
    case TabVarTagBits:
    case TabNumberTagBits:
    case TabAtomTagBits:
      break;
    case TabPairTagBits:
      missing_nodes += 2;
      break;
    case TabApplTagBits:
      missing_nodes += ArityOfFunctor((Functor)NonTagPart(t));
      break;
    default:
      abort_optyap("unknown type tag in function chain_subgoal_frames");
  }
  if (missing_nodes) {
    free_subgoal_trie_branch(TrNode_child(node), missing_nodes);
  } else {
    sg_fr_ptr sg_fr;
    sg_fr = (sg_fr_ptr) TrNode_sg_fr(node);
    if (sg_fr) {
      free_answer_hash_chain(SgFr_hash_chain(sg_fr));
      free_answer_trie(sg_fr);
      FREE_SUBGOAL_FRAME(sg_fr);
    }
  }

  FREE_SUBGOAL_TRIE_NODE(node);
  return;
}


void free_answer_trie(sg_fr_ptr sg_fr) {
  ans_node_ptr node;
  node = SgFr_answer_trie(sg_fr);
  if (TrNode_child(node))
    free_answer_trie_branch(TrNode_child(node));
  FREE_ANSWER_TRIE_NODE(node);
  return;
}


void update_answer_trie(sg_fr_ptr sg_fr) {
  ans_node_ptr node;
  node = TrNode_child(SgFr_answer_trie(sg_fr));
  if (node) {
    TrNode_instr(node) -= 1;
#ifdef TABLING_INNER_CUTS
    update_answer_trie_branch(NULL, node);
#else
    update_answer_trie_branch(node);
#endif /* TABLING_INNER_CUTS */
  }
  SgFr_state(sg_fr) = executable;
  return;
}

static struct trie_statistics{
  int show;
  long subgoals;
  long subgoals_abolished;
  long subgoal_trie_nodes;
  long subgoal_linear_nodes;
  int  subgoal_trie_max_depth;
  int  subgoal_trie_min_depth;
  long answers;
  long answers_pruned;
  long answer_trie_nodes;
  long answer_linear_nodes;
  int  answer_trie_max_depth;
  int  answer_trie_min_depth;
} trie_stats;
#define TrStat_show               trie_stats.show
#define TrStat_subgoals           trie_stats.subgoals
#define TrStat_subgoals_abolished trie_stats.subgoals_abolished
#define TrStat_sg_nodes           trie_stats.subgoal_trie_nodes
#define TrStat_sg_linear_nodes    trie_stats.subgoal_linear_nodes
#define TrStat_sg_max_depth       trie_stats.subgoal_trie_max_depth
#define TrStat_sg_min_depth       trie_stats.subgoal_trie_min_depth
#define TrStat_answers            trie_stats.answers
#define TrStat_answers_pruned     trie_stats.answers_pruned
#define TrStat_ans_nodes          trie_stats.answer_trie_nodes
#define TrStat_ans_linear_nodes   trie_stats.answer_linear_nodes
#define TrStat_ans_max_depth      trie_stats.answer_trie_max_depth
#define TrStat_ans_min_depth      trie_stats.answer_trie_min_depth
#define SHOW_INFO(MESG, ARGS...) fprintf(stream, MESG, ##ARGS)
#define SHOW_TRIE(MESG, ARGS...) if (TrStat_show) fprintf(stream, MESG, ##ARGS)

void traverse_trie(FILE *stream, sg_node_ptr sg_node, int pred_arity, Atom pred_atom, int show) {
  char str[1000];
  int arity[100];
  int str_index;

  TrStat_show = show;
  TrStat_subgoals = 0;
  TrStat_subgoals_abolished = 0;
  TrStat_sg_nodes = 0;
  TrStat_sg_linear_nodes = 0;
  TrStat_sg_max_depth = -1;
  TrStat_sg_min_depth = -1;
  TrStat_answers = 0;
  TrStat_answers_pruned = 0;
  TrStat_ans_nodes = 0;
  TrStat_ans_linear_nodes = 0;
  TrStat_ans_max_depth = -1;
  TrStat_ans_min_depth = -1;
  str_index = sprintf(str, "  ?- %s(", AtomName(pred_atom));
  arity[0] = 1;
  arity[1] = pred_arity;
  SHOW_INFO("\n[ Trie structure for predicate '%s/%d' ]\n[\n", AtomName(pred_atom), pred_arity);
  TrStat_sg_nodes++;
  if (traverse_subgoal_trie(stream, sg_node, str, str_index, arity, 0)) {
    SHOW_INFO("\n  Subgoal Trie structure\n    %ld subgoals", TrStat_subgoals);
    if (TrStat_subgoals_abolished)
      SHOW_INFO(" (including %ld abolished)", TrStat_subgoals_abolished);
    SHOW_INFO("\n    %ld nodes (%ld%c reuse)\n    %.2f average depth (%d min - %d max)", 
              TrStat_sg_nodes,
              TrStat_sg_linear_nodes == 0 ? 0 : (TrStat_sg_linear_nodes - TrStat_sg_nodes + 1) * 100 / TrStat_sg_linear_nodes,
              '%',
              TrStat_subgoals == 0 ? 0 : (float)TrStat_sg_linear_nodes / (float)TrStat_subgoals,
              TrStat_sg_min_depth < 0 ? 0 : TrStat_sg_min_depth,
              TrStat_sg_max_depth < 0 ? 0 : TrStat_sg_max_depth);
    SHOW_INFO("\n  Answer Trie Structure\n    %ld answers", TrStat_answers);
    if (TrStat_answers_pruned)
      SHOW_INFO(" (including %ld pruned)", TrStat_answers_pruned);
    SHOW_INFO("\n    %ld nodes (%ld%c reuse)\n    %.2f average depth (%d min - %d max)",
              TrStat_ans_nodes,
              TrStat_ans_linear_nodes == 0 ? 0 : (TrStat_ans_linear_nodes - TrStat_ans_nodes + TrStat_subgoals) * 100 / TrStat_ans_linear_nodes,
              '%',
              TrStat_answers == 0 ? 0 : (float)TrStat_ans_linear_nodes / (float)TrStat_answers,
              TrStat_ans_min_depth < 0 ? 0 : TrStat_ans_min_depth,
              TrStat_ans_max_depth < 0 ? 0 : TrStat_ans_max_depth);
  }
  SHOW_INFO("\n]\n\n");
  return;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

static
int traverse_subgoal_trie(FILE *stream, sg_node_ptr sg_node, char *str, int str_index, int *arity, int depth) {
  int tag;
  Term t;
  int new_arity[100];

  if (arity[0] == 0) {
    ans_node_ptr ans_node;
    str[str_index] = 0;
    TrStat_subgoals++;
    TrStat_sg_linear_nodes+= depth;
    if (TrStat_sg_max_depth < 0) {
      TrStat_sg_min_depth = TrStat_sg_max_depth = depth;
    } else if (depth < TrStat_sg_min_depth) {
      TrStat_sg_min_depth = depth;
    } else if (depth > TrStat_sg_max_depth) {
      TrStat_sg_max_depth = depth;
    }
    if (sg_node == NULL) { 
      TrStat_subgoals_abolished++;
      SHOW_TRIE("%s.\n    ABOLISHED\n", str);
      return TRUE;
    }
    if (! SgFr_state((sg_fr_ptr)sg_node)) {
      SHOW_INFO("%s. --> TRIE ERROR: subgoal not completed !!!\n", str);
      return FALSE;
    }
    SHOW_TRIE("%s.\n", str);
    ans_node = SgFr_answer_trie((sg_fr_ptr)sg_node);
    TrStat_ans_nodes++;
    if (IS_ANSWER_LEAF_NODE(ans_node)) {
      SHOW_TRIE("    YES\n");
      if (TrStat_ans_max_depth < 0)
        TrStat_ans_max_depth = 0;
      TrStat_ans_min_depth = 0;
      TrStat_answers++;
    } else if (TrNode_child(ans_node) == NULL) {
      SHOW_TRIE("    NO\n");
      if (TrStat_ans_max_depth < 0)
        TrStat_ans_max_depth = 0;
      TrStat_ans_min_depth = 0;
    } else {
      char answer_str[1000];
      int answer_arity[1000];
      answer_arity[0] = 0;
      if (! traverse_answer_trie(stream, TrNode_child(ans_node), answer_str, 0, answer_arity, 0, 1))
        return FALSE;
    }
    return TRUE;
  }

  if (sg_node == NULL) 
    return TRUE;
  if (IS_SUBGOAL_HASH(sg_node)) {
    sg_node_ptr *bucket, *last_bucket;
    sg_hash_ptr hash;

    hash = (sg_hash_ptr) sg_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    do {
      if (*bucket) {
        sg_node = *bucket;
        memcpy(new_arity, arity, 100);
        if (! traverse_subgoal_trie(stream, sg_node, str, str_index, new_arity, depth))
          return FALSE;
      }
    } while (++bucket != last_bucket);
    return TRUE;
  }
  TrStat_sg_nodes++;
  memcpy(new_arity, arity, 100);
  if (! traverse_subgoal_trie(stream, TrNode_next(sg_node), str, str_index, new_arity, depth))
    return FALSE;

  t = TrNode_entry(sg_node);
  tag = t & TabTagBits;
  switch (tag) {
    case TabVarTagBits:
      str_index += sprintf(& str[str_index], "VAR%d", VarIndexOfTableTerm(t));
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], "]");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], "|");
            break;
	  }
	}
      }
      break;
    case TabNumberTagBits:
      str_index += sprintf(& str[str_index], "%d", IntOfTerm(t));
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str[str_index] = 0;
            SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
            return FALSE;
	  }
          str_index += sprintf(& str[str_index], "|");
          break;
	}
      }
      break;
    case TabAtomTagBits:
      if (arity[arity[0]] == -1) {
        if (strcmp("[]", AtomName(AtomOfTerm(t)))) {
          str[str_index] = 0;
          SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
          return FALSE;
	}
        str[str_index - 1] = ']';
        arity[0]--;
      } else {
        str_index += sprintf(& str[str_index], "%s", AtomName(AtomOfTerm(t)));
      }
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str[str_index] = 0;
            SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
            return FALSE;
	  }
          str_index += sprintf(& str[str_index], "|");
          break;
	}
      }
      break;
    case TabPairTagBits:
      if (arity[arity[0]] == -1) {
        str[str_index - 1] = ',';
        arity[arity[0]] = -2;
      } else {
        str_index += sprintf(& str[str_index], "[");
        arity[0]++;
        arity[arity[0]] = -2;
      }
      break;
    case TabApplTagBits:
      str_index += sprintf(& str[str_index], "%s(", AtomName(NameOfFunctor((Functor)NonTagPart(t))));
      arity[0]++;
      arity[arity[0]] = ArityOfFunctor((Functor)NonTagPart(t));
      break;
    default:
      abort_optyap("unknown type tag in function traverse_subgoal_trie");
  }

  if (! traverse_subgoal_trie(stream, TrNode_child(sg_node), str, str_index, arity, depth + 1))
    return FALSE;
  return TRUE;
}


static
int traverse_answer_trie(FILE *stream, ans_node_ptr ans_node, char *str, int str_index, int *arity, int var_index, int depth) {
  int tag;
  Term t;
  int new_arity[100];

  if (ans_node == NULL) 
    return TRUE;
  TrStat_ans_nodes++;
  memcpy(new_arity, arity, 100);
  if (! traverse_answer_trie(stream, TrNode_next(ans_node), str, str_index, new_arity, var_index, depth))
    return FALSE;

  if (arity[0] == 0) {
    str_index += sprintf(& str[str_index], "    VAR%d: ", var_index);
    var_index++;
  }

  t = TrNode_entry(ans_node);
  tag = t & TabTagBits;
  switch (tag) {
    case TabVarTagBits:
      str_index += sprintf(& str[str_index], "ANSVAR%d", VarIndexOfTableTerm(t));
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], "]");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], "|");
            break;
	  }
	}
      }
      break;
    case TabNumberTagBits:
      str_index += sprintf(& str[str_index], "%d", IntOfTerm(t));
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str[str_index] = 0;
            SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
            return FALSE;
	  }
          str_index += sprintf(& str[str_index], "|");
          break;
	}
      }
      break;
    case TabAtomTagBits:
      if (arity[arity[0]] == -1) {
        if (strcmp("[]", AtomName(AtomOfTerm(t)))) {
          str[str_index] = 0;
          SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
          return FALSE;
	}
        str[str_index - 1] = ']';
        arity[0]--;
      } else {
        str_index += sprintf(& str[str_index], "%s", AtomName(AtomOfTerm(t)));
      }
      while (arity[0]) {
        if (arity[arity[0]] > 0) {
          arity[arity[0]]--;
          if (arity[arity[0]] == 0) {
            str_index += sprintf(& str[str_index], ")");
            arity[0]--;
	  } else {
            str_index += sprintf(& str[str_index], ",");
            break;
	  }
	} else {
          arity[arity[0]]++;
          if (arity[arity[0]] == 0) {
            str[str_index] = 0;
            SHOW_INFO("%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
            return FALSE;
	  }
          str_index += sprintf(& str[str_index], "|");
          break;
	}
      }
      break;
    case TabPairTagBits:
      if (arity[arity[0]] == -1) {
        str[str_index - 1] = ',';
        arity[arity[0]] = -2;
      } else {
        str_index += sprintf(& str[str_index], "[");
        arity[0]++;
        arity[arity[0]] = -2;
      }
      break;
    case TabApplTagBits:
      str_index += sprintf(& str[str_index], "%s(", AtomName(NameOfFunctor((Functor)NonTagPart(t))));
      arity[0]++;
      arity[arity[0]] = ArityOfFunctor((Functor)NonTagPart(t));
      break;
    default:
      abort_optyap("unknown type tag in function traverse_answer_trie");
  }

  if (! IS_ANSWER_LEAF_NODE(ans_node)) {
#ifdef TABLING_INNER_CUTS
    if (! TrNode_child(ans_node)) {
      TrStat_answers_pruned++;
      return TRUE;
    }
#endif /* TABLING_INNER_CUTS */
    if (! traverse_answer_trie(stream, TrNode_child(ans_node), str, str_index, arity, var_index, depth + 1))
      return FALSE;
  } else {
    str[str_index] = 0;
    SHOW_TRIE("%s\n", str);
    TrStat_answers++;
    TrStat_ans_linear_nodes+= depth;
    if (TrStat_ans_max_depth < 0) {
      TrStat_ans_min_depth = TrStat_ans_max_depth = depth;
    } else if (depth < TrStat_ans_min_depth) {
      TrStat_ans_min_depth = depth;
    } else if (depth > TrStat_ans_max_depth) {
      TrStat_ans_max_depth = depth;
    }
  }
  return TRUE;
}


static
void free_answer_trie_branch(ans_node_ptr node) {
#ifdef TABLING_INNER_CUTS
  if (TrNode_child(node) && ! IS_ANSWER_LEAF_NODE(node))
#else
  if (! IS_ANSWER_LEAF_NODE(node))
#endif /* TABLING_INNER_CUTS */
    free_answer_trie_branch(TrNode_child(node));
  if (TrNode_next(node))
    free_answer_trie_branch(TrNode_next(node));
  FREE_ANSWER_TRIE_NODE(node);
  return;
}


#ifdef YAPOR
#ifdef TABLING_INNER_CUTS
static
int update_answer_trie_branch(ans_node_ptr previous_node, ans_node_ptr node) {
  int ltt;
  if (! IS_ANSWER_LEAF_NODE(node)) {
    if (TrNode_child(node)) {
      TrNode_instr(TrNode_child(node)) -= 1;  /* retry --> try */
      update_answer_trie_branch(NULL, TrNode_child(node));
      if (TrNode_child(node))
        goto update_next_trie_branch;
    }
    /* node belonging to a pruned answer */
    if (previous_node) {
      TrNode_next(previous_node) = TrNode_next(node);
      FREE_ANSWER_TRIE_NODE(node);
      if (TrNode_next(previous_node)) {
        return update_answer_trie_branch(previous_node, TrNode_next(previous_node));
      } else {
        TrNode_instr(previous_node) -= 2;  /* retry --> trust : try --> do */
        return 0;
      }
    } else {
      TrNode_child(TrNode_parent(node)) = TrNode_next(node);
      if (TrNode_next(node)) {
        TrNode_instr(TrNode_next(node)) -= 1;  /* retry --> try */
        update_answer_trie_branch(NULL, TrNode_next(node));          
      }
      FREE_ANSWER_TRIE_NODE(node);
      return 0;
    }
  }
update_next_trie_branch:
  if (TrNode_next(node)) {
    ltt = 1 + update_answer_trie_branch(node, TrNode_next(node));
  } else {
    TrNode_instr(node) -= 2;  /* retry --> trust : try --> do */
    ltt = 1;
  }

  TrNode_or_arg(node) = ltt;
  TrNode_instr(node) = _YAP_opcode(TrNode_instr(node));
  return ltt;
}
#else
static
int update_answer_trie_branch(ans_node_ptr node) {
  int ltt;
  if (! IS_ANSWER_LEAF_NODE(node)) {
    TrNode_instr(TrNode_child(node)) -= 1;  /* retry --> try */
    update_answer_trie_branch(TrNode_child(node));
  }
  if (TrNode_next(node)) {
    ltt = 1 + update_answer_trie_branch(TrNode_next(node));
  } else {
    TrNode_instr(node) -= 2;  /* retry --> trust : try --> do */
    ltt = 1;
  }
  TrNode_or_arg(node) = ltt;
  TrNode_instr(node) = _YAP_opcode(TrNode_instr(node));
  return ltt;
}
#endif /* TABLING_INNER_CUTS */
#else /* TABLING */
static
void update_answer_trie_branch(ans_node_ptr node) {
  if (! IS_ANSWER_LEAF_NODE(node)) {
    TrNode_instr(TrNode_child(node)) -= 1;  /* retry --> try */
    update_answer_trie_branch(TrNode_child(node));
  }
  if (TrNode_next(node)) {
    update_answer_trie_branch(TrNode_next(node));
  } else {
    TrNode_instr(node) -= 2;  /* retry --> trust : try --> do */
  }
  TrNode_instr(node) = _YAP_opcode(TrNode_instr(node));
  return;
}
#endif /* YAPOR */
#endif /* TABLING */
