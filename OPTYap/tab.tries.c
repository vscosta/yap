/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        tab.tries.C
  version:     $Id: tab.tries.c,v 1.15 2005-08-01 15:40:39 ricroc Exp $   
                                                                     
**********************************************************************/

/* ------------------ **
**      Includes      **
** ------------------ */

#include "Yap.h"
#ifdef TABLING
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "tab.macros.h"


/* ----------------- **
**      Defines      **
** ----------------- */

#define TRAVERSE_NORMAL    0
#define TRAVERSE_FLOAT     1
#define TRAVERSE_FLOAT2    2
#define TRAVERSE_FLOAT_END 3
#define TRAVERSE_LONG      4
#define TRAVERSE_LONG_END  5



/* ------------------------------------- **
**      Local functions declaration      **
** ------------------------------------- */

#ifdef YAPOR
#ifdef TABLING_INNER_CUTS
static int update_answer_trie_branch(ans_node_ptr previous_node, ans_node_ptr node);
#else
static int update_answer_trie_branch(ans_node_ptr node);
#endif /* TABLING_INNER_CUTS */
#else
static void update_answer_trie_branch(ans_node_ptr node);
#endif /* YAPOR */
static int traverse_subgoal_trie(sg_node_ptr sg_node, char *str, int str_index, int *arity, int depth, int mode);
static int traverse_answer_trie(ans_node_ptr ans_node, char *str, int str_index, int *arity, int var_index, int depth, int mode);



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
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
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
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
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
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
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
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
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
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
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
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
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
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
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
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
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

sg_fr_ptr subgoal_search(yamop *preg, CELL **Yaddr) {
  int i, j, count_vars, arity;
  CELL *stack_vars, *stack_terms_limit, *stack_terms_base, *stack_terms;
  sg_node_ptr current_sg_node;
  tab_ent_ptr tab_ent;
  sg_fr_ptr sg_fr;

  arity = preg->u.ld.s;
  tab_ent = preg->u.ld.te;
  count_vars = 0;
  stack_vars = *Yaddr;
  stack_terms_limit = (CELL *)TR;
  stack_terms_base = stack_terms = (CELL *)Yap_TrailTop;
  current_sg_node = TabEnt_subgoal_trie(tab_ent);

#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
  LOCK(TabEnt_lock(tab_ent));
#endif /* TABLE_LOCK_LEVEL */
  for (i = 1; i <= arity; i++) {
    STACK_PUSH_UP(XREGS[i], stack_terms);
    STACK_CHECK_EXPAND(stack_terms, stack_terms_limit, stack_terms_base);
    do {
      Term t = Deref(STACK_POP_DOWN(stack_terms));
      if (IsVarTerm(t)) {
	if (IsTableVarTerm(t)) {
	  t = MakeTableVarTerm(VarIndexOfTerm(t));
	  current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
	} else {
	  if (count_vars == MAX_TABLE_VARS)
	    Yap_Error(INTERNAL_ERROR, TermNil, "MAX_TABLE_VARS exceeded (subgoal_search)");
	  STACK_PUSH_UP(t, stack_vars);
	  *((CELL *)t) = GLOBAL_table_var_enumerator(count_vars);
	  t = MakeTableVarTerm(count_vars);
	  count_vars++;
	  current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
	}
      } else if (IsAtomOrIntTerm(t)) {
	current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, t);
      } else if (IsPairTerm(t)) {
	current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, AbsPair(NULL));
	STACK_PUSH_UP(*(RepPair(t) + 1), stack_terms);
	STACK_CHECK_EXPAND(stack_terms, stack_terms_limit, stack_terms_base);
	STACK_PUSH_UP(*(RepPair(t)), stack_terms);
	STACK_CHECK_EXPAND(stack_terms, stack_terms_limit, stack_terms_base);
      } else if (IsApplTerm(t)) {
	Functor f = FunctorOfTerm(t);
	current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, AbsAppl((Term *)f));
	if (f == FunctorDouble) {
	  volatile Float dbl = FloatOfTerm(t);
	  volatile Term *t_dbl = (Term *)((void *) &dbl);
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
	  current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, *(t_dbl + 1));
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
	  current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, *t_dbl);
	} else if (f == FunctorLongInt) {
	  Int li = LongIntOfTerm(t);
	  current_sg_node = subgoal_trie_node_check_insert(tab_ent, current_sg_node, li);
	} else {
	  for (j = ArityOfFunctor(f); j >= 1; j--) {
	    STACK_PUSH_UP(*(RepAppl(t) + j), stack_terms);
	    STACK_CHECK_EXPAND(stack_terms, stack_terms_limit, stack_terms_base);
	  }
	}
      } else {
	Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (subgoal_search)");
      }
    } while (STACK_NOT_EMPTY(stack_terms, stack_terms_base));
  }
#if defined(TABLE_LOCK_AT_NODE_LEVEL)
  LOCK(TrNode_lock(current_sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
  LOCK_TABLE(current_sg_node);
#endif /* TABLE_LOCK_LEVEL */
  if (TrNode_sg_fr(current_sg_node) == NULL) {
    /* new tabled subgoal */
    new_subgoal_frame(sg_fr, preg);
    TrNode_sg_fr(current_sg_node) = (sg_node_ptr) sg_fr;
  } else {
    sg_fr = (sg_fr_ptr) TrNode_sg_fr(current_sg_node);
  }
#if defined(TABLE_LOCK_AT_ENTRY_LEVEL)
  UNLOCK(TabEnt_lock(tab_ent));
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
  UNLOCK(TrNode_lock(current_sg_node));
#elif defined(TABLE_LOCK_AT_WRITE_LEVEL)
  UNLOCK_TABLE(current_sg_node);
#endif /* TABLE_LOCK_LEVEL */

  STACK_PUSH_UP(count_vars, stack_vars);
  *Yaddr = stack_vars++;
  /* reset variables */
  while (count_vars--) {
    Term t = STACK_POP_DOWN(stack_vars);
    RESET_VARIABLE(t);
  }

  return sg_fr;
}


ans_node_ptr answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr) {
  int i, j, count_vars, subs_arity;
  CELL *stack_vars, *stack_terms_base, *stack_terms;
  ans_node_ptr current_ans_node;

  count_vars = 0;
  subs_arity = *subs_ptr;
  stack_vars = (CELL *)TR;
  stack_terms_base = stack_terms = (CELL *)Yap_TrailTop;
  current_ans_node = SgFr_answer_trie(sg_fr);

  for (i = subs_arity; i >= 1; i--) {
    STACK_PUSH_UP(*(subs_ptr + i), stack_terms);
    STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
#ifdef TABLING_ERRORS
    if (IsNonVarTerm(*stack_terms))
      TABLING_ERROR_MESSAGE("IsNonVarTem(*stack_terms) (answer_search)");
#endif /* TABLING_ERRORS */
    do {
      Term t = Deref(STACK_POP_DOWN(stack_terms));
      if (IsVarTerm(t)) {
	if (IsTableVarTerm(t)) {
	  t = MakeTableVarTerm(VarIndexOfTerm(t));
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_val);
	} else {
	  if (count_vars == MAX_TABLE_VARS)
	    Yap_Error(INTERNAL_ERROR, TermNil, "MAX_TABLE_VARS exceeded (answer_search)");
	  STACK_PUSH_DOWN(t, stack_vars);
	  STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
	  *((CELL *)t) = GLOBAL_table_var_enumerator(count_vars);
	  t = MakeTableVarTerm(count_vars);
	  count_vars++;
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_var);
	}
      } else if (IsAtomOrIntTerm(t)) {
	current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, t, _trie_retry_atom);
      } else if (IsPairTerm(t)) {
	current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsPair(NULL), _trie_retry_list);
	STACK_PUSH_UP(*(RepPair(t) + 1), stack_terms);
	STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
	STACK_PUSH_UP(*(RepPair(t)), stack_terms);
	STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
      } else if (IsApplTerm(t)) {
	Functor f = FunctorOfTerm(t);
	if (f == FunctorDouble) {
	  volatile Float dbl = FloatOfTerm(t);
	  volatile Term *t_dbl = (Term *)((void *) &dbl);
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsAppl((Term *)f), _trie_retry_null);
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, *(t_dbl + 1), _trie_retry_extension);
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, *t_dbl, _trie_retry_extension);
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsAppl((Term *)f), _trie_retry_float);
	} else if (f == FunctorLongInt) {
	  Int li = LongIntOfTerm (t);
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsAppl((Term *)f), _trie_retry_null);
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, li, _trie_retry_extension);
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsAppl((Term *)f), _trie_retry_long);
	} else {
	  current_ans_node = answer_trie_node_check_insert(sg_fr, current_ans_node, AbsAppl((Term *)f), _trie_retry_struct);
	  for (j = ArityOfFunctor(f); j >= 1; j--) {
	    STACK_PUSH_UP(*(RepAppl(t) + j), stack_terms);
	    STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
	  }
	}
      } else {
	Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (answer_search)");
      }
    } while (STACK_NOT_EMPTY(stack_terms, stack_terms_base));
  }

  /* reset variables */
  while (count_vars--) {
    Term t = STACK_POP_UP(stack_vars);
    RESET_VARIABLE(t);
  }

  return current_ans_node;
}


void load_answer_trie(ans_node_ptr ans_node, CELL *subs_ptr) {
  CELL *stack_vars_base, *stack_vars, *stack_terms_base, *stack_terms;
  int subs_arity, i, n_vars = MAX_TABLE_VARS;
  Term t;

  if ((subs_arity = *subs_ptr) == 0)
    return;

#ifdef TABLING_ERRORS
  if (H < H_FZ)
    TABLING_ERROR_MESSAGE("H < H_FZ (load_answer_trie)");
#endif /* TABLING_ERRORS */
  stack_vars_base = stack_vars = (CELL *)TR;
  stack_terms_base = stack_terms = (CELL *)Yap_TrailTop;

  t = TrNode_entry(ans_node);
  ans_node = UNTAG_ANSWER_LEAF_NODE(TrNode_parent(ans_node));
  do {
    if (IsVarTerm(t)) {
      int var_index = VarIndexOfTableTerm(t);
      if (n_vars == MAX_TABLE_VARS) {
	stack_vars += var_index;
	STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
      }
      if (var_index < n_vars) {
	n_vars = var_index;
	stack_vars_base[var_index] = MkVarTerm();
      }
      STACK_PUSH_UP(stack_vars_base[var_index], stack_terms);
      STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
    } else if (IsAtomOrIntTerm(t)) {
      STACK_PUSH_UP(t, stack_terms);
      STACK_CHECK_EXPAND(stack_terms, stack_vars, stack_terms_base);
    } else if (IsPairTerm(t)) {
      Term head = STACK_POP_DOWN(stack_terms);
      Term tail = STACK_POP_DOWN(stack_terms);
      t = MkPairTerm(head, tail);
      STACK_PUSH_UP(t, stack_terms);
    } else if (IsApplTerm(t)) {
      Functor f = (Functor) RepAppl(t);
      if (f == FunctorDouble) {
	volatile Float dbl;
	volatile Term *t_dbl = (Term *)((void *) &dbl);
	t = TrNode_entry(ans_node);
	ans_node = TrNode_parent(ans_node);
	*t_dbl = t;
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
	t = TrNode_entry(ans_node);
	ans_node = TrNode_parent(ans_node);
	*(t_dbl + 1) = t;
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
	ans_node = TrNode_parent(ans_node);
	t = MkFloatTerm(dbl);
	STACK_PUSH_UP(t, stack_terms);
      } else if (f == FunctorLongInt) {
	Int li = TrNode_entry(ans_node);
	ans_node = TrNode_parent(ans_node);
	ans_node = TrNode_parent(ans_node);
	t = MkLongIntTerm(li);
	STACK_PUSH_UP(t, stack_terms);
      } else {
	int f_arity = ArityOfFunctor(f);
	t = Yap_MkApplTerm(f, f_arity, stack_terms);
	stack_terms += f_arity;
	STACK_PUSH_UP(t, stack_terms);
      }
    } else {
      Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (load_answer_trie)");
    }
    t = TrNode_entry(ans_node);
    ans_node = TrNode_parent(ans_node);
  } while (ans_node);

  for (i = subs_arity; i >= 1; i--) {
    CELL *subs_var = (CELL *) *(subs_ptr + i);
    t = STACK_POP_DOWN(stack_terms);
    Bind(subs_var, t);
  }

#ifdef TABLING_ERRORS
  if (stack_terms != (CELL *)Yap_TrailTop)
    TABLING_ERROR_MESSAGE("stack_terms != Yap_TrailTop (load_answer_trie)");
#endif /* TABLING_ERRORS */

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
  while (EQUAL_OR_YOUNGER_CP(DepFr_cons_cp(LOCAL_top_dep_fr), B)) {  /* never equal if batched scheduling */
    dep_fr_ptr dep_fr = DepFr_next(LOCAL_top_dep_fr);
    FREE_DEPENDENCY_FRAME(LOCAL_top_dep_fr);
    LOCAL_top_dep_fr = dep_fr;
  }

  /* adjust freeze registers */
  adjust_freeze_registers();

  return;
}


void free_subgoal_trie_branch(sg_node_ptr node, int missing_nodes) {
  Term t;

  if (TrNode_next(node))
    free_subgoal_trie_branch(TrNode_next(node), missing_nodes);

  t = TrNode_entry(node);
  if (IsVarTerm(t) || IsAtomOrIntTerm(t)) {
    missing_nodes -= 1;
  } else if (IsPairTerm(t)) {
    missing_nodes += 1;
  } else if (IsApplTerm(t)) {
    missing_nodes += ArityOfFunctor((Functor)RepAppl(t)) - 1;
  } else {
    Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (chain_subgoal_frames)");
  }
  if (missing_nodes) {
    free_subgoal_trie_branch(TrNode_child(node), missing_nodes);
  } else {
    sg_fr_ptr sg_fr;
    ans_node_ptr ans_node;
    sg_fr = (sg_fr_ptr) TrNode_sg_fr(node);
    free_answer_hash_chain(SgFr_hash_chain(sg_fr));
    ans_node = SgFr_answer_trie(sg_fr);
    if (TrNode_child(ans_node))
      free_answer_trie_branch(TrNode_child(ans_node));
    FREE_ANSWER_TRIE_NODE(ans_node);
    FREE_SUBGOAL_FRAME(sg_fr);
  }

  FREE_SUBGOAL_TRIE_NODE(node);
  return;
}


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


void update_answer_trie(sg_fr_ptr sg_fr) {
  ans_node_ptr node;

  free_answer_hash_chain(SgFr_hash_chain(sg_fr));
  SgFr_hash_chain(sg_fr) = NULL;
  node = TrNode_child(SgFr_answer_trie(sg_fr));
  if (node) {
    TrNode_instr(node) -= 1;
#ifdef TABLING_INNER_CUTS
    update_answer_trie_branch(NULL, node);
#else
    update_answer_trie_branch(node);
#endif /* TABLING_INNER_CUTS */
  }
  SgFr_state(sg_fr) = compiled;
  return;
}


static struct trie_statistics{
  int show;
  long subgoals;
  long subgoals_not_complete;
  long subgoal_trie_nodes;
  long subgoal_linear_nodes;
  int  subgoal_trie_max_depth;
  int  subgoal_trie_min_depth;
  long answers;
  long answers_yes;
  long answers_no;
  long answers_pruned;
  long answer_trie_nodes;
  long answer_linear_nodes;
  int  answer_trie_max_depth;
  int  answer_trie_min_depth;
} trie_stats;
#define TrStat_show              trie_stats.show
#define TrStat_subgoals          trie_stats.subgoals
#define TrStat_sg_not_complete   trie_stats.subgoals_not_complete
#define TrStat_sg_nodes          trie_stats.subgoal_trie_nodes
#define TrStat_sg_linear_nodes   trie_stats.subgoal_linear_nodes
#define TrStat_sg_max_depth      trie_stats.subgoal_trie_max_depth
#define TrStat_sg_min_depth      trie_stats.subgoal_trie_min_depth
#define TrStat_answers           trie_stats.answers
#define TrStat_answers_yes       trie_stats.answers_yes
#define TrStat_answers_no        trie_stats.answers_no
#define TrStat_ans_pruned        trie_stats.answers_pruned
#define TrStat_ans_nodes         trie_stats.answer_trie_nodes
#define TrStat_ans_linear_nodes  trie_stats.answer_linear_nodes
#define TrStat_ans_max_depth     trie_stats.answer_trie_max_depth
#define TrStat_ans_min_depth     trie_stats.answer_trie_min_depth

#define STR_ARRAY_SIZE  1000
#define ARITY_ARRAY_SIZE 100
#define SHOW_TABLE(MESG, ARGS...)  if (TrStat_show) fprintf(Yap_stderr, MESG, ##ARGS)


int traverse_table(tab_ent_ptr tab_ent, Atom pred_atom, int show_table) {
  sg_node_ptr sg_node = TrNode_child(TabEnt_subgoal_trie(tab_ent));

  TrStat_show = show_table;
  TrStat_subgoals = 0;
  TrStat_sg_not_complete = 0;
  TrStat_sg_nodes = 1;
  TrStat_sg_linear_nodes = 0;
  TrStat_sg_max_depth = -1;
  TrStat_sg_min_depth = -1;
  TrStat_answers = 0;
  TrStat_answers_yes = 0;
  TrStat_answers_no = 0;
  TrStat_ans_pruned = 0;
  TrStat_ans_nodes = 0;
  TrStat_ans_linear_nodes = 0;
  TrStat_ans_max_depth = -1;
  TrStat_ans_min_depth = -1;
  if (sg_node) {
    char str[STR_ARRAY_SIZE];
    int str_index = sprintf(str, "  ?- %s(", AtomName(pred_atom));
    int arity[ARITY_ARRAY_SIZE];
    arity[0] = 1;
    arity[1] = TabEnt_arity(tab_ent);
    return traverse_subgoal_trie(sg_node, str, str_index, arity, 1, TRAVERSE_NORMAL);
  }
  SHOW_TABLE("  empty\n");
  return TRUE;
}


void table_stats(void) {
  fprintf(Yap_stderr, "\n  Subgoal trie structure");
  fprintf(Yap_stderr, "\n    subgoals: %ld", TrStat_subgoals);
  fprintf(Yap_stderr, "\n    subgoals not complete: %ld", TrStat_sg_not_complete);
  fprintf(Yap_stderr, "\n    nodes: %ld (%ld%c saving)", 
          TrStat_sg_nodes,
	  TrStat_sg_linear_nodes == 0 ? 0 : (TrStat_sg_linear_nodes - TrStat_sg_nodes + 1) * 100 / TrStat_sg_linear_nodes,
	  '%');
  fprintf(Yap_stderr, "\n    average depth: %.2f (%d min - %d max)", 
	  TrStat_subgoals == 0 ? 0 : (float)TrStat_sg_linear_nodes / (float)TrStat_subgoals,
	  TrStat_sg_min_depth < 0 ? 0 : TrStat_sg_min_depth,
	  TrStat_sg_max_depth < 0 ? 0 : TrStat_sg_max_depth);
  fprintf(Yap_stderr, "\n  Answer trie structure");
  fprintf(Yap_stderr, "\n    answers: %ld", TrStat_answers);
  fprintf(Yap_stderr, "\n    yes answers: %ld", TrStat_answers_yes);
  fprintf(Yap_stderr, "\n    no answers: %ld", TrStat_answers_no);
  fprintf(Yap_stderr, "\n    pruned answers: %ld", TrStat_ans_pruned);
  fprintf(Yap_stderr, "\n    nodes: %ld (%ld%c saving)",
	  TrStat_ans_nodes,
	  TrStat_ans_linear_nodes == 0 ? 0 : (TrStat_ans_linear_nodes - TrStat_ans_nodes + TrStat_subgoals) * 100 / TrStat_ans_linear_nodes,
	    '%');
  fprintf(Yap_stderr, "\n    average depth: %.2f (%d min - %d max)",
	  TrStat_answers == 0 ? 0 : (float)TrStat_ans_linear_nodes / (float)TrStat_answers,
	  TrStat_ans_min_depth < 0 ? 0 : TrStat_ans_min_depth,
	  TrStat_ans_max_depth < 0 ? 0 : TrStat_ans_max_depth);
  fprintf(Yap_stderr, "\n  Total memory in use\n    %ld bytes\n",
	  sizeof(struct table_entry) + 
          TrStat_sg_nodes * sizeof(struct subgoal_trie_node) +
	  TrStat_ans_nodes * sizeof(struct answer_trie_node) +
	  TrStat_subgoals * sizeof(struct subgoal_frame));
  return;
}



/* ------------------------- **
**      Local functions      **
** ------------------------- */

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
  TrNode_instr(node) = Yap_opcode(TrNode_instr(node));
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
  TrNode_instr(node) = Yap_opcode(TrNode_instr(node));
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
  TrNode_instr(node) = Yap_opcode(TrNode_instr(node));
  return;
}
#endif /* YAPOR */


static
int traverse_subgoal_trie(sg_node_ptr sg_node, char *str, int str_index, int *arity, int depth, int mode) {
  int old_str_index, old_arity[ARITY_ARRAY_SIZE], old_mode;
  Term t;

  /* save the current state */
  old_mode = mode;
  old_str_index = str_index;
  memcpy(old_arity, arity, sizeof(int) * (arity[0] + 1));
  t = TrNode_entry(sg_node);

  /* test if hashing */
  if (IS_SUBGOAL_HASH(sg_node)) {
    sg_node_ptr *bucket, *last_bucket;
    sg_hash_ptr hash;
    hash = (sg_hash_ptr) sg_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    do {
      if (*bucket) {
        sg_node = *bucket;
        if (! traverse_subgoal_trie(sg_node, str, str_index, arity, depth, mode))
          return FALSE;
	memcpy(arity, old_arity, sizeof(int) * (old_arity[0] + 1));
      }
    } while (++bucket != last_bucket);
    return TRUE;
  }

  /* test the node type */
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
  if (mode == TRAVERSE_FLOAT) {
    arity[0]++;
    arity[arity[0]] = (int) t;
    mode = TRAVERSE_FLOAT2;
  } else if (mode == TRAVERSE_FLOAT2) {
    volatile Float dbl;
    volatile Term *t_dbl = (Term *)((void *) &dbl);
    *t_dbl = t;
    *(t_dbl + 1) = (Term) arity[arity[0]];
    arity[0]--;
#else /* SIZEOF_DOUBLE == SIZEOF_LONG_INT */
  if (mode == TRAVERSE_FLOAT) {
    Float dbl = (Float) t;
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
    str_index += sprintf(& str[str_index], "%.15g", dbl);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
    mode = TRAVERSE_NORMAL;
  } else if (mode == TRAVERSE_LONG) {
    Int li = (Int) t;
    str_index += sprintf(& str[str_index], "%d", li);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
    mode = TRAVERSE_NORMAL;
  } else if (IsVarTerm(t)) {
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
  } else if (IsIntTerm(t)) {
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
  } else if (IsAtomTerm(t)) {
    if (arity[arity[0]] == -1) {
      if (strcmp("[]", AtomName(AtomOfTerm(t)))) {
	str[str_index] = 0;
	fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
  } else if (IsPairTerm(t)) {    
    if (arity[arity[0]] == -1) {
      str[str_index - 1] = ',';
      arity[arity[0]] = -2;
    } else {
      str_index += sprintf(& str[str_index], "[");
      arity[0]++;
      arity[arity[0]] = -2;
    }
  } else if (IsApplTerm(t)) {
    Functor f = (Functor) RepAppl(t);
    if (f == FunctorDouble) {
      mode = TRAVERSE_FLOAT;
    } else if (f == FunctorLongInt) {
      mode = TRAVERSE_LONG;
    } else {
      str_index += sprintf(& str[str_index], "%s(", AtomName(NameOfFunctor(f)));
      arity[0]++;
      arity[arity[0]] = ArityOfFunctor(f);
    }
  } else {
    Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (traverse_subgoal_trie)");
  }

  TrStat_sg_nodes++;
  /* show answers ... */
  if (arity[0] == 0) {
    sg_fr_ptr sg_fr = (sg_fr_ptr) TrNode_child(sg_node);
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
    if (SgFr_state(sg_fr) == start || SgFr_state(sg_fr) == evaluating) {
      TrStat_sg_not_complete++;
      SHOW_TABLE("%s. ---> NOT COMPLETE\n", str);
    } else {
      SHOW_TABLE("%s.\n", str);
    }
    TrStat_ans_nodes++;
    if (SgFr_first_answer(sg_fr) == NULL) {
      if (TrStat_ans_max_depth < 0)
	TrStat_ans_max_depth = 0;
      TrStat_ans_min_depth = 0;
      TrStat_answers_no++;
      SHOW_TABLE("    NO ANSWERS\n");
    } else if (SgFr_first_answer(sg_fr) == SgFr_answer_trie(sg_fr)) {
      if (TrStat_ans_max_depth < 0)
	TrStat_ans_max_depth = 0;
      TrStat_ans_min_depth = 0;
      TrStat_answers_yes++;
      TrStat_answers++;
      SHOW_TABLE("    TRUE\n");
    } else {
      char answer_str[STR_ARRAY_SIZE];
      int answer_arity[ARITY_ARRAY_SIZE];
      answer_arity[0] = 0;
      if (! traverse_answer_trie(TrNode_child(SgFr_answer_trie(sg_fr)), answer_str, 0, answer_arity, 0, 1, TRAVERSE_NORMAL))
	return FALSE;
    }
  }

  /* ... or continue with child node */
  else if (! traverse_subgoal_trie(TrNode_child(sg_node), str, str_index, arity, depth + 1, mode))
    return FALSE;

  /* continue with sibling node */
  if (TrNode_next(sg_node))
    if (! traverse_subgoal_trie(TrNode_next(sg_node), str, old_str_index, old_arity, depth, old_mode))
      return FALSE;

  return TRUE;
}


static
int traverse_answer_trie(ans_node_ptr ans_node, char *str, int str_index, int *arity, int var_index, int depth, int mode) {
  int old_str_index, old_arity[ARITY_ARRAY_SIZE], old_var_index, old_mode;
  Term t;

  /* save the current state */
  old_mode = mode;
  old_var_index = var_index;
  old_str_index = str_index;
  memcpy(old_arity, arity, sizeof(int) * (arity[0] + 1));
  t = TrNode_entry(ans_node);

  /* test if hashing */
  if (IS_ANSWER_HASH(ans_node)) {
    ans_node_ptr *bucket, *last_bucket;
    ans_hash_ptr hash;
    hash = (ans_hash_ptr) ans_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    do {
      if (*bucket) {
        ans_node = *bucket;
        if (! traverse_answer_trie(ans_node, str, str_index, arity, var_index, depth, mode))
          return FALSE;
	memcpy(arity, old_arity, sizeof(int) * (old_arity[0] + 1));
      }
    } while (++bucket != last_bucket);
    return TRUE;
  }

  /* print VAR when starting a term */
  if (arity[0] == 0 && mode == TRAVERSE_NORMAL) {
    str_index += sprintf(& str[str_index], "    VAR%d: ", var_index);
    var_index++;
  }

  /* test the node type */
  if (mode == TRAVERSE_FLOAT) {
#if SIZEOF_DOUBLE == 2 * SIZEOF_LONG_INT
    arity[0]++;
    arity[arity[0]] = (int) t;
    mode = TRAVERSE_FLOAT2;
  } else if (mode == TRAVERSE_FLOAT2) {
    volatile Float dbl;
    volatile Term *t_dbl = (Term *)((void *) &dbl);
    *t_dbl = t;
    *(t_dbl + 1) = (Term) arity[arity[0]];
    arity[0]--;
#else /* SIZEOF_DOUBLE == SIZEOF_LONG_INT */
    Float dbl = (Float) t;
#endif /* SIZEOF_DOUBLE x SIZEOF_LONG_INT */
    str_index += sprintf(& str[str_index], "%.15g", dbl);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
    mode = TRAVERSE_FLOAT_END;
  } else if (mode == TRAVERSE_FLOAT_END) {
    mode = TRAVERSE_NORMAL;
  } else if (mode == TRAVERSE_LONG) {
    Int li = (Int) t;
    str_index += sprintf(& str[str_index], "%d", li);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
    mode = TRAVERSE_LONG_END;
  } else if (mode == TRAVERSE_LONG_END) {
    mode = TRAVERSE_NORMAL;
  } else if (IsVarTerm(t)) {
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
  } else if (IsIntTerm(t)) {
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
  } else if (IsAtomTerm(t)) {
    if (arity[arity[0]] == -1) {
      if (strcmp("[]", AtomName(AtomOfTerm(t)))) {
	str[str_index] = 0;
	fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
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
	  fprintf(Yap_stderr, "%s --> TRIE ERROR: pair without end atom '[]' !!!\n", str);
	  return FALSE;
	}
	str_index += sprintf(& str[str_index], "|");
	break;
      }
    }
  } else if (IsPairTerm(t)) {
    if (arity[arity[0]] == -1) {
      str[str_index - 1] = ',';
      arity[arity[0]] = -2;
    } else {
      str_index += sprintf(& str[str_index], "[");
      arity[0]++;
      arity[arity[0]] = -2;
    }
  } else if (IsApplTerm(t)) {
    Functor f = (Functor) RepAppl(t);
    if (f == FunctorDouble) {
      mode = TRAVERSE_FLOAT;
    } else if (f == FunctorLongInt) {
      mode = TRAVERSE_LONG;
    } else {
      str_index += sprintf(& str[str_index], "%s(", AtomName(NameOfFunctor(f)));
      arity[0]++;
      arity[arity[0]] = ArityOfFunctor(f);
    }
  } else {
    Yap_Error(INTERNAL_ERROR, TermNil, "unknown type tag (traverse_answer_trie)");
  }

  TrStat_ans_nodes++;
  /* show answer .... */
  if (IS_ANSWER_LEAF_NODE(ans_node)) {
    str[str_index] = 0;
    SHOW_TABLE("%s\n", str);
    TrStat_answers++;
    TrStat_ans_linear_nodes+= depth;
    if (TrStat_ans_max_depth < 0)
      TrStat_ans_min_depth = TrStat_ans_max_depth = depth;
    else if (depth < TrStat_ans_min_depth)
      TrStat_ans_min_depth = depth;
    else if (depth > TrStat_ans_max_depth)
      TrStat_ans_max_depth = depth;
  }

#ifdef TABLING_INNER_CUTS
  /* ... or continue with pruned node */
  else if (TrNode_child(ans_node) == NULL)
    TrStat_ans_pruned++;
#endif /* TABLING_INNER_CUTS */

  /* ... or continue with child node */
  else if (! traverse_answer_trie(TrNode_child(ans_node), str, str_index, arity, var_index, depth + 1, mode))
    return FALSE;

  /* continue with sibling node */
  if (TrNode_next(ans_node))
    if (! traverse_answer_trie(TrNode_next(ans_node), str, old_str_index, old_arity, old_var_index, depth, old_mode))
      return FALSE;

  return TRUE;
}
#endif /* TABLING */
