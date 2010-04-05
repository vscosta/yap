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

/*********************
**      Macros      **
*********************/

#ifdef IS_GLOBAL_TRIE_REFERENCE
#define NEW_SUBGOAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)        \
        INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                        \
        new_subgoal_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#define NEW_ANSWER_TRIE_NODE(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)  \
        INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                        \
        new_answer_trie_node(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)
#define NEW_GLOBAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)         \
        INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                        \
        new_global_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#else
#define NEW_SUBGOAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)        \
        new_subgoal_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#define NEW_ANSWER_TRIE_NODE(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)  \
        new_answer_trie_node(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)
#define NEW_GLOBAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)         \
        new_global_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#endif /* IS_GLOBAL_TRIE_REFERENCE */

#if defined(TABLE_LOCK_AT_WRITE_LEVEL)
#define LOCK_NODE(NODE)    LOCK_TABLE(NODE)
#define UNLOCK_NODE(NODE)  UNLOCK_TABLE(NODE)
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
#define LOCK_NODE(NODE)    TRIE_LOCK(TrNode_lock(NODE))
#define UNLOCK_NODE(NODE)  UNLOCK(TrNode_lock(NODE))
#else
#define LOCK_NODE(NODE)
#define UNLOCK_NODE(NODE)
#endif /* TABLE_LOCK_LEVEL */



/************************************************************************
**                 subgoal_trie_check_insert_(gt)_token                **
************************************************************************/

#ifdef INCLUDE_SUBGOAL_TRIE_CHECK_INSERT
#ifdef TABLE_LOCK_AT_WRITE_LEVEL
#ifdef IS_GLOBAL_TRIE_REFERENCE
static inline sg_node_ptr subgoal_trie_check_insert_gt_token(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#else
static inline sg_node_ptr subgoal_trie_check_insert_token(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#endif /* IS_GLOBAL_TRIE_REFERENCE */
  sg_node_ptr child_node;
  sg_hash_ptr hash;

  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
#ifdef ALLOC_BEFORE_CHECK
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (TrNode_child(parent_node)) {
      sg_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_TRIE_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_NODE(parent_node);
        hash = (sg_hash_ptr) chain_node;
        goto subgoal_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  } 

  if (! IS_SUBGOAL_TRIE_HASH(child_node)) {
    sg_node_ptr first_node = child_node;
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
#ifdef ALLOC_BEFORE_CHECK
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      sg_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_TRIE_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_NODE(parent_node);
        hash = (sg_hash_ptr) chain_node;
        goto subgoal_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_node_ptr chain_node, next_node, *bucket;
      new_subgoal_trie_hash(hash, count_nodes, tab_ent);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
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

  hash = (sg_hash_ptr) child_node;
subgoal_trie_hash:
  { /* trie nodes with hashing */
    sg_node_ptr *bucket, first_node;
    int seed, count_nodes = 0;

    seed = Hash_seed(hash);
    bucket = Hash_bucket(hash, HASH_ENTRY(t, seed));
    first_node = child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
#ifdef ALLOC_BEFORE_CHECK
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (seed != Hash_seed(hash)) {
      /* the hash has been expanded */ 
#ifdef ALLOC_BEFORE_CHECK
      FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
      UNLOCK_NODE(parent_node);
      goto subgoal_trie_hash;
    }
    if (first_node != *bucket) {
      sg_node_ptr chain_node = *bucket;
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = *bucket;
#else
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
    } else {
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      sg_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash));
      seed = Hash_seed(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), seed));
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
#else /* TABLE_LOCK_AT_ENTRY_LEVEL || TABLE_LOCK_AT_NODE_LEVEL || ! YAPOR */
#ifdef IS_GLOBAL_TRIE_REFERENCE
static inline sg_node_ptr subgoal_trie_check_insert_gt_token(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#else
static inline sg_node_ptr subgoal_trie_check_insert_token(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#endif /* IS_GLOBAL_TRIE_REFERENCE */
  sg_node_ptr child_node;

  LOCK_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  if (! IS_SUBGOAL_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_hash_ptr hash;
      sg_node_ptr chain_node, next_node, *bucket;
      new_subgoal_trie_hash(hash, count_nodes, tab_ent);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
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
    int count_nodes = 0;
    hash = (sg_hash_ptr) child_node;
    bucket = Hash_bucket(hash, HASH_ENTRY(t, Hash_seed(hash)));
    child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      sg_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      int seed;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash));
      seed = Hash_seed(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), seed));
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
#endif /* INCLUDE_SUBGOAL_TRIE_CHECK_INSERT */



/************************************************************************
**                 answer_trie_check_insert_(gt)_token                 **
************************************************************************/

#ifdef INCLUDE_ANSWER_TRIE_CHECK_INSERT
#ifdef TABLE_LOCK_AT_WRITE_LEVEL
#ifdef IS_GLOBAL_TRIE_REFERENCE
static inline ans_node_ptr answer_trie_check_insert_gt_token(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#else
static inline ans_node_ptr answer_trie_check_insert_token(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#endif /* IS_GLOBAL_TRIE_REFERENCE */
  ans_node_ptr child_node;
  ans_hash_ptr hash;

#ifdef TABLING_ERRORS
  if (IS_ANSWER_LEAF_NODE(parent_node))
    TABLING_ERROR_MESSAGE("IS_ANSWER_LEAF_NODE(parent_node) (answer_token_check_insert)");
#endif /* TABLING_ERRORS */

  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
#ifdef ALLOC_BEFORE_CHECK
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (TrNode_child(parent_node)) {
      ans_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_TRIE_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_NODE(parent_node);
        hash = (ans_hash_ptr) chain_node;
        goto answer_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
#endif /* ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  } 

  if (! IS_ANSWER_TRIE_HASH(child_node)) {
    ans_node_ptr first_node = child_node;
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
#ifdef ALLOC_BEFORE_CHECK
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      ans_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_TRIE_HASH(chain_node)) {
#ifdef ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
        UNLOCK_NODE(parent_node);
        hash = (ans_hash_ptr) chain_node; 
        goto answer_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    } else {
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_node_ptr chain_node, next_node, *bucket;
      new_answer_trie_hash(hash, count_nodes, sg_fr);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
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

  hash = (ans_hash_ptr) child_node;
answer_trie_hash:
  { /* trie nodes with hashing */
    ans_node_ptr *bucket, first_node;
    int seed, count_nodes = 0;

    seed = Hash_seed(hash);
    bucket = Hash_bucket(hash, HASH_ENTRY(t, seed));
    first_node = child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
#ifdef ALLOC_BEFORE_CHECK
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    LOCK_NODE(parent_node);
    if (seed != Hash_seed(hash)) {
      /* the hash has been expanded */ 
#ifdef ALLOC_BEFORE_CHECK
      FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
      UNLOCK_NODE(parent_node);
      goto answer_trie_hash;
    }
    if (first_node != *bucket) {
      ans_node_ptr chain_node = *bucket;
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ALLOC_BEFORE_CHECK */
          UNLOCK_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = *bucket;
#else
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, *bucket);
    } else {
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ALLOC_BEFORE_CHECK */
    }
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      ans_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash));
      seed = Hash_seed(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), seed));
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
#else /* TABLE_LOCK_AT_ENTRY_LEVEL || TABLE_LOCK_AT_NODE_LEVEL || ! YAPOR */
#ifdef IS_GLOBAL_TRIE_REFERENCE
static inline ans_node_ptr answer_trie_check_insert_gt_token(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#else
static inline ans_node_ptr answer_trie_check_insert_token(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#endif /* IS_GLOBAL_TRIE_REFERENCE */
  ans_node_ptr child_node;

#ifdef TABLING_ERRORS
  if (IS_ANSWER_LEAF_NODE(parent_node))
    TABLING_ERROR_MESSAGE("IS_ANSWER_LEAF_NODE(parent_node) (answer_token_check_insert)");
#endif /* TABLING_ERRORS */

  LOCK_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  if (! IS_ANSWER_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_hash_ptr hash;
      ans_node_ptr chain_node, next_node, *bucket;
      new_answer_trie_hash(hash, count_nodes, sg_fr);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
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
    int count_nodes = 0;
    hash = (ans_hash_ptr) child_node;
    bucket = Hash_bucket(hash, HASH_ENTRY(t, Hash_seed(hash)));
    child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */ 
      ans_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      int seed;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash));
      seed = Hash_seed(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), seed));
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
#endif /* INCLUDE_ANSWER_TRIE_CHECK_INSERT */



/************************************************************************
**                 global_trie_check_insert_(gt)_token                 **
************************************************************************/

#ifdef INCLUDE_GLOBAL_TRIE_CHECK_INSERT
#ifdef IS_GLOBAL_TRIE_REFERENCE
static inline gt_node_ptr global_trie_check_insert_gt_token(gt_node_ptr parent_node, Term t) {   
#else
static inline gt_node_ptr global_trie_check_insert_token(gt_node_ptr parent_node, Term t) {   
#endif /* IS_GLOBAL_TRIE_REFERENCE */
  gt_node_ptr child_node;
    
  LOCK_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  if (! IS_GLOBAL_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      gt_hash_ptr hash;
      gt_node_ptr chain_node, next_node, *bucket;
      new_global_trie_hash(hash, count_nodes);
      chain_node = child_node;
      do {
        bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS - 1));
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (gt_node_ptr) hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    gt_hash_ptr hash;
    gt_node_ptr *bucket;
    int count_nodes = 0;
    hash = (gt_hash_ptr) child_node; 
    bucket = Hash_bucket(hash, HASH_ENTRY(t, Hash_seed(hash)));
    child_node = *bucket;
    while (child_node) { 
      if (TrNode_entry(child_node) == t) {
        UNLOCK_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } 
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET && Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      gt_node_ptr chain_node, next_node, *first_old_bucket, *old_bucket;
      int seed;
      first_old_bucket = Hash_buckets(hash);
      old_bucket = first_old_bucket + Hash_num_buckets(hash);
      Hash_num_buckets(hash) *= 2;
      ALLOC_HASH_BUCKETS(Hash_buckets(hash), Hash_num_buckets(hash)); 
      seed = Hash_seed(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = Hash_bucket(hash, HASH_ENTRY(TrNode_entry(chain_node), seed));
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
#endif /* INCLUDE_GLOBAL_TRIE_CHECK_INSERT */
