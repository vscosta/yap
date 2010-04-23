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

#undef INCREMENT_GLOBAL_TRIE_REFERENCE
#undef NEW_SUBGOAL_TRIE_NODE
#undef NEW_ANSWER_TRIE_NODE
#undef NEW_GLOBAL_TRIE_NODE
#undef SUBGOAL_CHECK_INSERT_ENTRY
#undef ANSWER_CHECK_INSERT_ENTRY
#undef LOCK_NODE
#undef UNLOCK_NODE

#ifdef MODE_GLOBAL_TRIE_ENTRY
#define INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY)                                                          \
        { register gt_node_ptr entry_node = (gt_node_ptr) (ENTRY);                                      \
 	  TrNode_child(entry_node) = (gt_node_ptr) ((unsigned long int) TrNode_child(entry_node) + 1);  \
	}
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
#endif /* MODE_GLOBAL_TRIE_ENTRY */


#ifdef MODE_GLOBAL_TRIE_LOOP
#define SUBGOAL_CHECK_INSERT_ENTRY(TAB_ENT, NODE, ENTRY)                  \
        NODE = global_trie_check_insert_entry(NODE, ENTRY)
#define ANSWER_CHECK_INSERT_ENTRY(SG_FR, NODE, ENTRY, INSTR)              \
        NODE = global_trie_check_insert_entry(NODE, ENTRY)
#else
#define SUBGOAL_CHECK_INSERT_ENTRY(TAB_ENT, NODE, ENTRY)                  \
        NODE = subgoal_trie_check_insert_entry(TAB_ENT, NODE, ENTRY)
#define ANSWER_CHECK_INSERT_ENTRY(SG_FR, NODE, ENTRY, INSTR)	          \
        NODE = answer_trie_check_insert_entry(SG_FR, NODE, ENTRY, INSTR)
#endif /* MODE_GLOBAL_TRIE_LOOP */


#if defined(TABLE_LOCK_AT_WRITE_LEVEL)
#define LOCK_NODE(NODE)    LOCK_TABLE(NODE)
#define UNLOCK_NODE(NODE)  UNLOCK_TABLE(NODE)
#elif defined(TABLE_LOCK_AT_NODE_LEVEL)
#define LOCK_NODE(NODE)    TRIE_LOCK(TrNode_lock(NODE))
#define UNLOCK_NODE(NODE)  UNLOCK(TrNode_lock(NODE))
#else /* TABLE_LOCK_AT_ENTRY_LEVEL || ! YAPOR */
#define LOCK_NODE(NODE)
#define UNLOCK_NODE(NODE)
#endif /* TABLE_LOCK_LEVEL */



/************************************************************************
**                 subgoal_trie_check_insert_(gt)_entry                **
************************************************************************/

#ifdef INCLUDE_SUBGOAL_TRIE_CHECK_INSERT
#ifndef TABLE_LOCK_AT_WRITE_LEVEL /* TABLE_LOCK_AT_ENTRY_LEVEL || TABLE_LOCK_AT_NODE_LEVEL || ! YAPOR */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline sg_node_ptr subgoal_trie_check_insert_gt_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#else
static inline sg_node_ptr subgoal_trie_check_insert_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
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
#else /*  TABLE_LOCK_AT_WRITE_LEVEL */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline sg_node_ptr subgoal_trie_check_insert_gt_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#else
static inline sg_node_ptr subgoal_trie_check_insert_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node, Term t) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
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
#endif /* TABLE_LOCK_LEVEL */
#endif /* INCLUDE_SUBGOAL_TRIE_CHECK_INSERT */



/************************************************************************
**                 answer_trie_check_insert_(gt)_entry                 **
************************************************************************/

#ifdef INCLUDE_ANSWER_TRIE_CHECK_INSERT
#ifndef TABLE_LOCK_AT_WRITE_LEVEL /* TABLE_LOCK_AT_ENTRY_LEVEL || TABLE_LOCK_AT_NODE_LEVEL || ! YAPOR */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline ans_node_ptr answer_trie_check_insert_gt_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#else
static inline ans_node_ptr answer_trie_check_insert_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  ans_node_ptr child_node;

  TABLING_ERROR_CHECKING(answer_trie_check_insert_(gt)_entry, IS_ANSWER_LEAF_NODE(parent_node));
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
#else
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline ans_node_ptr answer_trie_check_insert_gt_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#else
static inline ans_node_ptr answer_trie_check_insert_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node, Term t, int instr) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  ans_node_ptr child_node;
  ans_hash_ptr hash;

  TABLING_ERROR_CHECKING(answer_trie_check_insert_(gt)_entry, IS_ANSWER_LEAF_NODE(parent_node));
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
#endif /* TABLE_LOCK_LEVEL */
#endif /* INCLUDE_ANSWER_TRIE_CHECK_INSERT */



/************************************************************************
**                 global_trie_check_insert_(gt)_entry                 **
************************************************************************/

#ifdef INCLUDE_GLOBAL_TRIE_CHECK_INSERT
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline gt_node_ptr global_trie_check_insert_gt_entry(gt_node_ptr parent_node, Term t) {   
#else
static inline gt_node_ptr global_trie_check_insert_entry(gt_node_ptr parent_node, Term t) {   
#endif /* MODE_GLOBAL_TRIE_ENTRY */
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



/************************************************************************
**             subgoal_search(_global_trie)(_terms)_loop               **
************************************************************************/

#ifdef INCLUDE_SUBGOAL_SEARCH_LOOP
#ifdef MODE_GLOBAL_TRIE_LOOP
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static inline gt_node_ptr subgoal_search_global_trie_terms_loop(Term t, int *subs_arity_ptr, CELL **stack_vars_ptr, CELL *stack_terms) {
#else
static inline gt_node_ptr subgoal_search_global_trie_loop(Term t, int *subs_arity_ptr, CELL **stack_vars_ptr) {
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
#else
#ifdef MODE_TERMS_LOOP
static inline sg_node_ptr subgoal_search_terms_loop(tab_ent_ptr tab_ent, sg_node_ptr current_node, Term t, int *subs_arity_ptr, CELL **stack_vars_ptr) {
#else
static inline sg_node_ptr subgoal_search_loop(tab_ent_ptr tab_ent, sg_node_ptr current_node, Term t, int *subs_arity_ptr, CELL **stack_vars_ptr) {
#endif /* MODE_TERMS_LOOP */
#endif /* MODE_GLOBAL_TRIE_LOOP */
/************************************************************************
                   ===========
                   |         |
                   |   ...   |
                   |         |
                   -----------
                   |  VAR_N  |  <-- stack_vars
                   -----------           *
                   |   ...   |          /|\
                   -----------           |  subs_arity (N+1)
                   |  VAR_0  |          \|/
                   -----------           *
         YENV -->  |         |
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
                   ----------|           |
                   |  TERM_1 |           |
                   ----------|           |
                   |   NULL  |          \|/
                   ===========           *
 Yap_TrailTop -->  |         |
                   -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  gt_node_ptr current_node = GLOBAL_root_gt;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  int subs_arity = *subs_arity_ptr;
  CELL *stack_vars = *stack_vars_ptr;
#if ! defined(MODE_GLOBAL_TRIE_LOOP) || ! defined(GLOBAL_TRIE_FOR_SUBTERMS)
  CELL *stack_terms = (CELL *) Yap_TrailTop;
#endif /* ! MODE_GLOBAL_TRIE_LOOP || ! GLOBAL_TRIE_FOR_SUBTERMS */
  CELL *stack_terms_limit = (CELL *) TR;
  AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);  /* + 1 because initially we stiil haven't done any STACK_POP_DOWN */
  STACK_PUSH_UP(NULL, stack_terms);

#if defined(MODE_GLOBAL_TRIE_LOOP)
  /* for the global trie, it is safe to skip the IsVarTerm() and IsAtomOrIntTerm() tests in the first iteration */
  goto subgoal_search_loop_non_atomic;
#endif /* MODE_GLOBAL_TRIE_LOOP */

  do {
    if (IsVarTerm(t)) {
      if (IsTableVarTerm(t)) {
	t = MakeTableVarTerm(VarIndexOfTerm(t));
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t);
      } else {
	if (subs_arity == MAX_TABLE_VARS)
	  Yap_Error(INTERNAL_ERROR, TermNil, "subgoal_search_loop: MAX_TABLE_VARS exceeded");
	STACK_PUSH_UP(t, stack_vars);
	*((CELL *)t) = GLOBAL_table_var_enumerator(subs_arity);
	t = MakeTableVarTerm(subs_arity);
	subs_arity = subs_arity + 1;
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t);
      }
    } else if (IsAtomOrIntTerm(t)) {
      SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t);
#ifdef MODE_TERMS_LOOP
    } else {
      gt_node_ptr entry_node;
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
      entry_node = subgoal_search_global_trie_terms_loop(t, &subs_arity, &stack_vars, stack_terms);
#else
      entry_node = subgoal_search_global_trie_loop(t, &subs_arity, &stack_vars);
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
      current_node = subgoal_trie_check_insert_gt_entry(tab_ent, current_node, (Term) entry_node);
#else /* ! MODE_TERMS_LOOP */
    } else 
#if defined(MODE_GLOBAL_TRIE_LOOP)
      /* for the global trie, it is safe to start here in the first iteration */
      subgoal_search_loop_non_atomic:
#endif /* MODE_GLOBAL_TRIE_LOOP */
#ifdef TRIE_COMPACT_PAIRS
    if (IsPairTerm(t)) {
      CELL *aux_pair = RepPair(t);
      if (aux_pair == PairTermMark) {
	t = STACK_POP_DOWN(stack_terms);
	if (IsPairTerm(t)) {
	  aux_pair = RepPair(t);
	  t = Deref(aux_pair[1]);
	  if (t == TermNil) {
	    SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairEndList);
	  } else {
	    /* AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);                   */
	    /* AUX_STACK_CHECK_EXPAND is not necessary here because the situation of pushing **
	    ** up 3 terms has already initially checked for the CompactPairInit term         */
	    STACK_PUSH_UP(t, stack_terms);
	    STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
	  }
	  STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
	} else {
	  SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairEndTerm);
	  STACK_PUSH_UP(t, stack_terms);
	}
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
      } else if (current_node != GLOBAL_root_gt) {
	gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(t, &subs_arity, &stack_vars, stack_terms);
	current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
      } else {
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairInit);
	t = Deref(aux_pair[1]);
	if (t == TermNil) {
	  SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairEndList);
	} else {
	  AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);
	  STACK_PUSH_UP(t, stack_terms);
	  STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
	}
	STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
      }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    } else if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(t, &subs_arity, &stack_vars, stack_terms);
      current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
#else /* ! TRIE_COMPACT_PAIRS */
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(t, &subs_arity, &stack_vars, stack_terms);
      current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
    } else 
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
    if (IsPairTerm(t)) {
      CELL *aux_pair = RepPair(t);
      SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsPair(NULL));
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
      STACK_PUSH_UP(Deref(aux_pair[1]), stack_terms);
      STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = FunctorOfTerm(t);
      if (f == FunctorDouble) {
	volatile Float dbl = FloatOfTerm(t);
	volatile Term *t_dbl = (Term *)((void *) &dbl);
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t_dbl[1]);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t_dbl[0]);
#ifdef MODE_GLOBAL_TRIE_LOOP
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
#endif /* MODE_GLOBAL_TRIE_LOOP */
      } else if (f == FunctorLongInt) {
	Int li = LongIntOfTerm(t);
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, li);
#ifdef MODE_GLOBAL_TRIE_LOOP
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
#endif /* MODE_GLOBAL_TRIE_LOOP */
      } else if (f == FunctorDBRef) {
	Yap_Error(INTERNAL_ERROR, TermNil, "subgoal_search_loop: unsupported type tag FunctorDBRef");
      } else if (f == FunctorBigInt) {
	Yap_Error(INTERNAL_ERROR, TermNil, "subgoal_search_loop: unsupported type tag FunctorBigInt");
      } else {
	int i;
	CELL *aux_appl = RepAppl(t);
	SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
	AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + ArityOfFunctor(f) - 1);
	for (i = ArityOfFunctor(f); i >= 1; i--)
	  STACK_PUSH_UP(Deref(aux_appl[i]), stack_terms);
      }
    } else {
      Yap_Error(INTERNAL_ERROR, TermNil, "subgoal_search_loop: unknown type tag");
#endif /* MODE_TERMS_LOOP */
    }
    t = STACK_POP_DOWN(stack_terms);
  } while (t);
  
  *subs_arity_ptr = subs_arity;
  *stack_vars_ptr = stack_vars;
  return current_node;
}
#endif /* INCLUDE_SUBGOAL_SEARCH_LOOP */



/************************************************************************
**               answer_search(_global_trie)(_terms)_loop              **
************************************************************************/

#ifdef INCLUDE_ANSWER_SEARCH_LOOP
#ifdef MODE_GLOBAL_TRIE_LOOP
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static inline gt_node_ptr answer_search_global_trie_terms_loop(Term t, int *vars_arity_ptr, CELL *stack_terms) {
#else
static inline gt_node_ptr answer_search_global_trie_loop(Term t, int *vars_arity_ptr) {
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
#else
#ifdef MODE_TERMS_LOOP
static inline ans_node_ptr answer_search_terms_loop(sg_fr_ptr sg_fr, ans_node_ptr current_node, Term t, int *vars_arity_ptr) {
#else
static inline ans_node_ptr answer_search_loop(sg_fr_ptr sg_fr, ans_node_ptr current_node, Term t, int *vars_arity_ptr) {
#endif /* MODE_TERMS_LOOP */
#endif /* MODE_GLOBAL_TRIE_LOOP */
/************************************************************************
                   ===========
                   |         |
                   |   ...   |
                   |         |
                   -----------
           TR -->  |  VAR_0  |  <-- stack_vars_base
                   -----------           *
                   |   ...   |          /|\
                   -----------           |   vars_arity (N+1)
                   |  VAR_N  |          \|/
                   -----------           *
                   |         |  <-- stack_terms_limit
                   -----------
                   |         |
                   |   ...   |
                   |         |
                   ----------|
                   |  TERM_N |  <-- stack_terms
                   ----------|           *
                   |   ...   |          /|\
                   ----------|           |
                   |  TERM_1 |           |
                   ----------|           |
                   |   NULL  |          \|/
                   ===========           *
 Yap_TrailTop -->  |         |
                   -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  gt_node_ptr current_node = GLOBAL_root_gt;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  int vars_arity = *vars_arity_ptr;
#if ! defined(MODE_GLOBAL_TRIE_LOOP) || ! defined(GLOBAL_TRIE_FOR_SUBTERMS)
  CELL *stack_terms = (CELL *) Yap_TrailTop;
#endif /* ! MODE_GLOBAL_TRIE_LOOP || ! GLOBAL_TRIE_FOR_SUBTERMS */
  CELL *stack_vars_base = (CELL *) TR;
#define stack_terms_limit (stack_vars_base + vars_arity)
#ifdef TRIE_COMPACT_PAIRS
  int in_pair = 0;
#else
#define in_pair 0
#endif /* TRIE_COMPACT_PAIRS */
  AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);  /* + 1 because initially we stiil haven't done any STACK_POP_DOWN */
  STACK_PUSH_UP(NULL, stack_terms);

#if defined(MODE_GLOBAL_TRIE_LOOP)
  /* for the global trie, it is safe to skip the IsVarTerm() and IsAtomOrIntTerm() tests in the first iteration */
  goto answer_search_loop_non_atomic;
#endif /* MODE_GLOBAL_TRIE_LOOP */

  do {
    if (IsVarTerm(t)) {
      t = Deref(t);
      if (IsTableVarTerm(t)) {
	t = MakeTableVarTerm(VarIndexOfTerm(t));
	 ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t, _trie_retry_val + in_pair);
      } else {
	if (vars_arity == MAX_TABLE_VARS)
	  Yap_Error(INTERNAL_ERROR, TermNil, "answer_search_loop: MAX_TABLE_VARS exceeded");
	stack_vars_base[vars_arity] = t;
	*((CELL *)t) = GLOBAL_table_var_enumerator(vars_arity);
	t = MakeTableVarTerm(vars_arity);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t, _trie_retry_var + in_pair);
	vars_arity = vars_arity + 1;
      }
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsAtomOrIntTerm(t)) {
      ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t, _trie_retry_atom + in_pair);
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
#ifdef MODE_TERMS_LOOP
    } else {
      gt_node_ptr entry_node;
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
      entry_node = answer_search_global_trie_terms_loop(t, &vars_arity, stack_terms);
#else
      entry_node = answer_search_global_trie_loop(t, &vars_arity);
#endif /*  GLOBAL_TRIE_FOR_SUBTERMS */
      current_node = answer_trie_check_insert_gt_entry(sg_fr, current_node, (Term) entry_node, _trie_retry_gterm + in_pair);
#else /* ! MODE_TERMS_LOOP */
    } else 
#if defined(MODE_GLOBAL_TRIE_LOOP)
      /* for the global trie, it is safe to start here in the first iteration */
      answer_search_loop_non_atomic:
#endif /* MODE_GLOBAL_TRIE_LOOP */
#ifdef TRIE_COMPACT_PAIRS
    if (IsPairTerm(t)) {
      CELL *aux_pair = RepPair(t);
      if (aux_pair == PairTermMark) {
	t = STACK_POP_DOWN(stack_terms);
	if (IsPairTerm(t)) {
	  aux_pair = RepPair(t);
	  t = Deref(aux_pair[1]);
	  if (t == TermNil) {
	     ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndList, _trie_retry_pair);
	  } else {
	    /* AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);                   */
	    /* AUX_STACK_CHECK_EXPAND is not necessary here because the situation of pushing **
	    ** up 3 terms has already initially checked for the CompactPairInit term         */
	    STACK_PUSH_UP(t, stack_terms);
	    STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
	    in_pair = 4;
	  }
	  STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
	} else {
	  ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndTerm, _trie_retry_null);
	  STACK_PUSH_UP(t, stack_terms);
	}
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
      } else if (current_node != GLOBAL_root_gt) {
	gt_node_ptr entry_node = answer_search_global_trie_terms_loop(t, &vars_arity, stack_terms);
	current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
      } else {
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairInit, _trie_retry_null + in_pair);
	t = Deref(aux_pair[1]);
	if (t == TermNil) {
	   ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndList, _trie_retry_pair);
	   in_pair = 0;
	} else {
	  AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);
	  STACK_PUSH_UP(t, stack_terms);
	  STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
	  in_pair = 4;
	}
	STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
      }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    } else if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = answer_search_global_trie_terms_loop(t, &vars_arity, stack_terms);
      current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
#else /* ! TRIE_COMPACT_PAIRS */
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = answer_search_global_trie_terms_loop(t, &vars_arity, stack_terms);
      current_node = global_trie_check_insert_gt_entry(current_node, (Term) entry_node);
    } else 
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
    if (IsPairTerm(t)) {
      CELL *aux_pair = RepPair(t);
      ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsPair(NULL), _trie_retry_pair);
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
      STACK_PUSH_UP(Deref(aux_pair[1]), stack_terms);
      STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = FunctorOfTerm(t);
      if (f == FunctorDouble) {
	volatile Float dbl = FloatOfTerm(t);
	volatile Term *t_dbl = (Term *)((void *) &dbl);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f), _trie_retry_null + in_pair);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t_dbl[1], _trie_retry_extension);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t_dbl[0], _trie_retry_extension);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f), _trie_retry_double);
      } else if (f == FunctorLongInt) {
	Int li = LongIntOfTerm (t);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f), _trie_retry_null + in_pair);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, li, _trie_retry_extension);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f), _trie_retry_longint);
      } else if (f == FunctorDBRef) {
	Yap_Error(INTERNAL_ERROR, TermNil, "answer_search_loop: unsupported type tag FunctorDBRef");
      } else if (f == FunctorBigInt) {
	Yap_Error(INTERNAL_ERROR, TermNil, "answer_search_loop: unsupported type tag FunctorBigInt");
      } else {
	int i;
	CELL *aux_appl = RepAppl(t);
	ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f), _trie_retry_appl + in_pair);
	AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + ArityOfFunctor(f) - 1);
	for (i = ArityOfFunctor(f); i >= 1; i--)
	  STACK_PUSH_UP(Deref(aux_appl[i]), stack_terms);
      }
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
    } else {
      Yap_Error(INTERNAL_ERROR, TermNil, "answer_search_loop: unknown type tag");
#endif /* MODE_TERMS_LOOP */
    }
    t = STACK_POP_DOWN(stack_terms);
  } while (t);

  *vars_arity_ptr = vars_arity;
  return current_node;

#undef stack_terms_limit
#ifndef TRIE_COMPACT_PAIRS
#undef in_pair
#endif /* TRIE_COMPACT_PAIRS */
}
#endif /* INCLUDE_ANSWER_SEARCH_LOOP */



/************************************************************************
**                   load_(answer|substitution)_loop                   **
************************************************************************/

#ifdef INCLUDE_LOAD_ANSWER_LOOP
#ifdef MODE_GLOBAL_TRIE_LOOP
static inline CELL *load_substitution_loop(gt_node_ptr current_node, int *vars_arity_ptr, CELL *stack_terms) {
#else
static inline CELL *load_answer_loop(ans_node_ptr current_node) {
#endif /* MODE_GLOBAL_TRIE_LOOP */
/************************************************************************
                   ===========
                   |         |
                   |   ...   |
                   |         |
                   -----------
           TR -->  |  VAR_0  |  <-- stack_vars_base
                   -----------           *
                   |   ...   |          /|\
                   -----------           |  vars_arity (N+1)
                   |  VAR_N  |          \|/
                   -----------           *
                   |         |  <-- stack_terms_limit
                   -----------
                   |         |
                   |   ...   |
                   |         |
                   ----------|
                   |  TERM_N |  <-- stack_terms
                   ----------|           *
                   |   ...   |          /|\
                   ----------|           |  stack_terms_pair_offset (TRIE_COMPACT_PAIRS)
                   |  TERM_1 |          \|/
                   ===========           *
 Yap_TrailTop -->  |         |  <-- stack_terms_base (TRIE_COMPACT_PAIRS)
                   -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  int vars_arity = *vars_arity_ptr;
#else
  int vars_arity = 0;
  CELL *stack_terms = (CELL *) Yap_TrailTop;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  CELL *stack_vars_base = (CELL *) TR;
#define stack_terms_limit (stack_vars_base + vars_arity)
#ifdef TRIE_COMPACT_PAIRS
#define stack_terms_base ((CELL *) Yap_TrailTop)
  int stack_terms_pair_offset = 0;
#endif /* TRIE_COMPACT_PAIRS */
  Term t = TrNode_entry(current_node);
#ifdef MODE_GLOBAL_TRIE_LOOP
  current_node = TrNode_parent(current_node);
#else
  current_node = UNTAG_ANSWER_LEAF_NODE(TrNode_parent(current_node));
#endif /* MODE_GLOBAL_TRIE_LOOP */

  do {
    if (IsVarTerm(t)) {
#if ! defined(MODE_GLOBAL_TRIE_LOOP) || defined(GLOBAL_TRIE_FOR_SUBTERMS)
      if (t > VarIndexOfTableTerm(MAX_TABLE_VARS)) {
	stack_terms = load_substitution_loop((gt_node_ptr) t, &vars_arity, stack_terms);
      } else 
#endif /* ! MODE_GLOBAL_TRIE_LOOP || GLOBAL_TRIE_FOR_SUBTERMS */
      { int var_index = VarIndexOfTableTerm(t);
	AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit - vars_arity + var_index + 1);
	if (var_index >= vars_arity) {
	  while (vars_arity < var_index)
	    stack_vars_base[vars_arity++] = 0; 
	  stack_vars_base[vars_arity++] = MkVarTerm(); 
	} else if (stack_vars_base[var_index] == 0)
	  stack_vars_base[var_index] = MkVarTerm(); 
	STACK_PUSH_UP(stack_vars_base[var_index], stack_terms);
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
	stack_terms_pair_offset = (int) STACK_POP_DOWN(stack_terms);
	STACK_PUSH_UP(tail, stack_terms);
      } else {  /* CompactPairEndList / CompactPairEndTerm */
	Term last;
	AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
	last = STACK_POP_DOWN(stack_terms);
	STACK_PUSH_UP(stack_terms_pair_offset, stack_terms);
	stack_terms_pair_offset = (int) (stack_terms_base - stack_terms);
	if (t == CompactPairEndList)
	  STACK_PUSH_UP(TermNil, stack_terms);
	STACK_PUSH_UP(last, stack_terms);
      }
#else /* ! TRIE_COMPACT_PAIRS */
      Term head = STACK_POP_DOWN(stack_terms);
      Term tail = STACK_POP_DOWN(stack_terms);
      t = MkPairTerm(head, tail);
      STACK_PUSH_UP(t, stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = (Functor) RepAppl(t);
      if (f == FunctorDouble) {
	volatile Float dbl;
	volatile Term *t_dbl = (Term *)((void *) &dbl);
	t = TrNode_entry(current_node);
	current_node = TrNode_parent(current_node);
	t_dbl[0] = t;
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
	t = TrNode_entry(current_node);
	current_node = TrNode_parent(current_node);
	t_dbl[1] = t;
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
	current_node = TrNode_parent(current_node);
	t = MkFloatTerm(dbl);
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

#ifdef MODE_GLOBAL_TRIE_LOOP
  *vars_arity_ptr = vars_arity;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  return stack_terms;

#undef stack_terms_limit
#ifdef TRIE_COMPACT_PAIRS
#undef stack_terms_base
#endif /* TRIE_COMPACT_PAIRS */
}
#endif /* INCLUDE_LOAD_ANSWER_LOOP */
