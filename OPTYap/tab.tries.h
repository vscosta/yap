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

#ifdef MODE_GLOBAL_TRIE_ENTRY
#define INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY)                                 \
  {                                                                            \
    register gt_node_ptr entry_node = (gt_node_ptr)(ENTRY);                    \
    TrNode_child(entry_node) =                                                 \
        (gt_node_ptr)((UInt)TrNode_child(entry_node) + 1);                     \
  }
#define NEW_SUBGOAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)                \
  INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                                      \
  new_subgoal_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#define NEW_ANSWER_TRIE_NODE(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)          \
  INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                                      \
  new_answer_trie_node(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)
#define NEW_GLOBAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)                 \
  INCREMENT_GLOBAL_TRIE_REFERENCE(ENTRY);                                      \
  new_global_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#else
#define NEW_SUBGOAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)                \
  new_subgoal_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#define NEW_ANSWER_TRIE_NODE(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)          \
  new_answer_trie_node(NODE, INSTR, ENTRY, CHILD, PARENT, NEXT)
#define NEW_GLOBAL_TRIE_NODE(NODE, ENTRY, CHILD, PARENT, NEXT)                 \
  new_global_trie_node(NODE, ENTRY, CHILD, PARENT, NEXT)
#endif /* MODE_GLOBAL_TRIE_ENTRY */

#ifdef MODE_GLOBAL_TRIE_LOOP
#define SUBGOAL_CHECK_INSERT_ENTRY(TAB_ENT, NODE, ENTRY)                       \
  NODE = global_trie_check_insert_entry(NODE, ENTRY PASS_REGS)
#define ANSWER_CHECK_INSERT_ENTRY(SG_FR, NODE, ENTRY, INSTR)                   \
  NODE = global_trie_check_insert_entry(NODE, ENTRY PASS_REGS)
#else
#define SUBGOAL_CHECK_INSERT_ENTRY(TAB_ENT, NODE, ENTRY)                       \
  NODE = subgoal_trie_check_insert_entry(TAB_ENT, NODE, ENTRY PASS_REGS)
#define ANSWER_CHECK_INSERT_ENTRY(SG_FR, NODE, ENTRY, INSTR)                   \
  NODE = answer_trie_check_insert_entry(SG_FR, NODE, ENTRY, INSTR PASS_REGS)
#endif /* MODE_GLOBAL_TRIE_LOOP */

#ifdef INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
#define ANSWER_SAFE_INSERT_ENTRY(NODE, ENTRY, INSTR)                           \
  {                                                                            \
    ans_node_ptr new_node;                                                     \
    NEW_ANSWER_TRIE_NODE(new_node, INSTR, ENTRY, NULL, NODE, NULL);            \
    TrNode_child(NODE) = new_node;                                             \
    NODE = new_node;                                                           \
  }
#ifdef THREADS
#define INVALIDATE_ANSWER_TRIE_NODE(NODE, SG_FR)                               \
  TrNode_next(NODE) = SgFr_invalid_chain(SG_FR);                               \
  SgFr_invalid_chain(SG_FR) = NODE
#else
#define INVALIDATE_ANSWER_TRIE_NODE(NODE, SG_FR) FREE_ANSWER_TRIE_NODE(NODE)
#endif /* THREADS */
#define INVALIDATE_ANSWER_TRIE_LEAF_NODE(NODE, SG_FR)                          \
  TAG_AS_ANSWER_INVALID_NODE(NODE);                                            \
  TrNode_next(NODE) = SgFr_invalid_chain(SG_FR);                               \
  SgFr_invalid_chain(SG_FR) = NODE
#endif /* INCLUDE_ANSWER_SEARCH_MODE_DIRECTED */

/************************************************************************
**                 subgoal_trie_check_insert_(gt)_entry                **
************************************************************************/

#ifdef INCLUDE_SUBGOAL_TRIE_CHECK_INSERT
#ifndef SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL /* SUBGOAL_TRIE_LOCK_AT_ENTRY_LEVEL   \
                                            || SUBGOAL_TRIE_LOCK_AT_NODE_LEVEL \
                                            || ! YAPOR */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline sg_node_ptr
subgoal_trie_check_insert_gt_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node,
                                   Term t USES_REGS) {
#else
static inline sg_node_ptr
subgoal_trie_check_insert_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node,
                                Term t USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  sg_node_ptr child_node;

  LOCK_SUBGOAL_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_SUBGOAL_NODE(parent_node);
    return child_node;
  }

  if (!IS_SUBGOAL_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_SUBGOAL_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node,
                          TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_hash_ptr hash;
      sg_node_ptr chain_node, next_node, *bucket;
      new_subgoal_trie_hash(hash, count_nodes, tab_ent);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (sg_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_SUBGOAL_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    sg_hash_ptr hash;
    sg_node_ptr *bucket;
    int count_nodes = 0;
    hash = (sg_hash_ptr)child_node;
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, Hash_num_buckets(hash));
    child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_SUBGOAL_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET &&
        Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      sg_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
          *new_hash_buckets;
      int num_buckets;
      num_buckets = Hash_num_buckets(hash) * 2;
      ALLOC_BUCKETS(new_hash_buckets, num_buckets);
      old_hash_buckets = Hash_buckets(hash);
      old_bucket = old_hash_buckets + Hash_num_buckets(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = new_hash_buckets +
                     HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != old_hash_buckets);
      Hash_buckets(hash) = new_hash_buckets;
      Hash_num_buckets(hash) = num_buckets;
      FREE_BUCKETS(old_hash_buckets);
    }
    UNLOCK_SUBGOAL_NODE(parent_node);
    return child_node;
  }
}
#else /* SUBGOAL_TRIE_LOCK_AT_WRITE_LEVEL */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline sg_node_ptr
subgoal_trie_check_insert_gt_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node,
                                   Term t USES_REGS) {
#else
static inline sg_node_ptr
subgoal_trie_check_insert_entry(tab_ent_ptr tab_ent, sg_node_ptr parent_node,
                                Term t USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  sg_node_ptr child_node;
  sg_hash_ptr hash;

  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_SUBGOAL_NODE(parent_node);
    if (TrNode_child(parent_node)) {
      sg_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_TRIE_HASH(chain_node)) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_SUBGOAL_NODE(parent_node);
        hash = (sg_hash_ptr)chain_node;
        goto subgoal_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_SUBGOAL_NODE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node,
                            TrNode_child(parent_node));
    } else {
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = child_node;
    UNLOCK_SUBGOAL_NODE(parent_node);
    return child_node;
  }

  if (!IS_SUBGOAL_TRIE_HASH(child_node)) {
    sg_node_ptr first_node = child_node;
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t)
        return child_node;
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_SUBGOAL_NODE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      sg_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_SUBGOAL_TRIE_HASH(chain_node)) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_SUBGOAL_NODE(parent_node);
        hash = (sg_hash_ptr)chain_node;
        goto subgoal_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
          FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_SUBGOAL_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node,
                            TrNode_child(parent_node));
    } else {
      NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
    }
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      sg_node_ptr chain_node, next_node, *bucket;
      new_subgoal_trie_hash(hash, count_nodes, tab_ent);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (sg_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_SUBGOAL_NODE(parent_node);
    return child_node;
  }

  hash = (sg_hash_ptr)child_node;
subgoal_trie_hash : { /* trie nodes with hashing */
  sg_node_ptr *bucket, first_node;
  int num_buckets, count_nodes = 0;

  do {
    num_buckets = Hash_num_buckets(hash);
    // __sync_synchronize();
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, num_buckets);
    first_node = child_node = *bucket;
  } while (num_buckets != Hash_num_buckets(hash));
  while (child_node) {
    if (TrNode_entry(child_node) == t)
      return child_node;
    count_nodes++;
    child_node = TrNode_next(child_node);
  }
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
  NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
  LOCK_SUBGOAL_NODE(parent_node);
  if (num_buckets != Hash_num_buckets(hash)) {
/* the hash has been expanded */
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
    FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
    UNLOCK_SUBGOAL_NODE(parent_node);
    goto subgoal_trie_hash;
  }
  if (first_node != *bucket) {
    sg_node_ptr chain_node = *bucket;
    do {
      if (TrNode_entry(chain_node) == t) {
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_SUBGOAL_TRIE_NODE(child_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_SUBGOAL_NODE(parent_node);
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    } while (chain_node != first_node);
#ifdef SUBGOAL_TRIE_ALLOC_BEFORE_CHECK
    TrNode_next(child_node) = *bucket;
#else
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
  } else {
    NEW_SUBGOAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* SUBGOAL_TRIE_ALLOC_BEFORE_CHECK */
  }
  *bucket = child_node;
  Hash_num_nodes(hash)++;
  count_nodes++;
  if (count_nodes >= MAX_NODES_PER_BUCKET &&
      Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
    /* expand current hash */
    sg_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
        *new_hash_buckets;
    num_buckets = Hash_num_buckets(hash) * 2;
    ALLOC_BUCKETS(new_hash_buckets, num_buckets);
    old_hash_buckets = Hash_buckets(hash);
    old_bucket = old_hash_buckets + Hash_num_buckets(hash);
    do {
      if (*--old_bucket) {
        chain_node = *old_bucket;
        do {
          bucket = new_hash_buckets +
                   HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
          next_node = TrNode_next(chain_node);
          TrNode_next(chain_node) = *bucket;
          *bucket = chain_node;
          chain_node = next_node;
        } while (chain_node);
      }
    } while (old_bucket != old_hash_buckets);
    Hash_buckets(hash) = new_hash_buckets;
    Hash_num_buckets(hash) = num_buckets;
    FREE_BUCKETS(old_hash_buckets);
  }
  UNLOCK_SUBGOAL_NODE(parent_node);
  return child_node;
}
}
#endif /* SUBGOAL_TRIE_LOCK_LEVEL */
#endif /* INCLUDE_SUBGOAL_TRIE_CHECK_INSERT */

/************************************************************************
**                 answer_trie_check_insert_(gt)_entry                 **
************************************************************************/

#ifdef INCLUDE_ANSWER_TRIE_CHECK_INSERT
#ifndef ANSWER_TRIE_LOCK_AT_WRITE_LEVEL /* ANSWER_TRIE_LOCK_AT_ENTRY_LEVEL ||  \
                                           ANSWER_TRIE_LOCK_AT_NODE_LEVEL || ! \
                                           YAPOR */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline ans_node_ptr
answer_trie_check_insert_gt_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node,
                                  Term t, int instr USES_REGS) {
#else
static inline ans_node_ptr
answer_trie_check_insert_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node,
                               Term t, int instr USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  ans_node_ptr child_node;

  TABLING_ERROR_CHECKING(answer_trie_check_insert_(gt) _entry,
                         IS_ANSWER_LEAF_NODE(parent_node));
  LOCK_ANSWER_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_ANSWER_NODE(parent_node);
    return child_node;
  }

  if (!IS_ANSWER_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_ANSWER_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node,
                         TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_hash_ptr hash;
      ans_node_ptr chain_node, next_node, *bucket;
      new_answer_trie_hash(hash, count_nodes, sg_fr);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (ans_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_ANSWER_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    ans_hash_ptr hash;
    ans_node_ptr *bucket;
    int count_nodes = 0;
    hash = (ans_hash_ptr)child_node;
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, Hash_num_buckets(hash));
    child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_ANSWER_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET &&
        Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      ans_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
          *new_hash_buckets;
      int num_buckets;
      num_buckets = Hash_num_buckets(hash) * 2;
      ALLOC_BUCKETS(new_hash_buckets, num_buckets);
      old_hash_buckets = Hash_buckets(hash);
      old_bucket = old_hash_buckets + Hash_num_buckets(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = new_hash_buckets +
                     HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != old_hash_buckets);
      Hash_buckets(hash) = new_hash_buckets;
      Hash_num_buckets(hash) = num_buckets;
      FREE_BUCKETS(old_hash_buckets);
    }
    UNLOCK_ANSWER_NODE(parent_node);
    return child_node;
  }
}
#else /* ANSWER_TRIE_LOCK_AT_WRITE_LEVEL */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline ans_node_ptr
answer_trie_check_insert_gt_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node,
                                  Term t, int instr USES_REGS) {
#else
static inline ans_node_ptr
answer_trie_check_insert_entry(sg_fr_ptr sg_fr, ans_node_ptr parent_node,
                               Term t, int instr USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  ans_node_ptr child_node;
  ans_hash_ptr hash;

  TABLING_ERROR_CHECKING(answer_trie_check_insert_(gt) _entry,
                         IS_ANSWER_LEAF_NODE(parent_node));
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_ANSWER_NODE(parent_node);
    if (TrNode_child(parent_node)) {
      ans_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_TRIE_HASH(chain_node)) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_ANSWER_NODE(parent_node);
        hash = (ans_hash_ptr)chain_node;
        goto answer_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_ANSWER_NODE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node,
                           TrNode_child(parent_node));
    } else {
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, NULL);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = child_node;
    UNLOCK_ANSWER_NODE(parent_node);
    return child_node;
  }

  if (!IS_ANSWER_TRIE_HASH(child_node)) {
    ans_node_ptr first_node = child_node;
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t)
        return child_node;
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_ANSWER_NODE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      ans_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_ANSWER_TRIE_HASH(chain_node)) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_ANSWER_NODE(parent_node);
        hash = (ans_hash_ptr)chain_node;
        goto answer_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
          FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_ANSWER_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node,
                           TrNode_child(parent_node));
    } else {
      NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
    }
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      ans_node_ptr chain_node, next_node, *bucket;
      new_answer_trie_hash(hash, count_nodes, sg_fr);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (ans_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_ANSWER_NODE(parent_node);
    return child_node;
  }

  hash = (ans_hash_ptr)child_node;
answer_trie_hash : { /* trie nodes with hashing */
  ans_node_ptr *bucket, first_node;
  int num_buckets, count_nodes = 0;

  do {
    num_buckets = Hash_num_buckets(hash);
    // __sync_synchronize();
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, num_buckets);
    first_node = child_node = *bucket;
  } while (num_buckets != Hash_num_buckets(hash));
  while (child_node) {
    if (TrNode_entry(child_node) == t)
      return child_node;
    count_nodes++;
    child_node = TrNode_next(child_node);
  }
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
  NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
  LOCK_ANSWER_NODE(parent_node);
  if (num_buckets != Hash_num_buckets(hash)) {
/* the hash has been expanded */
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
    FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
    UNLOCK_ANSWER_NODE(parent_node);
    goto answer_trie_hash;
  }
  if (first_node != *bucket) {
    ans_node_ptr chain_node = *bucket;
    do {
      if (TrNode_entry(chain_node) == t) {
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
        FREE_ANSWER_TRIE_NODE(child_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_ANSWER_NODE(parent_node);
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    } while (chain_node != first_node);
#ifdef ANSWER_TRIE_ALLOC_BEFORE_CHECK
    TrNode_next(child_node) = *bucket;
#else
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, *bucket);
  } else {
    NEW_ANSWER_TRIE_NODE(child_node, instr, t, NULL, parent_node, first_node);
#endif /* ANSWER_TRIE_ALLOC_BEFORE_CHECK */
  }
  *bucket = child_node;
  Hash_num_nodes(hash)++;
  count_nodes++;
  if (count_nodes >= MAX_NODES_PER_BUCKET &&
      Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
    /* expand current hash */
    ans_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
        *new_hash_buckets;
    num_buckets = Hash_num_buckets(hash) * 2;
    ALLOC_BUCKETS(new_hash_buckets, num_buckets);
    old_hash_buckets = Hash_buckets(hash);
    old_bucket = old_hash_buckets + Hash_num_buckets(hash);
    do {
      if (*--old_bucket) {
        chain_node = *old_bucket;
        do {
          bucket = new_hash_buckets +
                   HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
          next_node = TrNode_next(chain_node);
          TrNode_next(chain_node) = *bucket;
          *bucket = chain_node;
          chain_node = next_node;
        } while (chain_node);
      }
    } while (old_bucket != old_hash_buckets);
    Hash_buckets(hash) = new_hash_buckets;
    Hash_num_buckets(hash) = num_buckets;
    FREE_BUCKETS(old_hash_buckets);
  }
  UNLOCK_ANSWER_NODE(parent_node);
  return child_node;
}
}
#endif /* ANSWER_TRIE_LOCK_LEVEL */
#endif /* INCLUDE_ANSWER_TRIE_CHECK_INSERT */

/************************************************************************
**                 global_trie_check_insert_(gt)_entry                 **
************************************************************************/

#ifdef INCLUDE_GLOBAL_TRIE_CHECK_INSERT
#ifndef GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL /* GLOBAL_TRIE_LOCK_AT_NODE_LEVEL || ! \
                                           YAPOR */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline gt_node_ptr
global_trie_check_insert_gt_entry(gt_node_ptr parent_node, Term t USES_REGS) {
#else
static inline gt_node_ptr
global_trie_check_insert_entry(gt_node_ptr parent_node, Term t USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  gt_node_ptr child_node;

  LOCK_GLOBAL_NODE(parent_node);
  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
    TrNode_child(parent_node) = child_node;
    UNLOCK_GLOBAL_NODE(parent_node);
    return child_node;
  }

  if (!IS_GLOBAL_TRIE_HASH(child_node)) {
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_GLOBAL_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node,
                         TrNode_child(parent_node));
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      gt_hash_ptr hash;
      gt_node_ptr chain_node, next_node, *bucket;
      new_global_trie_hash(hash, count_nodes);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (gt_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_GLOBAL_NODE(parent_node);
    return child_node;
  }

  { /* trie nodes with hashing */
    gt_hash_ptr hash;
    gt_node_ptr *bucket;
    int count_nodes = 0;
    hash = (gt_hash_ptr)child_node;
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, Hash_num_buckets(hash));
    child_node = *bucket;
    while (child_node) {
      if (TrNode_entry(child_node) == t) {
        UNLOCK_GLOBAL_NODE(parent_node);
        return child_node;
      }
      count_nodes++;
      child_node = TrNode_next(child_node);
    }
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
    *bucket = child_node;
    Hash_num_nodes(hash)++;
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_BUCKET &&
        Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
      /* expand current hash */
      gt_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
          *new_hash_buckets;
      int num_buckets;
      num_buckets = Hash_num_buckets(hash) * 2;
      ALLOC_BUCKETS(new_hash_buckets, num_buckets);
      old_hash_buckets = Hash_buckets(hash);
      old_bucket = old_hash_buckets + Hash_num_buckets(hash);
      do {
        if (*--old_bucket) {
          chain_node = *old_bucket;
          do {
            bucket = new_hash_buckets +
                     HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
            next_node = TrNode_next(chain_node);
            TrNode_next(chain_node) = *bucket;
            *bucket = chain_node;
            chain_node = next_node;
          } while (chain_node);
        }
      } while (old_bucket != old_hash_buckets);
      Hash_buckets(hash) = new_hash_buckets;
      Hash_num_buckets(hash) = num_buckets;
      FREE_BUCKETS(old_hash_buckets);
    }
    UNLOCK_GLOBAL_NODE(parent_node);
    return child_node;
  }
}
#else /* GLOBAL_TRIE_LOCK_AT_WRITE_LEVEL */
#ifdef MODE_GLOBAL_TRIE_ENTRY
static inline gt_node_ptr
global_trie_check_insert_gt_entry(gt_node_ptr parent_node, Term t USES_REGS) {
#else
static inline gt_node_ptr
global_trie_check_insert_entry(gt_node_ptr parent_node, Term t USES_REGS) {
#endif /* MODE_GLOBAL_TRIE_ENTRY */
  gt_node_ptr child_node;
  gt_hash_ptr hash;

  child_node = TrNode_child(parent_node);
  if (child_node == NULL) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_GLOBAL_NODE(parent_node);
    if (TrNode_child(parent_node)) {
      gt_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_GLOBAL_TRIE_HASH(chain_node)) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_GLOBAL_NODE(parent_node);
        hash = (gt_hash_ptr)chain_node;
        goto global_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
          FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_GLOBAL_NODE(parent_node);
          return chain_node;
        }
        chain_node = TrNode_next(chain_node);
      } while (chain_node);
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node,
                           TrNode_child(parent_node));
    } else {
      NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, NULL);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
    }
    TrNode_child(parent_node) = child_node;
    UNLOCK_GLOBAL_NODE(parent_node);
    return child_node;
  }

  if (!IS_GLOBAL_TRIE_HASH(child_node)) {
    gt_node_ptr first_node = child_node;
    int count_nodes = 0;
    do {
      if (TrNode_entry(child_node) == t)
        return child_node;
      count_nodes++;
      child_node = TrNode_next(child_node);
    } while (child_node);
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
    LOCK_GLOBAL_NODE(parent_node);
    if (first_node != TrNode_child(parent_node)) {
      gt_node_ptr chain_node = TrNode_child(parent_node);
      if (IS_GLOBAL_TRIE_HASH(chain_node)) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_GLOBAL_NODE(parent_node);
        hash = (gt_hash_ptr)chain_node;
        goto global_trie_hash;
      }
      do {
        if (TrNode_entry(chain_node) == t) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
          FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
          UNLOCK_GLOBAL_NODE(parent_node);
          return chain_node;
        }
        count_nodes++;
        chain_node = TrNode_next(chain_node);
      } while (chain_node != first_node);
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
      TrNode_next(child_node) = TrNode_child(parent_node);
#else
      NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node,
                           TrNode_child(parent_node));
    } else {
      NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
    }
    count_nodes++;
    if (count_nodes >= MAX_NODES_PER_TRIE_LEVEL) {
      /* alloc a new hash */
      gt_node_ptr chain_node, next_node, *bucket;
      new_global_trie_hash(hash, count_nodes);
      chain_node = child_node;
      do {
        bucket = Hash_buckets(hash) +
                 HASH_ENTRY(TrNode_entry(chain_node), BASE_HASH_BUCKETS);
        next_node = TrNode_next(chain_node);
        TrNode_next(chain_node) = *bucket;
        *bucket = chain_node;
        chain_node = next_node;
      } while (chain_node);
      TrNode_child(parent_node) = (gt_node_ptr)hash;
    } else {
      TrNode_child(parent_node) = child_node;
    }
    UNLOCK_GLOBAL_NODE(parent_node);
    return child_node;
  }

  hash = (gt_hash_ptr)child_node;
global_trie_hash : { /* trie nodes with hashing */
  gt_node_ptr *bucket, first_node;
  int num_buckets, count_nodes = 0;

  do {
    num_buckets = Hash_num_buckets(hash);
    // __sync_synchronize();
    bucket = Hash_buckets(hash) + HASH_ENTRY(t, num_buckets);
    first_node = child_node = *bucket;
  } while (num_buckets != Hash_num_buckets(hash));
  while (child_node) {
    if (TrNode_entry(child_node) == t)
      return child_node;
    count_nodes++;
    child_node = TrNode_next(child_node);
  }
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
  NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
  LOCK_GLOBAL_NODE(parent_node);
  if (num_buckets != Hash_num_buckets(hash)) {
/* the hash has been expanded */
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
    FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
    UNLOCK_GLOBAL_NODE(parent_node);
    goto global_trie_hash;
  }
  if (first_node != *bucket) {
    gt_node_ptr chain_node = *bucket;
    do {
      if (TrNode_entry(chain_node) == t) {
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
        FREE_GLOBAL_TRIE_NODE(child_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
        UNLOCK_GLOBAL_NODE(parent_node);
        return chain_node;
      }
      count_nodes++;
      chain_node = TrNode_next(chain_node);
    } while (chain_node != first_node);
#ifdef GLOBAL_TRIE_ALLOC_BEFORE_CHECK
    TrNode_next(child_node) = *bucket;
#else
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, *bucket);
  } else {
    NEW_GLOBAL_TRIE_NODE(child_node, t, NULL, parent_node, first_node);
#endif /* GLOBAL_TRIE_ALLOC_BEFORE_CHECK */
  }
  *bucket = child_node;
  Hash_num_nodes(hash)++;
  count_nodes++;
  if (count_nodes >= MAX_NODES_PER_BUCKET &&
      Hash_num_nodes(hash) > Hash_num_buckets(hash)) {
    /* expand current hash */
    gt_node_ptr chain_node, next_node, *old_bucket, *old_hash_buckets,
        *new_hash_buckets;
    num_buckets = Hash_num_buckets(hash) * 2;
    ALLOC_BUCKETS(new_hash_buckets, num_buckets);
    old_hash_buckets = Hash_buckets(hash);
    old_bucket = old_hash_buckets + Hash_num_buckets(hash);
    do {
      if (*--old_bucket) {
        chain_node = *old_bucket;
        do {
          bucket = new_hash_buckets +
                   HASH_ENTRY(TrNode_entry(chain_node), num_buckets);
          next_node = TrNode_next(chain_node);
          TrNode_next(chain_node) = *bucket;
          *bucket = chain_node;
          chain_node = next_node;
        } while (chain_node);
      }
    } while (old_bucket != old_hash_buckets);
    Hash_buckets(hash) = new_hash_buckets;
    Hash_num_buckets(hash) = num_buckets;
    FREE_BUCKETS(old_hash_buckets);
  }
  UNLOCK_GLOBAL_NODE(parent_node);
  return child_node;
}
}
#endif /* GLOBAL_TRIE_LOCK_LEVEL */
#endif /* INCLUDE_GLOBAL_TRIE_CHECK_INSERT */

/************************************************************************
**             subgoal_search(_global_trie)(_terms)_loop               **
************************************************************************/

#ifdef INCLUDE_SUBGOAL_SEARCH_LOOP
#ifdef MODE_GLOBAL_TRIE_LOOP
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
static inline gt_node_ptr
subgoal_search_global_trie_terms_loop(Term t, int *subs_arity_ptr,
                                      CELL **stack_vars_ptr,
                                      CELL *stack_terms USES_REGS) {
#else
static inline gt_node_ptr
subgoal_search_global_trie_loop(Term t, int *subs_arity_ptr,
                                CELL **stack_vars_ptr USES_REGS) {
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
#else
#ifdef MODE_TERMS_LOOP
static inline sg_node_ptr
subgoal_search_terms_loop(tab_ent_ptr tab_ent, sg_node_ptr current_node, Term t,
                          int *subs_arity_ptr,
                          CELL **stack_vars_ptr USES_REGS) {
#else
static inline sg_node_ptr subgoal_search_loop(tab_ent_ptr tab_ent,
                                              sg_node_ptr current_node, Term t,
                                              int *subs_arity_ptr,
                                              CELL **stack_vars_ptr USES_REGS) {
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
 LOCAL_TrailTop -->  |         |
                     -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  gt_node_ptr current_node = GLOBAL_root_gt;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  int subs_arity = *subs_arity_ptr;
  CELL *stack_vars = *stack_vars_ptr;
#if !defined(MODE_GLOBAL_TRIE_LOOP) || !defined(GLOBAL_TRIE_FOR_SUBTERMS)
  CELL *stack_terms = (CELL *)LOCAL_TrailTop;
#endif /* ! MODE_GLOBAL_TRIE_LOOP || ! GLOBAL_TRIE_FOR_SUBTERMS */
  CELL *stack_terms_limit = (CELL *)TR;
  AUX_STACK_CHECK_EXPAND(
      stack_terms, stack_terms_limit + 1); /* + 1 because initially we stiil
                                              haven't done any STACK_POP_DOWN */
  STACK_PUSH_UP(NULL, stack_terms);

#if defined(MODE_GLOBAL_TRIE_LOOP)
  /* for the global trie, it is safe to skip the IsVarTerm() and
   * IsAtomOrIntTerm() tests in the first iteration */
  goto subgoal_search_loop_non_atomic;
#endif /* MODE_GLOBAL_TRIE_LOOP */

#ifdef TRIE_RATIONAL_TERMS
  /* Needed structures, variables to support rational terms */
  term_array Ts;
  void *CyclicTerm;
  term_array_init(&Ts, 10);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */

  do {
    if (IsVarTerm(t)) {
      if (IsTableVarTerm(t)) {
        t = MakeTableVarTerm(VarIndexOfTerm(t));
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t);
      } else {
        if (subs_arity == MAX_TABLE_VARS)
          Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                    "subgoal_search_loop: MAX_TABLE_VARS exceeded");
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
      entry_node = subgoal_search_global_trie_terms_loop(
          t, &subs_arity, &stack_vars, stack_terms PASS_REGS);
#else
      entry_node = subgoal_search_global_trie_loop(t, &subs_arity,
                                                   &stack_vars PASS_REGS);
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
      current_node = subgoal_trie_check_insert_gt_entry(
          tab_ent, current_node, (Term)entry_node PASS_REGS);
#else /* ! MODE_TERMS_LOOP */
    } else
#ifdef TRIE_RATIONAL_TERMS
        if (IsRationalTerm(t)) {
      t = STACK_POP_DOWN(stack_terms);
      SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, t);
    } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
#if defined(MODE_GLOBAL_TRIE_LOOP)
    /* for the global trie, it is safe to start here in the first iteration */
    subgoal_search_loop_non_atomic:
#endif /* MODE_GLOBAL_TRIE_LOOP */
#ifdef TRIE_COMPACT_PAIRS
    if (IsPairTerm(t)) {
#ifdef TRIE_RATIONAL_TERMS
      CyclicTerm = NULL;
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
      CELL *aux_pair = RepPair(t);
      if (aux_pair == PairTermMark) {
        t = STACK_POP_DOWN(stack_terms);
#ifdef TRIE_RATIONAL_TERMS
        if (IsPairTerm(t) && !IsRationalTerm(t)) {
          term_array_push(&Ts, (void *)t, (void *)current_node);
#else
        if (IsPairTerm(t)) {
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
          aux_pair = RepPair(t);
          t = Deref(aux_pair[1]);
#ifdef TRIE_RATIONAL_TERMS
          if (IsVarTerm(aux_pair[1]) || IsPairTerm(aux_pair[1])) {
            CyclicTerm = term_array_member(Ts, (void *)t);
          }
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
            STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
              if (t == TermNil) {
            SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node,
                                       CompactPairEndList);
          } else {
            /* AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2); */
            /* AUX_STACK_CHECK_EXPAND is not necessary here because the
            *situation of pushing **
            ** up 3 terms has already initially checked for the CompactPairInit
            *term         */
            STACK_PUSH_UP(t, stack_terms);
            STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
          }
#ifdef TRIE_RATIONAL_TERMS
          CyclicTerm = NULL;
          if (IsVarTerm(aux_pair[0]) || IsPairTerm(aux_pair[0]))
            CyclicTerm = term_array_member(Ts, (void *)Deref(aux_pair[0]));
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
        } else {
          SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairEndTerm);
          STACK_PUSH_UP(t, stack_terms);
        }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
      } else if (current_node != GLOBAL_root_gt) {
        gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(
            t, &subs_arity, &stack_vars, stack_terms PASS_REGS);
        current_node = global_trie_check_insert_gt_entry(
            current_node, (Term)entry_node PASS_REGS);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
      } else {
#ifdef TRIE_RATIONAL_TERMS
        term_array_push(&Ts, (void *)t, (void *)current_node);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairInit);
        t = Deref(aux_pair[1]);
#ifdef TRIE_RATIONAL_TERMS
        if (IsVarTerm(aux_pair[1]) || IsPairTerm(aux_pair[1])) {
          CyclicTerm = term_array_member(Ts, (void *)t);
        }
        if (CyclicTerm != NULL) {
          STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
          STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
        } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            if (t == TermNil) {
          SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, CompactPairEndList);
        } else {
          AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);
          STACK_PUSH_UP(t, stack_terms);
          STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
        }
#ifdef TRIE_RATIONAL_TERMS
        CyclicTerm = NULL;
        if (IsVarTerm(aux_pair[0]) || IsPairTerm(aux_pair[0]))
          CyclicTerm = term_array_member(Ts, (void *)Deref(aux_pair[0]));
        if (CyclicTerm != NULL) {
          STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
        } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
          STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
      }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    } else if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(
          t, &subs_arity, &stack_vars, stack_terms PASS_REGS);
      current_node = global_trie_check_insert_gt_entry(
          current_node, (Term)entry_node PASS_REGS);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
#else  /* ! TRIE_COMPACT_PAIRS */
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
        if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = subgoal_search_global_trie_terms_loop(
          t, &subs_arity, &stack_vars, stack_terms PASS_REGS);
      current_node = global_trie_check_insert_gt_entry(
          current_node, (Term)entry_node PASS_REGS);
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
        union {
          Term t_dbl[sizeof(Float) / sizeof(Term)];
          Float dbl;
        } u;
        u.dbl = FloatOfTerm(t);
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, u.t_dbl[1]);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, u.t_dbl[0]);
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
      } else if (f == FunctorBigInt || f == FunctorString) {
        CELL *new = Yap_HeapStoreOpaqueTerm(t);
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, (CELL) new);
#ifdef MODE_GLOBAL_TRIE_LOOP
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
#endif /* MODE_GLOBAL_TRIE_LOOP */
      } else if (f == FunctorDBRef) {
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "subgoal_search_loop: unsupported type tag FunctorDBRef");
      } else {
#ifdef TRIE_RATIONAL_TERMS
        term_array_push(&Ts, (void *)t, (void *)current_node);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        int i;
        CELL *aux_appl = RepAppl(t);
        SUBGOAL_CHECK_INSERT_ENTRY(tab_ent, current_node, AbsAppl((Term *)f));
        AUX_STACK_CHECK_EXPAND(stack_terms,
                               stack_terms_limit + ArityOfFunctor(f) - 1);
        for (i = ArityOfFunctor(f); i >= 1; i--) {
#ifdef TRIE_RATIONAL_TERMS
          CyclicTerm = NULL;
          if (IsVarTerm(aux_appl[i]) || IsApplTerm(aux_appl[i]))
            CyclicTerm = term_array_member(Ts, (void *)Deref(aux_appl[i]));
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            STACK_PUSH_UP(Deref(aux_appl[i]), stack_terms);
        }
      }
    } else {
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "subgoal_search_loop: unknown type tag");
#endif /* MODE_TERMS_LOOP */
    }
    t = STACK_POP_DOWN(stack_terms);
  } while (t);
#ifdef TRIE_RATIONAL_TERMS
  term_array_free(&Ts);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
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
static inline gt_node_ptr
answer_search_global_trie_terms_loop(Term t, int *vars_arity_ptr,
                                     CELL *stack_terms USES_REGS) {
#else
static inline gt_node_ptr
answer_search_global_trie_loop(Term t, int *vars_arity_ptr USES_REGS) {
#endif /* GLOBAL_TRIE_FOR_SUBTERMS */
#else
#ifdef MODE_TERMS_LOOP
static inline ans_node_ptr
answer_search_terms_loop(sg_fr_ptr sg_fr, ans_node_ptr current_node, Term t,
                         int *vars_arity_ptr USES_REGS) {
#else
static inline ans_node_ptr answer_search_loop(sg_fr_ptr sg_fr,
                                              ans_node_ptr current_node, Term t,
                                              int *vars_arity_ptr USES_REGS) {
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
 LOCAL_TrailTop -->  |         |
                     -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  gt_node_ptr current_node = GLOBAL_root_gt;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  int vars_arity = *vars_arity_ptr;
#if !defined(MODE_GLOBAL_TRIE_LOOP) || !defined(GLOBAL_TRIE_FOR_SUBTERMS)
  CELL *stack_terms = (CELL *)LOCAL_TrailTop;
#endif /* ! MODE_GLOBAL_TRIE_LOOP || ! GLOBAL_TRIE_FOR_SUBTERMS */
  CELL *stack_vars_base = (CELL *)TR;
#define stack_terms_limit (stack_vars_base + vars_arity)
#ifdef TRIE_COMPACT_PAIRS
  int in_pair = 0;
#else
#define in_pair 0
#endif /* TRIE_COMPACT_PAIRS */
  AUX_STACK_CHECK_EXPAND(
      stack_terms, stack_terms_limit + 1); /* + 1 because initially we stiil
                                              haven't done any STACK_POP_DOWN */
  STACK_PUSH_UP(NULL, stack_terms);

#if defined(MODE_GLOBAL_TRIE_LOOP)
  /* for the global trie, it is safe to skip the IsVarTerm() and
   * IsAtomOrIntTerm() tests in the first iteration */
  goto answer_search_loop_non_atomic;
#endif /* MODE_GLOBAL_TRIE_LOOP */

#ifdef TRIE_RATIONAL_TERMS
  term_array Ts;
  void *CyclicTerm;
  term_array_init(&Ts, 10);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */

  do {
    if (IsVarTerm(t)) {
      t = Deref(t);
      if (IsTableVarTerm(t)) {
        t = MakeTableVarTerm(VarIndexOfTerm(t));
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t,
                                  _trie_retry_val + in_pair);
      } else {
        if (vars_arity == MAX_TABLE_VARS)
          Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                    "answer_search_loop: MAX_TABLE_VARS exceeded");
        stack_vars_base[vars_arity] = t;
        *((CELL *)t) = GLOBAL_table_var_enumerator(vars_arity);
        t = MakeTableVarTerm(vars_arity);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t,
                                  _trie_retry_var + in_pair);
        vars_arity = vars_arity + 1;
      }
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsAtomOrIntTerm(t)) {
      ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t,
                                _trie_retry_atom + in_pair);
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
#ifdef MODE_TERMS_LOOP
    } else {
      gt_node_ptr entry_node;
#ifdef GLOBAL_TRIE_FOR_SUBTERMS
      entry_node = answer_search_global_trie_terms_loop(t, &vars_arity,
                                                        stack_terms PASS_REGS);
#else
      entry_node = answer_search_global_trie_loop(t, &vars_arity PASS_REGS);
#endif /*  GLOBAL_TRIE_FOR_SUBTERMS */
      current_node = answer_trie_check_insert_gt_entry(
          sg_fr, current_node, (Term)entry_node,
          _trie_retry_gterm + in_pair PASS_REGS);
#else /* ! MODE_TERMS_LOOP */
    } else
#ifdef TRIE_RATIONAL_TERMS
        if (IsRationalTerm(t)) {
      t = STACK_POP_DOWN(stack_terms);
      ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, t,
                                _trie_retry_var +
                                    in_pair); // TODO create _trie_.._rational
    } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
#if defined(MODE_GLOBAL_TRIE_LOOP)
    /* for the global trie, it is safe to start here in the first iteration */
    answer_search_loop_non_atomic:
#endif /* MODE_GLOBAL_TRIE_LOOP */
#ifdef TRIE_COMPACT_PAIRS
    if (IsPairTerm(t)) {
#ifdef TRIE_RATIONAL_TERMS
      CyclicTerm = NULL;
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
      CELL *aux_pair = RepPair(t);
      if (aux_pair == PairTermMark) {
        t = STACK_POP_DOWN(stack_terms);
#ifdef TRIE_RATIONAL_TERMS
        if (IsPairTerm(t) && !IsRationalTerm(t)) {
          term_array_push(&Ts, (void *)t, (void *)current_node);
#else
        if (IsPairTerm(t)) {
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
          aux_pair = RepPair(t);
          t = Deref(aux_pair[1]);
#ifdef TRIE_RATIONAL_TERMS
          if (IsVarTerm(aux_pair[1]) || IsPairTerm(aux_pair[1])) {
            CyclicTerm = term_array_member(Ts, (void *)t);
          }
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms); // CyclicTerm
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
            STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
            in_pair = 4;
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
              if (t == TermNil) {
            ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndList,
                                      _trie_retry_pair);
          } else {
            /* AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2); */
            /* AUX_STACK_CHECK_EXPAND is not necessary here because the
            *situation of pushing **
            ** up 3 terms has already initially checked for the CompactPairInit
            *term         */
            STACK_PUSH_UP(t, stack_terms);
            STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
            in_pair = 4;
          }
#ifdef TRIE_RATIONAL_TERMS
          CyclicTerm = NULL;
          if (IsVarTerm(aux_pair[0]) || IsPairTerm(aux_pair[0]))
            CyclicTerm = term_array_member(Ts, (void *)Deref(aux_pair[0]));
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
        } else {
          ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndTerm,
                                    _trie_retry_null);
          STACK_PUSH_UP(t, stack_terms);
        }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
      } else if (current_node != GLOBAL_root_gt) {
        gt_node_ptr entry_node = answer_search_global_trie_terms_loop(
            t, &vars_arity, stack_terms PASS_REGS);
        current_node = global_trie_check_insert_gt_entry(
            current_node, (Term)entry_node PASS_REGS);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
      } else {
#ifdef TRIE_RATIONAL_TERMS
        term_array_push(&Ts, (void *)t, (void *)current_node);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairInit,
                                  _trie_retry_null + in_pair);
        t = Deref(aux_pair[1]);
#ifdef TRIE_RATIONAL_TERMS
        if (IsVarTerm(aux_pair[1]) || IsPairTerm(aux_pair[1])) {
          CyclicTerm = term_array_member(Ts, (void *)t);
        }
        if (CyclicTerm != NULL) {
          STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
          STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
          in_pair = 4;
        } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            if (t == TermNil) {
          ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, CompactPairEndList,
                                    _trie_retry_pair);
          in_pair = 0;
        } else {
          AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 2);
          STACK_PUSH_UP(t, stack_terms);
          STACK_PUSH_UP(AbsPair(PairTermMark), stack_terms);
          in_pair = 4;
        }
#ifdef TRIE_RATIONAL_TERMS
        CyclicTerm = NULL;
        if (IsVarTerm(aux_pair[0]) || IsPairTerm(aux_pair[0]))
          CyclicTerm = term_array_member(Ts, (void *)Deref(aux_pair[0]));
        if (CyclicTerm != NULL) {
          STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
        } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
          STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
      }
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
    } else if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = answer_search_global_trie_terms_loop(
          t, &vars_arity, stack_terms PASS_REGS);
      current_node = global_trie_check_insert_gt_entry(
          current_node, (Term)entry_node PASS_REGS);
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
#else  /* ! TRIE_COMPACT_PAIRS */
#if defined(MODE_GLOBAL_TRIE_LOOP) && defined(GLOBAL_TRIE_FOR_SUBTERMS)
        if (current_node != GLOBAL_root_gt) {
      gt_node_ptr entry_node = answer_search_global_trie_terms_loop(
          t, &vars_arity, stack_terms PASS_REGS);
      current_node = global_trie_check_insert_gt_entry(
          current_node, (Term)entry_node PASS_REGS);
    } else
#endif /* MODE_GLOBAL_TRIE_LOOP && GLOBAL_TRIE_FOR_SUBTERMS */
        if (IsPairTerm(t)) {
      CELL *aux_pair = RepPair(t);
      ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsPair(NULL),
                                _trie_retry_pair);
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
      STACK_PUSH_UP(Deref(aux_pair[1]), stack_terms);
      STACK_PUSH_UP(Deref(aux_pair[0]), stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = FunctorOfTerm(t);
      if (f == FunctorDouble) {
        union {
          Term t_dbl[sizeof(Float) / sizeof(Term)];
          Float dbl;
        } u;
        u.dbl = FloatOfTerm(t);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_null + in_pair);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, u.t_dbl[1],
                                  _trie_retry_extension);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, u.t_dbl[0],
                                  _trie_retry_extension);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_double);
      } else if (f == FunctorLongInt) {
        Int li = LongIntOfTerm(t);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_null + in_pair);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, li,
                                  _trie_retry_extension);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_longint);
      } else if (f == FunctorBigInt || f == FunctorString) {
        CELL *opq = Yap_HeapStoreOpaqueTerm(t);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_null + in_pair);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, (CELL)opq,
                                  _trie_retry_extension);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_bigint);
      } else if (f == FunctorDBRef) {
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "answer_search_loop: unsupported type tag FunctorDBRef");
      } else {
#ifdef TRIE_RATIONAL_TERMS
        term_array_push(&Ts, (void *)t, (void *)current_node);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        int i;
        CELL *aux_appl = RepAppl(t);
        ANSWER_CHECK_INSERT_ENTRY(sg_fr, current_node, AbsAppl((Term *)f),
                                  _trie_retry_appl + in_pair);
        AUX_STACK_CHECK_EXPAND(stack_terms,
                               stack_terms_limit + ArityOfFunctor(f) - 1);
        for (i = ArityOfFunctor(f); i >= 1; i--) {
#ifdef TRIE_RATIONAL_TERMS
          CyclicTerm = NULL;
          if (IsVarTerm(aux_appl[i]) || IsApplTerm(aux_appl[i]))
            CyclicTerm = term_array_member(Ts, (void *)Deref(aux_appl[i]));
          if (CyclicTerm != NULL) {
            STACK_PUSH_UP((Term)CyclicTerm, stack_terms);
            STACK_PUSH_UP((Term)RationalMark, stack_terms);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            STACK_PUSH_UP(Deref(aux_appl[i]), stack_terms);
        }
      }
#ifdef TRIE_COMPACT_PAIRS
      in_pair = 0;
#endif /* TRIE_COMPACT_PAIRS */
    } else {
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "answer_search_loop: unknown type tag");
#endif /* MODE_TERMS_LOOP */
    }
    t = STACK_POP_DOWN(stack_terms);
  } while (t);
#ifdef TRIE_RATIONAL_TERMS
  term_array_free(&Ts);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
  *vars_arity_ptr = vars_arity;
  return current_node;

#undef stack_terms_limit
#ifndef TRIE_COMPACT_PAIRS
#undef in_pair
#endif /* TRIE_COMPACT_PAIRS */
}
#endif /* INCLUDE_ANSWER_SEARCH_LOOP */

/**************************************************************
**                   answer_search_min_max                   **
**************************************************************/

#ifdef INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
static inline ans_node_ptr answer_search_min_max(sg_fr_ptr sg_fr,
                                                 ans_node_ptr current_node,
                                                 Term t, int mode USES_REGS) {
  ans_node_ptr child_node;
  Term child_term;
  Term trie_value = 0, term_value = t;
  int cmp;

  /* start by computing the current value on the trie (trie_value) */
  child_node = TrNode_child(current_node);
  child_term = TrNode_entry(child_node);
  if (IsIntTerm(child_term)) {
    trie_value = child_term;
  } else if (IsApplTerm(child_term)) {
    Functor f = (Functor)RepAppl(child_term);
    child_node = TrNode_child(child_node);
    if (f == FunctorLongInt) {
      trie_value = MkLongIntTerm((Int)TrNode_entry(child_node));
    } else if (f == FunctorDouble) {
      union {
        Term t_dbl[sizeof(Float) / sizeof(Term)];
        Float dbl;
      } u;
      u.t_dbl[0] = TrNode_entry(child_node);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      child_node = TrNode_child(child_node);
      u.t_dbl[1] = TrNode_entry(child_node);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      trie_value = MkFloatTerm(u.dbl);
    } else if (f == FunctorBigInt) {
      trie_value = AbsAppl((CELL *)TrNode_entry(child_node));
    } else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "answer_search_min_max: invalid arithmetic value");
    child_node = TrNode_child(child_node);
  }

  cmp = Yap_acmp(term_value, trie_value PASS_REGS);
  /* worse answer */
  if ((mode == MODE_DIRECTED_MIN && cmp > 0) ||
      (mode == MODE_DIRECTED_MAX && cmp < 0))
    return NULL;
  /* equal answer */
  if (cmp == 0)
    return child_node;
  /* better answer */
  if (IsAtomOrIntTerm(t)) {
    ANSWER_SAFE_INSERT_ENTRY(current_node, t, _trie_retry_atom);
  } else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorDouble) {
      union {
        Term t_dbl[sizeof(Float) / sizeof(Term)];
        Float dbl;
      } u;
      u.dbl = FloatOfTerm(t);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      ANSWER_SAFE_INSERT_ENTRY(current_node, u.t_dbl[1], _trie_retry_extension);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      ANSWER_SAFE_INSERT_ENTRY(current_node, u.t_dbl[0], _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_double);
    } else if (f == FunctorLongInt) {
      Int li = LongIntOfTerm(t);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
      ANSWER_SAFE_INSERT_ENTRY(current_node, li, _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_longint);
    } else if (f == FunctorBigInt) {
      CELL *li = Yap_HeapStoreOpaqueTerm(t);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
      ANSWER_SAFE_INSERT_ENTRY(current_node, (CELL)li, _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_bigint);
    }
  }
  return current_node;
}
#endif /* INCLUDE_ANSWER_SEARCH_MODE_DIRECTED */

/**********************************************************
**                   answer_search_sum                   **
**********************************************************/

#ifdef INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
static inline ans_node_ptr answer_search_sum(sg_fr_ptr sg_fr,
                                             ans_node_ptr current_node,
                                             Term t USES_REGS) {
  ans_node_ptr child_node;
  Term child_term;
  Term trie_value = 0, term_value = t, sum_value = 0;

  /* start by computing the current value on the trie (trie_value) */
  child_node = TrNode_child(current_node);
  child_term = TrNode_entry(child_node);
  if (IsIntTerm(child_term)) {
    trie_value = child_term;
  } else if (IsApplTerm(child_term)) {
    Functor f = (Functor)RepAppl(child_term);
    child_node = TrNode_child(child_node);
    if (f == FunctorLongInt) {
      trie_value = MkLongIntTerm((Int)TrNode_entry(child_node));
    } else if (f == FunctorDouble) {
      union {
        Term t_dbl[sizeof(Float) / sizeof(Term)];
        Float dbl;
      } u;
      u.t_dbl[0] = TrNode_entry(child_node);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      child_node = TrNode_child(child_node);
      u.t_dbl[1] = TrNode_entry(child_node);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      trie_value = MkFloatTerm(u.dbl);
    } else if (f == FunctorBigInt) {
      trie_value = AbsAppl((CELL *)TrNode_entry(child_node));
    } else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "answer_search_min_max: invalid arithmetic value");
    child_node = TrNode_child(child_node);
  }

  sum_value = p_plus(trie_value, term_value PASS_REGS);
  if (IsAtomOrIntTerm(sum_value)) {
    ANSWER_SAFE_INSERT_ENTRY(current_node, sum_value, _trie_retry_atom);
  } else if (IsApplTerm(sum_value)) {
    Functor f = FunctorOfTerm(sum_value);
    if (f == FunctorDouble) {
      union {
        Term t_dbl[sizeof(Float) / sizeof(Term)];
        Float dbl;
      } u;
      u.dbl = FloatOfTerm(sum_value);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      ANSWER_SAFE_INSERT_ENTRY(current_node, u.t_dbl[1], _trie_retry_extension);
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
      ANSWER_SAFE_INSERT_ENTRY(current_node, u.t_dbl[0], _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_double);
    } else if (f == FunctorLongInt) {
      Int li = LongIntOfTerm(sum_value);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
      ANSWER_SAFE_INSERT_ENTRY(current_node, li, _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_longint);
    } else if (f == FunctorBigInt) {
      CELL *li = Yap_HeapStoreOpaqueTerm(sum_value);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_null);
      ANSWER_SAFE_INSERT_ENTRY(current_node, (CELL)li, _trie_retry_extension);
      ANSWER_SAFE_INSERT_ENTRY(current_node, AbsAppl((Term *)f),
                               _trie_retry_bigint);
    }
  }
  return current_node;
}
#endif /* INCLUDE_ANSWER_SEARCH_MODE_DIRECTED */

/***************************************************************
**                   invalidate_answer_trie                   **
***************************************************************/

#ifdef INCLUDE_ANSWER_SEARCH_MODE_DIRECTED
static void invalidate_answer_trie(ans_node_ptr current_node, sg_fr_ptr sg_fr,
                                   int position USES_REGS) {
  if (IS_ANSWER_TRIE_HASH(current_node)) {
    ans_hash_ptr hash;
    ans_node_ptr *bucket, *last_bucket;
    hash = (ans_hash_ptr)current_node;
    bucket = Hash_buckets(hash);
    last_bucket = bucket + Hash_num_buckets(hash);
    do {
      current_node = *bucket;
      if (current_node) {
        ans_node_ptr next_node = TrNode_next(current_node);
        if (IS_ANSWER_LEAF_NODE(current_node)) {
          INVALIDATE_ANSWER_TRIE_LEAF_NODE(current_node, sg_fr);
        } else {
          invalidate_answer_trie(TrNode_child(current_node), sg_fr,
                                 TRAVERSE_POSITION_FIRST PASS_REGS);
          INVALIDATE_ANSWER_TRIE_NODE(current_node, sg_fr);
        }
        while (next_node) {
          current_node = next_node;
          next_node = TrNode_next(current_node);
          invalidate_answer_trie(current_node, sg_fr,
                                 TRAVERSE_POSITION_NEXT PASS_REGS);
        }
      }
    } while (++bucket != last_bucket);
    if (Hash_next(hash))
      Hash_previous(Hash_next(hash)) = Hash_previous(hash);
    if (Hash_previous(hash))
      Hash_next(Hash_previous(hash)) = Hash_next(hash);
    else
      SgFr_hash_chain(sg_fr) = Hash_next(hash);
    FREE_BUCKETS(Hash_buckets(hash));
    FREE_ANSWER_TRIE_HASH(hash);
  } else {
    if (position == TRAVERSE_POSITION_FIRST) {
      ans_node_ptr next_node = TrNode_next(current_node);
      if (IS_ANSWER_LEAF_NODE(current_node)) {
        INVALIDATE_ANSWER_TRIE_LEAF_NODE(current_node, sg_fr);
      } else {
        invalidate_answer_trie(TrNode_child(current_node), sg_fr,
                               TRAVERSE_POSITION_FIRST PASS_REGS);
        INVALIDATE_ANSWER_TRIE_NODE(current_node, sg_fr);
      }
      while (next_node) {
        current_node = next_node;
        next_node = TrNode_next(current_node);
        invalidate_answer_trie(current_node, sg_fr,
                               TRAVERSE_POSITION_NEXT PASS_REGS);
      }
    } else {
      if (IS_ANSWER_LEAF_NODE(current_node)) {
        INVALIDATE_ANSWER_TRIE_LEAF_NODE(current_node, sg_fr);
      } else {
        invalidate_answer_trie(TrNode_child(current_node), sg_fr,
                               TRAVERSE_POSITION_FIRST PASS_REGS);
        INVALIDATE_ANSWER_TRIE_NODE(current_node, sg_fr);
      }
    }
  }
  return;
}
#endif /* INCLUDE_ANSWER_SEARCH_MODE_DIRECTED */

/************************************************************************
**                   load_(answer|substitution)_loop                   **
************************************************************************/

#ifdef INCLUDE_LOAD_ANSWER_LOOP
#ifdef MODE_GLOBAL_TRIE_LOOP
static inline CELL *load_substitution_loop(gt_node_ptr current_node,
                                           int *vars_arity_ptr,
                                           CELL *stack_terms USES_REGS) {
#else
static inline CELL *load_answer_loop(ans_node_ptr current_node USES_REGS) {
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
                     ----------|           |  stack_terms_pair_offset
(TRIE_COMPACT_PAIRS)
                     |  TERM_1 |          \|/
                     ===========           *
 LOCAL_TrailTop -->  |         |  <-- stack_terms_base (TRIE_COMPACT_PAIRS)
                     -----------
************************************************************************/
#ifdef MODE_GLOBAL_TRIE_LOOP
  int vars_arity = *vars_arity_ptr;
#else
  int vars_arity = 0;
  CELL *stack_terms = (CELL *)LOCAL_TrailTop;
#endif /* MODE_GLOBAL_TRIE_LOOP */
  CELL *stack_vars_base = (CELL *)TR;
#define stack_terms_limit (stack_vars_base + vars_arity)
#ifdef TRIE_COMPACT_PAIRS
#define stack_terms_base ((CELL *)LOCAL_TrailTop)
  int stack_terms_pair_offset = 0;
#endif /* TRIE_COMPACT_PAIRS */
  Term t = TrNode_entry(current_node);
#ifdef MODE_GLOBAL_TRIE_LOOP
  current_node = TrNode_parent(current_node);
#else
  current_node = (ans_node_ptr)UNTAG_ANSWER_NODE(TrNode_parent(current_node));
#endif /* MODE_GLOBAL_TRIE_LOOP */

#ifdef TRIE_RATIONAL_TERMS
  term_array Ts;
  void *CyclicTerm;
  term_array_init(&Ts, 10);
  Term RationalTermTMP; // a temporary temp to be used from the rational code
#endif                  /* RATIONAL TERM SUPPORT FOR TRIES */

  do {
#ifdef TRIE_RATIONAL_TERMS
    CyclicTerm = term_array_member(Ts, (void *)current_node);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
    if (IsVarTerm(t)) {
#ifdef TRIE_RATIONAL_TERMS
      if (t > VarIndexOfTableTerm(MAX_TABLE_VARS) &&
          TrNode_child((gt_node_ptr)t) !=
              (gt_node_ptr)(1)) { // TODO: substitute the != 1 test to something
                                  // more appropriate
        /* Rational term */
        RationalTermTMP = (Term)term_array_member(Ts, (void *)t);
        if (RationalTermTMP) {
          /* rational term is assigned a variable already */
          AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
          STACK_PUSH_UP(RationalTermTMP, stack_terms);
        } else {
          RationalTermTMP = MkVarTerm();
          STACK_PUSH_UP(RationalTermTMP, stack_terms);
          /* memorize the rational term and assign it a variable */
          term_array_push(&Ts, (void *)t, (void *)RationalTermTMP);
        }
      } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
      {
#if !defined(MODE_GLOBAL_TRIE_LOOP) || defined(GLOBAL_TRIE_FOR_SUBTERMS)
        if (t > VarIndexOfTableTerm(MAX_TABLE_VARS)) {
          stack_terms = load_substitution_loop((gt_node_ptr)t, &vars_arity,
                                               stack_terms PASS_REGS);
        } else
#endif /* ! MODE_GLOBAL_TRIE_LOOP || GLOBAL_TRIE_FOR_SUBTERMS */
        {
          int var_index = VarIndexOfTableTerm(t);
          AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit - vars_arity +
                                                  var_index + 1);
          if (var_index >= vars_arity) {
            while (vars_arity < var_index)
              stack_vars_base[vars_arity++] = 0;
            stack_vars_base[vars_arity++] = MkVarTerm();
          } else if (stack_vars_base[var_index] == 0)
            stack_vars_base[var_index] = MkVarTerm();
          STACK_PUSH_UP(stack_vars_base[var_index], stack_terms);
        }
      }
    } else if (IsAtomOrIntTerm(t)) {
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
#ifdef TRIE_RATIONAL_TERMS
      if (CyclicTerm) {
        AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 4);
        STACK_PUSH_UP((Term)RationalMark, stack_terms); // Add a rational term
                                                        // marker necessary as
                                                        // we read both ways the
                                                        // stack //
        STACK_PUSH_UP(t, stack_terms);                  // Add the term //
        STACK_PUSH_UP(
            CyclicTerm,
            stack_terms); // Add the variable that the term will unify with //
        STACK_PUSH_UP((Term)RationalMark, stack_terms); // Add a rational term
                                                        // marker necessary as
                                                        // we read both ways the
                                                        // stack //
      } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        STACK_PUSH_UP(t, stack_terms);
    } else if (IsPairTerm(t)) {
#ifdef TRIE_COMPACT_PAIRS
      if (t == CompactPairInit) {
        Term *stack_aux = stack_terms_base - stack_terms_pair_offset;
        Term head, tail = STACK_POP_UP(stack_aux);
#ifdef TRIE_RATIONAL_TERMS
        if (IsRationalTerm(tail)) {
          Yap_Error(SYSTEM_ERROR_INTERNAL, tail, "Rational element of a "
                                                 "Rational Term appears as the "
                                                 "first Tail of a list");
        }
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        while (STACK_NOT_EMPTY(stack_aux, stack_terms)) {
          head = STACK_POP_UP(stack_aux);
#ifdef TRIE_RATIONAL_TERMS
          if (IsRationalTerm(head)) {
            head = STACK_POP_UP(stack_aux); // thats the rational term
            RationalTermTMP =
                STACK_POP_UP(stack_aux);   // that is the variable to unify with
            (void)STACK_POP_UP(stack_aux); // eat the second rational mark
            tail = MkPairTerm(head, tail);
            Yap_unify(RationalTermTMP, tail);
          } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
            tail = MkPairTerm(head, tail);
        }
        stack_terms = stack_terms_base - stack_terms_pair_offset;
        stack_terms_pair_offset = (int)STACK_POP_DOWN(stack_terms);
        STACK_PUSH_UP(tail, stack_terms);
      } else { /* CompactPairEndList / CompactPairEndTerm */
        Term last;
        AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit + 1);
        last = STACK_POP_DOWN(stack_terms);
#ifdef TRIE_RATIONAL_TERMS
        RationalTermTMP = TermNil;
        if (IsRationalTerm(last)) { // rather unlikely case the rational term is
                                    // the last of a list
          RationalTermTMP = STACK_POP_DOWN(stack_terms); // in this case we need
                                                         // to invert the term
                                                         // with the end of list
          last = STACK_POP_DOWN(stack_terms); // variable to unify with
          (void)STACK_POP_DOWN(stack_terms);  // eat the second rational mark
        }
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
        STACK_PUSH_UP(stack_terms_pair_offset, stack_terms);
        stack_terms_pair_offset = (int)(stack_terms_base - stack_terms);
        if (t == CompactPairEndList)
          STACK_PUSH_UP(TermNil, stack_terms);
#ifdef TRIE_RATIONAL_TERMS
        if (RationalTermTMP && RationalTermTMP != TermNil) {
          /* most probably this never occurs */
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
          STACK_PUSH_UP(last, stack_terms);
          STACK_PUSH_UP(RationalTermTMP, stack_terms);
          STACK_PUSH_UP((Term)RationalMark, stack_terms);
        } else
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
          STACK_PUSH_UP(last, stack_terms);
      }
#else  /* ! TRIE_COMPACT_PAIRS */
      Term head = STACK_POP_DOWN(stack_terms);
      Term tail = STACK_POP_DOWN(stack_terms);
      t = MkPairTerm(head, tail);
      STACK_PUSH_UP(t, stack_terms);
#endif /* TRIE_COMPACT_PAIRS */
    } else if (IsApplTerm(t)) {
      Functor f = (Functor)RepAppl(t);
      if (f == FunctorDouble) {
        union {
          Term t_dbl[sizeof(Float) / sizeof(Term)];
          Float dbl;
        } u;
        t = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        u.t_dbl[0] = t;
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
        t = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        u.t_dbl[1] = t;
#endif /* SIZEOF_DOUBLE x SIZEOF_INT_P */
        current_node = TrNode_parent(current_node);
        t = MkFloatTerm(u.dbl);
      } else if (f == FunctorLongInt) {
        Int li = TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        current_node = TrNode_parent(current_node);
        t = MkLongIntTerm(li);
      } else if (f == FunctorBigInt || f == FunctorString) {
        CELL *ptr = (CELL *)TrNode_entry(current_node);
        current_node = TrNode_parent(current_node);
        current_node = TrNode_parent(current_node);
        t = AbsAppl(ptr);
      } else {
        int f_arity = ArityOfFunctor(f);
        t = Yap_MkApplTerm(f, f_arity, stack_terms);
        stack_terms += f_arity;
      }
      AUX_STACK_CHECK_EXPAND(stack_terms, stack_terms_limit);
      STACK_PUSH_UP(t, stack_terms);
    }
#ifdef TRIE_RATIONAL_TERMS
    if (CyclicTerm) {
      RationalTermTMP = STACK_POP_DOWN(stack_terms);
      if
        IsRationalTerm(RationalTermTMP) {
          // printf("Special Case\n");
        }
      else if (IsPairTerm(RationalTermTMP)) {
        Yap_unify((Term)CyclicTerm, RationalTermTMP);
      } else if (IsApplTerm(RationalTermTMP)) {
        Yap_unify((Term)CyclicTerm, RationalTermTMP);
      }
      STACK_PUSH_UP(RationalTermTMP, stack_terms);
    }
    RationalTermTMP = TermNil;
    CyclicTerm = NULL;
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
    t = TrNode_entry(current_node);
    current_node = TrNode_parent(current_node);
  } while (current_node);
#ifdef TRIE_RATIONAL_TERMS
  term_array_free(&Ts);
#endif /* RATIONAL TERM SUPPORT FOR TRIES */
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

/***************************
**      Undef Macros      **
***************************/

#undef INCREMENT_GLOBAL_TRIE_REFERENCE
#undef NEW_SUBGOAL_TRIE_NODE
#undef NEW_ANSWER_TRIE_NODE
#undef NEW_GLOBAL_TRIE_NODE
#undef SUBGOAL_CHECK_INSERT_ENTRY
#undef ANSWER_CHECK_INSERT_ENTRY
