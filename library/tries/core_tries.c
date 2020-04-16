/*********************************************
  File:     core_tries.c
  Author:   Ricardo Rocha
  Comments: Tries core module for Yap Prolog
  version:  $ID$
*********************************************/

/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "core_tries.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

static TrNode put_entry(TrNode node, YAP_Term entry);
static TrNode check_entry(TrNode node, YAP_Term entry);
static YAP_Term get_entry(TrNode node, YAP_Term *stack_list, TrNode *cur_node);
static void remove_entry(TrNode node);
static void remove_child_nodes(TrNode node);
static TrNode copy_child_nodes(TrNode parent_dest, TrNode node_source);
static void traverse_and_add(TrNode parent_dest, TrNode parent_source);
static void traverse_and_join(TrNode parent_dest, TrNode parent_source);
static void traverse_and_intersect(TrNode parent_dest, TrNode parent_source);
static YAP_Int traverse_and_count_common_entries(TrNode parent1,
                                                 TrNode parent2);
static YAP_Int traverse_and_count_entries(TrNode node);
static void traverse_and_get_usage(TrNode node, YAP_Int depth);
static void traverse_and_save(TrNode node, FILE *file, int float_block);
static void traverse_and_load(TrNode parent, FILE *file);
static void traverse_and_print(TrNode node, int *arity, char *str,
                               int str_index, int mode);

static YAP_Term trie_to_list(TrNode node);
static YAP_Term trie_to_list_node(TrNode node);
static YAP_Term trie_to_list_floats(TrNode node);

/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

static TrEngine CURRENT_TRIE_ENGINE;

static YAP_Int USAGE_ENTRIES, USAGE_NODES, USAGE_VIRTUAL_NODES;
static YAP_Int CURRENT_AUXILIARY_TERM_STACK_SIZE, CURRENT_TRIE_MODE,
    CURRENT_LOAD_VERSION, CURRENT_DEPTH, CURRENT_INDEX;
static YAP_Term *AUXILIARY_TERM_STACK;
YAP_Term *stack_args, *stack_args_base, *stack_vars, *stack_vars_base;
static YAP_Functor FunctorComma;
static void (*DATA_SAVE_FUNCTION)(TrNode, FILE *);
static void (*DATA_LOAD_FUNCTION)(TrNode, YAP_Int, FILE *);
static void (*DATA_PRINT_FUNCTION)(TrNode);
static void (*DATA_ADD_FUNCTION)(TrNode, TrNode);
static void (*DATA_COPY_FUNCTION)(TrNode, TrNode);
static void (*DATA_DESTRUCT_FUNCTION)(TrNode);

static YAP_Int TRIE_DISABLE_HASH_TABLE = 0;

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

static TrNode trie_node_check_insert(TrNode parent, YAP_Term t) {
  TrNode child;

  CURRENT_DEPTH++;
  child = TrNode_child(parent);
  if (child == NULL) {
    new_trie_node(child, t, parent, NULL, NULL, NULL);
    TrNode_child(parent) = child;
  } else if (IS_HASH_NODE(child)) {
    TrHash hash;
    TrNode *bucket;
    int count;
    hash = (TrHash)child;
    bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
    child = *bucket;
    count = 0;
    while (child) {
      if ((TrNode_entry(child) == t) ||
          (((TrNode_entry(child) == PairEndTermTag) ||
            (TrNode_entry(child) == PairEndEmptyTag)) &&
           ((CURRENT_TRIE_MODE & TRIE_MODE_MINIMAL) == TRIE_MODE_MINIMAL)))
        return child;
      count++;
      child = TrNode_next(child);
    }
    while (child)
      ;
    TrHash_num_nodes(hash)++;
    new_trie_node(child, t, parent, NULL, *bucket, AS_TR_NODE_NEXT(bucket));
    if (*bucket)
      TrNode_previous(*bucket) = child;
    *bucket = child;
    if (count > MAX_NODES_PER_BUCKET &&
        TrHash_num_nodes(hash) > TrHash_num_buckets(hash)) {
      /* expand trie hash */
      TrNode chain, next, *first_bucket, *new_bucket;
      int seed;
      first_bucket = TrHash_buckets(hash);
      bucket = first_bucket + TrHash_num_buckets(hash);
      TrHash_num_buckets(hash) *= 2;
      new_hash_buckets(hash, TrHash_num_buckets(hash));
      seed = TrHash_num_buckets(hash) - 1;
      do {
        if (*--bucket) {
          chain = *bucket;
          do {
            new_bucket =
                TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), seed));
            next = TrNode_next(chain);
            TrNode_next(chain) = *new_bucket;
            TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
            if (*new_bucket)
              TrNode_previous(*new_bucket) = chain;
            *new_bucket = chain;
            chain = next;
          } while (chain);
        }
      } while (bucket != first_bucket);
      free_hash_buckets(first_bucket, TrHash_num_buckets(hash) / 2);
    }
  } else {
    int count = 0;
    do {
      if ((TrNode_entry(child) == t) ||
          (((TrNode_entry(child) == PairEndTermTag) ||
            (TrNode_entry(child) == PairEndEmptyTag)) &&
           ((CURRENT_TRIE_MODE & TRIE_MODE_MINIMAL) == TRIE_MODE_MINIMAL)))
        return child;
      count++;
      child = TrNode_next(child);
    } while (child);
    new_trie_node(child, t, parent, NULL, TrNode_child(parent), NULL);
    TrNode_previous(TrNode_child(parent)) = child;
    if ((++count > MAX_NODES_PER_TRIE_LEVEL) &&
        (TRIE_DISABLE_HASH_TABLE == 0)) {
      /* alloc a new trie hash */
      TrHash hash;
      TrNode chain, next, *bucket;
      new_trie_hash(hash, count, BASE_HASH_BUCKETS);
      chain = child;
      do {
        bucket = TrHash_bucket(
            hash, HASH_TERM(TrNode_entry(chain), BASE_HASH_BUCKETS - 1));
        next = TrNode_next(chain);
        TrNode_next(chain) = *bucket;
        TrNode_previous(chain) = AS_TR_NODE_NEXT(bucket);
        if (*bucket)
          TrNode_previous(*bucket) = chain;
        *bucket = chain;
        chain = next;
      } while (chain);
      TrNode_child(parent) = (TrNode)hash;
    } else
      TrNode_child(parent) = child;
  }
  return child;
}

static TrNode trie_node_insert(TrNode parent, YAP_Term t, TrHash hash) {
  TrNode child;

  CURRENT_DEPTH++;
  if (hash) {
    /* is trie hash */
    TrNode *bucket;
    TrHash_num_nodes(hash)++;
    bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
    new_trie_node(child, t, parent, NULL, *bucket, AS_TR_NODE_NEXT(bucket));
    if (*bucket)
      TrNode_previous(*bucket) = child;
    *bucket = child;
  } else {
    new_trie_node(child, t, parent, NULL, TrNode_child(parent), NULL);
    if (TrNode_child(parent))
      TrNode_previous(TrNode_child(parent)) = child;
    TrNode_child(parent) = child;
  }
  return child;
}

static TrNode trie_node_check(TrNode parent, YAP_Term t) {
  TrNode child;

  child = TrNode_child(parent);
  if (IS_HASH_NODE(child)) {
    TrHash hash;
    TrNode *bucket;
    hash = (TrHash)child;
    bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
    child = *bucket;
    if (!child)
      return NULL;
  }
  do {
    if (TrNode_entry(child) == t)
      return child;
    child = TrNode_next(child);
  } while (child);
  return NULL;
}

static YAP_Term trie_to_list_create_simple(const char *atom_name, TrNode node) {
  YAP_Functor f = YAP_MkFunctor(YAP_LookupAtom(atom_name), 1);
  YAP_Term child = trie_to_list(TrNode_child(node));

  return YAP_MkApplTerm(f, 1, &child);
}

static YAP_Term trie_to_list_create_simple_end(const char *atom_name,
                                               TrNode node) {
  YAP_Atom atom = YAP_LookupAtom(atom_name);

  if (IS_LEAF_TRIE_NODE(node)) {
    return YAP_MkAtomTerm(atom);
  } else {
    YAP_Functor f = YAP_MkFunctor(atom, 1);
    YAP_Term child = trie_to_list(TrNode_child(node));
    return YAP_MkApplTerm(f, 1, &child);
  }
}

static YAP_Term trie_to_list_create_two(const char *atom_name, TrNode node,
                                        YAP_Term operand) {
  YAP_Atom atom = YAP_LookupAtom(atom_name);

  if (IS_LEAF_TRIE_NODE(node)) {
    YAP_Functor f = YAP_MkFunctor(atom, 1);
    return YAP_MkApplTerm(f, 1, &operand);
  } else {
    YAP_Functor f = YAP_MkFunctor(atom, 2);
    YAP_Term args[2] = {operand, trie_to_list(TrNode_child(node))};
    return YAP_MkApplTerm(f, 2, args);
  }
}

/* -------------------------- */
/*            API             */
/* -------------------------- */

TrEngine core_trie_init_module(void) {
  static int init_once = 1;
  TrEngine engine;

  if (init_once) {
    new_struct(AUXILIARY_TERM_STACK, YAP_Term,
               BASE_AUXILIARY_TERM_STACK_SIZE * sizeof(YAP_Term));
    CURRENT_AUXILIARY_TERM_STACK_SIZE = BASE_AUXILIARY_TERM_STACK_SIZE;
    CURRENT_TRIE_MODE = TRIE_MODE_STANDARD;
    FunctorComma = YAP_MkFunctor(YAP_LookupAtom(","), 2);
    init_once = 0;
  }
  new_trie_engine(engine);
  return engine;
}

TrNode core_trie_open(TrEngine engine) {
  TrNode node;

  CURRENT_TRIE_ENGINE = engine;
  new_trie_node(node, 0, NULL, NULL, TrEngine_trie(engine),
                AS_TR_NODE_NEXT(&TrEngine_trie(engine)));
  if (TrEngine_trie(engine))
    TrNode_previous(TrEngine_trie(engine)) = node;
  TrEngine_trie(engine) = node;
  INCREMENT_TRIES(CURRENT_TRIE_ENGINE);
  return node;
}

void core_trie_close(TrEngine engine, TrNode node,
                     void (*destruct_function)(TrNode)) {
  CURRENT_TRIE_ENGINE = engine;
  DATA_DESTRUCT_FUNCTION = destruct_function;
  if (TrNode_child(node))
    remove_child_nodes(TrNode_child(node));
  if (TrNode_next(node)) {
    TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
    TrNode_next(TrNode_previous(node)) = TrNode_next(node);
  } else
    TrNode_next(TrNode_previous(node)) = NULL;
  free_trie_node(node);
  DECREMENT_TRIES(CURRENT_TRIE_ENGINE);
  return;
}

void core_trie_close_all(TrEngine engine, void (*destruct_function)(TrNode)) {
  while (TrEngine_trie(engine))
    core_trie_close(engine, TrEngine_trie(engine), destruct_function);
  return;
}

void core_trie_set_mode(YAP_Int mode) {
  CURRENT_TRIE_MODE = mode;
  return;
}

YAP_Int core_trie_get_mode(void) { return CURRENT_TRIE_MODE; }

TrNode core_trie_put_entry(TrEngine engine, TrNode node, YAP_Term entry,
                           YAP_Int *depth) {
  CURRENT_TRIE_ENGINE = engine;
  CURRENT_DEPTH = 0;
  stack_args_base = stack_args = AUXILIARY_TERM_STACK;
  stack_vars_base = stack_vars =
      AUXILIARY_TERM_STACK + CURRENT_AUXILIARY_TERM_STACK_SIZE - 1;
  node = put_entry(node, entry);
  if (!IS_LEAF_TRIE_NODE(node)) {
    MARK_AS_LEAF_TRIE_NODE(node);
    INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  }
  /* reset var terms */
  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) {
    (void)POP_DOWN(stack_vars);
    *((YAP_Term *)*stack_vars) = *stack_vars;
  }
  if (depth)
    *depth = CURRENT_DEPTH;
  return node;
}

TrNode core_trie_check_entry(TrNode node, YAP_Term entry) {
  if (!TrNode_child(node))
    return NULL;
  stack_args_base = stack_args = AUXILIARY_TERM_STACK;
  stack_vars_base = stack_vars =
      AUXILIARY_TERM_STACK + CURRENT_AUXILIARY_TERM_STACK_SIZE - 1;
  node = check_entry(node, entry);
  /* reset var terms */
  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) {
    (void)POP_DOWN(stack_vars);
    *((YAP_Term *)*stack_vars) = *stack_vars;
  }
  return node;
}

YAP_Term core_trie_get_entry(TrNode node) {
  CURRENT_INDEX = -1;
  stack_vars_base = stack_vars = AUXILIARY_TERM_STACK;
  stack_args_base = stack_args =
      AUXILIARY_TERM_STACK + CURRENT_AUXILIARY_TERM_STACK_SIZE - 1;
  return get_entry(node, stack_args, &node);
}

void core_trie_remove_entry(TrEngine engine, TrNode node,
                            void (*destruct_function)(TrNode)) {
  CURRENT_TRIE_ENGINE = engine;
  DATA_DESTRUCT_FUNCTION = destruct_function;
  if (DATA_DESTRUCT_FUNCTION)
    (*DATA_DESTRUCT_FUNCTION)(node);
  DECREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  remove_entry(node);
  return;
}

void core_trie_remove_subtree(TrEngine engine, TrNode node,
                              void (*destruct_function)(TrNode)) {
  TrNode parent;

  CURRENT_TRIE_ENGINE = engine;
  DATA_DESTRUCT_FUNCTION = destruct_function;
  parent = TrNode_parent(node);
  remove_child_nodes(TrNode_child(parent));
  remove_entry(parent);
  return;
}

void core_trie_add(TrNode node_dest, TrNode node_source,
                   void (*add_function)(TrNode, TrNode)) {
  DATA_ADD_FUNCTION = add_function;
  if (TrNode_child(node_dest) && TrNode_child(node_source))
    traverse_and_add(node_dest, node_source);
  return;
}

void core_trie_join(TrEngine engine, TrNode node_dest, TrNode node_source,
                    void (*add_function)(TrNode, TrNode),
                    void (*copy_function)(TrNode, TrNode)) {
  CURRENT_TRIE_ENGINE = engine;
  DATA_ADD_FUNCTION = add_function;
  DATA_COPY_FUNCTION = copy_function;
  if (TrNode_child(node_dest)) {
    if (TrNode_child(node_source))
      traverse_and_join(node_dest, node_source);
  } else if (TrNode_child(node_source))
    TrNode_child(node_dest) =
        copy_child_nodes(node_dest, TrNode_child(node_source));
  return;
}

void core_trie_intersect(TrEngine engine, TrNode node_dest, TrNode node_source,
                         void (*add_function)(TrNode, TrNode),
                         void (*destruct_function)(TrNode)) {
  CURRENT_TRIE_ENGINE = engine;
  DATA_ADD_FUNCTION = add_function;
  DATA_DESTRUCT_FUNCTION = destruct_function;
  if (TrNode_child(node_dest)) {
    if (TrNode_child(node_source))
      traverse_and_intersect(node_dest, node_source);
    else {
      remove_child_nodes(TrNode_child(node_dest));
      TrNode_child(node_dest) = NULL;
    }
  }
  return;
}

YAP_Int core_trie_count_join(TrNode node1, TrNode node2) {
  YAP_Int count = 0;

  if (TrNode_child(node1)) {
    count += traverse_and_count_entries(TrNode_child(node1));
    if (TrNode_child(node2)) {
      count += traverse_and_count_entries(TrNode_child(node2));
      count -= traverse_and_count_common_entries(node1, node2);
    }
  } else if (TrNode_child(node2))
    count += traverse_and_count_entries(TrNode_child(node2));
  return count;
}

YAP_Int core_trie_count_intersect(TrNode node1, TrNode node2) {
  YAP_Int count = 0;

  if (TrNode_child(node1))
    if (TrNode_child(node2))
      count = traverse_and_count_common_entries(node1, node2);
  return count;
}

void core_trie_save(TrNode node, FILE *file,
                    void (*save_function)(TrNode, FILE *)) {
  CURRENT_INDEX = -1;
  DATA_SAVE_FUNCTION = save_function;
  if (TrNode_child(node)) {
    fprintf(file, "BEGIN_TRIE_v2 ");
    traverse_and_save(TrNode_child(node), file, 0);
    fprintf(file, "END_TRIE_v2");
    fflush(file);
  }
  return;
}

TrNode core_trie_load(TrEngine engine, FILE *file,
                      void (*load_function)(TrNode, YAP_Int, FILE *)) {
  TrNode node;
  char version[15];
  fpos_t curpos;
  int n;

  n = fscanf(file, "%14s", version);
  if (fgetpos(file, &curpos))
    return NULL;

  if (!strcmp(version, "BEGIN_TRIE_v2")) {
    fseek(file, -11, SEEK_END);
    n = fscanf(file, "%s", version);
    if (strcmp(version, "END_TRIE_v2")) {
      fprintf(stderr, "******************************************\n");
      fprintf(stderr, "  Tries core module: trie file corrupted\n");
      fprintf(stderr, "******************************************\n");
      fflush(stderr);
      return NULL;
    }
    if (fsetpos(file, &curpos))
      return NULL;
    CURRENT_LOAD_VERSION = 2;
  } else if (!strcmp(version, "BEGIN_TRIE")) {
    fseek(file, -8, SEEK_END);
    n = fscanf(file, "%s", version);
    if (strcmp(version, "END_TRIE")) {
      fprintf(stderr, "******************************************\n");
      fprintf(stderr, "  Tries core module: trie file corrupted\n");
      fprintf(stderr, "******************************************\n");
      fflush(stderr);
      return NULL;
    }
    if (fsetpos(file, &curpos))
      return NULL;
    CURRENT_LOAD_VERSION = 1;
  } else {
    fprintf(stderr, "****************************************\n");
    fprintf(stderr, "  Tries core module: invalid trie file\n");
    fprintf(stderr, "****************************************\n");
    fflush(stderr);
    return NULL;
  }
  CURRENT_TRIE_ENGINE = engine;
  CURRENT_INDEX = -1;
  CURRENT_DEPTH = 0;
  DATA_LOAD_FUNCTION = load_function;
  node = core_trie_open(engine);
  traverse_and_load(node, file);
  if (n)
    n = 0; // just added to remove the warning of not used!
  return node;
}

void core_trie_stats(TrEngine engine, YAP_Int *memory, YAP_Int *tries,
                     YAP_Int *entries, YAP_Int *nodes) {
  *memory = TrEngine_memory(engine);
  *tries = TrEngine_tries(engine);
  *entries = TrEngine_entries(engine);
  *nodes = TrEngine_nodes(engine);
  return;
}

void core_trie_max_stats(TrEngine engine, YAP_Int *memory, YAP_Int *tries,
                         YAP_Int *entries, YAP_Int *nodes) {
  *memory = TrEngine_memory_max(engine);
  *tries = TrEngine_tries_max(engine);
  *entries = TrEngine_entries_max(engine);
  *nodes = TrEngine_nodes_max(engine);
  return;
}

void core_trie_usage(TrNode node, YAP_Int *entries, YAP_Int *nodes,
                     YAP_Int *virtual_nodes) {
  USAGE_ENTRIES = 0;
  USAGE_NODES = 0;
  USAGE_VIRTUAL_NODES = 0;
  if (TrNode_child(node))
    traverse_and_get_usage(TrNode_child(node), 0);
  *entries = USAGE_ENTRIES;
  *nodes = USAGE_NODES;
  *virtual_nodes = USAGE_VIRTUAL_NODES;
  return;
}

void core_trie_print(TrNode node, void (*print_function)(TrNode)) {
  DATA_PRINT_FUNCTION = print_function;
  if (TrNode_child(node)) {
    int arity[1000];
    char str[10000];
    arity[0] = 0;
    traverse_and_print(TrNode_child(node), arity, str, 0, TRIE_PRINT_NORMAL);
  } else
    fprintf(stdout, "(empty)\n");
  fflush(stdout);
  return;
}

void core_disable_hash_table(void) { TRIE_DISABLE_HASH_TABLE = 1; }

void core_enable_hash_table(void) { TRIE_DISABLE_HASH_TABLE = 0; }

YAP_Term core_trie_to_list(TrNode node) {
  TrNode root = TrNode_child(node);

  if (root)
    return trie_to_list(root);
  else
    return YAP_MkAtomTerm(YAP_LookupAtom("empty"));
}

/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

static TrNode put_entry(TrNode node, YAP_Term entry) {
  YAP_Term t = YAP_Deref(entry);
  if (YAP_IsVarTerm(t)) {
    if (IsTrieVar(t, stack_vars, stack_vars_base)) {
      node = trie_node_check_insert(
          node, MkTrieVar((stack_vars_base - 1 - (YAP_Term *)t) / 2));
    } else {
      node = trie_node_check_insert(
          node, MkTrieVar((stack_vars_base - stack_vars) / 2));
      PUSH_UP(stack_vars, t, stack_args);
      *((YAP_Term *)t) = (YAP_Term)stack_vars;
      PUSH_UP(stack_vars, stack_vars, stack_args);
    }
  } else if (YAP_IsAtomTerm(t)) {
    node = trie_node_check_insert(node, t);
  } else if (YAP_IsIntTerm(t)) {
    node = trie_node_check_insert(node, t);
  } else if (YAP_IsFloatTerm(t)) {
    volatile union {
      double f;
      YAP_Term p[SIZE_FLOAT_AS_TERM];
    } tf; /* to avoid gcc warning */
    tf.f = YAP_FloatOfTerm(t);
    node = trie_node_check_insert(node, FloatInitTag);
    node = trie_node_check_insert(node, tf.p[0]);
#ifdef TAG_LOW_BITS_32
    node = trie_node_check_insert(node, tf.p[1]);
#endif /* TAG_LOW_BITS_32 */
    node = trie_node_check_insert(node, FloatEndTag);
  } else if (YAP_IsPairTerm(t)) {
    node = trie_node_check_insert(node, PairInitTag);
    if ((CURRENT_TRIE_MODE & TRIE_MODE_REVERSE) == TRIE_MODE_STANDARD) {
      do {
        node = put_entry(node, YAP_HeadOfTerm(t));
        t = YAP_Deref(YAP_TailOfTerm(t));
      } while (YAP_IsPairTerm(t));
      if (t == YAP_TermNil()) {
        node = trie_node_check_insert(node, PairEndEmptyTag);
      } else {
        node = put_entry(node, t);
        node = trie_node_check_insert(node, PairEndTermTag);
      }
    } else if (CURRENT_TRIE_MODE & TRIE_MODE_REVERSE) { /* TRIE_MODE_REVERSE */
      YAP_Term *stack_list = stack_args;
      do {
        PUSH_DOWN(stack_args, YAP_HeadOfTerm(t), stack_vars);
        t = YAP_Deref(YAP_TailOfTerm(t));
      } while (YAP_IsPairTerm(t));
      if (t == YAP_TermNil()) {
        while (STACK_NOT_EMPTY(stack_args, stack_list))
          node = put_entry(node, POP_UP(stack_args));
        node = trie_node_check_insert(node, PairEndEmptyTag);
      } else {
        PUSH_DOWN(stack_args, t, stack_vars);
        while (STACK_NOT_EMPTY(stack_args, stack_list))
          node = put_entry(node, POP_UP(stack_args));
        node = trie_node_check_insert(node, PairEndTermTag);
      }
    }
  } else if (YAP_IsApplTerm(t)) {
    YAP_Functor f = YAP_FunctorOfTerm(t);
    if (f == FunctorComma) {
      node = trie_node_check_insert(node, CommaInitTag);
      do {
        node = put_entry(node, YAP_ArgOfTerm(1, t));
        t = YAP_Deref(YAP_ArgOfTerm(2, t));
      } while (YAP_IsApplTerm(t) && YAP_FunctorOfTerm(t) == FunctorComma);
      node = put_entry(node, t);
      node = trie_node_check_insert(node, CommaEndTag);
    } else {
      int i;
      node = trie_node_check_insert(node, ApplTag | ((YAP_Term)f));
      for (i = 1; i <= YAP_ArityOfFunctor(f); i++)
        node = put_entry(node, YAP_ArgOfTerm(i, t));
    }
  } else {
    fprintf(stderr, "***************************************\n");
    fprintf(stderr, "  Tries core module: unknown type tag\n");
    fprintf(stderr, "***************************************\n");
    fflush(stderr);
  }

  return node;
}

static TrNode check_entry(TrNode node, YAP_Term entry) {
  YAP_Term t = YAP_Deref(entry);
  if (YAP_IsVarTerm(t)) {
    if (IsTrieVar(t, stack_vars, stack_vars_base)) {
      if (!(node = trie_node_check(
                node, MkTrieVar((stack_vars_base - 1 - (YAP_Term *)t) / 2))))
        return NULL;
    } else {
      if (!(node = trie_node_check(
                node, MkTrieVar((stack_vars_base - stack_vars) / 2))))
        return NULL;
      PUSH_UP(stack_vars, t, stack_args);
      *((YAP_Term *)t) = (YAP_Term)stack_vars;
      PUSH_UP(stack_vars, stack_vars, stack_args);
    }
  } else if (YAP_IsAtomTerm(t)) {
    if (!(node = trie_node_check(node, t)))
      return NULL;
  } else if (YAP_IsIntTerm(t)) {
    if (!(node = trie_node_check(node, t)))
      return NULL;
  } else if (YAP_IsFloatTerm(t)) {
    volatile union {
      double f;
      YAP_Term p[SIZE_FLOAT_AS_TERM];
    } tf; /* to avoid gcc warning */
    tf.f = YAP_FloatOfTerm(t);
    if (!(node = trie_node_check(node, FloatInitTag)))
      return NULL;
    if (!(node = trie_node_check(node, tf.p[0])))
      return NULL;
#ifdef TAG_LOW_BITS_32
    if (!(node = trie_node_check(node, tf.p[1])))
      return NULL;
#endif /* TAG_LOW_BITS_32 */
    if (!(node = trie_node_check(node, FloatEndTag)))
      return NULL;
  } else if (YAP_IsPairTerm(t)) {
    if (!(node = trie_node_check(node, PairInitTag)))
      return NULL;
    if ((CURRENT_TRIE_MODE & TRIE_MODE_REVERSE) == TRIE_MODE_STANDARD) {
      do {
        if (!(node = check_entry(node, YAP_HeadOfTerm(t))))
          return NULL;
        t = YAP_Deref(YAP_TailOfTerm(t));
      } while (YAP_IsPairTerm(t));
      if (t == YAP_TermNil()) {
        if (!(node = trie_node_check(node, PairEndEmptyTag)))
          return NULL;
      } else {
        if (!(node = check_entry(node, t)))
          return NULL;
        if (!(node = trie_node_check(node, PairEndTermTag)))
          return NULL;
      }
    } else if (CURRENT_TRIE_MODE & TRIE_MODE_REVERSE) { /* TRIE_MODE_REVERSE */
      YAP_Term *stack_list = stack_args;
      do {
        PUSH_DOWN(stack_args, YAP_HeadOfTerm(t), stack_vars);
        t = YAP_Deref(YAP_TailOfTerm(t));
      } while (YAP_IsPairTerm(t));
      if (t == YAP_TermNil()) {
        while (STACK_NOT_EMPTY(stack_args, stack_list))
          if (!(node = check_entry(node, POP_UP(stack_args))))
            return NULL;
        if (!(node = trie_node_check(node, PairEndEmptyTag)))
          return NULL;
      } else {
        PUSH_DOWN(stack_args, t, stack_vars);
        while (STACK_NOT_EMPTY(stack_args, stack_list))
          if (!(node = check_entry(node, POP_UP(stack_args))))
            return NULL;
        if (!(node = trie_node_check(node, PairEndTermTag)))
          return NULL;
      }
    }
  } else if (YAP_IsApplTerm(t)) {
    YAP_Functor f = YAP_FunctorOfTerm(t);
    if (f == FunctorComma) {
      if (!(node = trie_node_check(node, CommaInitTag)))
        return NULL;
      do {
        if (!(node = check_entry(node, YAP_ArgOfTerm(1, t))))
          return NULL;
        t = YAP_Deref(YAP_ArgOfTerm(2, t));
      } while (YAP_IsApplTerm(t) && YAP_FunctorOfTerm(t) == FunctorComma);
      if (!(node = check_entry(node, t)))
        return NULL;
      if (!(node = trie_node_check(node, CommaEndTag)))
        return NULL;
    } else {
      int i;
      if (!(node = trie_node_check(node, ApplTag | ((YAP_Term)f))))
        return NULL;
      for (i = 1; i <= YAP_ArityOfFunctor(f); i++)
        if (!(node = check_entry(node, YAP_ArgOfTerm(i, t))))
          return NULL;
    }
  } else {
    fprintf(stderr, "***************************************\n");
    fprintf(stderr, "  Tries core module: unknown type tag\n");
    fprintf(stderr, "***************************************\n");
    fflush(stderr);
  }

  return node;
}

static YAP_Term get_entry(TrNode node, YAP_Term *stack_mark, TrNode *cur_node) {
  YAP_Term t = (YAP_Term)&t;
  while (TrNode_parent(node)) {
    t = TrNode_entry(node);
    if (YAP_IsVarTerm(t)) {
      int index = TrieVarIndex(t);
      if (index > CURRENT_INDEX) {
        int i;
        stack_vars = &stack_vars_base[index + 1];
        if (stack_vars > stack_args + 1) {
          fprintf(stderr, "**************************************\n");
          fprintf(stderr, "  Tries core module: term stack full\n");
          fprintf(stderr, "**************************************\n");
          fflush(stderr);
        }
        for (i = index; i > CURRENT_INDEX; i--)
          stack_vars_base[i] = 0;
        CURRENT_INDEX = index;
      }
      if (stack_vars_base[index]) {
        t = stack_vars_base[index];
      } else {
        t = YAP_MkVarTerm();
        stack_vars_base[index] = t;
      }
      PUSH_UP(stack_args, t, stack_vars);
    } else if (YAP_IsAtomTerm(t)) {
      PUSH_UP(stack_args, t, stack_vars);
    } else if (YAP_IsIntTerm(t)) {
      PUSH_UP(stack_args, t, stack_vars);
    } else if (YAP_IsPairTerm(t)) {
      if (t == PairInitTag) {
        YAP_Term t2;
        if ((CURRENT_TRIE_MODE & TRIE_MODE_REVERSE) == TRIE_MODE_STANDARD) {
          YAP_Term *stack_aux = stack_mark;
          t = *stack_aux--;
          while (STACK_NOT_EMPTY(stack_aux, stack_args)) {
            t2 = *stack_aux--;
            t = YAP_MkPairTerm(t2, t);
          }
        } else if (CURRENT_TRIE_MODE &
                   TRIE_MODE_REVERSE) { /* TRIE_MODE_REVERSE */
          YAP_Term *stack_aux = stack_mark;
          t = *stack_aux;
          if (t == YAP_TermNil())
            stack_aux--;
          else
            t = POP_DOWN(stack_args);
          while (STACK_NOT_EMPTY(stack_args, stack_aux)) {
            t2 = POP_DOWN(stack_args);
            t = YAP_MkPairTerm(t2, t);
          }
        }
        stack_args = stack_mark;
        *cur_node = node;
        return t;
      } else if (t == PairEndEmptyTag) {
        t = YAP_TermNil();
        PUSH_UP(stack_args, t, stack_vars);
        node = TrNode_parent(node);
        t = get_entry(node, &stack_args[1], &node);
        PUSH_UP(stack_args, t, stack_vars);
      } else if (t == PairEndTermTag) {
        node = TrNode_parent(node);
        t = get_entry(node, stack_args, &node);
        PUSH_UP(stack_args, t, stack_vars);
      } else if (t == CommaEndTag) {
        node = TrNode_parent(node);
        t = get_entry(node, stack_args, &node);
        PUSH_UP(stack_args, t, stack_vars);
      } else if (t == CommaInitTag) {
        YAP_Term *stack_aux = stack_mark;
        stack_aux--;
        while (STACK_NOT_EMPTY(stack_aux, stack_args)) {
          t = YAP_MkApplTerm(FunctorComma, 2, stack_aux);
          *stack_aux = t;
          stack_aux--;
        }
        stack_args = stack_mark;
        *cur_node = node;
        return t;
      } else if (t == FloatEndTag) {
        volatile union {
          double f;
          YAP_Term p[SIZE_FLOAT_AS_TERM];
        } tf; /* to avoid gcc warning */
#ifdef TAG_LOW_BITS_32
        node = TrNode_parent(node);
        tf.p[1] = TrNode_entry(node);
#endif /* TAG_LOW_BITS_32 */
        node = TrNode_parent(node);
        tf.p[0] = TrNode_entry(node);
        node = TrNode_parent(node); /* ignore FloatInitTag */
        t = YAP_MkFloatTerm(tf.f);
        PUSH_UP(stack_args, t, stack_vars);
      } else if (t == FloatInitTag) {
      }
    } else if (ApplTag & t) {
      YAP_Functor f = (YAP_Functor)(~ApplTag & t);
      int arity = YAP_ArityOfFunctor(f);
      t = YAP_MkApplTerm(f, arity, &stack_args[1]);
      stack_args += arity;
      PUSH_UP(stack_args, t, stack_vars);
    } else {
      fprintf(stderr, "***************************************\n");
      fprintf(stderr, "  Tries core module: unknown type tag\n");
      fprintf(stderr, "***************************************\n");
      fflush(stderr);
    }
    node = TrNode_parent(node);
  }
  *cur_node = node;
  return t;
}

static void remove_entry(TrNode node) {
  TrNode parent = TrNode_parent(node);
  while (parent) {
    if (TrNode_previous(node)) {
      if (IS_HASH_NODE(TrNode_child(parent))) {
        TrHash hash = (TrHash)TrNode_child(parent);
        TrHash_num_nodes(hash)--;
        if (TrHash_num_nodes(hash)) {
          if (TrNode_next(node)) {
            TrNode_next(TrNode_previous(node)) = TrNode_next(node);
            TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
          } else {
            TrNode_next(TrNode_previous(node)) = NULL;
          }
          free_trie_node(node);
          return;
        }
        free_hash_buckets(TrHash_buckets(hash), TrHash_num_buckets(hash));
        free_trie_hash(hash);
      } else {
        if (TrNode_next(node)) {
          TrNode_next(TrNode_previous(node)) = TrNode_next(node);
          TrNode_previous(TrNode_next(node)) = TrNode_previous(node);
        } else {
          TrNode_next(TrNode_previous(node)) = NULL;
        }
        free_trie_node(node);
        return;
      }
    } else if (TrNode_next(node)) {
      TrNode_child(parent) = TrNode_next(node);
      TrNode_previous(TrNode_next(node)) = NULL;
      free_trie_node(node);
      return;
    }
    free_trie_node(node);
    node = parent;
    parent = TrNode_parent(node);
  }
  TrNode_child(node) = NULL;
  return;
}

static void remove_child_nodes(TrNode node) {
  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash)node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket)
        remove_child_nodes(*bucket);
    } while (bucket != first_bucket);
    free_hash_buckets(first_bucket, TrHash_num_buckets(hash));
    free_trie_hash(hash);
    return;
  }
  if (TrNode_next(node))
    remove_child_nodes(TrNode_next(node));
  if (!IS_LEAF_TRIE_NODE(node)) {
    remove_child_nodes(TrNode_child(node));
  } else {
    if (DATA_DESTRUCT_FUNCTION)
      (*DATA_DESTRUCT_FUNCTION)(node);
    DECREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  }
  free_trie_node(node);
  return;
}

static TrNode copy_child_nodes(TrNode parent_dest, TrNode child_source) {
  TrNode child_dest, next_dest;

  if (IS_HASH_NODE(child_source)) {
    TrNode *bucket_dest, *first_bucket_source, *bucket_source;
    TrHash hash_dest, hash_source;
    hash_source = (TrHash)child_source;
    first_bucket_source = TrHash_buckets(hash_source);
    bucket_source = first_bucket_source + TrHash_num_buckets(hash_source);
    new_trie_hash(hash_dest, TrHash_num_nodes(hash_source),
                  TrHash_num_buckets(hash_source));
    bucket_dest = TrHash_buckets(hash_dest) + TrHash_num_buckets(hash_dest);
    do {
      bucket_dest--;
      if (*--bucket_source) {
        *bucket_dest = copy_child_nodes(parent_dest, *bucket_source);
        TrNode_previous(*bucket_dest) = AS_TR_NODE_NEXT(bucket_dest);
      } else
        *bucket_dest = NULL;
    } while (bucket_source != first_bucket_source);
    return (TrNode)hash_dest;
  }

  if (TrNode_next(child_source))
    next_dest = copy_child_nodes(parent_dest, TrNode_next(child_source));
  else
    next_dest = NULL;
  new_trie_node(child_dest, TrNode_entry(child_source), parent_dest, NULL,
                next_dest, NULL);
  if (next_dest)
    TrNode_previous(next_dest) = child_dest;
  if (IS_LEAF_TRIE_NODE(child_source)) {
    MARK_AS_LEAF_TRIE_NODE(child_dest);
    INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
    if (DATA_COPY_FUNCTION)
      (*DATA_COPY_FUNCTION)(child_dest, child_source);
  } else
    TrNode_child(child_dest) =
        copy_child_nodes(child_dest, TrNode_child(child_source));
  return child_dest;
}

static void traverse_and_add(TrNode parent_dest, TrNode parent_source) {
  TrNode child_dest, child_source;

  /* parent_source is not a leaf node */
  child_source = TrNode_child(parent_source);
  if (IS_HASH_NODE(child_source)) {
    TrNode *first_bucket_source, *bucket_source;
    TrHash hash_source;
    hash_source = (TrHash)child_source;
    first_bucket_source = TrHash_buckets(hash_source);
    bucket_source = first_bucket_source + TrHash_num_buckets(hash_source);
    do {
      child_source = *--bucket_source;
      while (child_source) {
        /* parent_dest is not a leaf node */
        child_dest = trie_node_check(parent_dest, TrNode_entry(child_source));
        if (child_dest) {
          if (IS_LEAF_TRIE_NODE(child_dest)) {
            /* child_source is a leaf node */
            if (DATA_ADD_FUNCTION)
              (*DATA_ADD_FUNCTION)(child_dest, child_source);
          } else
            /* child_dest and child_source are not leaf nodes */
            traverse_and_add(child_dest, child_source);
        }
        child_source = TrNode_next(child_source);
      }
    } while (bucket_source != first_bucket_source);
    return;
  }
  while (child_source) {
    /* parent_dest is not a leaf node */
    child_dest = trie_node_check(parent_dest, TrNode_entry(child_source));
    if (child_dest) {
      if (IS_LEAF_TRIE_NODE(child_dest)) {
        /* child_source is a leaf node */
        if (DATA_ADD_FUNCTION)
          (*DATA_ADD_FUNCTION)(child_dest, child_source);
      } else
        /* child_dest and child_source are not leaf nodes */
        traverse_and_add(child_dest, child_source);
    }
    child_source = TrNode_next(child_source);
  }
  return;
}

static void traverse_and_join(TrNode parent_dest, TrNode parent_source) {
  TrNode child_dest, child_source;

  /* parent_source is not a leaf node */
  child_source = TrNode_child(parent_source);
  if (IS_HASH_NODE(child_source)) {
    TrNode *first_bucket_source, *bucket_source;
    TrHash hash_source;
    hash_source = (TrHash)child_source;
    first_bucket_source = TrHash_buckets(hash_source);
    bucket_source = first_bucket_source + TrHash_num_buckets(hash_source);
    do {
      child_source = *--bucket_source;
      while (child_source) {
        /* parent_dest is not a leaf node */
        child_dest = trie_node_check(parent_dest, TrNode_entry(child_source));
        if (child_dest) {
          if (IS_LEAF_TRIE_NODE(child_dest)) {
            /* child_source is a leaf node */
            if (DATA_ADD_FUNCTION)
              (*DATA_ADD_FUNCTION)(child_dest, child_source);
          } else
            /* child_dest and child_source are not leaf nodes */
            traverse_and_join(child_dest, child_source);
        } else {
          child_dest =
              trie_node_check_insert(parent_dest, TrNode_entry(child_source));
          if (IS_LEAF_TRIE_NODE(child_source)) {
            MARK_AS_LEAF_TRIE_NODE(child_dest);
            INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
            if (DATA_COPY_FUNCTION)
              (*DATA_COPY_FUNCTION)(child_dest, child_source);
          } else
            TrNode_child(child_dest) =
                copy_child_nodes(child_dest, TrNode_child(child_source));
        }
        child_source = TrNode_next(child_source);
      }
    } while (bucket_source != first_bucket_source);
    return;
  }
  while (child_source) {
    /* parent_dest is not a leaf node */
    child_dest = trie_node_check(parent_dest, TrNode_entry(child_source));
    if (child_dest) {
      if (IS_LEAF_TRIE_NODE(child_dest)) {
        /* child_source is a leaf node */
        if (DATA_ADD_FUNCTION)
          (*DATA_ADD_FUNCTION)(child_dest, child_source);
      } else
        /* child_dest and child_source are not leaf nodes */
        traverse_and_join(child_dest, child_source);
    } else {
      child_dest =
          trie_node_check_insert(parent_dest, TrNode_entry(child_source));
      if (IS_LEAF_TRIE_NODE(child_source)) {
        MARK_AS_LEAF_TRIE_NODE(child_dest);
        INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
        if (DATA_COPY_FUNCTION)
          (*DATA_COPY_FUNCTION)(child_dest, child_source);
      } else
        TrNode_child(child_dest) =
            copy_child_nodes(child_dest, TrNode_child(child_source));
    }
    child_source = TrNode_next(child_source);
  }
  return;
}

static void traverse_and_intersect(TrNode parent_dest, TrNode parent_source) {
  TrNode child_dest, child_source, child_next;

  /* parent_dest is not a leaf node */
  child_dest = TrNode_child(parent_dest);
  if (IS_HASH_NODE(child_dest)) {
    TrNode *first_bucket_dest, *bucket_dest;
    TrHash hash_dest;
    hash_dest = (TrHash)child_dest;
    first_bucket_dest = TrHash_buckets(hash_dest);
    bucket_dest = first_bucket_dest + TrHash_num_buckets(hash_dest);
    do {
      child_dest = *--bucket_dest;
      while (child_dest) {
        child_next = TrNode_next(child_dest);
        /* parent_source is not a leaf node */
        child_source = trie_node_check(parent_source, TrNode_entry(child_dest));
        if (child_source) {
          if (IS_LEAF_TRIE_NODE(child_dest)) {
            /* child_source is a leaf node */
            if (DATA_ADD_FUNCTION)
              (*DATA_ADD_FUNCTION)(child_dest, child_source);
          } else
            /* child_dest and child_source are not leaf nodes */
            traverse_and_intersect(child_dest, child_source);
        } else {
          if (IS_LEAF_TRIE_NODE(child_dest)) {
            if (DATA_DESTRUCT_FUNCTION)
              (*DATA_DESTRUCT_FUNCTION)(child_dest);
            DECREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
          } else
            remove_child_nodes(TrNode_child(child_dest));
          remove_entry(child_dest);
        }
        child_dest = child_next;
      }
    } while (bucket_dest != first_bucket_dest);
    return;
  }
  while (child_dest) {
    child_next = TrNode_next(child_dest);
    /* parent_source is not a leaf node */
    child_source = trie_node_check(parent_source, TrNode_entry(child_dest));
    if (child_source) {
      if (IS_LEAF_TRIE_NODE(child_dest)) {
        /* child_source is a leaf node */
        if (DATA_ADD_FUNCTION)
          (*DATA_ADD_FUNCTION)(child_dest, child_source);
      } else
        /* child_dest and child_source are not leaf nodes */
        traverse_and_intersect(child_dest, child_source);
    } else {
      if (IS_LEAF_TRIE_NODE(child_dest)) {
        if (DATA_DESTRUCT_FUNCTION)
          (*DATA_DESTRUCT_FUNCTION)(child_dest);
        DECREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
      } else
        remove_child_nodes(TrNode_child(child_dest));
      remove_entry(child_dest);
    }
    child_dest = child_next;
  }
  return;
}

static YAP_Int traverse_and_count_common_entries(TrNode parent1,
                                                 TrNode parent2) {
  TrNode child1, child2;
  YAP_Int count = 0;

  /* parent1 is not a leaf node */
  child1 = TrNode_child(parent1);
  if (IS_HASH_NODE(child1)) {
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash)child1;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      child1 = *--bucket;
      while (child1) {
        /* parent2 is not a leaf node */
        child2 = trie_node_check(parent2, TrNode_entry(child1));
        if (child2) {
          if (IS_LEAF_TRIE_NODE(child1))
            /* child2 is a leaf node */
            count++;
          else
            /* child1 and child2 are not leaf nodes */
            count += traverse_and_count_common_entries(child1, child2);
        }
        child1 = TrNode_next(child1);
      }
    } while (bucket != first_bucket);
    return count;
  }
  while (child1) {
    /* parent2 is not a leaf node */
    child2 = trie_node_check(parent2, TrNode_entry(child1));
    if (child2) {
      if (IS_LEAF_TRIE_NODE(child1))
        /* child2 is a leaf node */
        count++;
      else
        /* child1 and child2 are not leaf nodes */
        count += traverse_and_count_common_entries(child1, child2);
    }
    child1 = TrNode_next(child1);
  }
  return count;
}

static YAP_Int traverse_and_count_entries(TrNode node) {
  YAP_Int count = 0;

  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash)node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket) {
        node = *bucket;
        count += traverse_and_count_entries(node);
      }
    } while (bucket != first_bucket);
    return count;
  }

  if (TrNode_next(node))
    count += traverse_and_count_entries(TrNode_next(node));
  if (!IS_LEAF_TRIE_NODE(node))
    count += traverse_and_count_entries(TrNode_child(node));
  else
    count++;
  return count;
}

static void traverse_and_get_usage(TrNode node, YAP_Int depth) {
  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash)node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket) {
        node = *bucket;
        traverse_and_get_usage(node, depth);
      }
    } while (bucket != first_bucket);
    return;
  }

  USAGE_NODES++;
  if (TrNode_next(node))
    traverse_and_get_usage(TrNode_next(node), depth);
  depth++;
  if (!IS_LEAF_TRIE_NODE(node)) {
    traverse_and_get_usage(TrNode_child(node), depth);
  } else {
    USAGE_ENTRIES++;
    USAGE_VIRTUAL_NODES += depth;
  }
  return;
}

static void traverse_and_save(TrNode node, FILE *file, int float_block) {
  YAP_Term t;

  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash)node;
    fprintf(file, UInt_FORMAT " %d ", HASH_SAVE_MARK, TrHash_num_buckets(hash));
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket) {
        node = *bucket;
        traverse_and_save(node, file, float_block);
      }
    } while (bucket != first_bucket);
    return;
  }

  if (TrNode_next(node))
    traverse_and_save(TrNode_next(node), file, float_block);

  t = TrNode_entry(node);
  if (float_block) {
    float_block--;
    fprintf(file, UInt_FORMAT " " UInt_FORMAT "  ", FLOAT_SAVE_MARK, t);
  } else if (YAP_IsPairTerm(t)) {
    if (t == FloatInitTag) {
#ifdef TAG_LOW_BITS_32
      float_block++;
#endif /* TAG_LOW_BITS_32 */
      float_block++;
    }
    fprintf(file, UInt_FORMAT " ", t);
  } else if (YAP_IsVarTerm(t) || YAP_IsIntTerm(t))
    fprintf(file, UInt_FORMAT " ", t);
  else {
    int index;
    for (index = 0; index <= CURRENT_INDEX; index++)
      if (AUXILIARY_TERM_STACK[index] == t)
        break;
    if (index > CURRENT_INDEX) {
      CURRENT_INDEX = index;
      if (CURRENT_INDEX == CURRENT_AUXILIARY_TERM_STACK_SIZE)
        expand_auxiliary_term_stack();
      AUXILIARY_TERM_STACK[CURRENT_INDEX] = t;
      if (YAP_IsAtomTerm(t))
        fprintf(file, UInt_FORMAT " %d %s%c ", ATOM_SAVE_MARK, index,
                YAP_AtomName(YAP_AtomOfTerm(t)), '\0');
      else /* (ApplTag & t) */
        fprintf(file, UInt_FORMAT " %d %s " UInt_FORMAT " ", FUNCTOR_SAVE_MARK,
                index,
                YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & t))),
                YAP_ArityOfFunctor((YAP_Functor)(~ApplTag & t)));
    } else if (YAP_IsAtomTerm(t))
      fprintf(file, UInt_FORMAT " %d ", ATOM_SAVE_MARK, index);
    else
      fprintf(file, UInt_FORMAT " %d ", FUNCTOR_SAVE_MARK, index);
  }
  if (IS_LEAF_TRIE_NODE(node)) {
    fprintf(file, "- ");
    if (DATA_SAVE_FUNCTION)
      (*DATA_SAVE_FUNCTION)(node, file);
  } else {
    traverse_and_save(TrNode_child(node), file, float_block);
    fprintf(file, "- ");
  }
  return;
}

static void traverse_and_load(TrNode parent, FILE *file) {
  TrHash hash = NULL;
  YAP_Term t;
  int n;

  if (!fscanf(file, UInt_FORMAT, &t)) {
    MARK_AS_LEAF_TRIE_NODE(parent);
    INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
    if (DATA_LOAD_FUNCTION)
      (*DATA_LOAD_FUNCTION)(parent, CURRENT_DEPTH, file);
    CURRENT_DEPTH--;
    return;
  }
  if (t == HASH_SAVE_MARK) {
    /* alloc a new trie hash */
    int num_buckets;
    n = fscanf(file, "%d", &num_buckets);
    new_trie_hash(hash, 0, num_buckets);
    TrNode_child(parent) = (TrNode)hash;
    n = fscanf(file, UInt_FORMAT, &t);
  }
  do {
    TrNode child;
    if (t == ATOM_SAVE_MARK) {
      int index;
      n = fscanf(file, "%d", &index);
      if (index > CURRENT_INDEX) {
        char atom[1000];
        if (CURRENT_LOAD_VERSION == 2) {
          char *ptr, ch;
          ptr = atom;
          fgetc(file); /* skip the first empty space */
          while ((ch = fgetc(file)))
            *ptr++ = ch;
          *ptr = '\0';
        } else if (CURRENT_LOAD_VERSION == 1) {
          n = fscanf(file, "%s", atom);
        }
        CURRENT_INDEX = index;
        if (CURRENT_INDEX == CURRENT_AUXILIARY_TERM_STACK_SIZE)
          expand_auxiliary_term_stack();
        AUXILIARY_TERM_STACK[CURRENT_INDEX] =
            YAP_MkAtomTerm(YAP_LookupAtom(atom));
      }
      t = AUXILIARY_TERM_STACK[index];
    } else if (t == FUNCTOR_SAVE_MARK) {
      int index;
      n = fscanf(file, "%d", &index);
      if (index > CURRENT_INDEX) {
        char atom[1000];
        int arity;
        n = fscanf(file, "%s %d", atom, &arity);
        CURRENT_INDEX = index;
        if (CURRENT_INDEX == CURRENT_AUXILIARY_TERM_STACK_SIZE)
          expand_auxiliary_term_stack();
        AUXILIARY_TERM_STACK[CURRENT_INDEX] =
            ApplTag | ((YAP_Term)YAP_MkFunctor(YAP_LookupAtom(atom), arity));
      }
      t = AUXILIARY_TERM_STACK[index];
    } else if (t == FLOAT_SAVE_MARK)
      n = fscanf(file, UInt_FORMAT, &t);
    child = trie_node_insert(parent, t, hash);
    traverse_and_load(child, file);
  } while (fscanf(file, UInt_FORMAT, &t));
  CURRENT_DEPTH--;
  if (n)
    n = 0; // just added to remove the warning of not used!
  return;
}

static void traverse_and_print(TrNode node, int *arity, char *str,
                               int str_index, int mode) {
  YAP_Term t;
  int last_pair_mark = -arity[arity[0]];

  if (IS_HASH_NODE(node)) {
    int *current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    TrNode *first_bucket, *bucket;
    TrHash hash;
    hash = (TrHash)node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    do {
      if (*--bucket) {
        node = *bucket;
        traverse_and_print(node, arity, str, str_index, mode);
        memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
        if (mode != TRIE_PRINT_FLOAT2 && arity[arity[0]] < 0) {
          /* restore possible PairEndEmptyTag/PairEndTermTag/CommaEndTag
           * side-effect */
          if (str_index > 0 && str[str_index - 1] != '[')
            str[str_index - 1] = ',';
          /* restore possible PairEndTermTag side-effect */
          if (str[last_pair_mark] == '|')
            str[last_pair_mark] = ',';
        }
      }
    } while (bucket != first_bucket);
    free(current_arity);
    return;
  }

  if (TrNode_next(node)) {
    int *current_arity = (int *)malloc(sizeof(int) * (arity[0] + 1));
    memmove(current_arity, arity, sizeof(int) * (arity[0] + 1));
    traverse_and_print(TrNode_next(node), arity, str, str_index, mode);
    memmove(arity, current_arity, sizeof(int) * (current_arity[0] + 1));
    if (mode != TRIE_PRINT_FLOAT2 && arity[arity[0]] < 0) {
      /* restore possible PairEndEmptyTag/PairEndTermTag/CommaEndTag side-effect
       */
      if (str_index > 0 && str[str_index - 1] != '[')
        str[str_index - 1] = ',';
      /* restore possible PairEndTermTag side-effect */
      if (str[last_pair_mark] == '|')
        str[last_pair_mark] = ',';
    }
    free(current_arity);
  }

  /* update position for possible PairEndTermTag side-effect */
  if (mode != TRIE_PRINT_FLOAT2 && arity[arity[0]] < 0 && str_index > 1)
    arity[arity[0]] = -str_index + 1;

  t = TrNode_entry(node);
  if (mode == TRIE_PRINT_FLOAT) {
#ifdef TAG_LOW_BITS_32
    arity[arity[0]] = (YAP_Int)t;
    mode = TRIE_PRINT_FLOAT2;
  } else if (mode == TRIE_PRINT_FLOAT2) {
    volatile union {
      double f;
      YAP_Term p[SIZE_FLOAT_AS_TERM];
    } tf; /* to avoid gcc warning */
    tf.p[1] = t;
    tf.p[0] = (YAP_Term)arity[arity[0]];
    arity[arity[0]] = -1;
#else  /* TAG_64BITS */
    volatile union {
      double f;
      YAP_Term p[SIZE_FLOAT_AS_TERM];
    } tf; /* to avoid gcc warning */
    tf.p[0] = t;
#endif /* TAG_SCHEME */
    str_index += sprintf(&str[str_index], "%.15g", tf.f);
    mode = TRIE_PRINT_FLOAT_END;
  } else if (mode == TRIE_PRINT_FLOAT_END) {
    arity[0]--;
    while (arity[0]) {
      if (arity[arity[0]] == 1) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else {
        if (arity[arity[0]] > 1)
          arity[arity[0]]--;
        str_index += sprintf(&str[str_index], ",");
        break;
      }
    }
    mode = TRIE_PRINT_NORMAL;
  } else if (YAP_IsVarTerm(t)) {
    str_index += sprintf(&str[str_index], "VAR" UInt_FORMAT, TrieVarIndex(t));
    while (arity[0]) {
      if (arity[arity[0]] == 1) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else {
        if (arity[arity[0]] > 1)
          arity[arity[0]]--;
        str_index += sprintf(&str[str_index], ",");
        break;
      }
    }
  } else if (YAP_IsAtomTerm(t)) {
    str_index +=
        sprintf(&str[str_index], "%s", YAP_AtomName(YAP_AtomOfTerm(t)));
    while (arity[0]) {
      if (arity[arity[0]] == 1) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else {
        if (arity[arity[0]] > 1)
          arity[arity[0]]--;
        str_index += sprintf(&str[str_index], ",");
        break;
      }
    }
  } else if (YAP_IsIntTerm(t)) {
    str_index += sprintf(&str[str_index], UInt_FORMAT, YAP_IntOfTerm(t));
    while (arity[0]) {
      if (arity[arity[0]] == 1) {
        str_index += sprintf(&str[str_index], ")");
        arity[0]--;
      } else {
        if (arity[arity[0]] > 1)
          arity[arity[0]]--;
        str_index += sprintf(&str[str_index], ",");
        break;
      }
    }
  } else if (YAP_IsPairTerm(t)) {
    if (t == FloatInitTag) {
      mode = TRIE_PRINT_FLOAT;
      arity[0]++;
      arity[arity[0]] = -1;
    } else if (t == PairInitTag) {
      str_index += sprintf(&str[str_index], "[");
      arity[0]++;
      arity[arity[0]] = -1;
    } else if (t == CommaInitTag) {
      str_index += sprintf(&str[str_index], "(");
      arity[0]++;
      arity[arity[0]] = -1;
    } else {
      if (t == PairEndEmptyTag)
        str[str_index - 1] = ']';
      else if (t == PairEndTermTag) {
        str[last_pair_mark] = '|';
        str[str_index - 1] = ']';
      } else /*   (t == CommaEndTag)   */
        str[str_index - 1] = ')';
      arity[0]--;
      while (arity[0]) {
        if (arity[arity[0]] == 1) {
          str_index += sprintf(&str[str_index], ")");
          arity[0]--;
        } else {
          if (arity[arity[0]] > 1)
            arity[arity[0]]--;
          str_index += sprintf(&str[str_index], ",");
          break;
        }
      }
    }
  } else if (ApplTag & t) {
    str_index +=
        sprintf(&str[str_index], "%s(",
                YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & t))));
    arity[0]++;
    arity[arity[0]] = YAP_ArityOfFunctor((YAP_Functor)(~ApplTag & t));
  } else {
    fprintf(stderr, "***************************************\n");
    fprintf(stderr, "  Tries core module: unknown type tag\n");
    fprintf(stderr, "***************************************\n");
    fflush(stderr);
  }

  if (arity[0]) {
    traverse_and_print(TrNode_child(node), arity, str, str_index, mode);
  } else {
    str[str_index] = 0;
    fprintf(stdout, "%s\n", str);
    if (DATA_PRINT_FUNCTION)
      (*DATA_PRINT_FUNCTION)(node);
  }
  return;
}

static YAP_Term trie_to_list(TrNode node) {
  YAP_Term tail = YAP_MkAtomTerm(YAP_LookupAtom("[]"));

#define CONSUME_NODE_LIST                                                      \
  do {                                                                         \
    /* add node result to list */                                              \
    tail = YAP_MkPairTerm(trie_to_list_node(node), tail);                      \
  } while ((node = TrNode_next(node)));

  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash)node;

    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);

    /* iterate through valid hash positions and consume each list */
    do {
      if (*--bucket) {
        node = *bucket;
        CONSUME_NODE_LIST;
      }
    } while (bucket != first_bucket);
  } else {
    CONSUME_NODE_LIST;
  }
#undef CONSUME_NODE_LIST

  /* return list of trie options at this level */
  return tail;
}

static YAP_Term trie_to_list_node(TrNode node) {
  YAP_Term t = TrNode_entry(node);

  if (YAP_IsIntTerm(t) || YAP_IsAtomTerm(t)) {
    return trie_to_list_create_two(YAP_IsIntTerm(t) ? "int" : "atom", node, t);
  } else if (YAP_IsVarTerm(t)) {
    int index = TrieVarIndex(t);
    YAP_Term index_term = YAP_MkIntTerm((YAP_Int)index);
    return trie_to_list_create_two("var", node, index_term);
  } else if (YAP_IsPairTerm(t)) {
    if (t == FloatInitTag) {
      node = TrNode_child(node); /* consume FloatInitTag */
      YAP_Functor f = YAP_MkFunctor(YAP_LookupAtom("floats"), 1);
      YAP_Term child = trie_to_list_floats(node);
      return YAP_MkApplTerm(f, 1, &child);
    } else if (t == PairInitTag) {
      return trie_to_list_create_simple("list", node);
    } else if (t == PairEndEmptyTag) {
      return trie_to_list_create_simple_end("endlist", node);
    } else if (t == CommaInitTag) {
      return trie_to_list_create_simple("comma", node);
    } else if (t == CommaEndTag) {
      return trie_to_list_create_simple_end("endcomma", node);
    }
  } else if (ApplTag & t) {
    YAP_Functor f = (YAP_Functor)(~ApplTag & t);
    int arity = YAP_ArityOfFunctor(f);
    YAP_Functor new_f = YAP_MkFunctor(YAP_LookupAtom("functor"), 3);
    YAP_Term args[3] = {YAP_MkAtomTerm(YAP_NameOfFunctor(f)),
                        YAP_MkIntTerm((YAP_Int)arity),
                        trie_to_list(TrNode_child(node))};
    return YAP_MkApplTerm(new_f, 3, args);
  }
  fprintf(stderr, "***************************************\n");
  fprintf(stderr, "  Tries core module: unknown type tag\n");
  fprintf(stderr, "***************************************\n");
  fflush(stderr);

  return YAP_MkAtomTerm(YAP_LookupAtom("fail"));
}

#define PUSH_NEW_FLOAT_TERM(val)                                               \
  result = YAP_MkPairTerm(trie_to_list_create_two("float", TrNode_child(node), \
                                                  YAP_MkFloatTerm(val)),       \
                          result);

#ifdef TAG_LOW_BITS_32

YAP_Term trie_to_list_floats_tag_low_32(YAP_Term result, TrNode node,
                                        volatile YAP_Term *p,
                                        volatile double *f) {
  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash)node;

    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);

    do {
      if (*--bucket) {
        node = *bucket;
        do {
          p[1] = TrNode_entry(node);
          PUSH_NEW_FLOAT_TERM(*f);
        } while ((node = TrNode_next(node)));
      }
    } while (bucket != first_bucket);
  } else {
    do {
      p[1] = TrNode_entry(node);
      PUSH_NEW_FLOAT_TERM(*f);
    } while ((node = TrNode_next(node)));
  }

  return result;
}
#endif /* TAG_LOW_BITS_32 */

static YAP_Term trie_to_list_floats(TrNode node) {
  volatile union {
    double f;
    YAP_Term p[SIZE_FLOAT_AS_TERM];
  } tf; /* to avoid gcc warning */
  YAP_Term result = YAP_MkAtomTerm(YAP_LookupAtom("[]"));

  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash)node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if (*--bucket) {
        node = *bucket;
        do {
          tf.p[0] = TrNode_entry(node);
#ifdef TAG_LOW_BITS_32
          result = trie_to_list_floats_tag_low_32(result, TrNode_child(node),
                                                  &tf.p, &tf.f);
#else
          PUSH_NEW_FLOAT_TERM(tf.f);
#endif /* TAG_LOW_BITS_32 */
        } while ((node = TrNode_next(node)));
      }
    } while (bucket != first_bucket);
  } else {
    do {
      tf.p[0] = TrNode_entry(node);
#ifdef TAG_LOW_BITS_32
      result = trie_to_list_floats_tag_low_32(result, TrNode_child(node), &tf.p,
                                              &tf.f);
#else
      PUSH_NEW_FLOAT_TERM(tf.f);
#endif /* TAG_LOW_BITS_32 */
    } while ((node = TrNode_next(node)));
  }

  return result;
}
#undef PUSH_NEW_FLOAT_TERM

#include "core_dbtries.c"
