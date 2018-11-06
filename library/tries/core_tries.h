/*********************************************
  File:     core_tries.h
  Author:   Ricardo Rocha
  Comments: Tries core module for Yap Prolog
  version:  $ID$
*********************************************/

/* -------------------------------------- */
/*           Yap Tagging Scheme           */
/* -------------------------------------- */

#include "YapInterface.h"
#if SIZEOF_INT_P == 4
#define TAG_LOW_BITS_32 /* 'Tags_32LowTag.h' tagging scheme */
#define SIZE_FLOAT_AS_TERM 2
#elif SIZEOF_INT_P == 8
#define TAG_64BITS /* 'Tags_64bits.h' tagging scheme */
#define SIZE_FLOAT_AS_TERM 1
#else
#error Unknown tagging scheme
#endif /* YAP_SCHEME */

/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#ifdef TAG_LOW_BITS_32
#define ApplTag 1          /* 0x01 */
#else                      /* TAG_64BITS */
#define ApplTag 5          /* 0x05 */
#endif                     /* TAG_SCHEME */
#define PairInitTag 3      /* 0x03 */
#define PairEndEmptyTag 19 /* 0x13 */
#define PairEndTermTag 99  /* 0x63 */
#define CommaInitTag 35    /* 0x23 */
#define CommaEndTag 51     /* 0x33 */
#define FloatInitTag 67    /* 0x43 */
#define FloatEndTag 83     /* 0x53 */

#define TRIE_MODE_STANDARD 0
#define TRIE_MODE_REVERSE 1
#define TRIE_MODE_MINIMAL 2
#define TRIE_MODE_REVMIN TRIE_MODE_REVERSE || TRIE_MODE_MINIMAL // 3

#define TRIE_PRINT_NORMAL 0
#define TRIE_PRINT_FLOAT 1
#define TRIE_PRINT_FLOAT2 2
#define TRIE_PRINT_FLOAT_END 3

#define BASE_AUXILIARY_TERM_STACK_SIZE 100000

/* --------------------------- */
/*           Structs           */
/* --------------------------- */

typedef struct trie_engine {
  struct trie_node *first_trie;
  /* in use */
  YAP_Int memory_in_use;
  YAP_Int tries_in_use;
  YAP_Int entries_in_use;
  YAP_Int nodes_in_use;
  /* max used */
  YAP_Int memory_max_used;
  YAP_Int tries_max_used;
  YAP_Int entries_max_used;
  YAP_Int nodes_max_used;
} * TrEngine;

#define TrEngine_trie(X) ((X)->first_trie)
#define TrEngine_memory(X) ((X)->memory_in_use)
#define TrEngine_tries(X) ((X)->tries_in_use)
#define TrEngine_entries(X) ((X)->entries_in_use)
#define TrEngine_nodes(X) ((X)->nodes_in_use)
#define TrEngine_memory_max(X) ((X)->memory_max_used)
#define TrEngine_tries_max(X) ((X)->tries_max_used)
#define TrEngine_entries_max(X) ((X)->entries_max_used)
#define TrEngine_nodes_max(X) ((X)->nodes_max_used)

typedef struct trie_node {
  struct trie_node *parent;
  struct trie_node *child;
  struct trie_node *next;
  struct trie_node *previous;
  YAP_Term entry;
} * TrNode;

#define TrNode_parent(X) ((X)->parent)
#define TrNode_child(X) ((X)->child)
#define TrNode_next(X) ((X)->next)
#define TrNode_previous(X) ((X)->previous)
#define TrNode_entry(X) ((X)->entry)

typedef struct trie_hash {
  struct trie_node
      *parent; /* for compatibility with the trie_node data structure */
  struct trie_node **buckets;
  int number_of_buckets;
  int number_of_nodes;
} * TrHash;

#define TrHash_mark(X) ((X)->parent)
#define TrHash_buckets(X) ((X)->buckets)
#define TrHash_bucket(X, N) ((X)->buckets + N)
#define TrHash_num_buckets(X) ((X)->number_of_buckets)
#define TrHash_seed(X) ((X)->number_of_buckets - 1)
#define TrHash_num_nodes(X) ((X)->number_of_nodes)

#define TYPE_TR_ENGINE struct trie_engine
#define TYPE_TR_NODE struct trie_node
#define TYPE_TR_HASH struct trie_hash
#define SIZEOF_TR_ENGINE sizeof(TYPE_TR_ENGINE)
#define SIZEOF_TR_NODE sizeof(TYPE_TR_NODE)
#define SIZEOF_TR_HASH sizeof(TYPE_TR_HASH)
#define SIZEOF_TR_BUCKET sizeof(TYPE_TR_NODE *)

#define AS_TR_NODE_NEXT(ADDR)                                                  \
  (TrNode)((YAP_UInt)(ADDR)-2 * sizeof(struct trie_node *))

/* --------------------------- */
/*           Macros            */
/* --------------------------- */

#define TAG_ADDR(ADDR) ((YAP_UInt)(ADDR) | 0x1)
#define UNTAG_ADDR(ADDR) ((YAP_UInt)(ADDR) & ~(0x1))
#define PUT_DATA_IN_LEAF_TRIE_NODE(TR_NODE, DATA)                              \
  TrNode_child(TR_NODE) = (TrNode)TAG_ADDR(DATA)
#define GET_DATA_FROM_LEAF_TRIE_NODE(TR_NODE) UNTAG_ADDR(TrNode_child(TR_NODE))
#define MARK_AS_LEAF_TRIE_NODE(TR_NODE)                                        \
  PUT_DATA_IN_LEAF_TRIE_NODE(TR_NODE, TrNode_child(TR_NODE))
#define IS_LEAF_TRIE_NODE(TR_NODE) ((YAP_UInt)(TrNode_child(TR_NODE)) & 0x1)

#define IsTrieVar(TERM, STACK, STACK_BASE)                                     \
  ((YAP_Term *)(TERM) > STACK && (YAP_Term *)(TERM) <= STACK_BASE)
#define MkTrieVar(INDEX) ((INDEX) << 4)
#define TrieVarIndex(TERM) ((TERM) >> 4)

#define BASE_HASH_BUCKETS 256
#define MAX_NODES_PER_TRIE_LEVEL 32
#define MAX_NODES_PER_BUCKET (MAX_NODES_PER_TRIE_LEVEL / 2)
#define HASH_TERM(TERM, SEED) (((TERM) >> 4) & (SEED))
#define IS_HASH_NODE(NODE) (TrHash_mark(NODE) == NULL)

#define BASE_SAVE_MARK                                                         \
  10000 /* could lead to errors if the number of different variables in a term \
           is greater than it */
#define HASH_SAVE_MARK ((YAP_Term)MkTrieVar(BASE_SAVE_MARK))
#define ATOM_SAVE_MARK ((YAP_Term)MkTrieVar(BASE_SAVE_MARK + 1))
#define FUNCTOR_SAVE_MARK ((YAP_Term)MkTrieVar(BASE_SAVE_MARK + 2))
#define FLOAT_SAVE_MARK ((YAP_Term)MkTrieVar(BASE_SAVE_MARK + 3))

#define STACK_NOT_EMPTY(STACK, STACK_BASE) STACK != STACK_BASE
#define POP_UP(STACK) *--STACK
#define POP_DOWN(STACK) *++STACK
#define PUSH_UP(STACK, ITEM, STACK_TOP)                                        \
  {                                                                            \
    if (STACK < STACK_TOP) {                                                   \
      fprintf(stderr, "**************************************\n");             \
      fprintf(stderr, "  Tries core module: term stack full\n");               \
      fprintf(stderr, "**************************************\n");             \
    }                                                                          \
    *STACK = (YAP_Term)(ITEM);                                                 \
    STACK--;                                                                   \
  }
#define PUSH_DOWN(STACK, ITEM, STACK_TOP)                                      \
  {                                                                            \
    if (STACK > STACK_TOP) {                                                   \
      fprintf(stderr, "**************************************\n");             \
      fprintf(stderr, "  Tries core module: term stack empty\n");              \
      fprintf(stderr, "**************************************\n");             \
    }                                                                          \
    *STACK = (YAP_Term)(ITEM);                                                 \
    STACK++;                                                                   \
  }

#define new_struct(STR, STR_TYPE, STR_SIZE)                                    \
  STR = (STR_TYPE *)YAP_AllocSpaceFromYap(STR_SIZE)
#define new_trie_engine(TR_ENGINE)                                             \
  {                                                                            \
    new_struct(TR_ENGINE, TYPE_TR_ENGINE, SIZEOF_TR_ENGINE);                   \
    TrEngine_trie(TR_ENGINE) = NULL;                                           \
    TrEngine_memory(TR_ENGINE) = 0;                                            \
    TrEngine_tries(TR_ENGINE) = 0;                                             \
    TrEngine_entries(TR_ENGINE) = 0;                                           \
    TrEngine_nodes(TR_ENGINE) = 0;                                             \
    TrEngine_memory_max(TR_ENGINE) = 0;                                        \
    TrEngine_tries_max(TR_ENGINE) = 0;                                         \
    TrEngine_entries_max(TR_ENGINE) = 0;                                       \
    TrEngine_nodes_max(TR_ENGINE) = 0;                                         \
  }
#define new_trie_node(TR_NODE, ENTRY, PARENT, CHILD, NEXT, PREVIOUS)           \
  {                                                                            \
    new_struct(TR_NODE, TYPE_TR_NODE, SIZEOF_TR_NODE);                         \
    TrNode_entry(TR_NODE) = ENTRY;                                             \
    TrNode_parent(TR_NODE) = PARENT;                                           \
    TrNode_child(TR_NODE) = CHILD;                                             \
    TrNode_next(TR_NODE) = NEXT;                                               \
    TrNode_previous(TR_NODE) = PREVIOUS;                                       \
    INCREMENT_NODES(CURRENT_TRIE_ENGINE);                                      \
    INCREMENT_MEMORY(CURRENT_TRIE_ENGINE, SIZEOF_TR_NODE);                     \
  }
#define new_trie_hash(TR_HASH, NUM_NODES, NUM_BUCKETS)                         \
  {                                                                            \
    new_struct(TR_HASH, TYPE_TR_HASH, SIZEOF_TR_HASH);                         \
    TrHash_mark(TR_HASH) = NULL;                                               \
    TrHash_num_buckets(TR_HASH) = NUM_BUCKETS;                                 \
    new_hash_buckets(TR_HASH, NUM_BUCKETS);                                    \
    TrHash_num_nodes(TR_HASH) = NUM_NODES;                                     \
    INCREMENT_MEMORY(CURRENT_TRIE_ENGINE, SIZEOF_TR_HASH);                     \
  }
#define new_hash_buckets(TR_HASH, NUM_BUCKETS)                                 \
  {                                                                            \
    int i;                                                                     \
    void **ptr;                                                                \
    new_struct(ptr, void *, NUM_BUCKETS * sizeof(void *));                     \
    TrHash_buckets(TR_HASH) = (TYPE_TR_NODE **)ptr;                            \
    for (i = NUM_BUCKETS; i != 0; i--)                                         \
      *ptr++ = NULL;                                                           \
    INCREMENT_MEMORY(CURRENT_TRIE_ENGINE, (NUM_BUCKETS)*SIZEOF_TR_BUCKET);     \
  }

#define expand_auxiliary_term_stack()                                          \
  {                                                                            \
    YAP_Term *aux_stack;                                                       \
    YAP_Int aux_size = CURRENT_AUXILIARY_TERM_STACK_SIZE * sizeof(YAP_Term);   \
    new_struct(aux_stack, YAP_Term, aux_size * 2);                             \
    memmove(aux_stack, AUXILIARY_TERM_STACK, aux_size);                        \
    free_struct(AUXILIARY_TERM_STACK);                                         \
    AUXILIARY_TERM_STACK = aux_stack;                                          \
    CURRENT_AUXILIARY_TERM_STACK_SIZE *= 2;                                    \
  }

#define free_struct(STR) YAP_FreeSpaceFromYap((char *)(STR))
#define free_trie_node(STR)                                                    \
  {                                                                            \
    free_struct(STR);                                                          \
    DECREMENT_NODES(CURRENT_TRIE_ENGINE);                                      \
    DECREMENT_MEMORY(CURRENT_TRIE_ENGINE, SIZEOF_TR_NODE);                     \
  }
#define free_trie_hash(STR)                                                    \
  {                                                                            \
    free_struct(STR);                                                          \
    DECREMENT_MEMORY(CURRENT_TRIE_ENGINE, SIZEOF_TR_HASH);                     \
  }
#define free_hash_buckets(STR, NUM_BUCKETS)                                    \
  {                                                                            \
    free_struct(STR);                                                          \
    DECREMENT_MEMORY(CURRENT_TRIE_ENGINE, (NUM_BUCKETS)*SIZEOF_TR_BUCKET);     \
  }

#define INCREMENT_MEMORY(TR_ENGINE, SIZE)                                      \
  {                                                                            \
    TrEngine_memory(TR_ENGINE) += SIZE;                                        \
    if (TrEngine_memory(TR_ENGINE) > TrEngine_memory_max(TR_ENGINE))           \
      TrEngine_memory_max(TR_ENGINE) = TrEngine_memory(TR_ENGINE);             \
  }
#define INCREMENT_TRIES(TR_ENGINE)                                             \
  {                                                                            \
    TrEngine_tries(TR_ENGINE)++;                                               \
    if (TrEngine_tries(TR_ENGINE) > TrEngine_tries_max(TR_ENGINE))             \
      TrEngine_tries_max(TR_ENGINE) = TrEngine_tries(TR_ENGINE);               \
  }
#define INCREMENT_ENTRIES(TR_ENGINE)                                           \
  {                                                                            \
    TrEngine_entries(TR_ENGINE)++;                                             \
    if (TrEngine_entries(TR_ENGINE) > TrEngine_entries_max(TR_ENGINE))         \
      TrEngine_entries_max(TR_ENGINE) = TrEngine_entries(TR_ENGINE);           \
  }
#define INCREMENT_NODES(TR_ENGINE)                                             \
  {                                                                            \
    TrEngine_nodes(TR_ENGINE)++;                                               \
    if (TrEngine_nodes(TR_ENGINE) > TrEngine_nodes_max(TR_ENGINE))             \
      TrEngine_nodes_max(TR_ENGINE) = TrEngine_nodes(TR_ENGINE);               \
  }
#define DECREMENT_MEMORY(TR_ENGINE, SIZE) TrEngine_memory(TR_ENGINE) -= SIZE
#define DECREMENT_TRIES(TR_ENGINE) TrEngine_tries(TR_ENGINE)--
#define DECREMENT_ENTRIES(TR_ENGINE) TrEngine_entries(TR_ENGINE)--
#define DECREMENT_NODES(TR_ENGINE) TrEngine_nodes(TR_ENGINE)--

#define IS_FUNCTOR_NODE(N)                                                     \
  (((ApplTag & TrNode_entry(N)) == ApplTag) &&                                 \
   (TrNode_entry(N) != PairInitTag) && (TrNode_entry(N) != PairEndEmptyTag) && \
   (TrNode_entry(N) != PairEndTermTag))

/* --------------------------- */
/*             API             */
/* --------------------------- */

extern TrEngine core_trie_init_module(void);
extern TrNode core_trie_open(TrEngine engine);
extern void core_trie_close(TrEngine engine, TrNode node,
                            void (*destruct_function)(TrNode));
extern void core_trie_close_all(TrEngine engine,
                                void (*destruct_function)(TrNode));
extern void core_trie_set_mode(YAP_Int mode);
extern YAP_Int core_trie_get_mode(void);
extern TrNode core_trie_put_entry(TrEngine engine, TrNode node, YAP_Term entry,
                                  YAP_Int *depth);
extern TrNode core_trie_check_entry(TrNode node, YAP_Term entry);
extern YAP_Term core_trie_get_entry(TrNode node);
extern void core_trie_remove_entry(TrEngine engine, TrNode node,
                                   void (*destruct_function)(TrNode));
extern void core_trie_remove_subtree(TrEngine engine, TrNode node,
                                     void (*destruct_function)(TrNode));
extern void core_trie_add(TrNode node_dest, TrNode node_source,
                          void (*add_function)(TrNode, TrNode));
extern void core_trie_join(TrEngine engine, TrNode node_dest,
                           TrNode node_source,
                           void (*add_function)(TrNode, TrNode),
                           void (*copy_function)(TrNode, TrNode));
extern void core_trie_intersect(TrEngine engine, TrNode node_dest,
                                TrNode node_source,
                                void (*add_function)(TrNode, TrNode),
                                void (*destruct_function)(TrNode));
extern YAP_Int core_trie_count_join(TrNode node1, TrNode node2);
extern YAP_Int core_trie_count_intersect(TrNode node1, TrNode node2);
extern void core_trie_save(TrNode node, FILE *file,
                           void (*save_function)(TrNode, FILE *));
extern TrNode core_trie_load(TrEngine engine, FILE *file,
                             void (*load_function)(TrNode, YAP_Int, FILE *));
extern void core_trie_stats(TrEngine engine, YAP_Int *memory, YAP_Int *tries,
                            YAP_Int *entries, YAP_Int *nodes);
extern void core_trie_max_stats(TrEngine engine, YAP_Int *memory,
                                YAP_Int *tries, YAP_Int *entries,
                                YAP_Int *nodes);
extern void core_trie_usage(TrNode node, YAP_Int *entries, YAP_Int *nodes,
                            YAP_Int *virtual_nodes);
extern void core_trie_print(TrNode node, void (*print_function)(TrNode));

extern void core_disable_hash_table(void);
extern void core_enable_hash_table(void);

extern YAP_Term core_trie_to_list(TrNode node);

#include "core_dbtries.h"
