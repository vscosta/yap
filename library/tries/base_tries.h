/*********************************************
  File:     base_tries.h
  Author:   Ricardo Rocha
  Comments: Tries base module for Yap Prolog
  version:  $ID$
*********************************************/



/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#define TRAVERSE_MODE_FORWARD   0
#define TRAVERSE_MODE_BACKWARD  1


/* --------------------------- */
/*           Structs           */
/* --------------------------- */

typedef struct trie_entry {
  struct trie_node  *top_trie_node;
  struct trie_data  *first_trie_data;
  struct trie_data  *last_trie_data;
  struct trie_data  *traverse_trie_data;
  struct trie_entry *next;
  struct trie_entry *previous;
} *TrEntry;

#define TrEntry_trie(X)           ((X)->top_trie_node)
#define TrEntry_first_data(X)     ((X)->first_trie_data)
#define TrEntry_last_data(X)      ((X)->last_trie_data)
#define TrEntry_traverse_data(X)  ((X)->traverse_trie_data)
#define TrEntry_next(X)           ((X)->next)
#define TrEntry_previous(X)       ((X)->previous)

typedef struct trie_data {
  struct trie_entry *trie;
  struct trie_node *leaf_trie_node;
  struct trie_data *next;
  struct trie_data *previous;
} *TrData;

#define TrData_trie(X)      ((X)->trie)
#define TrData_leaf(X)      ((X)->leaf_trie_node)
#define TrData_next(X)      ((X)->next)
#define TrData_previous(X)  ((X)->previous)

#define TYPE_TR_ENTRY         struct trie_entry
#define TYPE_TR_DATA          struct trie_data
#define SIZEOF_TR_ENTRY       sizeof(TYPE_TR_ENTRY)
#define SIZEOF_TR_DATA        sizeof(TYPE_TR_DATA)

#define AS_TR_ENTRY_NEXT(ADDR) (TrEntry)((unsigned long int)(ADDR) - sizeof(struct trie_node *) - 3 * sizeof(struct trie_data *))
#define AS_TR_DATA_NEXT(ADDR)  (TrData)((unsigned long int)(ADDR) - sizeof(struct trie_entry *) - sizeof(struct trie_node *))



/* --------------------------- */
/*           Macros            */
/* --------------------------- */

#define new_trie_entry(TR_ENTRY, TR_NODE)                                                \
        { new_struct(TR_ENTRY, TYPE_TR_ENTRY, SIZEOF_TR_ENTRY);                          \
          TrEntry_trie(TR_ENTRY) = TR_NODE;                                              \
          TrEntry_first_data(TR_ENTRY) = NULL;                                           \
          TrEntry_last_data(TR_ENTRY) = AS_TR_DATA_NEXT(&TrEntry_first_data(TR_ENTRY));  \
          TrEntry_traverse_data(TR_ENTRY) = NULL;                                        \
          TrEntry_next(TR_ENTRY) = FIRST_TRIE;                                           \
          TrEntry_previous(TR_ENTRY) = AS_TR_ENTRY_NEXT(&FIRST_TRIE);                    \
          INCREMENT_MEMORY(TRIE_ENGINE, SIZEOF_TR_ENTRY);                                \
	}
#define new_trie_data(TR_DATA, TR_ENTRY, TR_NODE)                                        \
        { TrData first_data = TrEntry_first_data(TR_ENTRY);                              \
          new_struct(TR_DATA, TYPE_TR_DATA, SIZEOF_TR_DATA);                             \
          TrData_trie(TR_DATA) = TR_ENTRY;                                               \
          TrData_leaf(TR_DATA) = TR_NODE;                                                \
          TrData_next(TR_DATA) = NULL;                                                   \
          if (first_data) {                                                              \
            TrData last_data = TrEntry_last_data(TR_ENTRY);                              \
            TrData_next(last_data) = TR_DATA;	         	                         \
            TrData_previous(TR_DATA) = last_data;		                         \
          } else { 	                                                                 \
            TrData_previous(TR_DATA) = AS_TR_DATA_NEXT(&TrEntry_first_data(TR_ENTRY));   \
            TrEntry_first_data(TR_ENTRY) = TR_DATA;                                      \
          }                                                                              \
          TrEntry_last_data(TR_ENTRY) = TR_DATA;                                         \
          INCREMENT_MEMORY(TRIE_ENGINE, SIZEOF_TR_DATA);                                 \
        } 



#define free_trie_entry(STR)                                                           \
        { free_struct(STR);                                                            \
          DECREMENT_MEMORY(TRIE_ENGINE, SIZEOF_TR_ENTRY);                              \
        }
#define free_trie_data(STR)                                                            \
        { free_struct(STR);                                                            \
          DECREMENT_MEMORY(TRIE_ENGINE, SIZEOF_TR_DATA);                               \
	}



/* --------------------------- */
/*             API             */
/* --------------------------- */

inline void     trie_init_module(void);
inline void     trie_data_load(TrNode node, YAP_Int depth, FILE *file);
inline void     trie_data_copy(TrNode node_dest, TrNode node_source);
inline void     trie_data_destruct(TrNode node);
inline TrEntry  trie_open(void);
inline void     trie_close(TrEntry trie);
inline void     trie_close_all(void);
inline void     trie_set_mode(YAP_Int mode);
inline YAP_Int  trie_get_mode(void);
inline TrData   trie_put_entry(TrEntry trie, YAP_Term entry);
inline TrData   trie_check_entry(TrEntry trie, YAP_Term entry);
inline YAP_Term trie_get_entry(TrData data);
inline TrData   trie_get_first_entry(TrEntry trie);
inline TrData   trie_get_last_entry(TrEntry trie);
inline TrData   trie_traverse_init(TrEntry trie, TrData init_data);
inline TrData   trie_traverse_cont(TrEntry trie);
inline void     trie_remove_entry(TrData data);
inline void     trie_remove_subtree(TrData data);
inline void     trie_join(TrEntry trie_dest, TrEntry trie_source);
inline void     trie_intersect(TrEntry trie_dest, TrEntry trie_source);
inline YAP_Int  trie_count_join(TrEntry trie1, TrEntry trie2);
inline YAP_Int  trie_count_intersect(TrEntry trie1, TrEntry trie2);
inline void     trie_save(TrEntry trie, FILE *file);
inline TrEntry  trie_load(FILE *file);
inline void     trie_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes);
inline void     trie_max_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes);
inline void     trie_usage(TrEntry trie, YAP_Int *entries, YAP_Int *nodes, YAP_Int *virtual_nodes);
inline void     trie_print(TrEntry trie);

inline void     trie_data_construct(TrNode node);
inline void     trie_set_traverse_mode(YAP_Int mode);
inline YAP_Int  trie_get_traverse_mode(void);
inline TrData   trie_traverse_first(TrEntry trie);
inline TrData   trie_traverse_next(TrData data);
inline void     trie_disable_hash_table(void);
inline void     trie_enable_hash_table(void);

inline YAP_Term trie_to_list(TrEntry trie);

#include "base_dbtries.h"
