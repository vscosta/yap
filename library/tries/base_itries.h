/*********************************
  File:     base_itries.h
  Author:   Ricardo Rocha
  Comments: Tries module for ILP
  version:  $ID$
*********************************/



/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#define ITRIES_MODE_NONE      0
#define ITRIES_MODE_INC_POS   1
#define ITRIES_MODE_DEC_POS   2
#define ITRIES_MODE_INC_NEG   3
#define ITRIES_MODE_DEC_NEG   4
#define BASE_TR_DATA_BUCKETS 20



/* --------------------------- */
/*           Structs           */
/* --------------------------- */

typedef struct itrie_entry {
  struct trie_node *top_trie_node;
  struct itrie_data **trie_data_buckets;
  struct itrie_data *traverse_trie_data;
  struct itrie_entry *next;
  struct itrie_entry *previous;
  YAP_Int mode;
  YAP_Int timestamp;
  YAP_Int number_of_buckets;
  YAP_Int traverse_bucket;
} *TrEntry;

#define TrEntry_trie(X)            ((X)->top_trie_node)
#define TrEntry_buckets(X)         ((X)->trie_data_buckets)
#define TrEntry_bucket(X,N)        ((X)->trie_data_buckets + N)
#define TrEntry_traverse_data(X)   ((X)->traverse_trie_data)
#define TrEntry_next(X)            ((X)->next)
#define TrEntry_previous(X)        ((X)->previous)
#define TrEntry_mode(X)            ((X)->mode)
#define TrEntry_timestamp(X)       ((X)->timestamp)
#define TrEntry_num_buckets(X)     ((X)->number_of_buckets)
#define TrEntry_traverse_bucket(X) ((X)->traverse_bucket)

typedef struct itrie_data {
  struct itrie_entry *itrie;
  struct trie_node *leaf_trie_node;
  struct itrie_data *next;
  struct itrie_data *previous;
  YAP_Int pos;
  YAP_Int neg;
  YAP_Int timestamp;
  YAP_Int depth;
} *TrData;

#define TrData_itrie(X)     ((X)->itrie)
#define TrData_leaf(X)      ((X)->leaf_trie_node)
#define TrData_next(X)      ((X)->next)
#define TrData_previous(X)  ((X)->previous)
#define TrData_pos(X)       ((X)->pos)
#define TrData_neg(X)       ((X)->neg)
#define TrData_timestamp(X) ((X)->timestamp)
#define TrData_depth(X)     ((X)->depth)

#define TYPE_TR_ENTRY         struct itrie_entry
#define TYPE_TR_DATA          struct itrie_data
#define SIZEOF_TR_ENTRY       sizeof(TYPE_TR_ENTRY)
#define SIZEOF_TR_DATA        sizeof(TYPE_TR_DATA)
#define SIZEOF_TR_DATA_BUCKET sizeof(TYPE_TR_DATA *)

#define AS_TR_ENTRY_NEXT(ADDR) (TrEntry)((YAP_UInt)(ADDR) - sizeof(struct trie_node *) - sizeof(struct itrie_data **) - sizeof(struct itrie_data *))
#define AS_TR_DATA_NEXT(ADDR)  (TrData)((YAP_UInt)(ADDR) - sizeof(struct itrie_entry *) - sizeof(struct trie_node *))



/* --------------------------- */
/*           Macros            */
/* --------------------------- */

#define new_itrie_entry(TR_ENTRY, TR_NODE)                                       \
        { new_struct(TR_ENTRY, TYPE_TR_ENTRY, SIZEOF_TR_ENTRY);                  \
          TrEntry_mode(TR_ENTRY) = ITRIES_MODE_NONE;                             \
          TrEntry_timestamp(TR_ENTRY) = -1;                                      \
          TrEntry_num_buckets(TR_ENTRY) = BASE_TR_DATA_BUCKETS;                  \
          new_itrie_buckets(TR_ENTRY, BASE_TR_DATA_BUCKETS);                     \
          TrEntry_trie(TR_ENTRY) = TR_NODE;                                      \
          TrEntry_next(TR_ENTRY) = FIRST_ITRIE;                                  \
          TrEntry_previous(TR_ENTRY) = AS_TR_ENTRY_NEXT(&FIRST_ITRIE);           \
          INCREMENT_MEMORY(ITRIE_ENGINE, SIZEOF_TR_ENTRY);                       \
	}
#define new_itrie_buckets(TR_ENTRY, NUM_BUCKETS)                                 \
        { YAP_Int i; void **ptr;                                                 \
          new_struct(ptr, void *, NUM_BUCKETS * sizeof(void *));                 \
          TrEntry_buckets(TR_ENTRY)  = (TYPE_TR_DATA **) ptr;                    \
          for (i = NUM_BUCKETS; i != 0; i--)                                     \
            *ptr++ = NULL;                                                       \
          INCREMENT_MEMORY(ITRIE_ENGINE, (NUM_BUCKETS) * SIZEOF_TR_DATA_BUCKET); \
        }
#define new_itrie_data(TR_DATA, TR_ENTRY, TR_NODE, POS, NEG, TIME, DEPTH)        \
        { TrData *bucket;                                                        \
          new_struct(TR_DATA, TYPE_TR_DATA, SIZEOF_TR_DATA);                     \
          TrData_pos(TR_DATA) = POS;                                             \
          TrData_neg(TR_DATA) = NEG;                                             \
          TrData_timestamp(TR_DATA) = TIME;                                      \
          TrData_depth(TR_DATA) = DEPTH;                                         \
          TrData_itrie(TR_DATA) = TR_ENTRY;                                      \
          TrData_leaf(TR_DATA) = TR_NODE;                                        \
          if (DEPTH >= TrEntry_num_buckets(TR_ENTRY)) {                          \
            YAP_Int i, new_num_buckets = DEPTH + BASE_TR_DATA_BUCKETS;           \
            bucket = TrEntry_buckets(TR_ENTRY);                                  \
            new_itrie_buckets(TR_ENTRY, new_num_buckets);                        \
            memmove(TrEntry_buckets(TR_ENTRY), bucket,                            \
                   TrEntry_num_buckets(TR_ENTRY) * SIZEOF_TR_DATA_BUCKET);       \
            free_itrie_buckets(bucket, TrEntry_num_buckets(TR_ENTRY));           \
            bucket = TrEntry_buckets(TR_ENTRY);                                  \
            for (i = 0; i != TrEntry_num_buckets(TR_ENTRY); i++) {               \
               if (*bucket)                                                      \
                 TrData_previous(*bucket) = AS_TR_DATA_NEXT(bucket);             \
                bucket++;                                                        \
	    }                                                                    \
            TrEntry_num_buckets(TR_ENTRY) = new_num_buckets;                     \
	  }                                                                      \
          bucket = TrEntry_bucket(TR_ENTRY, DEPTH);                              \
          TrData_next(TR_DATA) = *bucket;                                        \
          TrData_previous(TR_DATA) = AS_TR_DATA_NEXT(bucket);                    \
          if (*bucket)                                                           \
            TrData_previous(*bucket) = TR_DATA;                                  \
          *bucket = TR_DATA;                                                     \
          INCREMENT_MEMORY(ITRIE_ENGINE, SIZEOF_TR_DATA);                        \
        }
#define update_itrie_data(TR_DATA, TIMESTAMP, ITRIES_MODE)                       \
        if (TrData_timestamp(TR_DATA) != TIMESTAMP) {                            \
          if (ITRIES_MODE == ITRIES_MODE_INC_POS)                                \
            TrData_pos(TR_DATA)++;                                               \
          else if (ITRIES_MODE == ITRIES_MODE_DEC_POS)                           \
            TrData_pos(TR_DATA)--;                                               \
          else if (ITRIES_MODE == ITRIES_MODE_INC_NEG)                           \
            TrData_neg(TR_DATA)++;                                               \
          else if (ITRIES_MODE == ITRIES_MODE_DEC_NEG)                           \
            TrData_neg(TR_DATA)--;                                               \
          TrData_timestamp(TR_DATA) = TIMESTAMP;                                 \
        } 



#define free_itrie_entry(STR)                                                    \
        { free_itrie_buckets(TrEntry_buckets(STR), TrEntry_num_buckets(STR));    \
          free_struct(STR);                                                      \
          DECREMENT_MEMORY(ITRIE_ENGINE, SIZEOF_TR_ENTRY);                       \
        }
#define free_itrie_buckets(STR, NUM_BUCKETS)                                     \
        { free_struct(STR);                                                      \
          DECREMENT_MEMORY(ITRIE_ENGINE, (NUM_BUCKETS) * SIZEOF_TR_DATA_BUCKET); \
        }
#define free_itrie_data(STR)                                                     \
        { free_struct(STR);                                                      \
          DECREMENT_MEMORY(ITRIE_ENGINE, SIZEOF_TR_DATA);                        \
	}



/* --------------------------- */
/*             API             */
/* --------------------------- */

void     itrie_init_module(void);
void     itrie_data_save(TrNode node, FILE *file);
void     itrie_data_load(TrNode node, YAP_Int depth, FILE *file);
void     itrie_data_print(TrNode node);
void     itrie_data_copy(TrNode node_dest, TrNode node_source);
void     itrie_data_destruct(TrNode node);
void     itrie_data_add(TrNode node_dest, TrNode node_source);
void     itrie_data_subtract(TrNode node_dest, TrNode node_source);
TrEntry  itrie_open(void);
void     itrie_close(TrEntry itrie);
void     itrie_close_all(void);
void     itrie_set_mode(TrEntry itrie, YAP_Int mode);
YAP_Int  itrie_get_mode(TrEntry itrie);
void     itrie_set_timestamp(TrEntry itrie, YAP_Int timestamp);
YAP_Int  itrie_get_timestamp(TrEntry itrie);
void     itrie_put_entry(TrEntry itrie, YAP_Term entry);
void     itrie_update_entry(TrEntry itrie, YAP_Term entry);
TrData   itrie_check_entry(TrEntry itrie, YAP_Term entry);
YAP_Term itrie_get_entry(TrData data);
void     itrie_get_data(TrData data, YAP_Int *pos, YAP_Int *neg, YAP_Int *timestamp);
TrData   itrie_traverse_init(TrEntry itrie);
TrData   itrie_traverse_cont(TrEntry itrie);
void     itrie_remove_entry(TrData data);
void     itrie_remove_subtree(TrData data);
void     itrie_add(TrEntry itrie_dest, TrEntry itrie_source);
void     itrie_subtract(TrEntry itrie_dest, TrEntry itrie_source);
void     itrie_join(TrEntry itrie_dest, TrEntry itrie_source);
void     itrie_intersect(TrEntry itrie_dest, TrEntry itrie_source);
YAP_Int  itrie_count_join(TrEntry itrie1, TrEntry itrie2);
YAP_Int  itrie_count_intersect(TrEntry itrie1, TrEntry itrie2);
void     itrie_save(TrEntry itrie, FILE *file);
void     itrie_save_as_trie(TrEntry itrie, FILE *file);
TrEntry  itrie_load(FILE *file);
void     itrie_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes);
void     itrie_max_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes);
void     itrie_usage(TrEntry itrie, YAP_Int *entries, YAP_Int *nodes, YAP_Int *virtual_nodes);
void     itrie_print(TrEntry itrie);
