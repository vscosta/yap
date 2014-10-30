/*********************************
  File:     base_itries.c
  Author:   Ricardo Rocha
  Comments: Tries module for ILP
  version:  $ID$
*********************************/



/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <YapInterface.h>
#include <stdio.h>
#include <string.h>
#include "core_tries.h"
#include "base_itries.h"



/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

static TrEngine ITRIE_ENGINE;
static TrEntry FIRST_ITRIE, CURRENT_ITRIE;



/* -------------------------- */
/*            API             */     
/* -------------------------- */


void itrie_init_module(void) {
  ITRIE_ENGINE = core_trie_init_module();
  FIRST_ITRIE = NULL;
  return;
}



void itrie_data_save(TrNode node, FILE *file) {
  TrData data;

  data = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node);
  fprintf(file, Int_FORMAT " " Int_FORMAT " " Int_FORMAT " ", TrData_pos(data), TrData_neg(data), TrData_timestamp(data));
  return;
}



void itrie_data_load(TrNode node, YAP_Int depth, FILE *file) {
  TrData data;
  YAP_Int pos, neg, timestamp;

  fscanf(file,  Int_FORMAT " "  Int_FORMAT " "  Int_FORMAT, &pos, &neg, &timestamp);
  new_itrie_data(data, CURRENT_ITRIE, node, pos, neg, timestamp, depth);
  PUT_DATA_IN_LEAF_TRIE_NODE(node, data);
  return;
}



void itrie_data_print(TrNode node) {
  TrData data;

  data = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node);
  printf("   pos: " Int_FORMAT " neg: " Int_FORMAT "  timestamp: " Int_FORMAT "\\n", TrData_pos(data), TrData_neg(data), TrData_timestamp(data));
  return;
}



void itrie_data_copy(TrNode node_dest, TrNode node_source) {
  TrData data_dest, data_source;

  data_source = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node_source);
  new_itrie_data(data_dest, CURRENT_ITRIE, node_dest, TrData_pos(data_source), TrData_neg(data_source), TrData_timestamp(data_source), TrData_depth(data_source));
  PUT_DATA_IN_LEAF_TRIE_NODE(node_dest, data_dest);
  return;
}



void itrie_data_destruct(TrNode node) {
  TrEntry itrie;
  TrData data;

  data = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node);
  itrie = TrData_itrie(data);
  if (data == TrEntry_traverse_data(itrie))
    TrEntry_traverse_data(itrie) = TrData_next(data);
  if (TrData_next(data)) {
    TrData_previous(TrData_next(data)) = TrData_previous(data);
    TrData_next(TrData_previous(data)) = TrData_next(data);
  } else
    TrData_next(TrData_previous(data)) = NULL;
  free_itrie_data(data);
  return;
}



void itrie_data_add(TrNode node_dest, TrNode node_source) {
  TrData data_dest, data_source;

  data_dest = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node_dest);
  data_source = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node_source);
  TrData_pos(data_dest) += TrData_pos(data_source);
  TrData_neg(data_dest) += TrData_neg(data_source);
  if (TrData_timestamp(data_dest) < TrData_timestamp(data_source))
    TrData_timestamp(data_dest) = TrData_timestamp(data_source);
  return;
}



void itrie_data_subtract(TrNode node_dest, TrNode node_source) {
  TrData data_dest, data_source;

  data_dest = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node_dest);
  data_source = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node_source);
  TrData_pos(data_dest) -= TrData_pos(data_source);
  TrData_neg(data_dest) -= TrData_neg(data_source);
  if (TrData_timestamp(data_dest) < TrData_timestamp(data_source))
    TrData_timestamp(data_dest) = TrData_timestamp(data_source);
  return;
}



TrEntry itrie_open(void) {
  TrEntry itrie;
  TrNode node;

  node = core_trie_open(ITRIE_ENGINE);
  new_itrie_entry(itrie, node);
  if (FIRST_ITRIE)
    TrEntry_previous(FIRST_ITRIE) = itrie;
  FIRST_ITRIE = itrie;
  return itrie;
}



void itrie_close(TrEntry itrie) {
  core_trie_close(ITRIE_ENGINE, TrEntry_trie(itrie), &itrie_data_destruct);
  if (TrEntry_next(itrie)) {
    TrEntry_previous(TrEntry_next(itrie)) = TrEntry_previous(itrie);
    TrEntry_next(TrEntry_previous(itrie)) = TrEntry_next(itrie);
  } else
    TrEntry_next(TrEntry_previous(itrie)) = NULL;
  free_itrie_entry(itrie);  
  return;
}



void itrie_close_all(void) {
  TrEntry itrie;

  core_trie_close_all(ITRIE_ENGINE, &itrie_data_destruct);
  while (FIRST_ITRIE) {
    itrie = TrEntry_next(FIRST_ITRIE);
    free_itrie_entry(FIRST_ITRIE);
    FIRST_ITRIE = itrie;
  }
  return;
}



void itrie_set_mode(TrEntry itrie, YAP_Int mode) {
  TrEntry_mode(itrie) = mode;
  return;
}



YAP_Int itrie_get_mode(TrEntry itrie) {
  return TrEntry_mode(itrie);
}



void itrie_set_timestamp(TrEntry itrie, YAP_Int timestamp) {
  TrEntry_timestamp(itrie) = timestamp;
  return;
}



YAP_Int itrie_get_timestamp(TrEntry itrie) {
  return TrEntry_timestamp(itrie);
}



void itrie_put_entry(TrEntry itrie, YAP_Term entry) {
  TrData data;
  TrNode node;
  YAP_Int depth;

  node = core_trie_put_entry(ITRIE_ENGINE, TrEntry_trie(itrie), entry, &depth);
  if (!(data = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node))) {
    new_itrie_data(data, itrie, node, 0, 0, -1, depth);
    PUT_DATA_IN_LEAF_TRIE_NODE(node, data);
  }
  update_itrie_data(data, TrEntry_timestamp(itrie), TrEntry_mode(itrie));
  return;
}



void itrie_update_entry(TrEntry itrie, YAP_Term entry) {
  TrData data;
  TrNode node;

  if ((node = core_trie_check_entry(TrEntry_trie(itrie), entry)) != NULL) {
    data = (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node);
    update_itrie_data(data, TrEntry_timestamp(itrie), TrEntry_mode(itrie));
  }
  return;
}



TrData itrie_check_entry(TrEntry itrie, YAP_Term entry) {
  TrNode node;

  if (!(node = core_trie_check_entry(TrEntry_trie(itrie), entry)))
    return NULL;
  return (TrData) GET_DATA_FROM_LEAF_TRIE_NODE(node);
}



YAP_Term itrie_get_entry(TrData data) {
  return core_trie_get_entry(TrData_leaf(data));
}



void itrie_get_data(TrData data, YAP_Int *pos, YAP_Int *neg, YAP_Int *timestamp) {
  *pos = TrData_pos(data);
  *neg = TrData_neg(data);
  *timestamp = TrData_timestamp(data);
  return;
}



TrData itrie_traverse_init(TrEntry itrie) {
  TrData data, *bucket;
  YAP_Int traverse_bucket = 0;

  do {
    bucket = TrEntry_bucket(itrie, traverse_bucket);
    traverse_bucket++;
  } while (!*bucket && traverse_bucket != TrEntry_num_buckets(itrie));
  data = *bucket;
  if (data) {
    TrEntry_traverse_bucket(itrie) = traverse_bucket;
    TrEntry_traverse_data(itrie) = TrData_next(data);
  }
  return data;
}



TrData itrie_traverse_cont(TrEntry itrie) {
  TrData data, *bucket;
  YAP_Int traverse_bucket;

  data = TrEntry_traverse_data(itrie);
  if (!data) {
    traverse_bucket = TrEntry_traverse_bucket(itrie);
    if (traverse_bucket == TrEntry_num_buckets(itrie))
      return NULL;
    do {
      bucket = TrEntry_bucket(itrie, traverse_bucket);
      traverse_bucket++;
    } while (!*bucket && traverse_bucket != TrEntry_num_buckets(itrie));
    data = *bucket;
    if (data) {
      TrEntry_traverse_bucket(itrie) = traverse_bucket;
      TrEntry_traverse_data(itrie) = TrData_next(data);
    }
  } else
    TrEntry_traverse_data(itrie) = TrData_next(data);
  return data;
}



void itrie_remove_entry(TrData data) {
  core_trie_remove_entry(ITRIE_ENGINE, TrData_leaf(data), &itrie_data_destruct);
  return;
}



void itrie_remove_subtree(TrData data) {
  core_trie_remove_subtree(ITRIE_ENGINE, TrData_leaf(data), &itrie_data_destruct);
  return;
}



void itrie_add(TrEntry itrie_dest, TrEntry itrie_source) {
  core_trie_add(TrEntry_trie(itrie_dest), TrEntry_trie(itrie_source), &itrie_data_add);
  return;
}



void itrie_subtract(TrEntry itrie_dest, TrEntry itrie_source) {
  core_trie_add(TrEntry_trie(itrie_dest), TrEntry_trie(itrie_source), &itrie_data_subtract);
  return;
}



void itrie_join(TrEntry itrie_dest, TrEntry itrie_source) {
  CURRENT_ITRIE = itrie_dest;
  core_trie_join(ITRIE_ENGINE, TrEntry_trie(itrie_dest), TrEntry_trie(itrie_source), &itrie_data_add, &itrie_data_copy);
  return;
}



void itrie_intersect(TrEntry itrie_dest, TrEntry itrie_source) {
  core_trie_intersect(ITRIE_ENGINE, TrEntry_trie(itrie_dest), TrEntry_trie(itrie_source), &itrie_data_add, &itrie_data_destruct);
  return;
}



YAP_Int itrie_count_join(TrEntry itrie1, TrEntry itrie2) {
  return core_trie_count_join(TrEntry_trie(itrie1), TrEntry_trie(itrie2));
}



YAP_Int itrie_count_intersect(TrEntry itrie1, TrEntry itrie2) {
  return core_trie_count_intersect(TrEntry_trie(itrie1), TrEntry_trie(itrie2));
}



void itrie_save(TrEntry itrie, FILE *file) {
  core_trie_save(TrEntry_trie(itrie), file, &itrie_data_save);
  return;
}



void itrie_save_as_trie(TrEntry itrie, FILE *file) {
  core_trie_save(TrEntry_trie(itrie), file, NULL);
  return;
}



TrEntry itrie_load(FILE *file) {
  TrEntry itrie;
  TrNode node;

  new_itrie_entry(itrie, NULL);
  CURRENT_ITRIE = itrie;
  if (!(node = core_trie_load(ITRIE_ENGINE, file, &itrie_data_load))) {
    free_itrie_entry(itrie);
    return NULL;
  }
  TrEntry_trie(itrie) = node;
  if (FIRST_ITRIE)
    TrEntry_previous(FIRST_ITRIE) = itrie;
  FIRST_ITRIE = itrie;
  return itrie;
}



void itrie_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes) {
  core_trie_stats(ITRIE_ENGINE, memory, tries, entries, nodes);
  return;
}



void itrie_max_stats(YAP_Int *memory, YAP_Int *tries, YAP_Int *entries, YAP_Int *nodes) {
  core_trie_max_stats(ITRIE_ENGINE, memory, tries, entries, nodes);
  return;
}



void itrie_usage(TrEntry itrie, YAP_Int *entries, YAP_Int *nodes, YAP_Int *virtual_nodes) {
  core_trie_usage(TrEntry_trie(itrie), entries, nodes, virtual_nodes);
  return;
}



void itrie_print(TrEntry itrie) {
  core_trie_print(TrEntry_trie(itrie), &itrie_data_print);
  return;
}
