/*********************************
  File:     itries.c
  Author:   Ricardo Rocha
  Comments: Tries module for ILP
  version:  $ID$
*********************************/



/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <YapInterface.h>
#include <string.h>
#include <stdio.h>
#include "core_tries.h"
#include "base_itries.h"



/* -------------------------- */
/*         Procedures         */
/* -------------------------- */

void init_itries(void);
static YAP_Bool p_itrie_open(void);
static YAP_Bool p_itrie_close(void);
static YAP_Bool p_itrie_close_all(void);
static YAP_Bool p_itrie_mode(void);
static YAP_Bool p_itrie_timestamp(void);
static YAP_Bool p_itrie_put_entry(void);
static YAP_Bool p_itrie_update_entry(void);
static YAP_Bool p_itrie_check_entry(void);
static YAP_Bool p_itrie_get_entry(void);
static YAP_Bool p_itrie_get_data(void);
static YAP_Bool p_itrie_traverse_init(void);
static YAP_Bool p_itrie_traverse_cont(void);
static YAP_Bool p_itrie_remove_entry(void);
static YAP_Bool p_itrie_remove_subtree(void);
static YAP_Bool p_itrie_add(void);
static YAP_Bool p_itrie_subtract(void);
static YAP_Bool p_itrie_join(void);
static YAP_Bool p_itrie_intersect(void);
static YAP_Bool p_itrie_count_join(void);
static YAP_Bool p_itrie_count_intersect(void);
static YAP_Bool p_itrie_save(void);
static YAP_Bool p_itrie_save_as_trie(void);
static YAP_Bool p_itrie_load(void);
static YAP_Bool p_itrie_save2stream(void);
static YAP_Bool p_itrie_loadFromStream(void);
static YAP_Bool p_itrie_stats(void);
static YAP_Bool p_itrie_max_stats(void);
static YAP_Bool p_itrie_usage(void);
static YAP_Bool p_itrie_print(void);



/* -------------------------- */
/*    Module Init Procedure   */     
/* -------------------------- */

void init_itries(void) {
  itrie_init_module();

  YAP_UserCPredicate("itrie_open", p_itrie_open, 1);
  YAP_UserCPredicate("itrie_close", p_itrie_close, 1);
  YAP_UserCPredicate("itrie_close_all", p_itrie_close_all, 0);
  YAP_UserCPredicate("itrie_mode", p_itrie_mode, 2);
  YAP_UserCPredicate("itrie_timestamp", p_itrie_timestamp, 2);
  YAP_UserCPredicate("itrie_put_entry", p_itrie_put_entry, 2);
  YAP_UserCPredicate("itrie_update_entry", p_itrie_update_entry, 2);
  YAP_UserCPredicate("itrie_check_entry", p_itrie_check_entry, 3);
  YAP_UserCPredicate("itrie_get_entry", p_itrie_get_entry, 2);
  YAP_UserCPredicate("itrie_get_data", p_itrie_get_data, 2);
  YAP_UserBackCPredicate("itrie_traverse", p_itrie_traverse_init, p_itrie_traverse_cont, 2, 0);
  YAP_UserCPredicate("itrie_remove_entry", p_itrie_remove_entry, 1);
  YAP_UserCPredicate("itrie_remove_subtree", p_itrie_remove_subtree, 1);
  YAP_UserCPredicate("itrie_add", p_itrie_add, 2);
  YAP_UserCPredicate("itrie_subtract", p_itrie_subtract, 2);
  YAP_UserCPredicate("itrie_join", p_itrie_join, 2);
  YAP_UserCPredicate("itrie_intersect", p_itrie_intersect, 2);
  YAP_UserCPredicate("itrie_count_join", p_itrie_count_join, 3);
  YAP_UserCPredicate("itrie_count_intersect", p_itrie_count_intersect, 3);
  YAP_UserCPredicate("itrie_save", p_itrie_save, 2);
  YAP_UserCPredicate("itrie_save_as_trie", p_itrie_save_as_trie, 2);
  YAP_UserCPredicate("itrie_load", p_itrie_load, 2);
  YAP_UserCPredicate("itrie_save2stream", p_itrie_save2stream, 2);
  YAP_UserCPredicate("itrie_loadFromstream", p_itrie_loadFromStream, 2);
  YAP_UserCPredicate("itrie_stats", p_itrie_stats, 4);
  YAP_UserCPredicate("itrie_max_stats", p_itrie_max_stats, 4);
  YAP_UserCPredicate("itrie_usage", p_itrie_usage, 4);
  YAP_UserCPredicate("itrie_print", p_itrie_print, 1);
  return;
}



/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

/* itrie_open(-Itrie) */
#define arg_itrie YAP_ARG1
static YAP_Bool p_itrie_open(void) {
  TrEntry itrie;

  /* check arg */
  if (!YAP_IsVarTerm(arg_itrie)) 
    return FALSE;

  /* open itrie */
  itrie = itrie_open();
  return YAP_Unify(arg_itrie, YAP_MkIntTerm((YAP_Int) itrie));
}
#undef arg_itrie


/* itrie_close(+Itrie) */
#define arg_itrie YAP_ARG1
static YAP_Bool p_itrie_close(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* close itrie */
  itrie_close((TrEntry) YAP_IntOfTerm(arg_itrie));
  return TRUE;
}
#undef arg_itrie


/* itrie_close_all() */
static YAP_Bool p_itrie_close_all(void) {
  itrie_close_all();
  return TRUE;
}


/* itrie_mode(+Itrie,?Mode) */
#define arg_itrie YAP_ARG1
#define arg_mode  YAP_ARG2
static YAP_Bool p_itrie_mode(void) {
  YAP_Term mode_term;
  const char *mode_str;
  YAP_Int mode;

  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* get mode */
  if (YAP_IsVarTerm(arg_mode)) {
    mode = itrie_get_mode((TrEntry) YAP_IntOfTerm(arg_itrie));
    if (mode == ITRIES_MODE_INC_POS)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("inc_pos"));
    else if (mode == ITRIES_MODE_DEC_POS)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("dec_pos"));
    else if (mode == ITRIES_MODE_INC_NEG)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("inc_neg"));
    else if (mode == ITRIES_MODE_DEC_NEG)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("dec_neg"));
    else if (mode == ITRIES_MODE_NONE)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("none"));
    else
      return FALSE;
    return YAP_Unify(arg_mode, mode_term);
  }

  /* set mode */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "inc_pos"))
    mode = ITRIES_MODE_INC_POS;
  else if (!strcmp(mode_str, "dec_pos"))
    mode = ITRIES_MODE_DEC_POS;
  else if (!strcmp(mode_str, "inc_neg"))
    mode = ITRIES_MODE_INC_NEG;
  else if (!strcmp(mode_str, "dec_neg"))
    mode = ITRIES_MODE_DEC_NEG;
  else if (!strcmp(mode_str, "none"))
    mode = ITRIES_MODE_NONE;
  else
    return FALSE;
  itrie_set_mode((TrEntry) YAP_IntOfTerm(arg_itrie), mode);
  return TRUE;
}
#undef arg_itrie
#undef arg_mode


/* itrie_timestamp(+Itrie,?Time) */
#define arg_itrie YAP_ARG1
#define arg_time  YAP_ARG2
static YAP_Bool p_itrie_timestamp(void) {
  YAP_Int time;

  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* get mode */
  if (YAP_IsVarTerm(arg_time)) {
    time = itrie_get_timestamp((TrEntry) YAP_IntOfTerm(arg_itrie));
    return YAP_Unify(arg_time, YAP_MkIntTerm(time));
  } 

  /* set mode */
  if (YAP_IsIntTerm(arg_time)) {
    time = YAP_IntOfTerm(arg_time);
    itrie_set_timestamp((TrEntry) YAP_IntOfTerm(arg_itrie), time);
    return TRUE;
  }

  return FALSE;
}
#undef arg_itrie
#undef arg_time


/* itrie_put_entry(+Itrie,+Entry) */
#define arg_itrie YAP_ARG1
#define arg_entry YAP_ARG2
static YAP_Bool p_itrie_put_entry(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* put entry */
  itrie_put_entry((TrEntry) YAP_IntOfTerm(arg_itrie), arg_entry);
  return TRUE;
}
#undef arg_itrie
#undef arg_entry


/* itrie_update_entry(+Itrie,+Entry) */
#define arg_itrie YAP_ARG1
#define arg_entry YAP_ARG2
static YAP_Bool p_itrie_update_entry(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* update entry */
  itrie_update_entry((TrEntry) YAP_IntOfTerm(arg_itrie), arg_entry);
  return TRUE;
}
#undef arg_itrie
#undef arg_entry


/* itrie_check_entry(+Itrie,+Entry,-Ref) */
#define arg_itrie YAP_ARG1
#define arg_entry YAP_ARG2
#define arg_ref   YAP_ARG3
static YAP_Bool p_itrie_check_entry(void) {
  TrData data;

  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* check entry */
  if (!(data = itrie_check_entry((TrEntry) YAP_IntOfTerm(arg_itrie), arg_entry)))
    return FALSE;
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_itrie
#undef arg_entry
#undef arg_ref


/* itrie_get_entry(+Ref,-Entry) */
#define arg_ref   YAP_ARG1
#define arg_entry YAP_ARG2
static YAP_Bool p_itrie_get_entry(void) {
  YAP_Term entry;

  /* check arg */
  if (!YAP_IsIntTerm(arg_ref)) 
    return FALSE;

  /* get entry */
  entry = itrie_get_entry((TrData) YAP_IntOfTerm(arg_ref));
  return YAP_Unify(arg_entry, entry);
}
#undef arg_ref
#undef arg_entry


/* itrie_get_data(+Ref,-Data) */
#define arg_ref  YAP_ARG1
#define arg_data YAP_ARG2
static YAP_Bool p_itrie_get_data(void) {
  YAP_Term list;
  YAP_Term item;
  YAP_Functor f;
  YAP_Int pos, neg, time;

  /* check arg */
  if (!YAP_IsIntTerm(arg_ref)) 
    return FALSE;

  /* get data */
  itrie_get_data((TrData) YAP_IntOfTerm(arg_ref), &pos, &neg, &time);
  list = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
  f = YAP_MkFunctor(YAP_LookupAtom("timestamp"), 1);
  item = YAP_MkIntTerm(time);
  item = YAP_MkApplTerm(f, 1, &item);
  list = YAP_MkPairTerm(item, list);
  f = YAP_MkFunctor(YAP_LookupAtom("neg"), 1);
  item = YAP_MkIntTerm(neg);
  item = YAP_MkApplTerm(f, 1, &item);
  list = YAP_MkPairTerm(item, list);
  f = YAP_MkFunctor(YAP_LookupAtom("pos"), 1);
  item = YAP_MkIntTerm(pos);
  item = YAP_MkApplTerm(f, 1, &item);
  list = YAP_MkPairTerm(item, list);
  return YAP_Unify(arg_data, list);
}
#undef arg_ref
#undef arg_data


/* itrie_traverse(+Itrie,-Ref) */
#define arg_itrie YAP_ARG1
#define arg_ref   YAP_ARG2
static YAP_Bool p_itrie_traverse_init(void) {
  TrData data;

  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie)) 
    return FALSE;

  /* traverse itrie */
  if (!(data = itrie_traverse_init((TrEntry) YAP_IntOfTerm(arg_itrie)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_itrie
#undef arg_ref


/* itrie_traverse(+Itrie,-Ref) */
#define arg_itrie YAP_ARG1
#define arg_ref   YAP_ARG2
static YAP_Bool p_itrie_traverse_cont(void) {
  TrData data;

  /* traverse itrie */
  if (!(data = itrie_traverse_cont((TrEntry) YAP_IntOfTerm(arg_itrie)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_itrie
#undef arg_ref


/* itrie_remove_entry(+Ref) */
#define arg_ref   YAP_ARG1
static YAP_Bool p_itrie_remove_entry(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove entry */
  itrie_remove_entry((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* itrie_remove_subtree(+Ref) */
#define arg_ref   YAP_ARG1
static YAP_Bool p_itrie_remove_subtree(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove subtree */
  itrie_remove_subtree((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* itrie_add(+ItrieDest,+ItrieSource) */
#define arg_itrie_dest   YAP_ARG1
#define arg_itrie_source YAP_ARG2
static YAP_Bool p_itrie_add(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_itrie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie_source)) 
    return FALSE;

  /* add itrie */
  itrie_add((TrEntry) YAP_IntOfTerm(arg_itrie_dest), (TrEntry) YAP_IntOfTerm(arg_itrie_source));
  return TRUE;
}
#undef arg_itrie_dest
#undef arg_itrie_source


/* itrie_subtract(+ItrieDest,+ItrieSource) */
#define arg_itrie_dest   YAP_ARG1
#define arg_itrie_source YAP_ARG2
static YAP_Bool p_itrie_subtract(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_itrie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie_source)) 
    return FALSE;

  /* subtract itrie */
  itrie_subtract((TrEntry) YAP_IntOfTerm(arg_itrie_dest), (TrEntry) YAP_IntOfTerm(arg_itrie_source));
  return TRUE;
}
#undef arg_itrie_dest
#undef arg_itrie_source


/* itrie_join(+ItrieDest,+ItrieSource) */
#define arg_itrie_dest   YAP_ARG1
#define arg_itrie_source YAP_ARG2
static YAP_Bool p_itrie_join(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_itrie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie_source)) 
    return FALSE;

  /* join itrie */
  itrie_join((TrEntry) YAP_IntOfTerm(arg_itrie_dest), (TrEntry) YAP_IntOfTerm(arg_itrie_source));
  return TRUE;
}
#undef arg_itrie_dest
#undef arg_itrie_source


/* itrie_intersect(+ItrieDest,+ItrieSource) */
#define arg_itrie_dest   YAP_ARG1
#define arg_itrie_source YAP_ARG2
static YAP_Bool p_itrie_intersect(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_itrie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie_source)) 
    return FALSE;

  /* intersect itrie */
  itrie_intersect((TrEntry) YAP_IntOfTerm(arg_itrie_dest), (TrEntry) YAP_IntOfTerm(arg_itrie_source));
  return TRUE;
}
#undef arg_itrie_dest
#undef arg_itrie_source


/* itrie_count_join(+Itrie1,+Itrie2,-Entries) */
#define arg_itrie1   YAP_ARG1
#define arg_itrie2   YAP_ARG2
#define arg_entries YAP_ARG3
static YAP_Bool p_itrie_count_join(void) {
  YAP_Int entries;

  /* check args */
  if (!YAP_IsIntTerm(arg_itrie1)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie2)) 
    return FALSE;

  /* count join itrie */
  entries = itrie_count_join((TrEntry) YAP_IntOfTerm(arg_itrie1), (TrEntry) YAP_IntOfTerm(arg_itrie2));
  return YAP_Unify(arg_entries, YAP_MkIntTerm(entries));
}
#undef arg_itrie1
#undef arg_itrie2
#undef arg_entries


/* itrie_count_intersect(+Itrie1,+Itrie2,-Entries) */
#define arg_itrie1   YAP_ARG1
#define arg_itrie2   YAP_ARG2
#define arg_entries YAP_ARG3
static YAP_Bool p_itrie_count_intersect(void) {
  YAP_Int entries;

  /* check args */
  if (!YAP_IsIntTerm(arg_itrie1)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_itrie2)) 
    return FALSE;

  /* count intersect itrie */
  entries = itrie_count_intersect((TrEntry) YAP_IntOfTerm(arg_itrie1), (TrEntry) YAP_IntOfTerm(arg_itrie2));
  return YAP_Unify(arg_entries, YAP_MkIntTerm(entries));
}
#undef arg_itrie1
#undef arg_itrie2
#undef arg_entries


/* itrie_save(+Itrie,+FileName) */
#define arg_itrie YAP_ARG1
#define arg_file  YAP_ARG2
static YAP_Bool p_itrie_save(void) {
  const char *file_str;
  FILE *file;

  /* check args */
  if (!YAP_IsIntTerm(arg_itrie))
    return FALSE;
  if (!YAP_IsAtomTerm(arg_file))
    return FALSE;

  /* open file */
  file_str = YAP_AtomName(YAP_AtomOfTerm(arg_file));
  if (!(file = fopen(file_str, "w")))
    return FALSE;

  /* save itrie and close file */
  itrie_save((TrEntry) YAP_IntOfTerm(arg_itrie), file);
  if (fclose(file))
    return FALSE;
  return TRUE;
}
#undef arg_itrie
#undef arg_file


/* itrie_save_as_trie(+Itrie,+FileName) */
#define arg_itrie YAP_ARG1
#define arg_file  YAP_ARG2
static YAP_Bool p_itrie_save_as_trie(void) {
  const char *file_str;
  FILE *file;

  /* check args */
  if (!YAP_IsIntTerm(arg_itrie))
    return FALSE;
  if (!YAP_IsAtomTerm(arg_file))
    return FALSE;

  /* open file */
  file_str = YAP_AtomName(YAP_AtomOfTerm(arg_file));
  if (!(file = fopen(file_str, "w")))
    return FALSE;

  /* save itrie as trie and close file */
  itrie_save_as_trie((TrEntry) YAP_IntOfTerm(arg_itrie), file);
  if (fclose(file))
    return FALSE;
  return TRUE;
}
#undef arg_itrie
#undef arg_file


/* itrie_load(-Itrie,+FileName) */
#define arg_itrie YAP_ARG1
#define arg_file  YAP_ARG2
static YAP_Bool p_itrie_load(void) {
  TrEntry itrie;
  const char *file_str;
  FILE *file;

  /* check args */
  if (!YAP_IsVarTerm(arg_itrie)) 
    return FALSE;
  if (!YAP_IsAtomTerm(arg_file))
    return FALSE;

  /* open file */
  file_str = YAP_AtomName(YAP_AtomOfTerm(arg_file));
  if (!(file = fopen(file_str, "r")))
    return FALSE;

  /* load itrie and close file */
  if (!(itrie = itrie_load(file)))
    return FALSE;
  if (fclose(file))
    return FALSE;
  return YAP_Unify(arg_itrie, YAP_MkIntTerm((YAP_Int) itrie));
}
#undef arg_itrie
#undef arg_file


/* itrie_save2stream(+Itrie,+Stream) */
#define arg_itrie  YAP_ARG1
#define arg_stream YAP_ARG2
static YAP_Bool p_itrie_save2stream(void) {
  FILE *file;

  /* check args */
  if (!YAP_IsIntTerm(arg_itrie))
    return FALSE;
  if ((file = (FILE*) YAP_FileDescriptorFromStream(arg_stream)) == NULL)
    return FALSE;

  /* save itrie */
  itrie_save((TrEntry) YAP_IntOfTerm(arg_itrie), file);
  return TRUE;
}
#undef arg_itrie
#undef arg_stream


/* itrie_loadFromStream(-Itrie,+Stream) */
#define arg_itrie  YAP_ARG1
#define arg_stream YAP_ARG2
static YAP_Bool p_itrie_loadFromStream(void) {
  TrEntry itrie;
  FILE *file;

  /* check args */
  if (!YAP_IsVarTerm(arg_itrie)) 
    return FALSE;
  if (!(file = (FILE*) YAP_FileDescriptorFromStream(arg_stream)))
    return FALSE;

  /* load itrie */
  if (!(itrie = itrie_load(file)))
    return FALSE;
  return YAP_Unify(arg_itrie, YAP_MkIntTerm((YAP_Int) itrie));
}
#undef arg_itrie
#undef arg_stream


/* itrie_stats(-Memory,-Tries,-Entries,-Nodes) */
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static YAP_Bool p_itrie_stats(void) {
  YAP_Int memory, tries, entries, nodes;

  /* get stats */
  itrie_stats(&memory, &tries, &entries, &nodes);
  if (!YAP_Unify(arg_memory, YAP_MkIntTerm(memory)))
    return FALSE;
  if (!YAP_Unify(arg_tries, YAP_MkIntTerm(tries)))
    return FALSE;
  if (!YAP_Unify(arg_entries, YAP_MkIntTerm(entries)))
    return FALSE;
  if (!YAP_Unify(arg_nodes, YAP_MkIntTerm(nodes)))
    return FALSE;
  return TRUE;
}
#undef arg_memory
#undef arg_tries
#undef arg_entries
#undef arg_nodes


/* itrie_max_stats(-Memory,-Tries,-Entries,-Nodes) */
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static YAP_Bool p_itrie_max_stats(void) {
  YAP_Int memory, tries, entries, nodes;

  /* get stats */
  itrie_max_stats(&memory, &tries, &entries, &nodes);
  if (!YAP_Unify(arg_memory, YAP_MkIntTerm(memory)))
    return FALSE;
  if (!YAP_Unify(arg_tries, YAP_MkIntTerm(tries)))
    return FALSE;
  if (!YAP_Unify(arg_entries, YAP_MkIntTerm(entries)))
    return FALSE;
  if (!YAP_Unify(arg_nodes, YAP_MkIntTerm(nodes)))
    return FALSE;
  return TRUE;
}
#undef arg_memory
#undef arg_tries
#undef arg_entries
#undef arg_nodes


/* itrie_usage(+Itrie,-Entries,-Nodes,-VirtualNodes) */
#define arg_itrie        YAP_ARG1
#define arg_entries      YAP_ARG2
#define arg_nodes        YAP_ARG3
#define arg_virtualnodes YAP_ARG4
static YAP_Bool p_itrie_usage(void) {
  YAP_Int entries, nodes, virtualnodes;

  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie))
    return FALSE;

  /* get itrie usage */
  itrie_usage((TrEntry) YAP_IntOfTerm(arg_itrie), &entries, &nodes, &virtualnodes);
  if (!YAP_Unify(arg_entries, YAP_MkIntTerm(entries)))
    return FALSE;
  if (!YAP_Unify(arg_nodes, YAP_MkIntTerm(nodes)))
    return FALSE;
  if (!YAP_Unify(arg_virtualnodes, YAP_MkIntTerm(virtualnodes)))
    return FALSE;
  return TRUE;
}
#undef arg_itrie
#undef arg_entries
#undef arg_nodes
#undef arg_virtualnodes


/* itrie_print(+Itrie) */
#define arg_itrie YAP_ARG1
static YAP_Bool p_itrie_print(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_itrie))
    return FALSE;

  /* print itrie */
  itrie_print((TrEntry) YAP_IntOfTerm(arg_itrie));
  return TRUE;
}
#undef arg_itrie
