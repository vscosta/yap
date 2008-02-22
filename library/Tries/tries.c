/****************************************
  File:     tries.c
  Author:   Ricardo Rocha
  Comments: Tries module for Yap Prolog
  version:  $ID$
****************************************/



/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <YapInterface.h>
#include <string.h>
#include <stdio.h>
#include "core_tries.h"
#include "base_tries.h"



/* -------------------------- */
/*         Procedures         */
/* -------------------------- */

void init_tries(void);
static int p_trie_open(void);
static int p_trie_close(void);
static int p_trie_close_all(void);
static int p_trie_mode(void);
static int p_trie_put_entry(void);
static int p_trie_check_entry(void);
static int p_trie_get_entry(void);
static int p_trie_traverse_init(void);
static int p_trie_traverse_cont(void);
static int p_trie_remove_entry(void);
static int p_trie_remove_subtree(void);
static int p_trie_join(void);
static int p_trie_intersect(void);
static int p_trie_count_join(void);
static int p_trie_count_intersect(void);
static int p_trie_save(void);
static int p_trie_load(void);
static int p_trie_stats(void);
static int p_trie_max_stats(void);
static int p_trie_usage(void);
static int p_trie_print(void);
/* backwards compatibility */
static int p_open_trie(void);
static int p_close_trie(void);
static int p_close_all_tries(void);
static int p_put_trie_entry(void);
static int p_get_trie_entry(void);
static int p_remove_trie_entry(void);
static int p_print_trie(void);



/* -------------------------- */
/*    Module Init Procedure   */     
/* -------------------------- */

void init_tries(void) {
  trie_init_module();

  YAP_UserCPredicate("trie_open", p_trie_open, 1);
  YAP_UserCPredicate("trie_close", p_trie_close, 1);
  YAP_UserCPredicate("trie_close_all", p_trie_close_all, 0);
  YAP_UserCPredicate("trie_mode", p_trie_mode, 1);
  YAP_UserCPredicate("trie_put_entry", p_trie_put_entry, 3);
  YAP_UserCPredicate("trie_check_entry", p_trie_check_entry, 3);
  YAP_UserCPredicate("trie_get_entry", p_trie_get_entry, 2);
  YAP_UserBackCPredicate("trie_traverse", p_trie_traverse_init, p_trie_traverse_cont, 2, 0);
  YAP_UserCPredicate("trie_remove_entry", p_trie_remove_entry, 1);
  YAP_UserCPredicate("trie_remove_subtree", p_trie_remove_subtree, 1);
  YAP_UserCPredicate("trie_join", p_trie_join, 2);
  YAP_UserCPredicate("trie_intersect", p_trie_intersect, 2);
  YAP_UserCPredicate("trie_count_join", p_trie_count_join, 3);
  YAP_UserCPredicate("trie_count_intersect", p_trie_count_intersect, 3);
  YAP_UserCPredicate("trie_save", p_trie_save, 2);
  YAP_UserCPredicate("trie_load", p_trie_load, 2);
  YAP_UserCPredicate("trie_usage", p_trie_usage, 4);
  YAP_UserCPredicate("trie_stats", p_trie_stats, 4);
  YAP_UserCPredicate("trie_max_stats", p_trie_max_stats, 4);
  YAP_UserCPredicate("trie_print", p_trie_print, 1);
  /* backwards compatibility */
  YAP_UserCPredicate("open_trie", p_open_trie, 1);
  YAP_UserCPredicate("close_trie", p_close_trie, 1);
  YAP_UserCPredicate("close_all_tries", p_close_all_tries, 0);
  YAP_UserCPredicate("put_trie_entry", p_put_trie_entry, 4);
  YAP_UserCPredicate("get_trie_entry", p_get_trie_entry, 3);
  YAP_UserCPredicate("remove_trie_entry", p_remove_trie_entry, 1);
  YAP_UserCPredicate("print_trie", p_print_trie, 1);
  return;
}



/* --------------------------------- */
/*      Backwards Compatibility      */
/* --------------------------------- */

/* open_trie(+Trie) */
static int p_open_trie(void) {
  return p_trie_open();
}


/* close_trie(-Trie) */
static int p_close_trie(void) {
  return p_trie_close();
}


/* close_all_tries() */
static int p_close_all_tries(void) {
  return p_trie_close_all();
}


/* put_trie_entry(-Mode,-Trie,-Entry,+Ref) */
#define arg_mode  YAP_ARG1
#define arg_trie  YAP_ARG2
#define arg_entry YAP_ARG3
#define arg_ref   YAP_ARG4
static int p_put_trie_entry(void) {
  TrData data;
  const char *mode_str;
  YAP_Int mode, current_mode;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std"))
    mode = TRIE_MODE_STANDARD;
  else if (!strcmp(mode_str, "rev"))
    mode = TRIE_MODE_REVERSE;
  else
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* put trie entry */
  current_mode = trie_get_mode();
  trie_set_mode(mode);
  data = trie_put_entry((TrEntry) YAP_IntOfTerm(arg_trie), arg_entry);
  trie_set_mode(current_mode);
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_mode
#undef arg_trie
#undef arg_entry
#undef arg_ref


/* get_trie_entry(-Mode,-Ref,+Entry) */
#define arg_mode  YAP_ARG1
#define arg_ref   YAP_ARG2
#define arg_entry YAP_ARG3
static int p_get_trie_entry(void) {
  YAP_Term entry;
  const char *mode_str;
  YAP_Int mode, current_mode;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std"))
    mode = TRIE_MODE_STANDARD;
  else if (!strcmp(mode_str, "rev"))
    mode = TRIE_MODE_REVERSE;
  else
    return FALSE;
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* get trie entry */
  current_mode = trie_get_mode();
  trie_set_mode(mode);
  entry = trie_get_entry((TrData) YAP_IntOfTerm(arg_ref));
  trie_set_mode(current_mode);
  return YAP_Unify(arg_entry, entry);
}
#undef arg_mode
#undef arg_ref
#undef arg_entry


/* remove_trie_entry(-Ref) */
static int p_remove_trie_entry(void) {
  return p_trie_remove_entry();
}


/* print_trie(-Trie) */
static int p_print_trie(void) {
  return p_trie_print();
}



/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

/* trie_open(+Trie) */
#define arg_trie YAP_ARG1
static int p_trie_open(void) {
  TrEntry trie;

  /* check arg */
  if (!YAP_IsVarTerm(arg_trie)) 
    return FALSE;

  /* open trie */
  trie = trie_open();
  return YAP_Unify(arg_trie, YAP_MkIntTerm((YAP_Int) trie));
}
#undef arg_trie


/* trie_close(-Trie) */
#define arg_trie YAP_ARG1
static int p_trie_close(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* close trie */
  trie_close((TrEntry) YAP_IntOfTerm(arg_trie));
  return TRUE;
}
#undef arg_trie


/* trie_close_all() */
static int p_trie_close_all(void) {
  trie_close_all();
  return TRUE;
}


/* trie_mode(?Mode) */
#define arg_mode YAP_ARG1
static int p_trie_mode(void) {
  YAP_Term mode_term;
  const char *mode_str;
  YAP_Int mode;

  /* get mode */
  if (YAP_IsVarTerm(arg_mode)) {
    mode = trie_get_mode();
    if (mode == TRIE_MODE_STANDARD)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("std"));
    else if (mode == TRIE_MODE_REVERSE)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("rev"));
    else
      return FALSE;
    return YAP_Unify(arg_mode, mode_term);
  }

  /* set mode */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std"))
    mode = TRIE_MODE_STANDARD;
  else if (!strcmp(mode_str, "rev"))
    mode = TRIE_MODE_REVERSE;
  else
    return FALSE;
  trie_set_mode(mode);
  return TRUE;
}
#undef arg_mode


/* trie_put_entry(-Trie,-Entry,+Ref) */
#define arg_trie  YAP_ARG1
#define arg_entry YAP_ARG2
#define arg_ref   YAP_ARG3
static int p_trie_put_entry(void) {
  TrData data;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* put trie entry */
  data = trie_put_entry((TrEntry) YAP_IntOfTerm(arg_trie), arg_entry);
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_entry
#undef arg_ref


/* trie_check_entry(-Trie,-Entry,+Ref) */
#define arg_trie  YAP_ARG1
#define arg_entry YAP_ARG2
#define arg_ref   YAP_ARG3
static int p_trie_check_entry(void) {
  TrData data;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* check trie entry */
  if (!(data = trie_check_entry((TrEntry) YAP_IntOfTerm(arg_trie), arg_entry)))
    return FALSE;
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_entry
#undef arg_ref


/* trie_get_entry(-Ref,+Entry) */
#define arg_ref   YAP_ARG1
#define arg_entry YAP_ARG2
static int p_trie_get_entry(void) {
  YAP_Term entry;

  /* check args */
  if (!YAP_IsIntTerm(arg_ref)) 
    return FALSE;

  /* get trie entry */
  entry = trie_get_entry((TrData) YAP_IntOfTerm(arg_ref));
  return YAP_Unify(arg_entry, entry);
}
#undef arg_ref
#undef arg_entry


/* trie_traverse(-Trie,+Ref) */
#define arg_trie YAP_ARG1
#define arg_ref   YAP_ARG2
static int p_trie_traverse_init(void) {
  TrData data;

  /* check arg */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* traverse trie */
  if (!(data = trie_traverse_init((TrEntry) YAP_IntOfTerm(arg_trie)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_ref


/* trie_traverse(-Trie,+Ref) */
#define arg_trie YAP_ARG1
#define arg_ref   YAP_ARG2
static int p_trie_traverse_cont(void) {
  TrData data;

  /* traverse trie */
  if (!(data = trie_traverse_cont((TrEntry) YAP_IntOfTerm(arg_trie)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_ref


/* trie_remove_entry(-Ref) */
#define arg_ref YAP_ARG1
static int p_trie_remove_entry(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove trie entry */
  trie_remove_entry((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* trie_remove_subtree(-Ref) */
#define arg_ref YAP_ARG1
static int p_trie_remove_subtree(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove trie subtree */
  trie_remove_subtree((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* trie_join(-TrieDest,-TrieSource) */
#define arg_trie_dest   YAP_ARG1
#define arg_trie_source YAP_ARG2
static int p_trie_join(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_trie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie_source)) 
    return FALSE;

  /* join trie */
  trie_join((TrEntry) YAP_IntOfTerm(arg_trie_dest), (TrEntry) YAP_IntOfTerm(arg_trie_source));
  return TRUE;
}
#undef arg_trie_dest
#undef arg_trie_source


/* trie_intersect(-TrieDest,-TrieSource) */
#define arg_trie_dest   YAP_ARG1
#define arg_trie_source YAP_ARG2
static int p_trie_intersect(void) {
  /* check args */
  if (!YAP_IsIntTerm(arg_trie_dest)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie_source)) 
    return FALSE;

  /* intersect trie */
  trie_intersect((TrEntry) YAP_IntOfTerm(arg_trie_dest), (TrEntry) YAP_IntOfTerm(arg_trie_source));
  return TRUE;
}
#undef arg_trie_dest
#undef arg_trie_source


/* trie_count_join(-Trie1,-Trie2,+Entries) */
#define arg_trie1   YAP_ARG1
#define arg_trie2   YAP_ARG2
#define arg_entries YAP_ARG3
static int p_trie_count_join(void) {
  YAP_Int entries;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie1)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie2)) 
    return FALSE;

  /* count join trie */
  entries = trie_count_join((TrEntry) YAP_IntOfTerm(arg_trie1), (TrEntry) YAP_IntOfTerm(arg_trie2));
  return YAP_Unify(arg_entries, YAP_MkIntTerm(entries));
}
#undef arg_trie1
#undef arg_trie2
#undef arg_entries


/* trie_count_intersect(-Trie1,-Trie2,+Entries) */
#define arg_trie1   YAP_ARG1
#define arg_trie2   YAP_ARG2
#define arg_entries YAP_ARG3
static int p_trie_count_intersect(void) {
  YAP_Int entries;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie1)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie2)) 
    return FALSE;

  /* count intersect trie */
  entries = trie_count_intersect((TrEntry) YAP_IntOfTerm(arg_trie1), (TrEntry) YAP_IntOfTerm(arg_trie2));
  return YAP_Unify(arg_entries, YAP_MkIntTerm(entries));
}
#undef arg_trie1
#undef arg_trie2
#undef arg_entries


/* trie_save(-Trie,-FileName) */
#define arg_trie YAP_ARG1
#define arg_file YAP_ARG2
static int p_trie_save(void) {
  const char *file_str;
  FILE *file;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;
  if (!YAP_IsAtomTerm(arg_file))
    return FALSE;

  /* open file */
  file_str = YAP_AtomName(YAP_AtomOfTerm(arg_file));
  if (!(file = fopen(file_str, "w")))
    return FALSE;

  /* save trie and close file */
  trie_save((TrEntry) YAP_IntOfTerm(arg_trie), file);
  if (fclose(file))
    return FALSE;
  return TRUE;
}
#undef arg_trie
#undef arg_file


/* trie_load(+Trie,-FileName) */
#define arg_trie YAP_ARG1
#define arg_file YAP_ARG2
static int p_trie_load(void) {
  TrEntry data;
  const char *file_str;
  FILE *file;

  /* check args */
  if (!YAP_IsVarTerm(arg_trie)) 
    return FALSE;
  if (!YAP_IsAtomTerm(arg_file))
    return FALSE;

  /* open file */
  file_str = YAP_AtomName(YAP_AtomOfTerm(arg_file));
  if (!(file = fopen(file_str, "r")))
    return FALSE;

  /* load trie and close file */
  data = trie_load(file);
  if (fclose(file))
    return FALSE;
  return YAP_Unify(arg_trie, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_file


/* trie_stats(+Memory,+Tries,+Entries,+Nodes) */
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static int p_trie_stats(void) {
  YAP_Int memory, tries, entries, nodes;

  /* get stats */
  trie_stats(&memory, &tries, &entries, &nodes);
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


/* trie_max_stats(+Memory,+Tries,+Entries,+Nodes) */
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static int p_trie_max_stats(void) {
  YAP_Int memory, tries, entries, nodes;

  /* get stats */
  trie_max_stats(&memory, &tries, &entries, &nodes);
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


/* trie_usage(-Trie,+Entries,+Nodes,+VirtualNodes) */
#define arg_trie         YAP_ARG1
#define arg_entries      YAP_ARG2
#define arg_nodes        YAP_ARG3
#define arg_virtualnodes YAP_ARG4
static int p_trie_usage(void) {
  YAP_Int entries, nodes, virtualnodes;

  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* get trie usage */
  trie_usage((TrEntry) YAP_IntOfTerm(arg_trie), &entries, &nodes, &virtualnodes);
  if (!YAP_Unify(arg_entries, YAP_MkIntTerm(entries)))
    return FALSE;
  if (!YAP_Unify(arg_nodes, YAP_MkIntTerm(nodes)))
    return FALSE;
  if (!YAP_Unify(arg_virtualnodes, YAP_MkIntTerm(virtualnodes)))
    return FALSE;
  return TRUE;
}
#undef arg_trie
#undef arg_entries
#undef arg_nodes
#undef arg_virtualnodes


/* trie_print(-Trie) */
#define arg_trie YAP_ARG1
static int p_trie_print(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* print trie */
  trie_print((TrEntry) YAP_IntOfTerm(arg_trie));
  return TRUE;
}
#undef arg_trie
