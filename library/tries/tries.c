/****************************************
  File:     tries.c
  Author:   Ricardo Rocha
  Comments: Tries module for Yap Prolog
  version:  $ID$
****************************************/
/**
 @file tries.c
 @brief yap-C wrapper for tries.
*/


/**
@addtogroup tries

@{
*/

/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <YapInterface.h>
#include <string.h>
#include <stdio.h>
#include "core_tries.h"
#include "base_tries.h"


/* -------------------------- */
/*         Structures         */
/* -------------------------- */

typedef struct {
  YAP_Term value;
} db_trie_opt_level;


/* -------------------------- */
/*         Procedures         */
/* -------------------------- */

O_API void init_tries(void);
static YAP_Bool p_trie_open(void);
static YAP_Bool p_trie_close(void);
static YAP_Bool p_trie_close_all(void);
static YAP_Bool p_trie_mode(void);
static YAP_Bool p_trie_put_entry(void);
static YAP_Bool p_trie_check_entry(void);
static YAP_Bool p_trie_get_entry(void);
static YAP_Bool p_trie_get_first_entry(void);
static YAP_Bool p_trie_get_last_entry(void);
static YAP_Bool p_trie_traverse_init(void);
static YAP_Bool p_trie_traverse_cont(void);
static YAP_Bool p_trie_remove_entry(void);
static YAP_Bool p_trie_remove_subtree(void);
static YAP_Bool p_trie_join(void);
static YAP_Bool p_trie_intersect(void);
static YAP_Bool p_trie_count_join(void);
static YAP_Bool p_trie_count_intersect(void);
static YAP_Bool p_trie_save(void);
static YAP_Bool p_trie_load(void);
static YAP_Bool p_trie_stats(void);
static YAP_Bool p_trie_max_stats(void);
static YAP_Bool p_trie_usage(void);
static YAP_Bool p_trie_print(void);

static YAP_Bool p_trie_traverse_mode(void);
static YAP_Bool p_trie_disable_hash(void);
static YAP_Bool p_trie_enable_hash(void);
static YAP_Bool p_trie_traverse_first(void);
static YAP_Bool p_trie_traverse_next(void);

static YAP_Bool p_trie_to_list(void);

/* dbtries support */
static YAP_Bool p_trie_depth_breadth(void);
static YAP_Bool p_trie_get_depth_breadth_reduction_current_data(void);
static YAP_Bool p_trie_get_db_opt_level_count_init(void);
static YAP_Bool p_trie_get_db_opt_level_count_cont(void);
static YAP_Bool p_trie_replace_nested_trie(void);
static YAP_Bool p_trie_db_opt_min_prefix(void);

/* backwards compatibility */
static YAP_Bool p_open_trie(void);
static YAP_Bool p_close_trie(void);
static YAP_Bool p_close_all_tries(void);
static YAP_Bool p_put_trie_entry(void);
static YAP_Bool p_get_trie_entry(void);
static YAP_Bool p_remove_trie_entry(void);
static YAP_Bool p_print_trie(void);



/* -------------------------- */
/*    Module Init Procedure   */     
/* -------------------------- */

O_API void init_tries(void) {
  trie_init_module();

  YAP_UserCPredicate("trie_open", p_trie_open, 1);
  YAP_UserCPredicate("trie_close", p_trie_close, 1);
  YAP_UserCPredicate("trie_close_all", p_trie_close_all, 0);
  YAP_UserCPredicate("trie_mode", p_trie_mode, 1);
  YAP_UserCPredicate("trie_put_entry", p_trie_put_entry, 3);
  YAP_UserCPredicate("trie_check_entry", p_trie_check_entry, 3);
  YAP_UserCPredicate("trie_get_entry", p_trie_get_entry, 2);
  YAP_UserCPredicate("trie_get_first_entry", p_trie_get_first_entry, 2);
  YAP_UserCPredicate("trie_get_last_entry", p_trie_get_last_entry, 2);
  YAP_UserBackCPredicate("trie_traverse", p_trie_traverse_init, p_trie_traverse_cont, 3, 0);
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

  YAP_UserCPredicate("trie_traverse_mode", p_trie_traverse_mode, 1);
  YAP_UserCPredicate("trie_disable_hash", p_trie_disable_hash, 0);
  YAP_UserCPredicate("trie_enable_hash", p_trie_enable_hash, 0);
  YAP_UserCPredicate("trie_traverse_first", p_trie_traverse_first, 2);
  YAP_UserCPredicate("trie_traverse_next", p_trie_traverse_next, 2);

  YAP_UserCPredicate("trie_to_list", p_trie_to_list, 2);

  /* dbtries support */
  YAP_UserCPredicate("trie_depth_breadth", p_trie_depth_breadth, 6);
  YAP_UserCPredicate("trie_get_depth_breadth_reduction_entry", p_trie_get_depth_breadth_reduction_current_data, 1);
  YAP_UserBackCPredicate("trie_get_depth_breadth_reduction_opt_level_count", p_trie_get_db_opt_level_count_init, p_trie_get_db_opt_level_count_cont, 2, sizeof(db_trie_opt_level));
  YAP_UserCPredicate("trie_replace_nested_trie", p_trie_replace_nested_trie, 3);
  YAP_UserCPredicate("trie_db_opt_min_prefix", p_trie_db_opt_min_prefix, 1);

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

/* open_trie(-Trie) */
static YAP_Bool p_open_trie(void) {
  return p_trie_open();
}


/* close_trie(+Trie) */
static YAP_Bool p_close_trie(void) {
  return p_trie_close();
}


/* close_all_tries() */
static YAP_Bool p_close_all_tries(void) {
  return p_trie_close_all();
}


/* put_trie_entry(+Mode,+Trie,+Entry,-Ref) */
/** @pred trie_put_entry(+Mode,+ _Trie_,+ _Term_,- _Ref_) 



Add term  _Term_ to trie  _Trie_. The handle  _Ref_ gives
a reference to the term.

 
*/
#define arg_mode  YAP_ARG1
#define arg_trie  YAP_ARG2
#define arg_entry YAP_ARG3
#define arg_ref   YAP_ARG4
static YAP_Bool p_put_trie_entry(void) {
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


/* get_trie_entry(+Mode,+Ref,-Entry) */
/** @pred trie_get_entry(+ _Ref_,- _Term_) 


Unify  _Term_ with the entry for handle  _Ref_.

 
*/
#define arg_mode  YAP_ARG1
#define arg_ref   YAP_ARG2
#define arg_entry YAP_ARG3
static YAP_Bool p_get_trie_entry(void) {
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

/* remove_trie_entry(+Ref) */
static YAP_Bool p_remove_trie_entry(void) {
  return p_trie_remove_entry();
}


/* print_trie(+Trie) */
static YAP_Bool p_print_trie(void) {
  return p_trie_print();
}



/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

/* trie_open(-Trie) */
#define arg_trie YAP_ARG1
static YAP_Bool p_trie_open(void) {
  TrEntry trie;

  /* check arg */
  if (!YAP_IsVarTerm(arg_trie)) 
    return FALSE;

  /* open trie */
  trie = trie_open();
  return YAP_Unify(arg_trie, YAP_MkIntTerm((YAP_Int) trie));
}
#undef arg_trie


/* trie_close(+Trie) */
/** @pred trie_close(+ _Id_) 



Close trie with identifier  _Id_.

 
*/
#define arg_trie YAP_ARG1
static YAP_Bool p_trie_close(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* close trie */
  trie_close((TrEntry) YAP_IntOfTerm(arg_trie));
  return TRUE;
}
#undef arg_trie


/* trie_close_all() */
/** @pred trie_close_all 



Close all available tries.

 
*/
static YAP_Bool p_trie_close_all(void) {
  trie_close_all();
  return TRUE;
}


/* trie_mode(?Mode) */
/** @pred trie_mode(? _Mode_) 



Unify  _Mode_ with trie operation mode. Allowed values are either
`std` (default) or `rev`.

 
*/
#define arg_mode YAP_ARG1
static YAP_Bool p_trie_mode(void) {
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


/* trie_put_entry(+Trie,+Entry,-Ref) */
#define arg_trie  YAP_ARG1
#define arg_entry YAP_ARG2
#define arg_ref   YAP_ARG3
static YAP_Bool p_trie_put_entry(void) {
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


/* trie_check_entry(+Trie,+Entry,-Ref) */
/** @pred trie_check_entry(+ _Trie_,+ _Term_,- _Ref_) 



Succeeds if a variant of term  _Term_ is in trie  _Trie_. An handle
 _Ref_ gives a reference to the term.

 
*/
#define arg_trie  YAP_ARG1
#define arg_entry YAP_ARG2
#define arg_ref   YAP_ARG3
static YAP_Bool p_trie_check_entry(void) {
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


/* trie_get_entry(+Ref,-Entry) */
#define arg_ref   YAP_ARG1
#define arg_entry YAP_ARG2
static YAP_Bool p_trie_get_entry(void) {
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


/* trie_get_first_entry(+Trie,-Ref) */
#define arg_trie YAP_ARG1
#define arg_ref  YAP_ARG2
static YAP_Bool p_trie_get_first_entry(void) {
  TrData data;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* get first trie entry */
  if (!(data = trie_get_first_entry((TrEntry) YAP_IntOfTerm(arg_trie))))
    return FALSE;
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_ref


/* trie_get_last_entry(+Trie,-Ref) */
#define arg_trie YAP_ARG1
#define arg_ref  YAP_ARG2
static YAP_Bool p_trie_get_last_entry(void) {
  TrData data;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* get last trie entry */
  if (!(data = trie_get_last_entry((TrEntry) YAP_IntOfTerm(arg_trie))))
    return FALSE;
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_ref


/* trie_traverse(+Trie,+FirstRef,-Ref) */
#define arg_trie     YAP_ARG1
#define arg_init_ref YAP_ARG2
#define arg_ref      YAP_ARG3
static YAP_Bool p_trie_traverse_init(void) {
  TrData data;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;
  if (!YAP_IsIntTerm(arg_init_ref)) 
    return FALSE;

  /* traverse trie */
  if (!(data = trie_traverse_init((TrEntry) YAP_IntOfTerm(arg_trie), (TrData) YAP_IntOfTerm(arg_init_ref)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_init_ref
#undef arg_ref


/* trie_traverse(+Trie,+FirstRef,-Ref) */
#define arg_trie     YAP_ARG1
#define arg_init_ref YAP_ARG2
#define arg_ref      YAP_ARG3
static YAP_Bool p_trie_traverse_cont(void) {
  TrData data;

  /* traverse trie */
  if (!(data = trie_traverse_cont((TrEntry) YAP_IntOfTerm(arg_trie)))) {
    YAP_cut_fail();
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_init_ref
#undef arg_ref


/* trie_remove_entry(+Ref) */
/** @pred trie_remove_entry(+ _Ref_) 



Remove entry for handle  _Ref_.

 
*/
#define arg_ref YAP_ARG1
static YAP_Bool p_trie_remove_entry(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove trie entry */
  trie_remove_entry((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* trie_remove_subtree(+Ref) */
/** @pred trie_remove_subtree(+ _Ref_) 



Remove subtree rooted at handle  _Ref_.

 
*/
#define arg_ref YAP_ARG1
static YAP_Bool p_trie_remove_subtree(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove trie subtree */
  trie_remove_subtree((TrData) YAP_IntOfTerm(arg_ref));
  return TRUE;
}
#undef arg_ref


/* trie_join(+TrieDest,+TrieSource) */
#define arg_trie_dest   YAP_ARG1
#define arg_trie_source YAP_ARG2
static YAP_Bool p_trie_join(void) {
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


/* trie_intersect(+TrieDest,+TrieSource) */
#define arg_trie_dest   YAP_ARG1
#define arg_trie_source YAP_ARG2
static YAP_Bool p_trie_intersect(void) {
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


/* trie_count_join(+Trie1,+Trie2,-Entries) */
#define arg_trie1   YAP_ARG1
#define arg_trie2   YAP_ARG2
#define arg_entries YAP_ARG3
static YAP_Bool p_trie_count_join(void) {
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


/* trie_count_intersect(+Trie1,+Trie2,-Entries) */
#define arg_trie1   YAP_ARG1
#define arg_trie2   YAP_ARG2
#define arg_entries YAP_ARG3
static YAP_Bool p_trie_count_intersect(void) {
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

/** @pred trie_save(+ _Trie_,+ _FileName_) 


Dump trie  _Trie_ into file  _FileName_.

 
*/
#define arg_trie YAP_ARG1
#define arg_file YAP_ARG2
static YAP_Bool p_trie_save(void) {
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


/* trie_load(-Trie,+FileName) */
/** @pred trie_load(- _Trie_,+ _FileName_) 


Load trie  _Trie_ from the contents of file  _FileName_.

 
*/
#define arg_trie YAP_ARG1
#define arg_file YAP_ARG2
static YAP_Bool p_trie_load(void) {
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
  if (!(data = trie_load(file)))
    return FALSE;
  if (fclose(file))
    return FALSE;
  return YAP_Unify(arg_trie, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_file

/** @pred trie_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give generic statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/

/* trie_stats(-Memory,-Tries,-Entries,-Nodes) */
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static YAP_Bool p_trie_stats(void) {
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


/* trie_max_stats(-Memory,-Tries,-Entries,-Nodes) */
/** @pred trie_max_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give maximal statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/
#define arg_memory  YAP_ARG1
#define arg_tries   YAP_ARG2
#define arg_entries YAP_ARG3
#define arg_nodes   YAP_ARG4
static YAP_Bool p_trie_max_stats(void) {
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


/** @pred trie_usage(+ _Trie_,- _Entries_,- _Nodes_,- _VirtualNodes_) 


Give statistics on trie  _Trie_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_, and the
number of  _VirtualNodes_.

 
*/
/* trie_usage(+Trie,-Entries,-Nodes,-VirtualNodes) */
#define arg_trie         YAP_ARG1
#define arg_entries      YAP_ARG2
#define arg_nodes        YAP_ARG3
#define arg_virtualnodes YAP_ARG4
static YAP_Bool p_trie_usage(void) {
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


/* trie_print(+Trie) */
/** @pred trie_print(+ _Trie_) 


Print trie  _Trie_ on standard output.




 */
#define arg_trie YAP_ARG1
static YAP_Bool p_trie_print(void) {
  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* print trie */
  trie_print((TrEntry) YAP_IntOfTerm(arg_trie));
  return TRUE;
}
#undef arg_trie


/* trie_traverse_mode(?Mode) */
#define arg_mode YAP_ARG1
static YAP_Bool p_trie_traverse_mode(void) {
  YAP_Term mode_term;
  const char *mode_str;
  YAP_Int mode;

  /* get mode */
  if (YAP_IsVarTerm(arg_mode)) {
    mode = trie_get_traverse_mode();
    if (mode == TRAVERSE_MODE_FORWARD)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("forward"));
    else if (mode == TRAVERSE_MODE_BACKWARD)
      mode_term = YAP_MkAtomTerm(YAP_LookupAtom("backward"));
    else
      return FALSE;
    return YAP_Unify(arg_mode, mode_term);
  }

  /* set mode */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "forward"))
    mode = TRAVERSE_MODE_FORWARD;
  else if (!strcmp(mode_str, "backward"))
    mode = TRAVERSE_MODE_BACKWARD;
  else
    return FALSE;
  trie_set_traverse_mode(mode);
  return TRUE;
}
#undef arg_mode


/* trie_traverse_first(+Trie, -Ref) */
#define arg_trie   YAP_ARG1
#define arg_ref    YAP_ARG2
static YAP_Bool p_trie_traverse_first(void) {
  TrData data;

  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* traverse trie */
  if (!(data = trie_traverse_first((TrEntry) YAP_IntOfTerm(arg_trie)))) {
    return FALSE;
  }
  return YAP_Unify(arg_ref, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_trie
#undef arg_ref


/* trie_traverse_next(+Ref, -Ref) */
#define arg_cur    YAP_ARG1
#define arg_next   YAP_ARG2
static YAP_Bool p_trie_traverse_next(void) {
  TrData data;

  /* check arg */
  if (!YAP_IsIntTerm(arg_cur))
    return FALSE;

  /* traverse trie */
  if (!(data = trie_traverse_next((TrData) YAP_IntOfTerm(arg_cur)))) {
    return FALSE;
  }
  return YAP_Unify(arg_next, YAP_MkIntTerm((YAP_Int) data));
}
#undef arg_cur
#undef arg_next


/* trie_disable_hash */
static YAP_Bool p_trie_disable_hash(void) {
  trie_disable_hash_table();
  return TRUE;
}


/* trie_enable_hash */
static YAP_Bool p_trie_enable_hash(void) {
  trie_enable_hash_table();
  return TRUE;
}


/* trie_to_list(+Trie, -List) */
#define arg_trie YAP_ARG1
#define arg_list YAP_ARG2
static YAP_Bool p_trie_to_list(void) {
  YAP_Term list;
  
  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* get list from trie */
  list = trie_to_list((TrEntry) YAP_IntOfTerm(arg_trie));  
  return YAP_Unify(arg_list, list);
}
#undef arg_trie
#undef arg_list


/* trie_depth_breadth(+Trie, -DepthBreadthTrie, -FinalLabel, +OptimizationLevel, +StartCounter, -EndCounter) */
#define arg_trie               YAP_ARG1
#define arg_db_trie            YAP_ARG2
#define arg_final_label        YAP_ARG3
#define arg_opt_level          YAP_ARG4
#define arg_start_counter      YAP_ARG5
#define arg_end_counter        YAP_ARG6
static YAP_Bool p_trie_depth_breadth(void) {
  /* get depth breadth trie */
  if (!YAP_IsIntTerm(arg_opt_level))
    return FALSE;
  if (!YAP_IsIntTerm(arg_start_counter))
    return FALSE;
  YAP_Int End;
  YAP_Term t = trie_depth_breadth((TrEntry) YAP_IntOfTerm(arg_trie), (TrEntry) YAP_IntOfTerm(arg_db_trie), YAP_IntOfTerm(arg_opt_level), YAP_IntOfTerm(arg_start_counter), &End);
  int a = YAP_Unify(t, arg_final_label);
  a = a && YAP_Unify(YAP_MkIntTerm(End), arg_end_counter);
  return a;
}
#undef arg_trie
#undef arg_db_trie
#undef arg_final_label
#undef arg_opt_level
#undef arg_start_counter
#undef arg_end_counter


/* trie_get_depth_breadth_reduction_entry(-Entry) */
#define arg_entry YAP_ARG1
static YAP_Bool p_trie_get_depth_breadth_reduction_current_data(void) {
  TrData data = trie_get_depth_breadth_reduction_current_data();
  if (data)
    return YAP_Unify(arg_entry, YAP_MkIntTerm((YAP_Int) data));
  return FALSE;
}
#undef arg_entry


db_trie_opt_level *opt_level;

/* trie_get_depth_breadth_reduction_opt_level_count(?OptLevel, -Count) */
#define arg_opt_level YAP_ARG1
#define arg_count YAP_ARG2
static YAP_Bool p_trie_get_db_opt_level_count_init(void) {
  if (YAP_IsIntTerm(arg_opt_level)) {
    if (YAP_IntOfTerm(arg_opt_level) > 0 && YAP_IntOfTerm(arg_opt_level) < 4) {
      if (YAP_Unify(arg_count, YAP_MkIntTerm(trie_get_db_opt_level_count(YAP_IntOfTerm(arg_opt_level))))) {
        YAP_cut_succeed();
        return TRUE;
      } else {
        YAP_cut_fail();
        return FALSE;
      }
    } else {
      YAP_cut_fail();
      return FALSE;
    }
  }
  if (YAP_IsVarTerm(arg_opt_level)) {
    YAP_PRESERVE_DATA(opt_level, db_trie_opt_level);
    opt_level->value = YAP_MkIntTerm(1);
    if (YAP_Unify(arg_opt_level, opt_level->value)) {
      return YAP_Unify(arg_count, YAP_MkIntTerm(trie_get_db_opt_level_count(YAP_IntOfTerm(arg_opt_level))));
    }
    YAP_cut_fail();
    return FALSE;
  }
  YAP_cut_fail();
  return FALSE;
}
#undef arg_opt_level
#undef arg_count


/* trie_get_depth_breadth_reduction_opt_level_count(?OptLevel, -Count) */
#define arg_opt_level YAP_ARG1
#define arg_count YAP_ARG2
static YAP_Bool p_trie_get_db_opt_level_count_cont(void) {
  YAP_PRESERVED_DATA(opt_level, db_trie_opt_level);
  opt_level->value = YAP_MkIntTerm(YAP_IntOfTerm(opt_level->value) + 1);
  if (YAP_IntOfTerm(opt_level->value) < 4) {
    if (YAP_Unify(arg_opt_level, opt_level->value))
      return YAP_Unify(arg_count, YAP_MkIntTerm(trie_get_db_opt_level_count(YAP_IntOfTerm(arg_opt_level))));
    YAP_cut_fail();
    return FALSE;
  } else {
    YAP_cut_fail();
    return FALSE;
  }
}
#undef arg_opt_level
#undef arg_count


/* trie_replace_nested_trie(+Trie, +NestedTrieID, +ReplaceTerm) */
#define arg_trie YAP_ARG1
#define arg_nested_id YAP_ARG2
#define arg_term YAP_ARG3
static YAP_Bool p_trie_replace_nested_trie(void) {
  if (!YAP_IsIntTerm(arg_nested_id))
    return FALSE;
  if (!YAP_IsNonVarTerm(arg_term))
    return FALSE;
  trie_replace_nested_trie((TrEntry) YAP_IntOfTerm(arg_trie), YAP_IntOfTerm(arg_nested_id), arg_term);
  return TRUE;
}
#undef arg_trie
#undef arg_nested_id
#undef arg_term


/* trie_db_opt_min_prefix(?MinPrefix) */
#define arg_min_prefix YAP_ARG1
static YAP_Bool p_trie_db_opt_min_prefix(void) {
  YAP_Int min_prefix;

  /* get mode */
  if (YAP_IsVarTerm(arg_min_prefix)) {
    min_prefix = trie_get_db_opt_min_prefix();
    return YAP_Unify(arg_min_prefix, YAP_MkIntTerm(min_prefix));
  }

  /* set mode */
  if (YAP_IsIntTerm(arg_min_prefix)) {
    min_prefix = YAP_IntOfTerm(arg_min_prefix);
    if (min_prefix > 1) {
      trie_set_db_opt_min_prefix(min_prefix);
      return TRUE;
    }
  }
  return FALSE;
}
#undef min_prefix


#ifdef _WIN32

#include <windows.h>

int WINAPI win_tries(HANDLE, DWORD, LPVOID);

int WINAPI win_tries(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason) 
    {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }
  return 1;
}
#endif

/// @}
