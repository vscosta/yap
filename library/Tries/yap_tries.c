/******************************************
* File:	    yap_tries.c			  *
* Author:   Ricardo Rocha                 *
* Comments: Tries module for yap          *
******************************************/

#include "config.h"
#include "YapInterface.h"
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif

#include "tries.h"
#include "tries.c"

void init_tries(void);
static int p_open_trie(void);
static int p_close_trie(void);
static int p_close_all_tries(void);
static int p_put_trie_entry(void);
static int p_get_trie_entry(void);
static int p_remove_trie_entry(void);
static int p_trie_stats(void);
static int p_print_trie(void);


void init_tries(void) {
  TRIES = NULL;
  HASHES = NULL;
  MEMORY_IN_USE = 0;
  MEMORY_MAX_USED = 0;
  NODES_IN_USE = 0;
  NODES_MAX_USED = 0;
  HASHES_IN_USE = 0;
  HASHES_MAX_USED = 0;
  BUCKETS_IN_USE = 0;
  BUCKETS_MAX_USED = 0;

  FunctorComma = YAP_MkFunctor(YAP_LookupAtom(","), 2);

  YAP_UserCPredicate("open_trie", p_open_trie, 1);                  /* -> Trie                    */
  YAP_UserCPredicate("close_trie", p_close_trie, 1);                /* Trie ->                    */
  YAP_UserCPredicate("close_all_tries", p_close_all_tries, 0);      /* ->                         */
  YAP_UserCPredicate("put_trie_entry", p_put_trie_entry, 4);        /* Mode x Trie x Entry -> Ref */
  YAP_UserCPredicate("get_trie_entry", p_get_trie_entry, 3);        /* Mode x Ref -> Entry        */
  YAP_UserCPredicate("remove_trie_entry", p_remove_trie_entry, 1);  /* Ref ->                     */
  YAP_UserCPredicate("trie_statistics", p_trie_stats, 0);           /* ->                         */
  YAP_UserCPredicate("print_trie", p_print_trie, 1);                /* Trie ->                    */

  return;
}


/* open_trie(+Trie) */
static int p_open_trie(void) {
  YAP_Term arg_trie = YAP_ARG1;
  TrNode node;

  /* check arg */
  if (!YAP_IsVarTerm(arg_trie)) 
    return FALSE;

  /* open trie */
  node = open_trie();
  if (!YAP_Unify(arg_trie, YAP_MkIntTerm((int) node))) 
    return FALSE;
  return TRUE;
}


/* close_trie(-Trie) */
static int p_close_trie(void) {
  YAP_Term arg_trie = YAP_ARG1;

  /* check args */
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* close trie */
  close_trie((TrNode) YAP_IntOfTerm(arg_trie));
  return TRUE;
}


/* close_all_tries() */
static int p_close_all_tries(void) {
  close_all_tries();
  return TRUE;
}


/* put_trie_entry(-Mode,-Trie,-Entry,+Ref) */
static int p_put_trie_entry(void) {
  YAP_Term arg_mode = YAP_ARG1;
  YAP_Term arg_trie = YAP_ARG2;
  YAP_Term arg_entry = YAP_ARG3;
  YAP_Term arg_ref = YAP_ARG4;
  TrNode node;
  char *mode_str;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std")) {
    MODE = MODE_STANDARD;
  } else if (!strcmp(mode_str, "rev")) {
    MODE = MODE_REVERSE;
  } else
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* put trie entry */
  node = put_trie_entry((TrNode) YAP_IntOfTerm(arg_trie), arg_entry);
  if (!YAP_Unify(arg_ref, YAP_MkIntTerm((int) node)))
    return FALSE;
  return TRUE;
}


/* get_trie_entry(-Mode,-Ref,+Entry) */
static int p_get_trie_entry(void) {
  YAP_Term arg_mode = YAP_ARG1;
  YAP_Term arg_ref = YAP_ARG2;
  YAP_Term arg_entry = YAP_ARG3;
  YAP_Term entry;
  char *mode_str;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std")) {
    MODE = MODE_STANDARD;
  } else if (!strcmp(mode_str, "rev")) {
    MODE = MODE_REVERSE;
  } else
    return FALSE;
  if (!YAP_IsIntTerm(arg_ref)) 
    return FALSE;

  /* get trie entry */
  entry = get_trie_entry((TrNode) YAP_IntOfTerm(arg_ref));
  if (!YAP_Unify(arg_entry, entry))
    return FALSE;
  return TRUE;
}


/* remove_trie_entry(-Ref) */
static int p_remove_trie_entry(void) {
  YAP_Term arg_ref = YAP_ARG1;

  /* check arg */
  if (!YAP_IsIntTerm(arg_ref))
    return FALSE;

  /* remove trie entry */
  remove_trie_entry((TrNode) YAP_IntOfTerm(arg_ref));
  return TRUE;
}


/* trie_statistics() */
static int p_trie_stats(void) {
  trie_stats();
  return TRUE;
}


/* print_trie(-Trie) */
static int p_print_trie(void) {
  YAP_Term arg_trie = YAP_ARG1;

  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  /* print trie */
  print_trie((TrNode) YAP_IntOfTerm(arg_trie));
  return TRUE;
}
