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

  YAP_UserCPredicate("open_trie", p_open_trie, 1);                  /* -> Ref */
  YAP_UserCPredicate("close_trie", p_close_trie, 1);                /* Ref -> */
  YAP_UserCPredicate("close_all_tries", p_close_all_tries, 0);      /* -> */
  YAP_UserCPredicate("put_trie_entry", p_put_trie_entry, 3);        /* Ref x Entry -> Ref */
  YAP_UserCPredicate("get_trie_entry", p_get_trie_entry, 2);        /* Ref -> Entry */
  YAP_UserCPredicate("remove_trie_entry", p_remove_trie_entry, 1);  /* Ref -> */
  YAP_UserCPredicate("trie_statistics", p_trie_stats, 0);           /* -> */
  YAP_UserCPredicate("print_trie", p_print_trie, 1);                /* Ref -> */

  return;
}


/* open_trie(+Ref) */
static int p_open_trie(void) {
  YAP_Term arg1 = YAP_ARG1;
  TrNode node;

  /* check arg */
  if (!YAP_IsVarTerm(arg1)) 
    return FALSE;
  /* open trie */
  node = open_trie();
  /* return node reference */
  if (!YAP_Unify(arg1, YAP_MkIntTerm((int) node))) 
    return FALSE;
  return TRUE;
}


/* close_trie(-Ref) */
static int p_close_trie(void) {
  YAP_Term arg1 = YAP_ARG1;

  /* check args */
  if (!YAP_IsIntTerm(arg1)) 
    return FALSE;
  /* free trie */
  close_trie((TrNode) YAP_IntOfTerm(arg1));
  return TRUE;
}


/* close_all_tries() */
static int p_close_all_tries(void) {
  /* close all tries */
  close_all_tries();
  return TRUE;
}


/* put_trie_entry(-Ref,-Entry,+Ref) */
static int p_put_trie_entry(void) {
  YAP_Term arg1 = YAP_ARG1;
  YAP_Term arg2 = YAP_ARG2;
  YAP_Term arg3 = YAP_ARG3;
  TrNode node;

  /* check args */
  if (!YAP_IsIntTerm(arg1)) 
    return FALSE;
  /* put entry */
  node = put_trie_entry((TrNode) YAP_IntOfTerm(arg1), arg2);
  /* return node reference */
  if (!YAP_Unify(arg3, YAP_MkIntTerm((int) node)))
    return FALSE;
  return TRUE;
}


/* get_trie_entry(-Ref,+Entry) */
static int p_get_trie_entry(void) {
  YAP_Term arg1 = YAP_ARG1;
  YAP_Term arg2 = YAP_ARG2;
  YAP_Term entry;

  /* check args */
  if (!YAP_IsIntTerm(arg1)) 
    return FALSE;
  /* get entry */
  entry = get_trie_entry((TrNode) YAP_IntOfTerm(arg1));
  /* return entry reference */
  if (!YAP_Unify(arg2, entry))
    return FALSE;
  return TRUE;
}


/* remove_trie_entry(-Ref) */
static int p_remove_trie_entry(void) {
  YAP_Term arg1 = YAP_ARG1;

  /* check arg */
  if (!YAP_IsIntTerm(arg1))
    return FALSE;
  /* remove trie entry */
  remove_trie_entry((TrNode) YAP_IntOfTerm(arg1));
  return TRUE;
}


/* trie_statistics() */
static int p_trie_stats(void) {
  /* print trie statistics */
  trie_stats();
  return TRUE;
}


/* print_trie(-Ref) */
static int p_print_trie(void) {
  YAP_Term arg1 = YAP_ARG1;

  /* check arg */
  if (!YAP_IsIntTerm(arg1))
    return FALSE;
  /* print trie */
  print_trie((TrNode) YAP_IntOfTerm(arg1));
  return TRUE;
}
