/******************************************
* File:	    yap_tries.c			  *
* Author:   Ricardo Rocha                 *
* Comments: Tries module for yap          *
******************************************/



/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <YapInterface.h>
#include <string.h>
#include "tries.h"



/* -------------------------- */
/*         Procedures         */
/* -------------------------- */

void init_tries(void);
static int p_open_trie(void);
static int p_close_trie(void);
static int p_close_all_tries(void);
static int p_put_trie_entry(void);
static int p_get_trie_entry(void);
#ifdef ALLOW_REMOVE_TRIE
static int p_remove_trie_entry(void);
#endif /* ALLOW_REMOVE_TRIE */
static int p_trie_stats(void);
static int p_trie_max_stats(void);
static int p_trie_usage(void);
static int p_print_trie(void);



/* -------------------------- */
/*    Module Init Procedure   */     
/* -------------------------- */

void init_tries(void) {
  init_tries_module();

  /* open_trie: -> Trie                         */
  YAP_UserCPredicate("open_trie", p_open_trie, 1);
  /* close_trie: Trie ->                          */
  YAP_UserCPredicate("close_trie", p_close_trie, 1);
  /* close_all_tries: ->                                    */
  YAP_UserCPredicate("close_all_tries", p_close_all_tries, 0);
  /* put_trie_entry: Mode x Trie x Entry -> Ref           */
  YAP_UserCPredicate("put_trie_entry", p_put_trie_entry, 4);
  /* get_trie_entry: Mode x Ref -> Entry                  */
  YAP_UserCPredicate("get_trie_entry", p_get_trie_entry, 3);
#ifdef ALLOW_REMOVE_TRIE
  /* remove_trie_entry: Ref ->                                  */
  YAP_UserCPredicate("remove_trie_entry", p_remove_trie_entry, 1);
#endif /* ALLOW_REMOVE_TRIE */
  /* trie_stats: -> TrieNodes x HashNodes x HashBuckets x Mem */
  YAP_UserCPredicate("trie_stats", p_trie_stats, 4);
  /* trie_max_stats: -> TrieNodes x HashNodes x HashBuckets x Mem */
  YAP_UserCPredicate("trie_max_stats", p_trie_max_stats, 4);
  /* trie_usage: Trie -> Entries x Nodes x VirtualNodes */
  YAP_UserCPredicate("trie_usage", p_trie_usage, 4);
  /* print_trie: Trie ->                          */
  YAP_UserCPredicate("print_trie", p_print_trie, 1);

  return;
}



/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

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
  int mode;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std")) {
    mode = MODE_STANDARD;
  } else if (!strcmp(mode_str, "rev")) {
    mode = MODE_REVERSE;
  } else
    return FALSE;
  if (!YAP_IsIntTerm(arg_trie)) 
    return FALSE;

  /* put trie entry */
  node = put_trie_entry((TrNode) YAP_IntOfTerm(arg_trie), arg_entry, mode);
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
  int mode;

  /* check args */
  mode_str = YAP_AtomName(YAP_AtomOfTerm(arg_mode));
  if (!strcmp(mode_str, "std")) {
    mode = MODE_STANDARD;
  } else if (!strcmp(mode_str, "rev")) {
    mode = MODE_REVERSE;
  } else
    return FALSE;
  if (!YAP_IsIntTerm(arg_ref)) 
    return FALSE;

  /* get trie entry */
  entry = get_trie_entry((TrNode) YAP_IntOfTerm(arg_ref), mode);
  if (!YAP_Unify(arg_entry, entry))
    return FALSE;
  return TRUE;
}


#ifdef ALLOW_REMOVE_TRIE
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
#endif /* ALLOW_REMOVE_TRIE */


/* trie_stats(+TrieNodes,+HashNodes,+HashBuckets,+Mem) */
static int p_trie_stats(void) {
  YAP_Term arg_trienodes = YAP_ARG1;
  YAP_Term arg_hashnodes = YAP_ARG2;
  YAP_Term arg_hashbuckets = YAP_ARG3;
  YAP_Term arg_mem = YAP_ARG4;
  int trienodes;
  int hashnodes;
  int hashbuckets;
  int mem;

  trie_stats(&trienodes, &hashnodes, &hashbuckets, &mem);

  /* all args should be arguments */
  if (!YAP_Unify(arg_trienodes, YAP_MkIntTerm(trienodes)))
    return FALSE;
  if (!YAP_Unify(arg_hashnodes, YAP_MkIntTerm(hashnodes)))
    return FALSE;
  if (!YAP_Unify(arg_hashbuckets, YAP_MkIntTerm(hashbuckets)))
    return FALSE;
  if (!YAP_Unify(arg_mem, YAP_MkIntTerm(mem)))
    return FALSE;
  return TRUE;
}


/* trie_max_stats(+TrieNodes,+HashNodes,+HashBuckets,+Mem) */
static int p_trie_max_stats(void) {
  YAP_Term arg_trienodes  = YAP_ARG1;
  YAP_Term arg_hashnodes = YAP_ARG2;
  YAP_Term arg_hashbuckets = YAP_ARG3;
  YAP_Term arg_mem = YAP_ARG4;
  int trienodes;
  int hashnodes;
  int hashbuckets;
  int mem;

  trie_max_stats(&trienodes, &hashnodes, &hashbuckets, &mem);

  /* all args should be arguments */
  if (!YAP_Unify(arg_trienodes, YAP_MkIntTerm(trienodes)))
    return FALSE;
  if (!YAP_Unify(arg_hashnodes, YAP_MkIntTerm(hashnodes)))
    return FALSE;
  if (!YAP_Unify(arg_hashbuckets, YAP_MkIntTerm(hashbuckets)))
    return FALSE;
  if (!YAP_Unify(arg_mem, YAP_MkIntTerm(mem)))
    return FALSE;
  return TRUE;
}


/* trie_usage(-Trie,+Entries,+Nodes,+VirtualNodes) */
static int p_trie_usage(void) {
  YAP_Term arg_trie = YAP_ARG1;
  YAP_Term arg_entries = YAP_ARG2;
  YAP_Term arg_nodes = YAP_ARG3;
  YAP_Term arg_virtualnodes = YAP_ARG4;
  int entries;
  int nodes;
  int virtualnodes;

  /* check arg */
  if (!YAP_IsIntTerm(arg_trie))
    return FALSE;

  trie_usage((TrNode) YAP_IntOfTerm(arg_trie), &entries, &nodes, &virtualnodes);
  if (!YAP_Unify(arg_entries, YAP_MkIntTerm(entries)))
    return FALSE;
  if (!YAP_Unify(arg_nodes, YAP_MkIntTerm(nodes)))
    return FALSE;
  if (!YAP_Unify(arg_virtualnodes, YAP_MkIntTerm(virtualnodes)))
    return FALSE;

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

#ifdef _WIN32

#include <windows.h>

int WINAPI PROTO(win_tries, (HANDLE, DWORD, LPVOID));

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
