/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		tries.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	Tries manipulation routines	 			 *
*									 *
*************************************************************************/

:- module(tries, [
		  open_trie/1,
		  close_trie/1,
		  close_all_tries/0,
		  put_trie_entry/3,
		  get_trie_entry/2,
		  remove_trie_entry/1,
		  trie_statistics/0,
		  print_trie/1
          ]).

:- load_foreign_files([yap_tries], [], init_tries).

