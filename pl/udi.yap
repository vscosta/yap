/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2010	 *
*									 *
**************************************************************************
*									 *
* File:		udi.yap							 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	support user defined indexing				 *
*									 *
*************************************************************************/

:- meta_predicate udi(:).

/******************
*     udi/1     *
******************/

udi(Pred) :-
   '$udi_init'(rtree, Pred).

