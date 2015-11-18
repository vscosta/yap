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
* File:		regexp.yap						 *
* Last rev:	5/15/2000						 *
* mods:									 *
* comments:	AVL trees in YAP (from code by M. van Emden, P. Vasey)	 *
*									 *
*************************************************************************/

/**
 * @file   avl.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 00:59:28 2015
 * 
 * @brief  Support for constructing AVL trees
 * 
 * 
*/



:- module(avl, [
	avl_new/1,
	avl_insert/4,
	avl_lookup/3
          ]).

/**
* @defgroup avl AVL Trees
* @ingroup library
@{
Supports constructing AVL trees, available through the directive:

~~~~~~~
:- use_module(library(avl)).
~~~~~~~

It includes the following predicates:

  - avl_insert/4
  - avl_lookup/3
  - avl_new/1

AVL trees are balanced search binary trees. They are named after their
inventors, Adelson-Velskii and Landis, and they were the first
dynamically balanced trees to be proposed. The YAP AVL tree manipulation
predicates library uses code originally written by Martin van Emdem and
published in the Logic Programming Newsletter, Autumn 1981.  A bug in
this code was fixed by Philip Vasey, in the Logic Programming
Newsletter, Summer 1982. The library currently only includes routines to
insert and lookup elements in the tree. Please try red-black trees if
you need deletion.

 
*/


/** @pred avl_new(+ _T_) 


Create a new tree.

 
*/
avl_new([]).

/** @pred avl_insert(+ _Key_,? _Value_,+ _T0_,- _TF_) 


Add an element with key  _Key_ and  _Value_ to the AVL tree
 _T0_ creating a new AVL tree  _TF_. Duplicated elements are
allowed.

 
*/
avl_insert(Key, Value, T0, TF) :-
	insert(T0, Key, Value, TF, _).

insert([], Key, Value, avl([],Key,Value,-,[]), yes).
insert(avl(L,Root,RVal,Bl,R), E, Value, NewTree, WhatHasChanged) :-
	E @< Root, !,
	insert(L, E, Value, NewL, LeftHasChanged),
	adjust(avl(NewL,Root,RVal,Bl,R), LeftHasChanged, left, NewTree, WhatHasChanged).
insert(avl(L,Root,RVal,Bl,R), E, Val, NewTree, WhatHasChanged) :-
%	 E @>= Root, currently we allow duplicated values, although
%        lookup will only fetch the first.
	insert(R, E, Val,NewR, RightHasChanged),
	adjust(avl(L,Root,RVal,Bl,NewR), RightHasChanged, right, NewTree, WhatHasChanged).

adjust(Oldtree, no, _, Oldtree, no).
adjust(avl(L,Root,RVal,Bl,R), yes, Lor, NewTree, WhatHasChanged) :-
	table(Bl, Lor, Bl1, WhatHasChanged, ToBeRebalanced),
	rebalance(avl(L, Root, RVal, Bl, R), Bl1, ToBeRebalanced, NewTree).

%     balance  where     balance  whole tree  to be
%     before   inserted  after    increased   rebalanced
table(-      , left    , <      , yes       , no    ).
table(-      , right   , >      , yes       , no    ).
table(<      , left    , -      , no        , yes   ).
table(<      , right   , -      , no        , no    ).
table(>      , left    , -      , no        , no    ).
table(>      , right   , -      , no        , yes   ).

rebalance(avl(Lst, Root, RVal, _Bl, Rst), Bl1, no, avl(Lst, Root, RVal, Bl1,Rst)).
rebalance(OldTree, _, yes, NewTree) :-
	avl_geq(OldTree,NewTree).

avl_geq(avl(Alpha,A,VA,>,avl(Beta,B,VB,>,Gamma)),
	avl(avl(Alpha,A,VA,-,Beta),B,VB,-,Gamma)).
avl_geq(avl(avl(Alpha,A,VA,<,Beta),B,VB,<,Gamma),
	avl(Alpha,A,VA,-,avl(Beta,B,VB,-,Gamma))).
avl_geq(avl(Alpha,A,VA,>,avl(avl(Beta,X,VX,Bl1,Gamma),B,VB,<,Delta)),
	avl(avl(Alpha,A,VA,Bl2,Beta),X,VX,-,avl(Gamma,B,VB,Bl3,Delta))) :-
        table2(Bl1,Bl2,Bl3).
avl_geq(avl(avl(Alpha,A,VA,>,avl(Beta,X,VX,Bl1,Gamma)),B,VB,<,Delta),
	avl(avl(Alpha,A,VA,Bl2,Beta),X,VX,-,avl(Gamma,B,VB,Bl3,Delta))) :-
        table2(Bl1,Bl2,Bl3).

table2(< ,- ,> ).
table2(> ,< ,- ).
table2(- ,- ,- ).

/** @pred avl_lookup(+ _Key_,- _Value_,+ _T_) 


Lookup an element with key  _Key_ in the AVL tree
 _T_, returning the value  _Value_.

*/

avl_lookup(Key, Value, avl(L,Key0,KVal,_,R)) :-
	compare(Cmp, Key, Key0),
	avl_lookup(Cmp, Value, L, R, Key, KVal).

avl_lookup(=, Value, _, _, _, Value).
avl_lookup(<, Value, L, _, Key, _) :-
	avl_lookup(Key, Value, L).
avl_lookup(>, Value, _, R, Key, _) :-
	avl_lookup(Key, Value, R).


/**
@}
*/
