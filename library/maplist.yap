
/**
 * @file   maplist.yap
 * @author Lawrence Byrd + Richard A. O'Keefe, VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @author : E. Alphonse from code by Joachim Schimpf, Jan Wielemaker, Vitor Santos Costa
 * @date    4 August 1984 and Ken Johnson 11-8-87
 *
 * @brief  Macros to apply a predicate to all elements of a list.
 *
 *
*/

:- module(maplist,
	  [maplist/2,
	   maplist/3,
	   maplist/4,
	   maplist/5,
	   maplist/6,
	   checklist/2,
	   checknodes/2,
	   convlist/3,
	   convlist/4,
	   foldl/4,
	   foldl/5,
	   foldl/6,
	   foldl/7,
	   foldl2/6,
	   foldl2/7,
	   foldl2/8,
	   foldl3/8,
	   foldl4/10,
	   include/3,
	   exclude/3,
	   mapnodes/3,
	   partition/4,
	   partition/5,
	   scanl/4,
	   scanl/5,
	   scanl/6,
	   scanl/7,
	   selectlist/3,
	   selectlist/4,
	   selectlists/5,
	   sumlist/4,
	   sumnodes/4
	  ]).

:- meta_predicate
       selectlist(1,+,-),
       selectlist(2,+,+,-),
       checklist(1,+),
       maplist(1,+),
       maplist(2,+,-),
       maplist(3,+,+,-),
       maplist(4,+,+,+,-),
       maplist(5,+,+,+,+,-),
       convlist(2,+,-),
       convlist(3,?,?,?),
       mapnodes(2,+,-),
       mapnodes_list(2,+,-),
       checknodes(1,+),
       checknodes_list(1,+),
       sumlist(3,+,+,-),
       sumnodes(3,+,+,-),
       sumnodes_body(3,+,+,-,+,+),
       include(1,+,-),
       exclude(1,+,-),
       partition(1,+,-,-),
       partition(2,+,-,-,-),
       foldl(3, +, +, -),
       foldl2(5, +, +, -, +, -),
       foldl2(6, +, ?, +, -, +, -),
       foldl2(6, +, ?, ?, +, -, +, -),
       foldl3(5, +, +, -, +, -, +, -),
       foldl4(7, +, +, -, +, -, +, -, +, -),
       foldl(4, +, +, +, -),
       foldl(5, +, +, +, +, -),
       foldl(6, +, +, +, +, +, -),
       scanl(3, +, +, -),
       scanl(4, +, +, +, -),
       scanl(5, +, +, +, +, -),
       scanl(6, +, +, +, +, +, -).

:- use_module(library(maputils)).
:- use_module(library(lists), [append/3]).
:- use_module(library(charsio), [format_to_chars/3, read_from_chars/2]).
:- use_module(library(occurs), [sub_term/2]).

/**
  * @defgroup maplist Map List and Term Operations
  * @ingroup library
  * @{
  *
  * This library provides a set of utilities for applying a predicate to
  * all elements of a list. They allow one to easily perform the most common do-loop constructs in Prolog.
  *  To avoid performance degradation, each call creates an
  * equivalent Prolog program, without meta-calls, which is executed by
  * the Prolog engine instead. The library was based on code
  * by Joachim Schimpf and on code from SWI-Prolog, and it is also inspired by the GHC
  * libraries.
  *
  * The  routines are available once included with the
  * `use_module(library(maplist))` command.
  * Examples:
  * 
  * ~~~~
  * plus(X,Y,Z) :- Z is X + Y.
  * 
  * plus_if_pos(X,Y,Z) :- Y > 0, Z is X + Y.
  * 
  * vars(X, Y, [X|Y]) :- var(X), !.
  * vars(_, Y, Y).
  * 
  * trans(TermIn, TermOut) :-
  *         nonvar(TermIn),
  *         TermIn =.. [p|Args],
  *         TermOut =..[q|Args], !.
  * trans(X,X).
  * ~~~~
  * %success
  * 
  *   ?- maplist(plus(1), [1,2,3,4], [2,3,4,5]).
  * 
  *   ?- checklist(var, [X,Y,Z]).
  * 
  *   ?- selectlist(<(0), [-1,0,1], [1]).
  * 
  *   ?- convlist(plus_if_pos(1), [-1,0,1], [2]).
  * 
  *   ?- sumlist(plus, [1,2,3,4], 1, 11).
  * 
  *   ?- maplist(mapargs(number_atom),[c(1),s(1,2,3)],[c('1'),s('1','2','3')]).
  * ~~~~
  * 
  **/

/** @pred maplist( 2:Pred, + _List1_,+ _List2_)

Apply  _Pred_ on all successive pairs of elements from
 _List1_ and
 _List2_. Fails if  _Pred_ can not be applied to a
pair. See the example above.


*/

/** @pred maplist(3:Pred,+ List1,+ List2,+ List4)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_ and  _List3_. Fails if  _Pred_ can not be applied to a
triple. See the example above.

 */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Definitions for Metacalls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
  @pred  include( 2:Pred, + ListIn, ? ListOut)

  Same as selectlist/3.
*/
include(G,In,Out) :-
    selectlist(G, In, Out).

/**
  @pred  selectlist(1:Pred, + ListIn, ? ListOut))

  Creates  _ListOut_ of all list elements of  _ListIn_ that pass a given test
*/
selectlist(_, [], []).
selectlist(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	 ListOut = [In|NewListOut]
    ;
    ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, NewListOut).

/**
  @pred  selectlist( 2:Pred, + ListIn, + ListInAux, ? ListOut, ? ListOutAux)

  Creates  _ListOut_ and _ListOutAux_ of all list elements of  _ListIn_ and _ListInAux_ that
  pass the given test  _Pred_.
*/
selectlists(_, [], [], [], []).
selectlists(Pred, [In|ListIn], [In1|ListIn1], ListOut, ListOut1) :-
    (call(Pred, In, In1) ->
	 ListOut = [In|NewListOut],
	 ListOut1 = [In1|NewListOut1]
    ;
    ListOut1 = NewListOut1,
    ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, ListIn1, NewListOut, NewListOut1).

/** @pred  selectlist( 2:Pred, + ListIn, + ListInAux, ? ListOut)

  Creates  _ListOut_ of all list elements of  _ListIn_ that
  pass the given test  _Pred_ using + _ListInAux_ as an
  auxiliary element.
*/
selectlist(_, [], [], []).
selectlist(Pred, [In|ListIn], [In1|ListIn1], ListOut) :-
    (call(Pred, In, In1) ->
	 ListOut = [In|NewListOut]
    ;
    ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, ListIn1, NewListOut).

/**
  @pred  exclude( 2:Goal, + List1, ? List2)

  Filter elements for which  _Goal_ fails. True if  _List2_ contains
  those elements  _Xi_ of  _List1_ for which `call(Goal, Xi)` fails.
*/
exclude(_, [], []).
exclude(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	 ListOut = NewListOut
    ;
    ListOut = [In|NewListOut]
    ),
    exclude(Pred, ListIn, NewListOut).

/**
  @pred  partition(1:Pred,  + List1, ? Included, ? Excluded)

  Filter elements of  _List1_ according to  _Pred_. True if
  _Included_ contains all elements for which `call(Pred, X)`
  succeeds and  _Excluded_ contains the remaining elements.
 */
partition(_, [], [], []).
partition(Pred, [In|ListIn], List1, List2) :-
    (call(Pred, In) ->
	 List1 = [In|RList1],
	 List2 = RList2
    ;
    List1 = RList1,
    List2 = [In|RList2]
    ),
    partition(Pred, ListIn, RList1, RList2).

/**
  @pred  partition(2:Pred,  + List1, ? Lesser, ? Equal, ? Greater)

  Filter list according to  _Pred_ in three sets. For each element
  _Xi_ of  _List_, its destination is determined by
  `call(Pred, Xi, Place)`, where  _Place_ must be unified to one
  of `\<`, `=` or `\>`. `Pred` must be deterministic.


*/
partition(_, [], [], [], []).
partition(Pred, [In|ListIn], List1, List2, List3) :-
    call(Pred, In, Diff),
    ( Diff == (<)  ->
      List1 = [In|RList1],
      List2 = RList2,
      List3 = RList3
    ;
    Diff == (=)  ->
    List1 = RList1,
    List2 = [In|RList2],
    List3 = RList3
    ;
    Diff == (>)  ->
    List1 = RList1,
    List2 = RList2,
    List3 = [In|RList3]
    ;
    must_be(oneof([<,=,>]), Diff)
    ),
    partition(Pred, ListIn, RList1, RList2, RList3).

/**
  @pred  checklist( 1:Pred, + List)

  Succeeds if the predicate  _Pred_ succeeds on all elements of  _List_.
*/
checklist(_, []).
checklist(Pred, [In|ListIn]) :-
    call(Pred, In),
    checklist(Pred, ListIn).

/**
  @pred  
  ist(: Pred, ? ListIn)

  Applies predicate  _Pred_( _El_ ) to all
  elements _El_ of  _ListIn_.

*/
maplist(_, []).
maplist(Pred, [In|ListIn]) :-
    call(Pred, In),
    maplist(Pred, ListIn).


/**
  @pred  maplist(: Pred, ? L1, ? L2 )
  _L1_  and  _L2_ are such that
  `call( _Pred_, _A1_, _A2_)` holds for every
  corresponding element in lists  _L1_,   _L2_.

  Comment from Richard O'Keefe: succeeds when _Pred( _Old_, _New_) succeeds for each corresponding
  _Gi_ in _Listi_, _New_ in _NewList_.  In InterLisp, this is MAPCAR.
   It is also MAP2C.  Isn't bidirectionality wonderful?
*/
maplist(_, [], []).
maplist(Pred, [In|ListIn], [Out|ListOut]) :-
    call(Pred, In, Out),
    maplist(Pred, ListIn, ListOut).

/**
  @pred  maplist(: Pred, ? L1, ? L2, ? L3)
  _L1_,   _L2_, and  _L3_ are such that
  `call( _Pred_, _A1_, _A2_, _A3_)` holds for every
  corresponding element in lists  _L1_,   _L2_, and  _L3_.

*/
maplist(_, [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3]) :-
    call(Pred, A1, A2, A3),
    maplist(Pred, L1, L2, L3).

/**
  @pred  maplist(: Pred, ? L1, ? L2, ? L3, ? L4)

  _L1_,  _L2_,  _L3_, and  _L4_ are such that
  `call( _Pred_, _A1_, _A2_, _A3_, _A4_)` holds
  for every corresponding element in lists  _L1_,  _L2_,  _L3_, and
  _L4_.
*/
maplist(_, [], [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3], [A4|L4]) :-
    call(Pred, A1, A2, A3, A4),
    maplist(Pred, L1, L2, L3, L4).

/**
  @pred  maplist(: Pred, ? L1, ? L2, ? L3, ? L4, ? L5)

  _L1_,  _L2_,  _L3_, _L4_ and  _L5_ are such that
  `call( _Pred_, _A1_, _A2_, _A3_, _A4_,_A5_)` holds
  for every corresponding element in lists  _L1_,  _L2_,  _L3_, _L4_ and  _L5_.
*/
maplist(_, [], [], [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3], [A4|L4], [A5|L5]) :-
    call(Pred, A1, A2, A3, A4, A5),
    maplist(Pred, L1, L2, L3, L4, L5).

/**
  @pred convlist(: Pred, + ListIn, ? ListOut)

  A combination of maplist/3 and selectlist/3: creates  _ListOut_ by
  applying the predicate  _Pred_ to all list elements on which
  _Pred_ succeeds.

  ROK: convlist(Rewrite, OldList, NewList)
  is a sort of hybrid of maplist/3 and sublist/3.
  Each element of NewList is the image under Rewrite of some
  element of OldList, and order is preserved, but elements of
  OldList on which Rewrite is undefined (fails) are not represented.
  Thus if foo(X,Y) :- integer(X), Y is X+1.
  then convlist(foo, [1,a,0,joe(99),101], [2,1,102]).
*/
convlist(_, [], []).
convlist(Pred, [Old|Olds], NewList) :-
    call(Pred, Old, New),
    !,
    NewList = [New|News],
    convlist(Pred, Olds, News).
convlist(Pred, [_|Olds], News) :-
    convlist(Pred, Olds, News).

/**
  @pred convlist(: Pred, ? ListIn, ?ExtraList, ? ListOut)

  A combination of maplist/4 and selectlist/3: _ListIn_, _ListExtra_,
  and _ListOut_ are the sublists so that the predicate _Pred_ succeeds.

  ROK: convlist(Rewrite, OldList, NewList)
  is a sort of hybrid of maplist/3 and sublist/3.
  Each element of NewList is the image under Rewrite of some
  element of OldList, and order is preserved, but elements of
  OldList on which Rewrite is undefined (fails) are not represented.
  Thus if foo(X,Y) :- integer(X), Y is X+1.
  then convlist(foo, [1,a,0,joe(99),101], [2,1,102]).
*/
convlist(_, [], []).
convlist(Pred, [Old|Olds], NewList) :-
    call(Pred, Old, New),
    !,
    NewList = [New|News],
    convlist(Pred, Olds, News).
convlist(Pred, [_|Olds], News) :-
    convlist(Pred, Olds, News).

/**
  @pred mapnodes(+ _Pred_, + _TermIn_, ? _TermOut_)

  Creates  _TermOut_ by applying the predicate  _Pred_
  to all sub-terms of  _TermIn_ (depth-first and left-to-right order).
*/
mapnodes(Pred, TermIn, TermOut) :-
    (atomic(TermIn); var(TermIn)), !,
    call(Pred, TermIn, TermOut).
mapnodes(Pred, TermIn, TermOut) :-
    call(Pred, TermIn, Temp),
    Temp =.. [Func|ArgsIn],
    mapnodes_list(Pred, ArgsIn, ArgsOut),
    TermOut =.. [Func|ArgsOut].

mapnodes_list(_, [], []).
mapnodes_list(Pred, [TermIn|ArgsIn], [TermOut|ArgsOut]) :-
    mapnodes(Pred, TermIn, TermOut),
    mapnodes_list(Pred, ArgsIn, ArgsOut).

/**
  @pred checknodes(+ _Pred_, + _Term_)  

  Succeeds if the predicate  _Pred_ succeeds on all sub-terms of
  _Term_ (depth-first and left-to-right order)
*/
checknodes(Pred, Term) :-
    (atomic(Term); var(Term)), !,
    call(Pred, Term).
checknodes(Pred, Term) :-
    call(Pred, Term),
    Term =.. [_|Args],
    checknodes_list(Pred, Args).

checknodes_list(_, []).
checknodes_list(Pred, [Term|Args]) :-
    checknodes_body(Pred, Term),
    checknodes_list(Pred, Args).

/** 
  @pred sumlist(: _Pred_, + _List_, ? _AccIn_, ? _AccOut_)

  Calls  _Pred_ on all elements of List and collects a result in
  _Accumulator_. Same as fold/4.
*/
sumlist(_, [], Acc, Acc).
sumlist(Pred, [H|T], AccIn, AccOut) :-
    call(Pred, H, AccIn, A1),
    sumlist(Pred, T, A1, AccOut).

/**
  @pred sumnodes(+ _Pred_, + _Term_, ? _AccIn_, ? _AccOut_)

  Calls the predicate  _Pred_ on all sub-terms of  _Term_ and
  collect a result in  _Accumulator_ (depth-first and left-to-right
  order)
*/
sumnodes(Pred, Term, A0, A2) :-
    call(Pred, Term, A0, A1),
    (compound(Term) ->
	 functor(Term, _, N),
	 sumnodes_body(Pred, Term, A1, A2, 0, N)
    ;	% simple term or variable
	A1 = A2
    ).

sumnodes_body(Pred, Term, A1, A3, N0, Ar) :-
    N0 < Ar ->
	N is N0+1,
	arg(N, Term, Arg),
	sumnodes(Pred, Arg, A1, A2),
	sumnodes_body(Pred, Term, A2, A3, N, Ar)
    ;
    A1 = A3.


/*******************************
		 *	      FOLDL		*
		 *******************************/

%%
%% @pred foldl(:Goal, +List, +V0, -V, +W0, -WN).
%

/**
  @pred oldl(: _Pred_, + _List1_, + _List2_, ? _AccIn_, ? _AccOut_)

  The foldl family of predicates is defined
	  ==
	  foldl(P, [X11,...,X1n],V0, Vn, W0, WN) :-
		P(X11, V0, V1, W0, W1),
		...
		P(X1n, Vn1, Vn, Wn1, Wn).
	  ==
  Calls  _Pred_ on all elements of `List1` and collects a result in  _Accumulator_. Same as
  foldr/3.
*/
foldl(Goal, List, V0, V) :-
    foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
    call(Goal, H, V0, V1),
    foldl_(T, Goal, V1, V).

/**
  @pred foldl(: _Pred_, + _List1_, + _List2_, ? _AccIn_, ? _AccOut_)

  Calls  _Pred_ on all elements of _List1_ and
  _List2_ and collects a result in  _Accumulator_. Same as
  foldr/4.

*/
foldl(Goal, List1, List2, V0, V) :-
    foldl_(List1, List2, Goal, V0, V).

foldl_([], [], _, V, V).
foldl_([H1|T1], [H2|T2], Goal, V0, V) :-
    call(Goal, H1, H2, V0, V1),
    foldl_(T1, T2, Goal, V1, V).

/**

@pred foldl(Goal, List1, List2, List3, V0, V)

Apply _Goal_ plus five arguuments, three map to lists,
two can be used as a difference_type.

*/
foldl(Goal, List1, List2, List3, V0, V) :-
    foldl_(List1, List2, List3, Goal, V0, V).

foldl_([], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V) :-
    call(Goal, H1, H2, H3, V0, V1),
    foldl_(T1, T2, T3, Goal, V1, V).


/**

*/
foldl(Goal, List1, List2, List3, List4, V0, V) :-
    foldl_(List1, List2, List3, List4, Goal, V0, V).

foldl_([], [], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V0, V) :-
    call(Goal, H1, H2, H3, H4, V0, V1),
    foldl_(T1, T2, T3, T4, Goal, V1, V).


/**
   @pred foldl2(: _Pred_, + _List_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of `List` and collects a result in
  _X_ and  _Y_.

*/
foldl2(Goal, List, V0, V, W0, W) :-
    foldl2_(List, Goal, V0, V, W0, W).

foldl2_([], _, V, V, W, W).
foldl2_([H|T], Goal, V0, V, W0, W) :-
    call(Goal, H, V0, V1, W0, W1),
    foldl2_(T, Goal, V1, V, W1, W).

/**
v   @pred foldl2(: _Pred_, + _List_, ? _List1_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of  _List_  and  _List1_  and collects a result in
  _X_ and  _Y_.
*/
foldl2(Goal, List1, List2, V0, V, W0, W) :-
    foldl2_(List1, List2, Goal, V0, V, W0, W).

foldl2_([], [], _Goal, V, V, W, W).
foldl2_([H1|T1], [H2|T2], Goal, V0, V, W0, W) :-
    call(Goal, H1, H2, V0, V1, W0, W1),
    foldl2_(T1, T2, Goal, V1, V, W1, W).

/**
   @pred foldl2(: _Pred_, + _List_, ? _List1_, ? _List2_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of  _List_,  _List1_  and  _List2_  and collects a result in
  _X_ and  _Y_.

*/
foldl2(Goal, List1, List2, List3, V0, V, W0, W) :-
    foldl2_(List1, List2, List3, Goal, V0, V, W0, W).

foldl2_([], [], [], _Goal, V, V, W, W).
foldl2_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V, W0, W) :-
    call(Goal, H1, H2, H3, V0, V1, W0, W1),
    foldl2_(T1, T2, T3, Goal, V1, V, W1, W).


/**
   @pred foldl3(: _Pred_, + _List1_, ? _List2_, ? _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_ and  _Z_.
*/
foldl3(Goal, List, V0, V, W0, W, X0, X) :-
    foldl3_(List, Goal, V0, V, W0, W, X0, X).

foldl3_([], _, V, V, W, W, X, X).
foldl3_([H|T], Goal, V0, V, W0, W, X0, X) :-
    call(Goal, H, V0, V1, W0, W1, X0, X1),
    fold3_(T, Goal, V1, V, W1, W, X1, X).

/**
  @pred foldl4(: _Pred_, + _List1_, ? _List2_, ? _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_, ? _W0_, ? _W_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_,  _Z_ and  _W_.
*/
foldl4(Goal, List, V0, V, W0, W, X0, X, Y0, Y) :-
    foldl4_(List, Goal, V0, V, W0, W, X0, X, Y0, Y).

foldl4_([], _, V, V, W, W, X, X, Y, Y).
foldl4_([H|T], Goal, V0, V, W0, W, X0, X, Y0, Y) :-
    call(Goal, H, V0, V1, W0, W1, X0, X1, Y0, Y1),
    foldl4_(T, Goal, V1, V, W1, W, X1, X, Y1, Y).



/*******************************
		 *	       SCANL		*
		 *******************************/

%%	scanl(:Goal, +List, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +List4, +V0, -Values).
%
%	Left scan of  list.  The  scanl   family  of  higher  order list
%	operations is defined by:
%
%	  ==
%	  scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, [V0,V1,...,Vn]) :-
%		P(X11, ..., Xmn, V0, V1),
%		...
%	        P(X1n, ..., Xmn, V', Vn).
%	  ==

/**


Left scan of  list.  The  scanl   family  of  higher  order list
operations is defined by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, [V0,V1,...,Vn]) :-
        P(X11, ..., Xm1, V0, V1),
        ...
            P(X1n, ..., Xmn, Vn-1, Vn).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/
scanl(Goal, List, V0, [V0|Values]) :-
    scanl_(List, Goal, V0, Values).

scanl_([], _, _, []).
scanl_([H|T], Goal, V, [VH|VT]) :-
    call(Goal, H, V, VH),
    scanl_(T, Goal, VH, VT).

/**
  scanl(: _Pred_, + _List1_, + _List2_, ? _V0_, ? _Vs_)

Left scan of  list.
 */
scanl(Goal, List1, List2, V0, [V0|Values]) :-
    scanl_(List1, List2, Goal, V0, Values).

scanl_([], [], _, _, []).
scanl_([H1|T1], [H2|T2], Goal, V, [VH|VT]) :-
    call(Goal, H1, H2, V, VH),
    scanl_(T1, T2, Goal, VH, VT).

/**
 scanl(: _Pred_, + _List1_, + _List2_, + _List3_, ? _V0_, ? _Vs_)

Left scan of  list.
*/
scanl(Goal, List1, List2, List3, V0, [V0|Values]) :-
    scanl_(List1, List2, List3, Goal, V0, Values).

scanl_([], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], Goal, V, [VH|VT]) :-
    call(Goal, H1, H2, H3, V, VH),
    scanl_(T1, T2, T3, Goal, VH, VT).

/**
  scanl(: _Pred_, + _List1_, + _List2_, + _List3_, + _List4_, ? _V0_, ? _Vs_)

  Left scan of  list.
*/
scanl(Goal, List1, List2, List3, List4, V0, [V0|Values]) :-
    scanl_(List1, List2, List3, List4, Goal, V0, Values).

scanl_([], [], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V, [VH|VT]) :-
    call(Goal, H1, H2, H3, H4, V, VH),
    scanl_(T1, T2, T3, T4, Goal, VH, VT).


goal_expansion(checklist(Meta, List), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(checklist, 2, Proto, GoalName),
    append(MetaVars, [List], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[]], Base),
    append_args(HeadPrefix, [[In|Ins]], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(maplist(Meta, List), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(maplist, 2, Proto, GoalName),
    append(MetaVars, [List], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[]], Base),
    append_args(HeadPrefix, [[In|Ins]], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(maplist(Meta, ListIn, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(maplist, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], [Out|Outs]], RecursionHead),
    append_args(Pred, [In, Out], Apply),
    append_args(HeadPrefix, [Ins, Outs], RecursiveCall),
    compile_aux([	       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(maplist(Meta, L1, L2, L3), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(maplist, 4, Proto, GoalName),
    append(MetaVars, [L1, L2, L3], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], []], Base),
    append_args(HeadPrefix, [[A1|A1s], [A2|A2s], [A3|A3s]], RecursionHead),
    append_args(Pred, [A1, A2, A3], Apply),
    append_args(HeadPrefix, [A1s, A2s, A3s], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(maplist(Meta, L1, L2, L3, L4), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(maplist, 5, Proto, GoalName),
    append(MetaVars, [L1, L2, L3, L4], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], []], Base),
    append_args(HeadPrefix, [[A1|A1s], [A2|A2s], [A3|A3s], [A4|A4s]], RecursionHead),
    append_args(Pred, [A1, A2, A3, A4], Apply),
    append_args(HeadPrefix, [A1s, A2s, A3s, A4s], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(maplist(Meta, L1, L2, L3, L4, L5), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(maplist, 6, Proto, GoalName),
    append(MetaVars, [L1, L2, L3, L4, L5], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], [], []], Base),
    append_args(HeadPrefix, [[A1|A1s], [A2|A2s], [A3|A3s], [A4|A4s], [A5|A5s]], RecursionHead),
    append_args(Pred, [A1, A2, A3, A4, A5], Apply),
    append_args(HeadPrefix, [A1s, A2s, A3s, A4s, A5s], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(selectlist(Meta, ListIn, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(selectlist, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			    RecursiveCall)
		   ], Mod).

goal_expansion(selectlist(Meta, ListIn, ListIn1, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(selectlist, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListIn1, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], []], Base),
    append_args(HeadPrefix, [[In|Ins], [In1|Ins1], Outs], RecursionHead),
    append_args(Pred, [In, In1], Apply),
    append_args(HeadPrefix, [Ins, Ins1, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			    RecursiveCall)
		   ], Mod).

goal_expansion(selectlists(Meta, ListIn, ListIn1, ListOut, ListOut1), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(selectlists, 4, Proto, GoalName),
    append(MetaVars, [ListIn, ListIn1, ListOut, ListOut1], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], []], Base),
    append_args(HeadPrefix, [[In|Ins], [In1|Ins1], Outs, Outs1], RecursionHead),
    append_args(Pred, [In, Out], Apply),
    append_args(HeadPrefix, [Ins, Ins1, NOuts, NOuts1], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [Out|NOuts], Outs1 = [In1|NOuts1]; Outs = NOuts,  Outs1 = NOuts1),
			    RecursiveCall)
		   ], Mod).

% same as selectlist
goal_expansion(include(Meta, ListIn, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(include, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [In|NOuts]; Outs = NOuts),
			    RecursiveCall)
		   ], Mod).

goal_expansion(exclude(Meta, ListIn, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(exclude, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = NOuts; Outs = [In|NOuts]),
			    RecursiveCall)
		   ], Mod).

goal_expansion(partition(Meta, ListIn, List1, List2), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(partition, 4, Proto, GoalName),
    append(MetaVars, [ListIn, List1, List2], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs1, Outs2], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Ins, NOuts1, NOuts2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs1 = [In|NOuts1], Outs2 = NOuts2; Outs1 = NOuts1, Outs2 = [In|NOuts2]),
			    RecursiveCall)
		   ], Mod).

goal_expansion(partition(Meta, ListIn, List1, List2, List3), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(partition2, 5, Proto, GoalName),
    append(MetaVars, [ListIn, List1, List2, List3], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs1, Outs2, Outs3], RecursionHead),
    append_args(Pred, [In,Diff], Apply),
    append_args(HeadPrefix, [Ins, NOuts1, NOuts2, NOuts3], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            Apply,
		            (Diff == (<)  ->
				 Outs1 = [In|NOuts1],
				 Outs2 = NOuts2,
				 Outs3 = NOuts3
			    ;
			    Diff == (=)  ->
			    Outs1 = NOuts1,
			    Outs2 = [In|NOuts2],
			    Outs3 = NOuts3
			    ;
			    Diff == (>)  ->
			    Outs1 = NOuts1,
			    Outs2 = NOuts2,
			    Outs3 = [In|NOuts3]
			    ;
			    must_be(oneof([<,=,>]), Diff)
			    ),
			    RecursiveCall)
		   ], Mod).

goal_expansion(convlist(Meta, ListIn, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(convlist, 3, Proto, GoalName),
    append(MetaVars, [ListIn, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], Outs], RecursionHead),
    append_args(Pred, [In, Out], Apply),
    append_args(HeadPrefix, [Ins, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [Out|NOuts]; Outs = NOuts),
			    RecursiveCall)
		   ], Mod).

goal_expansion(convlist(Meta, ListIn, ListExtra, ListOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(convlist, 4, Proto, GoalName),
    append(MetaVars, [ListIn, ListExtra, ListOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], []], Base),
    append_args(HeadPrefix, [[In|Ins], [Extra|Extras], Outs], RecursionHead),
    append_args(Pred, [In, Extra, Out], Apply),
    append_args(HeadPrefix, [Ins, Extras, NOuts], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            (Apply -> Outs = [Out|NOuts]; Outs = NOuts),
			    RecursiveCall)
		   ], Mod).

goal_expansion(sumlist(Meta, List, AccIn, AccOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(sumlist, 4, Proto, GoalName),
    append(MetaVars, [List, AccIn, AccOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3], Apply),
    append_args(HeadPrefix, [Ins, Acc3, Acc2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl(Meta, List, AccIn, AccOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl, 4, Proto, GoalName),
    append(MetaVars, [List, AccIn, AccOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3], Apply),
    append_args(HeadPrefix, [Ins, Acc3, Acc2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl(Meta, List1, List2, AccIn, AccOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl, 5, Proto, GoalName),
    append(MetaVars, [List1, List2, AccIn, AccOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], Acc, Acc], Base),
    append_args(HeadPrefix, [[In|Ins], [I2|Is2], Acc1, Acc2], RecursionHead),
    append_args(Pred, [In, I2, Acc1, Acc3], Apply),
    append_args(HeadPrefix, [Ins, Is2, Acc3, Acc2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl(Meta, List1, List2, List3, AccIn, AccOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl, 6, Proto, GoalName),
    append(MetaVars, [List1, List2, List3, AccIn, AccOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], Acc, Acc], Base),
    append_args(HeadPrefix, [[In|Ins], [I2|I2s], [I3|I3s], Acc1, Acc2], RecursionHead),
    append_args(Pred, [In, I2, I3, Acc1, Acc3], Apply),
    append_args(HeadPrefix, [Ins, I2s, I3s, Acc3, Acc2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl2(Meta, List, AccIn, AccOut, W0, W), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl2, 6, Proto, GoalName),
    append(MetaVars, [List, AccIn, AccOut, W0, W], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc, W, W], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3, W1, W3], Apply),
    append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl2(Meta, List1, List2, AccIn, AccOut, W0, W), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl2, 7, Proto, GoalName),
    append(MetaVars, [List1, List2, AccIn, AccOut, W0, W], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], Acc, Acc, W, W], Base),
    append_args(HeadPrefix, [[In1|Ins1], [In2|Ins2], Acc1, Acc2, W1, W2], RecursionHead),
    append_args(Pred, [In1, In2, Acc1, Acc3, W1, W3], Apply),
    append_args(HeadPrefix, [Ins1, Ins2, Acc3, Acc2, W3, W2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl2(Meta, List1, List2, List3, AccIn, AccOut, W0, W), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl2, 7, Proto, GoalName),
    append(MetaVars, [List1, List2, List3, AccIn, AccOut, W0, W], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], [], [], Acc, Acc, W, W], Base),
    append_args(HeadPrefix, [[In1|Ins1], [In2|Ins2], [In3|Ins3], Acc1, Acc2, W1, W2], RecursionHead),
    append_args(Pred, [In1, In2, In3, Acc1, Acc3, W1, W3], Apply),
    append_args(HeadPrefix, [Ins1, Ins2, Ins3, Acc3, Acc2, W3, W2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl3(Meta, List, AccIn, AccOut, W0, W, X0, X), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl3, 8, Proto, GoalName),
    append(MetaVars, [List, AccIn, AccOut, W0, W, X0, X], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc, W, W, X, X], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2, X1, X2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3, W1, W3, X1, X3], Apply),
    append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2, X3, X2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(foldl4(Meta, List, AccIn, AccOut, W0, W, X0, X, Y0, Y), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(foldl4, 8, Proto, GoalName),
    append(MetaVars, [List, AccIn, AccOut, W0, W, X0, X, Y0, Y], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc, W, W, X, X, Y, Y], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2, W1, W2, X1, X2, Y1, Y2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3, W1, W3, X1, X3, Y1, Y3], Apply),
    append_args(HeadPrefix, [Ins, Acc3, Acc2, W3, W2, X3, X2, Y3, Y2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :- Apply, RecursiveCall)
		   ], Mod).

goal_expansion(mapnodes(Meta, InTerm, OutTerm), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(mapnodes, 3, Proto, GoalName),
    append(MetaVars, [[InTerm], [OutTerm]], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], []], Base),
    append_args(HeadPrefix, [[In|Ins], [Out|Outs]], RecursionHead),
    append_args(Pred, [In, Temp], Apply),
    append_args(HeadPrefix, [InArgs, OutArgs], SubRecursiveCall),
    append_args(HeadPrefix, [Ins, Outs], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            Apply,
			    (compound(Temp)
			    ->
				Temp =.. [F|InArgs],
				SubRecursiveCall,
				Out =.. [F|OutArgs]
			    ;
			    Out = Temp
			    ),
			    RecursiveCall)
		   ], Mod).

goal_expansion(checknodes(Meta, Term), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(checknodes, 2, Proto, GoalName),
    append(MetaVars, [[Term]], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[]], Base),
    append_args(HeadPrefix, [[In|Ins]], RecursionHead),
    append_args(Pred, [In], Apply),
    append_args(HeadPrefix, [Args], SubRecursiveCall),
    append_args(HeadPrefix, [Ins], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            Apply,
			    (compound(In)
			    ->
				In =.. [_|Args],SubRecursiveCall
			    ;
			    true
			    ),
			    RecursiveCall)
		   ], Mod).

goal_expansion(sumnodes(Meta, Term, AccIn, AccOut), Mod:Goal) :-
    goal_expansion_allowed,
    callable(Meta),
    current_source_module(Mod,Mod),
    aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
    !,
    % the new goal
    pred_name(sumnodes, 4, Proto, GoalName),
    append(MetaVars, [[Term], AccIn, AccOut], GoalArgs),
    Goal =.. [GoalName|GoalArgs],
    % the new predicate declaration
    HeadPrefix =.. [GoalName|PredVars],
    append_args(HeadPrefix, [[], Acc, Acc], Base),
    append_args(HeadPrefix, [[In|Ins], Acc1, Acc2], RecursionHead),
    append_args(Pred, [In, Acc1, Acc3], Apply),
    append_args(HeadPrefix, [Args, Acc3, Acc4], SubRecursiveCall),
    append_args(HeadPrefix, [Ins, Acc4, Acc2], RecursiveCall),
    compile_aux([
		       Base,
		       (RecursionHead :-
		            Apply,
			    (compound(In)
			    ->
				In =.. [_|Args],SubRecursiveCall
			    ;
			    Acc3 = Acc4
			    ),
			    RecursiveCall)
		   ], Mod).

/**
@}
*/
