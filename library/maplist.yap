

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
	   foldl/4,
	   foldl/5,
	   foldl/6,
	   foldl/7,
	   foldl/8,
	   foldl2/6,
	   foldl2/7,
	   foldl2/8,
	   foldl3/8,
	   foldl3/9,
	   foldl3/10,
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
       selectlist(1,+,+),
       selectlist(2,+,+,+),
       selectlists(2,+,+,+,+),
       checklist(1,+),
       maplist(1,+),
       maplist(2,+,-),
       maplist(3,+,+,-),
       maplist(4,+,+,+,-),
       maplist(5,+,+,+,+,-),
       maplist(6,+,+,+,+,+,-),
       maplist(7,+,+,+,+,+,+,-),
       maplist(8,+,+,+,+,+,+,+,-),
       convlist(2,+,-),
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
       foldl2(7, +, ?, ?, +, -, +, -),
       foldl3(7, +, +, -, +, -, +, -),
       foldl3(8, +, +, +, -, +, -, +, -),
       foldl3(9, +, +, +, +, -, +, -, +, -),
       foldl4(10, +, +, -, +, -, +, -, +, -),
       foldl(4, +, +, +, -),
       foldl(5, +, +, +, +, -),
       foldl(6, +, +, +, +, +, -),
       foldl(7, +, +, +, +, +, +, -),
       scanl(3, +, +, -),
       scanl(4, +, +, +, -),
       scanl(5, +, +, +, +, -),
       scanl(6, +, +, +, +, +, -).


:- use_module( library(lists),[append/3]).

/*

:- use[_module(library(charsio), [format_to_chars/3, read_from_chars/2]).
:- use_module(library(occurs), [sub_term/2]).
*/
/**
  * @defgroup maplist Map List and Term Operations
  * @ingroup YAPLibrary
  * @{
  *
  * @brief
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
  * ```
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
  * ```
  * 
  **/

/**

  @pred    maplist(: Pred, ? ListIn)

  Applies predicate  _Pred_( _El_ ) to all
  elements _El_ of  _ListIn_.

Examples:
```
one(1).
zero(0).

g(N,Ones)  :- length(N,Ones), maplist(one,Ones).

zeros(L) :- maplist(zero,L).

?-  zeros([_,0,_,0]).

?-  zeros([_,0,_,0]).

?-  zeros([_,_,_|_]).
```

*/
maplist(_, []).
maplist(Pred, [In|ListIn]) :-
    call(Pred, In),
    maplist(Pred, ListIn).


/** @pred maplist( 2:Pred, + _List1_,+ _List2_)

Apply  _Pred_ on all successive pairs of elements from

 _List1_ and
 _List2_. Fails if  _Pred_ can not be applied to a
pair. See the next example.

```
eqall(L1,L2) :- maplist(=,L1,L2).

eqall([X,Y,Z], LF).
?- eqall([1,2,3],L).
?- eqall(

*/
maplist(_, [], []).
maplist(Pred, [In|ListIn], [Out|ListOut]) :-
    call(Pred, In, Out),
    maplist(Pred, ListIn, ListOut).


/** @pred maplist(3:Pred,? List1,? List2,? List3)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_ and  _List3_. Fails if  _Pred_ can not be applied to a
triple. See the example:

```
zip(X-Y,X,Y).

dozip(X,Y,F) :-
    maplist(zip, X, Y, F).


?- dozip([1,2],[3,4],XY).

?- dozip(X,Y,[1-3,2-4]).

?- dozip([1,X],[Y,Y],[1-3,2-3]).

```



 */
maplist(_, [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3]) :- 
    call(Pred, H1, H2, H3),
    maplist(Pred, List1, List2, List3).

/** @pred maplist(4:Pred,? List1,? List2,? List3, ? List4)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_,  _List3_ and _List4_. Fails if  _Pred_ can not be applied to a
tuple. See maplist/3.



 */
maplist(_, [], [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3], [H4|List4]) :- 
    call(Pred, H1, H2, H3, H4),
    maplist(Pred, List1, List2, List3, List4).

/** @pred maplist(5:Pred,? List1,? List2,? List3, ? List4, ? List5)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_,  _List3_, _List4_ and _List5_. Fails if  _Pred_ can not be applied to a
tuple. See maplist/3.



 */
maplist(_, [], [], [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3], [H4|List4], [H5|List5]) :- 
    call(Pred, H1, H2, H3, H4, H5),
    maplist(Pred, List1, List2, List3, List4, List5).

/** @pred maplist(6:Pred,? List1,? List2,? List3, ? List4, ? List5, ? List6)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_,  _List3_, _List4_, _List5_ and _List6_. Fails if  _Pred_ can not be applied to a
tuple. See maplist/3.



 */
maplist(_, [], [], [], [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3], [H4|List4], [H5|List5], [H6|List6]) :- 
    call(Pred, H1, H2, H3, H4, H5, H6),
    maplist(Pred, List1, List2, List3, List4, List5, List6).

/** @pred maplist(7:Pred,? List1,? List2,? List3, ? List4, ? List5, ? List6, ? List7)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_,  _List3_, _List4_, _List5_, _List6_ and _List7_. Fails if  _Pred_ can not be applied to a
tuple. See maplist/3.



 */
maplist(_, [], [], [], [], [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3], [H4|List4], [H5|List5], [H6|List6], [H7|List7]) :- 
    call(Pred, H1, H2, H3, H4, H5, H6, H7),
    maplist(Pred, List1, List2, List3, List4, List5, List6, List7).

/** @pred maplist(8:Pred,? List1,? List2,? List3, ? List4, ? List5, ? List6, ? List7, ? List8)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_,  _List3_, _List4_, _List5_, _List6_, _List7_ and _List8_. Fails if  _Pred_ can not be applied to a
tuple. See maplist/3.



 */
maplist(_, [], [], [], [], [], [], [], []).
maplist(Pred, [H1|List1],[H2|List2], [H3|List3], [H4|List4], [H5|List5], [H6|List6], [H7|List7], [H8|List8]) :- 
    call(Pred, H1, H2, H3, H4, H5, H6, H7, H8),
    maplist(Pred, List1, List2, List3, List4, List5, List6, List7, List8).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Definitions for Metacalls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
  @pred  include( 1:Pred, + ListIn, ? ListOut)

  Same as selectlist/3.
*/

include(_, [], []).		
include(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	 ListOut = [In|NewListOut]
    ;
    ListOut = NewListOut
    ),
    include(Pred, ListIn, NewListOut).



/**
  @pred  selectlist(1:Pred, + ListIn, ? ListOut))

  Creates  _ListOut_ of all list elements of  _ListIn_ that pass a given test
*/
selectlist(_, [], []).		%
selectlist(Pred, [In|ListIn], ListOut) :-
    (call(Pred, In) ->
	 ListOut = [In|NewListOut]
    ;
    ListOut = NewListOut
    ),
    selectlist(Pred, ListIn, NewListOut).

 
/** @pred  selectlist( 2:Pred, + ListIn, + ListInAux, ? ListOut)

  Creates  _ListOut_ of all list elements of  _ListIn_ that
  pass the given test  _Pred_ using + _ListInAux_ as the
  secib element.
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
  @pred  selectlists( 2:Pred, + ListIn, + ListInAux, ? ListOut, ? ListOutAux)

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
    selectlists(Pred, ListIn, ListIn1, NewListOut, NewListOut1).

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
v*/
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
    checknodes(Pred, Term),
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
foldl(_,[],  V, V).
foldl(Goal,[H|T], V0, V) :-
    call(Goal, H, V0, V1),
    foldl(Goal, T,  V1, V).

/**
  @pred foldl(: _Pred_, + _List1_, + _List2_, ? _AccIn_, ? _AccOut_)

  Calls  _Pred_ on all elements of _List1_ and
  _List2_ and collects a result in  _Accumulator_. Same as
  foldr/4.

*/
foldl(_, [], [], V, V).
foldl(Goal, [H1|T1], [H2|T2], V0, V) :-
    call(Goal, H1, H2, V0, V1),
    foldl(Goal, T1, T2, V1, V).

/**

@pred foldl(Goal, List1, List2, List3, V0, V)

Apply _Goal_ plus five arguuments, three map to lists,
two can be used as a difference_type.

*/
foldl(_, [], [], [], V, V).
foldl(Goal,[H1|T1], [H2|T2], [H3|T3],  V0, V) :-
    call(Goal, H1, H2, H3, V0, V1),
    foldl(Goal,T1, T2, T3, V1, V).


/**

*/
foldl(_, [], [], [], [], V, V).
foldl(Goal, [H1|T1], [H2|T2], [H3|T3], [H4|T4], V0, V) :-
    call(Goal, H1, H2, H3, H4, V0, V1),
    foldl(Goal, T1, T2, T3, T4, V1, V).


/**
   @pred foldl2(: _Pred_, + _List_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of `List` and collects a result in
  _X_ and  _Y_.

*/
foldl2(_, [], V, V, W, W).
foldl2(Goal, [H|T], V0, V, W0, W) :-
    call(Goal, H, V0, V1, W0, W1),
    foldl2(Goal, T, V1, V, W1, W).

/**
   @pred foldl2(: _Pred_, + _List_, ? _List1_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of  _List_  and  _List1_  and collects a result in
  _X_ and  _Y_.
*/
foldl2( _Goal, [], [], V, V, W, W).
foldl2(Goal, [H1|T1], [H2|T2],  V0, V, W0, W) :-
    call(Goal, H1, H2, V0, V1, W0, W1),
    foldl2( Goal, T1, T2, V1, V, W1, W).

/**
   @pred foldl2(: _Pred_, + _List_, ? _List1_, ? _List2_, ? _X0_, ? _X_, ? _Y0_, ? _Y_)

  Calls  _Pred_ on all elements of  _List_,  _List1_  and  _List2_  and collects a result in
  _X_ and  _Y_.

*/
foldl2(_, [], [], [], V, V, W, W).
foldl2(Goal, [H1|T1], [H2|T2], [H3|T3], V0, V, W0, W) :-
    call(Goal, H1, H2, H3, V0, V1, W0, W1),
    foldl2(Goal, T1, T2, T3, V1, V, W1, W).


/**
   @pred foldl3(: _Pred_, + _List_, ? _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_ and  _Z_.
*/
foldl3(_, [], V, V, W, W, X, X).
foldl3(Goal, [H|T], V0, V, W0, W, X0, X) :-
    call(Goal, H, V0, V1, W0, W1, X0, X1),
    foldl3(Goal, T, V1, V, W1, W, X1, X).

/**
   @pred foldl3(: _Pred_, + _List0_, ? _List1_, _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_ and  _Z_.
*/
foldl3(_, [], [], V, V, W, W, X, X).
foldl3(Goal, [H|T], [H1|T1], V0, V, W0, W, X0, X) :-
    call(Goal, H, H1, V0, V1, W0, W1, X0, X1),
    foldl3(Goal, T, T1, V1, V, W1, W, X1, X).

/**
   @pred foldl3(: _Pred_, + _List1_, _List2_?_, _List3_? _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_ and  _Z_.
*/
foldl3(_, [], [], [], V, V, W, W, X, X).
foldl3(Goal, [H|T], [H1|T1], [H2|T2], V0, V, W0, W, X0, X) :-
    call(Goal, H, H1, H2, V0, V1, W0, W1, X0, X1),
    foldl3(Goal, T, T1, T2, V1, V, W1, W, X1, X).

/**
  @pred foldl4(: _Pred_, + _List1_, ? _X0_, ? _X_, ? _Y0_, ? _Y_, ? _Z0_, ? _Z_, ? _W0_, ? _W_)


  Calls  _Pred_ on all elements of `List` and collects a
  result in  _X_,  _Y_,  _Z_ and  _W_.
*/
foldl4(_, [], V, V, W, W, X, X, Y, Y).
foldl4(Goal, [H|T], V0, V, W0, W, X0, X, Y0, Y) :-
    call(Goal, H, V0, V1, W0, W1, X0, X1, Y0, Y1),
    foldl4(Goal, T,  V1, V, W1, W, X1, X, Y1, Y).

*-
+
+36214/

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
%
% ```
% scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, [V0,V1,...,Vn]) :-
% 		P(X11, ..., Xmn, V0, V1),
% 		...
% 	        P(X1n, ..., Xmn, V', Vn).
% ```
%
% Left scan of  list.  The  scanl   family  of  higher  order list
% operations is defined by:
%
% ```
% scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, [V0,V1,...,Vn]) :-
%       P(X11, ..., Xm1, V0, V1),
%       ...
%        P(X1n, ..., Xmn, Vn-1, Vn).
% ```
% 

scanl(Goal,[H|T], V, [VH|VT]) :-
    call(Goal, H, V, VH),
    scanl(T, Goal, VH, VT).

/**
  scanl(: _Pred_, + _List1_, + _List2_, ? _V0_, ? _Vs_)

Left scan of  list.
 */
scanl(_, [], [], Out, Out).
scanl(Goal, [H1|T1], [H2|T2], V, [VH|VT]) :-
    call(Goal, H1, H2, V, VH),
    scanl(Goal, T1, T2, VH, VT).

/**
 scanl(: _Pred_, + _List1_, + _List2_, + _List3_, ? _V0_, ? _Vs_)

Left scan of  list.
*/
scanl(_, [], [], [], V, V).
scanl(Goal, [H1|T1], [H2|T2], [H3|T3], V, [VH|VT]) :-
    call(Goal, H1, H2, H3, V, VH),
    scanl(Goal, T1, T2, T3, VH, VT).

/**
  scanl(: _Pred_, + _List1_, + _List2_, + _List3_, + _List4_, ? _V0_, ? _Vs_)

  Left scan of  list.
*/

scanl(_Goal, [], [], [], [], T, T).
scanl(Goal, [H1|T1], [H2|T2], [H3|T3], [H4|T4], V, [VH|VT]) :-
    call(Goal, H1, H2, H3, H4, V, VH),
    scanl(Goal, T1, T2, T3, T4 , VH, VT).

new_name(Name,N) :-
    retract(ids(I)),
    !,
    I1 is I+1,
    format(atom(N),'~s ~d',[Name,I]),
    assert(ids(I1)).
new_name(Name,N) :-
    format(atom(N),'~s ~d',[Name,0]),
    assert(ids(1)).

expand_rule1(maplist).
expand_rule1(checklist).
expand_rule1(convlist).
expand_rule1(exclude).
expand_rule1(foldl).
expand_rule1(foldl2).
expand_rule1(foldl3).
expand_rule1(foldl4).
expand_rule1(include).
expand_rule1(partition).
expand_rule1(selectlist).
expand_rule1(selectlists).
expand_rule1(sumlist).

/**
  @pred goal_expansion(Call, Unfolded)

  Active if argument to call is bound, Then we have

  Original call = meta( call(CallerArgs) CallerLink)
  results in a call to newname:
      newCall = newname(CallerLink CallerArgs)

  defined by performing the following transformations to the original meta-predicate:
      meta( BaseCase ) -> newname(BaseCase CallerArgs)
  and in the recursive definition:
      call(g(Args),LinkArgs) -> g(Args LinkArgs)
      meta(g(Args) LinkArgs) -> newname(LinkArgs Args)
*/

goal_expansion(Call0, M0:NG) :-
 %   current_prolog_flag( compiling, true ),
 %   current_prolog_flag( goal_expansion_allowed, true ),
    strip_module(Call0,M0,Call),
    Call =.. [Meta,M0Goal|LinkArgs],
    expand_rule1(Meta),
    strip_module(M0Goal,M1,Target),
    nonvar(M1),
    nonvar(Target),
    Target=..[G|Args],
    new_name(G,NewName),
    newvs(LinkArgs, LinkVs),
    HCall =.. [Meta,M0Goal|LinkVs],
      (
	findall(NewClause,
 (
     clause(maplist:HCall,Body),
      process(Body, Meta, HCall,Args,M1:G,NewName,NewClause)
	),
	NewClauses),
      compile_clauses(NewClauses),
      fail
    ;
  append(LinkArgs, Args, CallerArgs),
  NG =.. [NewName|CallerArgs]
    ).


process(true, _, HCall,Args,_MG,NewName,(NH :- true)) :-
    !,
    newvs(Args, Empties),
    HCall=..[_,_|Finals],
    append(Finals,Empties,AllArgs),
    NH=..[NewName|AllArgs].
process(Body, Meta, HCall,Args,MG,NewName,(NH:-NewBody)) :-
    HCall=..[_,_|Finals],
    append(Finals,Args,AllArgs),
    NH=..[NewName|AllArgs],
    new_body(Body, Args,Meta, MG, NewName, NewBody).

new_body((C1,C2),Args,Meta,MG,NewName,(NC1,NC2)) :-
    !,
    new_body(C1,Args,Meta,MG,NewName,NC1),
    new_body(C2,Args,Meta,MG,NewName,NC2).
new_body((C1|C2),Args,Meta,MG,NewName,(NC1|NC2)) :-
        !,
    new_body(C1,Args,Meta,MG,NewName,NC1),
    new_body(C2,Args,Meta,MG,NewName,NC2).
new_body((C1;C2),Args,Meta,MG,NewName,(NC1;NC2)) :-
    !,
new_body(C1,Args,Meta,MG,NewName,NC1),
    new_body(C2,Args,Meta,MG,NewName,NC2).
new_body((C1->C2),Args,Meta,MG,NewName,(NC1->NC2)) :-
    !,
    new_body(C1,Args,Meta,MG,NewName,NC1),
    new_body(C2,Args,Meta,MG,NewName,NC2).
new_body(C1,Args,Meta,_MG,NewName,New) :-  
    strip_module(C1,_M,C),
    C =.. [Meta,_|LinkArgs],
    !,
    append(LinkArgs,Args,BaseArgs),
    New =.. [NewName|BaseArgs].
new_body(C1,Args,_Meta,M:G,_NewName,M:New) :- 
    strip_module(C1,_M,C),
    C =.. [call,_|LinkArgs],
    !,
    append(Args,LinkArgs,BaseArgs),
    New =.. [G|BaseArgs].
new_body(C1,_Args ,_Meta,_MG,_NewName,C1).

newvs([],[]).
newvs([_|L],[_|NL]) :-
    newvs(L,NL).

