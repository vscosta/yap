/**
 * @file   libray/lists.yap
 * @author Bob Welham, Lawrence Byrd, and R. A. O'Keefe. Contributions from Vitor Santos Costa, Jan Wielemaker and others.
 * @date   1999
*/

% This file has been included as an YAP library by Vitor Santos Costa, 1999

:- module(lists,
	  [
	   append/3,
	   append/2,
	   delete/3,
	   intersection/3,
	   flatten/2,
	   last/2,
	   list_concat/2,
	   max_list/2,
	   list_to_set/2,
	   member/2,
	   memberchk/2,
	   min_list/2,
	   nextto/3,
	   nth/3,
	   nth/4,
	   nth0/3,
	   nth0/4,
	   nth1/3,
	   nth1/4,
	   numlist/3,
	   permutation/2,
	   prefix/2,
	   remove_duplicates/2,
	   reverse/2,
	   same_length/2,
	   select/3,
	   selectchk/3,
	   sublist/2,
	   substitute/4,
	   subtract/3,
	   suffix/2,
	   sum_list/2,
	   sum_list/3,
	   sumlist/2,
	   randomize/2
	  ]).


/**  
 * @{
 *
 * @addtogroup lists List Predicates in the Prolog Library
 * @ingroup library  
 *
 * @brief  List Manipulation Predicates
 *
 * The following list manipulation routines are available once included
    with the `use_module(library(lists))` command.
*/

%:- include(pl/bootlists).

/** @pred list_concat(+ _Lists_,? _List_)


True when  _Lists_ is a list of lists and  _List_ is the
concatenation of  _Lists_.


*/
/** @pred max_list(? _Numbers_, ? _Max_)


True when  _Numbers_ is a list of numbers, and  _Max_ is the maximum.


*/
/** @pred min_list(? _Numbers_, ? _Min_)


True when  _Numbers_ is a list of numbers, and  _Min_ is the minimum.


*/
/** @pred nth(? _N_, ? _List_, ? _Elem_)


The same as nth1/3.


*/
/** @pred nth(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Same as `nth1/4`.


*/
/** @pred nth0(? _N_, ? _List_, ? _Elem_)


True when  _Elem_ is the Nth member of  _List_,
counting the first as element 0.  (That is, throw away the first
N elements and unify  _Elem_ with the next.)  It can only be used to
select a particular element given the list and index.  For that
task it is more efficient than member/2


*/
/** @pred nth0(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_,
counting from 0, and  _Rest_ with the other elements.  It can be used
to select the Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of  _Rest_, when
it yields  _List_, e.g. `nth0(2, List, c, [a,b,d,e])` unifies List with
`[a,b,c,d,e]`.  `nth/4` is the same except that it counts from 1.  `nth0/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.


*/
/** @pred nth1(+ _Index_,? _List_,? _Elem_)


Succeeds when the  _Index_-th element of  _List_ unifies with
 _Elem_. Counting starts at 1.

Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence expand_file_name/2.


*/
/** @pred nth1(? _N_, ? _List_, ? _Elem_)


The same as nth0/3, except that it counts from
1, that is `nth(1, [H|_], H)`.


*/
/** @pred nth1(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_, counting from 1,
and  _Rest_ with the other elements.  It can be used to select the
Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of
 _Rest_, when it yields  _List_, e.g. `nth(3, List, c, [a,b,d,e])` unifies List with `[a,b,c,d,e]`.  `nth/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.


*/
/** @pred numlist(+ _Low_, + _High_, + _List_)


If  _Low_ and  _High_ are integers with  _Low_ =<
 _High_, unify  _List_ to a list `[Low, Low+1, ...High]`. See
also between/3.


*/
/** @pred permutation(+ _List_,? _Perm_)


True when  _List_ and  _Perm_ are permutations of each other.


*/
/** @pred remove_duplicates(+ _List_, ? _Pruned_)


Removes duplicated elements from  _List_.  Beware: if the  _List_ has
non-ground elements, the result may surprise you.


*/
/** @pred same_length(? _List1_, ? _List2_)


True when  _List1_ and  _List2_ are both lists and have the same number
of elements.  No relation between the values of their elements is
implied.
Modes `same_length(-,+)` and `same_length(+,-)` generate either list given
the other; mode `same_length(-,-)` generates two lists of the same length,
in which case the arguments will be bound to lists of length 0, 1, 2, ...

 */


%%   @pred append(? _Lists_,? _Combined_)
%
%	Concatenate a list of lists.  Is  true   if  Lists  is a list of
%	lists, and List is the concatenation of these lists.
%
%	@param	ListOfLists must be a list of -possibly- partial lists

append(ListOfLists, List) :-
%	must_be_list( ListOfLists),
	append_(ListOfLists, List).

append_([], []).
append_([L], L).
append_([L1,L2|Ls], L) :-
	append(L1,L2,LI),
	append_([LI|Ls],L).

%   reverse(List, Reversed)
%   is true when List and Reversed are lists with the same elements
%   but in opposite orders.  rev/2 is a synonym for reverse/2.

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], Sofar, Reversed) :-
	reverse(Tail, [Head|Sofar], Reversed).

/** @pred last(+ _List_,? _Last_)


True when  _List_ is a list and  _Last_ is identical to its last element.
d(_, [X], L).
*/

last([H|List], Last) :-
	last(List, H, Last).

last([], Last, Last).
last([H|List], _, Last) :-
	last(List, H, Last).

%   nextto(X, Y, List)
%   is true when X and Y appear side-by-side in List.  It could be written as
%	nextto(X, Y, List) :- append(_, [X,Y,_], List).
%   It may be used to enumerate successive pairs from the list.

nextto(X,Y, [X,Y|_]).
nextto(X,Y, [_|List]) :-
	nextto(X,Y, List).

%   nth0(?N, +List, ?Elem) is true when Elem is the Nth member of List,
%   counting the first as element 0.  (That is, throw away the first
%   N elements and unify Elem with the next.)  It can only be used to
%   select a particular element given the list and index.  For that
%   task it is more efficient than nmember.
%   nth(+N, +List, ?Elem) is the same as nth0, except that it counts from
%   1, that is nth(1, [H|_], H).

nth0(V, In, Element) :- var(V), !,
	generate_nth(0, V, In, Element).
nth0(0, [Head|_], Head) :- !.
nth0(N, [_|Tail], Elem) :-
	M is N-1,
	find_nth0(M, Tail, Elem).

find_nth0(0, [Head|_], Head) :- !.
find_nth0(N, [_|Tail], Elem) :-
	M is N-1,
	find_nth0(M, Tail, Elem).


nth1(V, In, Element) :- var(V), !,
	generate_nth(1, V, In, Element).
nth1(1, [Head|_], Head) :- !.
nth1(N, [_|Tail], Elem) :-
	nonvar(N), !,
	M is N-1,			% should be succ(M, N)
	find_nth(M, Tail, Elem).

nth(V, In, Element) :- var(V), !,
	generate_nth(1, V, In, Element).
nth(1, [Head|_], Head) :- !.
nth(N, [_|Tail], Elem) :-
	nonvar(N), !,
	M is N-1,			% should be succ(M, N)
	find_nth(M, Tail, Elem).

find_nth(1, [Head|_], Head) :- !.
find_nth(N, [_|Tail], Elem) :-
	M is N-1,
	find_nth(M, Tail, Elem).


generate_nth(I, I, [Head|_], Head).
generate_nth(I, IN, [_|List], El) :-
	I1 is I+1,
	generate_nth(I1, IN, List, El).



%   nth0(+N, ?List, ?Elem, ?Rest) unifies Elem with the Nth element of List,
%   counting from 0, and Rest with the other elements.  It can be used
%   to select the Nth element of List (yielding Elem and Rest), or to
%   insert Elem before the Nth (counting from 1) element of Rest, when
%   it yields List, e.g. nth0(2, List, c, [a,b,d,e]) unifies List with
%   [a,b,c,d,e].  nth is the same except that it counts from 1.  nth
%   can be used to insert Elem after the Nth element of Rest.

nth0(V, In, Element, Tail) :- var(V), !,
	generate_nth(0, V, In, Element, Tail).
nth0(0, [Head|Tail], Head, Tail) :- !.
nth0(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	nth0(M, Tail, Elem, Rest).

find_nth0(0, [Head|Tail], Head, Tail) :- !.
find_nth0(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	find_nth0(M, Tail, Elem, Rest).



nth1(V, In, Element, Tail) :- var(V), !,
	generate_nth(1, V, In, Element, Tail).
nth1(1, [Head|Tail], Head, Tail) :- !.
nth1(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	nth1(M, Tail, Elem, Rest).

nth(V, In, Element, Tail) :- var(V), !,
	generate_nth(1, V, In, Element, Tail).
nth(1, [Head|Tail], Head, Tail) :- !.
nth(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	nth(M, Tail, Elem, Rest).

find_nth(1, [Head|Tail], Head, Tail) :- !.
find_nth(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	find_nth(M, Tail, Elem, Rest).


generate_nth(I, I, [Head|Tail], Head, Tail).
generate_nth(I, IN, [E|List], El, [E|Tail]) :-
	I1 is I+1,
	generate_nth(I1, IN, List, El, Tail).



%   permutation(List, Perm)
%   is true when List and Perm are permutations of each other.  Of course,
%   if you just want to test that, the best way is to keysort/2 the two
%   lists and see if the results are the same.  Or you could use list_to_bag
%   (from BagUtl.Pl) to see if they convert to the same bag.  The point of
%   perm is to generate permutations.  The arguments may be either way round,
%   the only effect will be the order in which the permutations are tried.
%   Be careful: this is quite efficient, but the number of permutations of an
%   N-element list is N!, even for a 7-element list that is 5040.

permutation([], []).
permutation(List, [First|Perm]) :-
	select(First, List, Rest),	%  tries each List element in turn
	permutation(Rest, Perm).


% prefix(Part, Whole) iff Part is a leading substring of Whole

prefix([], _).
prefix([Elem | Rest_of_part], [Elem | Rest_of_whole]) :-
  prefix(Rest_of_part, Rest_of_whole).

%%   remove_duplicates(+List, Pruned)
%   removes duplicated elements from List.  Beware: if the List has
%   non-ground elements, the result may surprise you.

remove_duplicates([], []).
remove_duplicates([Elem|L], [Elem|NL]) :-
	delete(L, Elem, Temp),
	remove_duplicates(Temp, NL).

%%   remove_identical_duplicates(List, Pruned)
%   removes duplicated elements from List.  
remove_identical_duplicates([], []).
remove_identical_duplicates([Elem|L], [Elem|NL]) :-
	delete_identical(L, Elem, Temp),
	remove_identical_duplicates(Temp, NL).


delete_identical([],_, []).
delete_identical([H|L],Elem,Temp)  :-
    H == Elem,
    !,
	delete_identical(L, Elem, Temp).
delete_identical([H|L], Elem, [H|Temp]) :-
	delete_identical(L, Elem, Temp).



%   same_length(?List1, ?List2)
%   is true when List1 and List2 are both lists and have the same number
%   of elements.  No relation between the values of their elements is
%   implied.
%   Modes same_length(-,+) and same_length(+,-) generate either list given
%   the other; mode same_length(-,-) generates two lists of the same length,
%   in which case the arguments will be bound to lists of length 0, 1, 2, ...

same_length([], []).
same_length([_|List1], [_|List2]) :-
	same_length(List1, List2).


/** @pred selectchk(? _Element_, ? _List_, ? _Residue_)


Semi-deterministic selection from a list. Steadfast: defines as

~~~~~
selectchk(Elem, List, Residue) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.
~~~~~
*/
selectchk(Elem, List, Rest) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.



/** @pred select(? _Element_, ? _List_, ? _Residue_)


True when  _Set_ is a list,  _Element_ occurs in  _List_, and
 _Residue_ is everything in  _List_ except  _Element_ (things
stay in the same order).
*/
select(Element, [Element|Rest], Rest).
select(Element, [Head|Tail], [Head|Rest]) :-
	select(Element, Tail, Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	sublist(?Sub, +List) is nondet.
%
%	True if all elements of Sub appear in List in the same order.
%
%   ALlo, both `append(_,Sublist,S)` and `append(S,_,List)` hold.
sublist(L, L).
sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).

'$sublist1'(Sub, _, Sub).
'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).
'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).

%   substitute(X, XList, Y, YList)
%   is true when XList and YList only differ in that the elements X in XList
%   are replaced by elements Y in the YList.
substitute(X, XList, Y, YList) :-
	substitute2(XList, X, Y, YList).

substitute2([], _, _, []).
substitute2([X0|XList], X, Y, [Y|YList]) :-
	X == X0, !,
	substitute2(XList, X, Y, YList).
substitute2([X0|XList], X, Y, [X0|YList]) :-
	substitute2(XList, X, Y, YList).

/** @pred suffix(? _Suffix_, ? _List_)

Holds when `append(_,Suffix,List)` holds.
*/
suffix(Suffix, Suffix).
suffix(Suffix, [_|List]) :-
	suffix(Suffix,List).

/** @pred sumlist(? _Numbers_, ? _Total_)


True when  _Numbers_ is a list of integers, and  _Total_ is their
sum. The same as sum_list/2, please do use sum_list/2
instead.


*/
sumlist(Numbers, Total) :-
	sumlist(Numbers, 0, Total).

/** @pred sum_list(? _Numbers_, + _SoFar_, ? _Total_)

True when  _Numbers_ is a list of numbers, and  _Total_ is the sum of their total plus  _SoFar_.
*/
sum_list(Numbers, SoFar, Total) :-
	sumlist(Numbers, SoFar, Total).

/** @pred sum_list(? _Numbers_, ? _Total_)


True when  _Numbers_ is a list of numbers, and  _Total_ is their sum.
*/
sum_list(Numbers, Total) :-
	sumlist(Numbers, 0, Total).

sumlist([], Total, Total).
sumlist([Head|Tail], Sofar, Total) :-
	Next is Sofar+Head,
	sumlist(Tail, Next, Total).


%   list_concat(Lists, List)
%   is true when Lists is a list of lists, and List is the
%   concatenation of these lists.

list_concat([], []).
list_concat([H|T], L) :-
	list_concat(H, L, Li),
	list_concat(T, Li).

list_concat([], L, L).
list_concat([H|T], [H|Lf], Li) :-
	list_concat(T, Lf, Li).



/** @pred flatten(+ _List_, ? _FlattenedList_)


Flatten a list of lists  _List_ into a single list
 _FlattenedList_.

~~~~~
?- flatten([[1],[2,3],[4,[5,6],7,8]],L).

L = [1,2,3,4,5,6,7,8] ? ;

no
~~~~~
*/
flatten(X,Y) :- flatten_list(X,Y,[]).

flatten_list(V) --> {var(V)}, !, [V].
flatten_list([]) --> !.
flatten_list([H|T]) --> !, flatten_list(H),flatten_list(T).
flatten_list(H) --> [H].

max_list([H|L],Max) :-
	max_list(L,H,Max).

max_list([],Max,Max).
max_list([H|L],Max0,Max) :-
	(
	  H > Max0
	->
	  max_list(L,H,Max)
	;
	  max_list(L,Max0,Max)
	).

min_list([H|L],Max) :-
	min_list(L,H,Max).

min_list([],Max,Max).
min_list([H|L],Max0,Max) :-
	(
	  H < Max0
	->
	  min_list(L, H, Max)
	;
	  min_list(L, Max0, Max)
	).

%%      numlist(+Low, +High, -List) is semidet.
%
%       List is a list [Low, Low+1, ... High].  Fails if High < Low.%
%
%       @error type_error(integer, Low)
%       @error type_error(integer, High)

numlist(L, U, Ns) :-
        must_be(integer, L),
        must_be(integer, U),
        L =< U,
        numlist_(L, U, Ns).

numlist_(U, U, OUT) :- !, OUT = [U].
numlist_(L, U, [L|Ns]) :-
        succ(L, L2),
        numlist_(L2, U, Ns).


/** @pred intersection(+ _Set1_, + _Set2_, + _Set3_)


Succeeds if  _Set3_ unifies with the intersection of  _Set1_ and
 _Set2_.  _Set1_ and  _Set2_ are lists without duplicates. They
need not be ordered.

The code was copied from SWI-Prolog's list library.

*/

% copied from SWI lists library.
intersection([], _, []) :- !.
intersection([X|T], L, Intersect) :-
	memberchk(X, L), !,
	Intersect = [X|R],
	intersection(T, L, R).
intersection([_|T], L, R) :-
	intersection(T, L, R).

%%	subtract(+Set, +Delete, -Result) is det.
%
%	Delete all elements from `Set' that   occur  in `Delete' (a set)
%	and unify the  result  with  `Result'.   Deletion  is  based  on
%	unification using memberchk/2. The complexity is |Delete|*|Set|.
%
%	@see ord_subtract/3.

subtract([], _, []) :- !.
subtract([E|T], D, R) :-
	memberchk(E, D), !,
	subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
	subtract(T, D, R).

%%	list_to_set(+List, ?Set) is det.
%
%	True when Set has the same element   as  List in the same order.
%	The left-most copy of the duplicate  is retained. The complexity
%	of this operation is |List|^2.
%
%	@see sort/2.

list_to_set(List, Set) :-
	list_to_set_(List, Set0),
	Set = Set0.

list_to_set_([], R) :-
	close_list(R).
list_to_set_([H|T], R) :-
	memberchk(H, R), !,
	list_to_set_(T, R).

close_list([]) :- !.
close_list([_|T]) :-
	close_list(T).

/** randomize( +List, -RandomList).

Create a "raandom" peermutation of a list. The initial list may have repeated
elements,

%% @}
/** @} */
