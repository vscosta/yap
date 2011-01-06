% This file has been included as an YAP library by Vitor Santos Costa, 1999

%
% This file includes code from Bob Welham, Lawrence Byrd, and R. A. O'Keefe.
%
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
	   sumlist/2
	  ]).

:- use_module(library(error),
		 [must_be/2]).


%%	append(+ListOfLists, ?List)
%
%	Concatenate a list of lists.  Is  true   if  Lists  is a list of
%	lists, and List is the concatenation of these lists.
%	
%	@param	ListOfLists must be a list of -possibly- partial lists

append(ListOfLists, List) :-
%	must_be(list, ListOfLists),
	append_(ListOfLists, List).

append_([], []).
append_([L|Ls], As) :-
	append(L, Ws, As),
	append_(Ls, Ws).


%   delete(List, Elem, Residue)
%   is true when List is a list, in which Elem may or may not occur, and
%   Residue is a copy of List with all elements identical to Elem deleted.

delete([], _, []).
delete([Head|List], Elem, Residue) :-
	Head == Elem, !,
	delete(List, Elem, Residue).
delete([Head|List], Elem, [Head|Residue]) :-
	delete(List, Elem, Residue).


%   last(List, Last)
%   is true when List is a List and Last is identical to its last element.
%   This could be defined as last(L, X) :- append(_, [X], L).

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

%   remove_duplicates(List, Pruned)
%   removes duplicated elements from List.  Beware: if the List has
%   non-ground elements, the result may surprise you.

remove_duplicates([], []).
remove_duplicates([Elem|L], [Elem|NL]) :-
	delete(L, Elem, Temp),
	remove_duplicates(Temp, NL).

%   reverse(List, Reversed)
%   is true when List and Reversed are lists with the same elements
%   but in opposite orders.  rev/2 is a synonym for reverse/2.

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], Sofar, Reversed) :-
	reverse(Tail, [Head|Sofar], Reversed).


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

%%      selectchk(+Elem, +List, -Rest) is semidet.
%
%       Semi-deterministic removal of first element in List that unifies
%       Elem.

selectchk(Elem, List, Rest) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.


%   select(?Element, ?Set, ?Residue)
%   is true when Set is a list, Element occurs in Set, and Residue is
%   everything in Set except Element (things stay in the same order).

select(Element, [Element|Rest], Rest).
select(Element, [Head|Tail], [Head|Rest]) :-
	select(Element, Tail, Rest).


%   sublist(Sublist, List)
%   is true when both append(_,Sublist,S) and append(S,_,List) hold.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	sublist(?Sub, +List) is nondet.
%
%	True if all elements of Sub appear in List in the same order.

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

%   suffix(Suffix, List)
%   holds when append(_,Suffix,List) holds. 
suffix(Suffix, Suffix).
suffix(Suffix, [_|List]) :-
	suffix(Suffix,List).

%   sumlist(Numbers, Total)
%   is true when Numbers is a list of integers, and Total is their sum.

sumlist(Numbers, Total) :-
	sumlist(Numbers, 0, Total).

sum_list(Numbers, SoFar, Total) :-
	sumlist(Numbers, SoFar, Total).

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



%
% flatten a list
%
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
