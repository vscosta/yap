/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		setof.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	set predicates						 *
*									 *
*************************************************************************/

%   The "existential quantifier" symbol is only significant to bagof
%   and setof, which it stops binding the quantified variable.
%   op(200, xfy, ^) is defined during bootstrap.

% this is used by the all predicate

:- op(50,xfx,same).

_^Goal :-
	'$execute'(Goal).


%   findall/3 is a simplified version of bagof which has an implicit
%   existential quantifier on every variable.


findall(Template, Generator, Answers) :-
	'$check_list_for_bags'(Answers, findall(Template, Generator, Answers)),
	'$init_db_queue'(Ref),
	'$findall'(Template, Generator, Ref, [], Answers).


% If some answers have already been found
findall(Template, Generator, Answers, SoFar) :-
	'$init_db_queue'(Ref),
	'$findall'(Template, Generator, Ref, SoFar, Answers).

% starts by calling the generator,
% and recording the answers
'$findall'(Template, Generator, Ref, _, _) :-
	'$catch'(Error,'$clean_findall'(Ref,Error),_),
	'$execute'(Generator),
	'$db_enqueue'(Ref, Template),
	fail.
% now wraps it all
'$findall'(_, _, Ref, SoFar, Answers) :-
	'$collect_for_findall'(Ref, SoFar, Answers).

% error handling: be careful to recover all the space we used up
% in implementing findall.
%
'$clean_findall'(Ref,Ball) :-
	'$db_dequeue'(Ref,_), !,
	'$clean_findall'(Ref,Ball).
'$clean_findall'(_,Ball) :-
	% get this off the unwound computation.
	copy_term(Ball,NewBall),
	% get current jump point	
	'$jump_env_and_store_ball'(NewBall).


% by getting all answers
'$collect_for_findall'(Ref, SoFar, Out) :-
	( '$db_dequeue'(Ref, Term) ->
            Out = [Term|Answers],
	    '$collect_for_findall'(Ref, SoFar, Answers)
        ;
	    Out = SoFar
	).

% findall_with_key is very similar to findall, but uses the SICStus
% algorithm to guarantee that variables will have the same names.
%
'$findall_with_common_vars'(Template, Generator, Ref, _) :-
	'$execute'(Generator),
	'$db_enqueue'(Ref, Template),
	fail.
% now wraps it all
'$findall_with_common_vars'(_, _, Ref, Answers) :-
	'$collect_with_common_vars'(Ref, _, [], Answers).

% by getting all answers
'$collect_with_common_vars'(Ref, VarList, SoFar, Solution) :-
	'$db_dequeue'(Ref, BDEntry), !,
	BDEntry =  Key-_,
	Solution = [BDEntry|Answers],
	'$variables_in_term'(Key, _, VarList),
	'$collect_with_common_vars'(Ref, VarList, SoFar, Answers).
'$collect_with_common_vars'(_, _, Solution, Solution).

% This is the setof predicate

setof(Template, Generator, Set) :-
	'$check_list_for_bags'(Set, setof(Template, Generator, Set)),
	'$bagof'(Template, Generator, Bag),
	'$sort'(Bag, Set).

% And this is bagof

% Either we have excess of variables
% and we need to find the solutions for each instantiation
% of these variables

bagof(Template, Generator, Bag) :-
	'$bagof'(Template, Generator, Bag).
	
'$bagof'(Template, Generator, Bag) :-
	'$check_list_for_bags'(Bag, bagof(Template, Generator, Bag)),
	'$variables_in_term'(Template, [], TemplateV),
	'$excess_vars'(Generator, TemplateV, [], FreeVars),
	FreeVars \== [],
	!,
	'$variables_in_term'(FreeVars, [], LFreeVars),
	Key =.. ['$'|LFreeVars],
	'$init_db_queue'(Ref),
	'$findall_with_common_vars'(Key-Template, Generator, Ref, Bags0),
	'$keysort'(Bags0, Bags),
	'$pick'(Bags, Key, Bag).
% or we just have a list of answers
'$bagof'(Template, Generator, Bag) :-
	'$init_db_queue'(Ref),
	'$findall'(Template, Generator, Ref, [], Bag0),
	Bag0 \== [],
	Bag = Bag0.


% picks a solution attending to the free variables
'$pick'([K-X|Bags], Key, Bag) :-
	'$parade'(Bags, K, Bag1, Bags1),
	'$decide'(Bags1, [X|Bag1], K, Key, Bag).

'$parade'([K-X|L1], Key, [X|B], L) :- K == Key, !,
	'$parade'(L1, Key, B, L).
'$parade'(L, _, [], L).

%
% The first argument to decide gives if solutions still left;
% The second gives the solution currently found;
% The third gives the free variables that are supposed to be bound;
% The fourth gives the free variables being currently used.
% The fifth  outputs the current solution.
%
'$decide'([], Bag, Key, Key, Bag) :- !.
'$decide'(_, Bag, Key, Key, Bag).
'$decide'(Bags, _, _, Key, Bag) :-
	'$pick'(Bags, Key, Bag).

%
% Detect free variables in the source term
%
'$excess_vars'(V, X, L0, L) :-
	var(V),
	!,
	(   '$doesnt_include'(X, V) -> L = [V|L0]
	;   L = L0
	).
'$excess_vars'(A, _, L, L) :-
	atomic(A),  !.
'$excess_vars'(X^P, Y, L0, L) :- !,
	'$variables_in_term'(X+Y, [], NY),
	'$excess_vars'(P, NY, L0, L).
'$excess_vars'(setof(X,P,S), Y, L0, L) :- !,
	'$variables_in_term'(X+Y, [], NY),
	'$excess_vars'((P,S), NY, L0, L).
'$excess_vars'(bagof(X,P,S), Y, L0, L) :- !,
	'$variables_in_term'(X+Y, [], NY),
	'$excess_vars'((P,S), NY, L0, L).
'$excess_vars'(findall(_,_,S), Y, L0, L) :- !,
	'$excess_vars'(S, Y, L0, L).
'$excess_vars'(findall(_,_,_,S), Y, L0, L) :- !,
	'$excess_vars'(S, Y, L0, L).
'$excess_vars'(\+_, _, L0, LF) :- !,
	L0 = LF.
'$excess_vars'(_:G, Y, L0, LF) :- !,
	'$excess_vars'(G, Y, L0, LF).
'$excess_vars'(T, X, L0, L) :-
	T =.. [_|LArgs],
	'$recurse_for_excess_vars'(LArgs, X, L0, L).

'$recurse_for_excess_vars'([], _, L, L).
'$recurse_for_excess_vars'([T1|LArgs], X, L0, L) :-
	'$excess_vars'(T1, X, L0, L1),
	'$recurse_for_excess_vars'(LArgs, X, L1, L).

'$doesnt_include'([], _).
'$doesnt_include'([Y|L], X) :-
	Y \== X,
	'$doesnt_include'(L, X).

% as an alternative to setof you can use the predicate all(Term,Goal,Solutions)
% But this version of all does not allow for repeated answers
% if you want them use findall	

all(T,G same X,S) :- !, all(T same X,G,Sx), '$$produce'(Sx,S,X).
all(T,G,S) :- '$recorda'('$$one','$',R), (
	'$execute'(G), '$recorda'('$$one',T,_), fail ;
	'$$set'(S,R) ).

% $$set does its best to preserve space
'$$set'(S,R) :- '$$build'(S,[],R),
	( S=[], !, fail;
	  '$recorda'('$$set',S,_), fail ).
'$$set'(S,_) :- '$recorded'('$$set',S,R), erase(R).

'$$build'(Ns,S,Start) :- '$recorded'('$$one',X,R), erase(R),
	( Start==R, Ns=S;
	  '$$join'(S,X,Xs), '$$build'(Ns,Xs,Start) ), !.

'$$join'(S,El,S) :- '$$in'(S,El).
'$$join'(S,El,[El|S]).

'$$in'([El|_],El).
'$$in'([_|S],El) :- '$$in'(S,El).

'$$produce'([T1 same X1|Tn],S,X) :- '$$split'(Tn,T1,X1,S1,S2),
	( S=[T1|S1], X=X1;
	  !, produce(S2,S,X) ).

'$$split'([],_,_,[],[]).
'$$split'([T same X|Tn],T,X,S1,S2) :- '$$split'(Tn,T,X,S1,S2).
'$$split'([T1 same X|Tn],T,X,[T1|S1],S2) :- '$$split'(Tn,T,X,S1,S2).
'$$split'([T1|Tn],T,X,S1,[T1|S2]) :- '$$split'(Tn,T,X,S1,S2).


'$check_list_for_bags'(V, _) :- var(V), !.
'$check_list_for_bags'([], _) :- !.
'$check_list_for_bags'([_|B], T) :- !,
	'$check_list_for_bags'(B,T).
'$check_list_for_bags'(S, T) :-
	'$do_error'(type_error(list,S),T).

