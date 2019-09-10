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

/**
 * @file   setof.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:45:32 2015
 *
 * @brief  Setof and friends.
 *
 *
*/


:- system_module( '$_setof', [(^)/2,
        all/3,
        bagof/3,
        findall/3,
        findall/4,
        setof/3], []).

/**
 * 
 * @defgroup Sets Collecting Solutions to a Goal
 * @ingroup builtins
 * @{
 * 
 * 
 * When there are several solutions to a goal, if the user wants to collect all
 * the solutions he may be led to use the data base, because backtracking will
 * forget previous solutions.
 * 
 * YAP allows the programmer to choose from several system
 * predicates instead of writing his own routines.  findall/3 gives you
 * the fastest, but crudest solution. The other built-in predicates
 * post-process the result of the query in several different ways:
 * 
 */

:- use_system_module( '$_boot', ['$catch'/3]).

:- use_system_module( '$_errors', ['$do_error'/2]).

% this is used by the all predicate

:- op(50,xfx,same).


%% @pred (^)/2
%
% The "existential quantifier" symbol is only significant to bagof
%   and setof, which it stops binding the quantified variable.
%   The priority is `op(200, xfy, ^)` is defined during bootstrap.

_^Goal :-
	'$execute'(Goal).



/** @pred  findall( ?_Template_, 0:_Generator_, -_L_) is iso
 *
 * findall/3 is a simplified version of bagof which has an implicit 
 * existential quantifier on every variable. 
 * Unifies  _L_ with a list that contains all the instantiations of the 
 * term  _Template_ satisfying the goal  _Goal_. Consider the following program:
 *
 * ~~~~ 
 * a(2,1). 
 * a(1,1). 
 * a(2,2). 
 * ~~~~ 
 * the answer to the query 
 * ~~~~ 
 * ?- findall(X,a(X,Y),L). 
 * ~~~~ 
 * would be: 
 * ~~~~ 
 * L = [2,1,2]; 
 * no 
 * ~~~~ 
 */

findall(Template, Generator, Answers) :-
     must_be_of_type( list_or_partial_list, Answers ),
     '$findall'(Template, Generator, [], Answers).


% If some answers have already been found
/** @pred  findall( ?Key, +Goal, +InitialSolutions, -Solutions )

Similar to findall/3, but appends all answers to list  _L0_.


*/
findall(Template, Generator, Answers, SoFar) :-
     must_be_of_type( list_or_partial_list, Answers ),
     '$findall'(Template, Generator, SoFar, Answers).

% starts by calling the generator,
% and recording the answers
'$findall'(Template, Generator, SoFar, Answers) :-
	nb:nb_queue(Ref),
	(
	  '$execute'(Generator),
	  nb:nb_queue_enqueue(Ref, Template),
	 fail
	;
	 nb:nb_queue_close(Ref, Answers, SoFar)
	).


/**
 * findall_with_key( ?Template, 0:_Generator_, - _L_)
 *
 * This built-in is very similar to findall/3, but uses the SICStus
 * algorithm to guarantee that variables will have the same names.
 */
'$findall_with_common_vars'(Template, Generator, Answers) :-
	nb:nb_queue(Ref),
	(
	  '$execute'(Generator),
	  nb:nb_queue_enqueue(Ref, Template),
	  fail
	;
	  nb:nb_queue_close(Ref, Answers, []),
	  '$collect_with_common_vars'(Answers, _)
	).


'$collect_with_common_vars'([], _).
'$collect_with_common_vars'([Key-_|Answers], VarList) :-
	'$variables_in_term'(Key, _, VarList),
	'$collect_with_common_vars'(Answers, VarList).

% This is the setof predicate
/** @pred  setof( _X_,+ _P_,- _B_) is iso

Similar to `bagof( _T_, _G_, _L_)` but sorts list
 _L_, keeping only one copy of each element.  Again, assuming the
same clauses as in the examples above, the reply to the query

~~~
?- setof(X,a(X,Y),L).
~~~
would be:

~~~
Y = 1
L = [1,2];
Y = 2
L = [2];
no
~~~
 */
setof(Template, Generator, Set) :-

	( '$is_list_or_partial_list'(Set) ->
		true
	;
		'$do_error'(type_error(list,Set), setof(Template, Generator, Set))
	),
	'$bagof'(Template, Generator, Bag),
	'$sort'(Bag, Set).

% And this is bagof

% Either we have excess of variables
% and we need to find the solutions for each instantiation
% of these variables

/** @pred  bagof( _T_,+ _G_,- _L_) is iso


For each set of possible instances of the free variables occurring in
 _G_ but not in  _T_, generates the list  _L_ of the instances of
 _T_ satisfying  _G_. Again, assuming the same clauses as in the
examples above, the reply to the query

~~~
bagof(X,a(X,Y),L).

would be:
X = _32
Y = 1
L = [2,1];
X = _32
Y = 2
L = [2];
no
~~~


*/

bagof(Template, Generator, Bag) :-
	( '$is_list_or_partial_list'(Bag) ->
		true
	;
		'$do_error'(type_error(list,Bag), bagof(Template, Generator, Bag))
	),
	'$bagof'(Template, Generator, Bag).

'$bagof'(Template, Generator, Bag) :-
	'$free_variables_in_term'(Template^Generator, StrippedGenerator, Key),
	%format('TemplateV=~w v=~w ~w~n',[TemplateV,Key, StrippedGenerator]),

	( Key \== '$' ->
		'$findall_with_common_vars'(Key-Template, StrippedGenerator, Bags0),
		'$keysort'(Bags0, Bags),
		'$pick'(Bags, Key, Bag)
	;
		'$findall'(Template, StrippedGenerator, [], Bag0),
		Bag0 \== [],
		Bag = Bag0
	).


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
'$decide'([], Bag, Key0, Key, Bag) :- !,
	Key0=Key.
'$decide'(_, Bag, Key, Key, Bag).
'$decide'(Bags, _, _, Key, Bag) :-
	'$pick'(Bags, Key, Bag).

% as an alternative to setof you can use the predicate all(Term,Goal,Solutions)
% But this version of all does not allow for repeated answers
% if you want them use findall
/** @pred  all( _T_,+ _G_,- _L_)


Similar to `findall( _T_, _G_, _L_)` but eliminate
repeated elements. Thus, assuming the same clauses as in the above
example, the reply to the query

~~~
all(X,a(X,Y),L).
~~~
would be:

~~~
X = _32
Y = _33
L = [2,1];
no
~~~

Note that all/3 will fail if no answers are found.


*/
all(T, G same X,S) :- !, all(T same X,G,Sx), '$$produce'(Sx,S,X).
all(T,G,S) :-
	'$init_db_queue'(Ref),
	( catch(G, Error,'$clean_findall'(Ref,Error) ),
	  '$execute'(G),
	  '$db_enqueue'(Ref, T),
	  fail
        ;
	  '$$set'(S,Ref)
        ).

% $$set does its best to preserve space
'$$set'(S,R) :-
       '$$build'(S0,_,R),
        S0 = [_|_],
	S = S0.

'$$build'(Ns,S0,R) :- '$db_dequeue'(R,X), !,
	'$$build2'(Ns,S0,R,X).
'$$build'([],_,_).

'$$build2'([X|Ns],Hash,R,X) :-
	'$$new'(Hash,X), !,
	'$$build'(Ns,Hash,R).
'$$build2'(Ns,Hash,R,_) :-
	'$$build'(Ns,Hash,R).

'$$new'(V,El) :- var(V), !, V = n(_,El,_).
'$$new'(n(R,El0,L),El) :-
	compare(C,El0,El),
	'$$new'(C,R,L,El).

'$$new'(=,_,_,_) :- !, fail.
'$$new'(<,R,_,El) :- '$$new'(R,El).
'$$new'(>,_,L,El) :- '$$new'(L,El).


'$$produce'([T1 same X1|Tn],S,X) :- '$$split'(Tn,T1,X1,S1,S2),
	( S=[T1|S1], X=X1;
	  !, produce(S2,S,X) ).

'$$split'([],_,_,[],[]).
'$$split'([T same X|Tn],T,X,S1,S2) :- '$$split'(Tn,T,X,S1,S2).
'$$split'([T1 same X|Tn],T,X,[T1|S1],S2) :- '$$split'(Tn,T,X,S1,S2).
'$$split'([T1|Tn],T,X,S1,[T1|S2]) :- '$$split'(Tn,T,X,S1,S2).

/**
@}
*/
