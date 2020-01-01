/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
**************************************************************************
*								         *
* File:		boot.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* commen    ts:	boot file for Prolog					 *
*									 *
*************************************************************************/

/**
  @file boot.yap
  @brief YAP bootstrap

  @addtogroup YAPControl Control Predicates
  @ingroup builtins
  @{

*/

/** @pred  0:P,0:Q   is iso, meta
Conjunction of goals (and).

The conjunction is a fundamental construct of Prolog. Example:

~~~~~~~
 p(X) :- q(X), r(X).
~~~~~~~

should be read as `p( _X_) if q( _X_) and r( _X_).


*/
','(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        '$call'(X,CP,(X,Y),M),
        '$call'(Y,CP,(X,Y),M).

/** @pred   0:P ; 0:Q  is iso
Disjunction of goals (or).

Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 p(X) :- q(X); r(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
should be read as "p( _X_) if q( _X_) or r( _X_)".


*/
';'((X->A),Y) :- !,
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$execute'(X)
	->
	  '$call'(A,CP,(X->A;Y),M)
	;
	  '$call'(Y,CP,(X->A;Y),M)
	).
';'((X*->A),Y) :- !,
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
	(
	 '$current_choice_point'(DCP),
	 '$execute'(X),
	 yap_hacks:cut_at(DCP),
	 '$call'(A,CP,((X*->A),Y),M)
        ;
	 '$call'(Y,CP,((X*->A),Y),M)
	).
';'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X;Y),M) ; '$call'(Y,CP,(X;Y),M) ).


'|'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X|Y),M) ; '$call'(Y,CP,(X|Y),M) ).

/** @pred   0:Condition -> 0:Action  is iso

@short If _Condition__ has a solution, call _Action_;

@long
Read as "if-then-else" or "commit". This operator is similar to the
conditional operator of imperative languages and can be used alone or
with an else part as follows:


~~~~~
    +P -> +Q
~~~~~

"if P then Q".


~~~~~
  +P -> +Q; +R
~~~~~

"if P then Q else R".

These two predicates could be defined respectively in Prolog as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (P -> Q) :- P, !, Q.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (P -> Q; R) :- P, !, Q.
 (P -> Q; R) :- R.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if there were no "cuts" in  _P_,  _Q_ and  _R_.

vNote that the commit operator works by "cutting" any alternative
solutions of  _P_.

Note also that you can use chains of commit operators like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P -> Q ; R -> S ; T.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that `(->)/2` does not affect the scope of cuts in its
arguments.


*/
'->'(X,Y) :-
	yap_hacks:env_choice_point(CP),
	'$current_module'(M),
        ( '$call'(X,CP,(X->Y),M) -> '$call'(Y,CP,(X->Y),M) ).


/** @pred    0:Condition *-> 0:Action  is iso

This construct implements the so-called <em>soft-cut</em>. The control is
defined as follows:
  + If  _Condition_ succeeds at least once, the
semantics is the same as ( _Condition_,  _Action_).

  + If
 _Condition_ does not succeed, the semantics is that of (\\+
 _Condition_,  _Else_).

 In other words, if  _Condition_
succeeds at least once, simply behave as the conjunction of
 _Condition_ and  _Action_, otherwise execute  _Else_.

The construct  _A *-> B_, i.e. without an  _Else_ branch, is
translated as the normal conjunction  _A_,  _B_.


*/
'*->'(X,Y) :-
	current_module(M),
	yap_hacks:env_choice_point(CP),
        X, '$call'(Y,CP,(X*->Y),M).

/** @pred  ! is iso


Read as "cut". Cuts any choices taken in the current procedure.
When first found "cut" succeeds as a goal, but if backtracking should
later return to it, the parent goal (the one which matches the head of
the clause containing the "cut", causing the clause activation) will
fail. This is an extra-logical predicate and cannot be explained in
terms of the declarative semantics of Prolog.

example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 member(X,[X|_]).
 member(X,[_|L]) :- member(X,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the above definition

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ?- member(X,[1,2,3]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

will return each element of the list by backtracking. With the following
definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 member(X,[X|_]) :- !.
 member(X,[_|L]) :- member(X,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

the same query would return only the first element of the
list, since backtracking could not "pass through" the cut.

*/
! :-
	yap_hacks:parent_choice_point(CP),
	yap_hacks:cut_at(CP).

/** @pred   \+ 0:P   is iso, meta
Negation by failure.

Goal  _P_ is not provable. The execution of this predicate fails if
and only if the goal  _P_ finitely succeeds. It is not a true logical
negation, which is impossible in standard Prolog, but
"negation-by-failure".

This predicate might be defined as:

~~~~~~~~~~~~
 \+(P) :- P, !, fail.
 \+(_).
~~~~~~~~~~~~
if  _P_ did not include "cuts".

If _P_ includes cuts, the cuts are defined to be scoped by _P_: they cannot cut over the calling prredicate.

 ~~~~~~~~~~~~
  go(P).

:- \+ P, !, fail.
  \+(_).
 ~~~~~~~~~~~~

*/
\+(G) :-     \+ '$execute'(G).

not(G) :-    \+ '$execute'(G).




/** @pred  repeat is iso
Succeeds repeatedly.

In the next example, `repeat` is used as an efficient way to implement
a loop. The next example reads all terms in a file:
~~~~~~~~~~~~~
 a :- repeat, read(X), write(X), nl, X=end_of_file, !.
~~~~~~~~~~~~~
the loop is effectively terminated by the cut-goal, when the test-goal
`X=end` succeeds. While the test fails, the goals `read(X)`,
`write(X)`, and `nl` are executed repeatedly, because
backtracking is caught by the `repeat` goal.

The built-in `repeat/0` could be defined in Prolog by:

~~~~~

repeat.
repeat :- repeat.
~~~~~

The predicate between/3 can be used to iterate for a pre-defined
number of steps.

*/
 repeat :- '$repeat'.

 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat' :- '$repeat'.

/** @pred  + _P_ is nondet

The same as `call( _P_)`. This feature has been kept to provide
compatibility with C-Prolog. When compiling a goal, YAP
generates a `call( _X_)` whenever a variable  _X_ is found as
a goal.

~~~~~
 a(X) :- X.
~~~~~
is converted to:

~~~~~
 a(X) :- call(X).
~~~~~


*/

/** @pred  call( 0:P ) is iso
Meta-call predicate.

If _P_ is instantiated to an atom or a compound term, the goal `call(
_P_)` is executed as if the clause was originally written as _P_
instead as call( _P_ ), except that any "cut" occurring in _P_ only
cuts alternatives in the execution of _P_.


*/
call(G) :- '$execute'(G).

/** @pred  incore( 0:P )

  same as call/1.

  */

/** @pred  once( 0 G) is iso


Execute the goal  _G_ only once. The predicate is defined by:

~~~~~
 once(G) :- call(G), !.
~~~~~

Note that cuts inside once/1 can only cut the other goals inside
once/1.


*/
once(G) :-
	strip_module(G, M, C),
	'$meta_call'(C, M),
	!.

M:G ;- call(M:G).

(:- G) :- '$execute'(G), !.

(?- G) :- '$execute'(G).

'$$!'(CP) :- '$cut_by'(CP).

[] :- true.

%% @}
