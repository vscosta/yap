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
* File:		atts.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	attribute support for Prolog				 *
*									 *
*************************************************************************/

% :- yap_flag(unknown,error).
% :- style_check(all).

%
% Code originally written by Arvin Bansal and Vitor Santos Costa
% Includes nice extensions from Jan Wielemaker (from the SWI version).
%

:- module(coinduction,
          [ (coinductive)/1,
            op(1150, fx, (coinductive))
          ]).

:- use_module(library(error)).

/** <module> Co-Logic Programming

This simple module implements the   directive coinductive/1 as described
in "Co-Logic Programming: Extending Logic  Programming with Coinduction"
by Luke Somin et al. The idea behind coinduction is that a goal succeeds
if it unifies to a parent goal.  This enables some interesting programs,
notably on infinite trees (cyclic terms).

    ==
    :- use_module(library(coinduction)).

    :- coinductive stream/1. 
    stream([H|T]) :- i(H), stream(T).

    % inductive
    i(0).
    i(s(N)) :- i(N).

    ?- X=[s(s(A))|X], stream(X).
     X= [s(s(A))|X], stream(X).
     A = 0,
     X = [s(s(0)),**]
    ==

This predicate is  true  for  any   cyclic  list  containing  only  1-s,
regardless of the cycle-length.

@bug    Programs mixing normal predicates and coinductive predicates must
        be _stratified_.  The theory does not apply to normal Prolog calling
        coinductive predicates, calling normal Prolog predicates, etc.

        Stratification is not checked or enforced in any other way and thus
        left as a responsibility to the user.
@see    "Co-Logic Programming: Extending Logic  Programming with Coinduction"
        by Luke Somin et al.
*/

:- meta_predicate coinductive(:).

:- dynamic coinductive/3.


%-----------------------------------------------------

coinductive(Spec) :-
	var(Spec),
	!,
	throw(error(instantiation_error,coinductive(Spec))).
coinductive(Module:Spec) :-
        coinductive_declaration(Spec, Module, coinductive(Module:Spec)).
coinductive(Spec) :-
        prolog_load_context(module, Module),
        coinductive_declaration(Spec, Module, coinductive(Spec)).
	
coinductive_declaration(Spec, _M, G) :-
	var(Spec),
	!,
	throw(error(instantiation_error,G)).
coinductive_declaration((A,B), M, G) :- !,
	coinductive_declaration(A, M, G),
	coinductive_declaration(B, M, G).
coinductive_declaration(M:Spec, _, G) :- !,
	coinductive_declaration(Spec, M, G).
coinductive_declaration(Spec, M, _G) :-
	valid_pi(Spec, F, N),
	functor(S,F,N), 
	atomic_concat(['__coinductive__',F,'/',N],NF),
	functor(NS,NF,N), 
	match_args(N,S,NS),
	atomic_concat(['__stack_',M,':',F,'/',N],SF), 
	nb_setval(SF, _),
	assert((M:S :-
	       b_getval(SF,L),
		coinduction:in_stack(S, L, End),
		(
		 nonvar(End)
		->
		 true
		;
		 End = [S|_],
		 M:NS)
	       )
	      ),
	assert(coinduction:coinductive(S,M,NS)).
  
valid_pi(Name/Arity, Name, Arity) :-
        must_be(atom, Name),
        must_be(integer, Arity).

match_args(0,_,_) :- !.
match_args(I,S1,S2) :-
	arg(I,S1,A),
	arg(I,S2,A),
	I1 is I-1,
	match_args(I1,S1,S2).

%-----------------------------------------------------

co_term_expansion((M:H :- B), _, (M:NH :- B)) :- !,
	co_term_expansion((H :- B), M, (NH :- B)).
co_term_expansion((H :- B), M, (NH :- B)) :- !,
	coinductive(H, M, NH), !.
co_term_expansion(H, M, NH) :-
	coinductive(H, M, NH), !.

user:term_expansion(M:Cl,M:NCl ) :- !,
	co_term_expansion(Cl, M, NCl).

user:term_expansion(G, NG) :-
        prolog_load_context(module, Module),
	co_term_expansion(G, Module, NG).


%-----------------------------------------------------

in_stack(_, V, V) :- var(V), !.
in_stack(G, [G|_], [G|_]) :- !.
in_stack(G, [_|T], End) :- in_stack(G, T, End).

writeG_val(G_var) :- 
  b_getval(G_var, G_val),
  write(G_var), write(' ==> '), write(G_val), nl.

%-----------------------------------------------------

/**************************************

  Some examples from Coinductive Logic Programming and its Applications by Gopal Gupta et al, ICLP 97

:- coinductive stream/1. 
stream([H|T]) :- i(H), stream(T).

% inductive
i(0).
i(s(N)) :- i(N).

**************************************/

