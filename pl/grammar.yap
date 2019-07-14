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
* File:		grammar.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	BNF grammar for Prolog					 *
*									 *
*************************************************************************/

/**
 * @file   grammar.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:20:55 2015
 *
 * @brief  Grammar Rules
 *
 *
*/

:- system_module( '$_grammar', [!/2,
         (',')/4,
         (->)/4,
         ('.')/4,
         (;)/4,
        'C'/3,
        []/2,
        []/4,
        (\+)/3,
        phrase/2,
        phrase/3,
        {}/3,
        ('|')/4], ['$do_error'/2]).

/**
@defgroup Grammars Grammar Rules
@ingroup builtins
@{

Grammar rules in Prolog are both a convenient way to express definite
clause grammars and  an extension of the well known context-free grammars.

A grammar rule is of the form:

~~~~~
head --> body
~~~~~
where both \a head and \a body are sequences of one or more items
linked by the standard conjunction operator `,`.

<em>Items can be:</em>

+
a <em>non-terminal</em> symbol may be either a complex term or an atom.
+
a <em>terminal</em> symbol may be any Prolog symbol. Terminals are
written as Prolog lists.
+
an <em>empty body</em> is written as the empty list `[ ]`.
+
<em>extra conditions</em> may be inserted as Prolog procedure calls, by being
written inside curly brackets `{` and `}`.
+
the left side of a rule consists of a nonterminal and an optional list
of terminals.
+
alternatives may be stated in the right-hand side of the rule by using
the disjunction operator `;`.
+
the <em>cut</em> and <em>conditional</em> symbol (`->`) may be inserted in the
right hand side of a grammar rule


Grammar related built-in predicates:

*/

% :- meta_predicate ^(?,0,?).
% ^(Xs, Goal, Xs) :- call(Goal).

% :- meta_predicate ^(?,1,?,?).
% ^(Xs0, Goal, Xs0, Xs) :- call(Goal, Xs).

:- private(
	   [t_head/6,
	    t_hgoal/5,
	    t_hlist/5,
	    t_body/5,
	    dcg_extend/3,
	    t_tidy/2,
	   '$phrase_list'/3]
	  ).
	


/*
    Variables X in grammar rule bodies are translated as
    if phrase(X) had been written, where phrase/3 is obvious.
    Also, phrase/2-3 check their first argument.
*/

prolog:'$translate_rule'(Rule, (NH :- B) ) :-
    source_module( SM ),
    '$yap_strip_module'( SM:Rule,  M0, (LP-->RP) ),
    t_head(LP, NH0, NGs, S, SR, (LP-->SM:RP)),
    '$yap_strip_module'( M0:NH0,  M, NH1 ),
    ( M == SM -> NH = NH1 ; NH = M:NH1 ),
    (var(NGs) ->
	 t_body(RP, _, last, S, SR, B1)
     ;
     t_body((RP,{NGs}), _, last, S, SR, B1)
    ),
    t_tidy(B1, B).

t_head(V, _, _, _, _, G0) :- var(V), !,
	'$do_error'(instantiation_error,G0).
t_head((H,List), NH, NGs, S, S1, G0) :- !,
	t_hgoal(H, NH, S, SR, G0),
	t_hlist(List, S1, SR, NGs, G0).
t_head(H, NH, _, S, SR, G0) :-
	t_hgoal(H, NH, S, SR, G0).

t_hgoal(V, _, _, _, G0) :- var(V), !,
	'$do_error'(instantiation_error,G0).
t_hgoal(M:H, M:NH, S, SR, G0) :- !,
	t_hgoal(H, NH, S, SR, G0).
t_hgoal(H, NH, S, SR, _) :-
	dcg_extend([S,SR],H,NH).

t_hlist(V, _, _, _, G0) :- var(V), !,
	'$do_error'(instantiation_error,G0).
t_hlist([], _, _, true, _).
t_hlist(String, S0, SR, SF, G0) :- string(String), !,
	string_codes( String, X ),
	t_hlist( X, S0, SR, SF, G0).
t_hlist([H], S0, SR, ('C'(SR,H,S0)), _) :- !.
t_hlist([H|List], S0, SR, ('C'(SR,H,S1),G0), Goal) :- !,
	t_hlist(List, S0, S1, G0, Goal).
t_hlist(T, _, _, _, Goal) :-
	'$do_error'(type_error(list,T),Goal).


%
% Two extra variables:
% ToFill tells whether we need to explictly close the chain of
% variables.
% Last tells whether we are the ones who should close that chain.
%
t_body(Var, filled_in, _, S, S1, phrase(Var,S,S1)) :-
	var(Var),
	!.
t_body(!, to_fill, last, S, S1, (!, S1 = S)) :- !.
t_body(!, _, _, S, S, !) :- !.
t_body([], to_fill, last, S, S1, S1=S) :- !.
t_body([], _, _, S, S, true) :- !.
t_body(X, FilledIn, Last, S, SR, OS) :- string(X), !,
	string_codes( X, Codes),
	t_body(Codes, FilledIn, Last, S, SR, OS).
t_body([X], filled_in, _, S, SR, 'C'(S,X,SR)) :- !.
t_body([X|R], filled_in, Last, S, SR, ('C'(S,X,SR1),RB)) :- !,
	t_body(R, filled_in, Last, SR1, SR, RB).
t_body({T}, to_fill, last, S, S1, (T, S1=S)) :- !.
t_body({T}, _, _, S, S, T) :- !.
t_body((T,R), ToFill, Last, S, SR, (Tt,Rt)) :- !,
	t_body(T, ToFill, not_last, S, SR1, Tt),
	t_body(R, ToFill, Last, SR1, SR, Rt).
t_body((T->R), ToFill, Last, S, SR, (Tt->Rt)) :- !,
	t_body(T, ToFill, not_last, S, SR1, Tt),
	t_body(R, ToFill, Last, SR1, SR, Rt).
t_body(\+T, ToFill, _, S, SR, (Tt->fail ; S=SR)) :- !,
	t_body(T, ToFill, not_last, S, _, Tt).
t_body((T;R), _ToFill, _, S, SR, (Tt;Rt)) :- !,
	t_body(T, _, last, S, SR, Tt),
	t_body(R, _, last, S, SR, Rt).
t_body((T|R), _ToFill, _, S, SR, (Tt;Rt)) :- !,
       t_body(T, _, last, S, SR, Tt),
       t_body(R, _, last, S, SR, Rt).
t_body(M:G, ToFill, Last, S, SR, M:NG) :- !,
	t_body(G, ToFill, Last, S, SR, NG).
t_body(T, filled_in, _, S, SR, Tt) :-
	dcg_extend([S,SR], T, Tt).


dcg_extend(More, OldT, NewT) :-
	OldT =.. OldL,
	lists:append(OldL, More, NewL),
	NewT =.. NewL.

t_tidy(P,P) :- var(P), !.
t_tidy((P1;P2), (Q1;Q2)) :- !,
	t_tidy(P1, Q1),
	t_tidy(P2, Q2).
t_tidy((P1->P2), (Q1->Q2)) :- !,
	t_tidy(P1, Q1),
	t_tidy(P2, Q2).
t_tidy(((P1,P2),P3), Q) :-
	t_tidy((P1,(P2,P3)), Q).
t_tidy((true,P1), Q1) :- !,
	t_tidy(P1, Q1).
t_tidy((P1,true), Q1) :- !,
	t_tidy(P1, Q1).
t_tidy((P1,P2), (Q1,Q2)) :- !,
	t_tidy(P1, Q1),
	t_tidy(P2, Q2).
t_tidy(A, A).

/** @pred  `C`( _S1_, _T_, _S2_)


This predicate is used by the grammar rules compiler and is defined as
`C`([H|T],H,T)`.
 */
prolog:'C'([X|S],X,S).


/** @pred  phrase(+ _P_, _L_)

This predicate succeeds when  _L_ is a phrase of type  _P_. The
same as `phrase(P,L,[])`.

Both this predicate and the previous are used as a convenient way to
start execution of grammar rules.
*/
prolog:phrase(PhraseDef, WordList) :-
	prolog:phrase(PhraseDef, WordList, []).

/** @pred  phrase(+ _P_, _L_, _R_)


This predicate succeeds when the difference list ` _L_- _R_`
is a phrase of type  _P_.
*/
prolog:phrase(V, S0, S) :-
    var(V),
    !,
    '$do_error'(instantiation_error,phrase(V,S0,S)).
prolog:phrase([H|T], S0, S) :-
    !,
    S0 = [H|S1],
    '$phrase_list'(T, S1, S).
prolog:phrase([], S0, S) :-
    !,
    S0 = S.
prolog:phrase(P, S0, S) :-
	call(P, S0, S).

'$phrase_list'([], S, S).
'$phrase_list'([H|T], [H|S1], S0) :-
    '$phrase_list'(T, S1, S0).

prolog:!(S, S).

prolog:[](S, S).

prolog:[](H, T, S0, S) :- lists:append([H|T], S, S0).

prolog:'.'(H,T, S0, S) :-
	lists:append([H|T], S, S0).

prolog:{}(Goal, S0, S) :-
	Goal,
	S0 = S.

prolog:','(A,B, S0, S) :-
	 t_body((A,B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:';'(A,B, S0, S) :-
	 t_body((A;B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:('|'(A,B, S0, S)) :-
	 t_body((A|B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:'->'(A,B, S0, S) :-
	 t_body((A->B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:'\\+'(A, S0, S) :-
	 t_body(\+ A, _, last, S0, S, Goal),
	 '$execute'(Goal).

:- '$new_multifile'( goal_expansion(_,_), prolog).
:- '$mk_dynamic'( goal_expansion(_,_), prolog).

'$c_built_in_phrase'(NT, Xs0, Xs, Mod, NewGoal) :-
	nonvar(NT),
    catch(prolog:'$translate_rule'(
          (pseudo_nt --> Mod:NT), Rule),
     	  error(Pat,ImplDep),
     	  ( \+ '$harmless_dcgexception'(Pat),
     	    throw(error(Pat,ImplDep))
     	  )
     	 ),
         Rule = (pseudo_nt(Xs0c,Xsc) :- NewGoal0),
         Mod:NT \== NewGoal0,
         % apply translation only if we are safe
         \+ '$contains_illegal_dcgnt'(NT),
         !,
         (   var(Xsc), Xsc \== Xs0c
     	->  Xs = Xsc, NewGoal1 = NewGoal0
     	;   NewGoal1 = (NewGoal0, Xsc = Xs)
         ),
         (   var(Xs0c)
     	-> Xs0 = Xs0c,
     	   NewGoal2 = NewGoal1
     	;  ( Xs0 = Xs0c, NewGoal1 ) = NewGoal2
         ),
         '$yap_strip_module'(Mod:NewGoal2, M, NewGoal3),
         (nonvar(NewGoal3) -> NewGoal = M:NewGoal3
         ;
          var(M) -> NewGoal = '$execute_wo_mod'(NewGoal3,M)
         ;
          NewGoal = '$execute_in_mod'(NewGoal3,M)
         ).

do_c_built_in('C'(A,B,C), _, _, (A=[B|C])) :- !.

do_c_built_in(phrase(NT,Xs0, Xs),Mod, _, NewGoal) :-
    nonvar(NT), nonvar(Mod), !,
    '$c_built_in_phrase'(NT, Xs0, Xs, Mod, NewGoal).

do_c_built_in(phrase(NT,Xs),Mod,_,NewGoal) :-
    nonvar(NT), nonvar(Mod),
    '$c_built_in_phrase'(NT, Xs, [], Mod, NewGoal).

/**
@}
*/
