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

/**
@defgroup Grammars Grammar Rules
@ingroup Builtins
@{

Grammar rules in Prolog are both a convenient way to express definite
clause grammars and  an extension of the well known context-free grammars.

A grammar rule is of the form:

```
head --> body
```
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
:- module_( '$_grammar', []).
/*
!/2,
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
        ('|')/4]. [throw_error/2,
t_body/6]).
*/

% :- meta_predicate ^(?,0,?).
% ^(Xs, Goal, Xs) :- call(Goal).

% :- meta_predicate ^(?,1,?,?).
% ^(Xs0, Goal, Xs0, Xs) :- call(Goal, Xs).



/*
    Variables X in grammar rule bodies are translated as
    if phrase(X) had been written, where phrase/3 is obvious.
    Also, phrase/2-3 check their first argument.
a*/

prolog:'$translate_rule'(Rule, (NH :- B) ) :-
    current_source_module( SM, SM ),
    '$yap_strip_module'( SM:Rule,  M0, (LP-->RP) ),
    t_head(LP, NH0, NGs, S, SR, (LP-->SM:RP)),
    '$yap_strip_module'( M0:NH0,  M, NH1 ),
    ( M == SM -> NH = NH1 ; NH = M:NH1 ),
    (var(NGs) ->
	 t_body(RP, SM, _, last, S, SR, B1)
     ;
     t_body((RP,{NGs}), SM, _, last, S, SR, B1)
    ),
    t_tidy(B1, B).

t_head(V,_, _, _, _, G0) :- var(V), !,
    throw_error(instantiation_error,G0).
t_head((H,List), NH, NGs, S, S1, G0) :- !,
	t_hgoal(H, NH, S, SR, G0),
	t_hlist(List, S1, SR, NGs, G0).
t_head(H, NH, _, S, SR, G0) :-
	t_hgoal(H, NH, S, SR, G0).

t_hgoal(V,_,_,_,_,G0) :-
    var(V),
    !,
    throw_error(instantiation_error,G0).
t_hgoal(M:H, M:NH, S, SR, G0) :- !,
	t_hgoal(H, NH, S, SR, G0).
t_hgoal(H, NH, S, SR, _) :-
	dcg_extend([S,SR],H,NH).

t_hlist(V, S0, SR, phrase(V,S0,SR), _G0) :-
    var(V), !,
    phrase_(V, S0, SR).
t_hlist([], _, _, true, _).
t_hlist(String, S0, SR, SR=SF, _G0) :-
    string(String), !,
    string_codes( String, X ),
    diff_list(S0,X,SF).
t_hlist([H|List], S0, SR, SR=SF, _Goal) :-
     diff_list(S0,[H|List],SF),
    !.
t_hlist(T, _, _, _, Goal) :-
    throw_error(type_error(list,T),Goal).


%
% Two extra variables:
% ToFill tells whether we need to explictly close the chain of
% variables.
% Last tells whether we are the ones who should close that chain.
%
t_body(Var, SM, filled_in, _, S, S1, call(SM:Var,S,S1)) :-
    var(Var),
    !.
t_body(Var0, SM, filled_in, _, S, S1, call(SM:Var0,S,S1)) :-
    \+ callable(Var0),
    !.
t_body({T}, SM, _, _, S, S, call(SM:T)) :- var(T), !.
t_body({T}, SM, _, _, S, S, SM:T) :- !.
t_body(!,_, _, _, S, S, !) :- !.
t_body([], _,_, _, S, S, true) :- !.
t_body(X, SM, FilledIn, Last, S, SR, OS) :- string(X), !,
	string_codes( X, Codes),
	t_body(Codes, SM, FilledIn, Last, S, SR, OS).
t_body([X|R], _, filled_in, _Last, S, SR, (S=SF)) :-
    '$append'([X|R],SR,SF), !.
t_body((T,R), SM, ToFill, Last, S, SR, (Tt,Rt)) :- !,
	t_body(T, SM, ToFill, not_last, S, SR1, Tt),
	t_body(R, SM, ToFill, Last, SR1, SR, Rt).
t_body((T->R), SM, ToFill, Last, S, SR, (Tt->Rt)) :- !,
	t_body(T, SM, ToFill, not_last, S, SR1, Tt),
	t_body(R, SM, ToFill, Last, SR1, SR, Rt).
t_body(\+T, SM, ToFill, _, S, SR, (Tt->fail ; S=SR)) :- !,
	t_body(T, SM, ToFill, not_last, S, _, Tt).
t_body((T;R), SM, _ToFill, Last, S, SR, (O1;O2)) :- !,
	t_body(T, SM, _, Last, S, SR1,Tt),
	t_body(R, SM, _, Last, S, SR2,Rt),
	(S == SR1 -> O1=(Tt, SR1=SR) ; SR1=SR, O1=Tt),
	(S == SR2 -> O2=(Rt, SR2=SR) ; SR2=SR, O2=Rt).
t_body((T|R), SM, _ToFill, Last, S, SR, ((SF1=S,Tt,S01=SR);(SF2=S,Rt,S02=SR))) :- !,
	t_body(T, SM, _, Last, SF1, S01,Tt),
	t_body(R, SM, _, Last, SF2, S02,Rt).
t_body(M:G, _SM, ToFill, Last, S, SR, NG) :-
!,
    t_body(G, M, ToFill, Last, S, SR, NG).
t_body(call(T), _, filled_in, _, S, SR, call(T,S,SR)) :-
    !.
t_body(T, M, filled_in, _, S, SR, M:Tt) :-
	dcg_extend([S,SR], T, Tt).

diff_list(L,[],L).
diff_list(L,[C|Cs],[C|NL]) :-
    diff_list(L,Cs,NL).

dcg_extend(More, OldT, NewT) :-
	OldT =.. OldL,
	'$append'(OldL, More, NewL),
	NewT =.. NewL.

t_tidy(B,NB) :-
    t_tidy(B,BL,[]),
    l2g(BL,NB).

t_tidy(P,[P|L],L) :-
    var(P),
    !.
t_tidy((P1;P2), [(Q1;Q2)|L],L) :- !,
    t_tidy(P1, Q1),
    t_tidy(P2, Q2).
t_tidy((P1->P2), [(Q1->Q2)|L],L) :- !,
    t_tidy(P1, Q1),
    t_tidy(P2, Q2).
t_tidy((P1,P2), LF,L0) :- !,
    t_tidy(P1, LF, L1),
    t_tidy(P2, L1, L0).
t_tidy(SM:(P1;P2), [(Q1;Q2)|L],L) :- !,
    t_tidy(SM:P1, Q1),
    t_tidy(SM:P2, Q2).
t_tidy(SM:(P1->P2), [(Q1->Q2)|L],L) :- !,
    t_tidy(SM:P1, Q1),
    t_tidy(SM:P2, Q2).
t_tidy(SM:(P1,P2), LF,L0) :- !,
    t_tidy(SM:P1, LF, L1),
    t_tidy(SM:P2, L1, L0).
t_tidy(true, L, L) :- !.
t_tidy(_SM:P, [P|L0],L0) :-
    predicate_property(P, system),
    !.
t_tidy(A, [A|L], L).

l2g([], true).
l2g([G], G) :-
    !.
l2g([G|LGs], (G,Gs)) :-
    l2g(LGs,Gs).

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

:- meta_predicate phrase(2,-,+), phrase(1,-).

prolog:phrase(V, S, S0) :-
    strip_module(V, M, VF),
    phrase_(VF, M, S, S0).

phrase_(V, _, S, S0) :-
    var(V),
    !,
    throw_error(instantiation_error,phrase(V,S,S0)).
phrase_(String, _,S, S0) :-
    string(String),
    !,
    string_codes(String,Codes),
    '$append'(Codes,S0,S).
phrase_([H|T],_, SR, SL) :-
    !,
    '$append'([H|T],SL,SR).
phrase_([], _, S0, S) :-
    !,
    S0 = S.
phrase_(P, M, S0, S) :-
    call(M:P, S0, S).


prolog:!(S, S).

prolog:true(S, S).

prolog:[](S, S).

prolog:[](H, T, S0, S) :- '$append'([H|T], S, S0).

prolog:'.'(H,T, S0, S) :-
	'$append'([H|T], S, S0).

prolog:{}(Goal, S0, S) :-
	Goal,
	S0 = S.

prolog:','(A,B,C,D) :-
    call(A,C,E),
    call(B,E,D).

prolog:';'(A,B) -->
    call(A);
    call(B).


prolog:('|'(A,B, S0, S)) :-
	 t_body((A|B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:'->'(A,B, S0, S) :-
	 t_body((A->B), _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:'\\+'(A, S0, S) :-
	 t_body(\+ A, _, last, S0, S, Goal),
	 '$execute'(Goal).

prolog:'$inline'(Mod:phrase(NT,Xs0,Xs),B):-
    callable(NT),
     '$c_phrase'(NT, Xs0, Xs, Mod, B).
prolog:'$inline'(Mod:phrase(NT,Xs0),B):-
    callable(NT),
    '$c_phrase'(NT, Xs0, [], Mod, B).

prolog:'$inline'(_:'C'(A,B,C),(A=[B|C])) :- !.



'$c_phrase'(NT, Xs0, Xs, Mod, B) :-
    t_body(NT, Mod, _, last, Xs0, Xs, B1),

    t_tidy(B1, B) .

/**
@}
*/
