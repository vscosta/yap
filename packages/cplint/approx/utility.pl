/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File: parsing.pl
 *	Parses predicates to load LPADs (main predicate: parse(FileNameNoExt)
 *	Copyright (c) 2009, Stefano Bragaglia
 *============================================================================*/
  
:- dynamic rule/4, def_rule/2.

% :- source.
% :- yap_flag(single_var_warnings, on).





/* BUILTIN PREDICATES
 * ------------------
 * This section declares the builtin predicates.
 */
builtin(_A is _B).
builtin(_A > _B).
builtin(_A < _B).
builtin(_A >= _B).
builtin(_A =< _B).
builtin(_A =:= _B).
builtin(_A =\= _B).
builtin(true).
builtin(false).
builtin(_A = _B).
builtin(_A==_B).
builtin(_A\=_B).
builtin(_A\==_B).
builtin(length(_L, _N)).
builtin(member(_El, _L)).
builtin(average(_L, _Av)).
builtin(max_list(_L, _Max)).
builtin(min_list(_L, _Max)).
builtin(nth0(_, _, _)).
builtin(nth(_, _, _)).
builtin(eraseall(_Id)).
builtin(recordz(_Id, _Item, _)).
builtin(recordzifnot(_Id, _Item, _)).



member_eq(Item, [Head|_Tail]) :- 
	Item==Head, !.
	
member_eq(Item, [_Head|Tail]) :- 
	member_eq(Item, Tail).



not_already_present_with_a_different_head(_HeadId, _RuleId, _Subst, []).

not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(HeadId1, RuleId, Subst1)|Tail]) :- 
	not_different(HeadId, HeadId1, Subst, Subst1), !, 
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).
	
not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(_HeadId1, RuleId1, _Subst1)|Tail]) :- 
	RuleId \== RuleId1, 
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).



not_different(_HeadId, _HeadId1, Subst, Subst1) :- 
	Subst \= Subst1, !.	

not_different(HeadId, HeadId1, Subst, Subst1) :- 
	HeadId \= HeadId1, !, 
	dif(Subst, Subst1).	

not_different(HeadId, HeadId, Subst, Subst).



get_groundc([], [], [], P, P) :- !.

get_groundc([H|T], [H|T1], TV, P0, P1) :- 
	ground(H), !, 
	H=(N, R, S), 
	rule_by_num(R, S, _N, Head, _Body), 
	nth0(N, Head, (_A:P)), 
	P2 is P0*P, 
	get_groundc(T, T1, TV, P2, P1).

get_groundc([H|T], T1, [H|TV], P0, P1) :- 
	get_groundc(T, T1, TV, P0, P1).

get_prob([], P, P) :- !.

get_prob([H|T], P0, P1) :- 
	H=(N, R, S), 
	rule_by_num(R, S, _N, Head, _Body), 
	nth0(N, Head, (_A:P)), 
	P2 is P0*P, 
	get_prob(T, P2, P1).




find_rulec(H, (R, S, N), Body, C, P) :- 
	rule(H, P, N, R, S, _NH, _Head, Body), 
	not_already_present_with_a_different_head(N, R, S, C).



/* var2numbers([(Rule, Subst)|CoupleTail], Index, [[Index, Heads, Probs]|TripleTail])
 * ----------------------------------------------------------------------------------
 * This tail recursive predicate converts a list of couples (Rule, Subst) into a
 * list of triples (Index, Count, Probs).
 * Rule and Subst are the index of their equivalent rule and substitution.
 * Index is a progressive identifier starting from 0.
 * Count is the number of head atoms and Probs is the vector of their 
 * probabilities.
 *
 * INPUT
 *  - Couples: list of couples to convert.
 *
 * OUTPUT
 *  - Triples: list of equivalent triples.
 */
var2numbers([], _N, []).

var2numbers([(Rule, Subst)|CoupleTail], Index, [[Index, Heads, Probs]|TripleTail]) :- 
	find_probs(Rule, Subst, Probs), 
	length(Probs, Heads), 
	Next is Index+1, 
	var2numbers(CoupleTail, Next, TripleTail).



/* build_formula(ListC, Formula, VarIn, VarOut) 
 * --------------------------------------------
 * This predicate parses a given list of C sets with a given list of variables 
 * and returns the equivalent formula with its list of variables.
 * 
 * Note: each Formula is expressed in the form: [Term1, ..., TermN], where each 
 *       term is expressed in the form: [Factor1, ..., FactorM], where each 
 *       factor is hence expressed in the form: (Var, Name).
 *       Finally, Var is the index of the multivalued variable Var, and Value is 
 *       the index of its value.
 *
 * INPUT
 *  - ListC: given list of C sets.
 *  - VarIn: list of variables pertaining to ListC.
 * 
 * OUTPUT
 *  - Formula: the formula equivalent to ListC.
 *  - VarOut: list of variables pertaining to Formula.
 */
build_formula([], [], Var, Var, Count, Count).
%% Closing condition: stop if no more terms (current Var is final Var, current Count is final Count)

build_formula([D|TD], [F|TF], VarIn, VarOut, C0, C1) :- 
	length(D, NC), 
	C2 is C0+NC, 
	reverse(D, D1), 
	build_term(D1, F, VarIn, Var1), 
	build_formula(TD, TF, Var1, VarOut, C2, C1).
	%% Recursive call: procedd to next terms, building rest of formula and handling vars and count.

build_formula([], [], Var, Var).

build_formula([D|TD], [F|TF], VarIn, VarOut) :- 
	build_term(D, F, VarIn, Var1), 
	build_formula(TD, TF, Var1, VarOut).



build_term([], [], Var, Var).

build_term([(_, pruned, _)|TC], TF, VarIn, VarOut) :- !,
	build_term(TC, TF, VarIn, VarOut).

build_term([(N, R, S)|TC], [[NVar, N]|TF], VarIn, VarOut) :-
	(nth0_eq(0, NVar, VarIn, (R, S)) ->
		Var1=VarIn;
		append(VarIn, [(R, S)], Var1),
		length(VarIn, NVar)), 
	build_term(TC, TF, Var1, VarOut).							



find_probs(R, S, Probs) :- 
	rule_by_num(R, S, _N, Head, _Body), 
	get_probs(Head, Probs).


	
get_probs(uniform(_A:1/Num, _P, _Number), ListP) :- 
	Prob is 1/Num, 
	list_el(Num, Prob, ListP).

get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :- 
	P1 is P, 
	get_probs(T, T1).



list_el(0, _P, []) :- !.

list_el(N, P, [P|T]) :- 
	N1 is N-1, 
	list_el(N1, P, T).



/* nth0_eq(PosIn, PosOut, List, Elem) 
 * ----------------------------------
 * This predicate searches for an element that matches with the given one in the 
 * given list, starting from the given position, and returns its position.
 *
 * INPUT
 *  - PosIn: initial position.
 *  - List: list to parse.
 *  - Elem: element to match.
 *
 * OUTPUT
 *  - PosOut: next position of a matching element.
 */
nth0_eq(N, N, [H|_T], Elem) :- 
	H==Elem, !.

nth0_eq(NIn, NOut, [_H|T], Elem) :- 
	N1 is NIn+1, 
	nth0_eq(N1, NOut, T, Elem).



list2and([X], X) :- 
	X\=(_, _), !.

list2and([H|T], (H, Ta)) :- !, 
	list2and(T, Ta).



list2or([X], X) :- 
	X\=;(_, _), !.

list2or([H|T], (H ; Ta)) :- !, 
	list2or(T, Ta).



choose_clausesc(_G, C, [], C).

choose_clausesc(CG0, CIn, [D|T], COut) :- 
	member((N, R, S), D), 
	choose_clauses_present(N, R, S, CG0, CIn, COut, T).
	
choose_clausesc(G0, CIn, [D|T], COut) :- 
	member((N, R, S), D), 
	new_head(N, R, S, N1), 
	\+ already_present(N1, R, S, CIn), 
	\+ already_present(N1, R, S, G0), 
	impose_dif_cons(R, S, CIn), 
	choose_clausesc(G0, [(N1, R, S)|CIn], T, COut).



choose_clauses_present(N, R, S, CG0, CIn, COut, T) :- 
	already_present_with_a_different_head_ground(N, R, S, CG0), !, 
	choose_clausesc(CG0, CIn, T, COut).

choose_clauses_present(N, R, S, CG0, CIn, COut, T) :- 
	already_present_with_a_different_head(N, R, S, CIn), 
	choose_a_head(N, R, S, CIn, C1), 
	choose_clausesc(CG0, C1, T, COut).



/* new_head(N, R, S, N1)
 * ---------------------
 * This predicate selects an head for rule R different from N with substitution 
 * S and returns it in N1.
 */
new_head(N, R, S, N1) :- 
	rule_by_num(R, S, Numbers, Head, _Body), 
	Head\=uniform(_, _, _), !, 
	nth0(N, Numbers, _Elem, Rest), 
	member(N1, Rest).

new_head(N, R, S, N1) :- 
	rule_uniform(_A, R, S, Numbers, 1/Tot, _L, _Number, _Body), 
	listN(0, Tot, Numbers), 
	nth0(N, Numbers, _Elem, Rest), 
	member(N1, Rest).



/* already_present(N, R, S, [(N, R, SH)|_T])
 * -----------------------------------------
 * This predicate checks if a rule R with head N and selection S (or one of its 
 * generalizations is in C) is already present in C.
 */
already_present(N, R, S, [(N, R, SH)|_T]) :- 
	S=SH.

already_present(N, R, S, [_H|T]) :- 
	already_present(N, R, S, T).



already_present_with_a_different_head(N, R, S, [(NH, R, SH)|_T]) :- 
	\+ \+ S=SH, NH \= N.

already_present_with_a_different_head(N, R, S, [_H|T]) :- 
	already_present_with_a_different_head(N, R, S, T).

already_present_with_a_different_head_ground(N, R, S, [(NH, R, SH)|_T]) :- 
	S=SH, NH \= N.

already_present_with_a_different_head_ground(N, R, S, [_H|T]) :- 
	already_present_with_a_different_head_ground(N, R, S, T).



impose_dif_cons(_R, _S, []) :- !.

impose_dif_cons(R, S, [(_NH, R, SH)|T]) :- !, 
	dif(S, SH), 
	impose_dif_cons(R, S, T).

impose_dif_cons(R, S, [_H|T]) :- 
	impose_dif_cons(R, S, T).



/* choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, SH)|T])
 * --------------------------------------------------------
 * This predicate chooses and returns an head. 
 * It instantiates a more general rule if it is contained in C with a different 
 * head.
 */
choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, SH)|T]) :- 
	S=SH, 
	dif(N, NH).

/* choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, S), (NH, R, SH)|T])
 * --------------------------------------------------------------------
 * This predicate chooses and returns an head.
 * It instantiates a more general rule if it is contained in C with a different 
 * head.
 * It ensures the same ground clause is not generated again.
 */
choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, S), (NH, R, SH)|T]) :- 
	\+ \+ S=SH, S\==SH, 
	dif(N, NH), 
	dif(S, SH).

choose_a_head(N, R, S, [H|T], [H|T1]) :- 
	choose_a_head(N, R, S, T, T1).



listN(N, N, []) :- !.

listN(NIn, N, [NIn|T]) :- 
	N1 is NIn+1, 
	listN(N1, N, T).


