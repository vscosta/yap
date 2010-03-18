/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File: parsing.pl
 *	Parses predicates to load LPADs (main predicate: parse(FileNameNoExt)
 *	Copyright (c) 2009, Stefano Bragaglia
 *============================================================================*/
  
:- dynamic rule/4, def_rule/2.

:- use_module(params).
:- use_module(utility).

% :- source.
% :- yap_flag(single_var_warnings, on).





/* parse(File) 
 * -----------
 * This predicate parses the given .cpl file. 
 *
 * Note: it can be called more than once without exiting yap 
 *
 * INPUT
 *  - File: .cpl file to parse, without extension.
 */
parse(File) :- 
	atom_concat(File, '.cpl', FileName),
	open(FileName, read, FileHandle), 
	read_clauses(FileHandle, Clauses), 
	close(FileHandle), 
	retractall(rule_by_num(_, _, _, _, _)), 
	retractall(rule(_, _, _, _, _, _, _, _)), 
	retractall(def_rule(_, _)), 
	process_clauses(Clauses, 1).







/* assert_rules()
 * --------------
 * This tail recursive predicate parses the given list of (Head:Prob) couples
 * and stores them incrementally as rules along with the other parameters.
 *
 * INPUT
 *  - Head: current head part.
 *  - Prob: probability of the current head part.
 *  - Index: index of the current head part.
 *  - Subst: substitution for the current head part.
 *  - Choices: list of current head parts indexes.
 *  - HeadList: complete head or list of its parts.
 *  - BodyList: complete body or list of its parts.
 */
assert_rules([], _Index, _HeadList, _BodyList, _Choices, _Id, _Subst) :- !. % Closing condition.

assert_rules(['':_Prob], _Index, _HeadList, _BodyList, _Choices, _Id, _Subst) :- !.

assert_rules([Head:Prob|Tail], Index, HeadList, BodyList, Choices, Id, Subst) :- 
	assertz(rule(Head, Prob, Index, Id, Subst, Choices, HeadList, BodyList)), 
	Next is Index + 1, 
	assert_rules(Tail, Next, Id, Subst, Choices, HeadList, BodyList).



delete_var(_Var, [], []).

delete_var(Var, [Current|Tail], [Current|Next]) :- 
	Var \== Current, !, 
	delete_var(Var, Tail, Next).

delete_var(_Var, [_Head|Tail], Tail).



extract_vars(Variable, Var0, Var1) :- 
	var(Variable), !, 
	(member_eq(Variable, Var0) ->
		Var1 = Var0;
		append(Var0, [Variable], Var1)).

extract_vars(Term, Var0, Var1) :- 
	Term=..[_F|Args], 
	extract_vars_list(Args, Var0, Var1).


 
extract_vars_clause(end_of_file, []).

extract_vars_clause(Clause, VarNames, Couples) :- 
	(Clause = (Head :- _Body) ->
		true;
		Head = Clause), 
	extract_vars(Head, [], Vars), 
	pair(VarNames, Vars, Couples).
	


extract_vars_list([], Var, Var).

extract_vars_list([Term|Tail], Var0, Var1) :- 
	extract_vars(Term, Var0, Var), 
	extract_vars_list(Tail, Var, Var1).



get_var(Var, [Var]) :- 
	var(Var), !. % Succeeds if Var is currently a free variable, otherwise fails. 

get_var(Var, Value) :- 
	Var=..[_F|Args],
	get_var_list(Args, Value).



get_var_list([], []).

get_var_list([Head|Tail], [Head|Next]) :- 
	var(Head), !, 
	get_var_list(Tail, Next).

get_var_list([Head|Tail], Vars) :- !, 
	get_var(Head, Var), 
	append(Var, Next, Vars), 
	get_var_list(Tail, Next).



/* ground_prob(HeadList)
 * ---------------------
 * This tail recursive predicate verifies if the given HeadList is ground.
 *
 * INPUT
 *  - HeadList: list of heads to verify its groundness.
 */
ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :- 
	ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
	ground_prob(Tail).



pair(_VarName, [], []).

pair([VarName = _Var|TailVarName], [Var|TailVar], [VarName = Var|Tail]) :- 
	pair(TailVarName, TailVar, Tail).	



/* process_head(HeadList, CompleteHeadList)
 * ----------------------------------------
 * Note: if the annotation in the head are not ground, the null atom is not 
 *       added and the eventual formulas are not evaluated.
 */	
process_head(HeadList, GroundHeadList) :- 
	ground_prob(HeadList), !,
	process_head_ground(HeadList, 0, GroundHeadList);
	 
process_head(HeadList, HeadList).



/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null]) :- 
	ProbLast is 1 - Prob - ProbHead, 
	setting(epsilon_parsing, Eps), 
	EpsNeg is - Eps, 
	ProbLast > EpsNeg, 
	(ProbLast > Eps ->
		Null = ['':ProbLast];
		Null = []). 

process_head_ground([Head:ProbHead|Tail], Prob, [Head:ProbHead|Next]) :- 
	ProbNext is Prob + ProbHead, 
	process_head_ground(Tail, ProbNext, Next).

















/* process_body(BodyList, Vars0, Vars1)
 * ------------------------------------
 * Note: setof must have a goal in the form B^G, where B is a term containing 
 *       the existential variables.
 */
process_body([], Vars, Vars).

process_body([setof(A, B^_G, _L)|Tail], Vars0, Vars1) :- !, 
	get_var(A, VarsA), 
	get_var(B, VarsB), 
	remove_vars(VarsA, Vars0, Vars3), 
	remove_vars(VarsB, Vars3, Vars2), 
	process_body(Tail, Vars2, Vars1).

process_body([setof(A, _G, _L)|Tail], Vars0, Vars1) :- !, 
	get_var(A, VarsA), 
	remove_vars(VarsA, Vars0, Vars2), 
	process_body(Tail, Vars2, Vars1).

process_body([bagof(A, B^_G, _L)|Tail], Vars0, Vars1) :- !, 
	get_var(A, VarsA), 
	get_var(B, VarsB), 
	remove_vars(VarsA, Vars0, Vars3), 
	remove_vars(VarsB, Vars3, Vars2), 
	process_body(Tail, Vars2, Vars1).

process_body([bagof(A, _G, _L)|Tail], Vars0, Vars1) :- !, 
	get_var(A, VarsA), 
	remove_vars(VarsA, Vars0, Vars2), 
	process_body(Tail, Vars2, Vars1).

process_body([_Head|Tail], Vars0, Vars1) :- !, 
	process_body(Tail, Vars0, Vars1).







process_clauses([(end_of_file, [])], _Id).

/* NB: il seguente predicato è stato commentato perchè usa predicati non conformi
 *     a quelli attesi (vedi 'rule\5').
 * /
process_clauses([((Head :- Body), Value)|Tail], Id) :- 
	Head=uniform(A, P, L), !, 
	list2and(BodyList, Body), 
	process_body(BodyList, Value, BodyListValue), 
	remove_vars([P], BodyListValue, V2), 
	append(BodyList, [length(L, Tot), nth0(Number, L, P)], BL1), 
	append(V2, ['Tot'=Tot], V3), 
	assertz(rule(Id, V3, _NH, uniform(A:1/Tot, L, Number), BL1)), 
	assertz(rule_uniform(A, Id, V3, _NH, 1/Tot, L, Number, BL1)), 
	N1 is Id+1, 
	process_clauses(Tail, N1). */

process_clauses([((Head :- Body), Value)|Tail], Id) :- 
	Head = (_;_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	list2and(BodyList, Body), 
	process_body(BodyList, Value, BodyListValue), 
	length(HeadList, LH), 
	listN(0, LH, NH), 
	assert_rules(HeadList, 0, HeadList, BodyList, NH, Id, BodyListValue), 
	assertz(rule_by_num(Id, BodyListValue, NH, HeadList, BodyList)), 
	N1 is Id+1, 
	process_clauses(Tail, N1).

process_clauses([((Head :- Body), Value)|Tail], Id) :- 
	Head = (_:_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	list2and(BodyList, Body), 
	process_body(BodyList, Value, BodyListValue), 
	length(HeadList, LH), 
	listN(0, LH, NH), 
	assert_rules(HeadList, 0, HeadList, BodyList, NH, Id, BodyListValue), 
	assertz(rule_by_num(Id, BodyListValue, NH, HeadList, BodyList)), 
	N1 is Id+1, 
	process_clauses(Tail, N1).
	
process_clauses([((Head :- Body), _V)|Tail], Id) :- !, 
	list2and(BodyList, Body), 
	assert(def_rule(Head, BodyList)), 
	process_clauses(Tail, Id).

process_clauses([(Head, Value)|Tail], Id) :- 
	Head=(_;_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	length(HeadList, LH), 
	listN(0, LH, NH), 
	assert_rules(HeadList, 0, HeadList, [], NH, Id, Value), 
	assertz(rule_by_num(Id, Value, NH, HeadList, [])), 
	N1 is Id+1, 
	process_clauses(Tail, N1).

process_clauses([(Head, Value)|Tail], Id) :- 
	Head=(_:_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	length(HeadList, LH), 
	listN(0, LH, NH), 
	assert_rules(HeadList, 0, HeadList, [], NH, Id, Value), 
	assertz(rule_by_num(Id, Value, NH, HeadList, [])), 
	N1 is Id+1, 
	process_clauses(Tail, N1).
	
process_clauses([(Head, _V)|Tail], Id) :- 
	assert(def_rule(Head, [])), 
	process_clauses(Tail, Id).



read_clauses(Stream, Clauses) :- 
	(setting(ground_body, true) ->
		read_clauses_ground_body(Stream, Clauses);
		read_clauses_exist_body(Stream, Clauses)).



read_clauses_exist_body(Stream, [(Clause, Vars)|Next]) :- 
	read_term(Stream, Clause, [variable_names(VarNames)]), 
	extract_vars_clause(Clause, VarNames, Vars), 
	(Clause = end_of_file ->
		Next = [];
		read_clauses_exist_body(Stream, Next)).



read_clauses_ground_body(Stream, [(Clause, Vars)|Next]) :- 
	read_term(Stream, Clause, [variable_names(Vars)]), 
	(Clause = end_of_file ->
		Next = [];
		read_clauses_ground_body(Stream, Next)).



remove_vars([], Vars, Vars).

remove_vars([Head|Tail], Vars0, Vars1) :- 
	delete_var(Head, Vars0, Vars2), 
	remove_vars(Tail, Vars2, Vars1).
