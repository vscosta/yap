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

:- sequential.

:- module(attributes, []).

:- op(1150, fx, attribute).

:- multifile
	user:goal_expansion/3.
:- multifile
	user:term_expansion/2.

:- dynamic_predicate(existing_attribute/3,logical).
:- dynamic_predicate(modules_with_attributes/1,logical).

:- user_defined_directive(attribute(G), attributes:new_attribute(G)).

modules_with_attributes([]).

user:goal_expansion(get_atts(Var,AccessSpec), Mod, Gs) :- !,
	expand_get_attributes(AccessSpec,Mod,Var,[],GL),
	convert_to_goals(GL,Gs).
user:goal_expansion(put_atts(Var,AccessSpec), Mod, Gs) :- !,
	expand_put_attributes(AccessSpec,Mod,Var,[],GL),
	convert_to_goals(GL,Gs).


%
% defining a new attribute is just a question of establishing a
% Functor, Mod -> INT mappings
%
new_attribute(V) :- var(V), !,
	throw(error(instantiation_error,attribute(V))).
new_attribute((At1,At2)) :-
	new_attribute(At1),
	new_attribute(At2).
new_attribute(Na/Ar) :-
	source_module(Mod),
	functor(S,Na,Ar),
	existing_attribute(S,Mod,_) , !.
new_attribute(Na/Ar) :-
	source_module(Mod),
	inc_n_of_atts(Key),
	functor(S,Na,Ar),
	store_new_module(Mod),
	assertz(existing_attribute(S,Mod,Key)).

store_new_module(Mod) :-
	existing_attribute(_,Mod,_), !.
store_new_module(Mod) :-
	retract(modules_with_attributes(Mods)),
	assertz(modules_with_attributes([Mod|Mods])).

expand_get_attributes(V,Mod,Var,GL0,GL) :- var(V), !,
	GL = [attributes:get_atts_at_run_time(Var,V,Mod)|GL0].
expand_get_attributes([],_,_,LG,LG) :- !.
expand_get_attributes([Att|Atts],Mod,Var,L0,LF) :- !,
	expand_get_attributes(Att,Mod,Var,L0,L1),
	expand_get_attributes(Atts,Mod,Var,L1,LF).
expand_get_attributes(+Att,Mod,Var,L0,LF) :- !,
	expand_get_attributes(Att,Mod,Var,L0,LF).
expand_get_attributes(-Att,Mod,Var,L0,[attributes:free_att(Var,Key)|L0]) :- !,
	existing_attribute(Att,Mod,Key).
expand_get_attributes(Att,Mod,Var,L0,[attributes:get_att(Var,Key,Att)|L0]) :-
	% searching for an attribute
	existing_attribute(Att,Mod,Key).

get_atts_at_run_time(Var,Atts,Module) :-
	var(Atts), !,
	get_all_atts(Var,LAtts),
	fetch_interesting_attributes(LAtts, Module, Atts).
get_atts_at_run_time(Var,Atts,Module) :-
	expand_get_attributes(Atts,Module,Var,[],GL),
	convert_to_goals(GL,Gs),
	call(Gs).

fetch_interesting_attributes([], _, []).
fetch_interesting_attributes([[I|Att]|LAtts], Module, Atts) :-
	fetch_interesting_attribute(Att, Module, I, Atts, AttsI),
	fetch_interesting_attributes(LAtts, Module, AttsI).

%
% only output attributes if they are for the current module.
%
fetch_interesting_attribute(Att, Module, Key, [Att|Atts], Atts) :-
	existing_attribute(Att, Module, Key), !.
fetch_interesting_attribute(_, _, _, Atts, Atts).

expand_put_attributes(V,Mod,Var,G0,GF) :- var(V), !,
	GF = [attributes:put_atts_at_run_time(Var,V,Mod)|G0].
expand_put_attributes([],_,_,G,G) :- !.
expand_put_attributes([Att|Atts],Mod,Var,G0,GF) :- !,
	expand_put_attributes(Att,Mod,Var,G0,GI),
	expand_put_attributes(Atts,Mod,Var,GI,GF).
expand_put_attributes(+Att,Mod,Var,G0,GF) :- !,
	expand_put_attributes(Att,Mod,Var,G0,GF).
expand_put_attributes(-Att,Mod,Var,G0,[attributes:rm_att(Var,Key)|G0]) :- !,
	existing_attribute(Att,Mod,Key).
expand_put_attributes(Att,Mod,Var,G0,[attributes:put_att(Var,Key,Att)|G0]) :-
	% searching for an attribute
	existing_attribute(Att,Mod,Key).

put_atts_at_run_time(Var,Atts,_) :-
	var(Atts), !,
	throw(error(instantiation_error,put_atts(Var,Atts))).
put_atts_at_run_time(Var,Atts,Module) :-
	expand_put_attributes(Atts,Module,Var,[],GL),
	convert_to_goals(GL,Gs),
	call(Gs).

woken_att_do(AttVar, Binding) :-
	modules_with_attributes(Mods),
	do_verify_attributes(Mods, AttVar, Binding, Goals),
	bind_attvar(AttVar),
	lcall(Goals).

do_verify_attributes([], _, _, []).
do_verify_attributes([Mod|Mods], AttVar, Binding, [Mod:Goal|Goals]) :-
	existing_attribute(_,Mod,Key),
	get_att(AttVar,Key,_),
	current_predicate(verify_attributes, Mod:verify_attributes(_,_,_)), !,
	do_verify_attributes(Mods, AttVar, Binding, Goals),
	Mod:verify_attributes(AttVar, Binding, Goal).
do_verify_attributes([_|Mods], AttVar, Binding, Goals) :-
	do_verify_attributes(Mods, AttVar, Binding, Goals).

lcall([]).
lcall([Mod:Gls|Goals]) :-
	lcall2(Gls,Mod),
	lcall(Goals).

lcall2([], _).
lcall2([Goal|Goals], Mod) :-
	call(Mod:Goal),
	lcall2(Goals, Mod).

convert_att_var(V, Gs) :-
	modules_with_attributes(LMods),
	fetch_att_goals(LMods,V,Gs0), !,
	simplify_trues(Gs0, Gs).
convert_att_var(_, true).

fetch_att_goals([Mod], Att, G1) :-
	call_module_attributes(Mod, Att, G1), !.
fetch_att_goals([_], _, true) :- !.
fetch_att_goals([Mod|LMods], Att, (G1,LGoal)) :-
	call_module_attributes(Mod, Att, G1), !,
	fetch_att_goals(LMods, Att, LGoal).
fetch_att_goals([_|LMods], Att, LGoal) :-
	fetch_att_goals(LMods, Att, LGoal).

%
% if there is an active attribute for this module call attribute_goal.
%
call_module_attributes(Mod, AttV, G1) :-
	existing_attribute(_,Mod,Key),
	get_att(AttV,Key,_), !,
	current_predicate(attribute_goal, Mod:attribute_goal(AttV,G1)),
	Mod:attribute_goal(AttV, G1).

simplify_trues((A,B), NG) :- !,
	simplify_trues(A, NA),
	simplify_trues(B, NB),
	simplify_true(NA, NB, NG).
simplify_trues(G, G).

simplify_true(true, G, G) :- !.
simplify_true(G, true, G) :- !.
simplify_true(A, B, (A,B)).


convert_to_goals([G],G) :- !.
convert_to_goals([A|G],(A,Gs)) :- 
	convert_to_goals(G,Gs).


