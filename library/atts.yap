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

:- module(attributes, [op(1150, fx, attribute)]).

:- multifile
	user:goal_expansion/3.
:- multifile
	user:term_expansion/2.

:- dynamic existing_attribute/4.
:- dynamic modules_with_attributes/1.
:- dynamic attributed_module/3.

modules_with_attributes([prolog]).

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
	existing_attribute(S,Mod,_,_) , !.
new_attribute(Na/Ar) :-
	source_module(Mod),
	functor(S,Na,Ar),
	store_new_module(Mod,Ar,Position),
	assertz(existing_attribute(S,Mod,Ar,Position)).

existing_attribute(delay(_),prolog,1,2).

store_new_module(Mod,Ar,ArgPosition) :-
	(
	  retract(attributed_module(Mod,Position,_))
	->
	  true
	;
	  retract(modules_with_attributes(Mods)),
	  assert(modules_with_attributes([Mod|Mods])), Position = 1
	),
	ArgPosition is Position+1,
	( Ar == 0 -> NOfAtts is Position+1 ; NOfAtts is Position+Ar),
	functor(AccessTerm,Mod,NOfAtts),
	assertz(attributed_module(Mod,NOfAtts,AccessTerm)).
	
:- user_defined_directive(attribute(G), attributes:new_attribute(G)).

user:goal_expansion(get_atts(Var,AccessSpec), Mod, Goal) :-
	expand_get_attributes(AccessSpec,Mod,Var,Goal).
user:goal_expansion(put_atts(Var,AccessSpec), Mod, Goal) :-
	expand_put_attributes(AccessSpec, Mod, Var, Goal).


expand_get_attributes(V,_,_,_) :- var(V), !, fail.
expand_get_attributes([],_,_,true) :- !.
expand_get_attributes([-G1],Mod,V,attributes:free_att(V,Mod,Pos)) :-
	existing_attribute(G1,Mod,_,Pos), !.
expand_get_attributes([+G1],Mod,V,attributes:get_att(V,Mod,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	arg(1,G1,A).
expand_get_attributes([G1],Mod,V,attributes:get_att(V,Mod,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	arg(1,G1,A).
expand_get_attributes(Atts,Mod,Var,attributes:get_module_atts(Var,AccessTerm)) :- Atts = [_|_], !,
	attributed_module(Mod,NOfAtts,AccessTerm),
	void_term(Void),
	cvt_atts(Atts,Mod,Void,LAtts),
	sort(LAtts,SortedLAtts),
	free_term(Free),
	build_att_term(1,NOfAtts,SortedLAtts,Free,AccessTerm).
expand_get_attributes(Att,Mod,Var,Goal) :- 
	expand_get_attributes([Att],Mod,Var,Goal).

build_att_term(NOfAtts,NOfAtts,[],_,_) :- !.
build_att_term(I0,NOfAtts,[I-Info|SortedLAtts],Void,AccessTerm) :-
	I is I0+1, !,
	copy_att_args(Info,I0,NI,AccessTerm),
	build_att_term(NI,NOfAtts,SortedLAtts,Void,AccessTerm).
build_att_term(I0,NOfAtts,SortedLAtts,Void,AccessTerm) :-
	I is I0+1,
	arg(I,AccessTerm,Void),
	build_att_term(I,NOfAtts,SortedLAtts,Void,AccessTerm).

cvt_atts(V,_,_,_) :- var(V), !, fail.
cvt_atts([],_,_,[]).
cvt_atts([V|_],_,_,_) :- var(V), !, fail.
cvt_atts([+Att|Atts],Mod,Void,[Pos-LAtts|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(atom(Att) -> LAtts = [_] ; Att=..[_|LAtts]),
	cvt_atts(Atts,Mod,Void,Read).
cvt_atts([-Att|Atts],Mod,Void,[Pos-LVoids|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(
	  atom(Att)
	->
	  LVoids = [Void]
	;
	  Att =..[_|LAtts],
	  void_vars(LAtts,Void,LVoids)
	),	  
	cvt_atts(Atts,Mod,Void,Read).
cvt_atts([Att|Atts],Mod,Void,[Pos-LAtts|Read]) :- !,
	existing_attribute(Att,Mod,_,Pos),
	(atom(Att) -> LAtts = [_] ; Att=..[_|LAtts]),
	cvt_atts(Atts,Mod,Void,Read).

copy_att_args([],I,I,_).
copy_att_args([V|Info],I,NI,AccessTerm) :-
	I1 is I+1,
	arg(I1,AccessTerm,V),
	copy_att_args(Info,I1,NI,AccessTerm).

void_vars([],_,[]).
void_vars([_|LAtts],Void,[Void|LVoids]) :-
	void_vars(LAtts,Void,LVoids).

expand_put_attributes(V,_,_,_) :- var(V), !, fail.
expand_put_attributes([-G1],Mod,V,attributes:rm_att(V,Mod,NOfAtts,Pos)) :-
	existing_attribute(G1,Mod,_,Pos), !,
	attributed_module(Mod,NOfAtts,_).
expand_put_attributes([+G1],Mod,V,attributes:put_att(V,Mod,NOfAtts,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	attributed_module(Mod,NOfAtts,_),
	arg(1,G1,A).
expand_put_attributes([G1],Mod,V,attributes:put_att(V,Mod,NOfAtts,Pos,A)) :-
	existing_attribute(G1,Mod,1,Pos), !,
	attributed_module(Mod,NOfAtts,_),
	arg(1,G1,A).
expand_put_attributes(Atts,Mod,Var,attributes:put_module_atts(Var,AccessTerm)) :- Atts = [_|_], !,
	attributed_module(Mod,NOfAtts,AccessTerm),
	void_term(Void),
	cvt_atts(Atts,Mod,Void,LAtts),
	sort(LAtts,SortedLAtts),
	free_term(Free),
	build_att_term(1,NOfAtts,SortedLAtts,Free,AccessTerm).
expand_put_attributes(Att,Mod,Var,Goal) :- 
	expand_put_attributes([Att],Mod,Var,Goal).

woken_att_do(AttVar, Binding) :-
	get_all_swi_atts(AttVar,SWIAtts),
	modules_with_attributes(AttVar,Mods0),
	modules_with_attributes(Mods),
	find_used(Mods,Mods0,[],ModsI),
	do_verify_attributes(ModsI, AttVar, Binding, Goals),
	process_goals(Goals, NGoals, DoNotBind),
	( DoNotBind == true
	->
	  unbind_attvar(AttVar)
	;
	  bind_attvar(AttVar)
	),
	do_hook_attributes(SWIAtts, Binding),
	lcall(NGoals).

% dirty trick to be able to unbind a variable that has been constrained.
process_goals([], [], _).
process_goals((M:do_not_bind_variable(Gs)).Goals, (M:Gs).NGoals, true) :- !,
	process_goals(Goals, NGoals, _).
process_goals(G.Goals, G.NGoals, Do) :-
	process_goals(Goals, NGoals, Do).

find_used([],_,L,L).
find_used([M|Mods],Mods0,L0,Lf) :-
        in(M,Mods0), !,
	find_used(Mods,Mods0,[M|L0],Lf).
find_used([_|Mods],Mods0,L0,Lf) :-
	find_used(Mods,Mods0,L0,Lf).

in(X,[X|_]).
in(X,[_|L]) :-
	in(X,L).

do_verify_attributes([], _, _, []).
do_verify_attributes([Mod|Mods], AttVar, Binding, [Mod:Goal|Goals]) :-
	current_predicate(verify_attributes,Mod:verify_attributes(_,_,_)), !,
	Mod:verify_attributes(AttVar, Binding, Goal),
	do_verify_attributes(Mods, AttVar, Binding, Goals).
do_verify_attributes([_|Mods], AttVar, Binding, Goals) :-
	do_verify_attributes(Mods, AttVar, Binding, Goals).

do_hook_attributes([], _).
do_hook_attributes(att(Mod,Att,Atts), Binding) :-
	current_predicate(attr_unify_hook,Mod:attr_unify_hook(_,_)),
	!,
	Mod:attr_unify_hook(Att, Binding),
	do_hook_attributes(Atts, Binding).
do_hook_attributes(att(_,_,Atts), Binding) :-
	do_hook_attributes(Atts, Binding).

lcall([]).
lcall([Mod:Gls|Goals]) :-
	lcall2(Gls,Mod),
	lcall(Goals).

lcall2([], _).
lcall2([Goal|Goals], Mod) :-
	call(Mod:Goal),
	lcall2(Goals, Mod).

convert_att_var(V, Gs) :-
	modules_with_attributes(V,LMods),
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



	