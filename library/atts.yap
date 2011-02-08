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

:- use_module(library(lists), [member/2]).

:- multifile
	user:goal_expansion/3.
:- multifile
	user:term_expansion/2.

:- dynamic existing_attribute/4.
:- dynamic modules_with_attributes/1.
:- dynamic attributed_module/3.

modules_with_attributes([]).

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

store_new_module(Mod,Ar,ArgPosition) :-
	(
	  retract(attributed_module(Mod,Position,_))
	->
	  true
	;
	  retract(modules_with_attributes(Mods)),
	  assert(modules_with_attributes([Mod|Mods])), Position = 2
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

woken_att_do(AttVar, Binding, NGoals, DoNotBind) :-
	modules_with_attributes(AttVar,Mods0),
	modules_with_attributes(Mods),
	find_used(Mods,Mods0,[],ModsI),
	do_verify_attributes(ModsI, AttVar, Binding, Goals),
	process_goals(Goals, NGoals, DoNotBind).

% dirty trick to be able to unbind a variable that has been constrained.
process_goals([], [], _).
process_goals((M:do_not_bind_variable(Gs)).Goals, (M:Gs).NGoals, true) :- !,
	process_goals(Goals, NGoals, _).
process_goals(G.Goals, G.NGoals, Do) :-
	process_goals(Goals, NGoals, Do).

find_used([],_,L,L).
find_used([M|Mods],Mods0,L0,Lf) :-
        member(M,Mods0), !,
	find_used(Mods,Mods0,[M|L0],Lf).
find_used([_|Mods],Mods0,L0,Lf) :-
	find_used(Mods,Mods0,L0,Lf).

do_verify_attributes([], _, _, []).
do_verify_attributes([Mod|Mods], AttVar, Binding, [Mod:Goal|Goals]) :-
	current_predicate(verify_attributes,Mod:verify_attributes(_,_,_)), !,
	Mod:verify_attributes(AttVar, Binding, Goal),
	do_verify_attributes(Mods, AttVar, Binding, Goals).
do_verify_attributes([_|Mods], AttVar, Binding, Goals) :-
	do_verify_attributes(Mods, AttVar, Binding, Goals).


	