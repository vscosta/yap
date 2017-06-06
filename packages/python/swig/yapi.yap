%% @file yapi.yap
%% @brief support yap shell
%%
:- module(yapi, [python_query/2,
		 python_ouput/0,
		 show_answer/2,
		 show_answer/3,
		 yap_query/4,
		 yapi_query/2]).

:- use_module( library(lists) ).
:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).
:- use_module( library(terms) ).
:- use_module( library(python) ).


%% @pred yap_query(sGoal, + VarList, +OutStream, - Dictionary)
%% @pred yap_query(0:Goal, + VarList, - Dictionary)
%%
%% dictionary, Examples
%%
%%
python_query( String, D ) :-
 	atomic_to_term( String, Goal, VarNames ),
	yap_query( Goal, VarNames, user_error, Dict),
	D := Dict,
	yap4py.yapi.bindings := Dict.

	%% @pred yapi_query( + VarList, - Dictionary)
	%%
	%% dictionary, Examples
	%%
	%%
	yapi_query( VarNames, Dict ) :-
		show_answer(VarNames, Dict).



%% @pred yap_query(0:Goal, + VarList, +OutStream, - Dictionary)
%% @pred yap_query(0:Goal, + VarList, - Dictionary)
%%
%% dictionary, Examples
%%
%%
yap_query( Goal, VarNames, Stream, Dictionary) :-
	(
	 call(Goal)
        *->
	 !,
	 show_answer(VarNames,  Stream, Dictionary)
	).

yap_query(  VarNames, Dictionary) :-
        yap_query( VarNames, user_output, Dictionary).

show_answer(QVs0,  Dict) :-
	show_answer(QVs0,  user_error, Dict).

show_answer(QVs0, Stream, Dict) :-
	copy_term(QVs0, QVs),
	copy_term(QVs0, QVs1),
	rb_new(RB),
	foldl2(bind_qv, QVs, QVs1, [], LP, {}-RB, Dict-_),
	!,
	term_variables(QVs, IVs),
	term_variables(QVs1, IVs1),
	foldl( enumerate, IVs, IVs1, 1,  _ ),
        out(LP, Stream ).
show_answer(_, _, {}) :-
	format(' yes.~n', [] ).

bind_qv(V=V0, V1 = V01, Vs, Vs, Vs1-RB, Vs1-RB) :-
	var(V0),
	!,
	'$VAR'(V) = V0,
	V1 = V01.
%	atom_string(V1, V01). 
bind_qv(V='$VAR'(Vi), V1=S1, Vs, [V='$VAR'(Vi)|Vs], D0-RB, D-RB) :- !,
	add2dict(D0, V1:S1, D).
bind_qv(V=S, V1=S1, Vs, [V=S|Vs], D0-RB0, D-RB0) :-
%	fix_tree( S, SS, S1, SS1, RB0, RBT),
	add2dict(D0, V1:S1, D).


add2dict({}, B, {B}).
add2dict({C}, B, {B,C}).

enumerate('$VAR'(A), A, I, I1) :-
	enum(I, Chars),
	atom_codes(A,[0'_|Chars]),
	I1 is I + 1.

enum(I, [C]) :-
	I < 26,
    !,
    C is "A" + I.
enum(I, [C|Cs]) :-
	J is I//26,
	K is I mod 26,
	C is "A" +K,
	enum(J, Cs).

out(Bs, S) :-
	output(Bs, S),
	!.
out([_|Bs], S) :-
	out(Bs, S).

output([V=B], S) :-
	!,
	format(S, '~a = ~q~n', [V, B]).
output([V=B|_Ns], S) :-
	format( S, '~a = ~q.~n', [V, B]),
	fail.



