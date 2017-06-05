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


							%% @pred yap_query(sGoal, + VarList, +OutStream, - Dictionary)
%% @pred yap_query(0:Goal, + VarList, - Dictionary)
%%
%% dictionary, Examples
%%
%%
python_query( Engine, String, Dict ) :-
 	atomic_to_term( String, Goal, VarNames ),
	yap_query( Goal, VarNames, user_error, Dict).

	%% @pred yapi_query( + VarList, - Dictionary)
	%%
	%% dictionary, Examples
	%%
	%%
	prologun:yapi_query( VarNames, Dict ) :-
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

yapi_query(  VarNames, Dictionary) :-
        yap_query( VarNames, user_output, Dictionary).

show_answer(QVs0,  Dict) :-
		show_answer(QVs0,  user_error, Dict).

show_answer(QVs0,  Stream, {Dict}) :-
	copy_term(QVs0, QVs),
		 writeln(ivs-IVs),
	term_variables(Goal, IVs),
	foldl(enumerate, IVs, 0, _Ns),
	!,
        out(QVs, Stream, D).
		Dictt := {D}.
show_answer(_, _, {}) :-
     format(' yes.~n', [] ).

bind_qv(V=V0, Vs, Vs) :- var(V0), !, V0='$VAR'(V).
bind_qv(V=V, Vs, Vs) :- !.
bind_qv(V=S, Vs, [V=S|Vs]).

enumerate('$VAR'(A), I, I1) :-
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

out(Bs, S, _Dict) :-
	output(Bs, S),
	fail.
out(Bs, _S, Dict) :-
	bvs(Bs, Dict).

v2py(v(I0) = _V, I0, I) :-
	!,
	I is I0+1.
v2py(v(I0) = v(I0), I0, I) :-
    I is I0+1.

output([V=B], S) :-
	!,
	format(S, '~a = ~q~n', [V, B]).
output([V=B|Ns], S) :-
		format( S, '~a = ~q.~n', [V, B]),
		output( Ns, S).

bvs([V=B], S:B) :-
	atom_atring(V,S),
		!.
bvs([V=B|Ns], (S:B,N) ) :-
	atom_string(V,S),
	output( Ns, N).

	bindvars( L, NL ) :-
		rb_new(T),
		foldl2( bind, L, NL, T, _ , 0, _),
		term_variables(NL, Vs),
		foldl( bind_new, Vs, 0, _).


	bind(X=Y, X=X, T0, T, N, N) :-
		var(Y),
		!,
		rb_update(T0, Y, X, T).
	bind(X = G, X = G, T, T, N0, N0) :-
		ground(G),
		!.
	bind(X = C, X = NC, T, NT, N0, NF) :-
	 	C =.. [N|L],
		foldl2( bind_new, L, NL, T, NT, N0, NF),
		NC =.. [N|NL].

	bind_new(Y, X, T, T, N, N) :-
		var(Y),
		rb_lookup(Y, X, T),
		!.
	bind_new(Y, X, T, TN, N, NF) :-
		var(Y),
		!,
		rb_insert(Y, T, X, TN),
		NF is N+1,
		atomic_concat('_',N,X).
	bind_new(Y, Y, T, T, N, N) :-
		ground(Y),
		!.
	bind_new(Y, X, T, NT, N0, NF) :-
	 	Y =.. [N|L],
		foldl2(v, L, NL, T, NT, N0, NF),
		X =.. [N|NL].
