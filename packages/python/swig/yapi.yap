%% @file yapi.yap
%% @brief support yap shell
%%
:- module(yapi, [python_query/2,
		 python_ouput/0,
		 yap_query/3]).

:- use_module( library(lists) ).
:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).


%% @pred yap_query(0:Goal, + VarList, +OutStream, - Dictionary)
%% @pred yap_query(0:Goal, + VarList, - Dictionary)
%%
%% dictionary, Examples
%%
%%
python_query( Engine, String ) :-
 	atomic_to_term( String, Goal, VarNames ), writeln(Goal),
	yap_query( Goal, VarNames, user_error, Dict), writeln(Dict),
	Engine.bindings := Dict.

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
	constraints(VarNames, Goal, Stream, Dictionary)
    ).

prolog:yap_query( Goal, VarNames, Dictionary) :-
        yap_query( Goal, VarNames, user_output, Dictionary).

constraints(QVs0, Goal0, Stream, {Dict}) :-
		 !,
		 copy_term(Goal0+QVs0, Goal+QVs),
		 writeln(ivs-IVs),
	term_variables(Goal, IVs),
	foldl(enumerate, IVs, 0, _Ns),
        out(QVs, Stream, Dict).
constraints(_, _, {}) :-
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
	atring_to_atom(V,S),
		!.
bvs([V=B|Ns], (S:B,N) ) :-
	atring_to_atom(V,S),
		output( Ns, N).

python_output :-
	:= import(sys),
	open('//python/sys.stdout', append, Output),
	open('//python/sys.stderr', append, Error),
	set_prolog_flag(user_output, Output),
	set_prolog_flag(user_error, Error).

