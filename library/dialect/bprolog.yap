
:- set_prolog_flag(dollar_as_lower_case,on).

:- use_module(library(lists)).
:- use_module(library(hacks),[
     current_choicepoint/1,
     cut_by/1]).
:- use_module(library(terms)).
:- use_module(library(system)).

:- ensure_loaded(bprolog/arrays).
:- ensure_loaded(bprolog/hashtable).

%:- ensure_loaded(bprolog/actionrules).
:- ensure_loaded(bprolog/foreach).
%:- ensure_loaded(bprolog/compile_foreach).

:- op(700, xfx, [?=]).
:- op(200, fx, (@)).

X ?= Y :- unifiable(X,Y,_).

global_set(F,N,Value) :-
	atomic_concat([F,'/',N],Key),
	nb_setval(Key, Value).

global_set(F,Value) :-
	atom_concat([F,'/0'],Key),
	nb_setval(Key, Value).

global_get(F,Arity,Value) :-
	atomic_concat([F,'/',Arity],Key),
	nb_getval(Key, Value).

global_get(F,Value) :-
	atom_concat([F,'/0'],Key),
	nb_getval(Key, Value).

global_del(F,Arity) :-
	atomic_concat([F,'/',Arity],Key),
	catch(nb_delete(Key),_,true).

global_del(F) :-
	atom_concat([F,'/0'],Key),
	catch(nb_delete(Key),_,true).

getclauses1(File, Prog, _Opts) :-
	findall(Clause, '$bpe_get_clause_from_file'(File, Clause), Prog0),
	'$bpe_get_preds'(Prog0, Prog).

'$bpe_open_file'(File, Dir, S) :-
	absolute_file_name(File, Abs, [expand(true),access(read)]),
	file_directory_name(Abs, Dir),
	open(Abs, read, S).

'$bpe_get_clause_from_file'(File, Clause) :-
	'$bpe_open_file'(File, Dir, S),
	working_directory(Old, Dir),
        repeat,
	read(S, Clause0),
	( Clause0 = end_of_file ->
	   !,
	   working_directory(Dir, Old),
	   fail
         ;
	   %ugh, but we have to process include directives on the spot...
	   Clause0 = (:- include(Include))
         ->
	   '$bpe_get_clause_from_file'(Include, Clause)
         ;
	   Clause = Clause0
         ).

'$bpe_get_preds'(Decl.Prog0, pred(F,N,Modes,Delay,Tabled,Cls).NProg) :-
        '$get_pred'(Decl, F, N, Modes,Delay, Tabled, Cls, Cls0), !,
	'$bpe_process_pred'(Prog0, F,N,Modes,Delay,Tabled, Cls0, ProgF, []),
        '$bpe_get_preds'(ProgF, NProg).
'$bpe_get_preds'(_Decl.Prog0, NProg) :-
	'$bpe_get_preds'(Prog0, NProg).
'$bpe_get_preds'([], []).

'$bpe_process_pred'([], _F, N, Mode, _Delay, _Tabled, []) -->
	{ '$init_mode'(N, Mode) }.
'$bpe_process_pred'([Call|Prog0], F,N, Modes, Delay, Tabled, Cls0)  -->
	{ '$get_pred'(Call, F, N, Modes, Delay, Tabled, Cls0, ClsI) }, !,
	'$bpe_process_pred'(Prog0, F, N, Modes, Delay, Tabled, ClsI).
'$bpe_process_pred'([Call|Prog0], F, N, Modes, Delay, Tabled, Cls0) -->
	[ Call ],
	'$bpe_process_pred'(Prog0, F,N,Modes,Delay,Tabled, Cls0).

'$init_mode'(_N, Mode) :- nonvar(Mode), !.
'$init_mode'(0, []) :- !.
'$init_mode'(I, [d|Mode]) :- !,
	I0 is I-1,
	'$init_mode'(I0, Mode).

'$get_pred'((P :- Q), F, N, _Modes, _Delay, _Tabled) -->
         { functor(P, F, N), ! },
	 [(P:-Q)].
'$get_pred'((:- mode Q), F, N, Modes, _Delay, _Tabled) -->
         { functor(Q, F, N), !, Q =.. [_|Modes0],
	   '$bpe_cvt_modes'(Modes0, Modes, [])
         },
	 [].
%'$get_pred'((:- table _), F, N, Modes, Delay, Tabled) -->
%         { functor(Q, F, N), !, Q =.. [_|Modes] },
%	 [].
'$get_pred'((:- Q), '$damon_load', 0, _Modes, _Delay, _Tabled) --> 
	[ ('$damon_load' :- '$query'( Q ) )].
'$get_pred'((P), F, N, _Modes, _Delay, _Tabled) -->
         { functor(P, F, N), ! },
	 [(P)].


'$bpe_cvt_modes'([Mode|Modes0]) --> [NewMode],
	{ '$bpe_cvt_mode'(Mode, NewMode) },
	'$bpe_cvt_modes'(Modes0).
'$bpe_cvt_modes'([]) --> [].

'$bpe_cvt_mode'(Mode, Mode).

list_to_and([], true).
list_to_and([G], G).
list_to_and([G1,G2|Gs], (G1, NGs)) :-
	list_to_and([G2|Gs], NGs).

preprocess_cl(Cl, Cl, _, _, _, _).

phase_1_process(Prog, Prog).

compileProgToFile(_, _File, []).
compileProgToFile(_, File, [Pred|Prog2]) :-
	consult_pred(Pred),
	compileProgToFile(_, File, Prog2).

consult_preds([], L) :- !,
	consult_preds(L).
consult_preds(L0, L) :-
	writeln(consult_preds(L0,L)).

consult_preds([]).
consult_preds([P|L]) :-
	consult_pred(P),
	consult_preds(L).

consult_pred(pred(F,N,_Mode,_Delay,Tabled,Clauses)) :-
	(nonvar(Tabled) -> table(F/N) ; true),
	functor(S,F,N),
	assert(b_IS_CONSULTED_c(S)),
	abolish(F/N),
	'$assert_clauses'(Clauses).

add_pred(Name, Arity, _Mode, _Delay, Tabled, Clauses) :-
	'$assert_clauses'(Clauses).

'$assert_clauses'([]).
'$assert_clauses'([Cl|Clauses]) :-
	assert_static(Cl),
	'$assert_clauses'(Clauses).

'$myload'(_F) :-
	'$damon_load'.

'$query'(G) :- call(G).

initialize_table :- abolish_all_tables.

:- dynamic b_IS_DEBUG_MODE/0.

'_$savecp'(B) :- current_choicepoint(B).
'_$cutto'(B) :- cut_by(B).

X <= Y :- subsumes_chk(Y,X).

cputime(X) :- statistics(cputime,[X,_]).

vars_set(Term, Vars) :-
	term_variables(Term, Vars).

sort(=<, L, R) :-
	length(L, N), 
	'$bp_sort'(@=<, N, L, _, R1), !, 
	R = R1.
sort(>=, L, R) :-
	length(L, N), 
	'$bp_sort'(@>=, N, L, _, R1), !, 
	R = R1.
sort(<, L, R) :-
	length(L, N), 
	'$bp_sort2'(@<, N, L, _, R1), !, 
	R = R1.
sort(>, L, R) :-
	length(L, N), 
	'$bp_sort2'(@>, N, L, _, R1), !, 
	R = R1.

'$bp_sort'(P, 2, [X1, X2|L], L, R) :- !, 
	(
	    call(P, X1, X2) ->
	    R = [X1,X2]
	;
	    R = [X2,X1]
	).
'$bp_sort'(_, 1, [X|L], L, [X]) :- !.
'$bp_sort'(_, 0, L, L, []) :- !.
'$bp_sort'(P, N, L1, L3, R) :-
	N1 is N // 2, 
	plus(N1, N2, N), 
	'$bp_sort'(P, N1, L1, L2, R1), 
	'$bp_sort'(P, N2, L2, L3, R2), 
	'$bp_predmerge'(P, R1, R2, R).

'$bp_predmerge'(_, [], R, R) :- !.
'$bp_predmerge'(_, R, [], R) :- !.
'$bp_predmerge'(P, [H1|T1], [H2|T2], [H1|Result]) :-
	call(P, H1, H2), !,
	'$bp_predmerge'(P, T1, [H2|T2], Result).
'$bp_predmerge'(P, [H1|T1], [H2|T2], [H2|Result]) :-
	'$bp_predmerge'(P, [H1|T1], T2, Result).

'$bp_sort2'(P, 2, [X1, X2|L], L, R) :- !, 
	(
	    call(P, X1, X2) ->
	    R = [X1,X2]
	;
	    X1 == X2
	->
	    R = [X1]
	;
	    R = [X2,X1]
	).
'$bp_sort2'(_, 1, [X|L], L, [X]) :- !.
'$bp_sort2'(_, 0, L, L, []) :- !.
'$bp_sort2'(P, N, L1, L3, R) :-
	N1 is N // 2, 
	plus(N1, N2, N), 
	'$bp_sort'(P, N1, L1, L2, R1), 
	'$bp_sort'(P, N2, L2, L3, R2), 
	'$bp_predmerge'(P, R1, R2, R).

'$bp_predmerge2'(_, [], R, R) :- !.
'$bp_predmerge2'(_, R, [], R) :- !.
'$bp_predmerge2'(P, [H1|T1], [H2|T2], [H1|Result]) :-
	call(P, H1, H2), !,
	'$bp_predmerge'(P, T1, [H2|T2], Result).
'$bp_predmerge2'(P, [H1|T1], [H2|T2], [H1|Result]) :-
	H1 == H2, !,
	'$bp_predmerge'(P, T1, T2, Result).
'$bp_predmerge2'(P, [H1|T1], [H2|T2], [H2|Result]) :-
	'$bp_predmerge'(P, [H1|T1], T2, Result).
