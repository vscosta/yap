
:-	 use_module(library(yapi)).
:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(python)).

:- python_import(sys).

jupyter_query(Self, Cell) :-
	setup_call_cleanup(
			   enter_cell(Self),
			   python_query(Self, Cell),
			   exit_cell(Self)
			  ).

enter_cell(_Self) :-
	open('//python/sys.stdout', append, _Output, [alias(jupo)]),
	open('//python/sys.stdout', append, _, [alias(jupe)]),
	set_prolog_flag(user_output, jupo),
	set_prolog_flag(user_error, jupe).

exit_cell(_Self) :-
	close( jupo),
	close( jupe).


completions(S, Self) :-
	open_mem_read_stream(S, St),
	scan_to_list(St, Tokens),
	close(St),
	reverse(Tokens, RTokens),
	strip_final_tokens(RTokens, MyTokens),
	setof( Completion, complete(MyTokens, Completion), Cs),
	Self.completions := Cs.


strip_final_tokens(['EOT'|Ts], Ts) :- !.
strip_final_tokens( Ts, Ts ).

complete( [atom(F)|LibRest], C) :-
	LibRest = [l, atom(Where)|Consult],
	isconsult(Consult, Rest),
	\+ arg( Rest ),
	check_library( F, Where, C).
complete( [atom(F)|Consult], C) :-
	isconsult(Consult, Rest),
	\+ arg( Rest ),
	check_file( F, C).
complete( [atom(F)|Rest], C) :-
	\+ arg( Rest ),
	predicate( F, Pred, Arity ),
	cont( Arity, F, Pred, C).

isconsult( [l, use_module| Rest], Rest).
isconsult( [l, ensure_loaded| Rest], Rest).
isconsult( [l, compile| Rest], Rest).
isconsult( [l, consult| Rest], Rest).
isconsult( [l, reconsult| Rest], Rest).
isconsult( [l, load_files| Rest], Rest).
isconsult( ['-', ']'| Rest], Rest).
isconsult( [']'| Rest], Rest  ).

arg([']'|_]).
arg([l|_]).

check_file(F,C) :-
	atom_concat( F, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(['\'',C0,'\''], C).

check_library( Lib, F, C) :-
	atom_concat( F, '*'	, Pat),
	LibF =.. [Lib(Pat)],
	absolute_file_name( LibF, Lib, [glob(true)]  ),
	file_directory_name( Lib, Name),
	( atom_concat(C, '.yap', Name) -> true ;
	 atom_concat(C, '.ypp', Name) -> true ;
	 atom_concat(C, '.prolog', Name) -> true
	).

predicate(N,P,A) :-
	system_predicate(P0/A),
	atom_concat(N,P,P0).
predicate(N,P,A) :-
	current_predicate(P0/A),
	atom_concat(N,P,P0).

cont(0, F, P, P0)- :-
		atom_concat( F, P, PB ).
cont( _, F, P, PB ):-
	atom_concat( [F, P, '('], PB ).
