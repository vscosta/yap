
:-	 use_module(library(yapi)).

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
	open(atom(S), read, St),
	scan_to_list(St, Tokens),
	reverse(Tokens, RTokens),
	setof( Completion, complete(RTokens, Completion), Cs),
	Self.completions := Cs.

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
	cont( Arity, Pred, C).

isconsult( [l, use_module| _Rest]).
isconsult( [l, ensure_loaded| _Rest]).
isconsult( [l, compile| _Rest]).
isconsult( [l, consult| _Rest]).
isconsult( [l, reconsult| _Rest]).
isconsult( [l, load_files| _Rest]).
isconsult( ['-', ']'| _Rest]).
isconsult( [']'| _Rest]).

arg(([']'|_]).
arg(([l|_]).

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

cont(0, P, P).
cont( _, P, PB ):-
	atom_concat( P, '(', PB ).
