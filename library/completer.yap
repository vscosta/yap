/**
 x  @file complete.yap
 *
 *  @brief Prolog completer.
 */

:- module( completer,
	   [
	       completions/2
	   ]).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(python)).

%% completions( +Text, +PythonCell )
% 
% Tries to complete the current text. The list with the set of completions
% is stored in the
% `matches` field of the python object.
%
completions(S, Self) :-
	open_mem_read_stream(S, St),
	scan_to_list(St, Tokens),
	close(St),
	reverse(Tokens, RTokens),
	strip_final_tokens(RTokens, MyTokens),
	setof( Completion, complete(MyTokens, Completion), Cs),
	Self.matches := Cs.


strip_final_tokens(['EOT'|Ts], Ts) :- !.
strip_final_tokens( Ts, Ts ).

complete([E,l,C,l,A|More],
	 isconsult(A),
	  %B = l,
	  library(C,Lib),
	  %D=l,
	  E=atom(Prefix),
	\+ arg( Rest ),
	check_library( Prefix, Lib, C).
complete([E,l,C,l,-,'['|More],
	 isconsult(A),
	  %B = l,
	  library(C,Lib),
	  %D=l,
	  E=atom(Prefix),
	\+ arg( Rest ),
	check_library( Prefix, Lib, C).
complete([C,l,A|More],
	 isconsult(A),
	  %B = l,
	  C=atom(Prefix),
	\+ arg( Rest ),
	file_or_library( Prefix, C).
complete([C,l,-,'['|More],
	 isconsult(A),
	  %B = l,
	  C=atom(Prefix),
	\+ arg( Rest ),
	file_or_library( Prefix, C).
complete( [atom(F)|Rest], C) :-
	\+ arg( Rest ),
	predicate( F, Pred, Arity ),
	cont( Arity, F, Pred, C).

isconsult( atom(use_module) ).
isconsult( atom(ensure_loaded) ).
isconsult( atom(compile) ).
isconsult( atom(consult) ).
isconsult( atom(reconsult) ).
isconsult( atom(load_files) ).
isconsult( '['   ).

arg([']'|_]).
arg([l|_]).

file_or_library(F,C) :-
	libsym(C0),
	atom_cooncat(F,C,C0).
file_or_library(F,C) :-
	check_file(F,C).

check_file(F0,C) :-
	atom_concat('\'',F,F0),
	!,
	absolute_file_name( F, FF, [access(none)]  ),
	atom_concat( FF, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(Pat,C00,C0),
	atom_conct(C00,'\'',C).
check_file(F0,C) :-
	atom_concat( F0, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(Pat,C,C0).

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

cont(0, F, P, P0) :-
		atom_concat( F, P, P0 ).
cont( _, F, P, PB ):-
	atom_concat( [F, P, '(  )'], PB ).





