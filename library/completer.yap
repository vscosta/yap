/**
 x  @file completer.yap
 *
 *  @brief Prolog completer.
 */

:- module( completer,
	   [
	       completer/2
	   ]).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(python)).

%% completer( +Text, +PythonCell )
% 
% Tries to complete the current text. The list with the set of completions
% is stored in the
% `matches` field of the python object.
%
completer(S, Cs) :-
	open_mem_read_stream(S, St),
	scan_stream(St, Tokens),
	close(St),
	strip_final_tokens(Tokens, MyTokens),
	setof( Completion, complete(MyTokens, Completion), Cs).


strip_final_tokens([], []) :- !.
strip_final_tokens([t('EOT',_,_,_)|_Ts], []) :- !.
strip_final_tokens( [t(A,_,_,_)|Ts], [A|NTs] )  :-
		  strip_final_tokens(Ts,NTs).

complete([atom(E),l, atom(C),l,A|_],Completion) :-
	 isconsult(E),
	  library(C,Lib),
	  %D=l,
	check_library( A, Lib, Completion).
complete([atom(E),l,atom(C),l,-,'['|<-],Completion) :-
	 isconsult(E),
	  library(C,Lib),
	check_library( '', Lib, Completion).
complete([atom(C),l,atom(A)|_],Completion) :-
	 isconsult(C),
	file_or_library( A, Completion).
complete([atom(A),l,-,'['|_],Completion) :-
	 isconsult(A),
	file_or_library( A, Completion).
complete( [atom(F)|_], Completion) :-
	predicate( F, Pred, Arity ),
	cont( Arity, F, Pred, Completion).

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
	library(F,C).
file_or_library(F,C) :-
	check_file(F,C).

check_file(F0,C) :-
	atom_concat('\'',F,F0),
	!,
	absolute_file_name( F, FF, [access(none)]  ),
	atom_concat( FF, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(Pat,C00,C0),
	atom_concat(C00,'\'',C).
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
	atom_concat( [F, P, '( '], PB ).

library(Library,Lib) :-
	 user:file_search_path(Library,Dir),
	 list_directory(Dir,Candidates),
	 member(Entry,Candidates),
	 (
	 Lib = Entry
	 ;
	 user:prolog_file_type(Suffix,prolog),
	 sub_atom(Entry,_,_,N,Suffix),
	 N1 is N-1,
	 sub_atom(Entry,0 ,_,N1,Lib)
	 ).
	 
	 
