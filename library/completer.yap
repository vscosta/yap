/**
 x  @file completer.yap
 *
 *  @brief Prolog completer.
 */

:- module( completer,
	   [
	       completer/3
	   ]).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).


%% completer( +Text, +PythonCell )
%% 
%% Tries to complete the current text. The list with the set of completions
%% is stored in the
%% `matches` field of the python object.
%%

completer( S, InsertPoint, Cs) :-
line2toks(S,Toks),
toks(InsertPoint, S, Info , Toks,  []),
all_expansions(Info, Cs).


toks(InsertPoint, S, info(Type, LeftSide, RightSide, Prefix,  Suffix) ) -->
    prefix(InsertPoint, S, Prefix),
what_to_expand(InsertPoint, S, Suffix, Type, LeftSide, RightSide).

prefix(InsertPoint, S,[Tok|Prefix]) -->
[Tok],
{arg(3,Tok,Beg),
  arg(4,Tok,Size),
  Beg +Size < InsertPoint
  },
!,
prefix(InsertPoint, S,Prefix).
prefix(InsertPoint, S, _) -->
[Tok],
{arg(3,Tok,Beg),
    Beg >= InsertPoint},
!,
{ throw(error(domain(nothing_to_complete,S,InsertPoint ),[])) }.
prefix(_,_,[]) -->
[].

what_to_expand(InsertPoint, S, Toks,  Type, LeftSide, RightSide, [Tok|Toks], Toks) :-
Tok = t(TypeVal,_,Beg,Size),
functor(TypeVal,Type,_),
NSz is InsertPoint-Beg,
    sub_string(S, Beg, NSz, _,LeftSide),
RightSize is Size - NSz,
    sub_string(S, InsertPoint,RightSize,_,RightSide)  .

   
all_expansions(Info, Cs) :-
	setof( Completion, complete(Info, Completion), Cs), writeln(user_error,Cs).

line2toks(S, Tokens) :-
	open_mem_read_stream(S, St),
	scan_stream(St, Tokens),
close(St).


strip_final_tokens([], []) :- !.
strip_final_tokens([t('EOT',_,_,_)|_Ts], []) :- !.
strip_final_tokens( [t(A,_,_,_)|Ts], [A|NTs] )  :-
		  strip_final_tokens(Ts,NTs).

/*
complete([atom, (E),l, atom(C),l,A|_],Completion) :-
	 isconsult(E),
	  library(C,Lib),
	  %D=l,
	check_library( A, Lib, ACompletion),
atom_string(ACompletion, Completion).
complete([atom(E),l,atom(C),l,-,'['|<-],Completion) :-
	 isconsult(E),
	  library(C,Lib),
	check_library( '', Lib, ACompletion),
atom_string(ACompletion, Completion).
complete([atom(C),l,atom(A)|_],Completion) :-
	 isconsult(C),
	file_or_library( A, ACompletion),
atom_string(ACompletion, Completion).
complete([atom(A),l,-,'['|_],Completion) :-
	 isconsult(A),
	file_or_library( A, ACompletion),
atom_string(ACompletion, Completion).
*/
complete( info(atom, F, _, _,_), Completion) :-
string_atom(F,Name),
  predicate( Name, Pred, Arity ),
	cont( Arity, Name, Pred, ACompletion),
atom_string(ACompletion, Completion).

isconsult( atom(use_module) ).
isconsult( atom(ensure_loaded) ).
isconsult( atom(compile) ).
isconsult( atom(consult) ).
isconsult( atom(reconsult) ).
isconsult( atom(load_files) ).
isconsult( '['   ).


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
	LibF =.. [Lib,Pat],
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
	 
	 
