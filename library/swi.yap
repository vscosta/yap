
:- module(swi, [
	absolute_file_name/3,
	concat_atom/3,
	setenv/2,
	nth1/3,
	forall/2,
	between/3,
	concat_atom/2]).

:- meta_predicate forall(+,:).

:- load_foreign_files([yap2swi], [], swi_install).

:- use_module(library(lists)).

absolute_file_name(jar(File), Opts, Path) :- !,
	absolute_file_name(library(File), Path).
absolute_file_name(library(File), Opts, Path) :- !,
	absolute_file_name(library(File), Path).
absolute_file_name(File, Opts, Path) :-
	absolute_file_name(File, Path).


concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).

concat_atom(List, New) :-
	atomic_concat(List, New).

add_separator_to_list([], _, []).
add_separator_to_list([T], _, [T]) :- !.
add_separator_to_list([H|T], Separator, [H,Separator|NT]) :-
	add_separator_to_list(T, Separator, NT).


setenv(X,Y) :- unix(putenv(X,Y)).

nth1(I,L,A) :- nth(I,L,A).

forall(X,Y) :- 
	catch(do_forall(X,Y), fail_forall, fail).

do_forall(X,Y) :-
	call(X),
	do_for_forall(Y).
do_forall(_,_).

do_for_forall(Y) :- call(Y), !, fail.
do_for_forall(Y) :- throw(fail_forall).

between(I,_,I).
between(I0,I,J) :- I0 < I, 
	I1 is I0+1,
	between(I1,I,J).

