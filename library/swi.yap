
:- module(swi, [
		absolute_file_name/3,
		concat_atom/3,
		setenv/2,
		nth1/3,
		forall/2,
		between/3,
		term_to_atom/2,
		concat_atom/2,
		volatile/1,
		b_setval/2,
		b_getval/2,
		nb_setval/2,
		nb_getval/2,
		nb_current/2,
		nb_delete/1]).


:- use_module(library(charsio),[write_to_chars/2,read_from_chars/2]).

:- use_module(library(lists),[nth/3]).

:- multifile user:file_search_path/2.

:- dynamic user:file_search_path/2.

user:file_search_path(swi, Home) :-
        current_prolog_flag(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :-
        current_prolog_flag(arch, Arch),
        atom_concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(lib)).

%
% maybe a good idea to eventually support this in YAP.
% but for now just ignore it.
%
:- meta_predicate volatile(:).

:- op(1150, fx, 'volatile').

volatile(P) :- var(P),
	throw(error(instantiation_error,volatile(P))).
volatile(M:P) :-
	do_volatile(P,M).
volatile((G1,G2)) :-
	volatile(G1),
	volatile(G2).
volatile(P) :-
	do_volatile(P,_).

do_volatile(_,_).

:- meta_predicate forall(+,:).

:- load_foreign_files([yap2swi], [], swi_install).

:- use_module(library(lists)).

absolute_file_name(jar(File), _Opts, Path) :- !,
	absolute_file_name(library(File), Path).
absolute_file_name(library(File), _Opts, Path) :- !,
	absolute_file_name(library(File), Path).
absolute_file_name(File, _Opts, Path) :-
	absolute_file_name(File, Path).


term_to_atom(Term,Atom) :-
	nonvar(Atom), !,
	atom_codes(Atom,S),
	read_from_chars(S,Term).
term_to_atom(Term,Atom) :-
	write_to_chars(Term,S),
	atom_codes(Atom,S).

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
do_for_forall(_) :- throw(fail_forall).

between(I,_,I).
between(I0,I,J) :- I0 < I, 
	I1 is I0+1,
	between(I1,I,J).

b_getval(GlobalVariable,Value) :-
	array_element(GlobalVariable,0,Value).

b_setval(GlobalVariable,Value) :-
	array(GlobalVariable,1),
	update_array(GlobalVariable,0,Value).

nb_getval(GlobalVariable,Value) :-
	array_element(GlobalVariable,0,Value).

nb_setval(GlobalVariable,Value) :-
	static_array(GlobalVariable,1,term),
	update_array(GlobalVariable,0,Value).

nb_delete(GlobalVariable) :-
	close_static_array(GlobalVariable).

nb_current(GlobalVariable,Val) :-
	static_array_properties(GlobalVariable,1,term),
	array_element(GlobalVariable,0,Val).

