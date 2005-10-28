
:- source.

:- style_check(all).

:- yap_flag(unknown,error).

% redefines stuff in prolog module.

:- module(swi, []).

:- ensure_loaded(library(atts)).

:- use_module(library(charsio),[write_to_chars/2,read_from_chars/2]).

:- use_module(library(lists),[nth/3]).

:- use_module(library(system),[datime/1,
   mktime/2]).

:- use_module(library(terms),[term_variables/2,
			      term_variables/3,
			      term_hash/2]).

:- multifile
   prolog:message/3.

:- multifile
   user:file_search_path/2.

:- dynamic
   user:file_search_path/2.

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
:- meta_predicate prolog:volatile(:).

:- op(1150, fx, 'volatile').

prolog:volatile(P) :- var(P),
	throw(error(instantiation_error,volatile(P))).
prolog:volatile(M:P) :-
	do_volatile(P,M).
prolog:volatile((G1,G2)) :-
	prolog:volatile(G1),
	prolog:volatile(G2).
prolog:volatile(P) :-
	do_volatile(P,_).

do_volatile(_,_).

:- meta_predicate prolog:forall(+,:).

:- load_foreign_files([yap2swi], [], swi_install).

:- use_module(library(lists)).

prolog:absolute_file_name(jar(File), _Opts, Path) :- !,
	absolute_file_name(library(File), Path).
prolog:absolute_file_name(library(File), _Opts, Path) :- !,
	absolute_file_name(library(File), Path).
prolog:absolute_file_name(File, _Opts, Path) :-
	absolute_file_name(File, Path).


prolog:term_to_atom(Term,Atom) :-
	nonvar(Atom), !,
	atom_codes(Atom,S),
	read_from_chars(S,Term).
prolog:term_to_atom(Term,Atom) :-
	write_to_chars(Term,S),
	atom_codes(Atom,S).

prolog:concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).

prolog:concat_atom(List, New) :-
	atomic_concat(List, New).

add_separator_to_list([], _, []).
add_separator_to_list([T], _, [T]) :- !.
add_separator_to_list([H|T], Separator, [H,Separator|NT]) :-
	add_separator_to_list(T, Separator, NT).


prolog:setenv(X,Y) :- unix(putenv(X,Y)).

prolog:nth1(I,L,A) :- nth(I,L,A).

prolog:forall(X,Y) :- 
	catch(do_forall(X,Y), fail_forall, fail).

do_forall(X,Y) :-
	call(X),
	do_for_forall(Y).
do_forall(_,_).

do_for_forall(Y) :- call(Y), !, fail.
do_for_forall(_) :- throw(fail_forall).

prolog:between(I,_,I).
prolog:between(I0,I,J) :- I0 < I, 
	I1 is I0+1,
	prolog:between(I1,I,J).

prolog:b_getval(GlobalVariable,Value) :-
	array_element(GlobalVariable,0,Value).

prolog:b_setval(GlobalVariable,Value) :-
	array(GlobalVariable,1),
	dynamic_update_array(GlobalVariable,0,Value).

prolog:nb_getval(GlobalVariable,Value) :-
	array_element(GlobalVariable,0,Value).

prolog:nb_setval(GlobalVariable,Value) :-
	static_array(GlobalVariable,1,nb_term),
	update_array(GlobalVariable,0,Value).

prolog:nb_delete(GlobalVariable) :-
	close_static_array(GlobalVariable).

prolog:nb_current(GlobalVariable,Val) :-
	static_array_properties(GlobalVariable,1,nb_term),
	array_element(GlobalVariable,0,Val).

% SWI has a dynamic attribute scheme

prolog:get_attr(Var, Mod, Att) :-
	      AttTerm =.. [Mod,_,Att],
	      attributes:get_module_atts(Var, AttTerm).

prolog:put_attr(Var, Mod, Att) :-
	      AttTerm =.. [Mod,_,Att],
	      attributes:put_module_atts(Var, AttTerm).

prolog:del_attr(Var, Mod) :-
	      AttTerm =.. [Mod,_,_],
	      attributes:del_all_module_atts(Var, AttTerm).

prolog:get_attrs(AttVar, SWIAtts) :-
	get_all_swi_atts(AttVar,SWIAtts).

prolog:put_attrs(_, []).
prolog:put_attrs(V, att(Mod,Att,Atts)) :-
	prolog:put_attr(V,Mod,Att),
	prolog:put_attrs(V, Atts).

bindings_message(V) -->
       { cvt_bindings(V, Bindings) },
       prolog:message(query(YesNo,Bindings)), !.

cvt_bindings([],[]).
cvt_bindings([[Name|Value]|L],[AName=Value|Bindings]) :-
	atom_codes(AName, Name),
	cvt_bindings(L,Bindings).

'$messages':prolog_message(_,L,L).

prolog:append([],L,L).
prolog:append([X|L0],L,[X|Lf]) :-
	prolog:append(L0,L,Lf).

prolog:member(X,[X|_]).
prolog:member(X,[_|L0]) :-
	prolog:member(X,L0).

tv(Term,List) :- term_variables(Term,List).

prolog:term_variables(Term,List) :- tv(Term,List).

tv(Term,List,Tail) :- term_variables(Term,List,Tail).

prolog:term_variables(Term,List,Tail) :- tv(Term,List,Tail).

prolog:working_directory(OCWD,NCWD) :-
	getcwd(OCWD),
	(var(NCWD) -> true ; cd(NCWD)).

prolog:chdir(X) :- cd(X).

% Time is given as int, not as float.
prolog:get_time(Secs) :- datime(Datime),  mktime(Datime, Secs).

% Time is received as int, and converted to "..."
prolog:convert_time(X,Y) :- swi:ctime(X,Y).

:- hide(atom_concat).

prolog:atom_concat(A,B) :- atomic_concat(A,B).

prolog:atom_concat(A,B,C) :- atomic_concat(A,B,C).

:- hide(create_mutable).

:- hide(get_mutable).

:- hide(update_mutable).

prolog:hash_term(X,Y) :- term_hash(X,Y).

:- meta_predicate prolog:maplist(:,?), prolog:maplist(:,?,?), prolog:maplist(:,?,?).


prolog:maplist(_, []).
prolog:maplist(G, [H|L]) :-
	call(G,H),
	prolog:maplist(G, L).

prolog:maplist(_, [], []).
prolog:maplist(G, [H1|L1], [H2|L2]) :-
	call(G,H1,H2),
	prolog:maplist(G, L1, L2).

prolog:maplist(_, [], [], []).
prolog:maplist(G, [H1|L1], [H2|L2], [H3|L3]) :-
	call(G,H1,H2,H3),
	prolog:maplist(G, L1, L2, L3).

prolog:make.

prolog:source_location(File,Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position, '$stream_position'(_,Line,_)).

prolog:memberchk(Element, [Element|_]) :- !.
prolog:memberchk(Element, [_|Rest]) :-
        prolog:memberchk(Element, Rest).


	


