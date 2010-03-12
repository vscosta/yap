
:- source.

:- style_check(all).

:- yap_flag(unknown,error).

:- yap_flag(open_expands_filename,false).

% redefines stuff in prolog module.

:- module(swi, []).

:- load_foreign_files([plstream], [], initIO).

:- set_prolog_flag(user_flags,silent).

:- use_module(library(charsio),[write_to_chars/2,read_from_chars/2]).

:- use_module(library(lists),[append/2,
			      append/3,
			      delete/3,
			      member/2,
			      min_list/2,
			      nth1/3,
			      nth0/3]).

:- use_module(library(apply),[maplist/2,
			      maplist/3,
			      maplist/4,
			      maplist/5,
			      include/3,
			      exclude/3,
			      partition/4,
			      partition/5
			     ]).

:- use_module(library(system),
	      [datime/1,
	       mktime/2,
	       file_property/2,
	       sleep/1]).

:- use_module(library(arg),
	      [genarg/3]).

:- use_module(library(apply_macros),
	      []).

:- use_module(library(terms),
	      [subsumes/2,
	       subsumes_chk/2,
	       term_hash/2,
	       unifiable/3,
	       variant/2]).

:- unhide('$system_library_directories'),
	unhide('$dir_separator').

% make sure we also use 
:- user:library_directory(X),
	atom(X),
	atom_concat([X,'/swi'],SwiDir),
	\+ user:library_directory(SwiDir),
	asserta(user:library_directory(SwiDir)),
	fail
	;
	true.

:- multifile user:term_expansion/2.
:- multifile user:goal_expansion/3.
:- multifile user:goal_expansion/2.

:- dynamic user:goal_expansion/2.

:- multifile swi_predicate_table/4.

swi_predicate_table(_,append(X,Y),lists,append(X,Y)).
swi_predicate_table(_,append(X,Y,Z),lists,append(X,Y,Z)).
swi_predicate_table(_,member(X,Y),lists,member(X,Y)).
swi_predicate_table(_,nextto(X,Y,Z),lists,nextto(X,Y,Z)).
swi_predicate_table(_,delete(X,Y,Z),lists,delete(X,Y,Z)).
swi_predicate_table(_,select(X,Y,Z),lists,select(X,Y,Z)).
swi_predicate_table(_,selectchk(X,Y,Z),lists,selectchk(X,Y,Z)).
swi_predicate_table(_,nth0(X,Y,Z),lists,nth0(X,Y,Z)).
swi_predicate_table(_,nth1(X,Y,Z),lists,nth1(X,Y,Z)).
swi_predicate_table(_,last(X,Y),lists,last(X,Y)).
swi_predicate_table(_,reverse(X,Y),lists,reverse(X,Y)).
swi_predicate_table(_,permutation(X,Y),lists,permutation(X,Y)).
swi_predicate_table(_,flatten(X,Y),lists,flatten(X,Y)).
swi_predicate_table(_,sumlist(X,Y),lists,sumlist(X,Y)).
swi_predicate_table(_,min_list(X,Y),lists,min_list(X,Y)).
swi_predicate_table(_,max_list(X,Y),lists,max_list(X,Y)).
swi_predicate_table(_,memberchk(X,Y),lists,memberchk(X,Y)).
swi_predicate_table(_,flatten(X,Y),lists,flatten(X,Y)).
swi_predicate_table(_,select(X,Y,Z),lists,select(X,Y,Z)).
swi_predicate_table(_,sublist(X,Y),lists,sublist(X,Y)).
swi_predicate_table(_,hash_term(X,Y),terms,term_hash(X,Y)).
swi_predicate_table(_,term_hash(X,Y),terms,term_hash(X,Y)).
swi_predicate_table(_,subsumes(X,Y),terms,subsumes(X,Y)).
swi_predicate_table(_,subsumes_chk(X,Y),terms,subsumes_chk(X,Y)).
swi_predicate_table(_,unifiable(X,Y,Z),terms,unifiable(X,Y,Z)).
swi_predicate_table(_,cyclic_term(X),terms,cyclic_term(X)).
swi_predicate_table(_,acyclic_term(X),terms,acyclic_term(X)).
swi_predicate_table(_,genarg(X,Y,Z),arg,genarg(X,Y,Z)).
swi_predicate_table(_,tmp_file(X,Y),system,tmp_file(X,Y)).
swi_predicate_table(_,maplist(X,Y),apply,maplist(X,Y)).
swi_predicate_table(_,maplist(X,Y,Z),apply,maplist(X,Y,Z)).
swi_predicate_table(_,maplist(X,Y,Z,A),apply,maplist(X,Y,Z,A)).
swi_predicate_table(_,maplist(X,Y,Z,A,B),apply,maplist(X,Y,Z,A,B)).
swi_predicate_table(_,include(X,Y,Z),apply,include(X,Y,Z)).
swi_predicate_table(_,exclude(X,Y,Z),apply,exclude(X,Y,Z)).
swi_predicate_table(_,partition(X,Y,Z,A),apply,partition(X,Y,Z,A)).
swi_predicate_table(_,partition(X,Y,Z,A,B),apply,partition(X,Y,Z,A,B)).
% swi_predicate_table(_,set_test_options(X),plunit,set_test_options(X)).
% swi_predicate_table(_,begin_tests(X),plunit,begin_tests(X)).
% swi_predicate_table(_,begin_tests(X,Y),plunit,begin_tests(X,Y)).
% swi_predicate_table(_,end_tests(X),plunit,end_tests(X)).
% swi_predicate_table(_,run_tests,plunit,run_tests).
% swi_predicate_table(_,run_tests(X),plunit,run_tests(X)).
% swi_predicate_table(_,load_test_files(X),plunit,load_test_files(X)).
% swi_predicate_table(_,running_tests,plunit,running_tests).
% swi_predicate_table(_,test_report(X),plunit,test_report(X)).

swi_mchk(X,Y) :- lists:memberchk(X,Y).

prolog:memberchk(X,Y) :- swi_mchk(X,Y).

:- dynamic
   prolog:message/3.

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

:- meta_predicate prolog:predsort(:,+,-).

prolog:plus(X, Y, Z) :-
       integer(X),
       integer(Y), !,
       Z is X + Y.
prolog:plus(X, Y, Z) :-
       integer(X),
       integer(Z), !,
       Y is Z - X.
prolog:plus(X, Y, Z) :-
       integer(Y),
       integer(Z), !,
       X is Z - Y.

%%	predsort(:Compare, +List, -Sorted) is det.
%
%	 Sorts similar to sort/2, but determines  the order of two terms
%	 by calling Compare(-Delta, +E1,  +E2).   This  call  must unify
%	 Delta with one of <, > or =. If built-in predicate compare/3 is
%	 used, the result is the same as sort/2. See also keysort/2.

prolog:predsort(P, L, R) :-
	length(L, N), 
	predsort(P, N, L, _, R1), !, 
	R = R1.

predsort(P, 2, [X1, X2|L], L, R) :- !, 
	call(P, Delta, X1, X2),
	sort2(Delta, X1, X2, R).
predsort(_, 1, [X|L], L, [X]) :- !.
predsort(_, 0, L, L, []) :- !.
predsort(P, N, L1, L3, R) :-
	N1 is N // 2, 
	plus(N1, N2, N), 
	predsort(P, N1, L1, L2, R1), 
	predsort(P, N2, L2, L3, R2), 
	predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
sort2(=, X1, _,  [X1]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2),
	predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	predmerge(P, T1, [H2|T2], R).


%
% maybe a good idea to eventually support this in YAP.
% but for now just ignore it.
%
prolog:load_foreign_library(P,Command) :-
	absolute_file_name(P,[file_type(executable),solutions(first),file_errors(fail)],Lib),
	load_foreign_files([Lib],[],Command).

prolog:load_foreign_library(P) :-
	prolog:load_foreign_library(P,install).

:- use_module(library(lists)).

prolog:term_to_atom(Term,Atom) :-
	nonvar(Atom), !,
	atom_codes(Atom,S),
	read_from_chars(S,Term).
prolog:term_to_atom(Term,Atom) :-
	write_to_chars(Term,S),
	atom_codes(Atom,S).

prolog:concat_atom([A|List], Separator, New) :- var(List), !,
	atom_codes(Separator,[C]),
	atom_codes(New, NewChars),
	split_atom_by_chars(NewChars,C,L,L,A,List).
prolog:concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).

prolog:concat_atom(List, New) :-
	atomic_concat(List, New).


split_atom_by_chars([],_,[],L,A,[]):-
	atom_codes(A,L).
split_atom_by_chars([C|NewChars],C,[],L,A,[NA|Atoms]) :- !,
	atom_codes(A,L),
	split_atom_by_chars(NewChars,C,NL,NL,NA,Atoms).
split_atom_by_chars([C1|NewChars],C,[C1|LF],LAtom,Atom,Atoms) :-
	split_atom_by_chars(NewChars,C,LF,LAtom,Atom,Atoms).

add_separator_to_list([], _, []).
add_separator_to_list([T], _, [T]) :- !.
add_separator_to_list([H|T], Separator, [H,Separator|NT]) :-
	add_separator_to_list(T, Separator, NT).


prolog:setenv(X,Y) :- unix(putenv(X,Y)).

prolog:prolog_to_os_filename(X,X).

prolog:is_absolute_file_name(X) :-
	absolute_file_name(X,X).

prolog:read_clause(X,Y) :-
	read_term(X,Y,[singetons(warning)]).

prolog:string(_) :- fail.

slp(T) :- sleep(T).

prolog:sleep(T) :-
	slp(T).

bindings_message(V) -->
       { cvt_bindings(V, Bindings) },
       prolog:message(query(_YesNo,Bindings)), !.

cvt_bindings([],[]).
cvt_bindings([[Name|Value]|L],[AName=Value|Bindings]) :-
	atom_codes(AName, Name),
	cvt_bindings(L,Bindings).

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

% copied from SWI lists library.
lists:intersection([], _, []) :- !.
lists:intersection([X|T], L, Intersect) :-
	memberchk(X, L), !, 
	Intersect = [X|R], 
	lists:intersection(T, L, R).
lists:intersection([_|T], L, R) :-
	lists:intersection(T, L, R).

prolog:compile_aux_clauses([]).
prolog:compile_aux_clauses([(:- G)|Cls]) :-
	prolog_load_context(module, M),
	once(M:G),
	prolog:compile_aux_clauses(Cls).
prolog:compile_aux_clauses([Cl|Cls]) :-
	prolog_load_context(module, M),
	assert_static(M:Cl),
	prolog:compile_aux_clauses(Cls).

% fix different semantics for arg/3.
user:goal_expansion(arg(X,Y,Z),arg:genarg(X,Y,Z)) :-
	nonvar(X), !.

prolog:'$set_source_module'(Source0, SourceF) :-
	prolog_load_context(module, Source0),
	module(SourceF).

prolog:'$set_source_module'(Source0, SourceF) :-
	current_module(Source0, SourceF).

prolog:'$declare_module'(Name, Context, _, _, _) :-
	add_import_module(Name, Context, start).

prolog:'$set_predicate_attribute'(_, _, _).

prolog:time_file(File, Time) :-
	file_property(File, mod_time(Date)),
	Time is Date*1.0.

prolog:flag(Key, Old, New) :-
	recorded(Key, Old, R), !,
	(
	 Old \== New
	->
	 erase(R),
	 recorda(Key, New, _)
	;
	 true
	).
prolog:flag(Key, 0, New) :-
	functor(Key, N, Ar),
	functor(K, N, Ar),
	assert(swi:flag(K)),
	recorda(K, New, _).

prolog:current_flag(Key) :-
	swi:flag(Key).
