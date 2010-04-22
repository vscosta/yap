
% SWI emulation.
% written in an on-demand basis.


:- module(system, [term_to_atom/2,
		   concat_atom/2,
		   concat_atom/3,
		   setenv/2,
		   prolog_to_os_filename/2,
		   is_absolute_file_name/1,
		   read_clause/1,
		   string/1,
		   working_directory/2,
		   chdir/1,
		   compile_aux_clauses/1,
		   convert_time/2,
		   '$set_source_module'/2,
		   '$declare_module'/5,
		   '$set_predicate_attribute'/3,
		   load_foreign_library/1,
		   load_foreign_library/2,
		   time_file/2,
		   flag/3,
		   current_flag/1
		]).

:- reexport(library(charsio),[
			      write_to_chars/2,
			      read_from_chars/2
			     ]).

:- reexport(library(lists),[append/2,
			    append/3,
			    delete/3,
			    member/2,
			    flatten/2,
			    intersection/3,
			    last/2,
			    memberchk/2,
			    max_list/2,
			    min_list/2,
			    nextto/3,
			    permutation/2,
			    reverse/2,
			    select/3,
			    selectchk/3,
			    sublist/2,
			    sumlist/2,
			    nth1/3,
			    nth0/3]).

:- reexport(library(apply),[maplist/2,
			    maplist/3,
			    maplist/4,
			    maplist/5,
			    include/3,
			    exclude/3,
			    partition/4,
			    partition/5
			   ]).

:- reexport(library(system),
	      [datime/1,
	       mktime/2,
	       file_property/2,
	       sleep/1]).

:- reexport(library(arg),
	      [genarg/3]).

:- reexport(library(apply_macros),
	      []).

:- reexport(library(terms),
	      [subsumes/2,
	       subsumes_chk/2,
	       term_hash/2,
	       unifiable/3,
	       cyclic_term/1,
	       variant/2]).

:- source.

:- style_check(all).

:- yap_flag(unknown,error).

:- yap_flag(open_expands_filename,false).

:- yap_flag(autoload,true).


:- set_prolog_flag(user_flags,silent).

goal_expansion(atom_concat(A,B),atomic_concat(A,B)).
goal_expansion(atom_concat(A,B,C),atomic_concat(A,B,C)).
goal_expansion(arg(A,B,C),genarg(A,B,C)).
goal_expansion(time_file(A,B),system:swi_time_file(A,B)).
goal_expansion(get_time(A),system:swi_get_time(A)).

:- load_foreign_files([plstream], [], initIO).

% make sure we also use 
:- user:library_directory(X),
	atom(X),
	atom_concat([X,'/dialect/swi'],SwiDir),
	\+ user:library_directory(SwiDir),
	asserta(user:library_directory(SwiDir)),
	fail
	;
	true.

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
load_foreign_library(P,Command) :-
	absolute_file_name(P,[file_type(executable),solutions(first),file_errors(fail)],Lib),
	load_foreign_files([Lib],[],Command).

load_foreign_library(P) :-
	prolog:load_foreign_library(P,install).

term_to_atom(Term,Atom) :-
	nonvar(Atom), !,
	atom_codes(Atom,S),
	read_from_chars(S,Term).
term_to_atom(Term,Atom) :-
	write_to_chars(Term,S),
	atom_codes(Atom,S).

concat_atom([A|List], Separator, New) :- var(List), !,
	atom_codes(Separator,[C]),
	atom_codes(New, NewChars),
	split_atom_by_chars(NewChars,C,L,L,A,List).
concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).


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

concat_atom(List, New) :-
	atomic_concat(List, New).


setenv(X,Y) :- unix(putenv(X,Y)).

prolog_to_os_filename(X,X).

is_absolute_file_name(X) :-
	absolute_file_name(X,X).

read_clause(X,Y) :-
	read_term(X,Y,[singetons(warning)]).

string(_) :- fail.

bindings_message(V) -->
       { cvt_bindings(V, Bindings) },
       prolog:message(query(_YesNo,Bindings)), !.

cvt_bindings([],[]).
cvt_bindings([[Name|Value]|L],[AName=Value|Bindings]) :-
	atom_codes(AName, Name),
	cvt_bindings(L,Bindings).

working_directory(OCWD,NCWD) :-
	getcwd(OCWD),
	(var(NCWD) -> true ; cd(NCWD)).

chdir(X) :- cd(X).

% Time is given as a float in SWI-Prolog.
swi_get_time(FSecs) :- datime(Datime),  mktime(Datime, Secs), FSecs is Secs*1.0.

% Time is received as int, and converted to "..."
% ctime is a built-in.
convert_time(X,Y) :- swi:ctime(X,Y).

compile_aux_clauses([]).
compile_aux_clauses([(:- G)|Cls]) :-
	prolog_load_context(module, M),
	once(M:G),
	compile_aux_clauses(Cls).
compile_aux_clauses([Cl|Cls]) :-
	prolog_load_context(module, M),
	assert_static(M:Cl),
	compile_aux_clauses(Cls).

'$set_source_module'(Source0, SourceF) :-
	prolog_load_context(module, Source0), !,
	module(SourceF).
'$set_source_module'(Source0, SourceF) :-
	current_module(Source0, SourceF).

'$declare_module'(Name, Context, _, _, _) :-
	add_import_module(Name, Context, start).

'$set_predicate_attribute'(_, _, _).

swi_time_file(File, Time) :-
	file_property(File, mod_time(Date)),
	Time is Date*1.0.

flag(Key, Old, New) :-
	recorded(Key, Old, R), !,
	(
	 Old \== New
	->
	 erase(R),
	 recorda(Key, New, _)
	;
	 true
	).
flag(Key, 0, New) :-
	functor(Key, N, Ar),
	functor(K, N, Ar),
	assert(flag(K)),
	recorda(K, New, _).

current_flag(Key) :-
	swi:flag(Key).
