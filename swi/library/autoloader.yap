/**
 * @file swi/library/autoloader.yap

 */
:- module(autoloader,[make_library_index/0]).

:- use_module(library(lists),[append/3]).

:- dynamic exported/3, loaded/1.

make_library_index :-
	scan_library_exports,
	scan_swi_exports.

scan_library_exports :-
				% init table file.
	open('INDEX.pl', write, W),
	close(W),
	scan_exports('../GPL/aggregate', library(aggregate)),
	scan_exports(apply, library(apply)),
	scan_exports(arg, library(arg)),
	scan_exports(assoc, library(assoc)),
	scan_exports(avl, library(avl)),
	scan_exports(bhash, library(bhash)),
	scan_exports(charsio, library(charsio)),
	scan_exports('../packages/chr/chr_swi', library(chr)),
	scan_exports(clp/clpfd, library(clpfd)),
	scan_exports('../packages/clpqr/clpr', library(clpr)),
	scan_exports(gensym, library(gensym)),
	scan_exports(heaps, library(heaps)),
	scan_exports('../packages/jpl/jpl', library(jpl)),
	scan_exports(lists, library(lists)),
	scan_exports(nb, library(nb)),
	scan_exports(occurs, library(occurs)),
	scan_exports('../LGPL/option', library(option)),
	scan_exports(ordsets, library(ordsets)),
	scan_exports(pairs, library(pairs)),
	scan_exports('../LGPL/prolog_xref', library(prolog_xref)),
	scan_exports('../packages/plunit/plunit', library(plunit)),
	scan_exports(queues, library(queues)),
	scan_exports(random, library(random)),
	scan_exports(rbtrees, library(rbtrees)),
	scan_exports('../LGPL/readutil', library(readutil)),
	scan_exports(regexp, library(regexp)),
	scan_exports('../LGPL/shlib', library(shlib)),
	scan_exports(system, library(system)),
	scan_exports(terms, library(terms)),
	scan_exports(timeout, library(timeout)),
	scan_exports(trees, library(trees)).

scan_exports(Library, CallName) :-
	absolute_file_name(Library, Path,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]),
	open(Path, read, O),
	!,
	get_exports(O, Exports, Module),
	close(O),
	open('INDEX.pl', append, W),
	publish_exports(Exports, W, CallName, Module),
	close(W).
scan_exports(Library) :-
	format(user_error,'[ warning: library ~w not defined ]~n',[Library]).

%
% SWI is the only language that uses autoload.
%
scan_swi_exports :-
	retractall(exported(_,_,_)),
	absolute_file_name(dialect/swi, Path,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]),
	open(Path, read, O),
	get_exports(O, Exports, Module),
	get_reexports(O, Reexports, Exports),
	close(O),
	open('dialect/swi/INDEX.pl', write, W),
	publish_exports(Reexports, W, library(dialect/swi), Module),
	close(W).

get_exports(O, Exports, Module) :-
	read(O, (:- module(Module,Exports))), !.
get_exports(O, Exports, Module) :-
	get_exports(O, Exports, Module).

get_reexports(O, Exports, ExportsL) :-
	read(O, (:- reexport(_File,ExportsI))), !,
	get_reexports(O, Exports0, ExportsL),
	append(ExportsI, Exports0, Exports).
get_reexports(_, Exports, Exports).

publish_exports([], _, _, _).
publish_exports([F/A|Exports], W, Path, Module) :-
	publish_export(F, A, W, Path, Module),
	publish_exports(Exports, W, Path, Module).
publish_exports([F//A0|Exports], W, Path, Module) :-
	A is A0+2,
	publish_export(F, A, W, Path, Module),
	publish_exports(Exports, W, Path, Module).
publish_exports([op(_,_,_)|Exports], W, Path, Module) :-
	publish_exports(Exports, W, Path, Module).

publish_export(F, A, _, _, Module) :-
	exported(F, A, M), M \= Module, !,
	format(user_error,'[ warning: clash between ~a and ~a over ~a/~d ]~n',[Module,M,F,A]).
publish_export(F, A, W, Path, Module) :-
	assert(exported(F, A, Module)), !,
	portray_clause(W, index(F, A, Module, Path)).

find_predicate(G,ExportingModI) :-
	nonvar(G), !,
	functor(G, Name, Arity),
	index(Name,Arity,ExportingModI,File),
	ensure_file_loaded(File).
find_predicate(G,ExportingModI) :-
        var(G),
	index(Name,Arity,ExportingModI,File),
	functor(G, Name, Arity),
	ensure_file_loaded(File).

ensure_file_loaded(File) :-
	loaded(File), !.
ensure_file_loaded(File) :-
	load_files(autoloader:File,[silent(true),if(not_loaded)]),
	assert(loaded(File)).

:- include('INDEX').

