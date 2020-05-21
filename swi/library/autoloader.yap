/**
 * @file swi/library/autoloader.yap

 */
:- module(swi_autoloader,[make_library_index/0,
			  get_exports/3,
			  exported/3,
			  loaded/1]).

:- use_module(library(lists),[append/3]).

:- dynamic exported/3, loaded/1.

make_library_index :-
    scan_swi_exports.


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
scan_exports(Library, _) :-
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

