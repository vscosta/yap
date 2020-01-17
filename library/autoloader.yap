/**
 * 

 */
:- module(autoloader,[make_library_index/0]).

:- use_module(library(lists),[append/3]).

:- dynamic exported/3, loaded/1.

scan_library_exports :-
				% init table file.
    directory_files('.',Files),
    (
	member(F,Files),
	file_name_extension(B, S, F),
	(S == pl; S == yap; S = ypp),
	scan_exports(F, library(S)),
	fail
    ;
    close(W)
    ),

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
	publish_exports(Exports, CallName, Module).
scan_exports(Library) :-
    format(user_error,'[ warning: library ~w not defined ]~n',[Library]).


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
    functor(G, F, A),
    index(G, _, M),
    M \= module,
    format(user_error,'[ warning: clash between ~a and ~a over ~a/~d ]~n',[Module,M,F,A]).
publish_export(F, A, W, Path, Module) :-
    functor(G, F, A),
    assert(index(G, Path, Module)).



