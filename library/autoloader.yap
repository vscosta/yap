/**
 * @file autoloader.yap

 @defgroup autoloader SWI-Like autoloader
@ingroup YAPLibrary
@{
 */
:- module(autoloader,[make_library_index/0]).

:- use_module(library(lists),[append/3]).

:- dynamic exported/3, loaded/1.

:- initialization( scan_library ).

:- reexport(library(swi/autoloader),
	    [	make_library_index/0 as make_swi_index/0,
		get_exports/3,
		exported/3 as autoloaded/2,
		loaded/1 as autoloaded_library/1]).


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

%% @}

