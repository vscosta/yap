/**
  * @file   pl/swi.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:18:05 2017
  * 
  * @brief  SWI Emulation support
  *
  * @ingroup dialects
  * 
  * 
*/
:- module('$swi',
	  []).

%%	file_alias_path(-Alias, ?Dir) is nondet.
%
%	True if file Alias points to Dir.  Multiple solutions are
%	generated with the longest directory first.

%%	file_name_on_path(+File:atom, -OnPath) is det.
%
%	True if OnPath a description of File   based  on the file search
%	path. This performs the inverse of absolute_file_name/3.

prolog:file_name_on_path(Path, ShortId) :-
	(   prolog:file_alias_path(Alias, Dir),
	    atom_concat(Dir, Local, Path)
	->  (   Alias == '.'
	    ->  ShortId = Local
	    ;   file_name_extension(Base, pl, Local)
	    ->  ShortId =.. [Alias, Base]
	    ;   ShortId =.. [Alias, Local]
	    )
	;   ShortId = Path
	).


:- dynamic
	alias_cache/2.

prolog:file_alias_path(Alias, Dir) :-
	(   alias_cache(_, _)
	->  true
	;   build_alias_cache
	),
	(   nonvar(Dir)
	->  ensure_slash(Dir, DirSlash),
	    alias_cache(Alias, DirSlash)
	;   alias_cache(Alias, Dir)
	).

build_alias_cache :-
	findall(t(DirLen, AliasLen, Alias, Dir),
		search_path(Alias, Dir, AliasLen, DirLen), Ts),
	sort(Ts, List0),
	reverse(List0, List),
	forall(lists:member(t(_, _, Alias, Dir), List),
	       assert(alias_cache(Alias, Dir))).

search_path('.', Here, 999, DirLen) :-
	working_directory(Here0, Here0),
	ensure_slash(Here0, Here),
	atom_length(Here, DirLen).
search_path(Alias, Dir, AliasLen, DirLen) :-
	user:file_search_path(Alias, _),
	Alias \== autoload,
	Spec =.. [Alias,'.'],
	atom_length(Alias, AliasLen0),
	AliasLen is 1000 - AliasLen0,	% must do reverse sort
	absolute_file_name(Spec, Dir0,
			   [ file_type(directory),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ]),
	ensure_slash(Dir0, Dir),
	atom_length(Dir, DirLen).

ensure_slash(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).


/** @pred reverse(+ _List_, ? _Reversed_) 


True when  _List_ and  _Reversed_ are lists with the same elements
but in opposite orders. 

 
*/
reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], Sofar, Reversed) :-
	reverse(Tail, [Head|Sofar], Reversed).


%%      win_add_dll_directory(+AbsDir) is det.
%
%       Add AbsDir to the directories where  dependent DLLs are searched
%       on Windows systems.

:- if(current_prolog_flag(windows, true)).
prolog:win_add_dll_directory(Dir) :-
        win_add_dll_directory(Dir, _), !.
prolog:win_add_dll_directory(Dir) :-
        prolog_to_os_filename(Dir, OSDir),
        getenv('PATH', Path0),
        atomic_list_concat([Path0, OSDir], ';', Path),
        setenv('PATH', Path).
:- endif.
