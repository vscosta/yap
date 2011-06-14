:- module('$swi',
	  []).

%%	file_alias_path(-Alias, ?Dir) is nondet.
%
%	True if file Alias points to Dir.  Multiple solutions are
%	generated with the longest directory first.

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
	forall(member(t(_, _, Alias, Dir), List),
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


