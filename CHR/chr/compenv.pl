%
% Provides compile time environment for fcompiling CHR
%

env_fcompile( File) :-
	( file_mod( File, Module) ->
	    fcompile( Module:File)
	;   File = library(File0), file_mod( File0, Module ) ->
	    fcompile( Module:File)
	;   fcompile( File)
	).

file_mod( chr,	    chr) :-
	use_module( library(atts)),
	use_module( getval),
	use_module( sbag).
file_mod( trace,    chr) :-
	use_module( getval).
file_mod( operator, chrcmp).
file_mod( chrcmp, chrcmp) :-
	[library(operator)],
	use_module( matching),
	use_module( getval).
file_mod( ordering, ordering) :-
	use_module( library(atts)).




