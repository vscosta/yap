
/**
  * @file   dialect.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 10:50:33 2017
  * 
  * @brief  support Prolog dialects
  *
  * @defgroup Dialects
  * @ingroup Builtins
  * 
*/


:- system_module(dialect,
	  [
	   source_exports/2
	  ], []).

:- use_system_module( '$_errors', [throw_error/2]).


				%
%%
%	@pred expects_dialect(+Dialect)
%
%	True if YAP can enable support for a different Prolog dialect.
%   Currently there is support for bprolog, hprolog and swi-prolog.
%   Notice that this support may be incomplete.
%
%   The
prolog:expects_dialect(yap) :- !,
	eraseall('$dialect'),
	recorda('$dialect',yap,_).
prolog:expects_dialect(Dialect) :-
	check_dialect(Dialect),
	eraseall('$dialect'),
	load_files(library(dialect/Dialect),[silent(true),if(not_loaded)]),
	(   current_predicate(Dialect:setup_dialect/0)
	->  Dialect:setup_dialect
	;   true
	),
	recorda('$dialect',Dialect,_).

check_dialect(Dialect) :-
	var(Dialect),!,
	throw_error(instantiation_error,(:- expects_dialect(Dialect))).
check_dialect(Dialect) :-
	\+ atom(Dialect),!,
	throw_error(type_error(Dialect),(:- expects_dialect(Dialect))).
check_dialect(Dialect) :-
	exists_source(library(dialect/Dialect)), !.
check_dialect(Dialect) :-
	throw_error(domain_error(dialect,Dialect),(:- expects_dialect(Dialect))).


%%	source_exports(+Source, +Export) is semidet.
%%	source_exports(+Source, -Export) is nondet.
%
%	True if Source exports Export. Fails   without  error if this is
%	not the case.  See also exists_source/1.
%
%	@tbd	Should we also allow for source_exports(-Source, +Export)?

source_exports(Source, Export) :-
	open_source(Source, In),
	catch(call_cleanup(exports(In, Exports), close(In)), _, fail),
	(   ground(Export)
	->  '$memberchk'(Export, Exports)
	;   '$member'(Export, Exports)
	).

%%	open_source(+Source, -In:stream) is semidet.
%
%	Open a source location.

open_source(File, In) :-
	exists_source(File, Path),
	open(Path, read, In),
	(   peek_char(In, #)
	->  skip(In, 10)
	;   true
	).

exports(In, Exports) :-
	read(In, Term),
	Term = (:- module(_Name, Exports)).
