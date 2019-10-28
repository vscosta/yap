
/**
  * @file   dialect.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 10:50:33 2017
  *
  * @brief  support Prolog dialects
  */


% :- module(dialect,
% 	  [
% 	   exists_source/1,
% 	   source_exports/2
% 	  ]).

    /**
     * @defgroup Dialects Compatibility with other Prolog dialects
     * @ingroup extensions
     * @{
     * @brief Prolog dialects
     *
   */


:- use_system_module( '$_errors', ['$do_error'/2]).

:- private(
	   [check_dialect/1]
	  ).
	

%%
%	@pred expects_dialect(+Dialect)
%
%	  True if YAP can enable support for a different Prolog dialect.
%   Currently there is support for bprolog, hprolog and swi-prolog.
%   Notice that this support may be incomplete.
%
% 
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
	'$do_error'(instantiation_error,(:- expects_dialect(Dialect))).
check_dialect(Dialect) :-
	\+ atom(Dialect),!,
	'$do_error'(type_error(Dialect),(:- expects_dialect(Dialect))).
check_dialect(Dialect) :-
	exists_source(library(dialect/Dialect)), !.
check_dialect(Dialect) :-
	'$do_error'(domain_error(dialect,Dialect),(:- expects_dialect(Dialect))).


/**
 * @pred exists_source( +_File_ , -AbsolutePath_ )
 *
 * True if the term _File_ is likely to be a Prolog program stored in
 * path _AbsolutePath_. The file must allow read-access, and
 * user-expansion will * be performed. The predicate only succeeds or
 * fails, it never generates an exception.
 *
 *
 */
exists_source(File, AbsFile) :-
	catch(
	      absolute_file_name(File, AbsFile,
				 [access(read), file_type(prolog),
				  file_errors(fail), solutions(first), expand(true)]), _, fail ).

/**
 * @pred exists_source( +_File_  )
 *
 * True if the term _File_ matches a Prolog program. The file must allow read-access, and
 * user-expansion will * be performed. The predicate only succeeds or
 * fails, it never generates an exception.
 *
 *
 */
exists_source(File) :-
	exists_source(File, _AbsFile).


%%	@pred source_exports(+Source, +Export) is semidet.
%%	@pred source_exports(+Source, -Export) is nondet.
%
%	True if Source exports Export. Fails   without  error if this is
%	not the case.  See also exists_source/1.
%
%	@tbd	Should we also allow for source_exports(-Source, +Export)?

prolog:source_exports(Source, Export) :-
    open_source(Source, In),
	catch(call_cleanup(exports(In, Exports), close(In)), _, fail),
	(   ground(Export)
	->  lists:memberchk(Export, Exports)
	;   lists:member(Export, Exports)
	).

exports(In, Exports) :-
	read(In, Term),
	Term = (:- module(_Name, Exports)).

%% @}
