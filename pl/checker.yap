/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		checker.yap						 *
* comments:	style checker for Prolog				 *
*									 *
* Last rev:     $Date: 2008-03-31 22:56:22 $,$Author: vsc $		 *
*									 *
*************************************************************************/

:- system_module( style_checker, [no_style_check/1,
        style_check/1], ['$check_term'/5,
        '$sv_warning'/2,
        '$syntax_check_discontiguous'/2,
        '$syntax_check_multiple'/2,
        '$syntax_check_single_var'/2]).

%% @{

/**

@defgroup   YAPStyle Checker
    @ingroup  YAPCompilerSettings

YAP implements a style-checker thay currently verifies whether:

1 named variables occur once in a clause.

2 clauses from dofferent predicates are mixed together.

3 clauses for the same predicate occur in different files.

One can declare a predicate to be discontiguous (see the
discontiguous/1 declaration) and/or multifile/1.

*/

/*
@pred style_check(+ _X_)

Turns on style checking according to the attribute specified by  _X_,
which must be one of the following:

    + single_var
      Checks single occurrences of named variables in a clause.

    + discontiguous
      Checks non-contiguous clauses for the same predicate in a file.

    + multiple
      Checks the presence of clauses for the same predicate in more than one
      file when the predicate has not been declared as `multifile`

    + all
      Performs style checking for all the cases mentioned above.


By default, style checking is disabled in YAP unless we are in
`sicstus` or `iso` language mode.

The style_check/1 built-in is now deprecated. Please use
`set_prolog_flag/1` instead.

  **/
%
% A Small style checker for YAP

:- op(1150, fx, [multifile,discontiguous]).

style_check(V) :- var(V), !, fail.
style_check(V) :-
	style_check_(V), !.
style_check(V) :-
	\+atom(V), \+ list(V), V \= + _, V \= + _, !,
	'$do_error'( type_error('+|-|?(Flag)', V), style_check(V) ).
style_check(V) :-
	\+atom(V), \+ list(V), V \= + _, V \= + _, !,
	'$do_error'( domain_error(style_name, V), style_check(V) ).
	

style_check_(all) :-
	'$style_checker'( [ singleton, discontiguous, multiple ] ).
style_check_(single_var) :-
	'$style_checker'( [ singleton ] ).
style_check_(singleton) :-
	'$style_checker'( [ singleton ] ).
style_check_(+single_var) :-
	'$style_checker'( [ singleton ] ).
style_check_(+singleton) :-
	'$style_checker'( [ singleton ] ).
style_check_(-single_var) :-
	'$style_checker'( [ -singleton ] ).
style_check_(-singleton) :-
	'$style_checker'( [ -singleton ] ).
style_check_(discontiguous) :-
	'$style_checker'( [ discontiguous ] ).
style_check_(+discontiguous) :-
	'$style_checker'( [ discontiguous ] ).
style_check_(-discontiguous) :-
	'$style_checker'( [ -discontiguous ] ).
style_check_(multiple) :-
	'$style_checker'( [  multiple ] ).
style_check_(+multiple) :-
	'$style_checker'( [  multiple ] ).
style_check_(-multiple) :-
	'$style_checker'( [  -multiple ] ).
style_check_(no_effect) :-
	'$style_checker'( [  no_effect ] ).
style_check_(+no_effect) :-
	'$style_checker'( [  no_effect ] ).
style_check_(-no_effect) :-
	'$style_checker'( [  -no_effect ] ).
style_check_(var_branches) :-
	'$style_checker'( [  var_branches ] ).
style_check_(+var_branches) :-
	'$style_checker'( [  var_branches ] ).
style_check_(-var_branches) :-
	'$style_checker'( [  -var_branches ] ).
style_check_(atom) :-
	'$style_checker'( [  atom ] ).
style_check_(+atom) :-
	'$style_checker'( [  atom ] ).
style_check_(-atom) :-
	'$style_checker'( [  -atom ] ).
style_check_(charset) :-
	'$style_checker'( [  charset ] ).
style_check_(+charset) :-
	'$style_checker'( [  charset ] ).
style_check_(-charset) :-
	'$style_checker'( [  -charset ] ).
style_check_('?'(Info) ) :-
	L =  [ singleton, discontiguous, multiple ],
	( lists:member(Style, L ) -> Info = +Style ; Info = -Style ).
style_check_([]).
style_check_([H|T]) :- style_check(H), style_check(T).

/** @pred no_style_check(+ _X_)

Turns off style checking according to the attribute specified by
 _X_, which have the same meaning as in style_check/1.

The no_style_check/1 built-in is now deprecated. Please use 
`set_prolog_flag/1` instead.

**/
no_style_check(V) :- var(V), !, fail.
no_style_check(all) :-
	'$style_checker'( [ -singleton, -discontiguous, -multiple ] ).
no_style_check(-single_var) :-
	'$style_checker'( [ -singleton ] ).
no_style_check(-singleton) :-
	'$style_checker'( [ -singleton ] ).
no_style_check(-discontiguous) :-
	'$style_checker'( [ -discontiguous ] ).
no_style_check(-multiple) :-
	'$style_checker'( [  -multiple ] ).
no_style_check([]).
no_style_check([H|T]) :- no_style_check(H), no_style_check(T).

/** @pred discontiguous(+ _G_) is iso
    Avoid warnings from the sytax checker.

Declare that the predicate _G_ or list of predicates are discontiguous
procedures, that is, clauses for discontigous procedures may be
separated by clauses from other procedures.

*/



/*
@}
*/
