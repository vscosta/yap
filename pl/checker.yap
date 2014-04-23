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
* Last rev:     $Date: 2008-03-31 22:56:22 $,$Author: vsc $						 *
* $Log: not supported by cvs2svn $
* Revision 1.23  2007/11/26 23:43:09  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.22  2006/11/17 12:10:46  vsc
* style_checker was failing on DCGs
*
* Revision 1.21  2006/03/24 16:26:31  vsc
* code review
*
* Revision 1.20  2005/11/05 23:56:10  vsc
* should have meta-predicate definitions for calls,
*   multifile and discontiguous.
* have discontiguous as a builtin, not just as a
*   declaration.
*
* Revision 1.19  2005/10/28 17:38:50  vsc
* sveral updates
*
* Revision 1.18  2005/04/20 20:06:11  vsc
* try to improve error handling and warnings from within consults.
*
* Revision 1.17  2005/04/20 04:08:20  vsc
* fix warnings
*
* Revision 1.16  2005/01/13 05:47:27  vsc
* lgamma broke arithmetic optimisation
* integer_y has type y
* pass original source to checker (and maybe even use option in parser)
* use warning mechanism for checker messages.
*
* Revision 1.15  2004/06/29 19:12:01  vsc
* fix checker messages
*
* Revision 1.14  2004/06/29 19:04:46  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.13  2004/03/19 11:35:42  vsc
* trim_trail for default machine
* be more aggressive about try-retry-trust chains.
*    - handle cases where block starts with a wait
*    - don't use _killed instructions, just let the thing rot by itself.
*                                                                  *
*									 *
*************************************************************************/

:- system_module( style_checker, [no_style_check/1,
        style_check/1], ['$check_term'/5,
        '$init_style_check'/1,
        '$sv_warning'/2,
        '$syntax_check_discontiguous'/2,
        '$syntax_check_multiple'/2,
        '$syntax_check_single_var'/2]).

%
% A Small style checker for YAP

:- op(1150, fx, multifile).

style_check(V) :- var(V), !, fail.
style_check(V) :-
	style_check_(V), !.
style_check(V) :-
	\+atom(V), \+ list(V), V \= + _, V \= + _, !,
	'$do_error'( type_error('+|-|?(Flag)', V), style_check(V) ).
style_check(V) :-
	\+atom(V), \+ list(V), V \= + _, V \= + _, !,
	'$do_error'( domain_error(style_name(Flag), V), style_check(V) ).
	

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
	'$style_checker  '( [ L ] ),
	lists:member( Style,  [ singleton, discontiguous, multiple ] ),
	( lists:member(Style, L ) -> Info = +Style ; Info = -Style ).
style_check_([]).
style_check_([H|T]) :- style_check(H), style_check(T).

no_style_check(V) :- var(V), !, fail.
no_style_check(all) :-
	'$style_checker'( [ -singleton, -discontiguous, -multiple ] ).
no_style_check(-single_var) :-
	'$style_checker'( [ -singleton ] ).
no_style_check(-singleton) :-
	'$style_checker'( [ -singleton ] ).
no_style_check(-discontiguous) :-
	'$stylechecker'( [ -discontiguous ] ).
no_style_check(-multiple) :-
	'$style_checker'( [  -multiple ] ).
no_style_check([]).
no_style_check([H|T]) :- no_style_check(H), no_style_check(T).


'$syntax_check_single_var'(O,N) :- 
	'$values'('$syntaxchecksinglevar',O,N),
	'$checking_on'.

'$syntax_check_discontiguous'(O,N) :- 
	'$values'('$syntaxcheckdiscontiguous',O,N),
	'$checking_on'.

'$syntax_check_multiple'(O,N) :- 
	'$values'('$syntaxcheckmultiple',O,N),
	'$checking_on'.

%
% cases where you need to check a clause
%
'$checking_on' :-
	( 
	  get_value('$syntaxchecksinglevar',on)
	;
	  get_value('$syntaxcheckdiscontiguous',on)
	;
	  get_value('$syntaxcheckmultiple',on)
	), !,
	set_value('$syntaxcheckflag',on).
'$checking_on' :-
	set_value('$syntaxcheckflag',off).

% reset current state of style checker.
'$init_style_check'(File) :-
	recorded('$predicate_defs','$predicate_defs'(_,_,_,File),R),
	erase(R),
	fail.
'$init_style_check'(_).

% style checker proper..
'$check_term'(_, T, _,P,M) :-
	get_value('$syntaxcheckdiscontiguous',on),
	strip_module(T, M, T1),
	'$pred_arity'( T1, Name, Arity ),
	% should always fail
	'$handle_discontiguous'(Name, Arity, M),
	fail.
'$check_term'(_, T,_,P,M) :-
	get_value('$syntaxcheckmultiple',on),
	strip_module(T, M, T1),
	'$pred_arity'( T1, Name, Arity ),
	'$handle_multiple'( Name , Arity, M), 
	fail.
'$check_term'(_, T,_,_,M) :-
	( 
	    get_value('$syntaxcheckdiscontiguous',on)
	->
	       true
	;
	    get_value('$syntaxcheckmultiple',on)
	),
	source_location( File, _ ),
	strip_module(T, M, T1),
	'$pred_arity'( T1, Name, Arity ),
	\+ (
	    % allow duplicates if we are not the last predicate to have
	    % been asserted.
	    once(recorded('$predicate_defs','$predicate_defs'(F0,A0,M0,File),_)),
	    F0 = F, A0 = A, M0 = NM
	),
	recorda('$predicate_defs','$predicate_defs'(F,A,NM,File),_),
	fail.
'$check_term'(_,_,_,_,_).

% check if a predicate is discontiguous.
'$handle_discontiguous'(F,A,M) :-
	recorded('$discontiguous_defs','$df'(F,A,M),_), !,
	fail.
'$handle_discontiguous'(F,A,M) :-
	functor(Head, F, A),
	'$is_multifile'(Head, M), !,
	fail.
'$handle_discontiguous'((:-),1,_) :- !,
	fail.
'$handle_discontiguous'(F,A,M) :-
	source_location( FileName, _ ),
	% we have been there before
	once(recorded('$predicate_defs','$predicate_defs'(F, A, M, FileName),_)),
	% and we are not 
	\+ (
	    % the last predicate to have been asserted
	    once(recorded('$predicate_defs','$predicate_defs'(F0,A0,M0,FileName),_)),
	    F0 = F, A0 = A, M0 = M
	),
	print_message(warning,clauses_not_together((M:F/A))),
        fail.

% never complain the second time
'$handle_multiple'(F,A,M) :-
	source_location(FileName, _),
	recorded('$predicate_defs','$predicate_defs'(F,A,M,FileName),_), !.
% first time we have a definition
'$handle_multiple'(F,A,M) :-
	source_location(FileName0, _),
	recorded('$predicate_defs','$predicate_defs'(F,A,M,FileName),_),
	FileName \= FileName0,
	'$multiple_has_been_defined'(FileName, F/A, M), !.

% be careful about these cases.
% consult does not count
'$multiple_has_been_defined'(_, _, _) :-
	'$nb_getval'('$consulting_file', _, fail), !.	
% multifile does not count
'$multiple_has_been_defined'(_, F/A, M) :-
	functor(S, F, A),
	'$is_multifile'(S, M), !.
'$multiple_has_been_defined'(Fil,F/A,M) :-
	% first, clean up all definitions in other files
	% don't forget, we just removed everything.
	recorded('$predicate_defs','$predicate_defs'(F,A,M,FileName),R),
	erase(R),
	fail.
'$multiple_has_been_defined'(Fil,P,M) :-
	print_message(warning,defined_elsewhere(M:P,Fil)).



