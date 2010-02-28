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

%
% A Small style checker for YAP

:- op(1150, fx, multifile).

style_check(V) :- var(V), !, fail.
style_check(all) :- '$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on).
style_check(single_var) :- '$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on).
style_check(singleton) :-
	style_check(single_var).
style_check(-single_var) :-
	no_style_check(single_var).
style_check(-singleton) :-
	no_style_check(single_var).
style_check(discontiguous) :- '$syntax_check_mode'(_,on),
	'$syntax_check_discontiguous'(_,on).
style_check(-discontiguous) :-
	no_style_check(discontiguous).
style_check(multiple) :- '$syntax_check_mode'(_,on),
	'$syntax_check_multiple'(_,on).
style_check(-multiple) :-
	no_style_check(multiple).
style_check([]).
style_check([H|T]) :- style_check(H), style_check(T).

no_style_check(V) :- var(V), !, fail.
no_style_check(all) :- '$syntax_check_mode'(_,off),
	'$syntax_check_single_var'(_,off),
	'$syntax_check_discontiguous'(_,off),
	'$syntax_check_multiple'(_,off).
no_style_check(single_var) :- '$syntax_check_mode'(_,off),
	'$syntax_check_single_var'(_,off).
no_style_check(discontiguous) :- '$syntax_check_mode'(_,off),
	'$syntax_check_discontiguous'(_,off).
no_style_check(multiple) :- '$syntax_check_mode'(_,on),
	'$syntax_check_multiple'(_,off).
no_style_check([]).
no_style_check([H|T]) :- no_style_check(H), no_style_check(T).



'$syntax_check_mode'(O,N) :- 
	'$values'('$syntaxcheckflag',O,N).

'$syntax_check_single_var'(O,N) :- 
	'$values'('$syntaxchecksinglevar',O,N).

'$syntax_check_discontiguous'(O,N) :- 
	'$values'('$syntaxcheckdiscontiguous',O,N).

'$syntax_check_multiple'(O,N) :- 
	'$values'('$syntaxcheckmultiple',O,N).


'$check_term'(T,_,P,M) :-
	get_value('$syntaxcheckdiscontiguous',on),
	'$xtract_head'(T,M,NM,_,F,A),
	'$handle_discontiguous'(F,A,NM), fail.
'$check_term'(T,_,P,M) :-
	get_value('$syntaxcheckmultiple',on),
	'$xtract_head'(T,M,NM,_,F,A),
	'$handle_multiple'(F,A,NM), fail.
'$check_term'(T,VL,P,_) :-
	get_value('$syntaxchecksinglevar',on),
	( '$chk_binding_vars'(T),
	  '$sv_list'(VL,Sv) ->
	  '$sv_warning'(Sv,T) ), fail.
'$check_term'(_,_,_,_).

'$chk_binding_vars'(V) :- var(V), !, V = '$V'(_).
'$chk_binding_vars'('$V'(off)) :- !.
'$chk_binding_vars'(A) :- primitive(A), !.
'$chk_binding_vars'(S) :- S =.. [_|L],
	'$chk_bind_in_struct'(L).

'$chk_bind_in_struct'([]).
'$chk_bind_in_struct'([H|T]) :-
	'$chk_binding_vars'(H),
	'$chk_bind_in_struct'(T).

'$sv_list'([],[]).
'$sv_list'([[[95|_]|_]|T],L) :-
	'$sv_list'(T,L).
'$sv_list'([[Name|'$V'(V)]|T],[Name|L]) :- var(V), !,
	'$sv_list'(T,L).
'$sv_list'([_|T],L) :-
	'$sv_list'(T,L).
	

'$sv_warning'([],_) :- !.
'$sv_warning'(SVs,T) :-
	'$current_module'(OM),
	'$xtract_head'(T,OM,M,H,Name,Arity),
	( nb_getval('$consulting',false),
	   '$first_clause_in_file'(Name,Arity, OM) ->
	    ClN = 1 ;
		'$number_of_clauses'(H,M,ClN0),
		ClN is ClN0+1
	),
	print_message(warning,singletons(SVs,(M:Name/Arity),ClN)).

'$xtract_head'(V,M,M,V,call,1) :- var(V), !.
'$xtract_head'((H:-_),OM,M,NH,Name,Arity) :- !,
        '$xtract_head'(H,OM,M,NH,Name,Arity).
'$xtract_head'((H,_),OM,M,H1,Name,Arity) :- !,
	'$xtract_head'(H,OM,M,H1,Name,Arity).
'$xtract_head'((H-->_),OM,M,HL,Name,Arity) :- !,
	'$xtract_head'(H,M,OM,_,Name,A1),
	Arity is A1+2,
	functor(HL,Name,Arity).
'$xtract_head'(M:H,_,NM,NH,Name,Arity) :- !,
	'$xtract_head'(H,M,NM,NH,Name,Arity).
'$xtract_head'(H,M,M,H,Name,Arity) :-
	functor(H,Name,Arity).

'$handle_discontiguous'(F,A,M) :-
	recorded('$discontiguous_defs','$df'(F,A,M),_), !.
'$handle_discontiguous'(F,A,M) :-
	functor(Head, F, A),
	'$is_multifile'(Head, M), !.
'$handle_discontiguous'(F,A,M) :-
	'$in_this_file_before'(F,A,M),
	print_message(warning,clauses_not_together((M:F/A))).

'$handle_multiple'(F,A,M) :-
	\+ '$first_clause_in_file'(F,A,M), !.
'$handle_multiple'(_,_,_) :-
	nb_getval('$consulting',true), !.
'$handle_multiple'(F,A,M) :-
	recorded('$predicate_defs','$predicate_defs'(F,A,M,Fil),_), !,
	'$multiple_has_been_defined'(Fil, F/A, M), !.
'$handle_multiple'(F,A,M) :-
	( recorded('$reconsulting',Fil,_) -> true ),
	recorda('$predicate_defs','$predicate_defs'(F,A,M,Fil),_).

'$multiple_has_been_defined'(_, F/A, M) :-
	functor(S, F, A),
	'$is_multifile'(S, M), !.
'$multiple_has_been_defined'(Fil,P,M) :-
	recorded('$reconsulting',F,_), !,
	'$test_if_well_reconsulting'(F,Fil,M:P).

'$test_if_well_reconsulting'(F,F,_) :- !.
'$test_if_well_reconsulting'(_,Fil,P) :-
	print_message(warning,defined_elsewhere(P,Fil)).

'$multifile'(V, _) :- var(V), !,
	'$do_error'(instantiation_error,multifile(V)).
'$multifile'((X,Y), M) :- !, '$multifile'(X, M), '$multifile'(Y, M).
'$multifile'(Mod:PredSpec, _) :- !,
	'$multifile'(PredSpec, Mod).
'$multifile'(N//A, M) :- !,
	integer(A),
	A1 is A+2,
	'$multifile'(N/A1, M).
'$multifile'(N/A, M) :-
	'$add_multifile'(N,A,M),
	fail.
'$multifile'(N/A, M) :-
         functor(S,N,A),
	'$is_multifile'(S, M), !.
'$multifile'(N/A, M) :- !,
	'$new_multifile'(N,A,M).
'$multifile'([H|T], M) :- !,
	'$multifile'(H,M),
	'$multifile'(T,M).
'$multifile'(P, M) :-
	'$do_error'(type_error(predicate_indicator,P),multifile(M:P)).

discontiguous(V) :-
	var(V), !,
	'$do_error'(instantiation_error,discontiguous(V)).
discontiguous(M:F) :- !,
	'$discontiguous'(F,M).
discontiguous(F) :-
	'$current_module'(M),
	'$discontiguous'(F,M).

'$discontiguous'(V,M) :- var(V), !,
	'$do_error'(instantiation_error,M:discontiguous(V)).
'$discontiguous'((X,Y),M) :- !,
	'$discontiguous'(X,M),
	'$discontiguous'(Y,M).
'$discontiguous'(M:A,_) :- !,
	'$discontiguous'(A,M).
'$discontiguous'(N//A1, M) :- !,
	integer(A1), !,
	A is A1+2,
	'$discontiguous'(N/A, M).
'$discontiguous'(N/A, M) :- !,
	( recordzifnot('$discontiguous_defs','$df'(N,A,M),_) ->
	    true
	;
	    true
	).
'$discontiguous'(P,M) :-
	'$do_error'(type_error(predicate_indicator,P),M:discontiguous(P)).

%
% did we declare multifile properly?
%
'$check_multifile_pred'(Hd, M, _) :-
	functor(Hd,Na,Ar),
	nb_getval('$consulting_file',F),
	recorded('$multifile_defs','$defined'(F,Na,Ar,M),_), !.
% oops, we did not.
'$check_multifile_pred'(Hd, M, Fl) :-
	% so this is not a multi-file predicate any longer.
	functor(Hd,Na,Ar),
	NFl is \(0x20000000) /\ Fl,
	'$flags'(Hd,M,Fl,NFl),
	'$warn_mfile'(Na,Ar).

'$warn_mfile'(F,A) :-
	write(user_error,'% Warning: predicate '),
	write(user_error,F/A), write(user_error,' was a multifile predicate '),
	write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,')'),
	nl(user_error).	



