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
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	style checker Prolog					 *
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
style_check(discontiguous) :- '$syntax_check_mode'(_,on),
	'$syntax_check_discontiguous'(_,on).
style_check(multiple) :- '$syntax_check_mode'(_,on),
	'$syntax_check_multiple'(_,on).
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


'$check_term'(T,_) :-
	'$get_value'('$syntaxcheckdiscontiguous',on),
	'$current_module'(M),
	'$xtract_head'(T,M,NM,_,F,A),
	'$handle_discontiguous'(F,A,NM), fail.
'$check_term'(T,_) :-
	'$get_value'('$syntaxcheckmultiple',on),
	'$current_module'(M),
	'$xtract_head'(T,M,NM,_,F,A),
	'$handle_multiple'(F,A,NM), fail.
'$check_term'(T,VL) :-
	'$get_value'('$syntaxchecksinglevar',on),
	( '$chk_binding_vars'(T),
	  '$sv_list'(VL,Sv) ->
	  '$sv_warning'(Sv,T) ), fail.
'$check_term'(_,_).

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
	write(user_error,'[ Warning: singleton variable'),
	'$write_svs'(SVs),
	write(user_error,' in '),
	write(user_error,Name/Arity),
	write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,', clause '),
	( '$get_value'('$consulting',false),
	   '$first_clause_in_file'(Name,Arity, OM) ->
	    ClN = 1 ;
		'$number_of_clauses'(H,M,ClN0),
		ClN is ClN0+1
	),
	write(user_error,ClN), 
	write(user_error,') ]'),
	nl(user_error). 

'$xtract_head'(V,M,M,V,call,1) :- var(V), !.
'$xtract_head'((H:-_),OM,M,NH,Name,Arity) :- !,
        '$xtract_head'(H,OM,M,NH,Name,Arity).
'$xtract_head'((H,_),OM,M,H1,Name,Arity) :- !,
	'$xtract_head'(H,OM,M,H1,Name,Arity).
'$xtract_head'((H-->_),OM,M,HL,Name,Arity) :- !,
	'$xtract_head'(H,M,OM,M,Name,A1),
	Arity is A1+2,
	functor(HL,Name,Arity).
'$xtract_head'(M:H,_,NM,NH,Name,Arity) :- !,
	'$xtract_head'(H,M,NM,NH,Name,Arity).
'$xtract_head'(H,M,M,H,Name,Arity) :-
	functor(H,Name,Arity).

'$write_svs'([H]) :- !, write(user_error,' '), '$write_svs1'([H]).
'$write_svs'(SVs) :- write(user_error,'s '), '$write_svs1'(SVs).

'$write_svs1'([H]) :- !,
        '$write_str_in_stderr'(H).
'$write_svs1'([H|T]) :- 
        '$write_str_in_stderr'(H),
        write(user_error,','),
        '$write_svs1'(T).

'$write_str_in_stderr'([]).
'$write_str_in_stderr'([C|T]) :-
	put(user_error,C),
	'$write_str_in_stderr'(T).


'$handle_discontiguous'(F,A,M) :-
	'$recorded'('$discontiguous_defs','$df'(F,A,M),_), !.
'$handle_discontiguous'(F,A,M) :-
	'$in_this_file_before'(F,A,M),
	write(user_error,'[ Warning: discontiguous definition of '),
	write(user_error,F/A), write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,') ]'),
	nl(user_error).

'$handle_multiple'(F,A,M) :-
	\+ '$first_clause_in_file'(F,A,M), !.
'$handle_multiple'(_,_,_) :-
	'$get_value'('$consulting',true), !.
'$handle_multiple'(F,A,M) :-
	'$recorded'('$predicate_defs','$predicate_defs'(F,A,M,Fil),_), !,
	'$multiple_has_been_defined'(Fil, F/A, M), !.
'$handle_multiple'(F,A,M) :-
	( '$recorded'('$reconsulting',Fil,_) -> true ),
	'$recorda'('$predicate_defs','$predicate_defs'(F,A,M,Fil),_).

'$multiple_has_been_defined'(_, F/A, M) :-
	functor(S, F, A),
	'$is_multifile'(S, M), !.
'$multiple_has_been_defined'(Fil,P,_) :-
	'$recorded'('$reconsulting',F,_), !,
	'$test_if_well_reconsulting'(F,Fil,P).

'$test_if_well_reconsulting'(F,F,_) :- !.
'$test_if_well_reconsulting'(_,Fil,P) :-
	write(user_error,'[ Warning: predicate '),
	write(user_error,P), write(user_error,' already defined in '),
	write(user_error,Fil), write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,') ]'),
	nl(user_error).	

'$multifile'(V, _) :- var(V), !,
	throw(error(instantiation_error,multifile(V))).
'$multifile'((X,Y), M) :- '$multifile'(X, M), '$multifile'(Y, M).
'$multifile'(Mod:PredSpec, _) :- !,
	'$multifile'(PredSpec, Mod).
'$multifile'(N/A, M) :-
	'$get_value'('$consulting_file',F),
	'$recordzifnot'('$multifile_defs','$defined'(F,N,A,M),_),
	fail.
'$multifile'(N/A, M) :-
         functor(S,N,A),
	'$is_multifile'(S, M), !.
'$multifile'(N/A, M) :- !,
	'$new_multifile'(N,A,M).
'$multifile'(P, M) :-
	throw(error(type_error(predicate_indicator,P),multifile(M:P))).

'$discontiguous'(V,M) :- var(V), !,
	throw(error(instantiation_error,M:discontiguous(V))).
'$discontiguous'((X,Y),M) :- !,
	'$discontiguous'(X,M),
	'$discontiguous'(Y,M).
'$discontiguous'(M:A,_) :- !,
	'$discontiguous'(A,M).
'$discontiguous'(N/A, M) :- !,
	( '$recordzifnot'('$discontiguous_defs','$df'(N,A,M),_) ->
	    true
	;
	    true
	).
'$discontiguous'(P,M) :-
	throw(error(type_error(predicate_indicator,P),M:discontiguous(P))).

%
% did we declare multifile properly?
%
'$check_multifile_pred'(Hd, M, _) :-
	functor(Hd,Na,Ar),
	'$get_value'('$consulting_file',F),
	'$recorded'('$multifile_defs','$defined'(F,Na,Ar,M),_), !.
% oops, we did not.
'$check_multifile_pred'(Hd, M, Fl) :-
	% so this is not a multi-file predicate any longer.
	functor(Hd,Na,Ar),
	NFl is \(16'040000 ) /\ Fl,
	'$flags'(Hd,M,Fl,NFl),
	'$clear_multifile_pred'(Na,Ar,M),
	'$warn_mfile'(Na,Ar).

'$warn_mfile'(F,A) :-
	write(user_error,'[ Warning: predicate '),
	write(user_error,F/A), write(user_error,' was a multifile predicate '),
	write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,') ]'),
	nl(user_error).	

'$clear_multifile_pred'(Na,Ar,M) :-
	'$recorded'('$multifile_defs','$defined'(_,Na,Ar,M),R),
	erase(R),
	fail.
'$clear_multifile_pred'(Na,Ar,M) :-
	'$recorded'('$multifile'(_,_,_),'$mf'(Na,Ar,M,_,_),R),
	erase(R),
	fail.
'$clear_multifile_pred'(Na,Ar,M) :-
	'$recorded'('$multifile_dynamic'(_,_,_),'$mf'(Na,Ar,M,_,_),R),
	erase(R),
	fail.
'$clear_multifile_pred'(_,_,_).


