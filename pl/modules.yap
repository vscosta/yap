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
* File:		modules.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	module support						 *
*									 *
*************************************************************************/
% module handling

'$consulting_file_name'(Stream,F)  :-
	'$file_name'(Stream, F).


'$module'(_,N,P) :-
	'$module_dec'(N,P).

'$module'(O,N,P,Opts) :- !,
	'$module'(O,N,P),
	'$process_module_decls_options'(Opts,module(Opts,N,P)).

	
'$process_module_decls_options'(Var,Mod) :-
	var(Var), !,
	'$do_error'(instantiation_error,Mod).
'$process_module_decls_options'([],_) :- !.
'$process_module_decls_options'([H|L],M) :- !,
	'$process_module_decls_option'(H,M),
	'$process_module_decls_options'(L,M).
'$process_module_decls_options'(T,M) :-
	'$do_error'(type_error(list,T),M).

'$process_module_decls_option'(Var,M) :- 
	var(Var), 
	'$do_error'(instantiation_error,M).
'$process_module_decls_option'(At,_) :- 
	atom(At), !,
	'$use_module'(At).
'$process_module_decls_option'(library(L),_) :- !,
	'$use_module'(library(L)).
'$process_module_decls_option'(hidden(Bool),M) :- !,
	'$process_hidden_module'(Bool, M).
'$process_module_decls_option'(Opt,M) :- 
	'$do_error'(domain_error(module_decl_options,Opt),M).

'$process_hidden_module'(TNew,M) :-
        '$convert_true_off_mod3'(TNew, New, M),
	source_mode(Old, New),
	'$prepare_restore_hidden'(Old,New).

'$convert_true_off_mod3'(true, off, _).
'$convert_true_off_mod3'(false, on, _).
'$convert_true_off_mod3'(X, _, M) :-
	'$do_error'(domain_error(module_decl_options,hidden(X)),M).

'$prepare_restore_hidden'(Old,Old) :- !.
'$prepare_restore_hidden'(Old,New) :-
	recorda('$system_initialisation', source_mode(New,Old), _).

module(N) :-
	var(N), 
	'$do_error'(instantiation_error,module(N)).
module(N) :-
	atom(N), !,
	% set it as current module.
	'$current_module'(_,N).
module(N) :-
	'$do_error'(type_error(atom,N),module(N)).

'$module_dec'(N,P) :-
	'$current_module'(_,N),
	nb_getval('$consulting_file',F),
	'$add_module_on_file'(N, F, P).

'$add_module_on_file'(Mod, F, Exports) :-
	recorded('$module','$module'(F0,Mod,_),R), !,
	'$add_preexisting_module_on_file'(F, F0, Mod, Exports, R).
'$add_module_on_file'(Mod, F, Exports) :-
	'$process_exports'(Exports,Mod,ExportedPreds),
	recorda('$module','$module'(F,Mod,ExportedPreds),_).

'$process_exports'([],_,[]).
'$process_exports'([Name/Arity|Exports],Mod,[Name/Arity|ExportedPreds]):- !,
	'$process_exports'(Exports,Mod,ExportedPreds).
'$process_exports'([op(Prio,Assoc,Name)|Exports],Mod,ExportedPreds) :- !,
	op(Prio,Assoc,Name),
	'$process_exports'(Exports,Mod,ExportedPreds).
'$process_exports'([Trash|_],Mod,_) :-
	'$do_error'(type_error(predicate_indicator,Trash),module(Mod,[Trash])).

% redefining a previously-defined file, no problem.
'$add_preexisting_module_on_file'(F, F, Mod, Exports, R) :- !,
	erase(R),
	( recorded('$import','$import'(Mod,_,_,_,_,_),R), erase(R), fail; true),
	recorda('$module','$module'(F,Mod,Exports),_).
'$add_preexisting_module_on_file'(F,F0,Mod,Exports,R) :-
	repeat,
	format(user_error, "The module ~a is being redefined.~n    Old file:  ~a~n    New file:  ~a~nDo you really want to redefine it? (y or n)",[Mod,F0,F]),
	'$mod_scan'(C), !,
	( C is "y" ->
	    '$add_preexisting_module_on_file'(F, F, Mod, Exports, R)
	 ;
	    '$do_error'(permission_error(module,redefined,Mod),module(Mod,Exports))
	).

'$mod_scan'(C) :-
	get0(C),
	'$skipeol'(C),
	(C is "y" ; C is "n").

'$import'([],_,_) :- !.
'$import'([N/K|L],M,T) :-
	integer(K), atom(N), !,
	'$do_import'(N, K, M, T),
	'$import'(L,M,T).
'$import'([PS|L],_,_) :-
	'$do_error'(domain_error(predicate_spec,PS),import([PS|L])).

% $use_preds(Imports,Publics,Mod,M)
'$use_preds'(Imports,Publics,Mod,M) :- var(Imports), !,
	'$import'(Publics,Mod,M).
'$use_preds'(M:L,Publics,Mod,_) :-
	'$use_preds'(L,Publics,Mod,M).
'$use_preds'([],_,_,_) :- !.
'$use_preds'([P|Ps],Publics,Mod,M) :- !,
	'$use_preds'(P,Publics,Mod,M),
	'$use_preds'(Ps,Publics,Mod,M).
'$use_preds'(N/K,Publics,M,Mod) :-
    (  lists:memberchk(N/K,Publics) -> true ;
	print_message(warning,import(N/K,Mod,M,private))
    ),
    '$do_import'(N, K, M, Mod).
 

'$do_import'(N, K, M, T) :-
	functor(G,N,K),
	'$follow_import_chain'(M,G,M0,G0),
	functor(G0,N1,K),
	( '$check_import'(M0,T,N1,K) ->
	  ( T = user ->
	    ( recordzifnot('$import','$import'(M0,user,G0,G,N,K),_) -> true ; true)
	  ;
	    ( recordaifnot('$import','$import'(M0,T,G0,G,N,K),_) -> true ; true )
	  )
	;
	  true
	).

'$follow_import_chain'(M,G,M0,G0) :-
	recorded('$import','$import'(M1,M,G1,G,_,_),_), !,
	'$follow_import_chain'(M1,G1,M0,G0).
'$follow_import_chain'(M,G,M,G).

'$check_import'(M,T,N,K) :-
   recorded('$import','$import'(MI,T,_,_,N,K),R),
    \+ '$module_produced by'(M,T,N,K), !,
    format(user_error,"NAME CLASH: ~w was already imported to module ~w;~n",[MI:N/K,T]),
    format(user_error,"            Do you want to import it from ~w ? [y or n] ",M),
    repeat,
	get0(C), '$skipeol'(C),
	( C is "y" -> erase(R), !;
	  C is "n" -> !, fail;
	  format(user_error, ' Please answer with ''y'' or ''n'' ',[]), fail
	).
'$check_import'(_,_,_,_).

'$module_produced by'(M,M0,N,K) :-
	recorded('$import','$import'(M,M0,_,_,N,K),_), !.
'$module_produced by'(M,M0,N,K) :-
	recorded('$import','$import'(MI,M0,G1,_,N,K),_),
	functor(G1, N1, K1),
	'$module_produced by'(M,MI,N1,K1).	


% expand module names in a clause
'$module_expansion'(((Mod:H) :- B ),((Mod:H) :- B1),((Mod:H) :- BO),M) :- !,
	'$is_mt'(Mod,H,B,IB,MM),
	'$prepare_body_with_correct_modules'(IB, M, B0),
	'$module_u_vars'(H,UVars,M),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(B0,B1,BO,M,MM,M,UVars).			% expand body
'$module_expansion'((H:-B),(H:-B1),(H:-BO),M) :-
	'$is_mt'(M,H,B,IB,MM),
	'$module_u_vars'(H,UVars,M),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(IB,B1,BO,M,MM,M,UVars).
%	$trace_module((H:-B),(H:-B1)).

% expand module names in a body
'$prepare_body_with_correct_modules'(V,M,M:call(V)) :- var(V), !.
'$prepare_body_with_correct_modules'((A,B),M,(A1,B1)) :- !,
	'$prepare_body_with_correct_modules'(A,M,A1),
	'$prepare_body_with_correct_modules'(B,M,B1).
'$prepare_body_with_correct_modules'((A;B),M,(A1;B1)) :- !,
	'$prepare_body_with_correct_modules'(A,M,A1),
	'$prepare_body_with_correct_modules'(B,M,B1).
'$prepare_body_with_correct_modules'((A->B),M,(A1->B1)) :- !,
	'$prepare_body_with_correct_modules'(A,M,A1),
	'$prepare_body_with_correct_modules'(B,M,B1).
'$prepare_body_with_correct_modules'(true,_,true) :- !.
'$prepare_body_with_correct_modules'(fail,_,fail) :- !.
'$prepare_body_with_correct_modules'(false,_,false) :- !.
'$prepare_body_with_correct_modules'(M:G,M:G) :- !.
'$prepare_body_with_correct_modules'(G,M,G) :-
	'$system_predicate'(G,M), !.
'$prepare_body_with_correct_modules'(G,M,M:G).


'$trace_module'(X) :-
	telling(F),
	tell('P0:debug'),
	write(X),nl,
	tell(F), fail.
'$trace_module'(_).

'$trace_module'(X,Y) :- X==Y, !.
'$trace_module'(X,Y) :-
	telling(F),
	tell('~/.dbg.modules'),
	write('***************'), nl,
	portray_clause(X),
	portray_clause(Y),
	tell(F),fail.
'$trace_module'(_,_).

	
% expand module names in a body
% args are:
%       goals to expand
%       code to pass to compiler
%       code to pass to listing
%       current module for looking up preds
%       current module for fixing up meta-call arguments
%       current module for predicate
%       head variables.
'$module_expansion'(V,call(MM:V),call(MM:V),_M,MM,_TM,_) :- var(V), !.
'$module_expansion'((A,B),(A1,B1),(AO,BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'((A*->B;C),(A1*->B1;C1),(yap_hacks:current_choicepoint(DCP),AO,yap_hacks:cut_at(DCP),BO; CO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AOO,M,MM,TM,HVars),
	'$clean_cuts'(AOO, AO),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars),
	'$module_expansion'(C,C1,CO,M,MM,TM,HVars).
'$module_expansion'((A;B),(A1;B1),(AO;BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'((A|B),(A1|B1),(AO|BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'((A->B),(A1->B1),(AO->BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AOO,M,MM,TM,HVars),
	'$clean_cuts'(AOO, AO),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'(\+A,\+A1,\+AO,M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars).
'$module_expansion'(not(A),not(A1),not(AO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars).
'$module_expansion'(true,true,true,_,_,_,_) :- !.
'$module_expansion'(fail,fail,fail,_,_,_,_) :- !.
'$module_expansion'(false,false,false,_,_,_,_) :- !.
% if I don't know what the module is, I cannot do anything to the goal,
% so I just put a call for later on.
'$module_expansion'(M:G,call(M:G),'$execute_wo_mod'(G,M),_,_,_,_) :- var(M), !.
'$module_expansion'(M:G,G1,GO,_,_,TM,HVars) :-
	'$module_expansion'(G,G1,GO,M,M,TM,HVars).
% if M1 is given explicitly process G within M1's context.
% '$module_expansion'(M:G,G1,GO,_Mod,_MM,TM,HVars) :- !,
% 	% is this imported from some other module M1?
% 	( '$imported_pred'(G, M, M1) ->
% 	    % continue recursively...
% 	    '$module_expansion'(G,G1,GO,M1,M,TM,HVars)
% 	;
% 	  (
% 	      '$meta_expansion'(M, M, G, NG, HVars)
% 	  ;
% 	      G = NG
% 	  ),
% 	  '$complete_goal_expansion'(NG, M, M, TM, G1, GO, HVars)
% 	).
%
% next, check if this is something imported.
%
% first, try doing goal_expansion
'$module_expansion'(G, G1, G0, CurMod, MM, TM, HVars) :-
	'$pred_goal_expansion_on',
	user:goal_expansion(G, CurMod, GI), !,
	'$module_expansion'(GI, G1, G0, CurMod, MM, TM, HVars).
'$module_expansion'(G, G1, GO, CurMod, MM, TM, HVars) :-
	% is this imported from some other module M1?
	( '$imported_pred'(G, CurMod, GG, M1) ->
	    '$module_expansion'(GG, G1, GO, M1, MM, TM, HVars)
	;
	  (
	   '$meta_expansion'(CurMod, MM, G, GI, HVars)
	  ->
	   true
	   ;
	   GI = G
	  ),
	  '$complete_goal_expansion'(GI, CurMod, MM, TM, G1, GO, HVars)
	).


% be careful here not to generate an undefined exception.
'$imported_pred'(G, ImportingMod, G0, ExportingMod) :-
	'$enter_undefp',
	'$undefined'(G, ImportingMod),
	recorded('$import','$import'(ExportingMod,ImportingMod,G0,G,_,_),_),
	ExportingMod \= ImportingMod, !,
	'$exit_undefp'.
'$imported_pred'(G, ImportingMod, G0, ExportingMod) :-
	'$undefined'(G, ImportingMod),
	swi:swi_predicate_table(ImportingMod,G,ExportingMod,G0),
	ExportingMod \= ImportingMod,
	'$exit_undefp'.
'$imported_pred'(G, ImportingMod, _, _) :-
	'$exit_undefp',
	fail.

% args are:
%       goal to expand
%       current module for looking up pred
%       current module from top-level clause
%       goal to pass to compiler
%       goal to pass to listing
%       head variables.
'$complete_goal_expansion'(G, M, CM, TM, G1, G2, HVars) :-
	'$all_system_predicate'(G,M), !,
	'$c_built_in'(G, M, Gi),
	(Gi \== G ->
	   '$module_expansion'(Gi, _, G2, M, CM, TM, HVars),
	    % make built-in processing transparent.
	    (TM = M -> G1 = G ; G1 = M:G)
	 ; TM = M ->
	    G2 = G, G1 = G
	;
	    G2 = M:G, G1 = M:G % atts:
	).
'$complete_goal_expansion'(G, Mod, _, Mod, G, G, _) :- !.
'$complete_goal_expansion'(G, GMod, _, _, GMod:G, GMod:G, _).


% module_transparent declaration
% 

:- dynamic('$module_transparent'/4).

'$module_transparent'((P,Ps), M) :- !, 
	'$module_transparent'(P, M),
	'$module_transparent'(Ps, M).
'$module_transparent'(M:D, _) :- !,
	'$module_transparent'(D, M).
'$module_transparent'(F/N, M) :-
	'$module_transparent'(F,M,N,_), !.
'$module_transparent'(F/N, M) :-
	functor(P,F,N),
	asserta(prolog:'$module_transparent'(F,M,N,P)),
	'$flags'(P, M, Fl, Fl),
	NFlags is Fl \/ 0x200004,
	'$flags'(P, M, Fl, NFlags).

'$is_mt'(M,H0,B,(context_module(CM),B),CM) :-
	'$module_transparent'(_,M,_,H), !.
'$is_mt'(M,_,B,B,M).

% meta_predicate declaration
% records $meta_predicate(SourceModule,Functor,Arity,Declaration)

% directive now meta_predicate Ps :- $meta_predicate(Ps).

:- dynamic('$meta_predicate'/4).

:- multifile '$meta_predicate'/4.

'$meta_predicate'((P,Ps), M) :- !, 
	'$meta_predicate'(P, M),
	'$meta_predicate'(Ps, M).
'$meta_predicate'(M:D, _) :- !,
	'$meta_predicate'(D, M).
'$meta_predicate'(P, M1) :-
	'$install_meta_predicate'(P, M1).


'$install_meta_predicate'(P, M1) :-
	functor(P,F,N),
	( M1 = prolog -> M = _ ; M1 = M),
	( retractall('$meta_predicate'(F,M,N,_)), fail ; true),
	asserta(prolog:'$meta_predicate'(F,M,N,P)),
	'$flags'(P, M1, Fl, Fl),
	NFlags is Fl \/ 0x200000,
	'$flags'(P, M1, Fl, NFlags).

% return list of vars in expanded positions on the head of a clause.
%
% these variables should not be expanded by meta-calls in the body of the goal.
%
'$module_u_vars'(H,UVars,M) :-
	functor(H,F,N),
	'$meta_predicate'(F,M,N,D), !,
	'$module_u_vars'(N,D,H,UVars).
'$module_u_vars'(_,[],_).

'$module_u_vars'(0,_,_,[]) :- !.
'$module_u_vars'(I,D,H,[Y|L]) :-
	arg(I,D,X), ( X=':' ; integer(X)),
	arg(I,H,Y), var(Y), !,
	I1 is I-1,
	'$module_u_vars'(I1,D,H,L).
'$module_u_vars'(I,D,H,L) :-
	I1 is I-1,
	'$module_u_vars'(I1,D,H,L).

% expand arguments of a meta-predicate
% $meta_expansion(ModuleWhereDefined,CurrentModule,Goal,ExpandedGoal,MetaVariables)

'$meta_expansion'(Mod,MP,G,G1,HVars) :- 
	functor(G,F,N),
	'$meta_predicate'(F,Mod,N,D), !,
	functor(G1,F,N),
	'$meta_expansion_loop'(N,D,G,G1,HVars,MP).
%	format(user_error," gives ~w~n]",[G1]).

% expand argument
'$meta_expansion_loop'(0,_,_,_,_,_) :- !.
'$meta_expansion_loop'(I,D,G,G1,HVars,M) :- 
	arg(I,D,X), (X==':' ; integer(X)),
	arg(I,G,A), '$do_expand'(A,HVars), !,
	'$process_expanded_arg'(A, M, NA),
	arg(I,G1,NA),
	I1 is I-1,
	'$meta_expansion_loop'(I1,D,G,G1,HVars,M).
'$meta_expansion_loop'(I,D,G,G1,HVars,M) :- 
	arg(I,G,A),
	arg(I,G1,A),
	I1 is I-1,
	'$meta_expansion_loop'(I1,D,G,G1,HVars,M).

% check if an argument should be expanded
'$do_expand'(V,HVars) :- var(V), !, '$not_in_vars'(V,HVars).
'$do_expand'(_:_,_) :- !, fail.
'$do_expand'(_,_).

'$process_expanded_arg'(V, M, M:V) :- var(V), !.
'$process_expanded_arg'((V1,V2), M, (NV1,NV2)) :- !,
	'$process_expanded_arg'(V1, M, NV1),
	'$process_expanded_arg'(V2, M, NV2).
'$process_expanded_arg'((V1;V2), M, (NV1;NV2)) :- !,
	'$process_expanded_arg'(V1, M, NV1),
	'$process_expanded_arg'(V2, M, NV2).
'$process_expanded_arg'((V1|V2), M, (NV1|NV2)) :- !,
	'$process_expanded_arg'(V1, M, NV1),
	'$process_expanded_arg'(V2, M, NV2).
'$process_expanded_arg'((V1->V2), M, (NV1->NV2)) :- !,
	'$process_expanded_arg'(V1, M, NV1),
	'$process_expanded_arg'(V2, M, NV2).
'$process_expanded_arg'(\+V, M, \+NV) :- !,
	'$process_expanded_arg'(V, M, NV).
'$process_expanded_arg'(M:A, _, M:A) :- !.
%'$process_expanded_arg'(G, M, G) :-
%	'$system_predicate'(G,M), !.
'$process_expanded_arg'(A, M, M:A).
	
'$not_in_vars'(_,[]).
'$not_in_vars'(V,[X|L]) :- X\==V, '$not_in_vars'(V,L).

current_module(Mod) :-
	'$all_current_modules'(Mod),
	\+ '$system_module'(Mod).

current_module(Mod,TFN) :-
	'$all_current_modules'(Mod),
	( recorded('$module','$module'(TFN,Mod,_Publics),_) -> true ; TFN = user ).

source_module(Mod) :-
	'$current_module'(Mod).

% comma has its own problems.
:- '$install_meta_predicate'((:,:), prolog).

:- meta_predicate
	abolish(:),
	abolish(:,+),
	all(?,:,?),
	assert(:),
	assert(:,+),
	assert_static(:),
	asserta(:),
	asserta(:,+),
	asserta_static(:),
	assertz(:),
	assertz(:,+),
	assertz_static(:),
	bagof(?,:,?),
	bb_get(:,-),
	bb_put(:,+),
	bb_delete(:,?),
	bb_update(:,?,?),
	call(:),
	call(:,?),
	call(:,?,?),
	call(:,?,?,?),
	call_with_args(:),
	call_with_args(:,?),
	call_with_args(:,?,?),
	call_with_args(:,?,?,?),
	call_with_args(:,?,?,?,?),
	call_with_args(:,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?,?,?),
	format(+,:),
	format(+,+,:),
	call_cleanup(:,:),
	call_cleanup(:,?,:),
	call_residue(:,?),
	catch(:,+,:),
	clause(:,?),
	clause(:,?,?),
	compile(:),
	consult(:),
	current_op(?,?,:),
	current_predicate(:),
	current_predicate(?,:),
	depth_bound_call(:,+),
	discontiguous(:),
	ensure_loaded(:),
	findall(?,:,?),
	findall(?,:,?,?),
	forall(:,:),
	freeze(?,:),
	hide_predicate(:),
	if(:,:,:),
	ignore(:),
	incore(:),
	listing(:),
	multifile(:),
	nospy(:),
        not(:),
        once(:),
        phrase(:,?),
        phrase(:,?,+),
	predicate_property(:,?),
	predicate_statistics(:,-,-,-),
	on_exception(+,:,:),
	reconsult(:),
	retract(:),
	retract(:,?),
	retractall(:),
	reconsult(:),
	setof(?,:,?),
	spy(:),
	unknown(+,:),
	use_module(:),
	use_module(:,?),
	use_module(?,:,?),
	when(?,:),
	with_mutex(+,:),
	^(+,:),
	\+ : .

%
% if we are asserting something in somewhere else's module,
% we need this little bird.
%
% assert((a:g :- b)) then SICStus compiles this into the original module.
% YAP is not 100% compatible, as it will transform this into:
%    a:assert(g :- user:b))
%
'$preprocess_clause_before_mod_change'((H:-B),M,M1,(H:-B1)) :-
	'$module_u_vars'(H,UVars,M1),
	'$preprocess_body_before_mod_change'(B,M,UVars,B1).

'$preprocess_body_before_mod_change'(V,M,_,call(M:V)) :- var(V), !.
'$preprocess_body_before_mod_change'((G1,G2),M,UVars,(NG1,NG2)) :- !,
	'$preprocess_body_before_mod_change'(G1,M,UVars,NG1),
	'$preprocess_body_before_mod_change'(G2,M,UVars,NG2).
'$preprocess_body_before_mod_change'((G1;G2),M,UVars,(NG1;NG2)) :- !,
	'$preprocess_body_before_mod_change'(G1,M,UVars,NG1),
	'$preprocess_body_before_mod_change'(G2,M,UVars,NG2).
'$preprocess_body_before_mod_change'((G1->G2),M,UVars,(NG1->NG2)) :- !,
	'$preprocess_body_before_mod_change'(G1,M,UVars,NG1),
	'$preprocess_body_before_mod_change'(G2,M,UVars,NG2).
'$preprocess_body_before_mod_change'(M:G,_,_,M:G) :- !.
'$preprocess_body_before_mod_change'(true,_,_,true) :- !.
'$preprocess_body_before_mod_change'(fail,_,_,fail) :- !.
'$preprocess_body_before_mod_change'(false,_,_,false) :- !.
'$preprocess_body_before_mod_change'(G,M,UVars,M:NG) :-
	'$meta_expansion'(M, M, G, NG, UVars), !.
'$preprocess_body_before_mod_change'(G,M,_,G) :-
	'$system_predicate'(G,M), !.
'$preprocess_body_before_mod_change'(G,M,_,M:G).


%
% get rid of a module and of all predicates included in the module.
%
abolish_module(Mod) :-
	recorded('$module','$module'(_,Mod,_),R), erase(R),
	fail.
abolish_module(Mod) :-
	recorded('$import','$import'(Mod,_,_,_,_,_),R), erase(R),
	fail.
abolish_module(Mod) :-
	'$current_predicate'(Mod,Na,Ar),
	abolish(Mod:Na/Ar),
	fail.
abolish_module(_).

'$reexport'(ModuleSource, Spec, Module) :-
	nb_getval('$consulting_file',TopFile),
	(
	 Spec == all
	->
	 Goal =	reexport(ModuleSource)
	;
	 Goal =	reexport(ModuleSource,Spec)
	),
	absolute_file_name(ModuleSource, File),
	'$load_files'(File, [if(not_loaded)], Goal),
	recorded('$module', '$module'(FullFile, Mod, Exports),_),
	atom_concat(File, _, FullFile), !,
	'$convert_for_reexport'(Spec, Exports, Tab, MyExports, Goal),
	'$add_to_imports'(Tab, Module, Mod),
	recorded('$lf_loaded','$lf_loaded'(TopFile,TopModule,_),_),
	recorded('$module', '$module'(CurrentFile, Module, ModExports), Ref),
	erase(Ref),
	lists:append(ModExports, MyExports, AllExports),
	recorda('$module', '$module'(CurrentFile, Module, AllExports), _),
	'$import'(MyExports, Module, TopModule).

'$convert_for_reexport'(all, Exports, Tab, MyExports, _) :-
	'$simple_conversion'(Exports, Tab, MyExports).
'$convert_for_reexport'([P1|Ps], Exports, Tab, MyExports, Goal) :-
	'$clean_conversion'([P1|Ps], Exports, Tab, MyExports, Goal).
'$convert_for_reexport'(except(List), Exports, Tab, MyExports, Goal) :-
	'$neg_conversion'(Exports, List, Tab, MyExports, Goal).

'$simple_conversion'([], [], []).
'$simple_conversion'([P|Exports], [P-P|Tab], [P|MyExports]) :-
	'$simple_conversion'(Exports, Tab, MyExports).

'$clean_conversion'([], _, [], [], _).
'$clean_conversion'([P1|Ps], List, [P1-P1|Tab], [P1|MyExports], Goal) :-
	lists:memberchk(P1, List), !,
	'$clean_conversion'(Ps, List, Tab, MyExports, Goal).
'$clean_conversion'([(N1/A1 as N2)|Ps], List, [N1/A1-N2/A1|Tab], [N2/A1|MyExports], Goal) :-
	lists:memberchk(N1/A1, List), !,
	'$clean_conversion'(Ps, List, Tab, MyExports, Goal).
'$clean_conversion'([P|_], _, _, _, Goal) :-
	'$do_error'(domain_error(module_reexport_predicates,P), Goal).
	
'$neg_conversion'([], _, [], [], _).
'$neg_conversion'([P1|Ps], List, Tab, MyExports, Goal) :-
	lists:memberchk(P1, List), !,
	'$neg_conversion'(Ps, List, Tab, MyExports, Goal).
'$neg_conversion'([N1/A1|Ps], List, [N1/A1-N2/A1|Tab], [N2/A1|MyExports], Goal) :-
	lists:memberchk(N1/A1 as N2, List), !,
	'$neg_conversion'(Ps, List, Tab, MyExports, Goal).
'$neg_conversion'([P|Ps], List, [P-P|Tab], [P|MyExports], Goal) :-
	'$neg_conversion'(Ps, List, Tab, MyExports, Goal).
	
'$add_to_imports'([], _, _).
'$add_to_imports'([N0/K0-N1/_|Tab], Mod, ModR) :-
	functor(G,N0,K0),
	G=..[N0|Args],
	G1=..[N1|Args],
	recordaifnot('$import','$import'(ModR,Mod,G,G1,N0,K0),_),
	'$add_to_imports'(Tab, Mod, ModR).

% I assume the clause has been processed, so the
% var case is long gone! Yes :)
'$clean_cuts'(G,(yap_hacks:current_choicepoint(DCP),NG)) :-
	'$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,G).

'$conj_has_cuts'(!,DCP,'$$cut_by'(DCP), ok) :- !. 
'$conj_has_cuts'((G1,G2),DCP,(NG1,NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1;G2),DCP,(NG1;NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1->G2),DCP,(G1;NG2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1*->G2),DCP,(G1;NG2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'(if(G1,G2,G3),DCP,if(G1,NG2,NG3), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G2, DCP, NG2, OK),
	'$conj_has_cuts'(G3, DCP, NG3, OK).
'$conj_has_cuts'(G,_,G, _).


