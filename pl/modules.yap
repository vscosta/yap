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

:- '$switch_log_upd'(1).

use_module(M) :-
	'$use_module'(M).

'$use_module'(V) :- var(V), !,
	'$do_error'(instantiation_error,use_module(V)).
'$use_module'([]) :- !.
'$use_module'([A|B]) :- !,
	'$use_module'(A),
	'$use_module'(B).
'$use_module'(M:F) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$use_module'(F),
        '$change_module'(M0).
'$use_module'(File) :- 
	'$find_in_path'(File,X,use_module(File)), !,
	(  '$recorded'('$module','$module'(_,X,Publics),_) ->
		'$use_module'(File,Publics)
	;
		'$ensure_loaded'(File)
	).
'$use_module'(File) :-
	'$do_error'(permission_error(input,stream,File),use_module(File)).
	

use_module(M,I) :-
	'$use_module'(M, I).

'$use_module'(File,Imports) :- var(File), !,
	'$do_error'(instantiation_error,use_module(File,Imports)).
'$use_module'(File,Imports) :- var(Imports), !,
	'$do_error'(instantiation_error,use_module(File,Imports)).	
'$use_module'(M:F, Imports) :- atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$use_module'(F, Imports),
        '$change_module'(M0).
'$use_module'(File,Imports) :-
	'$current_module'(M),
	'$find_in_path'(File,X,use_module(File,Imports)), !,
	'$open'(X,'$csult',Stream,0), !,
	( '$loaded'(Stream,TrueFileName) -> true
	     ;
	       % the following avoids import of all public predicates
	     '$consulting_file_name'(Stream,TrueFileName),
	     '$recorda'('$importing','$importing'(TrueFileName),R),
	     '$reconsult'(File,Stream)
	 ),
	 '$close'(Stream),
	 ( var(R) -> true; erased(R) -> true; erase(R)),
	 ( '$recorded'('$module','$module'(TrueFileName,Mod,Publics),_) ->
	     '$use_preds'(Imports,Publics,Mod,M)
	 ;
	 '$format'(user_error,"[ use_module/2 can not find a module in file ~w]~n",File),
	 fail
	 ).
'$use_module'(File,Imports) :-
	'$do_error'(permission_error(input,stream,File),use_module(File,Imports)).
	
use_module(Mod,F,I) :-
	'$use_module'(Mod,F,I).

'$use_module'(Module,M:File,Imports) :- !,
	atom(M), !,
        '$current_module'(M0),
        '$change_module'(M),
        '$use_module'(Module,File,Imports),
        '$change_module'(M0).
'$use_module'(Module,File,Imports) :-
	'$find_in_path'(File,X,use_module(Module,File,Imports)),
	'$open'(X,'$csult',Stream,0), !,
	'$current_module'(M),
	'$file_name'(Stream,FName),
	(
	  '$loaded'(Stream, TrueFileName)
	  ->
	  true
	;
	  '$consulting_file_name'(Stream,TrueFileName),
	  % the following avoids import of all public predicates
	  '$recorda'('$importing','$importing'(TrueFileName),R),
	  '$reconsult'(File,Stream)
	  ),
	'$close'(Stream),
	( var(R) -> true; erased(R) -> true; erase(R)),
	(
	  '$recorded'('$module','$module'(TrueFileName,Module,Publics),_)
	  ->
	  '$use_preds'(Imports,Publics,Module,M)
	  ;
	  '$format'(user_error,"[ use_module/2 can not find module ~w in file ~w]~n",[Module,File]),
	  fail
	).
'$use_module'(Module,File,Imports) :-
	'$do_error'(permission_error(input,stream,File),use_module(Module,File,Imports)).
	
'$consulting_file_name'(Stream,F)  :-
	'$file_name'(Stream, F).


'$module'(reconsult,N,P) :- !,
	'$abolish_module_data'(N),
	'$module_dec'(N,P).
'$module'(consult,N,P) :-
	( '$recorded'('$module','$module'(F,N,_),_),
	     '$format'(user_error,"[ Module ~w was already defined in file ~w]~n",[N,F]),
		'$abolish_module_data'(N),
		fail
	;
	   	true
	),
	'$module_dec'(N,P).

'$module'(O,N,P,Opts) :- !,
	'$module'(O,N,P),
	'$process_module_decls_options'(Opts,module(Opts,N,P)).

	
'$process_module_decls_options'(Var,Mod) :-
	var(Var), 
	'$do_error'(instantiation_error,Mod).
'$process_module_decls_options'([],_).
'$process_module_decls_options'([H|L],M) :-
	'$process_module_decls_option'(H,M),
	'$process_module_decls_options'(L,M).
'$process_module_decls_options'(T,M) :-
	'$do_error'(type_error(list,T),M).

'$process_module_decls_option'(Var,M) :- 
	var(Var), 
	'$do_error'(instantiation_error,M).
'$process_module_decls_option'(At,_) :- 
	atom(At), 
	'$use_module'(At).
'$process_module_decls_option'(library(L),_) :- 
	'$use_module'(library(L)).
'$process_module_decls_option'(hidden(Bool),M) :- 
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
	'$recorda'('$system_initialisation', source_mode(New,Old), _).

module(N) :-
	var(N), 
	'$do_error'(instantiation_error,module(N)).
module(N) :-
	atom(N), !,
	'$current_module'(_,N),
	'$get_value'('$consulting_file',F),
	( recordzifnot('$module','$module'(N),_) -> true; true),
	( recorded('$module','$module'(F,N,[]),_) ->
	  true ;
	  recorda('$module','$module'(F,N,[]),_)
	).
module(N) :-
	'$do_error'(type_error(atom,N),module(N)).

'$module_dec'(N,P) :-
	'$current_module'(Old,N),
	'$get_value'('$consulting_file',F),
	( recordzifnot('$module','$module'(N),_) -> true; true),
	recorda('$module','$module'(F,N,P),_),
	( '$recorded'('$importing','$importing'(F),_) ->
	         true
	;
	 		'$import'(P,N,Old)
	).

'$import'([],_,_) :- !.
'$import'([N/K|L],M,T) :-
	integer(K), atom(N), !,
	( '$check_import'(M,T,N,K) ->
%	    '$format'(user_error,"[vsc1: Importing ~w to ~w]~n",[M:N/K,T]),
	     ( T = user ->
	       ( recordzifnot('$import','$import'(M,user,N,K),_) -> true ; true)
             ;
	       ( recordaifnot('$import','$import'(M,T,N,K),_) -> true ; true )
             )
	 ;
	    true
	),
	'$import'(L,M,T).
'$import'([PS|L],M,T) :-
	'$format'(user_error,"[Illegal pred specification(~w) in module declaration for module ~w]~n",[PS,M]),
	'$import'(L,M,T).

'$check_import'(M,T,N,K) :-
    '$recorded'('$import','$import'(M1,T0,N,K),R), T0 == T, M1 \= M, /* ZP */ !,
    '$format'(user_error,"NAME CLASH: ~w was already imported to module ~w;~n",[M1:N/K,T]),
    '$format'(user_error,"            Do you want to import it from ~w ? [y or n] ",M),
    repeat,
	get0(C), '$skipeol'(C),
	( C is "y" -> erase(R), !;
	  C is "n" -> !, fail;
	  write(user_error, ' Please answer with ''y'' or ''n'' '), fail
	).
'$check_import'(_,_,_,_).

% $use_preds(Imports,Publics,Mod,M)
'$use_preds'(M:L,Publics,Mod,_) :-
	'$use_preds'(L,Publics,Mod,M).
'$use_preds'([],_,_,_) :- !.
'$use_preds'([P|Ps],Publics,Mod,M) :- !,
	'$use_preds'(P,Publics,Mod,M),
	'$use_preds'(Ps,Publics,Mod,M).
'$use_preds'(N/K,Publics,M,Mod) :-
    (  '$member'(N/K,Publics) -> true ;
	print_message(warning,import(N/K,Mod,M,private))
    ),
    ( '$check_import'(M,Mod,N,K) -> 
	%	     '$format'(user_error,'[ Importing ~w to ~w]~n',[M:N/K,Mod]),
        %            '$trace_module'(importing(M:N/K,Mod)),
%         '$format'(user_error,"[vsc2: Importing ~w to ~w]~n",[M:N/K,T]),
	  (Mod = user ->
             ( recordzifnot('$import','$import'(M,user,N,K),_) -> true ; true )
	     ;
	     ( recordaifnot('$import','$import'(M,Mod,N,K),_) -> true ; true )
          )
	 ;
	   true
    ). 

'$abolish_module_data'(M) :-
	'$current_module'(T),
	( '$recorded'('$import','$import'(M,T0,_,_),R), T0 == T, erase(R), fail; true),
	'$recorded'('$module','$module'(_,M,_),R),
	erase(R),
	fail.
'$abolish_module_data'(_).


% expand module names in a clause
'$module_expansion'(((Mod:H) :-B ),((Mod:H) :- B1),((Mod:H) :- BO),M) :- !,
	'$prepare_body_with_correct_modules'(B, M, B0),
	'$module_u_vars'(H,UVars,M),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(B0,B1,BO,M,M,M,UVars). % expand body
'$module_expansion'((H:-B),(H:-B1),(H:-BO),M) :-
	'$module_u_vars'(H,UVars,M),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(B,B1,BO,M,M,M,UVars). % expand body
%	$trace_module((H:-B),(H:-B1)).

% expand module names in a body
'$prepare_body_with_correct_modules'(V,M,call(M:V)) :- var(V), !.
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
'$prepare_body_with_correct_modules'(M:G,_,M:G) :- !.
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

%
% calling the meta-call expansion facility and expand_goal from
% a meta-call.
%
'$expand_goal'(G0, GoalMod, CurMod, G, NM) :-
	'$meta_expansion'(GoalMod, CurMod, G0, GF, []), !,
	'$expand_goal2'(GF,GoalMod,G,NM).
'$expand_goal'(G, GoalMod, _, NG, NM) :-
	'$expand_goal2'(G, GoalMod, NG, NM).

'$expand_goal2'(G, M, NG, NM) :-
	'$undefined'(G,M),
	functor(G,F,N),
	'$recorded'('$import','$import'(ExportingMod,M,F,N),_),
	ExportingMod \= M,
	!,
	'$expand_goal2'(G, ExportingMod, NG, NM).
'$expand_goal2'(G, M, GF, M) :-
	'$pred_goal_expansion_on',
	% make sure we do not try to expand conjs, etc...
	user:goal_expansion(G,M,GF0), !,
	% allow recursive goal expansion
	'$expand_goal2'(GF0,M,GF,M).
'$expand_goal2'(G, M, G, M). 
	
		
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
'$module_expansion'((A;B),(A1;B1),(AO;BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'((A|B),(A1|B1),(AO|BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
	'$module_expansion'(B,B1,BO,M,MM,TM,HVars).
'$module_expansion'((A->B),(A1->B1),(AO->BO),M,MM,TM,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,MM,TM,HVars),
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
'$module_expansion'(M:G,call(M:G),call(M:G),_,_,_,_) :- var(M), !.
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
'$module_expansion'(G, G1, GO, CurMod, MM, TM, HVars) :-
	% is this imported from some other module M1?
	( '$imported_pred'(G, CurMod, M1) ->
	    '$module_expansion'(G, G1, GO, M1, MM, TM, HVars)
	;
	( '$meta_expansion'(CurMod, MM, G, GI, HVars)
          ;
          GI = G
        ),
	'$complete_goal_expansion'(GI, CurMod, MM, TM, G1, GO, HVars)
	).


'$imported_pred'(G, ImportingMod, ExportingMod) :-
	'$undefined'(G, ImportingMod),
	functor(G,F,N),
	'$recorded'('$import','$import'(ExportingMod,ImportingMod,F,N),_),
	ExportingMod \= ImportingMod.

% args are:
%       goal to expand
%       current module for looking up pred
%       current module from top-level clause
%       goal to pass to compiler
%       goal to pass to listing
%       head variables.
'$complete_goal_expansion'(G, M, CM, TM, G1, G2, HVars) :-
	'$pred_goal_expansion_on',
	user:goal_expansion(G,M,GI), !,
	'$module_expansion'(GI,G1,G2,M,CM,TM,HVars).
'$complete_goal_expansion'(G, M, CM, TM, G1, G2, HVars) :-
	'$system_predicate'(G,M), !,
	'$c_built_in'(G,M,Gi),
	(Gi \== G ->
	   '$module_expansion'(Gi,Gi,G2,M,CM,TM,HVars) ;
	 TM = M ->
	    G2 = G, G1 = G,
	;
	    G2 = M:G, G1 = M:G % atts:
	).
'$complete_goal_expansion'(G, Mod, _, Mod, G, G, _) :- !.
'$complete_goal_expansion'(G, GMod, _, _, GMod:G, GMod:G, _).

% meta_predicate declaration
% records $meta_predicate(SourceModule,Functor,Arity,Declaration)

% directive now meta_predicate Ps :- $meta_predicate(Ps).

:- dynamic_predicate('$meta_predicate'/4,logical).

'$meta_predicate'((P,Ps), M) :- !,
	'$meta_predicate'(P, M),
	'$meta_predicate'(Ps, M).
'$meta_predicate'(M:D, _) :- !,
	'$meta_predicate'(D, M).
'$meta_predicate'(P, M1) :-
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
%	'$format'(user_error,"[expanding ~w:~w in ~w",[Mod,G,MP]),
	'$meta_expansion_loop'(N,D,G,G1,HVars,MP).
%	'$format'(user_error," gives ~w~n]",[G1]).

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
	'$all_current_modules'(Mod).

current_module(Mod,TFN) :-
	'$all_current_modules'(Mod),
	( '$recorded'('$module','$module'(TFN,Mod,_Publics),_) -> true ; TFN = user ).

source_module(Mod) :-
	'$current_module'(Mod).

'$member'(X,[X|_]) :- !.
'$member'(X,[_|L]) :- '$member'(X,L).

:- meta_predicate
%	[:,:],
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
	call_residue(:,?),
	catch(:,+,:),
	clause(:,?),
	clause(:,?,?),
	compile(:),
	consult(:),
	current_predicate(:),
	current_predicate(?,:),
	ensure_loaded(:),
	findall(?,:,?),
	findall(?,:,?,?),
	freeze(?,:),
	hide_predicate(:),
	if(:,:,:),
	incore(:),
	listing(:),
	nospy(:),
        not(:),
        once(:),
        phrase(:,?),
        phrase(:,?,+),
	predicate_property(:,?),
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

:- '$switch_log_upd'(0).

