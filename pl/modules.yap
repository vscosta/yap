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

use_module(V) :- var(V), !,
	throw(error(instantiation_error,use_module(V))).	
use_module([]) :- !.
use_module([A|B]) :- !,
	use_module(A),
	use_module(B).
use_module(File) :- 
	'$find_in_path'(File,X),
	(  '$recorded'('$module','$module'(_,X,Publics),_) ->
		use_module(File,Publics)
	;
		'$ensure_loaded'(File)
	).

use_module(File,Imports) :- var(File), !,
	throw(error(instantiation_error,use_module(File,Imports))).	
use_module(File,Imports) :- var(Imports), !,
	throw(error(instantiation_error,use_module(File,Imports))).	
use_module(File,Imports) :-
	atom(File), !,
	'$current_module'(M),
	'$find_in_path'(File,X),
	( open(X,$csult,Stream), !,
	'$consulting_file_name'(Stream,TrueFileName),
	( '$loaded'(Stream) -> true
	     ;
	     '$record_loaded'(Stream),
	       % the following avoids import of all public predicates
	     '$recorda'('$importing','$importing'(TrueFileName),R),
	     '$reconsult'(File,Stream)
	 ),
	 close(Stream),
	 ( var(R) -> true; erased(R) -> true; erase(R)),
	 ( '$recorded'('$module','$module'(TrueFileName,Mod,Publics),_) ->
	     $use_preds(Imports,Publics,Mod,M)
	 ;
	 format(user_error,'[ use_module/2 can not find a module in file ~w]~n',File),
	 fail
     )
    ;  
	throw(error(permission_error(input,stream,X),use_module(X,Imports)))
    ).
use_module(library(File),Imports) :- !,
	'$current_module'(M),
	'$find_in_path'(library(File),X),
	( open(X,$csult,Stream), !,
	'$consulting_file_name'(Stream,TrueFileName),
	( '$loaded'(Stream) -> true
	     ;
	     '$record_loaded'(Stream),
	       % the following avoids import of all public predicates
	     '$recorda'('$importing','$importing'(TrueFileName),R),
	     '$reconsult'(library(File),Stream)
	 ),
	 close(Stream),
	 ( var(R) -> true; erased(R) -> true; erase(R)),
	 ( '$recorded'('$module','$module'(TrueFileName,Mod,Publics),_) ->
	     $use_preds(Imports,Publics,Mod,M)
	 ;
	 format(user_error,'[ use_module/2 can not find a module in file ~w]~n',[File]),
	 fail
     )
    ;  
	throw(error(permission_error(input,stream,library(X)),use_module(library(X),Imports)))
    ).
use_module(V,Decls) :- 
	throw(error(type_error(atom,V),use_module(V,Decls))).
	
use_module(Module,File,Imports) :-
	'$current_module'(M),
	'$find_in_path'(File,X),
	( open(X,'$csult',Stream), !,
	'$consulting_file_name'(Stream,TrueFileName),
	( '$loaded'(Stream) -> true
	     ;
	     '$record_loaded'(Stream),
	       % the following avoids import of all public predicates
	     '$recorda'('$importing','$importing'(TrueFileName),R),
	     '$reconsult'(File,Stream)
	 ),
	 close(Stream),
	 ( var(R) -> true; erased(R) -> true; erase(R)),
	 ( '$recorded'('$module','$module'(TrueFileName,Module,Publics),_) ->
	     '$use_preds'(Imports,Publics,Module,M)
	 ;
	     format(user_error,'[ use_module/2 can not find module ~w in file ~w]~n',[Module,File]),
	 fail
     )
    ;  
	throw(error(permission_error(input,stream,library(X)),use_module(Module,File,Imports)))
    ).
use_module(Module,V,Decls) :- 
	throw(error(type_error(atom,V),use_module(Module,V,Decls))).
	
'$consulting_file_name'(Stream,F)  :-
	'$file_name'(Stream, F).


'$module'(reconsult,N,P) :- !,
	'$abolish_module_data'(N),
	'$module_dec'(N,P).
'$module'(consult,N,P) :-
	( '$recorded'('$module','$module'(F,N,_),_),
	     format(user_error,'[ Module ~w was already defined in file ~w]~n',[N,F]),
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
	throw(error(instantiation_error,Mod)).
'$process_module_decls_options'([],_).
'$process_module_decls_options'([H|L],M) :-
	'$process_module_decls_option'(H,M),
	'$process_module_decls_options'(L,M).
'$process_module_decls_options'(T,M) :-
	throw(error(type_error(list,T),M)).

'$process_module_decls_option'(Var,M) :- 
	var(Var), 
	throw(error(instantiation_error,M)).
'$process_module_decls_option'(At,_) :- 
	atom(At), 
	use_module(At).
'$process_module_decls_option'(library(L),_) :- 
	use_module(library(L)).
'$process_module_decls_option'(hidden(Bool),M) :- 
	'$process_hidden_module'(Bool, M).
'$process_module_decls_option'(Opt,M) :- 
	throw(error(domain_error(module_decl_options,Opt),M)).

'$process_hidden_module'(TNew,M) :-
        '$convert_true_off_mod3'(TNew, New, M),
	source_mode(Old, New),
	'$prepare_restore_hidden'(Old,New).

'$convert_true_off_mod3'(true, off, _).
'$convert_true_off_mod3'(false, on, _).
'$convert_true_off_mod3'(X, _, M) :-
	throw(error(domain_error(module_decl_options,hidden(X)),M)).

'$prepare_restore_hidden'(Old,Old) :- !.
'$prepare_restore_hidden'(Old,New) :-
	'$recorda'('$system_initialisation', source_mode(New,Old), _).

module(N) :-
	var(N), 
	throw(error(instantiation_error,module(N))).
module(N) :-
	atom(N), !,
	'$current_module'(Old,N),
	'$get_value'('$consulting_file',F),
	( recordzifnot('$module','$module'(N),_) -> true; true),
	( recorded('$module','$module'(F,N,[]),_) ->
	  true ;
	  recorda('$module','$module'(F,N,[]),_)
	).
module(N) :-
	throw(error(type_error(atom,N),module(N))).

'$module_dec'(N,P) :-
	$current_module(Old,N),
	$get_value('$consulting_file',F),
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
	( $check_import(M,T,N,K) ->
%	    format(user_error,'[Importing ~w to ~w]~n',[M:N/K,T]),
	     ( T = user ->
	       recordz('$import','$import'(M,_,N,K),_)
             ;
	       recorda('$import','$import'(M,T,N,K),_)
             )
	 ;
	    true
	),
	'$import'(L,M,T).
'$import'([PS|L],M,T) :-
	format(user_error,'[Illegal pred specification(~w) in module declaration for module ~w]~n',[PS,M]),
	'$import'(L,M,T).

$check_import(M,T,N,K) :-
    '$recorded'('$import','$import'(M1,T0,N,K),R), T0 == T, M1 \= M, /* ZP */ !,
    format(user_error,'NAME CLASH: ~w was already imported to module ~w;~n',[M1:N/K,T]),
    format(user_error,'            Do you want to import it from ~w ? [y or n] ',M),
    repeat,
	get0(C), $skipeol(C),
	( C is "y" -> erase(R), !;
	  C is "n" -> !, fail;
	  write(user_error, ' Please answer with ''y'' or ''n'' '), fail
	).
$check_import(_,_,_,_).

% $use_preds(Imports,Publics,Mod,M)
'$use_preds'([],_,_,_) :- !.
'$use_preds'([P|Ps],Publics,Mod,M) :- !,
	'$use_preds'(P,Publics,Mod,M),
	'$use_preds'(Ps,Publics,Mod,M).
'$use_preds'(N/K,Publics,M,Mod) :-
    (  '$member'(N/K,Publics) -> true ;
		format(user_error,'[ Warning: there is no exported predicate ~w in module ~w]~n',
			   [N/K,M])
    ),
    ( '$check_import'(M,Mod,N,K) -> 
	%	     format(user_error,'[ Importing ~w to ~w]~n',[M:N/K,Mod]),
        %            '$trace_module'(importing(M:N/K,Mod)),
	  (Mod = user ->
             recordz('$import','$import'(M,_,N,K),_)
	     ;
	     recorda('$import','$import'(M,Mod,N,K),_)
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
'$module_expansion'(((Mod:H) :-B ),((Mod:H) :- B1),((Mod:H) :- BO)) :- !,
	'$current_module'(M),
	'$prepare_body_with_correct_modules'(B, M, B0),
	'$module_u_vars'(H,UVars),	 % collect head variables in
					 % expanded positions
	'$module_expansion'(B0,B1,BO,M,UVars). % expand body
'$module_expansion'((H:-B),(H:-B1),(H:-BO)) :-
	'$module_u_vars'(H,UVars),	 % collect head variables in
					 % expanded positions
	'$current_module'(M),
	'$module_expansion'(B,B1,BO,M,UVars). % expand body
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
'$prepare_body_with_correct_modules'(G,_,G) :-
	'$system_predicate'(G), !.
'$prepare_body_with_correct_modules'(G,M,M:G).


$trace_module(X) :-
	telling(F),
	tell('P0:debug'),
	write(X),nl,
	tell(F), fail.
$trace_module(X).

$trace_module(X,Y) :- X==Y, !.
$trace_module(X,Y) :-
	telling(F),
	tell('~/.dbg.modules'),
	write('***************'), nl,
	portray_clause(X),
	portray_clause(Y),
	tell(F),fail.
$trace_module(X,Y).

%
% calling the meta-call expansion facility and expand_goal from
% a meta-call.
%
'$exec_with_expansion'(G0, GoalMod, CurMod) :-
	'$meta_expansion'(GoalMod, CurMod, G0, GF, []), !,
	'$mod_switch'(GoalMod,'$exec_with_expansion2'(GF,GoalMod)).
'$exec_with_expansion'(G, GoalMod, _) :-
	'$mod_switch'(GoalMod,'$exec_with_expansion2'(G,GoalMod)).

'$exec_with_expansion2'(G, M) :-
	'$pred_goal_expansion_on',
	user:goal_expansion(G,M,GF), !,
	'$execute'(M:GF).
'$exec_with_expansion2'(G, _) :- !,
	'$execute0'(G).
	
		
'$complete_goal_expansion'(G, M, _, G1, G2, HVars) :-
	'$pred_goal_expansion_on',
	user:goal_expansion(G,M,GI), !,
	'$prepare_body_with_correct_modules'(GI, M, GF),
	'$module_expansion'(GF,G1,G2,M,HVars).
'$complete_goal_expansion'(G, _, _, G, GF, _) :-
	'$system_predicate'(G), !,
	'$c_built_in'(G,GF).
'$complete_goal_expansion'(G, Mod, Mod, G, G, _) :- '$current_module'(Mod), !.
'$complete_goal_expansion'(G, GMod, _, GMod:G, GMod:G, _).


% expand module names in a body
'$module_expansion'(V,call(M:V),call(M:V),M,HVars) :- var(V), !.
'$module_expansion'((A,B),(A1,B1),(AO,BO),M,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,HVars),
	'$module_expansion'(B,B1,BO,M,HVars).
'$module_expansion'((A;B),(A1;B1),(AO;BO),M,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,HVars),
	'$module_expansion'(B,B1,BO,M,HVars).
'$module_expansion'((A->B),(A1->B1),(AO->BO),M,HVars) :- !,
	'$module_expansion'(A,A1,AO,M,HVars),
	'$module_expansion'(B,B1,BO,M,HVars).
'$module_expansion'(true,true,true,_,_) :- !.
'$module_expansion'(fail,fail,fail,_,_) :- !.
'$module_expansion'(false,false,false,_,_) :- !.
% if I don't know what the module is, I cannot do anything to the goal,
% so I just put a call for later on.
'$module_expansion'(M:G,call(M:G),call(M:G),_,HVars) :- var(M), !.
'$module_expansion'(M:(M1:G),G1,GO,M0,HVars) :- !,
	'$module_expansion'(M1:G,G1,GO,M0,HVars).
'$module_expansion'(M:G,G1,GO,Mod,HVars) :- !,
	% is this imported from some other module M1?
	( '$imported_pred'(G, M, M1) ->
	    % continue recursively...
	    '$module_expansion'(M1:G,G1,GO,Mod,HVars)
	;
	  (
	      '$meta_expansion_of_subgoal'(G, M, Mod, NG, HVars, NM) 
	  ;
	      G = NG, M = NM
	  ),
	  '$complete_goal_expansion'(NG, NM, Mod, G1, GO, HVars)
	).
%
% next, check if this is something imported.
%
'$module_expansion'(G, G1, GO, CurMod, HVars) :-
	% is this imported from some other module M1?
	( '$imported_pred'(G, CurMod, M1) ->
	    % continue recursively...
	    '$module_expansion'(M1:G,G1,GO,CurMod,HVars)
	;
	( '$meta_expansion_of_subgoal'(G, CurMod, CurMod, GI, HVars, GoalModule)
          ;
          GI = G, GoalModule = CurMod
        ),
	'$complete_goal_expansion'(GI, GoalModule, CurMod, G1, GO, HVars)
	).

'$meta_expansion_of_subgoal'(G, GMod, CurMod, GF, HVars, ImportedMod) :-
	functor(G,F,N),
	'$recorded'('$import','$import'(ImportedMod,GMod,F,N),_), !,
	'$meta_expansion'(ImportedMod, CurMod, G, GF, HVars).
'$meta_expansion_of_subgoal'(G, GMod, CurMod, NG, HVars, GMod) :-
	'$meta_expansion'(GMod, CurMod, G, NG, HVars).

'$imported_pred'(G, ImportingMod, ExportingMod) :-
	'$undefined'(ImportingMod:G),
	functor(G,F,N),
	'$recorded'('$import','$import'(ExportingMod,ImportingMod,F,N),_),
	ExportingMod \= ImportingMod.

% meta_predicate declaration
% records $meta_predicate(SourceModule,Functor,Arity,Declaration)

% directive now meta_predicate Ps :- $meta_predicate(Ps).

'$meta_predicate'((P,Ps)) :- !,
	$meta_predicate(P),
	$meta_predicate(Ps).
'$meta_predicate'(P) :-
	functor(P,F,N),
	'$current_module'(M1),
	( M1 = prolog -> M = _ ; M1 = M),
%	( '$recorded'($meta_predicate,$meta_predicate(M,F,N,_),R), erase(R), fail;
%	  true
%	),
%	recorda('$meta_predicate','$meta_predicate'(M,F,N,P),_),
	( retractall('$meta_predicate'(F,M,N,_)), fail ; true),
	asserta($meta_predicate(F,M,N,P)),
	'$flags'(P, Fl, Fl),
	NFlags is Fl \/ 0x200000,
	'$flags'(P, Fl, NFlags).	

% return list of vars in expanded positions on the head of a clause.
%
% these variables should not be expanded by meta-calls in the body of the goal.
%
'$module_u_vars'(H,UVars) :-
	functor(H,F,N),
	'$current_module'(M),
%	'$recorded'($meta_predicate,$meta_predicate(M,F,N,D),_), !,
	$meta_predicate(F,M,N,D), !,
	'$module_u_vars'(N,D,H,UVars).
'$module_u_vars'(H,[]).

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
%	'$recorded'('$meta_predicate','$meta_predicate'(Mod,F,N,D),_), !,
	'$meta_predicate'(F,Mod,N,D), !,
	functor(G1,F,N),
%	format(user_error,'[expanding ~w:~w in ~w',[Mod,G,MP]),
	'$meta_expansion_loop'(N,D,G,G1,HVars,MP).
%	format(user_error,' gives ~w~n]',[G1]).

% expand argument
'$meta_expansion_loop'(0,_,_,_,_,_) :- !.
'$meta_expansion_loop'(I,D,G,G1,HVars,M) :- 
	arg(I,D,X), (X==':' ; integer(X)),
	arg(I,G,A), '$do_expand'(A,HVars), !,
	arg(I,G1,M:A),
	I1 is I-1,
	'$meta_expansion_loop'(I1,D,G,G1,HVars,M).
'$meta_expansion_loop'(I,D,G,G1,HVars,M) :- 
	arg(I,G,A),
	arg(I,G1,A),
	I1 is I-1,
	'$meta_expansion_loop'(I1,D,G,G1,HVars,M).

% check if an argument should be expanded
'$do_expand'(V,HVars) :- var(V), !, '$not_in_vars'(V,HVars).
'$do_expand'(M:F,_) :- !, fail.
'$do_expand'(X,_).

$not_in_vars(_,[]).
$not_in_vars(V,[X|L]) :- X\==V, $not_in_vars(V,L).

current_module(Mod) :- 
	'$recorded'($module,'$module'(Mod),_).

current_module(Mod,TFN) :-
	'$recorded'('$module','$module'(TFN,Mod,_Publics),_).

source_module(Mod) :-
	'$current_module'(Mod).


$member(X,[X|_]) :- !.
$member(X,[_|L]) :- $member(X,L).

%
% this declaration should only be here, as meta_predicates should belong
% to the user module, not to the prolog module

:- meta_predicate
	abolish(:),
	abolish(:,+),
	all(?,:,?),
	assert(:),
	assert(:,+),
	asserta(:),
	asserta(:,+),
	assertz(:),
	assertz(:,+),
	bagof(?,:,?),
	call(:),
	clause(:,?),
	clause(:,?,?),
	current_predicate(:),
	current_predicate(?,:),
	findall(?,:,?),
	findall(?,:,?,?),
	incore(:),
	listing(?),
	nospy(:),
        not(:),
	retract(:),
	retract(:,?),
	retractall(:),
	setof(?,:,?),
	spy(:),
	^(+,:),
	\+(:),
	catch(:,+,:),
	on_exception(+,:,:),
	unknown(+,:),
	bb_get(:,-),
	bb_put(:,+),
	bb_delete(:,?),
	bb_update(:,?,?),
	call_with_args(:),
	call_with_args(:,?),
	call_with_args(:,?,?),
	call_with_args(:,?,?,?),
	call_with_args(:,?,?,?,?),
	call_with_args(:,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?,?),
	call_with_args(:,?,?,?,?,?,?,?,?,?).


%
% if we are asserting something in somewhere else's module,
% we need this little bird.
%
% assert((a:g :- b)) then SICStus compiles this into the original module.
% YAP is not 100% compatible, as it will transform this into:
%    a:assert(g :- user:b))
%
'$preprocess_clause_before_mod_change'((H:-B),M,M1,(H:-B1)) :-
	'$mod_switch'(M1,'$module_u_vars'(H,UVars)),
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
'$preprocess_body_before_mod_change'(G,_,_,G) :-
	'$system_predicate'(G), !.
'$preprocess_body_before_mod_change'(G,M,_,M:G).

	
:- '$switch_log_upd'(0).

