/**
 ** @file imports.yap
 *
 * @brief Module systemm code to import predicates
 *
 * This code does not provide visible builtins.
 */

/**
 *   @addtogroup ModuleBuiltins
 *   @{
 *
 * YAP follows the following protovol:
 *     - predicate is in current module;
 *     - predicate is in user
 *     - predicate will be autoloaded, SWI style.
 */

:- '$mk_dynamic'('$parent_module'(_,_),prolog).

/** @pred mimp

debug import table

*/
mimp :-
    recorded('$import',I,_),
    %'$import'(ExportingMod,ImportingMod,G0,G,_,_),_),
writeln(I),
%(ImportingMod:G :- ExportingMod:G0)),
fail.


%:- start_low_level_trace.
% parent module mechanism
'$get_undefined_predicates'(ImportingMod:G,ExportingMod:G0) :-
	recorded('$import','$import'(ExportingMod,ImportingMod,G,G0,_,_),_)
    ->
    true
    ;
        %% this should have been caught before 
	'$is_system_predicate'(G, prolog)
    ->
    true
    ;
% autoload
    current_prolog_flag(autoload, true)
->
    '$autoload'(G, ImportingMod, ExportingMod, swi)
;
    '$parent_module'(ImportingMod, NewImportingMod)
    ->
    '$get_undefined_predicates'(NewImportingMod:G, ExportingMod:G0).

'$continue_imported'(Mod:Pred,Mod,Pred) :-
    '$pred_exists'(Pred, Mod),
    !.
'$continue_imported'(FM:FPred,Mod:Pred) :-
    '$get_undefined_predicates'(FM:FPred, ModI:PredI),
    '$continue_imported'(ModI:PredI,Mod:Pred).

%
'$get_undefined_pred'(ImportingMod:G, ExportingMod:G0) :-
    must_be_callable( ImportingMod:G ),
    '$get_undefined_predicates'(ImportingMod:G, ExportingMod:G0).

% be careful here not to generate an undefined exception.
'$imported_predicate'(ImportingMod:G, ExportingMod:G0) :-
   var(G) ->
    '$current_predicate'(_,G,ImportingMod,_),
    '$imported_predicate'(ImportingMod:G, ExportingMod:G0)
    ;
      var(ImportingMod) ->
      current_module(ImportingMod),
      '$imported_predicate'(ImportingMod:G, ExportingMod:G0)
      ;
      '$undefined'(G, ImportingMod),
      '$get_undefined_predicates'(ImportingMod:G, ExportingMod:G0),
      ExportingMod \= ImportingMod.
 
    
% check if current module redefines an imported predicate.
% and remove import.
%
'$not_imported'(H, Mod) :-
    recorded('$import','$import'(NM,Mod,NH,H,_,_),R),
    NM \= Mod,
    functor(NH,N,Ar),
    print_message(warning,redefine_imported(Mod,NM,N/Ar)),
    erase(R),
    fail.
'$not_imported'(_, _).


'$verify_import'(_M:G, prolog:G) :-
    '$is_system_predicate'(G, prolog).
'$verify_import'(M:G, NM:NG) :-
    '$get_undefined_predicates'(M:G, M, NM:NG),
    !.
'$verify_import'(MG, MG).




'$autoload'(G, _mportingMod, ExportingMod, Dialect) :-
    functor(G, Name, Arity),
    '$pred_exists'(index(Name,Arity,ExportingMod,_),Dialect),
    call(Dialect:index(Name,Arity,ExportingMod,_)),
    !.
'$autoload'(G, ImportingMod, ExportingMod, _Dialect) :-
    functor(G, N, K),
    functor(G0, N, K),
    '$autoloader_find_predicate'(G0,ExportingMod),
    ExportingMod \= ImportingMod,
    (recordzifnot('$import','$import'(ExportingMod,ImportingMod,G0,G0, N  ,K),_),
     \+ '$system_predicate'(G0,prolog)
    ->
     '$compile'((G:-ExportingMod:G0), reconsult ,(ImportingMod:G:-ExportingMod:G0), ImportingMod, _)
    ;
     true
    ).


'$autoloader_find_predicate'(G,ExportingMod) :-
    '__NB_getval__'('$autoloader_set', true, false), !,
    autoloader:find_predicate(G,ExportingMod).
'$autoloader_find_predicate'(G,ExportingMod) :-
    yap_flag(autoload, true, false),
    yap_flag( unknown, Unknown, fail),
    yap_flag(debug, Debug, false), !,
    load_files([library(autoloader)],[silent(true)]),
    nb_setval('$autoloader_set', true),
    yap_flag(autoload, _, true),
    yap_flag( unknown, _, Unknown),
    yap_flag( debug, _, Debug),
    autoloader:find_predicate(G,ExportingMod).




/**
	 *
	 * @}
	 */
