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
%% system has priority
'$get_predicate_definition'(_ImportingMod:G,prolog:G) :-
    nonvar(G).
%% I am there, no need to import
'$get_predicate_definition'(Mod:Pred,Mod:Pred) :-
    nonvar(Pred).
%% export table
'$get_predicate_definition'(ImportingMod:G,ExportingMod:G0) :-
    recorded('$import','$import'(ExportingMod,ImportingMod,G0,G,_,_),_).
%% parent/user
'$get_predicate_definition'(ImportingMod:G,PMod:G) :-
    ( '$parent_module'(ImportingMod, PMod) ; PMod = user ),
    ImportingMod \= PMod.
%% autoload`
%'$get_predicate_definition'(ImportingMod:G,ExportingMod:G) :-
%    current_prolog_flag(autoload, true),
%    '$autoload'(G, ImportingMod, ExportingMod, swi).


'$predicate_definition'(Imp:Pred,Exp:NPred) :-
    '$predicate_definition'(Imp:Pred,[],Exp:NPred),
    '$pred_exists'(NPred,Exp),
%writeln((Imp:Pred -> Exp:NPred )).
    !.

'$one_predicate_definition'(Imp:Pred,Exp:NPred) :-
    '$get_predicate_definition'(Imp:Pred,[],Exp:NPred),
    '$pred_exists'(NPred,Exp),
%writeln((Imp:Pred -> Exp:NPred )).
    !.
'$one_predicate_definition'(Exp:Pred,Exp:Pred).
    
'$predicate_definition'(M0:Pred0,Path,ModF:PredF) :-
    '$get_predicate_definition'(M0:Pred0, Mod:Pred),
    (
    '$pred_exists'(Pred,Mod), Mod = ModF, Pred = PredF
    ;
    \+ lists:member(Mod:Pred,Path),
      '$predicate_definition'(Mod:Pred,[Mod:Pred|Path], ModF:PredF)

    ).

%
'$get_undefined_predicate'(ImportingMod:G, ExportingMod:G0) :-
    must_be_callable( ImportingMod:G ),
    '$predicate_definition'(ImportingMod:G,[], ExportingMod:G0),
    ImportingMod:G \= ExportingMod:G0,
    !.

% be careful here not to generate an undefined exception.
'$imported_predicate'(ImportingMod:G, ExportingMod:G0) :-
   ( var(ImportingMod) ->
	current_module(ImportingMod)
   ;
   true
   ),
    (
	var(G) ->
       '$current_predicate'(_,G,ImportingMod,_)
   ;
   true
   ),
   (
       '$undefined'(G, ImportingMod)
   ->
      '$predicate_definition'(ImportingMod:G, ExportingMod:G0),
      ExportingMod \= ImportingMod
   ;
   ExportingMod = ImportingMod, G = G0
   ).
 
    
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
     \+ '$is_system_predicate'(G0, ExportingMod)
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
    setup_autoloader:find_predicate(G,ExportingMod).




/**
	 *
	 * @}
	 */
