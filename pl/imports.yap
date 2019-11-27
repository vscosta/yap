
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

'$import'(G00,GF) :-
    (user:expand_term(G00,G0) -> true ; G00 = G0),
    '$yap_strip_module'(G0,M0,H0),
    '$_import'(M0:H0, GF),
    !.

'$_import'(M0:H0, GF) :-
    atom(M0),
    nonvar(H0),
    !,
    (
	'$pred_exists'(H0,M0)
    ->
    M0:H0 = GF
    ;
    '$import__'(M0:H0,[M0:H0],GF)
    ).


'$_import'(M0:H0, GF) :-
    var(M0),
    nonvar(H0),
    !,
    current_module(M0),
    '$_import'(M0:H0, GF).
'$_import'(M0:H0, GF) :-
     '$current_predicate'(_,M0,H0,_),
    '$import__'(M0:H0,[M0:H0],GF).

'$imports'(M0:H0,M0:H0) :-
      '$current_predicate'(_,M0,H0,_).
'$imports'(M0:H0,GF) :-
         recorded('$import','$import'(M,M0,H,H0,_,_),_),
    '$_import'(M:H, GF).

'$import__'(G0,Visited,GF) :-
   '$import_goal'(G0, G1),
    G1=M1:H1,
    \+ lists:memberchk(G1, Visited),
    (
    '$pred_exists'(H1,M1)
    ->
    GF=M1:H1,
    !
       ;
    '$import__'(G1,[G1|Visited],GF)
    ).

%:- start_low_level_trace.
% parent module mechanism
%% system has priority
%'$import_goal'(C, EC) :-
%		       '$expand_term'(C, top, EC).
'$import_goal'(_ImportingMod:G,prolog:G) :-
    nonvar(G),
    '$pred_exists'(G,prolog),
    !.
%% I am there, no need to import
'$import_goal'(Mod:Pred,Mod:Pred) :-
    nonvar(Pred),
    '$pred_exists'(Pred,Mod),
    !.
%% export table
'$import_goal'(ImportingMod:G,ExportingMod:G0) :-
    recorded('$import','$import'(ExportingMod,ImportingMod,G0,G,_,_),_).
%% parent/user
'$import_goal'(ImportingMod:G,IMod:NG) :-
    ( '$parent_module'(ImportingMod, PMod) ; PMod = user ),
    ImportingMod \= PMod,
    (
    '$pred_exists'(G,PMod)
    -> G= NG, PMod = IMod
    ;
    recorded('$import','$import'(PMod,IMod,_,NG,_,_),_)
%% parent/user
    ).
%% autoload`
%'$import_goal'(ImportingMod:G,ExportingMod:G) :-
%    current_prolog_flag(autoload, true),
%    '$autoload'(G, ImportingMod, ExportingMod, swi).



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
