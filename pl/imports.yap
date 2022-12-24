/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
****************              
*									 *
*************************************************************************/

/**
 *
 * @ file imports.yap
 * 
 * this file implements search for available predicates for the current module. Search can be called
 * at compile-time or at run-time.
 *
 * @defgroup PImport Predicate Import Mechanism
 * @ingroup YAPModules
 * @{
 * @brief steps to search for an undefined predicate
 * 
 * The import mechanism is as follows:
 *   - built-in (module prolog)
 *   - explicit imports (import table).
 *   - parent module mechanism.
 *   - SWI auto-loader.
 */
:- '$mk_dynamic'( prolog:'$parent_module'(_,_)).

% You can have a default parent (user)

'$pred_graph_edge'(ImportingMod:G1,ExportingModI:G) :-
    '$import'(ExportingModI,ImportingMod,G1,G,_,_).
'$pred_graph_edge'(_:G,ExportingModuleI:G)  :-
    current_prolog_flag(default_parent_module, ExportingModuleI),
    '$module'( _, ExportingModuleI, _, Exports),
    '$member'(G, Exports).
% parent module mechanism
'$pred_graph_edge'(ImportingMod:G, ExportingModI:G ) :-  
    '$parent_module'(ImportingMod,ExportingModI),
    '$module'( _, ExportingModI, _, Exports),
    '$member'(G, Exports).
  % autoload
'$pred_graph_edge'(_ImportingMod:G, ExportingModI:G ) :-  
    recorded('$dialect',swi,_),
    prolog_flag(autoload, true),
    prolog_flag(unknown, _OldUnk, fail),
    (
	'$module'( _, autoloader, _, _Exports)
    ->
    true
    ;
    use_module(library(autoloader))
    ;
    true
    ),
    autoload(G, _File, ExportingModI).

'$pred_path'(V:G, _Visited, _G)  :-
    (var(V);var(G)),
    !,
    fail.
'$pred_path'(Mod:G, _Visited, Mod:G)  :-
    '$pred_exists'(G, Mod),
    !.
'$pred_path'(G1, V,GF)  :-
    '$pred_graph_edge'(G1, G2),
    \+ '$member'(G2,V),
    '$pred_path'(G2, [G2|V], GF).


'$pred_candidate'(Mod:G, _Visited, Mod:G).
'$pred_candidate'(G1, V,GF)  :-
    '$pred_graph_edge'(G1, G2),
    \+ '$member'(G2,V),
    '$pred_candidate'(G2, [G2|V], GF).


'$imported_predicate'(MG,NMG) :-
    '$pred_path'(MG,[MG],NMG).

'$import_chain'(ImportingM,G,M0,G0) :-
    '$import'(ExportingM1,ImportingM,G1,G,_,_), 
    '$import_chain'(ExportingM1,G1,M0,G0).



