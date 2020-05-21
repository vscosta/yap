/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
****************              
	;icate importing support						 *
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
 * 
 * @{
 * 
 * The import mechanism is as follows:
 *   - built-ina (module prolog)
 *   - explicit imports (import table).
 *   - parent module mechanism.
 *   - SWI auto-loader.
 */

:- '$mk_dynamic'('$parent_module'(_,_),prolog).

'$check_definition'(Mod:G, Visited, M:GF) :-
    (
      '$pred_exists'(G, Mod)
      ->
      Mod:G = M:GF
      ;
      '$pred_exists'(G, prolog)
      ->
            Mod:G = M:GF
      ;
      '$across_modules'(Mod:G, Visited, M:GF )
    ).

/**
 * LOok in the predicate grraph for possible ancestores,
 * 
 */
'$across_modules'(ImportingMod0:G0, Visited, ExportingMod:G) :-
    recorded('$import','$import'(ExportingModI,ImportingMod0,GI,G0,_,_),_),
    \+ lists:member(ExportingModI:GI, Visited),
    '$check_definition'(ExportingModI:GI, [ExportingModI:GI|Visited], ExportingMod:G).  
    
% You can have a default parent (user)                                                                                                               
'$across_modules'(_:G, Visited, ExportingMod:G)  :-
    current_prolog_flag(default_parent_module, ExportingModuleI),
    recorded('$module','$module'( _, ExportingModuleI, _, _, Exports), _),
    lists:member(G, Exports),
    \+ lists:member(ExportingModuleI:G, Visited),
    '$check_definition'(ExportingModuleI:G, [ExportingModuleI:G|Visited], ExportingMod:G).  
 % parent module mechanism
'$across_modules'(ImportingMod:G, Visited, ExportingMod:G ) :-  
    '$parent_module'(ImportingMod,ExportingModI),
    recorded('$module','$module'( _, ExportingModI, _, _, Exports), _),	lists:member(G, Exports),
	\+ lists:member(ExportingModI:G, Visited),
	'$check_definition'(ExportingModI , [ExportingModI:G|Visited], ExportingMod:G).  
  % autoload
'$across_modules'(_ImportingMod:G, Visited, ExportingMod:G ) :-  
    recorded('$dialect',swi,_),
    prolog_flag(autoload, true),
    prolog_flag(unknown, OldUnk, fail),
    (
	recorded('$module','$module'( _, autoloader, _, _, _Exports), _)
    ->
    true
    ;
    use_module(library(autoloader))
    ;
    true
    ),
    autoload(G, _File, ExportingModI), 
    \+ lists:member(ExportingModI:G, Visited),
    '$check_definition'(ExportingModI , [ExportingModI:G|Visited], ExportingMod:G).  

% be careful here not to generate an undefined exception.
'$imported_predicate'(G, ImportingMod, G, ImportingMod) :-
	nonvar(G), '$is_system_predicate'(G, prolog), !.
'$imported_predicate'(G, ImportingMod, G0, ExportingMod) :-
	nonvar(G),
	  '$undefined'(G, ImportingMod),
	  '$check_definition'(ImportingMod:G, [ImportingMod:G], ExportingMod:G0).

'$get_undefined'(G0, M0, G, M) :-
    '$check_definition'(M0:G0, [M0:G], M:G ).

