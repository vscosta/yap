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

/**
 *
 * @ file imports.yap
 * 
 * this file manages search for available predicates for the current module.
 *
 * @defgroup PImport Predicate Import Mechanism
 * 
 * The import mechanism is as follows:
 *   - built-ina (module prolog)
 *   - explicit imports (import table).
 *   - parent module mechanism.
 *   - SWI auto-loader.
 * /

:- '$mk_dynamic'('$parent_module'(_,_),prolog).

'$get_undefined_predicates'(G, Mod, G, Mod) :-
  !,
    '$pred_exists'(G,prolog).
	recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G0I).
'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
	recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G0I).
% SWI builtin
'$get_undefined_predicates'(G, _ImportingMod, G, user) :-
	nonvar(G),
	'$pred_exists'(G, user).
% parent module mechanism
'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
	'$parent_module'(ImportingMod,ExportingModI),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G).
% autoload
'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
    recorded('$dialect',swi,_),
    prolog_flag(autoload, true),
    prolog_flag(unknown, OldUnk, fail),
    (
     '$autoload'(G, ImportingMod, ExportingModI, swi)
    ->
     prolog_flag(unknown, _, OldUnk)
     ;
     prolog_flag(unknown, _, OldUnk),
     fail
     ),
    '$continue_imported'(ExportingMod, ExportingModI, G0, G).

'$get_undefined_pred'(G, ImportingMod, G0, ExportingMod) :-
    '$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod),
    !.


% be careful here not to generate an undefined exception.
'$imported_predicate'(G, _ImportingMod, G, prolog) :-
	nonvar(G), '$is_system_predicate'(G, prolog), !.
'$imported_predicate'(G, ImportingMod, G0, ExportingMod) :-
	( var(G) -> true ;
	  var(ImportingMod) -> true ;
	  '$undefined'(G, ImportingMod)
	),
	'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod),
	ExportingMod \= ImportingMod.