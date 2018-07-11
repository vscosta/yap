/**
 ** @file imports.yapi
 *
 * @brief Module systemm code to import predicates
 *
 * This code does not provide visible builtins.
 */

/**
 *   @ingroup ModuleBuiltins
 *   @{
 *
 * YAP follows the following protovol:
 *     - predicate is in current module;
 *     - predicate is in user
 *     - predicate will be autoloaded, SWI style.
 */
:- '$mk_dynamic'('$parent_module'(_,_),prolog).


'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
	recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G0I).
% SWI builtin
'$get_undefined_predicates'(G, _ImportingMod, G, user) :-
	nonvar(G),
	'$pred_exists'(G, user).
% autoload
'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
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
% parent module mechanism
'$get_undefined_predicates'(G, ImportingMod, G0, ExportingMod) :-
	'$parent_module'(ImportingMod,ExportingModI),
	'$continue_imported'(ExportingMod, ExportingModI, G0, G).
'$get_undefined_predicates'(G, _ImportingMod, G0, ExportingMod) :-
	yap_flag(default_parent_module,ExportingModI),
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

	/**
	 *
	 * @}
	 */

