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


	% be careful here not to generate an undefined exception.
	'$generate_imported_predicate'(G, ImportingMod, G0, ExportingMod) :-
		(
		 recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_)
		 ;
		 '$parent_module'(ImportingMod,ExportingModI),
	\+ recorded('$import','$import'(ExportingModI,ImportingMod,G0I,G,_,_),_)
		),
		ImportingMod \= ExportingModI,
		(
		'$undefined'(G, ExportingModI)
		->
		'$generate_imported_predicate'(G, ExportingModI, G0, ExportingMod)
		;
		G=G0,
		ExportingModI=ExportingMod
		).

		/**
		   *
		   * @pred '$continue_imported'(+ModIn, +ModOut, +PredIn ,+PredOut)
		   *
		   * @return
		 */
		'$continue_imported'(Mod,Mod,Pred,Pred) :-
			'$pred_exists'(Pred, Mod),
		    !.
		'$continue_imported'(FM,Mod,FPred,Pred) :-
			recorded('$import','$import'(IM,Mod,IPred,Pred,_,_),_),
			'$continue_imported'(FM, IM, FPred, IPred), !.
		'$continue_imported'(FM,Mod,FPred,Pred) :-
			prolog:'$parent_module'(Mod,IM),
			'$continue_imported'(FM, IM, FPred, Pred).


		'$autoload'(G, _ImportingMod, ExportingMod, Dialect) :-
		    functor(G, Name, Arity),
		    '$pred_exists'(index(Name,Arity,ExportingMod,_),Dialect),
		    call(Dialect:index(Name,Arity,ExportingMod,_)),
		    !.
		'$autoload'(G, ImportingMod, ExportingMod, _Dialect) :-
		    functor(G, N, K),
			functor(G0, N, K),
			'$autoloader_find_predicate'(G0,ExportingMod),
			ExportingMod \= ImportingMod,
		    (recordzifnot('$import','$import'(ExportingMod,ImportingMod,G0,G0, N  ,K),_) -> true ; true ).


		'$autoloader_find_predicate'(G,ExportingModI) :-
			'__NB_getval__'('$autoloader_set', true, false), !,
			autoloader:find_predicate(G,ExportingModI).
		'$autoloader_find_predicate'(G,ExportingModI) :-
			yap_flag(autoload, true, false),
		    yap_flag( unknown, Unknown, fail),
			yap_flag(debug, Debug, false), !,
			load_files([library(autoloader),
				    autoloader:library('INDEX'),
				    swi:library('dialect/swi/INDEX')],
				   [autoload(true),if(not_loaded)]),
			nb_setval('$autoloader_set', true),
			yap_flag(autoload, _, true),
		    yap_flag( unknown, _, Unknown),
		    yap_flag( debug, _, Debug),
			autoloader:find_predicate(G,ExportingModI).




	/**
	 *
	 * @}
	 */
