/**

  @file newmod.yap
  @brief support for creating a new module.
  */

 /**

  @addtogroup ModuleBuiltins
@{
  */


  /**
  @pred module(+M) is det

  set the type-in module


Defines  _M_ to be the current working or type-in module. All files
which are not bound to a module are assumed to belong to the working
module (also referred to as type-in module). To compile a non-module
file into a module which is not the working one, prefix the file name
with the module name, in the form ` _Module_: _File_`, when
loading the file.

**/
module(N) :-
	var(N),
	'$do_error'(instantiation_error,module(N)).
module(N) :-
	atom(N), !,
	% set it as current module.
	'$change_module'(N).
module(N) :-
	'$do_error'(type_error(atom,N),module(N)).

/**

This directive defines the file where it appears as a _module file_;
it must be the first declaration in the file.  _Module_ must be an
atom specifying the module name; _ExportList_ must be a list
containing the module's public predicates specification, in the form
`[predicate_name/arity,...]`. The _ExportList_ can  include
operator declarations for operators that are exported by the module.

The public predicates of a module file can be made accessible to other
files through loading the source file, using the directives
use_module/1 or use_module/2,
ensure_loaded/1 and the predicates
consult/1 or reconsult/1. The
non-public predicates of a module file are not supposed to be visible
to other modules; they can, however, be accessed by prefixing the module
name with the `:/2` operator.

**/
'$module_dec'(system(N, Ss), Ps) :- !,
		new_system_module(N),
    '$mk_system_predicates'( Ss , N ),
    '$module_dec'(N, Ps).
'$module_dec'(system(N), Ps) :- !,
		new_system_module(N),
%    '$mk_system_predicates'( Ps , N ),
    '$module_dec'(N, Ps).
'$module_dec'(N, Ps) :-
   source_location(F,Line),
  '__NB_getval__'( '$user_source_file', F0 , fail),
	'$add_module_on_file'(N, F, Line,F0, Ps),
	current_source_module(_M0,N).

'$mk_system_predicates'( Ps, _N ) :-
    lists:member(Name/A , Ps),
    '$new_system_predicate'(Name, A, prolog),
    fail.
'$mk_system_predicates'( _Ps, _N ).

/*
declare_module(Mod) -->
	arguments(file(+file:F),
		  line(+integer:L),
		  parent(+module:P),
		  type(+module_type:T),
		  exports(+list(exports):E),

		  Props, P0) -> true ; Props = P0),
	( deleteline(L), P0, P1) -> true ; P0 == P1),
	( delete(parent(P), P1, P2) -> true ; P1 == P2),
	( delete(line(L), P2, P3) -> true ; P3 == P4),
	( delete(file(F), Props, P0) -> true ; Props = P0),
	( delete(file(F), Props, P0) -> true ; Props = P0),
	( delete(file(F), Props, P0) -> true ; Props = P0),
	de
*/
'$module'(_,N,P) :-
	'$module_dec'(N,P).

/** set_module_property( +Mod, +Prop)

   Set a property for a module. Currently this includes:
     - base module, a module from where we automatically import all definitions, see add_import_module/2.
	 - the export list
	 - module class is currently ignored.
*/
set_module_property(Mod, base(Base)) :-
	must_be_of_type( module, Mod),
	add_import_module(Mod, Base, start).
set_module_property(Mod, exports(Exports)) :-
	must_be_of_type( module, Mod),
	'$add_module_on_file'(Mod, user_input, 1, '/dev/null', Exports).
set_module_property(Mod, exports(Exports, File, Line)) :-
	must_be_of_type( module, Mod),
	'$add_module_on_file'(Mod, File, Line, '/dev/null', Exports).
set_module_property(Mod, class(Class)) :-
	must_be_of_type( module, Mod),
	must_be_of_type( atom, Class).

    '$add_module_on_file'(DonorMod, DonorF, _LineF, SourceF, Exports) :-
        recorded('$module','$module'(OtherF, DonorMod, _, _, _, _),R),
        % the module has been found, are we reconsulting?
        (
    	DonorF \= OtherF
        ->
            '$do_error'(permission_error(module,redefined,DonorMod, OtherF, DonorF),module(DonorMod,Exports))
        ;
          recorded('$module','$module'(DonorF,DonorMod, SourceF,  _, _, _), R),
          erase( R ),
          fail
        ).
    '$add_module_on_file'(DonorM, DonorF, Line, SourceF, Exports) :-
        '$current_module'( HostM ),
        ( recorded('$module','$module'( HostF, HostM, _, _, _, _),_) -> true ; HostF = user_input ),
        % first build the initial export table
        '$convert_for_export'(all, Exports, DonorM, HostM, TranslationTab, AllExports0, load_files),
        '$sort'( AllExports0, AllExports ),
        '$add_to_imports'(TranslationTab, DonorM, DonorM), % insert ops, at least for now
        % last, export everything to the host: if the loading crashed you didn't actually do
        % no evil.
        recorda('$module','$module'(DonorF,DonorM,SourceF, AllExports, Line),_),
        ( recorded('$source_file','$source_file'( DonorF, Time, _), R), erase(R),
          recorda('$source_file','$source_file'( DonorF, Time, DonorM), _) ).


          '$convert_for_export'(all, Exports, _Module, _ContextModule, Tab, MyExports, _) :-
          	'$simple_conversion'(Exports, Tab, MyExports).
          '$convert_for_export'([], Exports, Module, ContextModule, Tab, MyExports, Goal) :-
          	'$clean_conversion'([], Exports, Module, ContextModule, Tab, MyExports, Goal).
          '$convert_for_export'([P1|Ps], Exports, Module, ContextModule, Tab, MyExports, Goal) :-
          	'$clean_conversion'([P1|Ps], Exports, Module, ContextModule, Tab, MyExports, Goal).
          '$convert_for_export'(except(Excepts), Exports, Module, ContextModule, Tab, MyExports, Goal) :-
          	'$neg_conversion'(Excepts, Exports, Module, ContextModule, MyExports, Goal),
          	'$simple_conversion'(MyExports, Tab, _).

          '$simple_conversion'([], [], []).
          '$simple_conversion'([F/N|Exports], [F/N-F/N|Tab], [F/N|E]) :-
          	'$simple_conversion'(Exports, Tab, E).
          '$simple_conversion'([F//N|Exports], [F/N2-F/N2|Tab], [F/N2|E]) :-
          	N2 is N+1,
          	'$simple_conversion'(Exports, Tab, E).
          '$simple_conversion'([F/N as NF|Exports], [F/N-NF/N|Tab], [NF/N|E]) :-
          	'$simple_conversion'(Exports, Tab, E).
          '$simple_conversion'([F//N as NF|Exports], [F/N2-NF/N2|Tab], [NF/N2|E]) :-
          	N2 is N+1,
          	'$simple_conversion'(Exports, Tab, E).
          '$simple_conversion'([op(Prio,Assoc,Name)|Exports], [op(Prio,Assoc,Name)|Tab], [op(Prio,Assoc,Name)|E]) :-
          	'$simple_conversion'(Exports, Tab, E).

          '$clean_conversion'([], _, _, _, [], [], _).
          '$clean_conversion'([(N1/A1 as N2)|Ps], List, Module, ContextModule, [N1/A1-N2/A1|Tab], [N2/A1|MyExports], Goal) :- !,
          	( lists:memberchk(N1/A1, List)
          	->
          	  true
          	;
          	  '$bad_export'((N1/A1 as N2), Module, ContextModule)
          	),
          	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
          '$clean_conversion'([N1/A1|Ps], List, Module, ContextModule, [N1/A1-N1/A1|Tab], [N1/A1|MyExports], Goal) :- !,
          	(
          	 lists:memberchk(N1/A1, List)
          	->
          	   true
          	;
          	  '$bad_export'(N1/A1, Module, ContextModule)
          	),
          	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
          '$clean_conversion'([N1//A1|Ps], List, Module, ContextModule, [N1/A2-N1/A2|Tab], [N1/A2|MyExports], Goal) :- !,
          	A2 is A1+2,
          	(
          	  lists:memberchk(N1/A2, List)
          	->
          	  true
          	;
          	  '$bad_export'(N1//A1, Module, ContextModule)

          	),
          	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
          '$clean_conversion'([N1//A1 as N2|Ps], List, Module, ContextModule, [N2/A2-N1/A2|Tab], [N2/A2|MyExports], Goal) :- !,
          	A2 is A1+2,
          	(
          	  lists:memberchk(N2/A2, List)
          	->
          	  true
          	;
          	  '$bad_export'((N1//A1 as A2), Module, ContextModule)
          	),
          	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
          '$clean_conversion'([op(Prio,Assoc,Name)|Ps], List, Module, ContextModule, [op(Prio,Assoc,Name)|Tab], [op(Prio,Assoc,Name)|MyExports], Goal) :- !,
          	(
          	 lists:memberchk(op(Prio,Assoc,Name), List)
          	->
          	 true
          	;
          	 '$bad_export'(op(Prio,Assoc,Name), Module, ContextModule)
          	),
          	'$clean_conversion'(Ps, List, Module, ContextModule, Tab, MyExports, Goal).
          '$clean_conversion'([P|_], _List, _, _, _, _, Goal) :-
          	'$do_error'(domain_error(module_export_predicates,P), Goal).

          '$bad_export'(_, _Module, _ContextModule) :- !.
          '$bad_export'(Name/Arity, Module, ContextModule) :-
          	functor(P, Name, Arity),
          	predicate_property(Module:P, _), !,
          	print_message(warning, declaration(Name/Arity, Module, ContextModule, private)).
          '$bad_export'(Name//Arity, Module, ContextModule) :-
          	Arity2 is Arity+2,
          	functor(P, Name, Arity2),
          	predicate_property(Module:P, _), !,
          	print_message(warning, declaration(Name/Arity, Module, ContextModule, private)).
          '$bad_export'(Indicator, Module, ContextModule) :- !,
          	print_message(warning, declaration( Indicator, Module, ContextModule, undefined)).

          '$neg_conversion'([], Exports, _, _, Exports, _).
          '$neg_conversion'([N1/A1|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
          	(
          	 lists:delete(List, N1/A1, RList)
          	->
          	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
          	;
          	 '$bad_export'(N1/A1, Module, ContextModule)
          	).
          '$neg_conversion'([N1//A1|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
          	A2 is A1+2,
          	(
          	 lists:delete(List, N1/A2, RList)
          	->
          	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
          	;
          	 '$bad_export'(N1//A1, Module, ContextModule)
          	).
          '$neg_conversion'([op(Prio,Assoc,Name)|Ps], List, Module, ContextModule, MyExports, Goal) :- !,
          	(
          	 lists:delete(List, op(Prio,Assoc,Name), RList)
          	->
          	 '$neg_conversion'(Ps, RList, Module, ContextModule, MyExports, Goal)
          	;
          	 '$bad_export'(op(Prio,Assoc,Name), Module, ContextModule)
          	).
          '$clean_conversion'([P|_], _List, _, _, _, Goal) :-
          	'$do_error'(domain_error(module_export_predicates,P), Goal).

%% @}
