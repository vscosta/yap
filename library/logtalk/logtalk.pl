
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.25.1
%
%  Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operators
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% message sending operators

:- op(600, xfy, ::).			% send to object
:- op(600,  fy, ::).			% send to self

:- op(600,  fy, ^^).			% super call


% mode operators

:- op(200, fy, +).				% input argument (instantiated)
:- op(200, fy, ?).				% input/output argument
:- op(200, fy, @).				% input argument (not modified by the call)
:- op(200, fy, -).				% output argument (not instantiated)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  runtime directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% tables of defined events and monitors

:- dynamic('$lgt_before_'/5).				% '$lgt_before_'(Obj, Msg, Sender, Monitor, Call)
:- dynamic('$lgt_after_'/5).				% '$lgt_after_'(Obj, Msg, Sender, Monitor, Call)


% tables of loaded entities and respective relationships

:- dynamic('$lgt_current_protocol_'/3).		% '$lgt_current_protocol_'(Ptc, Prefix, Type)
:- dynamic('$lgt_current_category_'/3).		% '$lgt_current_category_'(Ctg, Prefix, Type)
:- dynamic('$lgt_current_object_'/6).		% '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, Type)

:- dynamic('$lgt_implements_protocol_'/3).	% '$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope)
:- dynamic('$lgt_imports_category_'/3).		% '$lgt_imports_category_'(ObjOrCtg, Ctg, Scope)
:- dynamic('$lgt_instantiates_class_'/3).	% '$lgt_instantiates_class_'(Instance, Class, Scope)
:- dynamic('$lgt_specializes_class_'/3).	% '$lgt_specializes_class_'(Class, Superclass, Scope)
:- dynamic('$lgt_extends_protocol_'/3).		% '$lgt_extends_protocol_'(Ptc1, Ptc2, Scope)
:- dynamic('$lgt_extends_object_'/3).		% '$lgt_extends_object_'(Prototype, Parent, Scope)


% debugger status and tables

:- dynamic('$lgt_debugging_'/1).			% '$lgt_debugging_'(Entity)

:- dynamic('$lgt_dbg_debugging_'/0).		% '$lgt_dbg_debugging_'
:- dynamic('$lgt_dbg_tracing_'/0).			% '$lgt_dbg_tracing_'
:- dynamic('$lgt_dbg_skipping_'/0).			% '$lgt_dbg_skipping_'
:- dynamic('$lgt_dbg_spying_'/2).			% '$lgt_dbg_spying_'(Functor, Arity)
:- dynamic('$lgt_dbg_spying_'/4).			% '$lgt_dbg_spying_'(Sender, This, Self, Goal)
:- dynamic('$lgt_dbg_leashing_'/1).			% '$lgt_dbg_leashing_'(Port)


% runtime flags

:- dynamic('$lgt_current_flag_'/2).			% '$lgt_current_flag_'(Option, Value)


% lookup caches for messages to an object, messages to self, and super calls

:- dynamic('$lgt_obj_lookup_cache_'/4).		% '$lgt_obj_lookup_cache_'(Obj, Pred, Sender, Call)
:- dynamic('$lgt_self_lookup_cache_'/4).	% '$lgt_self_lookup_cache_'(Obj, Pred, Sender, Call)
:- dynamic('$lgt_super_lookup_cache_'/5).	% '$lgt_super_lookup_cache_'(Self, Pred, This, Sender, Call)


% lookup caches for asserting and retracting dynamic facts

:- dynamic('$lgt_db_lookup_cache_'/6).		% '$lgt_db_lookup_cache_'(Obj, Pred, Sender, Scope, Call, UpdateGoal)


% table of library paths

:- dynamic(logtalk_library_path/2).			% logtalk_library_path(Library, Path)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor directives (used for source file compilation)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic('$lgt_pp_compiler_flag_'/2).			% '$lgt_pp_compiler_flag_'(Option, Value)

:- dynamic('$lgt_pp_dcl_'/1).					% '$lgt_pp_dcl_'(Clause)
:- dynamic('$lgt_pp_ddcl_'/1).					% '$lgt_pp_ddcl_'(Clause)
:- dynamic('$lgt_pp_def_'/1).					% '$lgt_pp_def_'(Clause)
:- dynamic('$lgt_pp_ddef_'/1).					% '$lgt_pp_ddef_'(Clause)
:- dynamic('$lgt_pp_super_'/1).					% '$lgt_pp_super_'(Clause)

:- dynamic('$lgt_pp_dynamic_'/2).				% '$lgt_pp_dynamic_'(Functor, Arity)
:- dynamic('$lgt_pp_discontiguous_'/2).			% '$lgt_pp_discontiguous_'(Functor, Arity)
:- dynamic('$lgt_pp_mode_'/2).					% '$lgt_pp_mode_'(Mode, Determinism)
:- dynamic('$lgt_pp_public_'/2).				% '$lgt_pp_public_'(Functor, Arity)
:- dynamic('$lgt_pp_protected_'/2).				% '$lgt_pp_protected_'(Functor, Arity)
:- dynamic('$lgt_pp_private_'/2).				% '$lgt_pp_private_'(Functor, Arity)
:- dynamic('$lgt_pp_metapredicate_'/1).			% '$lgt_pp_metapredicate_'(Pred)
:- dynamic('$lgt_pp_alias_'/3).					% '$lgt_pp_alias_'(Entity, Pred, Alias)
:- dynamic('$lgt_pp_non_terminal_'/3).			% '$lgt_pp_non_terminal_'(Functor, Args, Arity)

:- dynamic('$lgt_pp_object_'/11).				% '$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Mode)
:- dynamic('$lgt_pp_category_'/6).				% '$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Mode)
:- dynamic('$lgt_pp_protocol_'/5).				% '$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Mode)

:- dynamic('$lgt_pp_uses_'/1).					% '$lgt_pp_uses_'(Obj)
:- dynamic('$lgt_pp_uses_'/2).					% '$lgt_pp_uses_'(Obj, Predicate)
:- dynamic('$lgt_pp_calls_'/1).					% '$lgt_pp_calls_'(Entity)
:- dynamic('$lgt_pp_info_'/1).					% '$lgt_pp_info_'(List)
:- dynamic('$lgt_pp_info_'/2).					% '$lgt_pp_info_'(Functor/Arity, List) or '$lgt_pp_info_'(Functor//Args, List)

:- dynamic('$lgt_pp_implemented_protocol_'/4).	% '$lgt_pp_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)
:- dynamic('$lgt_pp_imported_category_'/5).		% '$lgt_pp_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_pp_extended_object_'/10).		% '$lgt_pp_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_instantiated_class_'/10).	% '$lgt_pp_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_specialized_class_'/10).	% '$lgt_pp_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_extended_protocol_'/4).		% '$lgt_pp_extended_protocol_'(Ptc, Prefix, Dcl, Scope)

:- dynamic('$lgt_pp_file_init_'/1).				% '$lgt_pp_file_init_'(Goal)	
:- dynamic('$lgt_pp_entity_init_'/2).			% '$lgt_pp_entity_init_'(Entity, Goal)

:- dynamic('$lgt_pp_entity_init_'/1).			% '$lgt_pp_entity_init_'(Goal)
:- dynamic('$lgt_pp_fentity_init_'/1).			% '$lgt_pp_fentity_init_'(Goal)

:- dynamic('$lgt_pp_redefined_built_in_'/3).	% '$lgt_pp_redefined_built_in_'(Head, Context, THead)

:- dynamic('$lgt_pp_directive_'/1).				% '$lgt_pp_directive_'(Dir)
:- dynamic('$lgt_pp_ppclause_'/1).				% '$lgt_pp_ppclause_'(Clause)
:- dynamic('$lgt_pp_rclause_'/1).				% '$lgt_pp_rclause_'(Clause)
:- dynamic('$lgt_pp_eclause_'/1).				% '$lgt_pp_eclause_'(Clause)
:- dynamic('$lgt_pp_feclause_'/1).				% '$lgt_pp_feclause_'(Clause)

:- dynamic('$lgt_pp_defs_pred_'/2).				% '$lgt_pp_defs_pred_'(Functor, Arity)
:- dynamic('$lgt_pp_calls_pred_'/2).			% '$lgt_pp_calls_pred_'(Functor, Arity)
:- dynamic('$lgt_non_portable_call_'/2).		% '$lgt_non_portable_call_'(Functor, Arity)

:- dynamic('$lgt_pp_referenced_object_'/1).		% '$lgt_pp_referenced_object_'(Object)
:- dynamic('$lgt_pp_referenced_protocol_'/1).	% '$lgt_pp_referenced_protocol_'(Protocol)
:- dynamic('$lgt_pp_referenced_category_'/1).	% '$lgt_pp_referenced_category_'(Category)

:- dynamic('$lgt_pp_global_op_'/3).				% '$lgt_pp_global_op_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_file_op_'/3).				% '$lgt_pp_file_op_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_entity_op_'/3).				% '$lgt_pp_entity_op_'(Priority, Specifier, Operator)

:- dynamic('$lgt_pp_warnings_top_argument_'/1).	% '$lgt_pp_warnings_top_argument_'(Term)
:- dynamic('$lgt_pp_comp_warnings_counter_'/1).	% '$lgt_pp_comp_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_load_warnings_counter_'/1).	% '$lgt_pp_load_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_entity_warnings_flag_'/0).	% '$lgt_pp_entity_warnings_flag_'




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  top level runtime predicate for message sending: ::/2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



Obj::Pred :-
	'$lgt_tr_msg'(Pred, Obj, Call, user),
	(('$lgt_dbg_debugging_', '$lgt_debugging_'(Obj)) ->
		'$lgt_ctx_ctx'(Ctx, user, user, Obj, '$lgt_po_user0_', []),
		catch('$lgt_dbg_goal'(Obj::Pred, Call, Ctx), Error, '$lgt_runtime_error_handler'(Error))
		;
		catch(Call, Error, '$lgt_runtime_error_handler'(Error))).



% '$lgt_runtime_error_handler'(@term)
%
% top-level runtime error handler

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), context(TFunctor2/TArity2, _))) :-	% SWI-Prolog
	'$lgt_reverse_predicate_functor'(TFunctor1, TArity1, Entity, Type, Functor1, Arity1),
	'$lgt_reverse_predicate_functor'(TFunctor2, TArity2, Entity, Type, Functor2, Arity2),
	throw(error(existence_error(procedure, Functor1/Arity1), context(Type, Entity, Functor2/Arity2))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), TFunctor2/TArity2)) :-				% GNU Prolog
	'$lgt_reverse_predicate_functor'(TFunctor1, TArity1, Entity, Type, Functor1, Arity1),
	'$lgt_reverse_predicate_functor'(TFunctor2, TArity2, Entity, Type, Functor2, Arity2),
	throw(error(existence_error(procedure, Functor1/Arity1), context(Type, Entity, Functor2/Arity2))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor/TArity), _)) :-
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, ModTFunctor/TArity), _)) :-								% CIAO
	atom_concat('user:', TFunctor, ModTFunctor),
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, ':'(_, TFunctor/TArity)), _)) :-
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(_, _, procedure, ':'(_, TFunctor/TArity), _), _)) :-					% SICStus Prolog
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(undefined_predicate(TFunctor, TArity, _), _, _)) :-									% XSB
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(undefined_predicate(TFunctor/TArity)) :-													% B-Prolog
	'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(logtalk_debugger_aborted)) :-
	write('Debugging session aborted by user. Debugger still on.'), nl,
	fail.

'$lgt_runtime_error_handler'(Error) :-
	throw(Error).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_object(?object_identifier)

current_object(Obj) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), current_object(Obj))).

current_object(Obj) :-
	'$lgt_current_object_'(Obj, _, _, _, _, _).



% current_protocol(?protocol_identifier)

current_protocol(Ptc) :-
	nonvar(Ptc),
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), current_protocol(Ptc))).

current_protocol(Ptc) :-
	'$lgt_current_protocol_'(Ptc, _, _).



% current_category(?category_identifier)

current_category(Ctg) :-
	nonvar(Ctg),
	\+ atom(Ctg),
	throw(error(type_error(category_identifier, Ctg), current_category(Ctg))).

current_category(Ctg) :-
	'$lgt_current_category_'(Ctg, _, _).



% object_property(?object_identifier, ?object_property)

object_property(Obj, Prop) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), object_property(Obj, Prop))).

object_property(Obj, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_entity_property'(Prop),
	throw(error(domain_error(object_property, Prop), object_property(Obj, Prop))).

object_property(user, built_in).
object_property(debugger, built_in).

object_property(Obj, Prop) :-
	'$lgt_current_object_'(Obj, _, _, _, _, Prop).



% category_property(?category_identifier, ?category_property)

category_property(Ctg, Prop) :-
	nonvar(Ctg),
	\+ atom(Ctg),
	throw(error(type_error(category_identifier, Ctg), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_entity_property'(Prop),
	throw(error(domain_error(category_property, Prop), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	'$lgt_current_category_'(Ctg, _, Prop).



% protocol_property(?protocol_identifier, ?protocol_property)

protocol_property(Ptc, Prop) :-
	nonvar(Ptc),
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_entity_property'(Prop),
	throw(error(domain_error(protocol_property, Prop), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	'$lgt_current_protocol_'(Ptc, _, Prop).



% create_object(+object_identifier, +list, +list, +list)

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Obj); var(Rels); var(Dirs); var(Clauses)),
	throw(error(instantiation_error, create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(permission_error(replace, object, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_current_category_'(Obj, _, _),
	throw(error(permission_error(replace, category, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_current_protocol_'(Obj, _, _),
	throw(error(permission_error(replace, protocol, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Rels),
	throw(error(type_error(list, Rels), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Dirs),
	throw(error(type_error(list, Dirs), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Clauses),
	throw(error(type_error(list, Clauses), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_clean_pp_clauses',
	'$lgt_tr_object_id'(Obj, (dynamic)),
	'$lgt_tr_object_relations'(Rels, Obj),
	'$lgt_tr_directives'(Dirs, _),
	'$lgt_tr_clauses'(Clauses),
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_object_clauses',
	'$lgt_gen_object_directives',
	'$lgt_assert_tr_entity'.



% create_category(+category_identifier, +list, +list, +list)

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Ctg); var(Rels); var(Dirs); var(Clauses)),
	throw(error(instantiation_error, create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ atom(Ctg),
	throw(error(type_error(category_identifier, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_current_category_'(Ctg, _, _),
	throw(error(permission_error(replace, category, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_current_object_'(Ctg, _, _, _, _, _),
	throw(error(permission_error(replace, object, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_current_protocol_'(Ctg, _, _),
	throw(error(permission_error(replace, protocol, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Rels),
	throw(error(type_error(list, Rels), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Dirs),
	throw(error(type_error(list, Dirs), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ '$lgt_proper_list'(Clauses),
	throw(error(type_error(list, Clauses), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_clean_pp_clauses',
	'$lgt_tr_category_id'(Ctg, (dynamic)),
	'$lgt_tr_category_relations'(Rels, Ctg),
	'$lgt_tr_directives'(Dirs, _),
	'$lgt_tr_clauses'(Clauses),
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_category_clauses',
	'$lgt_gen_category_directives',
	'$lgt_assert_tr_entity'.



% create_protocol(+protocol_identifier, +list, +list)

create_protocol(Ptc, Rels, Dirs) :-
	(var(Ptc); var(Rels); var(Dirs)),
	throw(error(instantiation_error, create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_current_protocol_'(Ptc, _, _),
	throw(error(permission_error(replace, protocol, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_current_object_'(Ptc, _, _, _, _, _),
	throw(error(permission_error(replace, object, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_current_category_'(Ptc, _, _),
	throw(error(permission_error(replace, category, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	\+ '$lgt_proper_list'(Rels),
	throw(error(type_error(list, Rels), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	\+ '$lgt_proper_list'(Dirs),
	throw(error(type_error(list, Dirs), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_clean_pp_clauses',
	'$lgt_tr_protocol_id'(Ptc, (dynamic)),
	'$lgt_tr_protocol_relations'(Rels, Ptc),
	'$lgt_tr_directives'(Dirs, _),
	'$lgt_gen_protocol_clauses',
	'$lgt_gen_protocol_directives',
	'$lgt_assert_tr_entity'.



% abolish_object(@object_identifier)

abolish_object(Obj) :-
	var(Obj),
	throw(error(instantiation_error, abolish_object(Obj))).

abolish_object(Obj) :-
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_object(Obj))).

abolish_object(Obj) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, Type) ->
		(Type = (dynamic) ->
			'$lgt_call'(Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm),
			'$lgt_abolish_entity_predicates'(Def),
			'$lgt_abolish_entity_predicates'(DDef),
			abolish(Dcl/4),
			abolish(Dcl/6),
			abolish(Def/5),
			abolish(Def/6),
			abolish(Super/6),
			abolish(IDcl/6),
			abolish(IDef/6),
			abolish(DDcl/2),
			abolish(DDef/5),
			abolish(Rnm/3),
			abolish(Prefix/8),
			retractall('$lgt_current_object_'(Obj, _, _, _, _, _)),
			retractall('$lgt_extends_object_'(Obj, _, _)),
			retractall('$lgt_instantiates_class_'(Obj, _, _)),
			retractall('$lgt_specializes_class_'(Obj, _, _)),
			retractall('$lgt_implements_protocol_'(Obj, _, _)),
			retractall('$lgt_imports_category_'(Obj, _, _)),
			retractall('$lgt_debugging_'(Obj)),
			'$lgt_clean_lookup_caches'
			;
			throw(error(permission_error(modify, static_object, Obj), abolish_object(Obj))))
		;
		throw(error(existence_error(object, Obj), abolish_object(Obj))).



% abolish_category(@category_identifier)

abolish_category(Ctg) :-
	var(Ctg),
	throw(error(instantiation_error, abolish_category(Ctg))).

abolish_category(Ctg) :-
	\+ atom(Ctg),
	throw(error(type_error(category_identifier, Ctg), abolish_category(Ctg))).

abolish_category(Ctg) :-
	'$lgt_current_category_'(Ctg, Prefix, Type) ->
		(Type = (dynamic) ->
			'$lgt_call'(Prefix, Dcl, Def, Rnm),
			'$lgt_abolish_entity_predicates'(Def),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Def/5),
			abolish(Rnm/3),
			abolish(Prefix/3),
			retractall('$lgt_current_category_'(Ctg, _, _)),
			retractall('$lgt_imports_category_'(Ctg, _, _)),
			retractall('$lgt_implements_protocol_'(Ctg, _, _)),
			'$lgt_clean_lookup_caches'
			;
			throw(error(permission_error(modify, static_category, Ctg), abolish_category(Ctg))))
		;
		throw(error(existence_error(category, Ctg), abolish_category(Ctg))).



% abolish_protocol(@protocol_identifier)

abolish_protocol(Ptc) :-
	var(Ptc),
	throw(error(instantiation_error, abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	'$lgt_current_protocol_'(Ptc, Prefix, Type) ->
		(Type = (dynamic) ->
			'$lgt_call'(Prefix, Dcl, Rnm),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Rnm/3),
			abolish(Prefix/2),
			retractall('$lgt_current_protocol_'(Ptc, _, _)),
			retractall('$lgt_extends_protocol_'(Ptc, _, _)),
			'$lgt_clean_lookup_caches'
			;
			throw(error(permission_error(modify, static_protocol, Ptc), abolish_protocol(Ptc))))
		;
		throw(error(existence_error(protocol, Ptc), abolish_protocol(Ptc))).



% '$lgt_abolish_entity_predicates'(+atom)

'$lgt_abolish_entity_predicates'(Def) :-
	'$lgt_call'(Def, _, _, _, _, Pred),
	functor(Pred, Functor, Arity),
	abolish(Functor/Arity),
	fail.

'$lgt_abolish_entity_predicates'(_).



% implements_protocol(?object_identifier, ?protocol_identifier)
% implements_protocol(?category_identifier, ?protocol_identifier)

implements_protocol(ObjOrCtg, Ptc) :-
	catch(
		implements_protocol(ObjOrCtg, Ptc, _),
		error(Error, _),
		throw(error(Error, implements_protocol(ObjOrCtg, Ptc)))).



% implements_protocol(?object_identifier, ?protocol_identifier, ?atom)
% implements_protocol(?category_identifier, ?protocol_identifier, ?atom)

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	nonvar(ObjOrCtg),
	\+ callable(ObjOrCtg),
	throw(error(type_error(object_identifier, ObjOrCtg), implements_protocol(ObjOrCtg, Ptc, Scope))).

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	nonvar(Ptc),
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), implements_protocol(ObjOrCtg, Ptc, Scope))).

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), implements_protocol(ObjOrCtg, Ptc, Scope))).

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	'$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope).



% imports_category(?object_identifier, ?category_identifier)
% imports_category(?category_identifier, ?category_identifier)

imports_category(ObjOrCtg, Ctg) :-
	catch(
		imports_category(ObjOrCtg, Ctg, _),
		error(Error, _),
		throw(error(Error, imports_category(ObjOrCtg, Ctg)))).



% imports_category(?object_identifier, ?category_identifier, ?atom)
% imports_category(?category_identifier, ?category_identifier, ?atom)

imports_category(ObjOrCtg, Ctg, Scope) :-
	nonvar(ObjOrCtg),
	\+ callable(ObjOrCtg),
	throw(error(type_error(object_identifier, ObjOrCtg), imports_category(ObjOrCtg, Ctg, Scope))).

imports_category(ObjOrCtg, Ctg, Scope) :-
	nonvar(Ctg),
	\+ atom(Ctg),
	throw(error(type_error(category_identifier, Ctg), imports_category(ObjOrCtg, Ctg, Scope))).

imports_category(ObjOrCtg, Ctg, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), imports_category(ObjOrCtg, Ctg, Scope))).

imports_category(ObjOrCtg, Ctg, Scope) :-
	'$lgt_imports_category_'(ObjOrCtg, Ctg, Scope).



% instantiates_class(?object_identifier, ?object_identifier)

instantiates_class(Obj, Class) :-
	catch(
		instantiates_class(Obj, Class, _),
		error(Error, _),
		throw(error(Error, instantiates_class(Obj, Class)))).



% instantiates_class(?object_identifier, ?object_identifier, ?atom)

instantiates_class(Obj, Class, Scope) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Class),
	\+ callable(Class),
	throw(error(type_error(object_identifier, Class), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	'$lgt_instantiates_class_'(Obj, Class, Scope).



% specializes_class(?object_identifier, ?object_identifier)

specializes_class(Class, Superclass) :-
	catch(
		specializes_class(Class, Superclass, _),
		error(Error, _),
		throw(error(Error, specializes_class(Class, Superclass)))).



% specializes_class(?object_identifier, ?object_identifier, ?atom)

specializes_class(Class, Superclass, Scope) :-
	nonvar(Class),
	\+ callable(Class),
	throw(error(type_error(object_identifier, Class), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Superclass),
	\+ callable(Superclass),
	throw(error(type_error(object_identifier, Superclass), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	'$lgt_specializes_class_'(Class, Superclass, Scope).



% extends_protocol(?protocol_identifier, ?protocol_identifier)

extends_protocol(Ptc1, Ptc2) :-
	catch(
		extends_protocol(Ptc1, Ptc2, _),
		error(Error, _),
		throw(error(Error, extends_protocol(Ptc1, Ptc2)))).



% extends_protocol(?protocol_identifier, ?protocol_identifier, ?atom)

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc1),
	\+ atom(Ptc1),
	throw(error(type_error(protocol_identifier, Ptc1), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc2),
	\+ atom(Ptc2),
	throw(error(type_error(protocol_identifier, Ptc2), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	'$lgt_extends_protocol_'(Ptc1, Ptc2, Scope).



% extends_object(?object_identifier, ?object_identifier)

extends_object(Prototype, Parent) :-
	catch(
		extends_object(Prototype, Parent, _),
		error(Error, _),
		throw(error(Error, extends_object(Prototype, Parent)))).



% extends_object(?object_identifier, ?object_identifier, ?atom)

extends_object(Prototype, Parent, Scope) :-
	nonvar(Prototype),
	\+ callable(Prototype),
	throw(error(type_error(object_identifier, Prototype), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	nonvar(Parent),
	\+ callable(Parent),
	throw(error(type_error(object_identifier, Parent), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	'$lgt_extends_object_'(Prototype, Parent, Scope).



% current_event(?event, ?object_identifier, ?callable, ?object_identifier, ?object_identifier)

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ callable(Msg),
	throw(error(type_error(callable, Msg), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ callable(Sender),
	throw(error(type_error(object_identifier, Sender), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ callable(Monitor),
	throw(error(type_error(object_identifier, Monitor), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(before, Obj, Msg, Sender, Monitor) :-
	'$lgt_before_'(Obj, Msg, Sender, Monitor, _).

current_event(after, Obj, Msg, Sender, Monitor) :-
	'$lgt_after_'(Obj, Msg, Sender, Monitor, _).



%define_events(@event, @object_identifier, @callable, @object_identifier, +object_identifier)

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ callable(Msg),
	throw(error(type_error(callable, Msg), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ callable(Sender),
	throw(error(type_error(object_identifier, Sender), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Monitor),
	throw(error(instantiation_error, define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ callable(Monitor),
	throw(error(type_error(object_identifier, Monitor), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Event),
	!,
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_once'(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, BCall, _),
	'$lgt_once'(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, ACall, _),
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_before_'(Obj, Msg, Sender, Monitor, BCall)),
	retractall('$lgt_after_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_after_'(Obj, Msg, Sender, Monitor, ACall)).

define_events(before, Obj, Msg, Sender, Monitor) :-
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_once'(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_before_'(Obj, Msg, Sender, Monitor, Call)).

define_events(after, Obj, Msg, Sender, Monitor) :-
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_once'(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
	retractall('$lgt_after_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_after_'(Obj, Msg, Sender, Monitor, Call)).



% abolish_events(@event, @object_identifier, @callable, @object_identifier, @object_identifier)

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ callable(Msg),
	throw(error(type_error(callable, Msg), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ callable(Sender),
	throw(error(type_error(object_identifier, Sender), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ callable(Monitor),
	throw(error(type_error(object_identifier, Monitor), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Event),
	!,
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)),
	retractall('$lgt_after_'(Obj, Msg, Sender, Monitor, _)).

abolish_events(before, Obj, Msg, Sender, Monitor) :-
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)).

abolish_events(after, Obj, Msg, Sender, Monitor) :-
	retractall('$lgt_after_'(Obj, Msg, Sender, Monitor, _)).



% compiling and loading built-in predicates


% '$lgt_compiler_flag'(+atom, ?atom)
%
% gets/checks the current value of a compiler flag

'$lgt_compiler_flag'(Option, Value) :-
	'$lgt_pp_compiler_flag_'(Option, Value2),
	!,
	Value = Value2.

'$lgt_compiler_flag'(Option, Value) :-
	'$lgt_current_flag_'(Option, Value2),
	!,
	Value = Value2.

'$lgt_compiler_flag'(Option, Value) :-
	'$lgt_default_flag'(Option, Value).



% logtalk_compile(@source_file_name)
% logtalk_compile(@source_file_name_list)
%
% compiles to disk a source file or list of source files using default options

logtalk_compile(Files) :-
	catch(
		logtalk_compile(Files, []),
		error(Error, _),
		throw(error(Error, logtalk_compile(Files)))).



% logtalk_compile(@source_file_name, @list)
% logtalk_compile(@source_file_name_list, @list)
%
% compiles to disk a source file or a list of source files using a list of flag options

logtalk_compile(File, Flags) :-
	(atom(File), File \= []; compound(File), File \= [_| _]),
	!,
	catch(
		('$lgt_init_warnings_counter'(logtalk_compile(File, Flags)),
		 '$lgt_check_source_file'(File),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_set_compiler_flags'(Flags),
		 '$lgt_compile_file'(File),
		 '$lgt_report_warning_numbers'(logtalk_compile(File, Flags))),
		Error,
		('$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_compile(File, Flags))))).

logtalk_compile(Files, Flags) :-
	catch(
		('$lgt_init_warnings_counter'(logtalk_compile(Files, Flags)),
		 '$lgt_check_source_files'(Files),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_set_compiler_flags'(Flags),
		 '$lgt_compile_files'(Files),
		 '$lgt_report_warning_numbers'(logtalk_compile(Files, Flags))),
		Error,
		('$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_compile(Files, Flags))))).



% predicates for compilation warning counting and reporting

'$lgt_reset_entity_warnings_flag' :-
	retractall('$lgt_pp_entity_warnings_flag_').


'$lgt_entity_warnings_flag' :-
	'$lgt_pp_entity_warnings_flag_'.


'$lgt_reset_warnings_counter' :-
	retractall('$lgt_pp_warnings_top_argument_'(_)),
	retractall('$lgt_pp_comp_warnings_counter_'(_)),
	retractall('$lgt_pp_load_warnings_counter_'(_)),
	retractall('$lgt_pp_entity_warnings_flag_').


'$lgt_init_warnings_counter'(Term) :-
	'$lgt_pp_warnings_top_argument_'(_) ->
		true
		;
		asserta('$lgt_pp_warnings_top_argument_'(Term)),
		retractall('$lgt_pp_comp_warnings_counter_'(_)),
		asserta('$lgt_pp_comp_warnings_counter_'(0)),
		retractall('$lgt_pp_load_warnings_counter_'(_)),
		asserta('$lgt_pp_load_warnings_counter_'(0)),
		retractall('$lgt_pp_entity_warnings_flag_').


'$lgt_inc_compile_warnings_counter' :-
	retract('$lgt_pp_comp_warnings_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_comp_warnings_counter_'(New)),
	('$lgt_pp_entity_warnings_flag_' ->
		true
		;
		assertz('$lgt_pp_entity_warnings_flag_')).


'$lgt_inc_load_warnings_counter' :-
	retract('$lgt_pp_load_warnings_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_load_warnings_counter_'(New)).


'$lgt_report_warning_numbers'(Term) :-
	retract('$lgt_pp_warnings_top_argument_'(Term)),
	retract('$lgt_pp_comp_warnings_counter_'(CCounter)),
	retract('$lgt_pp_load_warnings_counter_'(LCounter)),
	'$lgt_compiler_flag'(report, on),
	Counter is CCounter + LCounter,
	'$lgt_write_warning_numbers'(Counter, CCounter, LCounter),
	!.

'$lgt_report_warning_numbers'(_).


'$lgt_write_warning_numbers'(0, _, _) :-
	!,
	write('(0 warnings)'), nl.

'$lgt_write_warning_numbers'(_, 0, LCounter) :-
	!,
	write('('), write(LCounter), write(' loading '),
	'$lgt_write_warnings_word'(LCounter), write(')'), nl.

'$lgt_write_warning_numbers'(_, CCounter, 0) :-
	!,
	write('('), write(CCounter), write(' compilation '),
	'$lgt_write_warnings_word'(CCounter), write(')'), nl.

'$lgt_write_warning_numbers'(_, CCounter, LCounter) :-
	write('('), write(CCounter), write(' compilation '),
	'$lgt_write_warnings_word'(CCounter), write(' and '),
	write(LCounter), write(' loading '),
	'$lgt_write_warnings_word'(LCounter), write(')'), nl.


'$lgt_write_warnings_word'(Number) :-
	Number =:= 1 ->
		write(warning)
		;
		write(warnings).



% '$lgt_check_source_files'(@list)
%
% check if the source file names are valid and if they exist in the current working directory

'$lgt_check_source_files'(Files) :-
	var(Files),
	throw(instantiation_error).

'$lgt_check_source_files'(Files) :-
	\+ '$lgt_proper_list'(Files),
	throw(type_error(source_file_names, Files)).

'$lgt_check_source_files'(Files) :-
	'$lgt_check_source_files_list'(Files).


'$lgt_check_source_files_list'([]).

'$lgt_check_source_files_list'([File| Files]) :-
	'$lgt_check_source_file'(File),
	'$lgt_check_source_files_list'(Files).



'$lgt_check_source_file'(File) :-
	var(File),
	throw(instantiation_error).

'$lgt_check_source_file'(File) :-
	compound(File),
	!,
	'$lgt_check_library_source_file'(File).

'$lgt_check_source_file'(File) :-
	\+ atom(File),
	throw(type_error(source_file_name, File)).

'$lgt_check_source_file'(File) :-
	'$lgt_file_name'(logtalk, File, FileWithExtension),
	\+ '$lgt_file_exists'(FileWithExtension),
	throw(existence_error(file, File)).

'$lgt_check_source_file'(_).


'$lgt_check_library_source_file'(Term) :-
	\+ Term =.. [_, _],
	throw(type_error(source_file_name, Term)).

'$lgt_check_library_source_file'(Term) :-
	Term =.. [Library, File],
	'$lgt_check_library_source_file'(Library, File).


'$lgt_check_library_source_file'(Library, _) :-
	\+ logtalk_library_path(Library, _),
	throw(existence_error(library, Library)).

'$lgt_check_library_source_file'(Library, File) :-
	once(logtalk_library_path(Library, Path)),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),
	catch(
		'$lgt_check_source_file'(File),
		Error,
		('$lgt_change_directory'(Current), throw(Error))),
	'$lgt_change_directory'(Current).



% '$lgt_check_compiler_flags'(@list)
%
% check if the compiler flags are valid

'$lgt_check_compiler_flags'(Flags) :-
	var(Flags),
	throw(instantiation_error).

'$lgt_check_compiler_flags'(Flags) :-
	\+ '$lgt_proper_list'(Flags),
	throw(type_error(list, Flags)).

'$lgt_check_compiler_flags'(Flags) :-
	'$lgt_check_compiler_flag_list'(Flags).



'$lgt_check_compiler_flag_list'([]).

'$lgt_check_compiler_flag_list'([Flag| Flags]) :-
	'$lgt_valid_compiler_flag'(Flag) ->
		'$lgt_check_compiler_flag_list'(Flags)
		;
		throw(type_error(compiler_flag, Flag)).



% '$lgt_set_compiler_flags'(@list)
%
% sets the compiler flag options

'$lgt_set_compiler_flags'(Flags) :-
	retractall('$lgt_pp_compiler_flag_'(_, _)),
	'$lgt_assert_compiler_flags'(Flags),
	('$lgt_pp_compiler_flag_'(debug, on) ->
		retractall('$lgt_pp_compiler_flag_'(smart_compilation, _)),
		asserta('$lgt_pp_compiler_flag_'(smart_compilation, off))
		;
		true).


'$lgt_assert_compiler_flags'([]).

'$lgt_assert_compiler_flags'([Flag| Flags]) :-
	Flag =.. [Name, Value],
	asserta('$lgt_pp_compiler_flag_'(Name, Value)),
	'$lgt_assert_compiler_flags'(Flags).



% logtalk_load(@source_file_name)
% logtalk_load(@source_file_name_list)
%
% compiles to disk and then loads to memory a source file 
% or a list of source files using default compiler options

logtalk_load(Files) :-
	catch(
		logtalk_load(Files, []),
		error(Error, _),
		throw(error(Error, logtalk_load(Files)))).



% logtalk_load(@source_file_name, @list)
% logtalk_load(@source_file_name_list, @list)
%
% compiles to disk and then loads to memory a source file 
% or a list of source files using a list of compiler options

logtalk_load(File, Flags) :-
	(atom(File), File \= []; compound(File), File \= [_| _]),
	!,
	catch(
		('$lgt_init_warnings_counter'(logtalk_load(File, Flags)),
		 '$lgt_check_source_file'(File),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_set_compiler_flags'(Flags),
		 '$lgt_load_file'(File),
		 '$lgt_report_warning_numbers'(logtalk_load(File, Flags))),
		Error,
		('$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_load(File, Flags))))).

logtalk_load(Files, Flags) :-
	catch(
		('$lgt_init_warnings_counter'(logtalk_load(Files, Flags)),
		 '$lgt_check_source_files'(Files),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_set_compiler_flags'(Flags),
		 '$lgt_load_files'(Files),
		 '$lgt_report_warning_numbers'(logtalk_load(Files, Flags))),
		Error,
		('$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_load(Files, Flags))))).



% set_logtalk_flag(+atom, +nonvar)
%
% sets a Logtalk flag

set_logtalk_flag(Flag, Value) :-
	var(Flag),
	throw(error(instantiation_error, set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	var(Value),
	throw(error(instantiation_error, set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	nonvar(Flag),
	\+ atom(Flag),
	throw(error(type_error(atom, Flag), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	atom(Flag),
	\+ '$lgt_valid_flag'(Flag),
	throw(error(domain_error(valid_flag, Flag), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	'$lgt_read_only_flag'(Flag),
	throw(error(permission_error(modify, read_only_flag, Flag), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	\+ '$lgt_valid_flag'(Flag, Value),
	throw(error(domain_error(valid_flag_value, Value), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(debug, on) :-
	!,
	retractall('$lgt_current_flag_'(debug, _)),
	assertz('$lgt_current_flag_'(debug, on)),
	retractall('$lgt_current_flag_'(smart_compilation, _)),
	assertz('$lgt_current_flag_'(smart_compilation, off)).

set_logtalk_flag(Flag, Value) :-
	retractall('$lgt_current_flag_'(Flag, _)),
	assertz('$lgt_current_flag_'(Flag, Value)).



% current_logtalk_flag(?atom, ?nonvar)
%
% tests/gets Logtalk flags

current_logtalk_flag(Flag, Value) :-
	nonvar(Flag),
	\+ atom(Flag),
	throw(error(type_error(atom, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	atom(Flag),
	\+ '$lgt_valid_flag'(Flag),
	throw(error(domain_error(valid_flag, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	'$lgt_current_flag_'(Flag, Value).

current_logtalk_flag(Flag, Value) :-
	'$lgt_default_flag'(Flag, Value),
	\+ '$lgt_current_flag_'(Flag, _).

current_logtalk_flag(version, version(2, 25, 1)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in methods
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% checks if an object exists at runtime

'$lgt_obj_exists'(Obj, Pred, Sender) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _) ->
		throw(error(existence_error(object, Obj), Obj::Pred, Sender))
		;
		true.



% current_predicate/1 built-in method

'$lgt_current_predicate'(Obj, Pred, Sender, _) :-
	nonvar(Pred),
	Pred \= _/_,
	throw(error(type_error(predicate_indicator, Pred), Obj::current_predicate(Pred), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	nonvar(Functor),
	\+ atom(Functor),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	nonvar(Arity),
	\+ (integer(Arity), Arity >= 0),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, Scope) :-
	nonvar(Functor),
	nonvar(Arity),
	!,
	functor(Pred, Functor, Arity),
	once('$lgt_visible_predicate'(Obj, Pred, Sender, Scope)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, Scope) :-
	setof(
		Functor/Arity,
		(Pred, Scope)^('$lgt_visible_predicate'(Obj, Pred, Sender, Scope), functor(Pred, Functor, Arity)),
		Preds),
	'$lgt_member'(Functor/Arity, Preds).


% '$lgt_visible_predicate'(@object_identifier, ?callable, @object_identifier, @term)

'$lgt_visible_predicate'(Obj, Pred, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _),
	'$lgt_call'(Dcl, Pred, PScope, _, _, SCtn, _),
	 once((\+ \+ PScope = Scope; Sender = SCtn)).



% predicate_property/2 built-in method

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	nonvar(Prop),
	\+ '$lgt_valid_pred_property'(Prop),
	throw(error(domain_error(predicate_property, Prop), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ callable(Pred),
	throw(error(type_error(callable, Pred), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _),
	'$lgt_once'(Dcl, Pred, PScope, Type, Meta, SCtn, TCtn),
	!,
	once((\+ \+ PScope = Scope; Sender = SCtn)),
	('$lgt_scope'(Prop, PScope);
	 Prop = Type;
	 Prop = declared_in(TCtn);
	 '$lgt_once'(Def, Pred, _, _, _, _, DCtn),
	 Prop = defined_in(DCtn);
	 Meta \= no, Prop = metapredicate(Meta);
	 '$lgt_current_object_'(TCtn, _, TCtnDcl, _, _, _),
	 \+ '$lgt_call'(TCtnDcl, Pred, _, _, _),
	 '$lgt_alias_pred'(Obj, Prefix, Pred, Pred2),
	 Prop = alias(Pred2)).

'$lgt_predicate_property'(_, Pred, Prop, _, Scope) :-
	'$lgt_built_in_method'(Pred, PScope),
	!,
	\+ \+ PScope = Scope,
	(Prop = static;
	 Prop = built_in;
	 '$lgt_scope'(Prop, PScope);
	 functor(Pred, Functor, Arity),
	 functor(Meta, Functor, Arity),
	 ('$lgt_metapredicate'(Meta) -> Prop = metapredicate(Meta))).

'$lgt_predicate_property'(_, Pred, Prop, _, _) :-
	'$lgt_built_in'(Pred),
	(Prop = (public);
	 Prop = built_in;
	 ('$lgt_predicate_property'(Pred, (dynamic)) -> Prop = (dynamic); Prop = static);
	 functor(Pred, Functor, Arity),
	 functor(Meta, Functor, Arity),
	 ('$lgt_metapredicate'(Meta) -> Prop = metapredicate(Meta))).


% '$lgt_scope'(?atom, ?term).
%
% converts between user and system scope terms

'$lgt_scope'(private, p).
'$lgt_scope'(protected, p(p)).
'$lgt_scope'((public), p(p(p))).



% '$lgt_alias_pred'(+object_identifier, +atom, +callable, -callable)
%
% finds the predicate pointed by an alias

'$lgt_alias_pred'(Obj, Prefix, Alias, Pred) :-
	'$lgt_alias_pred'(Obj, Prefix, Alias, Pred, _).


'$lgt_alias_pred'(_, Prefix, Alias, Pred, _) :-
	'$lgt_construct_alias_functor'(Prefix, Functor),
	'$lgt_once'(Functor, _, Pred, Alias),
	Pred \= Alias,
	!.

'$lgt_alias_pred'(Obj, _, Alias, Pred, _) :-
	'$lgt_implements_protocol_'(Obj, Ptc, _),
	'$lgt_current_protocol_'(Ptc, Prefix, _),
	'$lgt_alias_pred'(Ptc, Prefix, Alias, Pred, _).

'$lgt_alias_pred'(Ptc1, _, Alias, Pred, _) :-
	'$lgt_extends_protocol_'(Ptc1, Ptc2, _),
	'$lgt_current_protocol_'(Ptc2, Prefix, _),
	'$lgt_alias_pred'(Ptc2, Prefix, Alias, Pred, _).

'$lgt_alias_pred'(Obj, _, Alias, Pred, _) :-
	'$lgt_imports_category_'(Obj, Ctg, _),
	'$lgt_current_category_'(Ctg, Prefix, _),
	'$lgt_alias_pred'(Ctg, Prefix, Alias, Pred, _).

'$lgt_alias_pred'(Obj, _, Alias, Pred, prototype) :-
	'$lgt_extends_object_'(Obj, Parent, _),
	'$lgt_current_object_'(Parent, Prefix, _, _, _, _),
	'$lgt_alias_pred'(Parent, Prefix, Alias, Pred, prototype).

'$lgt_alias_pred'(Instance, _, Alias, Pred, instance) :-
	'$lgt_instantiates_class_'(Instance, Class, _),
	'$lgt_current_object_'(Class, Prefix, _, _, _, _),
	'$lgt_alias_pred'(Class, Prefix, Alias, Pred, superclass).

'$lgt_alias_pred'(Class, _, Alias, Pred, superclass) :-
	'$lgt_specializes_class_'(Class, Superclass, _),
	'$lgt_current_object_'(Superclass, Prefix, _, _, _, _),
	'$lgt_alias_pred'(Superclass, Prefix, Alias, Pred, superclass).



% abolish/1 built-in method

'$lgt_abolish'(Obj, Pred, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::abolish(Pred), Sender)).

'$lgt_abolish'(Obj, Pred, Sender, _) :-
	Pred \= _/_,
	throw(error(type_error(predicate_indicator, Pred), Obj::abolish(predicate), Sender)).

'$lgt_abolish'(Obj, Functor/Arity, Sender, _) :-
	(var(Functor); var(Arity)),
	throw(error(instantiation_error, Obj::abolish(Functor/Arity), Sender)).

'$lgt_abolish'(Obj, Functor/Arity, Sender, _) :-
	\+ atom(Functor),
	throw(error(type_error(atom, Functor), Obj::abolish(Functor/Arity), Sender)).

'$lgt_abolish'(Obj, Functor/Arity, Sender, _) :-
	\+ integer(Arity),
	throw(error(type_error(integer, Arity), Obj::abolish(Functor/Arity), Sender)).

'$lgt_abolish'(Obj, Functor/Arity, Sender, _) :-
	integer(Arity),
	Arity =< 0,
	throw(error(domain_error(not_less_than_zero, Arity), Obj::abolish(Functor/Arity), Sender)).

'$lgt_abolish'(Obj, Functor/Arity, Sender, Scope) :-
	'$lgt_abolish_chk'(Obj, Functor/Arity, Sender, Scope).


'$lgt_abolish_chk'(Obj, Functor/Arity, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, _, _, _) ->
		((functor(Pred, Functor, Arity),
		  '$lgt_call'(Dcl, Pred, PScope, Compilation, _, SCtn, _)) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				(Compilation = (dynamic) ->
					'$lgt_call'(Prefix, _, _, _, _, _, DDcl, DDef, _),
					('$lgt_call'(DDcl, Pred, _) ->
						Clause =.. [DDcl, Pred, _],
						retractall(Clause),
						('$lgt_call'(DDef, Pred, _, _, _, Call) ->
							functor(Call, CFunctor, CArity),
							abolish(CFunctor/CArity),
							Clause2 =.. [DDef, Pred, _, _, _, Call],
							retractall(Clause2),
							'$lgt_clean_lookup_caches'(Pred)
							;
							true)
						;
						('$lgt_call'(Dcl, Pred, _, _, _) ->
							throw(error(permission_error(modify, predicate_declaration, Pred), Obj::abolish(Functor/Arity), Sender))
							;
							throw(error(existence_error(predicate_declaration, Pred), Obj::abolish(Functor/Arity), Sender))))
					;
					throw(error(permission_error(modify, static_predicate, Pred), Obj::abolish(Functor/Arity), Sender)))
				;
				(PScope = p ->
					throw(error(permission_error(modify, private_predicate, Pred), Obj::abolish(Functor/Arity), Sender))
					;
					throw(error(permission_error(modify, protected_predicate, Pred), Obj::abolish(Functor/Arity), Sender))))
			;
			throw(error(existence_error(predicate_declaration, Pred), Obj::abolish(Functor/Arity), Sender)))
		;
		throw(error(existence_error(object, Obj), Obj::abolish(Functor/Arity), Sender)).



% asserta/1 built-in method

'$lgt_asserta'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::asserta(Clause), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, Scope) :-
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, Scope, Call, _),
	!,
	asserta(Call).

'$lgt_asserta'(Obj, Clause, Sender, _) :-
	Clause \= (_ :- _),
	\+ callable(Clause),
	throw(error(type_error(callable, Clause), Obj::asserta(Clause), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, Scope) :-
	Clause = (_ :- _) ->
		'$lgt_asserta_rule_chk'(Obj, Clause, Sender, Scope)
		;
		'$lgt_asserta_fact_chk'(Obj, Clause, Sender, Scope).


'$lgt_asserta_rule_chk'(Obj, (Head:-Body), Sender, Scope) :-
	!,
	('$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
		'$lgt_call'(Prefix, Dcl, Def, _, _, _, DDcl, DDef, _),
		'$lgt_assert_pred_dcl'(Dcl, DDcl, Head, Scope, PScope, Type, Meta, SCtn),
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn)  ->
				'$lgt_assert_pred_call'(Def, DDef, Prefix, Head, Sender2, This, Self, Call, _),
				'$lgt_pred_metavars'(Head, Meta, Metavars),
				'$lgt_ctx_ctx'(Ctx, Sender2, This, Self, Prefix, Metavars),
				'$lgt_tr_body'(Body, TBody, DBody, Ctx),
				('$lgt_debugging_'(Obj) ->
					asserta((Call :- ('$lgt_nop'(Body), '$lgt_dbg_head'(Head, Ctx), DBody)))
					;
					asserta((Call :- ('$lgt_nop'(Body), TBody))))
				;
				(PScope = p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::asserta((Head:-Body)), Sender))
					;
					throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta((Head:-Body)), Sender))))
			;
			throw(error(permission_error(modify, static_predicate, Head), Obj::asserta((Head:-Body)), Sender)))
		;
		throw(error(existence_error(object, Obj), Obj::asserta((Head:-Body)), Sender))).

'$lgt_asserta_fact_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
		'$lgt_call'(Prefix, Dcl, Def, _, _, _, DDcl, DDef, _),
		'$lgt_assert_pred_dcl'(Dcl, DDcl, Head, Scope, PScope, Type, _, SCtn),
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn)  ->
				('$lgt_debugging_'(Obj) ->
					'$lgt_assert_pred_call'(Def, DDef, Prefix, Head, Sender2, This, Self, Call, _),
					'$lgt_ctx_ctx'(Ctx, Sender2, This, Self, Prefix, []),
					asserta((Call :- '$lgt_dbg_fact'(Head, Ctx)))
					;
					functor(Head, HFunctor, HArity), functor(GHead, HFunctor, HArity),
					functor(Obj, OFunctor, OArity), functor(GObj, OFunctor, OArity),
					functor(Sender, SFunctor, SArity), functor(GSender, SFunctor, SArity),
					'$lgt_assert_pred_call'(Def, DDef, Prefix, GHead, Sender2, This, Self, GCall, Update),
					asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, Update)),
					GObj = Obj, GHead = Head, GSender = Sender,
					asserta(GCall))
				;
				(PScope = p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::asserta(Head), Sender))
					;
					throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta(Head), Sender))))
			;
			throw(error(permission_error(modify, static_predicate, Head), Obj::asserta(Head), Sender)))
		;
		throw(error(existence_error(object, Obj), Obj::asserta(Head), Sender)).



% assertz/1 built-in method

'$lgt_assertz'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::assertz(Clause), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, _) :-
	Clause \= (_ :- _),
	\+ callable(Clause),
	throw(error(type_error(callable, Clause), Obj::asserta(Clause), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, Scope) :-
	Clause = (_ :- _) ->
		'$lgt_assertz_rule_chk'(Obj, Clause, Sender, Scope)
		;
		'$lgt_assertz_fact_chk'(Obj, Clause, Sender, Scope).


'$lgt_assertz_rule_chk'(Obj, (Head:-Body), Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
		'$lgt_call'(Prefix, Dcl, Def, _, _, _, DDcl, DDef, _),
		'$lgt_assert_pred_dcl'(Dcl, DDcl, Head, Scope, PScope, Type, Meta, SCtn),
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn)  ->
				'$lgt_assert_pred_call'(Def, DDef, Prefix, Head, Sender2, This, Self, Call, _),
				'$lgt_pred_metavars'(Head, Meta, Metavars),
				'$lgt_ctx_ctx'(Ctx, Sender2, This, Self, Prefix, Metavars),
				'$lgt_tr_body'(Body, TBody, DBody, Ctx),
				('$lgt_debugging_'(Obj) ->
					assertz((Call :- ('$lgt_nop'(Body), '$lgt_dbg_head'(Head, Ctx), DBody)))
					;
					assertz((Call :- ('$lgt_nop'(Body), TBody))))
				;
				(PScope = p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::assertz((Head:-Body)), Sender))
					;
					throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz((Head:-Body)), Sender))))
			;
			throw(error(permission_error(modify, static_predicate, Head), Obj::assertz((Head:-Body)), Sender)))
		;
		throw(error(existence_error(object, Obj), Obj::assertz((Head:-Body)), Sender)).


'$lgt_assertz_fact_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, Scope, Call, _) ->
		assertz(Call)
		;
		('$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
			'$lgt_call'(Prefix, Dcl, Def, _, _, _, DDcl, DDef, _),
			'$lgt_assert_pred_dcl'(Dcl, DDcl, Head, Scope, PScope, Type, _, SCtn),
			(Type = (dynamic) ->
				((\+ \+ PScope = Scope; Sender = SCtn)  ->
					('$lgt_debugging_'(Obj) ->
						'$lgt_assert_pred_call'(Def, DDef, Prefix, Head, Sender2, This, Self, Call, _),
						'$lgt_ctx_ctx'(Ctx, Sender2, This, Self, Prefix, []),
						assertz((Call :- '$lgt_dbg_fact'(Head, Ctx)))
						;
						functor(Head, HFunctor, HArity), functor(GHead, HFunctor, HArity),
						functor(Obj, OFunctor, OArity), functor(GObj, OFunctor, OArity),
						functor(Sender, SFunctor, SArity), functor(GSender, SFunctor, SArity),
						'$lgt_assert_pred_call'(Def, DDef, Prefix, GHead, Sender2, This, Self, GCall, Update),
						asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, Update)),
						GObj = Obj, GHead = Head, GSender = Sender,
						assertz(GCall))
					;
					(PScope = p ->
						throw(error(permission_error(modify, private_predicate, Head), Obj::asserta(Head), Sender))
						;
						throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta(Head), Sender))))
				;
				throw(error(permission_error(modify, static_predicate, Head), Obj::asserta(Head), Sender)))
			;
			throw(error(existence_error(object, Obj), Obj::assertz(Head), Sender))).



% get or set declaration for asserted predicate

'$lgt_assert_pred_dcl'(Dcl, _, Pred, _, PScope, Type, Meta, SCtn) :-
	'$lgt_call'(Dcl, Pred, PScope, Type, Meta, SCtn, _),
	!.

'$lgt_assert_pred_dcl'(_, DDcl, Pred, Scope, Scope, (dynamic), no, _) :-
 	'$lgt_convert_test_scope'(Scope, Scope2),
 	'$lgt_assert_ddcl_clause'(DDcl, Pred, Scope2).



% get or set compiled call for asserted predicate

'$lgt_assert_pred_call'(Def, _, _, Pred, Sender, This, Self, Call, true) :-
	'$lgt_call'(Def, Pred, Sender, This, Self, Call),
	!.
	
'$lgt_assert_pred_call'(_, DDef, _, Pred, Sender, This, Self, Call, '$lgt_update_ddef_table'(DDef, Pred, Call)) :-
	'$lgt_call'(DDef, Pred, Sender, This, Self, Call),
	!.

'$lgt_assert_pred_call'(_, DDef, Prefix, Pred, Sender, This, Self, Call, '$lgt_update_ddef_table'(DDef, Pred, Call)) :-
	functor(Pred, Functor, Arity),
	'$lgt_assert_ddef_clause'(Functor, Arity, Prefix, DDef, _),
	'$lgt_once'(DDef, Pred, Sender, This, Self, Call).



% clause/2 built-in method

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_clause_chk'(Obj, Head, Body, Sender, Scope).


'$lgt_clause_chk'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, Scope, Call, _),	
	!,
	clause(Call, TBody),
	(TBody = ('$lgt_nop'(Body), _) ->
		true
		;
		Body = TBody).

'$lgt_clause_chk'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
		'$lgt_call'(Prefix, Dcl, Def, _, _, _, _, DDef, _),
		('$lgt_call'(Dcl, Head, PScope, Type, _, SCtn, _) ->
			(Type = (dynamic) ->
				((\+ \+ PScope = Scope; Sender = SCtn) ->
					once(('$lgt_call'(Def, Head, _, _, _, Call); '$lgt_call'(DDef, Head, _, _, _, Call))),
					clause(Call, TBody),
					(TBody = ('$lgt_nop'(Body), _) ->
						true
						;
						Body = TBody)
					;
					(PScope = p ->
						throw(error(permission_error(access, private_predicate, Head), Obj::clause(Head, Body), Sender))
						;
						throw(error(permission_error(access, protected_predicate, Head), Obj::clause(Head, Body), Sender))))
				;
				throw(error(permission_error(access, static_predicate, Head), Obj::clause(Head, Body), Sender)))
			;
			((Obj = Sender, '$lgt_call'(DDef, Head, _, _, _, Call)) ->	% local dynamic predicate with no scope declaration
				clause(Call, TBody),
				(TBody = ('$lgt_nop'(Body), _) ->
					true
					;
					Body = TBody)
				;
				throw(error(existence_error(predicate_declaration, Head), Obj::clause(Head, Body), Sender))))
		;
		throw(error(existence_error(object, Obj), Obj::clause(Head, Body), Sender)).



% retract/1 built-in method

'$lgt_retract'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::retract(Clause), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, Clause, Sender, Scope) :-
	Clause = (_ :- _) ->
		'$lgt_retract_rule_chk'(Obj, Clause, Sender, Scope)
		;
		'$lgt_retract_fact_chk'(Obj, Clause, Sender, Scope).


'$lgt_retract_rule_chk'(Obj, (Head:-Body), Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
		'$lgt_call'(Prefix, Dcl, Def, _, _, _, _, DDef, _),
		('$lgt_call'(Dcl, Head, PScope, Type, _, SCtn, _) ->
			(Type = (dynamic) ->
				((\+ \+ PScope = Scope; Sender = SCtn) ->
					('$lgt_call'(Def, Head, _, _, _, Call) ->
						retract((Call :- ('$lgt_nop'(Body), _)))
						;
						('$lgt_call'(DDef, Head, _, _, _, Call) ->
							retract((Call :- ('$lgt_nop'(Body), _))),
							'$lgt_update_ddef_table'(DDef, Head, Call)
							;
							fail))
					;
					(PScope = p ->
						throw(error(permission_error(modify, private_predicate, Head), Obj::retract((Head:-Body)), Sender))
						;
						throw(error(permission_error(modify, protected_predicate, Head), Obj::retract((Head:-Body)), Sender))))
				;
				throw(error(permission_error(modify, static_predicate, Head), Obj::retract((Head:-Body)), Sender)))
			;
			((Obj = Sender, '$lgt_call'(DDef, Head, _, _, _, Call)) ->	% local dynamic predicate with no scope declaration
				retract((Call :- ('$lgt_nop'(Body), _)))
				;
				throw(error(existence_error(predicate_declaration, Head), Obj::retract((Head:-Body)), Sender))))
		;
		throw(error(existence_error(object, Obj), Obj::retract((Head:-Body)), Sender)).

'$lgt_retract_fact_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, Scope, Call, Update) ->
		retract(Call),
		call(Update)
		;
		('$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
			'$lgt_call'(Prefix, Dcl, Def, _, _, _, _, DDef, _),
			functor(Head, HFunctor, HArity), functor(GHead, HFunctor, HArity),
			functor(Obj, OFunctor, OArity), functor(GObj, OFunctor, OArity),
			functor(Sender, SFunctor, SArity), functor(GSender, SFunctor, SArity),
			('$lgt_call'(Dcl, Head, PScope, Type, _, SCtn, _) ->
				(Type = (dynamic) ->
					((\+ \+ PScope = Scope; Sender = SCtn) ->
						('$lgt_call'(Def, GHead, _, _, _, GCall) ->
							('$lgt_debugging_'(Obj) ->
								GObj = Obj, GHead = Head, GSender = Sender,
								retract((GCall :- '$lgt_dbg_fact'(_, _)))
								;
								asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, true)),
								GObj = Obj, GHead = Head, GSender = Sender,
								retract(GCall))
							;
							('$lgt_call'(DDef, GHead, _, _, _, GCall) ->
								('$lgt_debugging_'(Obj) ->
									GObj = Obj, GHead = Head, GSender = Sender,
									retract((GCall :- '$lgt_dbg_fact'(_, _)))
									;
									asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, '$lgt_update_ddef_table'(DDef, GHead, GCall))),
									GObj = Obj, GHead = Head, GSender = Sender,
									retract(GCall)),
								'$lgt_update_ddef_table'(DDef, GHead, GCall)
								;
								fail))
						;
						(PScope = p ->
							throw(error(permission_error(modify, private_predicate, Head), Obj::retract(Head), Sender))
							;
							throw(error(permission_error(modify, protected_predicate, Head), Obj::retract(Head), Sender))))
					;
					throw(error(permission_error(modify, static_predicate, Head), Obj::retract(Head), Sender)))
				;
				((GObj = GSender, '$lgt_call'(DDef, GHead, _, _, _, GCall)) ->	% local dynamic predicate with no scope declaration
					('$lgt_debugging_'(Obj) ->
						GObj = Obj, GHead = Head, GSender = Sender,
						retract((GCall :- '$lgt_dbg_fact'(_, _)))
						;
						asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, true)),
						GObj = Obj, GHead = Head, GSender = Sender,
						retract(GCall))
					;
					throw(error(existence_error(predicate_declaration, Head), Obj::retract(Head), Sender))))
			;
			throw(error(existence_error(object, Obj), Obj::retract(Head), Sender))).



% retractall/1 built-in method

'$lgt_retractall'(Obj, Head, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retractall(Head), Sender)).

'$lgt_retractall'(Obj, Head, Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::retractall(Head), Sender)).

'$lgt_retractall'(Obj, Head, Sender, Scope) :-
	'$lgt_retractall_chk'(Obj, Head, Sender, Scope).


'$lgt_retractall_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, Scope, Call, Update) ->
		retractall(Call),
		call(Update)
		;
		('$lgt_current_object_'(Obj, Prefix, _, _, _, _) ->
			'$lgt_call'(Prefix, Dcl, Def, _, _, _, _, DDef, _),
			functor(Head, HFunctor, HArity), functor(GHead, HFunctor, HArity),
			functor(Obj, OFunctor, OArity), functor(GObj, OFunctor, OArity),
			functor(Sender, SFunctor, SArity), functor(GSender, SFunctor, SArity),
			('$lgt_call'(Dcl, GHead, PScope, Type, _, SCtn, _) ->
				(Type = (dynamic) ->
					((\+ \+ PScope = Scope; Sender = SCtn) ->
						('$lgt_call'(Def, GHead, _, _, _, GCall) ->
							asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, true)),
							GObj = Obj, GHead = Head, GSender = Sender,
							retractall(GCall)
							;
							('$lgt_call'(DDef, GHead, _, _, _, GCall) ->
								GObj = Obj, GHead = Head, GSender = Sender,
								retractall(GCall),
								'$lgt_update_ddef_table'(DDef, GHead, GCall)
								;
								true))
						;
						(PScope = p ->
							throw(error(permission_error(modify, private_predicate, Head), Obj::retractall(Head), Sender))
							;
							throw(error(permission_error(modify, protected_predicate, Head), Obj::retractall(Head), Sender))))
					;
					throw(error(permission_error(modify, static_predicate, Head), Obj::retractall(Head), Sender)))
				;
				((GObj = GSender, '$lgt_call'(DDef, GHead, _, _, _, GCall)) ->	% local dynamic predicate with no scope declaration
					asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, Scope, GCall, true)),
					GObj = Obj, GHead = Head, GSender = Sender,
					retractall(GCall)
					;
					throw(error(existence_error(predicate_declaration, Head), Obj::retractall(Head), Sender))))
			;
			throw(error(existence_error(object, Obj), Obj::retractall(Head), Sender))).



% '$lgt_nop'(+goal)
%
% used in the implementation of the built-in method
% clause/2 to store the original clause body

'$lgt_nop'(_).



% '$lgt_phrase'(+object_identifier, +ruleset, ?list, +object_identifier, +scope)
%
% phrase/2 built-in method

'$lgt_phrase'(Obj, Ruleset, Input, Sender, Scope) :-
	catch(
		'$lgt_phrase'(Obj, Ruleset, Input, [], Sender, Scope),
		error(Error, _),
		throw(error(Error, Obj::phrase(Ruleset, Input), Sender))).



% '$lgt_phrase'(+object_identifier, +ruleset, +list, ?list, +object_identifier, +scope)
%
% phrase/3 built-in method

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	var(Ruleset),
	throw(error(instantiation_error, Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	\+ callable(Ruleset),
	throw(error(type_error(callable, Ruleset), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	nonvar(Input),
	\+ '$lgt_proper_list'(Input),
	throw(error(type_error(list, Input), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	nonvar(Rest),
	\+ '$lgt_proper_list'(Rest),
	throw(error(type_error(list, Rest), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, (Ruleset1, Ruleset2), Input, Rest, Sender, Scope) :-
	!,
	'$lgt_phrase'(Obj, Ruleset1, Input, Rest1, Sender, Scope),
	'$lgt_phrase'(Obj, Ruleset2, Rest1, Rest, Sender, Scope).

'$lgt_phrase'(Obj, (Ruleset1; Ruleset2), Input, Rest, Sender, Scope) :-
	!,
	('$lgt_phrase'(Obj, Ruleset1, Input, Rest, Sender, Scope)
	 ;
	 '$lgt_phrase'(Obj, Ruleset2, Input, Rest, Sender, Scope)).

'$lgt_phrase'(Obj, (Ruleset1 -> Ruleset2), Input, Rest, Sender, Scope) :-
	!,
	'$lgt_phrase'(Obj, Ruleset1, Input, Rest1, Sender, Scope),
	'$lgt_phrase'(Obj, Ruleset2, Rest1, Rest, Sender, Scope).

'$lgt_phrase'(_, !, Input, Rest, _, _) :-
	!,
	Input = Rest.

'$lgt_phrase'(Obj, \+ Ruleset, Input, Rest, Sender, Scope) :-
	!,
	\+ '$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, Scope).

'$lgt_phrase'(_, [], Input, Rest, _, _) :-
	!,
	Input = Rest.

'$lgt_phrase'(_, [Head| Tail], Input, Rest, _, _) :-
	!,
	'$lgt_append'([Head| Tail], Rest, Input).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _) ->
		Ruleset =.. [Functor| Args],
		'$lgt_append'(Args, [Input, Rest], Args2),
		Pred =.. [Functor| Args2],
		('$lgt_call'(Dcl, Pred, PScope, _, _, SCtn, _) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				'$lgt_once'(Def, Pred, Sender, Obj, Obj, Call, _),
				call(Call)
				;
				(PScope = p ->
					throw(error(permission_error(access, private_non_terminal, Ruleset), Obj::phrase(Ruleset, Input, Rest), Sender))
					;
					throw(error(permission_error(access, protected_non_terminal, Ruleset), Obj::phrase(Ruleset, Input, Rest), Sender))))
			;
			((Obj = Sender,
			  ('$lgt_call'(Def, Pred, Obj, Obj, Obj, Call)
			   ;
			   '$lgt_call'(Prefix, _, _, _, _, _, _, DDef, _), '$lgt_call'(DDef, Pred, Obj, Obj, Obj, Call))) ->
				call(Call)
				;
				throw(error(existence_error(non_terminal_declaration, Ruleset), Obj::phrase(Ruleset, Input, Rest), Sender))))
		;
		throw(error(existence_error(object, Obj), Obj::phrase(Ruleset, Input, Rest), Sender)).



% '$lgt_expand_term'(+object_identifier, @term, -clause, +object_identifier, +scope)
%
% expand_term/2 built-in method

'$lgt_expand_term'(_, Term, Clause, _, _) :-
	nonvar(Term),
	Term = (_ --> _),
	!,
	'$lgt_dcgrule_to_clause'(Term, Clause).

'$lgt_expand_term'(_, Term, Term, _, _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message sending
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_send_to_self'(+object, ?term, +object)

'$lgt_send_to_self'(Obj, Pred, Sender) :-
	nonvar(Pred) ->
		'$lgt_send_to_self_nv'(Obj, Pred, Sender)
		;
		throw(error(instantiation_error, Obj::Pred, Sender)).



% '$lgt_send_to_self_nv'(+object, +term, +object)

'$lgt_send_to_self_nv'(Obj, Pred, Sender) :-
	'$lgt_self_lookup_cache_'(Obj, Pred, Sender, Call) ->
		call(Call)
		;
		('$lgt_current_object_'(Obj, _, Dcl, Def, _, _),
		 ('$lgt_call'(Dcl, Pred, Scope, _, _, SCtn, _) ->
			((Scope = p(_); Sender = SCtn) ->
				functor(Pred, PFunctor, PArity), functor(GPred, PFunctor, PArity),
				functor(Obj, OFunctor, OArity), functor(GObj, OFunctor, OArity),
				functor(Sender, SFunctor, SArity), functor(GSender, SFunctor, SArity),
				'$lgt_once'(Def, GPred, GSender, GObj, GObj, GCall, _),
				asserta('$lgt_self_lookup_cache_'(GObj, GPred, GSender, GCall)),
				GObj = Obj, GPred = Pred, GSender = Sender,
				call(GCall)
				;
				throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender)))
			;
			('$lgt_built_in'(Pred) ->
				call(Pred)
				;
				throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))))).


% '$lgt_send_to_object'(@object, ?term, +object)

'$lgt_send_to_object'(Obj, Pred, Sender) :-
	(nonvar(Obj), nonvar(Pred)) ->
		'$lgt_send_to_object_nv'(Obj, Pred, Sender)
		;
		throw(error(instantiation_error, Obj::Pred, Sender)).



% '$lgt_send_to_object_nv'(+object, +term, +object)

'$lgt_send_to_object_nv'(Obj, Pred, Sender) :-
	'$lgt_obj_lookup_cache_'(Obj, Pred, Sender, Call) ->
		\+ ('$lgt_before_'(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
		call(Call),
		\+ ('$lgt_after_'(Obj, Pred, Sender, _, ACall), \+ call(ACall))
		;
		('$lgt_current_object_'(Obj, _, Dcl, Def, _, _) ->
			('$lgt_call'(Dcl, Pred, Scope, _, _, _, _) ->
				(Scope = p(p(_)) ->
					functor(Pred, Functor, Arity), functor(GPred, Functor, Arity),
					functor(Obj, OName, OArity), functor(GObj, OName, OArity),
					'$lgt_once'(Def, GPred, GSender, GObj, GObj, GCall, _),
					asserta('$lgt_obj_lookup_cache_'(GObj, GPred, GSender, GCall)),
					GObj = Obj, GPred = Pred, GSender = Sender,
					\+ ('$lgt_before_'(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
					call(GCall),
					\+ ('$lgt_after_'(Obj, Pred, Sender, _, ACall), \+ call(ACall))
					;
					(Scope = p ->
						throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
						;
						throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))))
				;
				('$lgt_built_in'(Pred) ->
					call(Pred)
					;
					throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))))
			;
			throw(error(existence_error(object, Obj), Obj::Pred, Sender))).



% '$lgt_send_to_object_ne'(@object, ?term, +object)

'$lgt_send_to_object_ne'(Obj, Pred, Sender) :-
	(nonvar(Obj), nonvar(Pred)) ->
		'$lgt_send_to_object_ne_nv'(Obj, Pred, Sender)
		;
		throw(error(instantiation_error, Obj::Pred, Sender)).



% '$lgt_send_to_object_ne_nv'(+object, +term, +object)

'$lgt_send_to_object_ne_nv'(Obj, Pred, Sender) :-
	'$lgt_obj_lookup_cache_'(Obj, Pred, Sender, Call) ->
		call(Call)
		;
		('$lgt_current_object_'(Obj, _, Dcl, Def, _, _) ->
			('$lgt_call'(Dcl, Pred, Scope, _, _, _, _) ->
				(Scope = p(p(_)) ->
					functor(Pred, Functor, Arity), functor(GPred, Functor, Arity),
					functor(Obj, OName, OArity), functor(GObj, OName, OArity),
					'$lgt_once'(Def, GPred, GSender, GObj, GObj, GCall, _),
					asserta('$lgt_obj_lookup_cache_'(GObj, GPred, GSender, GCall)),
					GObj = Obj, GPred = Pred, GSender = Sender,
					call(GCall)
					;
					(Scope = p ->
						throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
						;
						throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))))
				;
				('$lgt_built_in'(Pred) ->
					call(Pred)
					;
					throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))))
			;
			throw(error(existence_error(object, Obj), Obj::Pred, Sender))).



% '$lgt_send_to_super'(+object, ?term, +object, +object)

'$lgt_send_to_super'(Self, Pred, This, Sender) :-
	nonvar(Pred) ->
		'$lgt_send_to_super_nv'(Self, Pred, This, Sender)
		;
		throw(error(instantiation_error, ^^Pred, This)).



% '$lgt_send_to_super_nv'(+object, +term, +object, +object)

'$lgt_send_to_super_nv'(Self, Pred, This, Sender) :-
	'$lgt_super_lookup_cache_'(Self, Pred, This, Sender, Call) ->
		call(Call)
		;
		('$lgt_current_object_'(Self, _, Dcl, _, _, _),
		 ('$lgt_call'(Dcl, Pred, Scope, _, _, SCtn, _) ->
	 		((Scope = p(_); This = SCtn) ->
				'$lgt_current_object_'(This, _, _, _, Super, _),
				functor(Pred, Functor, Arity), functor(GPred, Functor, Arity),
				functor(This, TName, TArity), functor(GThis, TName, TArity),
				functor(Self, SName, SArity), functor(GSelf, SName, SArity),
				'$lgt_once'(Super, GPred, GSender, GThis, GSelf, GCall, Ctn),
				(Ctn \= GThis ->
					asserta('$lgt_super_lookup_cache_'(GSelf, GPred, GThis, GSender, GCall)),
					GSelf = Self, GPred = Pred, GThis = This, GSender = Sender,
					call(GCall)
					;
					throw(error(endless_loop(Pred), ^^Pred, This)))
				;
				throw(error(permission_error(access, private_predicate, Pred), ^^Pred, This)))
			;
			('$lgt_built_in'(Pred) ->
				call(Pred)
				;
				throw(error(existence_error(predicate_declaration, Pred), ^^Pred, This))))).



% '$lgt_metacall_in_object'(+object, ?term, +object)
%
% metacalls in predicate definitions

'$lgt_metacall_in_object'(Obj, Pred, Sender) :-
	var(Pred) ->
		throw(error(instantiation_error, Obj::call(Pred), Sender))
		;
		(Obj = user ->
			call(Pred)
			;
			'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
			'$lgt_ctx_ctx'(Ctx, Sender, Obj, Obj, Prefix, _),
			'$lgt_tr_body'(Pred, Call, DCall, Ctx),
			(('$lgt_dbg_debugging_', '$lgt_debugging_'(Obj)) ->
				call(DCall)
				;
				call(Call))).



% '$lgt_call_built_in'(+term, +term)
%
% needed for runtime translation of dynamic clauses

'$lgt_call_built_in'(Pred, Ctx) :-
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	'$lgt_current_object_'(This, _, _, Def, _, _),
	('$lgt_call'(Def, Pred, Sender, This, Self, Call) ->
		call(Call)
		;
		call(Pred)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in pseudo-object object table clauses
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_current_object_'(user, '$lgt_po_user0_', '$lgt_po_user0__dcl', '$lgt_po_user0__def', _, static).
'$lgt_current_object_'(debugger, '$lgt_po_debugger0_', '$lgt_po_debugger0__dcl', '$lgt_po_debugger0__def', _, static).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in pseudo-object user
%
%  represents the Prolog database (excluding built-in predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% the following clauses correspond to a virtual 
% compilation of the pseudo-object user


'$lgt_po_user0_'('$lgt_po_user0__dcl', '$lgt_po_user0__def', _, _, _, _, _).

'$lgt_po_user0__dcl'(Pred, p(p(p)), Type, no) :-
	(nonvar(Pred) ->
		\+ '$lgt_built_in'(Pred),
		functor(Pred, Functor, Arity),
		current_predicate(Functor/Arity)
		;
		current_predicate(Functor/Arity),
		\+ '$lgt_hidden_functor'(Functor),
		functor(Pred, Functor, Arity),
		\+ '$lgt_built_in'(Pred)),
	('$lgt_predicate_property'(Pred, (dynamic)) ->
		Type = (dynamic)
		;
		Type = static).

'$lgt_po_user0__dcl'(Pred, p(p(p)), Type, Meta, user, user) :-
	'$lgt_po_user0__dcl'(Pred, p(p(p)), Type, Meta).

'$lgt_po_user0__def'(Pred, _, _, _, Pred).

'$lgt_po_user0__def'(Pred, _, _, _, Pred, user).



% '$lgt_hidden_functor'(+atom)
%
% hidden functors include Logtalk pre-processor and runtime internal functors
% and those used in the compiled code of objects, protocols, and categories

'$lgt_hidden_functor'(Functor) :-
	atom_concat('$lgt_', _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_category_'(_, Prefix, _),
	atom_concat(Prefix, _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_object_'(_, Prefix, _, _, _, _),
	atom_concat(Prefix, _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_protocol_'(_, Prefix, _),
	atom_concat(Prefix, _, Functor),
	!.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in pseudo-object debugger
%
%  implements the Logtalk buit-in debugging features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% the following clauses correspond to a virtual 
% compilation of the pseudo-object debugger

'$lgt_po_debugger0_'('$lgt_po_debugger0__dcl', '$lgt_po_debugger0__def', _, _, _, _, _).


% debugger public protocol

'$lgt_po_debugger0__dcl'(reset, p(p(p)), static, no).

'$lgt_po_debugger0__dcl'(debug, p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(nodebug, p(p(p)), static, no).

'$lgt_po_debugger0__dcl'(debugging, p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(debugging(_), p(p(p)), static, no).

'$lgt_po_debugger0__dcl'(trace, p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(notrace, p(p(p)), static, no).

'$lgt_po_debugger0__dcl'(spy(_), p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(spy(_, _, _, _), p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(nospy(_), p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(nospy(_, _, _, _), p(p(p)), static, no).
'$lgt_po_debugger0__dcl'(nospyall, p(p(p)), static, no).

'$lgt_po_debugger0__dcl'(leash(_), p(p(p)), static, no).


'$lgt_po_debugger0__dcl'(Pred, p(p(p)), Type, Meta, debugger, debugger) :-
	'$lgt_po_debugger0__dcl'(Pred, p(p(p)), Type, Meta).


'$lgt_po_debugger0__def'(reset, _, _, _, '$lgt_dbg_reset').
'$lgt_po_debugger0__def'(debug, _, _, _, '$lgt_dbg_debug').
'$lgt_po_debugger0__def'(debugging, _, _, _, '$lgt_dbg_debugging').
'$lgt_po_debugger0__def'(debugging(Entity), _, _, _, '$lgt_dbg_debugging'(Entity)).
'$lgt_po_debugger0__def'(nodebug, _, _, _, '$lgt_dbg_nodebug').
'$lgt_po_debugger0__def'(trace, _, _, _, '$lgt_dbg_trace').
'$lgt_po_debugger0__def'(notrace, _, _, _, '$lgt_dbg_notrace').
'$lgt_po_debugger0__def'(spy(Preds), _, _, _, '$lgt_dbg_spy'(Preds)).
'$lgt_po_debugger0__def'(nospy(Preds), _, _, _, '$lgt_dbg_nospy'(Preds)).
'$lgt_po_debugger0__def'(spy(Sender, This, Self, Goal), _, _, _, '$lgt_dbg_spy'(Sender, This, Self, Goal)).
'$lgt_po_debugger0__def'(nospy(Sender, This, Self, Goal), _, _, _, '$lgt_dbg_nospy'(Sender, This, Self, Goal)).
'$lgt_po_debugger0__def'(nospyall, _, _, _, '$lgt_dbg_nospyall').
'$lgt_po_debugger0__def'(leash(Ports), _, _, _, '$lgt_dbg_leash'(Ports)).


'$lgt_po_debugger0__def'(Pred, _, _, _, Call, debugger) :-
	'$lgt_po_debugger0__def'(Pred, _, _, _, Call).


'$lgt_dbg_reset' :-
	'$lgt_dbg_nospyall',
	'$lgt_dbg_leash'(full),
	'$lgt_dbg_nodebug'.


'$lgt_dbg_debug' :-
	'$lgt_dbg_debugging_' ->
		write('Debugger is on: showing spy points for all objects compiled in debug mode.'), nl
		;
		assertz('$lgt_dbg_debugging_'),
		retractall('$lgt_dbg_tracing_'),
		write('Debugger switched on: showing spy points for all objects compiled in debug mode.'), nl.


'$lgt_dbg_nodebug' :-
	'$lgt_dbg_debugging_' ->
		retractall('$lgt_dbg_debugging_'),
		retractall('$lgt_dbg_tracing_'),
		write('Debugger switched off.'), nl
		;
		write('Debugger is off.'), nl.


'$lgt_dbg_trace' :-
	'$lgt_dbg_tracing_' ->
		write('Debugger is on: tracing everything for all objects compiled in debug mode.'), nl
		;
		assertz('$lgt_dbg_tracing_'),
		retractall('$lgt_dbg_debugging_'),
		assertz('$lgt_dbg_debugging_'),
		write('Debugger switched on: tracing everything for all objects compiled in debug mode.'), nl.


'$lgt_dbg_notrace' :-
	'$lgt_dbg_tracing_' ->
		retractall('$lgt_dbg_tracing_'),
		retractall('$lgt_dbg_debugging_'),
		write('Debugger switched off.'), nl
		;
		write('Debugger is off.'), nl.


'$lgt_dbg_debugging' :-
	('$lgt_dbg_debugging_' ->
		write('Debugger is on: '),
		('$lgt_dbg_tracing_' ->
			write('tracing everything.'), nl
			;
			write('showing spy points.'), nl)
		;
		write('Debugger is off.'), nl), nl,
	('$lgt_dbg_spying_'(_, _) ->
		write('Defined predicate spy points (Functor/Arity):'), nl,
		forall(
			'$lgt_dbg_spying_'(Functor, Arity),
			(write('    '), writeq(Functor), write('/'), write(Arity), nl))
		;
		write('No predicate spy points are defined.'), nl), nl,
	('$lgt_dbg_spying_'(_, _, _, _) ->
		write('Defined context spy points (Sender, This, Self, Goal):'), nl,
		forall(
			'$lgt_dbg_spying_'(Sender, This, Self, Goal),
			(write('    '), '$lgt_dbg_pretty_print_spypoint'(Sender, This, Self, Goal), nl))
		;
		write('No context spy points are defined.'), nl), nl,
	write('Leashed ports:'), nl, write('    '),
	('$lgt_dbg_leashing_'(_) ->
		forall(
			'$lgt_dbg_leashing_'(Port),
			(write(Port), write(' ')))
		;
		write('(none)')),
	nl.


'$lgt_dbg_debugging'(Entity) :-
	'$lgt_debugging_'(Entity).


'$lgt_dbg_pretty_print_spypoint'(Sender, This, Self, Goal) :-
	current_output(Output),
	(var(Sender) -> write('_, '); '$lgt_pretty_print_vars_quoted'(Output, Sender), write(', ')),
	(var(This) -> write('_, '); '$lgt_pretty_print_vars_quoted'(Output, This), write(', ')),
	(var(Self) -> write('_, '); '$lgt_pretty_print_vars_quoted'(Output, Self), write(', ')),
	(var(Goal) -> write('_'); '$lgt_pretty_print_vars_quoted'(Output, Goal)).


'$lgt_dbg_spy'(Preds) :-
	nonvar(Preds),
	'$lgt_dbg_spy_aux'(Preds),
	write('Predicate spy points set.'), nl,
	('$lgt_dbg_debugging_' ->
		true
		;
		'$lgt_dbg_debug').


'$lgt_dbg_spy_aux'([]).

'$lgt_dbg_spy_aux'([Functor/Arity| Preds]) :-
	nonvar(Functor),
	nonvar(Arity),
	('$lgt_dbg_spying_'(Functor, Arity) ->
		true
		;
		assertz('$lgt_dbg_spying_'(Functor, Arity))),
	'$lgt_dbg_spy_aux'(Preds).

'$lgt_dbg_spy_aux'(Functor/Arity) :-
	nonvar(Functor),
	nonvar(Arity),
	('$lgt_dbg_spying_'(Functor, Arity) ->
		true
		;
		assertz('$lgt_dbg_spying_'(Functor, Arity))).


'$lgt_dbg_nospy'(Preds) :-
	'$lgt_dbg_nospy_aux'(Preds),
	write('All matching predicate spy points removed.'), nl.


'$lgt_dbg_nospy_aux'(Preds) :-
	var(Preds) ->
		retractall('$lgt_dbg_spying_'(_, _))
		;
		'$lgt_dbg_nospy_aux2'(Preds).


'$lgt_dbg_nospy_aux2'([]).

'$lgt_dbg_nospy_aux2'([Functor/Arity| Preds]) :-
	retractall('$lgt_dbg_spying_'(Functor, Arity)),
	'$lgt_dbg_nospy_aux2'(Preds).

'$lgt_dbg_nospy_aux2'(Functor/Arity) :-
	retractall('$lgt_dbg_spying_'(Functor, Arity)).


'$lgt_dbg_spy'(Sender, This, Self, Goal) :-
	asserta('$lgt_dbg_spying_'(Sender, This, Self, Goal)),
	write('Context spy point set.'), nl,
	('$lgt_dbg_debugging_' ->
		true
		;
		'$lgt_dbg_debug').


'$lgt_dbg_nospy'(Sender, This, Self, Goal) :-
	retractall('$lgt_dbg_spying_'(Sender, This, Self, Goal)),
	write('All matching context spy points removed.'), nl.


'$lgt_dbg_nospyall' :-
	retractall('$lgt_dbg_spying_'(_, _)),
	write('All predicate spy points removed.'), nl,
	retractall('$lgt_dbg_spying_'(_, _, _, _)),
	write('All context spy points removed.'), nl.


'$lgt_dbg_leash'(Value) :-
	'$lgt_dbg_valid_leash_value'(Value, Ports),
	retractall('$lgt_dbg_leashing_'(_)),
	'$lgt_dbg_set_leash_ports'(Ports),
	write('Debugger leash ports set to '), write(Ports), nl.

	
'$lgt_dbg_set_leash_ports'([]).

'$lgt_dbg_set_leash_ports'([Port| Ports]) :-
	assertz('$lgt_dbg_leashing_'(Port)),
	'$lgt_dbg_set_leash_ports'(Ports).


'$lgt_dbg_leashing_'(fact).
'$lgt_dbg_leashing_'(rule).
'$lgt_dbg_leashing_'(call).
'$lgt_dbg_leashing_'(exit).
'$lgt_dbg_leashing_'(redo).
'$lgt_dbg_leashing_'(fail).
'$lgt_dbg_leashing_'(exception).


'$lgt_dbg_valid_leash_value'(Shorthand, Ports) :-
	atom(Shorthand),
	Shorthand \= [],
	!,
	'$lgt_dbg_leash_shortand_ports'(Shorthand, Ports).

'$lgt_dbg_valid_leash_value'(Ports, Ports) :-
	'$lgt_proper_list'(Ports),
	'$lgt_dbg_valid_leash_ports'(Ports).


'$lgt_dbg_valid_leash_ports'([]).

'$lgt_dbg_valid_leash_ports'([Port| Ports]) :-
	nonvar(Port),
	'$lgt_dbg_valid_leash_port'(Port),
	'$lgt_dbg_valid_leash_ports'(Ports).


'$lgt_dbg_valid_leash_port'(fact).
'$lgt_dbg_valid_leash_port'(rule).
'$lgt_dbg_valid_leash_port'(call).
'$lgt_dbg_valid_leash_port'(exit).
'$lgt_dbg_valid_leash_port'(redo).
'$lgt_dbg_valid_leash_port'(fail).
'$lgt_dbg_valid_leash_port'(exception).


'$lgt_dbg_leash_shortand_ports'(none, []).
'$lgt_dbg_leash_shortand_ports'(loose, [fact, rule, call]).
'$lgt_dbg_leash_shortand_ports'(half, [fact, rule, call, redo]).
'$lgt_dbg_leash_shortand_ports'(tight, [fact, rule, call, redo, fail, exception]).
'$lgt_dbg_leash_shortand_ports'(full, [fact, rule, call, exit, redo, fail, exception]).


'$lgt_dbg_leashing'(Port, Goal, Ctx, Code) :-
	'$lgt_dbg_leashing_'(Port),
	('$lgt_dbg_tracing_' ->
		Code = ' '
		;
		'$lgt_dbg_spying'(Port, Goal, Ctx, Code)).


'$lgt_dbg_spying'(_, Goal, _, '+') :-
	functor(Goal, Functor, Arity),
	\+ \+ '$lgt_dbg_spying_'(Functor, Arity),
	!.
	
'$lgt_dbg_spying'(_, Goal, Ctx, '*') :-
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	\+ \+ '$lgt_dbg_spying_'(Sender, This, Self, Goal).


'$lgt_dbg_fact'(Fact, Ctx) :-
	'$lgt_dbg_debugging_',
	\+ '$lgt_dbg_skipping_',
	!,
	'$lgt_dbg_port'(fact, Fact, _, Ctx, Action),
	call(Action).

'$lgt_dbg_fact'(_, _).


'$lgt_dbg_head'(Head, Ctx) :-
	'$lgt_dbg_debugging_',
	\+ '$lgt_dbg_skipping_',
	!,
	'$lgt_dbg_port'(rule, Head, _, Ctx, Action),
	call(Action).

'$lgt_dbg_head'(_, _).


'$lgt_dbg_goal'(Goal, TGoal, Ctx) :-
	'$lgt_dbg_debugging_',
	\+ '$lgt_dbg_skipping_',
	!,
	(	'$lgt_dbg_port'(call, Goal, _, Ctx, CAction),
		(CAction = skip ->
			retractall('$lgt_dbg_skipping_'),
			assertz('$lgt_dbg_skipping_'),
			CAction2 = true
			;
			CAction2 = CAction),
		call(CAction2),
		catch(
			call(TGoal),
			Error,
			('$lgt_dbg_port'(exception, Goal, Error, Ctx, TAction),
			 (TAction = fail -> fail; throw(Error)))),
		(	'$lgt_dbg_port'(exit, Goal, _, Ctx, EAction),
			call(EAction)
			;
			'$lgt_dbg_port'(redo, Goal, _, Ctx, RAction),
			(RAction = skip ->
				retractall('$lgt_dbg_skipping_'),
				assertz('$lgt_dbg_skipping_')),
			fail
		)
		;
		retractall('$lgt_dbg_skipping_'),
		'$lgt_dbg_port'(fail, Goal, _, Ctx, _), fail
	),
	retractall('$lgt_dbg_skipping_').

'$lgt_dbg_goal'(_, TGoal, _) :-
	call(TGoal).


'$lgt_dbg_port'(Port, Goal, Error, Ctx, Action) :-
	'$lgt_dbg_debugging_',
	!,
	('$lgt_dbg_leashing'(Port, Goal, Ctx, Code) ->
		repeat,
			write(Code), '$lgt_dbg_write_port_name'(Port), writeq(Goal), write(' ? '),
			catch('$lgt_read_single_char'(Option), _, fail),
		'$lgt_dbg_valid_port_option'(Option, Port, Code),
		catch('$lgt_dbg_do_port_option'(Option, Goal, Error, Ctx, Action), _, fail)
		;
		('$lgt_dbg_tracing_' ->
			write(' '), '$lgt_dbg_write_port_name'(Port), writeq(Goal), nl
			;
			true),
		Action = true
	),
	!.

'$lgt_dbg_port'(_, _, _, _, true).


'$lgt_dbg_write_port_name'(fact) :-
	write('   Fact: ').
'$lgt_dbg_write_port_name'(rule) :-
	write('   Rule: ').
'$lgt_dbg_write_port_name'(call) :-
	write('   Call: ').
'$lgt_dbg_write_port_name'(exit) :-
	write('   Exit: ').
'$lgt_dbg_write_port_name'(redo) :-
	write('   Redo: ').
'$lgt_dbg_write_port_name'(fail) :-
	write('   Fail: ').
'$lgt_dbg_write_port_name'(exception) :-
	write('   Exception: ').


'$lgt_dbg_valid_port_option'(' ', _, _).
'$lgt_dbg_valid_port_option'(c, _, _).
'$lgt_dbg_valid_port_option'(l, _, _).
'$lgt_dbg_valid_port_option'(s, call, _).
'$lgt_dbg_valid_port_option'(s, redo, _).
'$lgt_dbg_valid_port_option'(f, _, _).
'$lgt_dbg_valid_port_option'(n, _, _).
'$lgt_dbg_valid_port_option'(@, _, _).
'$lgt_dbg_valid_port_option'(b, _, _).
'$lgt_dbg_valid_port_option'(a, _, _).
'$lgt_dbg_valid_port_option'(d, _, _).
'$lgt_dbg_valid_port_option'(x, _, _).
'$lgt_dbg_valid_port_option'(h, _, _).
'$lgt_dbg_valid_port_option'(?, _, _).
'$lgt_dbg_valid_port_option'(=, _, _).
'$lgt_dbg_valid_port_option'(*, _, ' ').
'$lgt_dbg_valid_port_option'(+, _, ' ').
'$lgt_dbg_valid_port_option'(-, _, +).
'$lgt_dbg_valid_port_option'(e, exception, _).


'$lgt_dbg_do_port_option'(' ', _, _, _, true).
'$lgt_dbg_do_port_option'(c, _, _, _, true).

'$lgt_dbg_do_port_option'(l, _, _, _, true) :-
	retractall('$lgt_dbg_tracing_').

'$lgt_dbg_do_port_option'(s, _, _, _, skip).

'$lgt_dbg_do_port_option'(f, _, _, _, fail).

'$lgt_dbg_do_port_option'(n, _, _, _, true) :-
	'$lgt_dbg_nodebug'.

'$lgt_dbg_do_port_option'(=, _, _, _, true) :-
	'$lgt_dbg_debugging'.

'$lgt_dbg_do_port_option'(+, Goal, _, _, _) :-
	(Goal = (_ :: Pred) ->
		functor(Pred, Functor, Arity)
		;
		functor(Goal, Functor, Arity)),
	'$lgt_dbg_spy'(Functor/Arity),
	fail.

'$lgt_dbg_do_port_option'(-, Goal, _, _, true) :-
	(Goal = (_ :: Pred) ->
		functor(Pred, Functor, Arity)
		;
		functor(Goal, Functor, Arity)),
	'$lgt_dbg_nospy'(Functor/Arity).

'$lgt_dbg_do_port_option'(*, Goal, _, _, true) :-
	functor(Goal, Functor, Arity),
	functor(CGoal, Functor, Arity),
	write('    Enter a context spy point term formatted as (Sender, This, Self, Goal): '),
	read(Spypoint),
	Spypoint = (Sender, This, Self, CGoal),
	'$lgt_dbg_spy'(Sender, This, Self, CGoal).

'$lgt_dbg_do_port_option'(@, _, _, _, _) :-
	write('    ?- '),
	read(Goal),
	once((Goal; true)),
	fail.

'$lgt_dbg_do_port_option'(b, _, _, _, _) :-
	('$lgt_compiler_flag'(supports_break_predicate, true) ->
		break
		;
		write('    break no supportd on this Prolog compiler.'), nl),
	fail.

'$lgt_dbg_do_port_option'(a, _, _, _, _) :-
	throw(error(logtalk_debugger_aborted)).

'$lgt_dbg_do_port_option'(d, Goal, _, _, _) :-
	write('    Current goal: '), write_term(Goal, [ignore_ops(true)]), nl,
	fail.

'$lgt_dbg_do_port_option'(x, _, _, Ctx, _) :-
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	write('    Sender: '), writeq(Sender), nl,
	write('    This:   '), writeq(This), nl,
	write('    Self:   '), writeq(Self), nl,
	fail.

'$lgt_dbg_do_port_option'(e, _, Error, _, _) :-
	write('    Exception term: '), writeq(Error), nl,
	fail.

'$lgt_dbg_do_port_option'(h, _, _, _, _) :-
	write('    Available options are:'), nl,
	write('        c - creep (go on; you may use the spacebar in alternative)'), nl,
	write('        l - leep (continues execution until the next spy point is found)'), nl,
	write('        s - skip (skips debugging for the current goal; only valid at call and redo ports)'), nl,
	write('        f - fail (forces backtracking)'), nl,
	write('        n - nodebug (turns off debugging)'), nl,
	write('        @ - command (reads and executes a query)'), nl,
	write('        b - break (suspends execution and starts new interpreter; type end_of_file to terminate)'), nl,
	write('        a - abort (returns to top level interpreter)'), nl,
	write('        d - display (writes current goal without using operator notation)'), nl,
	write('        x - context (prints execution context)'), nl,
	write('        e - exception (prints exception term thrown by current goal)'), nl,
	write('        = - debugging (prints debugging information)'), nl,
	write('        * - add (adds a context spy point for current goal)'), nl,
	write('        + - add (adds a predicate spy point for current goal)'), nl,
	write('        - - remove (removes a predicate spy point for current goal)'), nl,
	write('        h - help (prints this list of options)'), nl,
	fail.

'$lgt_dbg_do_port_option'(?, Goal, Error, Ctx, Action) :-
	'$lgt_dbg_do_port_option'(h, Goal, Error, Ctx, Action).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor - compiles Logtalk source files to Prolog
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_load_files'(@source_file_name_list)
%
% compiles to disk and then loads to memory a list of source files

'$lgt_load_files'([]).

'$lgt_load_files'([File| Files]) :-
	'$lgt_load_file'(File),
	'$lgt_load_files'(Files).



% '$lgt_load_file'(@source_file_name)
%
% compiles to disk and then loads to memory a source file

'$lgt_load_file'(Term) :-
	compound(Term),
	!,
	Term =.. [Library, File],
	once(logtalk_library_path(Library, Path)),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),
	'$lgt_report_working_directory'(Path),
	'$lgt_load_file'(File),
	'$lgt_change_directory'(Current).

'$lgt_load_file'(File) :-
	'$lgt_report_loading_file'(File),
	'$lgt_compile_file'(File),
	'$lgt_file_name'(prolog, File, PFile),
	'$lgt_load_prolog_code'(PFile),
	'$lgt_report_loaded_file'(File).



% '$lgt_redefined_entity'(@entity_identifier, -atom)
%
% true if an entity of the same name is already loaded; returns entity type

'$lgt_redefined_entity'(Entity, object) :-
	'$lgt_current_object_'(Entity, _, _, _, _, _),
	!.

'$lgt_redefined_entity'(Entity, protocol) :-
	'$lgt_current_protocol_'(Entity, _, _),
	!.

'$lgt_redefined_entity'(Entity, category) :-
	'$lgt_current_category_'(Entity, _, _).



% '$lgt_clean_redefined_entity'(+atom, @entity_identifier)
%
% retract all clauses for all dynamically declared predicates from a redefined entity

'$lgt_clean_redefined_entity'(object, Entity) :-
	'$lgt_current_object_'(Entity, Prefix, _, _, _, _),
	'$lgt_call'(Prefix, _, _, _, _, _, DDcl, DDef, _),
	DDefClause =.. [DDef, _, _, _, _, DDefHead],
	forall(
		retract(DDefClause),
		retractall(DDefHead)),
	DDclClause =.. [DDcl, _, _],
	retractall(DDclClause).

'$lgt_clean_redefined_entity'(protocol, _).

'$lgt_clean_redefined_entity'(category, _).



% '$lgt_report_redefined_entity'(+atom, @entity_identifier)
%
% prints a warning for redefined entities

'$lgt_report_redefined_entity'(Type, Entity) :-
	'$lgt_compiler_flag'(report, on) ->
		'$lgt_inc_load_warnings_counter',
		write('  WARNING!  redefining '), write(Type), write(' '), 
		current_output(Output), '$lgt_pretty_print_vars_quoted'(Output, Entity), nl
		;
		true.



% '$lgt_report_compiling_entity'(+atom, +entity_identifier)
%
% prints a message that an entity is being compiled

'$lgt_report_compiling_entity'(Type, Entity) :-
	'$lgt_reset_entity_warnings_flag',
	('$lgt_compiler_flag'(report, on) ->
		write('compiling '), write(Type),	write(' '),
		current_output(Output), '$lgt_pretty_print_vars_quoted'(Output, Entity),
		('$lgt_compiler_flag'(debug, on) ->
			write(' in debug mode... ')
			;
			write('... '))
		;
		true).



% '$lgt_report_compiled_entity'(+atom, +entity_identifier)
%
% prints a message that an entity is finished compiling

'$lgt_report_compiled_entity'(_, _) :-
	'$lgt_compiler_flag'(report, on) ->
		('$lgt_entity_warnings_flag' -> nl; true),
		write('compiled'), nl
		;
		true.



% '$lgt_report_loaded_entity'(+entity_identifier)
%
% prints a message that an entity finished loading

'$lgt_report_loaded_entity'(Entity) :-
	'$lgt_compiler_flag'(report, on) ->
		write('< '), writeq(Entity), write(' loaded'), nl
		;
		true.



% '$lgt_report_working_directory'(+atom)
%
% prints the working directory being used for compiling/loading source files

'$lgt_report_working_directory'(Directory) :-
	'$lgt_compiler_flag'(report, on) ->
		nl, write('+++ working on directory '), write(Directory), nl
		;
		true.



% '$lgt_report_compiling_file'(+entity_identifier)
%
% prints a message that an entity is being compiled

'$lgt_report_compiling_file'(File) :-
	'$lgt_compiler_flag'(report, on) ->
		write('>>> compiling source file '), writeq(File),
		('$lgt_compiler_flag'(debug, on) ->
			write(' in debug mode...')
			;
			write('...')),
		nl
		;
		true.



% '$lgt_report_up_to_date_file'(+entity_identifier)
%
% prints a message that an entity is up-to-date

'$lgt_report_up_to_date_file'(File) :-
	'$lgt_compiler_flag'(report, on) ->
		write('>>> compiling source file '), writeq(File), write('... up-to-date'), nl
		;
		true.



% '$lgt_report_compiled_file'(+atom)
%
% prints a message that a source file is finished compiling

'$lgt_report_compiled_file'(File) :-
	'$lgt_compiler_flag'(report, on) ->
		write('>>> '), writeq(File), write(' source file compiled'), nl
		;
		true.



% '$lgt_report_loading_file'(+atom)
%
% prints a message that an entity is being compiled

'$lgt_report_loading_file'(File) :-
	'$lgt_compiler_flag'(report, on) ->
		write('<<< loading source file '), writeq(File), write('... '), nl
		;
		true.



% '$lgt_report_loaded_file'(+entity_identifier)
%
% prints a message that a source file finished loading

'$lgt_report_loaded_file'(File) :-
	'$lgt_compiler_flag'(report, on) ->
		write('<<< '), writeq(File), write(' source file loaded'), nl
		;
		true.



% '$lgt_compile_files'(@source_file_name_list)
%
% compiles to disk a list of source files

'$lgt_compile_files'([]).

'$lgt_compile_files'([File| Files]) :-
	'$lgt_compile_file'(File),
	'$lgt_compile_files'(Files).



% '$lgt_compile_file'(@source_file_name)
%
% compiles to disk a source file

'$lgt_compile_file'(Term) :-
	compound(Term),
	!,
	Term =.. [Library, File],
	once(logtalk_library_path(Library, Path)),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),
	'$lgt_report_working_directory'(Path),
	'$lgt_compile_file'(File),
	'$lgt_change_directory'(Current).

'$lgt_compile_file'(File) :-
	'$lgt_compiler_flag'(smart_compilation, on),
	\+ '$lgt_needs_recompilation'(File),
	!,
	'$lgt_report_up_to_date_file'(File).

'$lgt_compile_file'(File) :-
	'$lgt_report_compiling_file'(File),
	'$lgt_tr_file'(File),
	'$lgt_report_compiled_file'(File).



% '$lgt_needs_recompilation'(+atom)
%
% source file needs recompilation

'$lgt_needs_recompilation'(File) :-
	'$lgt_file_name'(prolog, File, Object),
	\+ '$lgt_file_exists'(Object),
	!.

'$lgt_needs_recompilation'(File) :-
	'$lgt_file_name'(logtalk, File, Source),
	'$lgt_file_name'(prolog, File, Object),
	('$lgt_compare_file_mtimes'(Result, Source, Object) ->
		Result = (>)
		;
		true).



% '$lgt_write_tr_entity'(+stream)
%
% writes to disk the entity compiled code

'$lgt_write_tr_entity'(Stream) :-
	catch((
		'$lgt_write_directives'(Stream),
		'$lgt_write_prolog_clauses'(Stream),
		'$lgt_write_logtalk_clauses'(Stream)),
		Error,
		'$lgt_compiler_error_handler'(Stream, Error)).



% '$lgt_write_entity_doc'(@entity_identifier)
%
% writes to disk the entity documentation in XML format

'$lgt_write_entity_doc'(Entity) :-
	'$lgt_compiler_flag'(xml, on) ->
		'$lgt_entity_doc_file_name'(Entity, File),
		catch((
			('$lgt_pp_directive_'(encoding(Encoding)) ->
				open(File, write, Stream, [encoding(Encoding)])
				;
				open(File, write, Stream)),
			 '$lgt_write_xml_file'(Stream),
			 close(Stream)),
			Error,
			'$lgt_compiler_error_handler'(Stream, Error))	
		;
		true.


'$lgt_entity_doc_file_name'(Entity, File) :-
	functor(Entity, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, '_', Aux),
	atom_concat(Aux, Atom, Name),
	'$lgt_file_name'(xml, Name, File).



% '$lgt_file_name'(+atom, +atom, -atom)
%
% constructs a filename given the type of file and the entity name

'$lgt_file_name'(Type, Entity, File) :-
	'$lgt_file_extension'(Type, Extension),
	(('$lgt_compiler_flag'(altdirs, on), '$lgt_alt_directory'(Type, Directory)) ->
		'$lgt_make_directory'(Directory),
		atom_concat(Entity, Extension, Aux),
		atom_concat(Directory, Aux, File)
		;
		atom_concat(Entity, Extension, File)).



% '$lgt_tr_file'(+atom)
%
% compiles a source file storing the resulting code in memory

'$lgt_tr_file'(File) :-
	'$lgt_file_name'(logtalk, File, Source),
	catch((
		open(Source, read, SourceStream),
		read_term(SourceStream, Term, [singletons(Singletons)])),
		SourceError,
		'$lgt_compiler_error_handler'(SourceStream, SourceError)),
	'$lgt_check_for_encoding_directive'(Term, SourceStream),	% encoding/1 directive, when present, must be the first term on file
	'$lgt_file_name'(prolog, File, Object),
	catch(
		('$lgt_pp_directive_'(encoding(Encoding)) ->
			open(Object, write, ObjectStream, [encoding(Encoding)])
			;
			open(Object, write, ObjectStream)),
		ObjectError,
		'$lgt_compiler_error_handler'(ObjectStream, ObjectError)),
	'$lgt_clean_pp_clauses',
	'$lgt_save_global_op_table',
	catch(
		 '$lgt_tr_file'(Term, Singletons, SourceStream, ObjectStream),
		Error,
		'$lgt_compiler_error_handler'(SourceStream, Error)),
	catch(
		('$lgt_write_directives'(ObjectStream),					% write out any Prolog code that may occur
		 '$lgt_write_prolog_clauses'(ObjectStream),				% after the last entity on the source file
		'$lgt_write_init_call'(ObjectStream)),
		ObjectError,
		'$lgt_compiler_error_handler'(ObjectStream, ObjectError)),
	'$lgt_restore_global_op_table',
	close(SourceStream),
	close(ObjectStream).



'$lgt_check_for_encoding_directive'((:- encoding(Encoding)), Stream) :-	% encoding/1 directives must be used during entity compilation
	!,
	('$lgt_compiler_flag'(supports_encoding_dir, true) ->
		'$lgt_set_stream_encoding'(Stream, Encoding)
		;
		throw(error(domain_error(directive, encoding/1), directive(encoding(Encoding))))).

'$lgt_check_for_encoding_directive'(_, _).



% '$lgt_tr_file'(+term, +list, +stream, +stream)

'$lgt_tr_file'(end_of_file, _, _, _) :-
	!.

'$lgt_tr_file'(Term, Singletons, SourceStream, ObjectStream) :-
	'$lgt_report_singletons'(Singletons, Term),
	'$lgt_tr_term'(Term, ObjectStream),
	read_term(SourceStream, Next, [singletons(NSingletons)]),
	'$lgt_tr_file'(Next, NSingletons, SourceStream, ObjectStream).



% '$lgt_report_singletons'(+list, +term)
%
% report the singleton variables found while compiling an entity term

'$lgt_report_singletons'(TSingletons, Term) :-
	'$lgt_filter_dont_care_vars'(TSingletons, FSingletons),
	'$lgt_singleton_var_names'(FSingletons, Names),
	'$lgt_report_singletons_aux'(Names, Term).


'$lgt_report_singletons_aux'([], _) :-
	!.	% cut needed to prevent problems with compilers with broken read_term/3

'$lgt_report_singletons_aux'([Name| Names], Term) :-
	('$lgt_compiler_flag'(singletons, warning), '$lgt_compiler_flag'(report, on)) ->
		'$lgt_inc_compile_warnings_counter',
		('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
		write('  WARNING!  singleton variables ('), '$lgt_write_list'([Name| Names]),
		arg(1, Term, Term2),
		functor(Term2, Functor, Arity),
		(Term = (:- _) ->
			write(') in directive ')
			;
			write(') in clause for predicate ')),
		write(Functor/Arity),
		('$lgt_pp_entity'(_, _, _, _, _) -> true; nl)
		;
		true.



% '$lgt_singleton_var_names'(@list, -list)
%
% colects singleton variable names into a list

'$lgt_singleton_var_names'([], []).

'$lgt_singleton_var_names'([Name = _| Singletons], [Name| Names]) :-
	'$lgt_singleton_var_names'(Singletons, Names).



% '$lgt_filter_dont_care_vars'(+list, -list)
%
% filter variables whose name start with an underscore from a singletons list if 
% the corresponding compiler flag sets their interpretation to don't care variables

'$lgt_filter_dont_care_vars'(List, Result) :-
	'$lgt_compiler_flag'(underscore_vars, dont_care) ->
		'$lgt_filter_dont_care_vars'(List, [], Result)
		;
		List = Result.


'$lgt_filter_dont_care_vars'([], Result, Result) :-
	!.	% cut needed to prevent problems with compilers with broken read_term/3

'$lgt_filter_dont_care_vars'([Atom = Var| List], Sofar, Result) :-
	atom_concat('_', _, Atom) ->
		'$lgt_filter_dont_care_vars'(List, Sofar, Result)
		;
		'$lgt_filter_dont_care_vars'(List, [Atom = Var| Sofar], Result).



% '$lgt_compiler_error_handler'(@term, +term)
%
% closes the stream opened for reading the entity file, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_error_handler'(Stream, Error) :-
	(nonvar(Stream) ->
		close(Stream)
		;
		true),
	'$lgt_restore_global_op_table',
	'$lgt_reset_warnings_counter',
	'$lgt_report_compiler_error'(Error),
	throw(Error).



% '$lgt_report_compiler_error'(+term)
%
% reports a compilation error

'$lgt_report_compiler_error'(error(Error, directive(Directive))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('  ERROR!  '), writeq(Error), nl,
	write('          in directive: '), writeq((:- Directive)), nl.

'$lgt_report_compiler_error'(error(Error, clause(Clause))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('  ERROR!  '), writeq(Error), nl,
	write('          in clause: '), writeq(Clause), nl.

'$lgt_report_compiler_error'(error(Error, dcgrule(Rule))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('  ERROR!  '), writeq(Error), nl,
	write('          in grammar rule: '), writeq((Rule)), nl.

'$lgt_report_compiler_error'(error(Error, Term)) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('  ERROR!  '), writeq(Error), nl,
	write('          in: '), writeq(Term), nl.

'$lgt_report_compiler_error'(Error) :-
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('  ERROR!  '), writeq(Error), nl.



% '$lgt_tr_entity'(+atom, @entity_identifier, +stream)

'$lgt_tr_entity'(Type, Entity, Stream) :-
	'$lgt_generate_code'(Type),
	'$lgt_report_problems'(Type),
	'$lgt_write_tr_entity'(Stream),
	'$lgt_write_entity_doc'(Entity),
	'$lgt_restore_file_op_table',
	'$lgt_clean_pp_entity_clauses'.



% clean up all dynamic predicates used during source file compilation

'$lgt_clean_pp_clauses' :-
	'$lgt_clean_pp_entity_clauses',
	retractall('$lgt_pp_global_op_'(_, _, _)),
	retractall('$lgt_pp_file_op_'(_, _, _)),
	retractall('$lgt_pp_file_init_'(_)),	
	retractall('$lgt_pp_entity_init_'(_, _)).



% clean up all dynamic predicates used during entity compilation

'$lgt_clean_pp_entity_clauses' :-
	retractall('$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_protocol_'(_, _, _, _, _)),
	retractall('$lgt_pp_category_'(_, _, _, _, _, _)),
	retractall('$lgt_pp_implemented_protocol_'(_, _, _, _)),
	retractall('$lgt_pp_imported_category_'(_, _, _, _, _)),
	retractall('$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_extended_protocol_'(_, _, _, _)),
	retractall('$lgt_pp_uses_'(_)),
	retractall('$lgt_pp_uses_'(_, _)),
	retractall('$lgt_pp_calls_'(_)),
	retractall('$lgt_pp_info_'(_)),
	retractall('$lgt_pp_info_'(_, _)),
	retractall('$lgt_pp_directive_'(_)),
	retractall('$lgt_pp_public_'(_, _)),
	retractall('$lgt_pp_protected_'(_, _)),
	retractall('$lgt_pp_private_'(_, _)),
	retractall('$lgt_pp_dynamic_'(_, _)),
	retractall('$lgt_pp_discontiguous_'(_, _)),
	retractall('$lgt_pp_mode_'(_, _)),
	retractall('$lgt_pp_metapredicate_'(_)),
	retractall('$lgt_pp_alias_'(_, _, _)),
	retractall('$lgt_pp_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_entity_init_'(_)),
	retractall('$lgt_pp_fentity_init_'(_)),
	retractall('$lgt_pp_dcl_'(_)),
	retractall('$lgt_pp_ddcl_'(_)),
	retractall('$lgt_pp_def_'(_)),
	retractall('$lgt_pp_ddef_'(_)),
	retractall('$lgt_pp_super_'(_)),
	retractall('$lgt_pp_ppclause_'(_)),
	retractall('$lgt_pp_rclause_'(_)),
	retractall('$lgt_pp_eclause_'(_)),
	retractall('$lgt_pp_feclause_'(_)),
	retractall('$lgt_pp_redefined_built_in_'(_, _, _)),
	retractall('$lgt_pp_defs_pred_'(_, _)),
	retractall('$lgt_pp_calls_pred_'(_, _)),
	retractall('$lgt_non_portable_call_'(_, _)),
	retractall('$lgt_pp_referenced_object_'(_)),
	retractall('$lgt_pp_referenced_protocol_'(_)),
	retractall('$lgt_pp_referenced_category_'(_)),
	retractall('$lgt_pp_entity_op_'(_, _, _)).



% '$lgt_clean_lookup_caches'
%
% clean lookup caches

'$lgt_clean_lookup_caches' :-
	retractall('$lgt_obj_lookup_cache_'(_, _, _, _)),
	retractall('$lgt_self_lookup_cache_'(_, _, _, _)),
	retractall('$lgt_super_lookup_cache_'(_, _, _, _, _)),
	retractall('$lgt_db_lookup_cache_'(_, _, _, _, _, _)).



% '$lgt_clean_lookup_caches'(@callable)
%
% clean lookup caches for the matching predicate

'$lgt_clean_lookup_caches'(Pred) :-
	retractall('$lgt_obj_lookup_cache_'(_, Pred, _, _)),
	retractall('$lgt_self_lookup_cache_'(_, Pred, _, _)),
	retractall('$lgt_super_lookup_cache_'(_, Pred, _, _, _)),
	retractall('$lgt_db_lookup_cache_'(_, Pred, _, _, _, _)).



% '$lgt_save_global_op_table'
%
% saves current operator table

'$lgt_save_global_op_table' :-
	current_op(Pr, Spec, Op),
		asserta('$lgt_pp_global_op_'(Pr, Spec, Op)),
	fail.

'$lgt_save_global_op_table'.



% '$lgt_restore_global_op_table'
%
% restores current operator table

'$lgt_restore_global_op_table' :-
	retract('$lgt_pp_entity_op_'(_, Spec, Op)),
		op(0, Spec, Op),
	fail.

'$lgt_restore_global_op_table' :-
	retractall('$lgt_pp_file_op_'(_, _, ',')),		% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_file_op_'(_, Spec, Op)),
		catch(op(0, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_global_op_table' :-
	retractall('$lgt_pp_global_op_'(_, _, ',')),	% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_global_op_'(Pr, Spec, Op)),
		catch(op(Pr, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_global_op_table'.



% '$lgt_save_file_op_table'
%
% saves current operator table

'$lgt_save_file_op_table' :-
	current_op(Pr, Spec, Op),
		asserta('$lgt_pp_file_op_'(Pr, Spec, Op)),
	fail.

'$lgt_save_file_op_table'.



% '$lgt_restore_file_op_table'
%
% restores current operator table

'$lgt_restore_file_op_table' :-
	retract('$lgt_pp_entity_op_'(_, Spec, Op)),
		op(0, Spec, Op),
	fail.

'$lgt_restore_file_op_table' :-
	retractall('$lgt_pp_file_op_'(_, _, ',')),		% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_file_op_'(Pr, Spec, Op)),
		catch(op(Pr, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_file_op_table'.



% '$lgt_assert_entity_ops'(+integer, +operator_specifier, +atom_or_atom_list)
%
% asserts local entity operators

'$lgt_assert_entity_ops'(_, _, []) :-
	!.

'$lgt_assert_entity_ops'(Pr, Spec, [Op| Ops]) :-
	!,
	asserta('$lgt_pp_entity_op_'(Pr, Spec, Op)),
	'$lgt_assert_entity_ops'(Pr, Spec, Ops).

'$lgt_assert_entity_ops'(Pr, Spec, Op) :-
	asserta('$lgt_pp_entity_op_'(Pr, Spec, Op)).



% '$lgt_pp_entity'(?atom, ?entity_identifier, ?atom, ?atom, ?atom)
%
% provides access to some common used data on the entity being compiled

'$lgt_pp_entity'(object, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_object_'(Entity, Prefix, Dcl, _, _, _, _, _, _, _, Mode),
	!.

'$lgt_pp_entity'(category, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_category_'(Entity, Prefix, Dcl, _, _, Mode),
	!.

'$lgt_pp_entity'(protocol, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_protocol_'(Entity, Prefix, Dcl, _, Mode).



% '$lgt_pp_rclause'(-compound)
%
% returns runtime table clauses for the entity being compiled

'$lgt_pp_rclause'(Clause) :-
	'$lgt_pp_rclause_'(Clause).

'$lgt_pp_rclause'('$lgt_debugging_'(Entity)) :-
	'$lgt_compiler_flag'(debug, on),
	'$lgt_pp_entity'(_, Entity, _, _, _).

'$lgt_pp_rclause'('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, Mode)) :-
	'$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, _, _, _, _, _, Mode),
	!.

'$lgt_pp_rclause'('$lgt_current_protocol_'(Ptc, Prefix, Mode)) :-
	'$lgt_pp_protocol_'(Ptc, Prefix, _, _, Mode),
	!.

'$lgt_pp_rclause'('$lgt_current_category_'(Ctg, Prefix, Mode)) :-
	'$lgt_pp_category_'(Ctg, Prefix, _, _, _, Mode).



% '$lgt_tr_terms'(+list, +stream)
%
% translates a list of entity terms (clauses and/or directives)

'$lgt_tr_terms'([], _, _).

'$lgt_tr_terms'([Term| Terms], Stream) :-
	'$lgt_tr_term'(Term, Stream),
	'$lgt_tr_terms'(Terms, Stream).



% '$lgt_tr_term'(+term, +stream, +stream)
%
% translates an entity term (either a clause or a directive)

'$lgt_tr_term'((Head :- Body), _) :-
	!,
	'$lgt_tr_clause'((Head :- Body)).

'$lgt_tr_term'((:- Directive), Stream) :-
	!,
	'$lgt_tr_directive'(Directive, Stream).

'$lgt_tr_term'((Head --> Body), _) :-
	!,
	'$lgt_dcgrule_to_clause'((Head --> Body), Clause),
	'$lgt_tr_clause'(Clause).

'$lgt_tr_term'(Fact, _) :-
	'$lgt_tr_clause'(Fact).



% '$lgt_tr_directives'(+list, @stream)
%
% translates a list of entity directives

'$lgt_tr_directives'([], _).

'$lgt_tr_directives'([Dir| Dirs], Stream) :-
	'$lgt_tr_directive'(Dir, Stream),
	'$lgt_tr_directives'(Dirs, Stream).



% '$lgt_tr_directive'(+term, @stream)
%
% translates an entity directive

'$lgt_tr_directive'(Dir, _) :-
	var(Dir),
	throw(error(instantiantion_error, directive(Dir))).

'$lgt_tr_directive'(Dir, _) :-
	\+ '$lgt_pp_entity'(_, _, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	'$lgt_lgt_closing_directive'(Functor, Arity),	% opening directive missing/misspelt
	throw(error(unmatched_directive, directive(Dir))).

'$lgt_tr_directive'(Dir, _) :-
	\+ '$lgt_pp_entity'(_, _, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	\+ '$lgt_lgt_opening_directive'(Functor, Arity),
	!,
	'$lgt_tr_file_directive'(Dir).

'$lgt_tr_directive'(Dir, Stream) :-
	functor(Dir, Functor, Arity),
	'$lgt_lgt_directive'(Functor, Arity),
	Dir =.. [Functor| Args],
	catch(
		'$lgt_tr_directive'(Functor, Args, Stream),
		Error,
		throw(error(Error, directive(Dir)))),
	!.

'$lgt_tr_directive'(Dir, _) :-
	functor(Dir, Functor, Arity),
	throw(error(domain_error(directive, Functor/Arity), directive(Dir))).



% '$lgt_tr_file_directive'(@nonvar)

'$lgt_tr_file_directive'(op(Pr, Spec, Ops)) :-	% op/3 directives must be used during entity compilation
	!,
	('$lgt_valid_op_priority'(Pr) ->
		('$lgt_valid_op_specifier'(Spec) ->
			('$lgt_valid_op_names'(Ops) ->
				assertz('$lgt_pp_directive_'(op(Pr, Spec, Ops))),
				assertz('$lgt_pp_file_op_'(op(Pr, Spec, Ops))),
				op(Pr, Spec, Ops)
				;
				throw(type_error(operator_name, Ops)))
			;
			throw(type_error(operator_specifier, Spec)))
		;
		throw(type_error(operator_priority, Pr))).

'$lgt_tr_file_directive'(initialization(Goal)) :-
	!,
	(callable(Goal) ->
		assertz('$lgt_pp_file_init_'(Goal))
		;
		throw(type_error(callable, Goal))).

'$lgt_tr_file_directive'(Dir) :-
	assertz('$lgt_pp_directive_'(Dir)).				% directive will be copied to the generated Prolog file



% '$lgt_tr_directive'(+atom, +list, @stream)
%
% translates a directive and its (possibly empty) list of arguments

'$lgt_tr_directive'(object, [Obj| Rels], _) :-
	callable(Obj) ->
		'$lgt_report_compiling_entity'(object, Obj),
		'$lgt_tr_object_id'(Obj, static),			% assume static category
		'$lgt_tr_object_relations'(Rels, Obj),
		'$lgt_save_file_op_table'
		;
		throw(type_error(object_identifier, Obj)).

'$lgt_tr_directive'(end_object, [], Stream) :-
	'$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		'$lgt_tr_entity'(object, Obj, Stream),
		'$lgt_report_compiled_entity'(object, Obj)
		;
		throw(closing_directive_mismatch).


'$lgt_tr_directive'(protocol, [Ptc| Rels], _) :-
	atom(Ptc) ->
		'$lgt_report_compiling_entity'(protocol, Ptc),
		'$lgt_tr_protocol_id'(Ptc, static),			% assume static category
		'$lgt_tr_protocol_relations'(Rels, Ptc),
		'$lgt_save_file_op_table'
		;
		throw(type_error(protocol_identifier, Ptc)).

'$lgt_tr_directive'(end_protocol, [], Stream) :-
	'$lgt_pp_protocol_'(Ptc, _, _, _, _) ->
		'$lgt_tr_entity'(protocol, Ptc, Stream),
		'$lgt_report_compiled_entity'(protocol, Ptc)
		;
		throw(closing_directive_mismatch).


'$lgt_tr_directive'(category, [Ctg| Rels], _) :-
	atom(Ctg) ->
		'$lgt_report_compiling_entity'(category, Ctg),
		'$lgt_tr_category_id'(Ctg, static),			% assume static category
		'$lgt_tr_category_relations'(Rels, Ctg),
		'$lgt_save_file_op_table'
		;
		throw(type_error(category_identifier, Ctg)).

'$lgt_tr_directive'(end_category, [], Stream) :-
	'$lgt_pp_category_'(Ctg, _, _, _, _, _) ->
		'$lgt_tr_entity'(category, Ctg, Stream),
		'$lgt_report_compiled_entity'(category, Ctg)
		;
		throw(closing_directive_mismatch).


% dynamic entity directive

'$lgt_tr_directive'((dynamic), [], _) :-
	!,
	'$lgt_update_entity_comp_mode'.


'$lgt_tr_directive'(initialization, [Goal], _) :-
	callable(Goal) ->
		'$lgt_pp_entity'(_, Entity, Prefix, _, _),
		'$lgt_ctx_ctx'(Ctx, Entity, Entity, Entity, Prefix, []),
		'$lgt_tr_body'(Goal, TGoal, _, Ctx),
		assertz('$lgt_pp_entity_init_'(TGoal))
		;
		throw(type_error(callable, Goal)).


'$lgt_tr_directive'(op, [Pr, Spec, Ops], _) :-
	'$lgt_valid_op_priority'(Pr) ->
		('$lgt_valid_op_specifier'(Spec) ->
			('$lgt_valid_op_names'(Ops) ->
				op(Pr, Spec, Ops),
				'$lgt_assert_entity_ops'(Pr, Spec, Ops)
				;
				throw(type_error(operator_name, Ops)))
			;
			throw(type_error(operator_specifier, Spec)))
		;
		throw(type_error(operator_priority, Pr)).


'$lgt_tr_directive'(uses, [Obj, Preds], _) :-
	!,
	(callable(Obj) ->
		assertz('$lgt_pp_referenced_object_'(Obj)),
		assertz('$lgt_pp_uses_'(Obj)),
		('$lgt_proper_list'(Preds) ->
			forall(
				'$lgt_member'(Pred, Preds),
				('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
					functor(Template, Functor, Arity),
					(\+ '$lgt_pp_uses_'(_, Template) ->
						assertz('$lgt_pp_uses_'(Obj, Template))
						;
						throw(permission_error(modify, uses_object_predicate, Functor/Arity)))
					;
					throw(type_error(predicate_indicator, Pred))))
			;
			throw(type_error(list, Preds)))
		;
		throw(type_error(object_identifier, Obj))).


'$lgt_tr_directive'(uses, [Obj], _) :-
	callable(Obj) ->
		assertz('$lgt_pp_referenced_object_'(Obj)),
		assertz('$lgt_pp_uses_'(Obj))
		;
		throw(type_error(object_identifier, Obj)).


'$lgt_tr_directive'(calls, Ptcs, _) :-
	'$lgt_convert_to_list'(Ptcs, Ptcs2),
	forall(
		'$lgt_member'(Ptc, Ptcs2),
		(atom(Ptc) ->
			assertz('$lgt_pp_referenced_protocol_'(Ptc)),
			assertz('$lgt_pp_calls_'(Ptc))
			;
			throw(type_error(protocol_identifier, Ptc)))).


'$lgt_tr_directive'(info, [List], _) :-
	!,
	('$lgt_tr_entity_info_list'(List) ->
		assertz('$lgt_pp_info_'(List))
		;
		throw(type_error(entity_info_list, List))).


'$lgt_tr_directive'(info, [Pred, List], _) :-
	'$lgt_valid_pred_or_gr_ind'(Pred, Functor, Arity) ->
		'$lgt_tr_pred_info_list'(List, Functor, Arity),
		assertz('$lgt_pp_info_'(Pred, List))
		;
		throw(type_error(predicate_indicator, Pred)).


'$lgt_tr_directive'((public), Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
			assertz('$lgt_pp_public_'(Functor, Arity))
			;
			('$lgt_valid_gr_ind'(Pred, Functor, Arity, Arity2) ->
				assertz('$lgt_pp_non_terminal_'(Functor, Arity, Arity2)),
				assertz('$lgt_pp_public_'(Functor, Arity2))
				;
				throw(type_error(predicate_indicator, Pred))))).


'$lgt_tr_directive'(protected, Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
			assertz('$lgt_pp_protected_'(Functor, Arity))
			;
			('$lgt_valid_gr_ind'(Pred, Functor, Arity, Arity2) ->
				assertz('$lgt_pp_non_terminal_'(Functor, Arity, Arity2)),
				assertz('$lgt_pp_protected_'(Functor, Arity2))
				;
				throw(type_error(predicate_indicator, Pred))))).


'$lgt_tr_directive'(private, Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
			assertz('$lgt_pp_private_'(Functor, Arity))
			;
			('$lgt_valid_gr_ind'(Pred, Functor, Arity, Arity2) ->
				assertz('$lgt_pp_non_terminal_'(Functor, Arity, Arity2)),
				assertz('$lgt_pp_private_'(Functor, Arity2))
				;
				throw(type_error(predicate_indicator, Pred))))).


'$lgt_tr_directive'((dynamic), Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
			assertz('$lgt_pp_dynamic_'(Functor, Arity))
			;
			('$lgt_valid_gr_ind'(Pred, Functor, Arity, Arity2) ->
				assertz('$lgt_pp_dynamic_'(Functor, Arity2))
				;
				throw(type_error(predicate_indicator, Pred))))).


'$lgt_tr_directive'((discontiguous), Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred, Functor, Arity) ->
			assertz('$lgt_pp_discontiguous_'(Functor, Arity))
			;
			('$lgt_valid_gr_ind'(Pred, Functor, Arity, Arity2) ->
				assertz('$lgt_pp_discontiguous_'(Functor, Arity2))
				;
				throw(type_error(predicate_indicator, Pred))))).


'$lgt_tr_directive'(metapredicate, Preds, _) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_metapred_term'(Pred) ->
			assertz('$lgt_pp_metapredicate_'(Pred))
			;
			throw(type_error(metapredicate_term, Pred)))).


'$lgt_tr_directive'((mode), [Mode, Solutions], _) :-
	'$lgt_valid_mode_term'(Mode) ->
		('$lgt_valid_number_of_solutions'(Solutions) ->
			assertz('$lgt_pp_mode_'(Mode, Solutions))
			;
			throw(type_error(number_of_solutions, Solutions)))
		;
		throw(type_error(mode_term, Mode)).


'$lgt_tr_directive'(alias, [_, PI1, _], _) :-
	\+ '$lgt_valid_pred_ind'(PI1, _, _),
	throw(type_error(predicate_indicator, PI1)).

'$lgt_tr_directive'(alias, [_, _, PI2], _) :-
	\+ '$lgt_valid_pred_ind'(PI2, _, _),
	throw(type_error(predicate_indicator, PI2)).

'$lgt_tr_directive'(alias, [Entity, _, _], _) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_directive'(alias, [_, Functor1/Arity1, Functor2/Arity2], _) :-
	Arity1 =\= Arity2,
	throw(domain_error(arity_mismatch, Functor1/Arity1, Functor2/Arity2)).

'$lgt_tr_directive'(alias, [Entity, PI1, PI2], _) :-
	('$lgt_pp_extended_protocol_'(Entity, _, _, _);
	 '$lgt_pp_implemented_protocol_'(Entity, _, _, _);
	 '$lgt_pp_imported_category_'(Entity, _, _, _, _);
	 '$lgt_pp_extended_object_'(Entity, _, _, _, _, _, _, _, _, _);
	 '$lgt_pp_instantiated_class_'(Entity, _, _, _, _, _, _, _, _, _);
	 '$lgt_pp_specialized_class_'(Entity, _, _, _, _, _, _, _, _, _)),
	!,
	'$lgt_tr_alias_directive'(Entity, PI1, PI2).

'$lgt_tr_directive'(alias, [Entity, _, _], _) :-
	throw(reference_error(entity_identifier, Entity)).



'$lgt_tr_alias_directive'(Entity, Functor1/Arity, Functor2/Arity) :-
	functor(Pred, Functor1, Arity),
	Pred =.. [_| Args],
	Alias =.. [Functor2| Args],
	assertz('$lgt_pp_alias_'(Entity, Pred, Alias)).



% '$lgt_tr_object_relations'(+list, +term)
%
% translates the relations of an object with other entities

'$lgt_tr_object_relations'([], _).

'$lgt_tr_object_relations'([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Args],
	('$lgt_tr_object_relation'(Functor, Args, Obj) ->
		'$lgt_tr_object_relations'(Clauses, Obj)
		;
		throw(domain_error(object_relation, Functor))).



% '$lgt_tr_object_relation'(+atom, +list, +term)
%
% translates a relation between an object (the last argument) with other entities

'$lgt_tr_object_relation'(implements, Ptcs, Obj) :-
	'$lgt_convert_to_list'(Ptcs, List),
	'$lgt_tr_implements_protocol'(List, Obj).

'$lgt_tr_object_relation'(imports, Ctgs, Obj) :-
	'$lgt_convert_to_list'(Ctgs, List),
	'$lgt_tr_imports_category'(List, Obj).

'$lgt_tr_object_relation'(instantiates, Classes, Obj) :-
	'$lgt_convert_to_list'(Classes, List),
	'$lgt_tr_instantiates_class'(List, Obj).

'$lgt_tr_object_relation'(specializes, Superclasses, Class) :-
	'$lgt_convert_to_list'(Superclasses, List),
	'$lgt_tr_specializes_class'(List, Class).

'$lgt_tr_object_relation'(extends, Parents, Prototype) :-
	'$lgt_convert_to_list'(Parents, List),
	'$lgt_tr_extends_object'(List, Prototype).



% '$lgt_tr_protocol_relations'(+list, +term)
%
% translates the relations of a protocol with other entities

'$lgt_tr_protocol_relations'([], _).

'$lgt_tr_protocol_relations'([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Args],
	('$lgt_tr_protocol_relation'(Functor, Args, Obj) ->
		'$lgt_tr_protocol_relations'(Clauses, Obj)
		;
		throw(domain_error(protocol_relation, Functor))).



% '$lgt_tr_protocol_relation'(+atom, +list, +term)
%
% translates a relation between a protocol (the last argument) with other entities

'$lgt_tr_protocol_relation'(extends, Ptcs, Ptc) :-
	'$lgt_convert_to_list'(Ptcs, List),
	'$lgt_tr_extends_protocol'(List, Ptc).



% '$lgt_tr_category_relations'(+list, +term)
%
% translates the relations of a category with other entities

'$lgt_tr_category_relations'([], _).

'$lgt_tr_category_relations'([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Args],
	('$lgt_tr_category_relation'(Functor, Args, Obj) ->
		'$lgt_tr_category_relations'(Clauses, Obj)
		;
		throw(domain_error(category_relation, Functor))).



% '$lgt_tr_category_relation'(+atom, +list, +term)
%
% translates a relation between a category (the last argument) with other entities

'$lgt_tr_category_relation'(implements, Ptcs, Ctg) :-
	'$lgt_convert_to_list'(Ptcs, List),
	'$lgt_tr_implements_protocol'(List, Ctg).

'$lgt_tr_category_relation'(imports, Ctgs, Ctg) :-
	'$lgt_convert_to_list'(Ctgs, List),
	'$lgt_tr_imports_category'(List, Ctg).



% '$lgt_tr_entity_info_list'(@list)
%
% true if the argument is a list of valid key-value pairs

'$lgt_tr_entity_info_list'(List) :-
	var(List),
	throw(instantiation_error). 

'$lgt_tr_entity_info_list'(List) :-
	\+ '$lgt_proper_list'(List),
	throw(type_error(list, List)).

'$lgt_tr_entity_info_list'([]).

'$lgt_tr_entity_info_list'([Head| _]) :-
	var(Head),
	throw(instantiation_error). 

'$lgt_tr_entity_info_list'([Head| _]) :-
	Head \= (_ is _),
	throw(type_error(key_value_info_pair, Head)).

'$lgt_tr_entity_info_list'([Key is Value| _]) :-
	(var(Key); var(Value)),
	throw(instantiation_error). 

'$lgt_tr_entity_info_list'([Key is _| _]) :-
	\+ atom(Key),
	throw(type_error(atom, Key)). 

'$lgt_tr_entity_info_list'([Key is Value| Tail]) :-
	'$lgt_tr_entity_info_key_value'(Key, Value),
	'$lgt_tr_entity_info_list'(Tail).



% '$lgt_tr_entity_info_key_value'(+atom, @nonvar)
%
% true if the argument is a valid key-value pair

'$lgt_tr_entity_info_key_value'(author, Author) :-
	!,
	(atom(Author) ->
		true
		;
		throw(type_error(atom, Author))).

'$lgt_tr_entity_info_key_value'(comment, Comment) :-
	!,
	(atom(Comment) ->
		true
		;
		throw(type_error(atom, Comment))).

'$lgt_tr_entity_info_key_value'(date, Date) :-
	!,
	(Date = Year/Month/Day ->
		(integer(Year) ->
			(integer(Month) ->
				(integer(Day) ->
					true
					;
					throw(type_error(integer, Day)))
				;
				throw(type_error(integer, Month)))
			;
			throw(type_error(integer, Year)))
		;
		throw(type_error(date, Date))).

'$lgt_tr_entity_info_key_value'(parameters, Parameters) :-
	!,
	('$lgt_proper_list'(Parameters) ->
		(('$lgt_member'(Parameter, Parameters), \+ '$lgt_valid_entity_parameter'(Parameter)) ->
			throw(type_error(parameter, Parameter))
			;
			(('$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
			  \+ \+ Obj =.. [_| Parameters]) ->
			 	true
			 	;
			 	throw(length_error(parameters_list, Parameters))))
		;
		throw(type_error(list, Parameters))).

'$lgt_tr_entity_info_key_value'(parnames, Parnames) :-
	!,
	('$lgt_proper_list'(Parnames) ->
		(('$lgt_member'(Name, Parnames), \+ atom(Name)) ->
			throw(type_error(atom, Name))
			;
			(('$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
			  \+ \+ Obj =.. [_| Parnames]) ->
			 	true
			 	;
			 	throw(length_error(parnames_list, Parnames))))
		;
		throw(type_error(list, Parnames))).

'$lgt_tr_entity_info_key_value'(version, Version) :-
	!,
	(atomic(Version) ->
		true
		;
		throw(type_error(atomic, Version))).

'$lgt_tr_entity_info_key_value'(_, _).



% '$lgt_tr_pred_info_list'(@list, +atom, +integer)
%
% true if the argument is a list of valid key-value pairs

'$lgt_tr_pred_info_list'(List, _, _) :-
	var(List),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'(List, _, _) :-
	\+ '$lgt_proper_list'(List),
	throw(type_error(list, List)).

'$lgt_tr_pred_info_list'([], _, _).

'$lgt_tr_pred_info_list'([Head| _], _, _) :-
	var(Head),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'([Head| _], _, _) :-
	Head \= (_ is _),
	throw(type_error(key_value_info_pair, Head)).

'$lgt_tr_pred_info_list'([Key is Value| _], _, _) :-
	(var(Key); var(Value)),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'([Key is _| _], _, _) :-
	\+ atom(Key),
	throw(type_error(atom, Key)). 

'$lgt_tr_pred_info_list'([Key is Value| Tail], Functor, Arity) :-
	'$lgt_tr_pred_info_key_value'(Key, Value, Functor, Arity),
	'$lgt_tr_pred_info_list'(Tail, Functor, Arity).



% '$lgt_tr_pred_info_key_value'(+atom, @nonvar, +atom, +integer)
%
% true if the argument is a valid key-value pair

'$lgt_tr_pred_info_key_value'(allocation, Allocation, _, _) :-
	!,
	(atom(Allocation) ->
		('$lgt_member'(Allocation, [container, descendants, instances, classes, subclasses, any]) ->
			true
			;
			throw(domain_error(allocation, Allocation)))
		;
		throw(type_error(atom, Allocation))).

'$lgt_tr_pred_info_key_value'(arguments, Arguments, Functor, Arity) :-
	!,
	('$lgt_proper_list'(Arguments) ->
		(('$lgt_member'(Argument, Arguments), \+ '$lgt_valid_pred_argument'(Argument)) ->
			throw(type_error(argument, Argument))
			;
			((functor(Pred, Functor, Arity), Pred =.. [_| Arguments]) ->
			 	true
			 	;
			 	throw(length_error(arguments_list, Arguments))))
		;
		throw(type_error(list, Arguments))).

'$lgt_tr_pred_info_key_value'(argnames, Argnames, Functor, Arity) :-
	!,
	('$lgt_proper_list'(Argnames) ->
		(('$lgt_member'(Name, Argnames), \+ atom(Name)) ->
			throw(type_error(atom, Name))
			;
			((functor(Pred, Functor, Arity), Pred =.. [_| Argnames]) ->
			 	true
			 	;
			 	throw(length_error(argnames_list, Argnames))))
		;
		throw(type_error(list, Argnames))).

'$lgt_tr_pred_info_key_value'(comment, Comment, _, _) :-
	!,
	(atom(Comment) ->
		true
		;
		throw(type_error(atom, Comment))).

'$lgt_tr_pred_info_key_value'(exceptions, Exceptions, _, _) :-
	!,
	('$lgt_proper_list'(Exceptions) ->
		(('$lgt_member'(Exception, Exceptions), \+ '$lgt_valid_pred_exception'(Exception)) ->
			throw(type_error(exception, Exception))
			;
			true)
		;
		throw(type_error(list, Exceptions))).

'$lgt_tr_pred_info_key_value'(examples, Examples, Functor, Arity) :-
	!,
	('$lgt_proper_list'(Examples) ->
		(('$lgt_member'(Example, Examples), \+ '$lgt_valid_pred_call_example'(Example, Functor, Arity)) ->
			throw(type_error(example, Example))
			;
			true)
		;
		throw(type_error(list, Examples))).

'$lgt_tr_pred_info_key_value'(redefinition, Redefinition, _, _) :-
	!,
	(atom(Redefinition) ->
		('$lgt_member'(Redefinition, [never, free, specialize, call_super_first, call_super_last]) ->
			true
			;
			throw(domain_error(redefinition, Redefinition)))
		;
		throw(type_error(atom, Redefinition))).

'$lgt_tr_pred_info_key_value'(_, _, _, _).



% '$lgt_tr_clauses'(+list)

'$lgt_tr_clauses'([]).

'$lgt_tr_clauses'([Clause| Clauses]) :-
	'$lgt_tr_clause'(Clause),
	'$lgt_tr_clauses'(Clauses).



% '$lgt_tr_clause'(+clause)

'$lgt_tr_clause'(Clause) :-
	\+ '$lgt_pp_entity'(_, _, _, _, _),		% clause occurs before opening entity directive
	!,
	assertz('$lgt_pp_ppclause_'(Clause)).	% clause will copied unchanged to the generated Prolog file

'$lgt_tr_clause'(Clause) :-
	'$lgt_pp_entity'(Type, Entity, Prefix, _, _),
	((Type = object, compound(Entity)) ->	% if the entity is a parametric object we need
		'$lgt_ctx_this'(Ctx, Entity)		% "this" for inline compilation of parameter/2
		;
		true),
	'$lgt_ctx_prefix'(Ctx, Prefix),
	catch(
		'$lgt_tr_clause'(Clause, TClause, DClause, Ctx),
		Error,
		throw(error(Error, clause(Clause)))),
	('$lgt_compiler_flag'(debug, on) ->
		assertz('$lgt_pp_eclause_'(DClause))
		;
		assertz('$lgt_pp_eclause_'(TClause))),
	!.

'$lgt_tr_clause'(Clause) :-
	throw(error(unknown_error, clause(Clause))).



% '$lgt_tr_clause'(+clause, -clause, -clause, +term)

'$lgt_tr_clause'((Head:-_), _, _, _) :-
	\+ callable(Head),
	throw(type_error(callable, Head)).

'$lgt_tr_clause'((Head:-Body), TClause, (THead:-'$lgt_dbg_head'(Head, Ctx),DBody), Ctx) :-
	functor(Head, Functor, Arity),
	'$lgt_pp_dynamic_'(Functor, Arity),
	!,
	'$lgt_pred_metavars'(Head, Metavars),
	'$lgt_ctx_metavars'(Ctx, Metavars),
	'$lgt_tr_head'(Head, THead, Ctx),
	'$lgt_tr_body'(Body, TBody, DBody, Ctx),
	'$lgt_simplify_body'(TBody, SBody),
	TClause = (THead:-'$lgt_nop'(Body), SBody).

'$lgt_tr_clause'((Head:-Body), TClause, (THead:-'$lgt_dbg_head'(Head, Ctx),DBody), Ctx) :-
	!,
	'$lgt_pred_metavars'(Head, Metavars),
	'$lgt_ctx_metavars'(Ctx, Metavars),
	'$lgt_tr_head'(Head, THead, Ctx),
	'$lgt_tr_body'(Body, TBody, DBody, Ctx),
	'$lgt_simplify_body'(TBody, SBody),
	(SBody == true ->
		TClause = THead
		;
		TClause = (THead:-SBody)).

'$lgt_tr_clause'(Fact, _, _, _) :-
	\+ callable(Fact),
	throw(type_error(callable, Fact)).

'$lgt_tr_clause'(Fact, TFact, (TFact:-'$lgt_dbg_fact'(Fact, Ctx)), Ctx) :-
	'$lgt_tr_head'(Fact, TFact, Ctx).



% '$lgt_tr_head'(+callable, -callable, +term)
%
% translates an entity clause head


% definition of dynamic predicates inside categories

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pp_category_'(_, _, _, _, _, _),
	functor(Head, Functor, Arity), 
	'$lgt_pp_dynamic_'(Functor, Arity),
	throw(permission_error(define, dynamic_predicate, Functor/Arity)).


% redefinition of Logtalk message sending and external call control constructs

'$lgt_tr_head'(Term1::Term2, _, _) :-
	throw(permission_error(modify, control_construct, Term1::Term2)).

'$lgt_tr_head'(::Term, _, _) :-
	throw(permission_error(modify, control_construct, ::Term)).

'$lgt_tr_head'(^^Term, _, _) :-
	throw(permission_error(modify, control_construct, ^^Term)).

'$lgt_tr_head'({Term}, _, _) :-
	throw(permission_error(modify, control_construct, {Term})).


% redefinition of Logtalk built-in methods

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_built_in_method'(Head, _),
	functor(Head, Functor, Arity), 
	throw(permission_error(modify, built_in_method, Functor/Arity)).


% conflict with a predicate specified in a uses/2 directive

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pp_uses_'(_, Head),
	functor(Head, Functor, Arity),
	throw(permission_error(modify, uses_object_predicate, Functor/Arity)).


% redefinition of Logtalk built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_lgt_built_in'(Head),
	'$lgt_compiler_flag'(lgtredef, warning),
	'$lgt_compiler_flag'(report, on),
	\+ '$lgt_pp_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	'$lgt_inc_compile_warnings_counter',
	nl, write('  WARNING!  redefining a Logtalk built-in predicate: '),
	writeq(Functor/Arity),
	fail.


% redefinition of Prolog built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pl_built_in'(Head),
	'$lgt_compiler_flag'(plredef, warning),
	'$lgt_compiler_flag'(report, on),
	\+ '$lgt_pp_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	'$lgt_inc_compile_warnings_counter',
	nl, write('  WARNING!  redefining a Prolog built-in predicate: '),
	writeq(Functor/Arity),
	fail.


% translate the head of a clause of a user defined predicate

'$lgt_tr_head'(Head, THead, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_ctx_prefix'(Ctx, EPrefix),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	(('$lgt_pp_dynamic_'(Functor, Arity),
	  \+ '$lgt_pp_public_'(Functor, Arity),
	  \+ '$lgt_pp_protected_'(Functor, Arity),
	  \+ '$lgt_pp_private_'(Functor, Arity)) ->
		'$lgt_add_ddef_clause'(Functor, Arity, PPrefix, Ctx)
		;
		'$lgt_add_def_clause'(Functor, Arity, PPrefix, Ctx)),
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	'$lgt_append'(Args, [Sender, This, Self], Args2),
	THead =.. [PPrefix| Args2].



% '$lgt_tr_body'(+callable, -callable, -callable, +term)
%
% translates an entity clause body


% meta-calls

'$lgt_tr_body'(Pred, TPred, '$lgt_dbg_goal'(Pred, TPred, Ctx), Ctx) :-
	var(Pred),
	!,
	'$lgt_ctx_metavars'(Ctx, Metavars),
	('$lgt_member_var'(Pred, Metavars) ->
		'$lgt_ctx_sender'(Ctx, Sender),
		TPred = '$lgt_metacall_in_object'(Sender, Pred, Sender)
		;
		'$lgt_ctx_this'(Ctx, This),
		TPred = '$lgt_metacall_in_object'(This, Pred, This)).


% pre-processor bypass (call of external code)

'$lgt_tr_body'({Pred}, _, _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_body'({Pred}, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

'$lgt_tr_body'({Pred}, Pred, '$lgt_dbg_goal'({Pred}, Pred, Ctx), Ctx) :-
	!.


% bagof/3 and setof/3 existential quantifiers

'$lgt_tr_body'(Var^Pred, Var^TPred, Var^DPred, Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).


% control constructs

'$lgt_tr_body'((Pred1, Pred2), (TPred1, TPred2), (DPred1, DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'((Pred1; Pred2), (TPred1; TPred2), (DPred1; DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'((Pred1 -> Pred2), (TPred1 -> TPred2), (DPred1 -> DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'(\+ Pred, \+ TPred, \+ DPred, Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).

'$lgt_tr_body'(!, !, ('$lgt_dbg_goal'(!, true, Ctx), !), Ctx) :-
	!.

'$lgt_tr_body'(true, true, '$lgt_dbg_goal'(true, true, Ctx), Ctx) :-
	!.

'$lgt_tr_body'(fail, fail, '$lgt_dbg_goal'(fail, fail, Ctx), Ctx) :-
	!.

'$lgt_tr_body'(repeat, repeat, '$lgt_dbg_goal'(repeat, repeat, Ctx), Ctx) :-
	!.

'$lgt_tr_body'(call(Pred), TPred, DPred, Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).

'$lgt_tr_body'(once(Pred), once(TPred), once(DPred), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).

'$lgt_tr_body'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), '$lgt_dbg_goal'(catch(Goal, Catcher, Recovery), catch(DGoal, Catcher, DRecovery)), Ctx) :-
	!,
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	'$lgt_tr_body'(Recovery, TRecovery, DRecovery, Ctx).

'$lgt_tr_body'(throw(Error), throw(Error), '$lgt_dbg_goal'(throw(Error), throw(Error), Ctx), Ctx) :-
	!.


% built-in metapredicates

'$lgt_tr_body'(bagof(Term, Pred, List), bagof(Term, TPred, List), '$lgt_dbg_goal'(bagof(Term, Pred, List), bagof(Term, DPred, List), Ctx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).

'$lgt_tr_body'(findall(Term, Pred, List), findall(Term, TPred, List), '$lgt_dbg_goal'(findall(Term, Pred, List), findall(Term, DPred, List), Ctx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).

'$lgt_tr_body'(forall(Gen, Test), forall(TGen, TTest), '$lgt_dbg_goal'(forall(Gen, Test), forall(DGen, DTest), Ctx), Ctx) :-
	!,
	'$lgt_tr_body'(Gen, TGen, DGen, Ctx),
	'$lgt_tr_body'(Test, TTest, DTest, Ctx).

'$lgt_tr_body'(setof(Term, Pred, List), setof(Term, TPred, List), '$lgt_dbg_goal'(setof(Term, Pred, List), setof(Term, DPred, List), Ctx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).


% message sending

'$lgt_tr_body'(Obj::Pred, TPred, '$lgt_dbg_goal'(Obj::Pred, TPred, Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This),
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_body'(::Pred, TPred, '$lgt_dbg_goal'(::Pred, TPred, Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This),
	'$lgt_ctx_self'(Ctx, Self),
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_body'(^^Pred, TPred, '$lgt_dbg_goal'(^^Pred, TPred, Ctx), Ctx) :-
	!,
	'$lgt_tr_super_sending'(Pred, TPred, Ctx).


% "reflection" built-in predicates

'$lgt_tr_body'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, _), '$lgt_dbg_goal'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).

'$lgt_tr_body'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, _), '$lgt_dbg_goal'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, _), Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).


% database handling built-in predicates

'$lgt_tr_body'(abolish(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This),
	('$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TCond = '$lgt_abolish'(This, Pred, This, _)
		;
		'$lgt_compiler_db_pred_ind_chk'(Pred),
		TCond = '$lgt_abolish_chk'(This, Pred, This, _)),
	DCond = '$lgt_dbg_goal'(abolish(Pred), TCond, Ctx).

'$lgt_tr_body'(asserta(Pred), TCond, DCond, Ctx) :-
	!,
	('$lgt_optimizable_local_db_call'(Pred, Ctx, TPred) ->
		TCond = asserta(TPred)
		;
		'$lgt_ctx_this'(Ctx, This),
		('$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_asserta'(This, Pred, This, _)
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TCond = '$lgt_asserta_rule_chk'(This, Pred, This, _)
				;
				TCond = '$lgt_asserta_fact_chk'(This, Pred, This, _)))),
	DCond = '$lgt_dbg_goal'(asserta(Pred), TCond, Ctx).

'$lgt_tr_body'(assertz(Pred), TCond, DCond, Ctx) :-
	!,
	('$lgt_optimizable_local_db_call'(Pred, Ctx, TPred) ->
		TCond = assertz(TPred)
		;
		'$lgt_ctx_this'(Ctx, This),
		('$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_assertz'(This, Pred, This, _)
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TCond = '$lgt_assertz_rule_chk'(This, Pred, This, _)
				;
				TCond = '$lgt_assertz_fact_chk'(This, Pred, This, _)))),
	DCond = '$lgt_dbg_goal'(assertz(Pred), TCond, Ctx).

'$lgt_tr_body'(clause(Head, Body), TCond, DCond, Ctx) :-
	!,
	('$lgt_optimizable_local_db_call'(Head, Ctx, THead) ->
		TCond = (clause(THead, TBody), (TBody = ('$lgt_nop'(Body), _) -> true; Body = TBody))
		;
		'$lgt_ctx_this'(Ctx, This),
		('$lgt_runtime_db_clause_chk'((Head :- Body)) ->
			TCond = '$lgt_clause'(This, Head, Body, This, _)
			;
			'$lgt_compiler_db_clause_chk'((Head :- Body)),
			TCond = '$lgt_clause_chk'(This, Head, Body, This, _))),
	DCond = '$lgt_dbg_goal'(clause(Head, Body), TCond, Ctx).

'$lgt_tr_body'(retract(Pred), TCond, DCond, Ctx) :-
	!,
	('$lgt_optimizable_local_db_call'(Pred, Ctx, TPred) ->
		TCond = retract(TPred)
		;
		'$lgt_ctx_this'(Ctx, This),
		('$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_retract'(This, Pred, This, _)
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TCond = '$lgt_retract_rule_chk'(This, Pred, This, _)
				;
				TCond = '$lgt_retract_fact_chk'(This, Pred, This, _)))),
	DCond = '$lgt_dbg_goal'(retract(Pred), TCond, Ctx).

'$lgt_tr_body'(retractall(Pred), TCond, DCond, Ctx) :-
	!,
	('$lgt_optimizable_local_db_call'(Pred, Ctx, TPred) ->
		TCond = retractall(TPred)
		;
		'$lgt_ctx_this'(Ctx, This),
		('$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_retractall'(This, Pred, This, _)
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			TCond = '$lgt_retractall_chk'(This, Pred, This, _))),
	DCond = '$lgt_dbg_goal'(retractall(Pred), TCond, Ctx).


% DCG predicates

'$lgt_tr_body'(expand_term(Term, Clause), '$lgt_expand_term'(This, Term, Clause, This, _), '$lgt_dbg_goal'(expand_term(Term, Clause), '$lgt_expand_term'(This, Term, Clause, This, _), Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).

'$lgt_tr_body'(phrase(Ruleset, Input), '$lgt_phrase'(This, Ruleset, Input, This, _), '$lgt_dbg_goal'(phrase(Ruleset, Input), '$lgt_phrase'(This, Ruleset, Input, This, _), Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).

'$lgt_tr_body'(phrase(Ruleset, Input, Rest), '$lgt_phrase'(This, Ruleset, Input, Rest, This, _), '$lgt_dbg_goal'(phrase(Ruleset, Input, Rest), '$lgt_phrase'(This, Ruleset, Input, Rest, This, _), Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).


% inline methods (translated to a single unification with the corresponding context argument)

'$lgt_tr_body'(sender(Sender), true, '$lgt_dbg_goal'(sender(Temp), Sender=Temp, Ctx), Ctx) :-
	!,
	'$lgt_ctx_sender'(Ctx, Sender).

'$lgt_tr_body'(this(This), true, '$lgt_dbg_goal'(this(Temp), This=Temp, Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This).

'$lgt_tr_body'(self(Self), true, '$lgt_dbg_goal'(self(Temp), Self=Temp, Ctx), Ctx) :-
	!,
	'$lgt_ctx_self'(Ctx, Self).

'$lgt_tr_body'(parameter(Arg, Value), TPred, '$lgt_dbg_goal'(parameter(Arg, Temp), DPred, Ctx), Ctx) :-
	!,
	'$lgt_ctx_this'(Ctx, This),
	(var(This) ->
		TPred = arg(Arg, This, Value),	% when using parameter/2 in categories
		DPred = (TPred, Temp=Value)
		;
		arg(Arg, This, Value),
		TPred = true,
		DPred = (Temp=Value)).


% term input predicates that need to be operator aware

'$lgt_tr_body'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), '$lgt_dbg_goal'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), '$lgt_dbg_goal'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), '$lgt_dbg_goal'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read(Term), '$lgt_iso_read'(Term, Ops), '$lgt_dbg_goal'(read(Term), '$lgt_iso_read'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.


% term output predicates that need to be operator aware

'$lgt_tr_body'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), '$lgt_dbg_goal'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), Ctx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), '$lgt_dbg_goal'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), Ctx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), '$lgt_dbg_goal'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write(Term), '$lgt_iso_write'(Term, Ops), '$lgt_dbg_goal'(write(Term), '$lgt_iso_write'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), '$lgt_dbg_goal'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), '$lgt_dbg_goal'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_pp_entity_op_'(Pr, Spec, Op), Ops),
	!.


% predicates specified in uses/2 directives

'$lgt_tr_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_pp_uses_'(Obj, Pred),
	!,
	'$lgt_tr_body'(Obj::Pred, TPred, DPred, Ctx).


% Logtalk and Prolog built-in predicates

'$lgt_tr_body'(Pred, _, _, _) :-
	'$lgt_pl_built_in'(Pred),
	\+ '$lgt_lgt_built_in'(Pred),
	\+ '$lgt_iso_spec_pred'(Pred),
	functor(Pred, Functor, Arity),
	assertz('$lgt_non_portable_call_'(Functor, Arity)),
	fail.

'$lgt_tr_body'(Pred, TPred, '$lgt_dbg_goal'(Pred, TPred, Ctx), Ctx) :-
	'$lgt_pl_built_in'(Pred),
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity), 
	'$lgt_pl_metapredicate'(Meta),
	!,
	Pred =.. [_| Args],
	Meta =.. [_| MArgs],
	'$lgt_tr_margs'(Args, MArgs, Ctx, TArgs),
	TPred =.. [Functor| TArgs].

'$lgt_tr_body'(Pred, '$lgt_call_built_in'(Pred, Ctx), '$lgt_dbg_goal'(Pred, '$lgt_call_built_in'(Pred, Ctx), Ctx), Ctx) :-
	'$lgt_built_in'(Pred),
	!.


% invalid goal

'$lgt_tr_body'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% goal is a call to a user predicate

'$lgt_tr_body'(Cond, TCond, '$lgt_dbg_goal'(Cond, TCond, Ctx), Ctx) :-
	Cond =.. [Functor| Args],
	functor(Cond, Functor, Arity),
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, EPrefix, _),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	'$lgt_append'(Args, [Sender, This, Self], Args2),
	TCond =.. [PPrefix| Args2],
	assertz('$lgt_pp_calls_pred_'(Functor, Arity)).



% '$lgt_tr_margs'(@list, @list, +term, -list)
%
% translates the meta-arguments contained in the list of 
% arguments of a call to a metapredicate

'$lgt_tr_margs'([], [], _, []).

'$lgt_tr_margs'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs]) :-
	'$lgt_tr_marg'(MArg, Arg, Ctx, TArg),
	'$lgt_tr_margs'(Args, MArgs, Ctx, TArgs).


'$lgt_tr_marg'(*, Arg, _, Arg).

'$lgt_tr_marg'(::, Arg, Ctx, TArg) :-
	'$lgt_tr_body'(Arg, TArg, _, Ctx).



% '$lgt_optimizable_local_db_call'(@term, @nonvar, -callable)
%
% checks if a call to a database built-in method can be optimized by direct
% translation to a call to the corresponding Prolog built-in predicate

'$lgt_optimizable_local_db_call'(Pred, Ctx, TPred) :-
	callable(Pred),
	functor(Pred, Functor, Arity),
	Functor \= (:-),					% only facts allowed
	'$lgt_pp_dynamic_'(Functor, Arity),
	once((								% a scope directive must be present
		'$lgt_pp_public_'(Functor, Arity);
		'$lgt_pp_protected_'(Functor, Arity);
		'$lgt_pp_private_'(Functor, Arity))),
	Pred =.. [Functor| Args],
	'$lgt_ctx_ctx'(Ctx, _, _, _, EPrefix, _),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	'$lgt_append'(Args, [_, _, _], Args2),
	TPred =.. [PPrefix| Args2].



% '$lgt_runtime_db_clause_chk'(@term)
%
% true if the argument forces runtime validity check

'$lgt_runtime_db_clause_chk'(Pred) :-
	var(Pred),
	!.

'$lgt_runtime_db_clause_chk'((Head :- _)) :-
	var(Head),
	!.

'$lgt_runtime_db_clause_chk'((_ :- Body)) :-
	var(Body).



% '$lgt_compiler_db_clause_chk'(@nonvar)
%
% throws an error if the argument is invalid

'$lgt_compiler_db_clause_chk'((Head :- _)) :-
	\+ callable(Head),
	throw(type_error(callable, Head)).

'$lgt_compiler_db_clause_chk'((_ :- Body)) :-
	\+ callable(Body),
	throw(type_error(callable, Body)).

'$lgt_compiler_db_clause_chk'(Clause) :-
	\+ callable(Clause),
	throw(type_error(callable, Clause)).

'$lgt_compiler_db_clause_chk'(_).




% '$lgt_runtime_db_pred_ind_chk'(@term)
%
% true if the argument forces runtime validity check

'$lgt_runtime_db_pred_ind_chk'(Pred) :-
	var(Pred),
	!.

'$lgt_runtime_db_pred_ind_chk'(Functor/_) :-
	var(Functor),
	!.

'$lgt_runtime_db_pred_ind_chk'(_/Arity) :-
	var(Arity).



% '$lgt_compiler_db_pred_ind_chk'(@nonvar)
%
% throws an error if the argument is invalid

'$lgt_compiler_db_pred_ind_chk'(Term) :-
	Term \= (_/_),
	throw(type_error(predicate_indicator, Term)).

'$lgt_compiler_db_pred_ind_chk'(Functor/_) :-
	\+ atom(Functor),
	throw(type_error(atom, Functor)).

'$lgt_compiler_db_pred_ind_chk'(_/Arity) :-
	\+ integer(Arity),
	throw(type_error(integer, Arity)).

'$lgt_compiler_db_pred_ind_chk'(_/Arity) :-
	integer(Arity),
	Arity =< 0,
	throw(domain_error(not_less_than_zero, Arity)).

'$lgt_compiler_db_pred_ind_chk'(_).



% '$lgt_tr_msg'(@term, @object_identifier, -nonvar, @object_identifier)
%
% translates the sending of a message to an object


'$lgt_tr_msg'(Pred, Obj, TPred, This) :-
	nonvar(Obj),
	((Obj = (_, _); Obj = (_; _)) ->
		!,
		'$lgt_tr_msg_broadcasting'(Obj, Pred, TPred, This)	% message broadcasting
		;
		(\+ callable(Obj) ->
			throw(type_error(object_identifier, Obj))		% invalid object identifier
			;
			This \= user,									% not runtime message translation
			assertz('$lgt_pp_referenced_object_'(Obj)),		% remember object receiving message
			fail)).


% translation performed at runtime

'$lgt_tr_msg'(Pred, Obj, TPred, This) :-
	var(Pred),
	!,
	('$lgt_compiler_flag'(events, on) ->
		TPred = '$lgt_send_to_object'(Obj, Pred, This)
		;
		TPred = '$lgt_send_to_object_ne'(Obj, Pred, This)).


% invalid message

'$lgt_tr_msg'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% control constructs

'$lgt_tr_msg'((Pred1, Pred2), Obj, (TPred1, TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'((Pred1; Pred2), Obj, (TPred1; TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'((Pred1 -> Pred2), Obj, (TPred1 -> TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'(\+ Pred, Obj, \+ TPred, This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_msg'(!, Obj, ('$lgt_obj_exists'(Obj, !, This), !), This) :-
	!.

'$lgt_tr_msg'(true, Obj, ('$lgt_obj_exists'(Obj, true, This), true), This) :-
	!.

'$lgt_tr_msg'(fail, Obj, ('$lgt_obj_exists'(Obj, fail, This), fail), This) :-
	!.

'$lgt_tr_msg'(repeat, Obj, ('$lgt_obj_exists'(Obj, repeat, This), repeat), This) :-
	!.

'$lgt_tr_msg'(call(Pred), Obj, TPred, This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_msg'(once(Pred), Obj, once(TPred), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_msg'(catch(Goal, Catcher, Recovery), Obj, catch(TGoal, Catcher, TRecovery), This) :-
	!,
	'$lgt_tr_msg'(Goal, Obj, TGoal, This),
	'$lgt_tr_msg'(Recovery, Obj, TRecovery, This).

'$lgt_tr_msg'(throw(Error), Obj, ('$lgt_obj_exists'(Obj, throw(Error), This), throw(Error)), This) :-
	!.


% built-in metapredicates

'$lgt_tr_msg'(bagof(Term, Pred, List), Obj, bagof(Term, TPred, List), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_msg'(findall(Term, Pred, List), Obj, findall(Term, TPred, List), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_msg'(forall(Gen, Test), Obj, forall(TGen, TTest), This) :-
	!,
	'$lgt_tr_msg'(Gen, Obj, TGen, This),
	'$lgt_tr_msg'(Test, Obj, TTest, This).

'$lgt_tr_msg'(setof(Term, Pred, List), Obj, setof(Term, TPred, List), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj, TPred, This).


% "reflection" built-in predicates

'$lgt_tr_msg'(current_predicate(Pred), Obj, '$lgt_current_predicate'(Obj, Pred, This, p(p(p))), This) :-
	!.

'$lgt_tr_msg'(predicate_property(Pred, Prop), Obj, '$lgt_predicate_property'(Obj, Pred, Prop, This, p(p(p))), This) :-
	!.


% database handling built-in predicates

'$lgt_tr_msg'(abolish(Pred), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TPred = '$lgt_abolish'(Obj, Pred, This, p(p(p)))
		;
		'$lgt_compiler_db_pred_ind_chk'(Pred),
		TPred = '$lgt_abolish_chk'(Obj, Pred, This, p(p(p)))).

'$lgt_tr_msg'(asserta(Pred), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
			TPred = '$lgt_asserta'(Obj, Pred, This, p(p(p)))
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TPred = '$lgt_asserta_rule_chk'(Obj, Pred, This, p(p(p)))
				;
				TPred = '$lgt_asserta_fact_chk'(Obj, Pred, This, p(p(p))))).

'$lgt_tr_msg'(assertz(Pred), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
			TPred = '$lgt_assertz'(Obj, Pred, This, p(p(p)))
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TPred = '$lgt_assertz_rule_chk'(Obj, Pred, This, p(p(p)))
				;
				TPred = '$lgt_assertz_fact_chk'(Obj, Pred, This, p(p(p))))).

'$lgt_tr_msg'(clause(Head, Body), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_clause_chk'((Head :- Body)) ->
		TPred = '$lgt_clause'(Obj, Head, Body, This, p(p(p)))
		;
		'$lgt_compiler_db_clause_chk'((Head :- Body)),
		TPred = '$lgt_clause_chk'(Obj, Head, Body, This, p(p(p)))).

'$lgt_tr_msg'(retract(Pred), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
			TPred = '$lgt_retract'(Obj, Pred, This, p(p(p)))
			;
			'$lgt_compiler_db_clause_chk'(Pred),
			(Pred = (_ :- _) ->
				TPred = '$lgt_retract_rule_chk'(Obj, Pred, This, p(p(p)))
				;
				TPred = '$lgt_retract_fact_chk'(Obj, Pred, This, p(p(p))))).

'$lgt_tr_msg'(retractall(Pred), Obj, TPred, This) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retractall'(Obj, Pred, This, p(p(p)))
		;
		'$lgt_compiler_db_clause_chk'(Pred),
		TPred = '$lgt_retractall_chk'(Obj, Pred, This, p(p(p)))).


% DCG predicates

'$lgt_tr_msg'(expand_term(Term, Clause), Obj, '$lgt_expand_term'(Obj, Term, Clause, This, p(p(p))), This) :-
	!.

'$lgt_tr_msg'(phrase(Ruleset, List), Obj, '$lgt_phrase'(Obj, Ruleset, List, This, p(p(p))), This) :-
	!.

'$lgt_tr_msg'(phrase(Ruleset, List, Rest), Obj, '$lgt_phrase'(Obj, Ruleset, List, Rest, This, p(p(p))), This) :-
	!.


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_msg'(Pred, Obj, TPred, This) :-
	var(Obj) ->
		('$lgt_compiler_flag'(events, on) ->
			TPred = '$lgt_send_to_object'(Obj, Pred, This)
			;
			TPred = '$lgt_send_to_object_ne'(Obj, Pred, This))
		;
		('$lgt_compiler_flag'(events, on) ->
			TPred = '$lgt_send_to_object_nv'(Obj, Pred, This)
			;
			TPred = '$lgt_send_to_object_ne_nv'(Obj, Pred, This)).



% '$lgt_tr_self_msg'(@term, -nonvar, @object_identifier, @object_identifier)
%
% translates the sending of a message to self


% translation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self'(Self, Pred, This), This, Self) :-
	var(Pred),
	!.


% invalid message

'$lgt_tr_self_msg'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% control constructs

'$lgt_tr_self_msg'((Pred1, Pred2), (TPred1, TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'(((Pred1; Pred2)), (TPred1; TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'((Pred1 -> Pred2), (TPred1 -> TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'(\+ Pred, \+ TPred, This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_self_msg'(!, !, _, _) :-
	!.

'$lgt_tr_self_msg'(true, true, _, _) :-
	!.

'$lgt_tr_self_msg'(fail, fail, _, _) :-
	!.

'$lgt_tr_self_msg'(repeat, repeat, _, _) :-
	!.

'$lgt_tr_self_msg'(call(Pred), TPred, This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_self_msg'(once(Pred), once(TPred), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_self_msg'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Goal, TGoal, This, Self),
	'$lgt_tr_self_msg'(Recovery, TRecovery, This, Self).

'$lgt_tr_self_msg'(throw(Error), throw(Error), _, _) :-
	!.


% built-in metapredicates

'$lgt_tr_self_msg'(bagof(Term, Pred, List), bagof(Term, TPred, List), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_self_msg'(findall(Term, Pred, List), findall(Term, TPred, List), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_self_msg'(forall(Gen, Test), forall(TGen, TTest), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Gen, TGen, This, Self),
	'$lgt_tr_self_msg'(Test, TTest, This, Self).

'$lgt_tr_self_msg'(setof(Term, Pred, List), setof(Term, TPred, List), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).


% "reflection" built-in predicates

'$lgt_tr_self_msg'(current_predicate(Pred), '$lgt_current_predicate'(Self, Pred, This, p(_)), This, Self) :-
	!.

'$lgt_tr_self_msg'(predicate_property(Pred, Prop), '$lgt_predicate_property'(Self, Pred, Prop, This, p(_)), This, Self) :-
	!.


% database handling built-in predicates

'$lgt_tr_self_msg'(abolish(Pred), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TPred = '$lgt_abolish'(Self, Pred, This, p(_))
		;
		'$lgt_compiler_db_pred_ind_chk'(Pred),
		TPred = '$lgt_abolish_chk'(Self, Pred, This, p(_))).

'$lgt_tr_self_msg'(asserta(Pred), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_asserta'(Self, Pred, This, p(_))
		;
		'$lgt_compiler_db_clause_chk'(Pred),
		(Pred = (_ :- _) ->
			TPred = '$lgt_asserta_rule_chk'(Self, Pred, This, p(_))
			;
			TPred = '$lgt_asserta_fact_chk'(Self, Pred, This, p(_)))).

'$lgt_tr_self_msg'(assertz(Pred), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_assertz'(Self, Pred, This, p(_))
		;
		'$lgt_compiler_db_clause_chk'(Pred),
		(Pred = (_ :- _) ->
			TPred = '$lgt_assertz_rule_chk'(Self, Pred, This, p(_))
			;
			TPred = '$lgt_assertz_fact_chk'(Self, Pred, This, p(_)))).

'$lgt_tr_self_msg'(clause(Head, Body), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_clause_chk'((Head :- Body)) ->
		TPred = '$lgt_clause'(Self, Head, Body, This, p(_))
		;
		'$lgt_compiler_db_clause_chk'((Head :- Body)),
		TPred = '$lgt_clause_chk'(Self, Head, Body, This, p(_))).

'$lgt_tr_self_msg'(retract(Pred), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retract'(Self, Pred, This, p(_))
		;
		'$lgt_compiler_db_clause_chk'(Pred),
		(Pred = (_ :- _) ->
			TPred = '$lgt_retract_rule_chk'(Self, Pred, This, p(_))
			;
			TPred = '$lgt_retract_fact_chk'(Self, Pred, This, p(_)))).

'$lgt_tr_self_msg'(retractall(Pred), TPred, This, Self) :-
	!,
	('$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retractall'(Self, Pred, This, p(_))
		;
		'$lgt_compiler_db_clause_chk'(Pred),
		TPred = '$lgt_retractall_chk'(Self, Pred, This, p(_))).



% DCG predicates

'$lgt_tr_self_msg'(expand_term(Term, Clause), '$lgt_expand_term'(Self, Term, Clause, This, p(_)), This, Self) :-
	!.

'$lgt_tr_self_msg'(phrase(Ruleset, List), '$lgt_phrase'(Self, Ruleset, List, This, p(_)), This, Self) :-
	!.

'$lgt_tr_self_msg'(phrase(Ruleset, List, Rest), '$lgt_phrase'(Self, Ruleset, List, Rest, This, p(_)), This, Self) :-
	!.


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self_nv'(Self, Pred, This), This, Self) :-
	!.



% message broadcasting

'$lgt_tr_msg_broadcasting'((Obj1, Obj2), Pred, (TP1, TP2), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj1, TP1, This),
	'$lgt_tr_msg'(Pred, Obj2, TP2, This).

'$lgt_tr_msg_broadcasting'((Obj1; Obj2), Pred, (TP1; TP2), This) :-
	!,
	'$lgt_tr_msg'(Pred, Obj1, TP1, This),
	'$lgt_tr_msg'(Pred, Obj2, TP2, This).



% '$lgt_tr_super_sending'(@term, -term, +term)
%
% translates calling of redefined predicates (super calls)


% invalid goal (not callable)

'$lgt_tr_super_sending'(Pred, _, _) :-
	nonvar(Pred),
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% translation performed at runtime

'$lgt_tr_super_sending'(Pred, TPred, Ctx) :-
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	(var(Pred) ->
		TPred = '$lgt_send_to_super'(Self, Pred, This, Sender)
		;
		TPred = '$lgt_send_to_super_nv'(Self, Pred, This, Sender)).


% '$lgt_pred_metavars'(+callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

'$lgt_pred_metavars'(Pred, Metavars) :-
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	('$lgt_pp_metapredicate_'(Meta) ->
		Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		'$lgt_extract_metavars'(Args, MArgs, Metavars)
		;
		Metavars = []).



% '$lgt_pred_metavars'(+callable, +callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

'$lgt_pred_metavars'(Pred, Meta, Metavars) :-
	Meta = no ->
		Metavars = []
		;
		Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		'$lgt_extract_metavars'(Args, MArgs, Metavars).



% '$lgt_extract_metavars'(+list, +list, -list)

'$lgt_extract_metavars'([], [], []).

'$lgt_extract_metavars'([Var| Args], [MArg| MArgs], [Var| Metavars]) :-
	var(Var),
	MArg = (::),
	!,
	'$lgt_extract_metavars'(Args, MArgs, Metavars).

'$lgt_extract_metavars'([_| Args], [_| MArgs], Metavars) :-
	'$lgt_extract_metavars'(Args, MArgs, Metavars).



% '$lgt_iso_read_term'(@stream, ?term, +read_options_list, @list)
%
% wraps read_term/3 call with the necessary operator settings

'$lgt_iso_read_term'(Stream, Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read_term'(?term, +read_options_list, @list)
%
% wraps read_term/2 call with the necessary operator settings

'$lgt_iso_read_term'(Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read_term(Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read'(@stream, ?term, @list)
%
% wraps read/2 call with the necessary operator settings

'$lgt_iso_read'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read'(?term, @list)
%
% wraps read/1 call with the necessary operator settings

'$lgt_iso_read'(Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write_term'(@stream_or_alias, @term, @write_options_list, @list)
%
% wraps write_term/3 call with the necessary operator settings

'$lgt_iso_write_term'(Stream, Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write_term'(@term, @write_options_list, @list)
%
% wraps write_term/2 call with the necessary operator settings

'$lgt_iso_write_term'(Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write_term(Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write'(@stream_or_alias, @term, @list)
%
% wraps write/2 call with the necessary operator settings

'$lgt_iso_write'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write'(@term, @list)
%
% wraps write/1 call with the necessary operator settings

'$lgt_iso_write'(Term, Ops):-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_writeq'(@stream_or_alias, @term, @list)
%
% wraps writeq/2 call with the necessary operator settings

'$lgt_iso_writeq'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 writeq(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_writeq'(@term, @list)
%
% wraps writeq/1 call with the necessary operator settings

'$lgt_iso_writeq'(Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 writeq(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_save_operators'(@list, -list)
%
% save currently defined operators that might be
% redefined when a list of operators is added

'$lgt_save_operators'([], []).

'$lgt_save_operators'([op(_, Spec, Op)| Ops], Saved) :-
	((current_op(Pr, SCSpec, Op), '$lgt_same_op_class'(Spec, SCSpec)) ->
		Saved = [op(Pr, SCSpec, Op)| Saved2]
		;
		Saved = Saved2),
	'$lgt_save_operators'(Ops, Saved2).



% '$lgt_add_operators'(@list)
%
% adds operators to the global operator table

'$lgt_add_operators'([]).

'$lgt_add_operators'([op(Pr, Spec, Op)| Ops]) :-
	op(Pr, Spec, Op),
	'$lgt_add_operators'(Ops).



% '$lgt_remove_operators'(@list)
%
% remove operators from the global operator table

'$lgt_remove_operators'([]).

'$lgt_remove_operators'([op(_, Spec, Op)| Ops]) :-
	op(0, Spec, Op),
	'$lgt_remove_operators'(Ops).



% '$lgt_iso_read_error_handler'(@list, @list, @nonvar)
%
% restores operator table to the its state before the call
% to one of the '$lgt_iso_read...' raised an error

'$lgt_iso_read_error_handler'(Ops, Saved, Error) :-
	'$lgt_remove_operators'(Ops),
	'$lgt_add_operators'(Saved),
	throw(Error).



% '$lgt_simplify_body'(+callable, -callable)
%
% remove redundant calls to true/0 from a translated clause body

'$lgt_simplify_body'((A;B), (SA;SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((A->B), (SA->SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((true, B), SB) :-
	!,
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((B, true), SB) :-
	!,
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((A, B), (SA, SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'(B, B).



% '$lgt_tr_object_id'(+object_identifier, +atom)
%
% from the object identifier construct the set of 
% functor prefixes used in the compiled code clauses

'$lgt_tr_object_id'(Obj, Mode) :-
	assertz('$lgt_pp_referenced_object_'(Obj)),
	'$lgt_construct_object_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm),
	assertz('$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Mode)).



% '$lgt_tr_category_id'(+category_identifier, +atom)
%
% from the category identifier construct the set of 
% functor prefixes used in the compiled code clauses

'$lgt_tr_category_id'(Ctg, Mode) :-
	assertz('$lgt_pp_referenced_category_'(Ctg)),
	'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, Rnm),
	assertz('$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Mode)).



% '$lgt_tr_protocol_id'(+protocol_identifier, +atom)
%
% from the protocol identifier construct the set of  
% functor prefixes used in the compiled code clauses

'$lgt_tr_protocol_id'(Ptc, Mode) :-
	assertz('$lgt_pp_referenced_protocol_'(Ptc)),
	'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm),
	assertz('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Mode)).



% '$lgt_update_entity_comp_mode'
%
% updates entity compilation mode to "dynamic" (entities are static by default)

'$lgt_update_entity_comp_mode' :-
	retract('$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _)),
	assertz('$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, (dynamic))),
	!.

'$lgt_update_entity_comp_mode' :-
	retract('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, _)),
	assertz('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, (dynamic))),
	!.

'$lgt_update_entity_comp_mode' :-
	retract('$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, _)),
	assertz('$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, (dynamic))).



% '$lgt_tr_implements_protocol'(+list, +object_identifier)
% '$lgt_tr_implements_protocol'(+list, +category_identifier)
%
% translates an "implementents" relation between 
%  a category or an object and a list of protocols

'$lgt_tr_implements_protocol'([], _).

'$lgt_tr_implements_protocol'([Ref| Refs], ObjOrCtg) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_protocol_ref'(Ref, Ptc) ->
			assertz('$lgt_pp_referenced_protocol_'(Ptc)),
			assertz('$lgt_pp_rclause_'('$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope))),
			'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, _),
			assertz('$lgt_pp_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)),
			'$lgt_tr_implements_protocol'(Refs, ObjOrCtg)
			;
			throw(type_error(protocol_identifier, Ptc)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_imports_category'(+list, +object_identifier)
% '$lgt_tr_imports_category'(+list, +category_identifier)
%
% translates an "imports" relation between 
% an object/category and a list of categories 

'$lgt_tr_imports_category'([], _).

'$lgt_tr_imports_category'([Ref| Refs], ObjOrCtg) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_category_ref'(Ref, Ctg) ->
			assertz('$lgt_pp_referenced_category_'(Ctg)),
			assertz('$lgt_pp_rclause_'('$lgt_imports_category_'(ObjOrCtg, Ctg, Scope))),
			'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, _),
			assertz('$lgt_pp_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)),
			'$lgt_tr_imports_category'(Refs, ObjOrCtg)
			;
			throw(type_error(category_identifier, Ctg)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_instantiates_class'(+list, +object_identifier)
%
% translates an "instantiates" relation between 
% an instance and a list of classes

'$lgt_tr_instantiates_class'([], _).

'$lgt_tr_instantiates_class'([Ref| Refs], Obj) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_object_ref'(Ref, Class) ->
			assertz('$lgt_pp_referenced_object_'(Class)),
			assertz('$lgt_pp_rclause_'('$lgt_instantiates_class_'(Obj, Class, Scope))),
			'$lgt_construct_object_functors'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
			assertz('$lgt_pp_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_instantiates_class'(Refs, Obj)
			;
			throw(type_error(object_identifier, Class)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_specializes_class'(+list, +object_identifier)
%
% translates a "specializes" relation between 
% a class and a list of superclasses

'$lgt_tr_specializes_class'([], _).

'$lgt_tr_specializes_class'([Ref| Refs], Class) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_object_ref'(Ref, Superclass) ->
			assertz('$lgt_pp_referenced_object_'(Superclass)),
			assertz('$lgt_pp_rclause_'('$lgt_specializes_class_'(Class, Superclass, Scope))),
			'$lgt_construct_object_functors'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
			assertz('$lgt_pp_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_specializes_class'(Refs, Class)
			;
			throw(type_error(object_identifier, Superclass)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_extends_object'(+list, +object_identifier)
%
% translates an "extends" relation between 
% a prototype and a list of parents

'$lgt_tr_extends_object'([], _).

'$lgt_tr_extends_object'([Ref| Refs], Obj) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_object_ref'(Ref, Parent) ->
			assertz('$lgt_pp_referenced_object_'(Parent)),
			assertz('$lgt_pp_rclause_'('$lgt_extends_object_'(Obj, Parent, Scope))),
			'$lgt_construct_object_functors'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
			assertz('$lgt_pp_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_extends_object'(Refs, Obj)
			;
			throw(type_error(object_identifier, Parent)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_extends_protocol'(+list, +protocol_identifier)
%
% translates an "extends" relation between 
% a protocol and a list of protocols

'$lgt_tr_extends_protocol'([], _).

'$lgt_tr_extends_protocol'([Ref| Refs], Ptc1) :-
	'$lgt_valid_ref_scope'(Ref, Scope) ->
		('$lgt_valid_protocol_ref'(Ref, Ptc2) ->
			assertz('$lgt_pp_referenced_protocol_'(Ptc2)),
			assertz('$lgt_pp_rclause_'('$lgt_extends_protocol_'(Ptc1, Ptc2, Scope))),
			'$lgt_construct_protocol_functors'(Ptc2, Prefix, Dcl, _),
			assertz('$lgt_pp_extended_protocol_'(Ptc2, Prefix, Dcl, Scope)),
			'$lgt_tr_extends_protocol'(Refs, Ptc1)
			;
			throw(type_error(protocol_identifier, Ptc2)))
		;
		throw(type_error(scope, Ref)).



% '$lgt_report_problems'(+atom)
%
% reports any potential problem found while compiling an entity 

'$lgt_report_problems'(protocol) :-
	'$lgt_compiler_flag'(report, on) ->
		'$lgt_report_unknown_entities'
		;
		true.

'$lgt_report_problems'(object) :-
	'$lgt_compiler_flag'(report, on) ->
		'$lgt_report_misspelt_calls',
		'$lgt_report_non_portable_calls',
		'$lgt_report_unknown_entities'
		;
		true.

'$lgt_report_problems'(category) :-
	'$lgt_report_problems'(object).



% '$lgt_report_unknown_entities'
%
% reports any unknown referenced entities found while compiling an entity

'$lgt_report_unknown_entities' :-
	'$lgt_compiler_flag'(unknown, warning) ->
		'$lgt_report_unknown_objects',
		'$lgt_report_unknown_protocols',
		'$lgt_report_unknown_categories'
		;
		true.



% '$lgt_report_unknown_objects'
%
% report any unknown referenced objects found while compiling an entity

'$lgt_report_unknown_objects' :-
	setof(Obj, '$lgt_unknown_object'(Obj), Objs) ->
		'$lgt_inc_compile_warnings_counter',
		nl, write('  WARNING!  references to unknown objects:    '),
		'$lgt_writeq_list'(Objs)
		;
		true.


'$lgt_unknown_object'(Obj) :-
	'$lgt_pp_referenced_object_'(Obj),
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	\+ '$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_entity_init_'(Obj, _).



% '$lgt_report_unknown_protocols'
%
% report any unknown referenced protocols found while compiling an entity

'$lgt_report_unknown_protocols' :-
	setof(Ptc, '$lgt_unknown_protocol'(Ptc), Ptcs) ->
		'$lgt_inc_compile_warnings_counter',
		nl, write('  WARNING!  references to unknown protocols:  '),
		'$lgt_writeq_list'(Ptcs)
		;
		true.


'$lgt_unknown_protocol'(Ptc) :-
	'$lgt_pp_referenced_protocol_'(Ptc),
	\+ '$lgt_current_protocol_'(Ptc, _, _),
	\+ '$lgt_pp_protocol_'(Ptc, _, _, _, _),
	\+ '$lgt_pp_entity_init_'(Ptc, _).



% '$lgt_report_unknown_categories'
%
% report any unknown referenced categories found while compiling an entity

'$lgt_report_unknown_categories' :-
	setof(Ctg, '$lgt_unknown_category'(Ctg), Ctgs) ->
		'$lgt_inc_compile_warnings_counter',
		nl, write('  WARNING!  references to unknown categories: '),
		'$lgt_writeq_list'(Ctgs)
		;
		true.


'$lgt_unknown_category'(Ctg) :-
	'$lgt_pp_referenced_category_'(Ctg),
	\+ '$lgt_current_category_'(Ctg, _, _),
	\+ '$lgt_pp_category_'(Ctg, _, _, _, _, _),
	\+ '$lgt_pp_entity_init_'(Ctg, _).



% '$lgt_writeq_list'(+list)
%
% auxiliary predicate for writing a non-empty list of elements (quoted)

'$lgt_writeq_list'([Term]) :-
	writeq(Term), !.

'$lgt_writeq_list'([Term1, Term2| Terms]) :-
	writeq(Term1), write(', '),
	'$lgt_writeq_list'([Term2| Terms]).



% '$lgt_write_list'(+list)
%
% auxiliary predicate for writing a non-empty list of elements (non-quoted)

'$lgt_write_list'([Term]) :-
	write(Term), !.

'$lgt_write_list'([Term1, Term2| Terms]) :-
	write(Term1), write(', '),
	'$lgt_write_list'([Term2| Terms]).



% '$lgt_add_def_clause'(+atom, +integer, +atom, +term)
%
% adds a "def clause" (used to translate a predicate call)

'$lgt_add_def_clause'(Functor, Arity, Prefix, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	'$lgt_append'(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	once(
		('$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _);
		 '$lgt_pp_category_'(_, _, _, Def, _, _))),
	Clause =.. [Def, Head, Sender, This, Self, THead],
	('$lgt_pp_def_'(Clause) ->
		true
		;
		assertz('$lgt_pp_def_'(Clause))),
	('$lgt_built_in'(Head) ->
		assertz('$lgt_pp_redefined_built_in_'(Head, Ctx, THead))
		;
		true),
	('$lgt_pp_defs_pred_'(Functor, Arity) ->
		true
		;
		assertz('$lgt_pp_defs_pred_'(Functor, Arity))).



% '$lgt_add_ddef_clause'(+atom, +integer, +atom, +term)
%
% adds a "ddef clause" (used to translate a predicate call)

'$lgt_add_ddef_clause'(Functor, Arity, Prefix, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_ctx_ctx'(Ctx, Sender, This, Self, _, _),
	'$lgt_append'(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, DDef, _, _),
	Clause =.. [DDef, Head, Sender, This, Self, THead],
	('$lgt_pp_ddef_'(Clause) ->
		true
		;
		assertz('$lgt_pp_ddef_'(Clause))),
	('$lgt_built_in'(Head) ->
		assertz('$lgt_pp_redefined_built_in_'(Head, Ctx, THead))
		;
		true),
	('$lgt_pp_defs_pred_'(Functor, Arity) ->
		true
		;
		assertz('$lgt_pp_defs_pred_'(Functor, Arity))).



% '$lgt_assert_ddef_clause'(+atom, +integer, +atom, +atom, -callable)
%
% asserts a dynamic "ddef clause" (used to translate a predicate call)

'$lgt_assert_ddef_clause'(Functor, Arity, OPrefix, DDef, Call) :-
	'$lgt_construct_predicate_functor'(OPrefix, Functor, Arity, PPrefix),
	functor(Pred, Functor, Arity),
	Pred =.. [_| Args],
	'$lgt_append'(Args, [Sender, This, Self], TArgs),
	Call =.. [PPrefix| TArgs],
	Clause =.. [DDef, Pred, Sender, This, Self, Call],
	assertz(Clause),
	'$lgt_clean_lookup_caches'(Pred).



% '$lgt_update_ddef_table'(+atom, +callable, +callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% if there are no more clauses for the predicate otherwise does nothing
%
% this is needed in order to allow definitions in ancestors to be found

'$lgt_update_ddef_table'(DDef, Head, Call) :-
	functor(Call, CFunctor, CArity),
	functor(GCall, CFunctor, CArity),
	(clause(GCall, _) ->
		true
		;
		functor(Head, HFunctor, HArity),
		functor(GHead, HFunctor, HArity),
		Clause =.. [DDef, GHead, _, _, _, _],
		retractall(Clause),
		'$lgt_clean_lookup_caches'(GHead)).



% '$lgt_convert_test_scope'(@term, +term),
%
% convert asserta/z test scope to predicate declaration scope

'$lgt_convert_test_scope'(Scope, Scope2) :-
	var(Scope) ->
		Scope2 = p
		;
		((Scope = p(V), var(V)) ->
			Scope2 = p(p)
			;
			Scope2 = p(p(p))).



% '$lgt_assert_ddcl_clause'(+atom, +term, +term)
%
% asserts a dynamic predicate declaration

'$lgt_assert_ddcl_clause'(DDcl, Pred, Scope) :-
	functor(Pred, Functor, Arity),
	functor(DPred, Functor, Arity),
	Clause =.. [DDcl, DPred, Scope],
	assertz(Clause).



% '$lgt_generate_code'(+atom)
%
% generates code for the entity being compiled

'$lgt_generate_code'(protocol) :-
	'$lgt_fix_redef_built_ins',		% needed because of possible initialization goal
	'$lgt_gen_protocol_clauses',
	'$lgt_gen_protocol_directives',
	'$lgt_gen_init_goal'.

'$lgt_generate_code'(object) :-
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_object_clauses',
	'$lgt_gen_object_directives',
	'$lgt_gen_init_goal'.

'$lgt_generate_code'(category) :-
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_category_clauses',
	'$lgt_gen_category_directives',
	'$lgt_gen_init_goal'.



'$lgt_gen_object_directives' :-
	'$lgt_gen_object_dynamic_directives',
	'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_directives' :-
	'$lgt_gen_category_dynamic_directives',
	'$lgt_gen_category_discontiguous_directives'.



'$lgt_gen_protocol_directives' :-
	'$lgt_pp_protocol_'(_, Prefix, Dcl, Rnm, (dynamic)) ->
		assertz('$lgt_pp_directive_'(dynamic(Prefix/1))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3)))
		;
		true.



'$lgt_gen_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, (dynamic)) ->
		'$lgt_gen_dynamic_object_dynamic_directives'
		;
		'$lgt_gen_static_object_dynamic_directives'.



'$lgt_gen_dynamic_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _),
	assertz('$lgt_pp_directive_'(dynamic(Prefix/7))),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(Def/5))),
	assertz('$lgt_pp_directive_'(dynamic(Def/6))),
	assertz('$lgt_pp_directive_'(dynamic(Super/6))),
	assertz('$lgt_pp_directive_'(dynamic(IDcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(IDef/6))),
	assertz('$lgt_pp_directive_'(dynamic(DDcl/2))),
	assertz('$lgt_pp_directive_'(dynamic(DDef/5))),
	assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
	'$lgt_gen_dynamic_entity_dynamic_predicate_directives'.


'$lgt_gen_dynamic_entity_dynamic_predicate_directives' :-
	'$lgt_pp_def_'(Clause),
	Clause \= (_ :- _),		% only local table; reject linking clauses
	arg(5, Clause, Call),
	functor(Call, Functor, Arity),
	assertz('$lgt_pp_directive_'(dynamic(Functor/Arity))),
	fail.

'$lgt_gen_dynamic_entity_dynamic_predicate_directives'.



'$lgt_gen_static_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, DDcl, DDef, _, _),
	assertz('$lgt_pp_directive_'(dynamic(DDcl/2))),
	assertz('$lgt_pp_directive_'(dynamic(DDef/5))),
	'$lgt_pp_dynamic_'(Functor, Arity),
	'$lgt_construct_predicate_functor'(Prefix, Functor, Arity, TFunctor),
	TArity is Arity + 3,
	assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity))),
	fail.

'$lgt_gen_static_object_dynamic_directives'.



'$lgt_gen_object_discontiguous_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
	'$lgt_construct_predicate_functor'(Prefix, Functor, Arity, TFunctor),
	TArity is Arity + 3,
	assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_dynamic_directives' :-
	'$lgt_pp_category_'(_, Prefix, _, Def, Rnm, (dynamic)) ->
		assertz('$lgt_pp_directive_'(dynamic(Prefix/2))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Def/5))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
		'$lgt_gen_dynamic_entity_dynamic_predicate_directives'
		 ;
		 true.



'$lgt_gen_category_discontiguous_directives' :-
	'$lgt_pp_category_'(_, Prefix, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
	'$lgt_construct_predicate_functor'(Prefix, Functor, Arity, TFunctor),
	TArity is Arity + 3,
	assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_category_discontiguous_directives'.



'$lgt_gen_object_clauses' :-
	('$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _);
	 '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _)) ->
		'$lgt_gen_ic_clauses'
		;
		'$lgt_gen_prototype_clauses'.



% '$lgt_gen_local_dcl_clauses'
%
% a (local) predicate declaration is only generated
% if there is a scope declaration for the predicate

'$lgt_gen_local_dcl_clauses' :-
	'$lgt_pp_entity'(_, _, _, Dcl, _),
	(('$lgt_pp_public_'(Functor, Arity), Scope = p(p(p)));
	 ('$lgt_pp_protected_'(Functor, Arity), Scope = p(p));
	 ('$lgt_pp_private_'(Functor, Arity), Scope = p)),
	functor(Meta, Functor, Arity),
	('$lgt_pp_metapredicate_'(Meta) ->
		Meta2 = Meta
		;
		Meta2 = no),
	functor(Pred, Functor, Arity),
	('$lgt_pp_dynamic_'(Functor, Arity)->
		Compilation = (dynamic)
		;
		Compilation = static),
	Fact =.. [Dcl, Pred, Scope, Compilation, Meta2],
	assertz('$lgt_pp_dcl_'(Fact)),
	fail.

'$lgt_gen_local_dcl_clauses' :-		% generate a catchall clause if needed
	\+ '$lgt_pp_dcl_'(_) ->
		'$lgt_pp_entity'(_, _, _, Dcl, _),
		Head =.. [Dcl, _, _, _, _],
		assertz('$lgt_pp_dcl_'((Head:-fail)))
		;
		true.



% '$lgt_gen_local_def_clauses'
%
% generates local def clauses for undefined but declared (via scope or
% dynamic directives) predicates

'$lgt_gen_local_def_clauses' :-
	'$lgt_pp_entity'(_, _, EPrefix, _, _),
	'$lgt_pp_dynamic_'(Functor, Arity),
	\+ '$lgt_pp_defs_pred_'(Functor, Arity),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	'$lgt_ctx_ctx'(Ctx),
	((\+ '$lgt_pp_public_'(Functor, Arity),
	  \+ '$lgt_pp_protected_'(Functor, Arity),
	  \+ '$lgt_pp_private_'(Functor, Arity)) ->
		'$lgt_add_ddef_clause'(Functor, Arity, PPrefix, Ctx)
		;
		'$lgt_add_def_clause'(Functor, Arity, PPrefix, Ctx)),
	fail.

'$lgt_gen_local_def_clauses'.



'$lgt_gen_obj_catchall_def_clause' :-
	\+ '$lgt_pp_def_'(_) ->
		'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _),
		Head =.. [Def, _, _, _, _, _],
		assertz('$lgt_pp_def_'((Head:-fail)))
		;
		true.



'$lgt_gen_protocol_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_protocol_linking_clauses',
	'$lgt_gen_protocol_extend_clauses'.



'$lgt_gen_protocol_linking_clauses' :-
	'$lgt_pp_protocol_'(Ptc, _, PDcl, _, _),
	Head =.. [PDcl, Pred, Scope, Compilation, Meta, Ptc],
	Body =.. [PDcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_pp_dcl_'((Head:-Body))).



'$lgt_gen_protocol_extend_clauses' :-
	'$lgt_pp_protocol_'(_, _, PDcl1, PRnm, _),
	'$lgt_pp_extended_protocol_'(Ptc2, _, PDcl2, EScope),
	(EScope = (public) ->
		Lookup =.. [PDcl2, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl2, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [PDcl2, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ptc2, _, _) ->
		Head =.. [PDcl1, Alias, Scope, Compilation, Meta, Ctn],
		Rename =.. [PRnm, Ptc2, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [PDcl1, Pred, Scope, Compilation, Meta, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_protocol_extend_clauses'.



'$lgt_construct_alias_functor'(Prefix, PRen) :-
	atom_concat(Prefix, '_alias', PRen).



'$lgt_gen_category_clauses' :-
	'$lgt_gen_category_dcl_clauses',
	'$lgt_gen_category_def_clauses'.



'$lgt_gen_category_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_category_linking_dcl_clauses',
	'$lgt_gen_category_implements_dcl_clauses',
	'$lgt_gen_category_imports_dcl_clauses'.



'$lgt_gen_category_linking_dcl_clauses' :-
	'$lgt_pp_category_'(Ctg, _, CDcl, _, _, _),
	Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctg],
	Body =.. [CDcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_pp_dcl_'((Head:-Body))).



'$lgt_gen_category_implements_dcl_clauses' :-
	'$lgt_pp_category_'(_, _, CDcl, _, PRnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EScope),
	(EScope = (public) ->
		Lookup =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ptc, _, _) ->
		Head =.. [CDcl, Alias, Scope, Compilation, Meta, Ctn],
		Rename =.. [PRnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),	
	fail.

'$lgt_gen_category_implements_dcl_clauses'.



'$lgt_gen_category_imports_dcl_clauses' :-
	'$lgt_pp_category_'(_, _, CDcl, _, PRnm, _),
	'$lgt_pp_imported_category_'(Ctg, _, ECDcl, _, EScope),
	(EScope = (public) ->
		Lookup =.. [ECDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [ECDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [ECDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [CDcl, Alias, Scope, Compilation, Meta, Ctn],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_category_imports_dcl_clauses'.



'$lgt_gen_category_def_clauses' :-
	'$lgt_gen_category_catchall_def_clause',
	'$lgt_gen_category_linking_def_clauses',
	'$lgt_gen_category_imports_def_clauses'.



'$lgt_gen_category_catchall_def_clause' :-
	\+ '$lgt_pp_def_'(_) ->
		'$lgt_pp_category_'(_, _, _, Def, _, _),
		Head =.. [Def, _, _, _, _, _],
		assertz('$lgt_pp_def_'((Head:-fail)))
		;
		true.



'$lgt_gen_category_linking_def_clauses' :-
	'$lgt_pp_category_'(Ctg, _, _, Def, _, _),
	Head =.. [Def, Pred, Sender, This, Self, Call, Ctg],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body))).



'$lgt_gen_category_imports_def_clauses' :-
	'$lgt_pp_category_'(Ctg, _, _, Def, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_imports_category_'(Ctg, Ctg2, _)),		% needed for parameter passing
	'$lgt_pp_imported_category_'(Ctg2, _, _, Def2, _),
	Lookup =.. [Def2, Pred, Sender, This, Self, Call, Ctn],
	('$lgt_pp_alias_'(Ctg2, _, _) ->
		Head =.. [Def, Alias, Sender, This, Self, Call, Ctn],
		Rename =.. [PRnm, Ctg2, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [Def, Pred, Sender, This, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_category_imports_def_clauses'.



'$lgt_gen_prototype_clauses' :-
	'$lgt_gen_prototype_dcl_clauses',
	'$lgt_gen_prototype_def_clauses',
	'$lgt_gen_prototype_super_clauses'.



'$lgt_gen_prototype_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_prototype_linking_dcl_clauses',
	'$lgt_gen_prototype_implements_dcl_clauses',
	'$lgt_gen_prototype_imports_dcl_clauses',
	'$lgt_gen_prototype_extends_dcl_clauses'.



'$lgt_gen_prototype_linking_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, Dcl, _, _, _, _, DDcl, _, _, _),
	Head =.. [Dcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_pp_dcl_'((Head:-Body))),
	Head2 =.. [Dcl, Pred, Scope, (dynamic), no, Obj, Obj],
	Body2 =.. [DDcl, Pred, Scope],
	assertz('$lgt_pp_dcl_'((Head2:-Body2))).



'$lgt_gen_prototype_implements_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, PRnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EScope),
	(EScope = (public) ->
		Lookup =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ptc, _, _) ->
		Head =.. [ODcl, Alias, Scope, Compilation, Meta, Obj, Ctn],
		Rename =.. [PRnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_prototype_implements_dcl_clauses'.



'$lgt_gen_prototype_imports_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, PRnm, _),
	'$lgt_pp_imported_category_'(Ctg, _, CDcl, _, EScope),
	(EScope = (public) ->
		Lookup =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [CDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [ODcl, Alias, Scope, Compilation, Meta, Obj, Ctn],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),	
	fail.

'$lgt_gen_prototype_imports_dcl_clauses'.



'$lgt_gen_prototype_extends_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, PRnm, _),
	'$lgt_pp_extended_object_'(Parent, _, PDcl, _, _, _, _, _, _, EScope),
	(EScope = (public) ->
		Lookup =.. [PDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Lookup = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	('$lgt_pp_alias_'(Parent, _, _) ->
		Head =.. [ODcl, Alias, Scope, Compilation, Meta, SCtn, TCtn],
		Rename =.. [PRnm, Parent, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_prototype_extends_dcl_clauses'.



'$lgt_gen_prototype_def_clauses' :-
	'$lgt_gen_local_def_clauses',
	'$lgt_gen_obj_catchall_def_clause',
	'$lgt_gen_prototype_linking_def_clauses',
	'$lgt_gen_prototype_imports_def_clauses',
	'$lgt_gen_prototype_extends_def_clauses'.



'$lgt_gen_prototype_linking_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body2))).



'$lgt_gen_prototype_imports_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),			% needed for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	Lookup =.. [CDef, Pred, Sender, Obj, Self, Call, Ctn],
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, Sender, Obj, Self, Call, Ctn],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_prototype_imports_def_clauses'.



'$lgt_gen_prototype_extends_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_extends_object_'(Obj, Parent, _)),			% needed for parameter passing
	'$lgt_pp_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	Lookup =.. [PDef, Pred, Sender, Parent, Self, Call, Ctn],
	('$lgt_pp_alias_'(Parent, _, _) ->
		Head =.. [ODef, Alias, Sender, Obj, Self, Call, Ctn],
		Rename =.. [PRnm, Parent, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_prototype_extends_def_clauses'.



% we can have a root object where super have nowhere to go ...

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_pp_object_'(_, _, _, _, OSuper, _, _, _, _, _, _),
	\+ '$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz('$lgt_pp_def_'((Head:-fail))),
	!.

% ... or we may extends some objects

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, OSuper, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_extends_object_'(Obj, Parent, _)),			% needed for parameter passing
	'$lgt_pp_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	Lookup =.. [PDef, Pred, Sender, Parent, Self, Call, Ctn],
	('$lgt_pp_alias_'(Parent, _, _) ->
		Head =.. [OSuper, Alias, Sender, Obj, Self, Call, Ctn],
		Rename =.. [PRnm, Parent, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [OSuper, Pred, Sender, Obj, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_prototype_super_clauses'.



'$lgt_gen_ic_clauses' :-
	'$lgt_gen_ic_dcl_clauses',
	'$lgt_gen_ic_idcl_clauses',
	'$lgt_gen_ic_def_clauses',
	'$lgt_gen_ic_idef_clauses',
	'$lgt_gen_ic_super_clauses'.



'$lgt_gen_ic_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_ic_hierarchy_dcl_clauses'.



'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	!,
	'$lgt_pp_object_'(_, _, ODcl, _, _, _, _, _, _, _, _),
	Head =.. [ODcl, _, _, _, _, _, _],
	assertz('$lgt_pp_dcl_'((Head:-fail))).

'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, PRnm, _),
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, CIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Lookup =.. [CIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Lookup = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	('$lgt_pp_alias_'(Class, _, _) ->
		Head =.. [ODcl, Alias, Scope, Compilation, Meta, SCtn, TCtn],
		Rename =.. [PRnm, Class, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),	
	fail.

'$lgt_gen_ic_hierarchy_dcl_clauses'.



% generates instance/class inherited declaration clauses

'$lgt_gen_ic_idcl_clauses' :-
	'$lgt_gen_ic_linking_idcl_clauses',
	'$lgt_gen_ic_protocol_idcl_clauses',
	'$lgt_gen_ic_category_idcl_clauses',
	'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_linking_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, Dcl, _, _, IDcl, _, DDcl, _, _, _),
	Head =.. [IDcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_pp_dcl_'((Head:-Body))),
	Head2 =.. [IDcl, Pred, Scope, (dynamic), no, Obj, Obj],
	Body2 =.. [DDcl, Pred, Scope],
	assertz('$lgt_pp_dcl_'((Head2:-Body2))).



'$lgt_gen_ic_protocol_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, OIDcl, _, _, _, PRnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EScope),
	(EScope = (public) ->
		Lookup =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ptc, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Compilation, Meta, Obj, Ctn],
		Rename =.. [PRnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_ic_protocol_idcl_clauses'.



'$lgt_gen_ic_category_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, OIDcl, _, _, _, PRnm, _),
	'$lgt_pp_imported_category_'(Ctg, _, CDcl, _, EScope),
	(EScope = (public) ->
		Lookup =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Lookup =.. [CDcl, Pred, _, Compilation, Meta, Ctn])),
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Compilation, Meta, Obj, Ctn],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_ic_category_idcl_clauses'.



'$lgt_gen_ic_hierarchy_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, CIDcl, _, _, _, PRnm, _),
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, SIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Lookup =.. [SIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Lookup = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Lookup = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	('$lgt_pp_alias_'(Super, _, _) ->
		Head =.. [CIDcl, Alias, Scope, Compilation, Meta, SCtn, TCtn],
		Rename =.. [PRnm, Super, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [CIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))),
	fail.

'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_def_clauses' :-
	'$lgt_gen_local_def_clauses',
	'$lgt_gen_obj_catchall_def_clause',
	'$lgt_gen_ic_linking_def_clauses',
	'$lgt_gen_ic_imports_def_clauses',
	'$lgt_gen_ic_hierarchy_def_clauses'.



'$lgt_gen_ic_linking_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body2))).



'$lgt_gen_ic_imports_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),		% needed for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	Lookup =.. [CDef, Pred, Sender, Obj, Self, Call, Ctn],
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, Sender, Obj, Self, Call, Ctn],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),	
	fail.

'$lgt_gen_ic_imports_def_clauses'.



'$lgt_gen_ic_hierarchy_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_instantiates_class_'(Obj, Class, _)),	% needed for parameter passing
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	Lookup =.. [CIDef, Pred, Sender, Class, Self, Call, Ctn],
	('$lgt_pp_alias_'(Class, _, _) ->
		Head =.. [ODef, Alias, Sender, Obj, Self, Call, Ctn],
		Rename =.. [PRnm, Class, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),	
	fail.

'$lgt_gen_ic_hierarchy_def_clauses'.




'$lgt_gen_ic_idef_clauses' :-
	'$lgt_gen_ic_linking_idef_clauses',
	'$lgt_gen_ic_category_idef_clauses',
	'$lgt_gen_ic_hierarchy_idef_clauses'.



'$lgt_gen_ic_linking_idef_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, IDef, _, DDef, _, _),
	Head =.. [IDef, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_pp_def_'((Head:-Body2))).



'$lgt_gen_ic_category_idef_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, _, OIDef, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),		% needed for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	Lookup =.. [CDef, Pred, Sender, Obj, Self, Call],
	('$lgt_pp_alias_'(Ctg, _, _) ->
		Head =.. [OIDef, Alias, Sender, Obj, Self, Call, Ctg],
		Rename =.. [PRnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [OIDef, Pred, Sender, Obj, Self, Call, Ctg],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_ic_category_idef_clauses'.



'$lgt_gen_ic_hierarchy_idef_clauses' :-
	'$lgt_pp_object_'(Class, _, _, _, _, _, CIDef, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_specializes_class_'(Class, Super, _)),		% needed for parameter passing
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	Lookup =.. [SIDef, Pred, Sender, Super, Self, Call, Ctn],
	('$lgt_pp_alias_'(Super, _, _) ->
		Head =.. [CIDef, Alias, Sender, Class, Self, Call, Ctn],
		Rename =.. [PRnm, Super, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [CIDef, Pred, Sender, Class, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),		
	fail.

'$lgt_gen_ic_hierarchy_idef_clauses'.



% we can have a root object where super have nowhere to go ...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(_, _, _, _, OSuper, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz('$lgt_pp_def_'((Head:-fail))),
	!.

% ... or predicates can be redefined in instances...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, OSuper, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_instantiates_class_'(Obj, Class, _)),		% needed for parameter passing
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	Lookup =.. [CIDef, Pred, Sender, Class, Obj, Call, Ctn],
	('$lgt_pp_alias_'(Class, _, _) ->
		Head =.. [OSuper, Alias, Sender, Obj, Obj, Call, Ctn],
		Rename =.. [PRnm, Class, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [OSuper, Pred, Sender, Obj, Obj, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),	
	fail.

% ... or/and in subclasses...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(Class, _, _, _, CSuper, _, _, _, _, PRnm, _),
	'$lgt_pp_rclause_'('$lgt_specializes_class_'(Class, Super, _)),		% needed for parameter passing
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	Lookup =.. [SIDef, Pred, Sender, Super, Self, Call, Ctn],
	('$lgt_pp_alias_'(Super, _, _) ->
		Head =.. [CSuper, Alias, Sender, Class, Self, Call, Ctn],
		Rename =.. [PRnm, Super, Pred, Alias],
		assertz('$lgt_pp_def_'((Head :- var(Alias) -> Lookup, Rename; Rename, Lookup)))
		;
		Head =.. [CSuper, Pred, Sender, Class, Self, Call, Ctn],
		assertz('$lgt_pp_def_'((Head:-Lookup)))),
	fail.

'$lgt_gen_ic_super_clauses'.



% '$lgt_fix_redef_built_ins'
%
% fix the calls of any redefined built-in predicate in all entity clauses 
% and initialization goals

'$lgt_fix_redef_built_ins' :-
	retract('$lgt_pp_eclause_'(Clause)),
	(Clause = (Head:-Body) ->
		'$lgt_fix_redef_built_ins'(Body, FBody),
		assertz('$lgt_pp_feclause_'((Head:-FBody)))
		;
		assertz('$lgt_pp_feclause_'(Clause))),
	fail.

'$lgt_fix_redef_built_ins' :-
	retract('$lgt_pp_entity_init_'(Call)),
	'$lgt_fix_redef_built_ins'(Call, Fixed),
	assertz('$lgt_pp_fentity_init_'(Fixed)),
	fail.

'$lgt_fix_redef_built_ins'.



% '$lgt_fix_redef_built_ins'(+body, -body)
%
% fix calls to redefined built-in predicates in a clause body

'$lgt_fix_redef_built_ins'(Pred, Pred) :-
	var(Pred),
	!.

'$lgt_fix_redef_built_ins'((Pred1, Pred2), (TPred1, TPred2)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred1, TPred1),
	'$lgt_fix_redef_built_ins'(Pred2, TPred2).

'$lgt_fix_redef_built_ins'((Pred1; Pred2), (TPred1; TPred2)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred1, TPred1),
	'$lgt_fix_redef_built_ins'(Pred2, TPred2).

'$lgt_fix_redef_built_ins'((Pred1 -> Pred2), (TPred1 -> TPred2)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred1, TPred1),
	'$lgt_fix_redef_built_ins'(Pred2, TPred2).

'$lgt_fix_redef_built_ins'(\+ Pred, \+ TPred) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(call(Pred), call(TPred)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(once(Pred), once(TPred)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery)) :-
	!,
	'$lgt_fix_redef_built_ins'(Goal, TGoal),
	'$lgt_fix_redef_built_ins'(Recovery, TRecovery).

'$lgt_fix_redef_built_ins'(bagof(Term, Pred, List), bagof(Term, TPred, List)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(findall(Term, Pred, List), findall(Term, TPred, List)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(forall(Gen, Test), forall(TGen, TTest)) :-
	!,
	'$lgt_fix_redef_built_ins'(Gen, TGen),
	'$lgt_fix_redef_built_ins'(Test, TTest).

'$lgt_fix_redef_built_ins'(setof(Term, Pred, List), setof(Term, TPred, List)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'('$lgt_dbg_goal'(OPred, Pred, Ctx), '$lgt_dbg_goal'(OPred, TPred, Ctx)) :-
	!,
	'$lgt_fix_redef_built_ins'(Pred, TPred).

'$lgt_fix_redef_built_ins'(Pred, TPred) :-
	'$lgt_pl_built_in'(Pred),
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity), 
	'$lgt_pl_metapredicate'(Meta),	% non-standard Prolog built-in metapredicates
	!,
	Pred =.. [_| Args],
	Meta =.. [_| MArgs],
	'$lgt_fix_redef_built_ins_in_margs'(Args, MArgs, TArgs),
	TPred =.. [Functor| TArgs].

'$lgt_fix_redef_built_ins'('$lgt_call_built_in'(Pred, Ctx), TPred) :-
	!,
	('$lgt_pp_redefined_built_in_'(Pred, Ctx, TPred) ->
		true
		;
		'$lgt_fix_redef_built_ins'(Pred, TPred)).

'$lgt_fix_redef_built_ins'(Pred, Pred).



% '$lgt_fix_redef_built_ins_in_margs'(@list, @list, -list)
%
% fix calls to redefined built-in predicates in non-standard
% metapredicate arguments

'$lgt_fix_redef_built_ins_in_margs'([], [], []).

'$lgt_fix_redef_built_ins_in_margs'([Arg| Args], [MArg| MArgs], [TArg| TArgs]) :-
	'$lgt_fix_redef_built_ins_in_marg'(MArg, Arg, TArg),
	'$lgt_fix_redef_built_ins_in_margs'(Args, MArgs, TArgs).


'$lgt_fix_redef_built_ins_in_marg'(*, Arg, Arg).

'$lgt_fix_redef_built_ins_in_marg'(::, Arg, TArg) :-
	'$lgt_fix_redef_built_ins'(Arg, TArg).



% find and report misspelt predicate calls in the body of object and category predicates

'$lgt_report_misspelt_calls' :-
	'$lgt_compiler_flag'(misspelt, warning),
	setof(Pred, '$lgt_misspelt_call'(Pred), Preds),
	'$lgt_inc_compile_warnings_counter',
	nl, write('  WARNING!  these static predicates are called but never defined: '),
	'$lgt_writeq_list'(Preds),
	!.

'$lgt_report_misspelt_calls'.


'$lgt_misspelt_call'(Functor/Arity) :-
	'$lgt_pp_calls_pred_'(Functor, Arity),
	\+ '$lgt_pp_defs_pred_'(Functor, Arity),
	\+ '$lgt_pp_dynamic_'(Functor, Arity).



% report non-portable predicate calls in the body of object and category predicates

'$lgt_report_non_portable_calls' :-
	'$lgt_compiler_flag'(portability, warning),
	setof(Pred, '$lgt_non_portable_call'(Pred), Preds),
	'$lgt_inc_compile_warnings_counter',
	nl, write('  WARNING!  non-ISO defined built-in predicate calls: '),
	'$lgt_writeq_list'(Preds),
	!.

'$lgt_report_non_portable_calls'.


'$lgt_non_portable_call'(Functor/Arity) :-
	'$lgt_non_portable_call_'(Functor, Arity),
	\+ '$lgt_pp_defs_pred_'(Functor, Arity),
	functor(Pred, Functor, Arity),
	\+ '$lgt_pp_redefined_built_in_'(Pred, _, _).



% '$lgt_pp_entity_functors'(-compound)

'$lgt_pp_entity_functors'(Clause) :-
	'$lgt_pp_object_'(_, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _),
	Clause =.. [Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm],
	!.

'$lgt_pp_entity_functors'(Clause) :-
	'$lgt_pp_category_'(_, Prefix, Dcl, Def, Rnm, _),
	Clause =.. [Prefix, Dcl, Def, Rnm],
	!.

'$lgt_pp_entity_functors'(Clause) :-
	'$lgt_pp_protocol_'(_, Prefix, Dcl, Rnm, _),
	Clause =.. [Prefix, Dcl, Rnm].



% '$lgt_write_directives'(+stream)
%
% writes the directives

'$lgt_write_directives'(Stream) :-
	'$lgt_pp_directive_'(Dir),
	write_canonical(Stream, (:- Dir)),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_directives'(_).



% '$lgt_write_prolog_clauses'(+stream)
%
% writes Prolog clauses which appear before the entity opening directive

'$lgt_write_prolog_clauses'(Stream) :-
	'$lgt_pp_ppclause_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_prolog_clauses'(_).



% '$lgt_write_logtalk_clauses'(+stream)
%
% writes Logtalk entity clauses

'$lgt_write_logtalk_clauses'(Stream) :-
	'$lgt_write_functors_clause'(Stream),
	'$lgt_write_dcl_clauses'(Stream),
	'$lgt_write_def_clauses'(Stream),
	'$lgt_write_ddef_clauses'(Stream),
	'$lgt_write_super_clauses'(Stream),
	'$lgt_write_alias_clauses'(Stream),
	'$lgt_write_pred_clauses'(Stream).



'$lgt_write_functors_clause'(Stream) :-
	'$lgt_pp_entity_functors'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream).



'$lgt_write_def_clauses'(Stream) :-
	'$lgt_pp_def_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_def_clauses'(_).



'$lgt_write_ddef_clauses'(Stream) :-
	'$lgt_pp_ddef_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_ddef_clauses'(_).



'$lgt_write_dcl_clauses'(Stream) :-
	'$lgt_pp_dcl_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_dcl_clauses'(_).



'$lgt_write_super_clauses'(Stream) :-
	'$lgt_pp_super_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_super_clauses'(_).



'$lgt_write_alias_clauses'(Stream) :-
	once((
		'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, Rnm, _);
		'$lgt_pp_category_'(_, _, _, _, Rnm, _);
		'$lgt_pp_protocol_'(_, _, _, Rnm, _))),
	'$lgt_write_alias_clauses'(Stream, Rnm).


'$lgt_write_alias_clauses'(Stream, Rnm) :-
	'$lgt_pp_alias_'(Entity, Pred, Alias),
	Clause =.. [Rnm, Entity, Pred, Alias],
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_alias_clauses'(Stream, Rnm) :-
	Catchall =.. [Rnm, _, Pred, Pred],
	write_canonical(Stream, Catchall),
	write(Stream, '.'),
	nl(Stream).



'$lgt_write_pred_clauses'(Stream) :-
	'$lgt_pp_feclause_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_pred_clauses'(_).



% '$lgt_write_init_call'(+stream)
%
% writes the initialization call for the compiled source file that will assert 
% the relation clauses for all defined entities and call any declared entity 
% initialization goals when the source file is loaded

'$lgt_write_init_call'(Stream) :-
	'$lgt_init_goal'(Goal),
	(Goal = true ->
		true
		;
		('$lgt_compiler_flag'(iso_initialization_dir, true) ->
			write_canonical(Stream, (:- initialization(Goal)))
			;
			write_canonical(Stream, (:- Goal))),
		write(Stream, '.'), nl(Stream)).



'$lgt_init_goal'(Goal) :-
	findall(EGoal, '$lgt_pp_entity_init_'(_, EGoal), EGoals),
	('$lgt_pp_file_init_'(FGoal) ->
		(EGoals \= [] ->
			'$lgt_list_to_conjunction'(EGoals, EGoals2),
			Goal = (EGoals2, FGoal)
			;
			Goal = FGoal)
		;
		(EGoals \= [] ->
			'$lgt_list_to_conjunction'(EGoals, Goal)
			;
			Goal = true)).



'$lgt_list_to_conjunction'([G], G) :-
	!.

'$lgt_list_to_conjunction'([G1, G2| Gs], (G1, R)) :-
	'$lgt_list_to_conjunction'([G2| Gs], R).



'$lgt_gen_init_goal' :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
	findall(Clause, '$lgt_pp_rclause'(Clause), Clauses),
	Goal1 = '$lgt_assert_runtime_clauses'(Clauses),
	('$lgt_pp_fentity_init_'(Goal2) ->
		Goal = (Goal1, Goal2)
		;
		Goal = Goal1),
	assertz('$lgt_pp_entity_init_'(Entity, Goal)).



% '$lgt_assert_tr_entity'
%
% adds a dynamically created entity to memory

'$lgt_assert_tr_entity' :-
	'$lgt_assert_directives',
	'$lgt_assert_functors_clause',
	'$lgt_assert_dcl_clauses',
	'$lgt_assert_def_clauses',
	'$lgt_assert_ddef_clauses',
	'$lgt_assert_super_clauses',
	'$lgt_assert_pred_clauses',
	'$lgt_assert_runtime_clauses',
	'$lgt_assert_init'.



'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(dynamic(Functor/Arity)),
	functor(Pred, Functor, Arity),
	asserta(Pred),
	retract(Pred),
	fail.

'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(op(Pr, Spec, Ops)),
	op(Pr, Spec, Ops),
	fail.
	
'$lgt_assert_directives'.



'$lgt_assert_functors_clause' :-
	'$lgt_pp_entity_functors'(Clause),
	assertz(Clause).



'$lgt_assert_dcl_clauses' :-
	'$lgt_pp_dcl_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_dcl_clauses'.



'$lgt_assert_def_clauses' :-
	'$lgt_pp_def_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_def_clauses'.



'$lgt_assert_ddef_clauses' :-
	'$lgt_pp_ddef_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_ddef_clauses'.



'$lgt_assert_super_clauses' :-
	'$lgt_pp_super_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_super_clauses'.



'$lgt_assert_pred_clauses' :-
	'$lgt_pp_feclause_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_pred_clauses'.



'$lgt_assert_runtime_clauses' :-
	'$lgt_pp_rclause'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_runtime_clauses'.



% '$lgt_assert_init'
%
% call any defined initialization goal for a dynamically created entity

'$lgt_assert_init' :-
	'$lgt_pp_fentity_init_'(Goal) ->
		once(Goal)
		;
		true.



% '$lgt_assert_runtime_clauses'(+list)
%
% called when loading a compiled Logtalk entity to update Logtalk 
% internal tables
%
% we may be reloading the entity so we must first retract any old
% runtime clauses before asserting the new ones

'$lgt_assert_runtime_clauses'([Clause| Clauses]) :-
	arg(1, Clause, Entity),
	('$lgt_redefined_entity'(Entity, Type) ->
		'$lgt_clean_lookup_caches',
		'$lgt_clean_redefined_entity'(Type, Entity),
		'$lgt_report_redefined_entity'(Type, Entity)
		;
		true),
	'$lgt_retract_old_runtime_clauses'(Entity),
	'$lgt_assert_new_runtime_clauses'([Clause| Clauses]).


'$lgt_retract_old_runtime_clauses'(Entity) :-
	retractall('$lgt_current_object_'(Entity, _, _, _, _, _)),
	retractall('$lgt_current_protocol_'(Entity, _, _)),
	retractall('$lgt_current_category_'(Entity, _, _)),
	retractall('$lgt_implements_protocol_'(Entity, _, _)),
	retractall('$lgt_imports_category_'(Entity, _, _)),
	retractall('$lgt_instantiates_class_'(Entity, _, _)),
	retractall('$lgt_specializes_class_'(Entity, _, _)),
	retractall('$lgt_extends_protocol_'(Entity, _, _)),
	retractall('$lgt_extends_object_'(Entity, _, _)),
	retractall('$lgt_debugging_'(Entity)).


'$lgt_assert_new_runtime_clauses'([]).

'$lgt_assert_new_runtime_clauses'([Clause| Clauses]) :-
	assertz(Clause),
	'$lgt_assert_new_runtime_clauses'(Clauses).



% '$lgt_construct_object_functors'(+compound, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of an object

'$lgt_construct_object_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm) :-
	'$lgt_construct_entity_prefix'(Obj, Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def),
	atom_concat(Prefix, '_super', Super),
	atom_concat(Prefix, '_idcl', IDcl),
	atom_concat(Prefix, '_idef', IDef),
	atom_concat(Prefix, '_ddcl', DDcl),
	atom_concat(Prefix, '_ddef', DDef),
	atom_concat(Prefix, '_alias', Rnm).



% '$lgt_construct_protocol_functors'(+compound, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of a protocol

'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm) :-
	'$lgt_construct_entity_prefix'(Ptc, Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_alias', Rnm).



% '$lgt_construct_category_functors'(+compound, -atom, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of a category

'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, Rnm) :-
	'$lgt_construct_entity_prefix'(Ctg, Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def),
	atom_concat(Prefix, '_alias', Rnm).



% '$lgt_construct_entity_prefix'(@entity_identifier, -atom)
%
% constructs the entity prefix used in the compiled code

'$lgt_construct_entity_prefix'(Entity, Prefix) :-
	'$lgt_compiler_flag'(code_prefix, CodePrefix),
	functor(Entity, Functor, Arity),
	atom_concat(CodePrefix, Functor, Aux1),
	number_codes(Arity, ArityCodes),
	atom_codes(ArityAtom, ArityCodes),
	atom_concat(Aux1, '_', Aux2),
	atom_concat(Aux2, ArityAtom, Aux3),
	atom_concat(Aux3, '_', Prefix).



% '$lgt_construct_predicate_functor'(+atom, +atom, +integer, -atom)
%
% constructs the functor used for a compiled predicate

'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix) :-
	atom_concat(EPrefix, Functor, Aux),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Aux, Atom, PPrefix).



% '$lgt_reverse_predicate_functor'(+atom, +integer, -callable, -atom, -atom, -integer)
%
% reverses the functor used for a compiled predicate

'$lgt_reverse_predicate_functor'(TFunctor, TArity, Entity, Type, Functor, Arity) :-
	atom(TFunctor),
	integer(TArity),
	Arity is TArity - 3,	% subtract message execution context arguments
	Arity >= 0,
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Prefix, FunctorPlusArity, TFunctor),	% generate and test
	('$lgt_current_object_'(Entity, Prefix, _, _, _, _), Type = object
	;
	'$lgt_current_category_'(Entity, Prefix, _), Type = category),
	atom_concat(Functor, Atom, FunctorPlusArity),
	!.



% '$lgt_built_in'(+callable)
%
% checks if the argument is either a Prolog or Logtalk built-in predicate

'$lgt_built_in'(Pred) :-
	'$lgt_pl_built_in'(Pred),
	!.

'$lgt_built_in'(Pred) :-
	'$lgt_lgt_built_in'(Pred).



% '$lgt_pl_built_in'(+callable)
%
% either host Prolog native built-ins or missing ISO built-ins
% that we have defined in the correspondent config file

'$lgt_pl_built_in'(Pred) :-
	\+ '$lgt_lgt_built_in'(Pred),	% Logtalk built-ins may also have the property built_in
	'$lgt_predicate_property'(Pred, built_in),
	!.

'$lgt_pl_built_in'(Pred) :-
	'$lgt_iso_predicate'(Pred).



% logtalk built-in methods
%
% '$lgt_built_in_method'(?callable, ?scope)

'$lgt_built_in_method'(parameter(_, _), p).
'$lgt_built_in_method'(self(_), p).
'$lgt_built_in_method'(sender(_), p).
'$lgt_built_in_method'(this(_), p).

'$lgt_built_in_method'(current_predicate(_), p(p(p))).
'$lgt_built_in_method'(predicate_property(_, _), p(p(p))).

'$lgt_built_in_method'(abolish(_), p(p(p))).
'$lgt_built_in_method'(asserta(_), p(p(p))).
'$lgt_built_in_method'(assertz(_), p(p(p))).
'$lgt_built_in_method'(clause(_, _), p(p(p))).
'$lgt_built_in_method'(retract(_), p(p(p))).
'$lgt_built_in_method'(retractall(_), p(p(p))).

'$lgt_built_in_method'(bagof(_, _, _), p(p(p))).
'$lgt_built_in_method'(findall(_, _, _), p(p(p))).
'$lgt_built_in_method'(forall(_, _), p(p(p))).
'$lgt_built_in_method'(setof(_, _, _), p(p(p))).

'$lgt_built_in_method'(expand_term(_, _), p(p(p))).
'$lgt_built_in_method'(phrase(_, _), p(p(p))).
'$lgt_built_in_method'(phrase(_, _, _), p(p(p))).



% Logtalk directives
%
% '$lgt_lgt_directive'(+atom, +integer)

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_opening_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_closing_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_entity_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_predicate_directive'(Functor, Arity).


'$lgt_lgt_opening_directive'(object, N) :-
	N >= 1, N =< 5.

'$lgt_lgt_opening_directive'(category, N) :-
	N >= 1, N =< 3.

'$lgt_lgt_opening_directive'(protocol, N) :-
	N =:= 1; N =:= 2.


'$lgt_lgt_closing_directive'(end_object, 0).

'$lgt_lgt_closing_directive'(end_category, 0).

'$lgt_lgt_closing_directive'(end_protocol, 0).



'$lgt_lgt_entity_directive'(encoding, 1).

'$lgt_lgt_entity_directive'(calls, N) :-
	N >= 1.
'$lgt_lgt_entity_directive'(uses, N) :-
	N =:= 1; N =:= 2.

'$lgt_lgt_entity_directive'((initialization), 1).

'$lgt_lgt_entity_directive'((dynamic), N) :-
	N =:= 0.

'$lgt_lgt_entity_directive'(op, 3).

'$lgt_lgt_entity_directive'(info, 1).



'$lgt_lgt_predicate_directive'((dynamic), N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'(metapredicate, N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((discontiguous), N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((public), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(protected, N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(private, N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((mode), 2).

'$lgt_lgt_predicate_directive'(info, 2).

'$lgt_lgt_predicate_directive'(alias, 3).



% built-in metapredicates

'$lgt_metapredicate'(Meta) :-			% Logtalk built-in metapredicate
	'$lgt_lgt_metapredicate'(Meta).

'$lgt_metapredicate'(Meta) :-			% (non ISO Standard) Prolog metapredicate
	'$lgt_pl_metapredicate'(Meta).		% specified in the config files



% built-in Logtalk (and Prolog) metapredicates

'$lgt_lgt_metapredicate'(catch(::, *, ::)).

'$lgt_lgt_metapredicate'(bagof(*, ::, *)).
'$lgt_lgt_metapredicate'(setof(*, ::, *)).
'$lgt_lgt_metapredicate'(findall(*, ::, *)).

'$lgt_lgt_metapredicate'(forall(::, ::)).

'$lgt_lgt_metapredicate'(call(::)).
'$lgt_lgt_metapredicate'(once(::)).

'$lgt_lgt_metapredicate'(\+ (::)).



% utility predicates used during compilation of Logtalk entities to store 
% and access context information which is represented by a compound term

'$lgt_ctx_ctx'(ctx(_, _, _, _, _)).

'$lgt_ctx_ctx'(ctx(Sender, This, Self, Prefix, Metavars), Sender, This, Self, Prefix, Metavars).

'$lgt_ctx_sender'(ctx(Sender, _, _, _, _), Sender).

'$lgt_ctx_this'(ctx(_, This, _, _, _), This).

'$lgt_ctx_self'(ctx(_, _, Self, _, _), Self).

'$lgt_ctx_prefix'(ctx(_, _, _, Prefix, _), Prefix).

'$lgt_ctx_metavars'(ctx(_, _, _, _, Metavars), Metavars).



% '$lgt_convert_to_list'(+pi_or_pi_list, -pi_list)

'$lgt_convert_to_list'([[A|B]], [A|B]) :-	% predicate indicator list
	!.

'$lgt_convert_to_list'([A|B], [A|B]) :-		% predicate indicator sequence
	!.

'$lgt_convert_to_list'(A, [A]).				% single predicate indicator



% '$lgt_valid_pred_ind(@term, -atom, -integer)
%
% valid predicate indicator

'$lgt_valid_pred_ind'(Term, Functor, Arity) :-
	nonvar(Term),
	Term = Functor/Arity,
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_gr_ind(@term, -atom, -integer, -integer)
%
% valid grammar rule indicator

'$lgt_valid_gr_ind'(Term, Functor, Arity, Arity2) :-
	nonvar(Term),
	Term = Functor//Arity,
	atom(Functor),
	integer(Arity),
	Arity >= 0,
	Arity2 is Arity + 2.



% '$lgt_valid_pred_or_gr_ind(@term, -atom, -integer)
%
% valid predicate indicator or grammar rule indicator

'$lgt_valid_pred_or_gr_ind'(Term, Functor, Arity) :-
	nonvar(Term),
	once((Term = Functor/Arity; Term = Functor//Arity)),
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_ref_scope'(@term, -atom)
	
'$lgt_valid_ref_scope'(Ref, Scope) :-
	nonvar(Ref),	
	(Ref = (Scope::_) ->
		nonvar(Scope),
		'$lgt_scope'(Scope, _)
		;
		Scope = (public)).



% '$lgt_valid_protocol_ref'(+term, -atom)

'$lgt_valid_protocol_ref'(_::Ptc, Ptc) :-
	!,
	atom(Ptc).

'$lgt_valid_protocol_ref'(Ptc, Ptc) :-
	atom(Ptc).



% '$lgt_valid_object_ref'(+term, -atom)

'$lgt_valid_object_ref'(_::Obj, Obj) :-
	!,
	callable(Obj).

'$lgt_valid_object_ref'(Obj, Obj) :-
	callable(Obj).



% '$lgt_valid_category_ref'(+term, -atom)

'$lgt_valid_category_ref'(_::Ctg, Ctg) :-
	!,
	atom(Ctg).

'$lgt_valid_category_ref'(Ctg, Ctg) :-
	atom(Ctg).



% '$lgt_valid_op_priority'(@term)

'$lgt_valid_op_priority'(Pr) :-
	integer(Pr),
	Pr >= 0,
	Pr =< 1200.



% '$lgt_valid_op_specifier'(@term)

'$lgt_valid_op_specifier'(Spec) :-
	nonvar(Spec),
	'$lgt_op_specifier'(Spec).



% '$lgt_op_specifier'(@nonvar)

'$lgt_op_specifier'(fx).
'$lgt_op_specifier'(fy).
'$lgt_op_specifier'(xfx).
'$lgt_op_specifier'(xfy).
'$lgt_op_specifier'(yfx).
'$lgt_op_specifier'(xf).
'$lgt_op_specifier'(yf).



% '$lgt_valid_op_names'(@term)

'$lgt_valid_op_names'(Ops) :-
	nonvar(Ops),
	'$lgt_convert_to_list'(Ops, List),
	'$lgt_valid_op_names_aux'(List).


'$lgt_valid_op_names_aux'([]).

'$lgt_valid_op_names_aux'([Op| Ops]) :-
	atom(Op),
	'$lgt_valid_op_names_aux'(Ops).



% '$lgt_same_op_class'(?atom, ?atom)

'$lgt_same_op_class'(fx, fx).
'$lgt_same_op_class'(fx, fy).

'$lgt_same_op_class'(fy, fx).
'$lgt_same_op_class'(fy, fy).

'$lgt_same_op_class'(xf, xf).
'$lgt_same_op_class'(xf, yf).

'$lgt_same_op_class'(yf, xf).
'$lgt_same_op_class'(yf, yf).

'$lgt_same_op_class'(xfx, xfx).
'$lgt_same_op_class'(xfx, xfy).
'$lgt_same_op_class'(xfx, yfx).

'$lgt_same_op_class'(xfy, xfx).
'$lgt_same_op_class'(xfy, xfy).
'$lgt_same_op_class'(xfy, yfx).

'$lgt_same_op_class'(yfx, xfx).
'$lgt_same_op_class'(yfx, xfy).
'$lgt_same_op_class'(yfx, yfx).



% '$lgt_valid_metapred_term'(@term)

'$lgt_valid_metapred_term'(Pred) :-
	nonvar(Pred),
	Pred =.. [_| Args],
	'$lgt_valid_metapred_term_args'(Args).


'$lgt_valid_metapred_term_args'([]).

'$lgt_valid_metapred_term_args'([Arg| Args]) :-
	nonvar(Arg),
	once((Arg = (::); Arg = (*))),
	'$lgt_valid_metapred_term_args'(Args).



% '$lgt_valid_mode_term'(@term)

'$lgt_valid_mode_term'(Pred) :-
	nonvar(Pred),
	Pred =.. [_| Args],
	'$lgt_valid_mode_term_args'(Args).


'$lgt_valid_mode_term_args'([]).

'$lgt_valid_mode_term_args'([Arg| Args]) :-
	nonvar(Arg),
	functor(Arg, Functor, Arity),
	Arity =< 1,
	'$lgt_pred_arg_instantiation_mode'(Functor),
	'$lgt_valid_mode_term_args'(Args).



% '$lgt_pred_arg_instantiation_mode'(@nonvar)

'$lgt_pred_arg_instantiation_mode'(?).
'$lgt_pred_arg_instantiation_mode'(+).
'$lgt_pred_arg_instantiation_mode'(-).
'$lgt_pred_arg_instantiation_mode'(@).



% '$lgt_valid_number_of_solutions'(@term)

'$lgt_valid_number_of_solutions'(Solutions) :-
	nonvar(Solutions),
	'$lgt_pred_number_of_solutions'(Solutions).



% '$lgt_pred_number_of_solutions'(@nonvar)

'$lgt_pred_number_of_solutions'(zero).
'$lgt_pred_number_of_solutions'(one).
'$lgt_pred_number_of_solutions'(zero_or_one).
'$lgt_pred_number_of_solutions'(zero_or_more).
'$lgt_pred_number_of_solutions'(one_or_more).
'$lgt_pred_number_of_solutions'(error).



% '$lgt_valid_pred_property'(@nonvar)

'$lgt_valid_pred_property'((public)).
'$lgt_valid_pred_property'(protected).
'$lgt_valid_pred_property'(private).
'$lgt_valid_pred_property'(static).
'$lgt_valid_pred_property'((dynamic)).
'$lgt_valid_pred_property'(declared_in(_)).
'$lgt_valid_pred_property'(defined_in(_)).
'$lgt_valid_pred_property'(metapredicate(_)).
'$lgt_valid_pred_property'(built_in).
'$lgt_valid_pred_property'(alias(_)).



% '$lgt_valid_entity_property'(@nonvar)

'$lgt_valid_entity_property'((dynamic)).
'$lgt_valid_entity_property'(static).
'$lgt_valid_entity_property'(built_in).



% '$lgt_valid_compiler_flags'(@list)
%
% true if all compiler flags are valid

'$lgt_valid_compiler_flags'([]).

'$lgt_valid_compiler_flags'([Flag| Flags]) :-
	nonvar(Flag),
	'$lgt_valid_compiler_flag'(Flag),
	'$lgt_valid_compiler_flags'(Flags).



% '$lgt_valid_compiler_flag'(@nonvar)

'$lgt_valid_compiler_flag'(iso_initialization_dir(Option)) :-
	once((Option == true; Option == false)).

'$lgt_valid_compiler_flag'(xml(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_flag'(xsl(File)) :-
	atom(File).

'$lgt_valid_compiler_flag'(unknown(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(singletons(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(misspelt(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(lgtredef(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(plredef(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(portability(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_flag'(report(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_flag'(smart_compilation(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_flag'(underscore_vars(Option)) :-
	once((Option == dont_care; Option == singletons)).

'$lgt_valid_compiler_flag'(code_prefix(Prefix)) :-
	atom(Prefix).

'$lgt_valid_compiler_flag'(doctype(Option)) :-
	once((Option == standalone; Option == (local); Option == web)).

'$lgt_valid_compiler_flag'(xmlspec(Option)) :-
	once((Option == dtd; Option == xsd)).

'$lgt_valid_compiler_flag'(debug(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_flag'(events(Option)) :-
	once((Option == on; Option == off)).



% '$lgt_valid_flag'(@nonvar)
%
% true if the argument is a valid Logtalk flag name

'$lgt_valid_flag'(iso_initialization_dir).
'$lgt_valid_flag'(xml).
'$lgt_valid_flag'(xsl).
'$lgt_valid_flag'(xmlspec).
'$lgt_valid_flag'(doctype).
'$lgt_valid_flag'(unknown).
'$lgt_valid_flag'(singletons).
'$lgt_valid_flag'(misspelt).
'$lgt_valid_flag'(lgtredef).
'$lgt_valid_flag'(plredef).
'$lgt_valid_flag'(portability).
'$lgt_valid_flag'(report).
'$lgt_valid_flag'(smart_compilation).
'$lgt_valid_flag'(startup_message).
'$lgt_valid_flag'(version).
'$lgt_valid_flag'(underscore_vars).
'$lgt_valid_flag'(code_prefix).
'$lgt_valid_flag'(debug).
'$lgt_valid_flag'(supports_break_predicate).
'$lgt_valid_flag'(events).
'$lgt_valid_flag'(altdirs).


% '$lgt_valid_flag'(@term, @term)
%
% true if the argument is a valid Logtalk flag name-value pair

'$lgt_valid_flag'(Name, Value) :-
	atom(Name),
	Flag =.. [Name, Value],
	'$lgt_valid_compiler_flag'(Flag).



% '$lgt_read_only_flag'(@nonvar)
%
% true if the argument is a read only Logtalk flag name

'$lgt_read_only_flag'(iso_initialization_dir).
'$lgt_read_only_flag'(startup_message).
'$lgt_read_only_flag'(supports_break_predicate).
'$lgt_read_only_flag'(version).
'$lgt_read_only_flag'(altdirs).
'$lgt_read_only_flag'(supports_encoding_dir).



% '$'$lgt_valid_pred_argument'(@term)
%
% valid predicate argument documentation on info/2 directive

'$lgt_valid_entity_parameter'(Name - Description) :-
	atom(Name),
	atom(Description).



% '$'$lgt_valid_pred_argument'(@term)
%
% valid predicate argument documentation on info/2 directive

'$lgt_valid_pred_argument'(Name - Description) :-
	atom(Name),
	atom(Description).



% '$lgt_valid_pred_exception'(@term)
%
% valid predicate exception documentation on info/2 directive

'$lgt_valid_pred_exception'(Description - Term) :-
	atom(Description),
	nonvar(Term).



% '$lgt_valid_pred_call_example'(@term)
%
% valid predicate call example documentation on info/1 directive

'$lgt_valid_pred_call_example'(Description - Call - {Bindings}) :-
	atom(Description),
	callable(Call),
	nonvar(Bindings),
	(atom(Bindings) ->
		once((Bindings == no; Bindings == yes))
		;
		'$lgt_valid_example_var_bindings'(Bindings)).



% '$lgt_valid_pred_call_example'(@term, +atom, +integer)
%
% valid predicate call example documentation on info/2 directive

'$lgt_valid_pred_call_example'((Description - Call - {Bindings}), Functor, Arity) :-
	atom(Description),
	nonvar(Call),
	functor(Pred, Functor, Arity),
	Call = Pred,
	nonvar(Bindings),
	(atom(Bindings) ->
		once((Bindings == no; Bindings == yes))
		;
		'$lgt_valid_example_var_bindings'(Bindings)).



'$lgt_valid_example_var_bindings'((Binding, Bindings)) :-
	!,
	'$lgt_valid_example_var_binding'(Binding),
	'$lgt_valid_example_var_bindings'(Bindings).

'$lgt_valid_example_var_bindings'(Binding) :-
	'$lgt_valid_example_var_binding'(Binding).


'$lgt_valid_example_var_binding'(Binding) :-
	nonvar(Binding),
	Binding = (Var = _),
	var(Var).



% '$lgt_xml_encoding'(-atom)
%
% returns the text encoding that should be used on the XML documenting file

'$lgt_xml_encoding'(XMLEncoding) :-
	'$lgt_pp_directive_'(encoding(Encoding)) ->
		'$lgt_xml_encoding_table'(Encoding, XMLEncoding)
		;
		XMLEncoding = 'utf-8'.



% '$lgt_xml_encoding_table'(?atom, ?atom)
%
% converts between Prolog stream encoding names and XML encoding names

'$lgt_xml_encoding_table'(ascii, 'us-ascii') :-
	!.
'$lgt_xml_encoding_table'(iso_8859_1, 'iso-8859-1') :-
	!.
'$lgt_xml_encoding_table'(iso_latin_1, 'iso-8859-1') :-
	!.
'$lgt_xml_encoding_table'(utf8, 'utf-8') :-
	!.
'$lgt_xml_encoding_table'(unicode_be, 'utf-16') :-
	!.
'$lgt_xml_encoding_table'(unicode_le, 'utf-16') :-
	!.
'$lgt_xml_encoding_table'(Encoding, Encoding).



% Logtalk built-in predicates
%
% '$lgt_lgt_built_in'(?callable)

'$lgt_lgt_built_in'(::(_, _)).

'$lgt_lgt_built_in'(forall(_, _)).
'$lgt_lgt_built_in'(retractall(_)).

'$lgt_lgt_built_in'(logtalk_compile(_)).
'$lgt_lgt_built_in'(logtalk_compile(_, _)).
'$lgt_lgt_built_in'(logtalk_load(_)).
'$lgt_lgt_built_in'(logtalk_load(_, _)).

'$lgt_lgt_built_in'(protocol_property(_, _)).
'$lgt_lgt_built_in'(category_property(_, _)).
'$lgt_lgt_built_in'(object_property(_, _)).

'$lgt_lgt_built_in'(current_protocol(_)).
'$lgt_lgt_built_in'(current_category(_)).
'$lgt_lgt_built_in'(current_object(_)).

'$lgt_lgt_built_in'(create_object(_, _, _, _)).
'$lgt_lgt_built_in'(create_category(_, _, _, _)).
'$lgt_lgt_built_in'(create_protocol(_, _, _)).

'$lgt_lgt_built_in'(abolish_object(_)).
'$lgt_lgt_built_in'(abolish_category(_)).
'$lgt_lgt_built_in'(abolish_protocol(_)).

'$lgt_lgt_built_in'(implements_protocol(_, _)).
'$lgt_lgt_built_in'(implements_protocol(_, _, _)).
'$lgt_lgt_built_in'(imports_category(_, _)).
'$lgt_lgt_built_in'(imports_category(_, _, _)).
'$lgt_lgt_built_in'(instantiates_class(_, _)).
'$lgt_lgt_built_in'(instantiates_class(_, _, _)).
'$lgt_lgt_built_in'(specializes_class(_, _)).
'$lgt_lgt_built_in'(specializes_class(_, _, _)).
'$lgt_lgt_built_in'(extends_protocol(_, _)).
'$lgt_lgt_built_in'(extends_protocol(_, _, _)).
'$lgt_lgt_built_in'(extends_object(_, _)).
'$lgt_lgt_built_in'(extends_object(_, _, _)).

'$lgt_lgt_built_in'(abolish_events(_, _, _, _, _)).
'$lgt_lgt_built_in'(define_events(_, _, _, _, _)).
'$lgt_lgt_built_in'(current_event(_, _, _, _, _)).

'$lgt_lgt_built_in'(current_logtalk_flag(_, _)).
'$lgt_lgt_built_in'(set_logtalk_flag(_, _)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  DCG rule conversion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_dcgrule_to_clause'(@dcgrule, -clause)
%
% converts a DCG rule to a normal clause


'$lgt_dcgrule_to_clause'(Rule, Clause) :-
	catch(
		'$lgt_dcg_rule'(Rule, Clause),
		Error,
		throw(error(Error, dcgrule(Rule)))).



% '$lgt_dcg_rule'(@dcgrule, -clause)
%
% converts a DCG rule to a normal clause

'$lgt_dcg_rule'((RHead --> RBody), CHead) :-
	RBody == [],
	!,
	'$lgt_dcg_head'(RHead, CHead, _, _, S, S, _).

'$lgt_dcg_rule'((RHead --> RBody), (CHead :- CBody)) :-
	'$lgt_dcg_head'(RHead, CHead, Body, Body2, S0, S1, S),
	'$lgt_dcg_body'(RBody, Body, S0, S1),
	'$lgt_dcg_fold_unifications'(Body2, CBody, S0, S).



% '$lgt_dcg_head'(@dcghead, -head, @goal, -goal, @var, @var, @var)
%
% translates DCG rule head to a Prolog clause head
% (the last argument returns the variable representing the ouput list)

'$lgt_dcg_head'(RHead, _, _, _, _, _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_head'((_, Terminals), _, _, _, _, _, _) :-
	\+ '$lgt_proper_list'(Terminals),
	throw(type_error(list, Terminals)).

'$lgt_dcg_head'((RHead, Terminals), CHead, Body, (Body,Goal), S0, S1, S) :-
	!,
	'$lgt_dcg_goal'(RHead, CHead, S0, S),
	'$lgt_dcg_terminals'(Terminals, Goal, S, S1).

'$lgt_dcg_head'(RHead, CHead, Body, Body, S0, S, S) :-
	'$lgt_dcg_goal'(RHead, CHead, S0, S).



% '$lgt_dcg_body'(@dcgbody, -body, @var, @var)
%
% translates DCG rule body to a Prolog clause body

'$lgt_dcg_body'(Var, phrase(Var, S0, S), S0, S) :-
	var(Var),
	!.

'$lgt_dcg_body'(::RGoal, ::CGoal, S0, S) :-
	!,
	'$lgt_dcg_body'(RGoal, CGoal, S0, S).

'$lgt_dcg_body'(Object::RGoal, Object::CGoal, S0, S) :-
	!,
	'$lgt_dcg_body'(RGoal, CGoal, S0, S).

'$lgt_dcg_body'((RGoal,RGoals), (CGoal,CGoals), S0, S) :-
	!,
	'$lgt_dcg_body'(RGoal, CGoal, S0, S1),
	'$lgt_dcg_body'(RGoals, CGoals, S1, S).

'$lgt_dcg_body'((RGoal1 -> RGoal2), (CGoal1 -> CGoal2), S0, S) :-
	!,
	'$lgt_dcg_body'(RGoal1, CGoal1, S0, S1),
	'$lgt_dcg_body'(RGoal2, CGoal2, S1, S).

'$lgt_dcg_body'((RGoal1;RGoal2), (CGoal1;CGoal2), S0, S) :-
	!,
	'$lgt_dcg_body'(RGoal1, CGoal1, S0, S),
	'$lgt_dcg_body'(RGoal2, CGoal2, S0, S).

'$lgt_dcg_body'({Goal}, (CGoal, S0=S), S0, S) :-
	!,
	(var(Goal) -> CGoal = call(Goal); CGoal = Goal).

'$lgt_dcg_body'(!, (!, S0=S), S0, S) :-
	!.

'$lgt_dcg_body'(\+ RGoal, \+ CGoal, S0, S0) :-
	!,
	'$lgt_dcg_body'(RGoal, CGoal, S0, _).

'$lgt_dcg_body'([], (S0=S), S0, S) :-
	!.

'$lgt_dcg_body'([Terminal| Terminals], CGoal, S0, S) :-
	!,
	'$lgt_dcg_terminals'([Terminal| Terminals], CGoal, S0, S).

'$lgt_dcg_body'(RGoal, CGoal, S0, S) :-
	'$lgt_dcg_goal'(RGoal, CGoal, S0, S).



% '$lgt_dcg_goal'(@goal, -goal, @var, @var)
%
% translates DCG goal to Prolog goal

'$lgt_dcg_goal'(RGoal, _, _, _) :-
	\+ callable(RGoal),
	throw(type_error(callable, RGoal)).

'$lgt_dcg_goal'(RGoal, CGoal, S0, S) :-
	RGoal =.. RList,
	'$lgt_append'(RList, [S0, S], CList),
	CGoal =.. CList.



% '$lgt_dcg_terminals'(+list, -goal, @var, @var)
%
% translate list of terminals

'$lgt_dcg_terminals'(Terminals, S0=List, S0, S) :-
	'$lgt_dcg_terminals'(Terminals, S, List).


'$lgt_dcg_terminals'([], S, S) :-
	!.		% make predicate determinist when first argument is [Var1| Var2]

'$lgt_dcg_terminals'([Terminal| Terminals], S, [Terminal| Rest]) :-
	'$lgt_dcg_terminals'(Terminals, S, Rest).



% '$lgt_dcg_fold_unifications'(+goal, -goal, @var, @var)
%
% folds redundant calls to =/2 by calling the unification
% goals except for output unifications

'$lgt_dcg_fold_unifications'((Goal1 -> Goal2), (SGoal1 -> SGoal2), S0, S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S0, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S0, S).

'$lgt_dcg_fold_unifications'((Goal1;Goal2), (SGoal1;SGoal2), S0, S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S0, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S0, S).

'$lgt_dcg_fold_unifications'((Goal1,Goal2), SGoal, S0, S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S0, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S0, S),
	'$lgt_dcg_simplify_and'((SGoal1,SGoal2), SGoal).

'$lgt_dcg_fold_unifications'(S1=S2, S1=S2, _, S) :-
	(S1 == S; S2 == S),		% avoid output unifications
	!.

'$lgt_dcg_fold_unifications'(S1=S2, true, _, _) :-
	var(S2),				% avoid unification with list of terminals
	!,
	S1 = S2.

'$lgt_dcg_fold_unifications'(Goal, Goal, _, _).



% '$lgt_dcg_simplify_and'(+goal, -goal)
%
% removes redundant calls to true/0 and flats conjunction of goals

'$lgt_dcg_simplify_and'((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	'$lgt_dcg_simplify_and'(Goal1, SGoal1),
	'$lgt_dcg_simplify_and'(Goal2, SGoal2).

'$lgt_dcg_simplify_and'((Goal1;Goal2), (SGoal1;SGoal2)) :-
	!,
	'$lgt_dcg_simplify_and'(Goal1, SGoal1),
	'$lgt_dcg_simplify_and'(Goal2, SGoal2).

'$lgt_dcg_simplify_and'(((Goal1,Goal2),Goal3), Body) :-
	!,
	'$lgt_dcg_simplify_and'((Goal1,(Goal2,Goal3)), Body).

'$lgt_dcg_simplify_and'((true,Goal), Body) :-
	!,
	'$lgt_dcg_simplify_and'(Goal, Body).

'$lgt_dcg_simplify_and'((Goal,true), Body) :-
	!,
	'$lgt_dcg_simplify_and'(Goal, Body).

'$lgt_dcg_simplify_and'((Goal1,Goal2), (Goal1,Goal3)) :-
	!,
	'$lgt_dcg_simplify_and'(Goal2, Goal3).

'$lgt_dcg_simplify_and'(\+ Goal, \+ SGoal) :-
	!,
	'$lgt_dcg_simplify_and'(Goal, SGoal).

'$lgt_dcg_simplify_and'(Goal, Goal).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  xml
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_write_xml_file'(+stream)
%
% writes a XML file containing the documentation of a compiled entity

'$lgt_write_xml_file'(Stream) :-
	'$lgt_write_xml_header'(Stream),
	'$lgt_write_xml_entity'(Stream),
	'$lgt_write_xml_relations'(Stream),
	'$lgt_write_xml_predicates'(Stream),
	'$lgt_write_xml_remarks'(Stream),
	'$lgt_write_xml_footer'(Stream).



'$lgt_write_xml_header'(Stream) :-
	'$lgt_compiler_flag'(xmlspec, XMLSpec),
	'$lgt_compiler_flag'(doctype, Doctype),
	'$lgt_write_xml_header'(Doctype, XMLSpec, Stream).



'$lgt_write_xml_header'(local, XMLSpec, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, no, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	(XMLSpec = dtd ->
		write(Stream, '<!DOCTYPE logtalk SYSTEM "logtalk.dtd">'), nl(Stream)
		;
		true),
	'$lgt_compiler_flag'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	(XMLSpec = dtd ->
		'$lgt_write_xml_open_tag'(Stream, logtalk, [])
		;
		'$lgt_write_xml_open_tag'(Stream, logtalk,
			['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
			 'xsi:noNamespaceSchemaLocation'-'logtalk.xsd'])).

'$lgt_write_xml_header'(web, XMLSpec, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, no, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	(XMLSpec = dtd ->
		write(Stream, '<!DOCTYPE logtalk SYSTEM "http://www.logtalk.org/xml/1.3/logtalk.dtd">'), nl(Stream)
		;
		true),
	'$lgt_compiler_flag'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	(XMLSpec = dtd ->
		'$lgt_write_xml_open_tag'(Stream, logtalk, [])
		;
		'$lgt_write_xml_open_tag'(Stream, logtalk,
			['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
			 'xsi:noNamespaceSchemaLocation'-'http://www.logtalk.org/xml/1.3/logtalk.xsd'])).

'$lgt_write_xml_header'(standalone, _, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, yes, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	'$lgt_compiler_flag'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	'$lgt_write_xml_open_tag'(Stream, logtalk, []).


'$lgt_xml_header_text'(Version, Encoding, Standalone, Text) :-
	atom_concat('?xml version="', Version, Aux1),
	atom_concat(Aux1, '" encoding="', Aux2),
	atom_concat(Aux2, Encoding, Aux3),
	atom_concat(Aux3, '" standalone="', Aux4),
	atom_concat(Aux4, Standalone, Aux5),
	atom_concat(Aux5, '"?', Text).



'$lgt_write_xml_footer'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, logtalk).



'$lgt_write_xml_entity'(Stream) :-
	'$lgt_pp_entity'(Type, Entity, _, _, Compilation),
	'$lgt_write_xml_open_tag'(Stream, entity, []),
	'$lgt_entity_to_xml_term'(Entity),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Entity),
	'$lgt_write_xml_element'(Stream, (type), [], Type),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation),
	('$lgt_pp_info_'(Info) ->
		('$lgt_member'(comment is Comment, Info) ->
			'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
			;
			true), 
		('$lgt_member'(parameters is Parameters, Info) ->
			'$lgt_write_xml_open_tag'(Stream, parameters, []),
			forall(
				'$lgt_member'(Parname-Description, Parameters),
		 		('$lgt_write_xml_open_tag'(Stream, parameter, []),
		 		 '$lgt_write_xml_cdata_element'(Stream, name, [], Parname),
		 		 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
		 		 '$lgt_write_xml_close_tag'(Stream, parameter))),
		 	'$lgt_write_xml_close_tag'(Stream, parameters)
			;
			true),
		('$lgt_member'(author is Author, Info) ->
			'$lgt_write_xml_cdata_element'(Stream, author, [], Author)
			;
			true), 
		('$lgt_member'(version is Version, Info) ->
			'$lgt_write_xml_element'(Stream, version, [], Version)
			;
			true), 
		('$lgt_member'(date is Date, Info) ->
			'$lgt_write_xml_element'(Stream, date, [], Date)
			;
			true),
		forall(
			('$lgt_member'(Key is Value, Info),
			 \+ '$lgt_member'(Key, [comment, author, version, date, parameters, parnames, remarks])),
			('$lgt_write_xml_open_tag'(Stream, info, []),
			 '$lgt_write_xml_element'(Stream, key, [], Key),
			 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
			 '$lgt_write_xml_close_tag'(Stream, info)))
		;
		true),
	'$lgt_write_xml_close_tag'(Stream, entity).



% '$lgt_entity_to_xml_term'(+entity)
%
% instantiates the parameters in a parametric object to
% user defined names or to the atom '_'

'$lgt_entity_to_xml_term'(Entity) :-
	'$lgt_pp_info_'(List),
	('$lgt_member'(parnames is Names, List) ->
		true
		;
		'$lgt_member'(parameters is Parameters, List),
		findall(Name, '$lgt_member'(Name - _, Parameters), Names)),
	!,
	Entity =.. [_| Args],
	'$lgt_vars_to_atoms'(Args, Args, Names).

'$lgt_entity_to_xml_term'(Entity) :-
	Entity =.. [_| Args],
	'$lgt_vars_to_underscore'(Args).



% '$lgt_relation_to_xml_term'(+entity, +entity)
%
% instantiates the parameters in a related entity taking
% in account the parameter sharing with the original entity

'$lgt_relation_to_xml_term'(Entity, Relation) :-
	'$lgt_entity_to_xml_term'(Entity),
	Relation =.. [_| Args],
	'$lgt_vars_to_underscore'(Args).



% '$lgt_pred_call_to_xml_term'(+atom, +integer, +nonvar, +nonvar, -nonvar, -nonvar)
%
% instantiates the arguments in a predicate call to user defined names or to the atom '_'

'$lgt_pred_call_to_xml_term'(Functor, Arity, Call, Bindings, QCall, QBindings) :-
	'$lgt_double_quote_atoms'(Call, QCall),
	'$lgt_double_quote_atoms'(Bindings, QBindings),
	'$lgt_pred_qcall_to_xml_term'(Functor, Arity, QCall, QBindings).


'$lgt_pred_qcall_to_xml_term'(Functor, Arity, Call, Bindings) :-
	once((
		'$lgt_pp_info_'(Functor/Arity, List)
		;
		'$lgt_pp_info_'(Functor//Arity, List))),
	('$lgt_member'(argnames is Names, List) ->
		true
		;
		'$lgt_member'(arguments is Arguments, List),
		findall(Name, '$lgt_member'(Name - _, Arguments), Names)),
	!,
	Call =.. [Functor| Args],
	'$lgt_binding_vars'(Bindings, Vars),
	'$lgt_vars_to_atoms'(Args, Vars, Names).

'$lgt_pred_qcall_to_xml_term'(Functor, _, Call, _) :-
	Call =.. [Functor| Args],
	'$lgt_vars_to_underscore'(Args).



'$lgt_double_quote_atoms'(Var, Var) :-
	var(Var),
	!.

'$lgt_double_quote_atoms'((Call1, Call2), (QCall1, QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'((Call1; Call2), (QCall1; QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'((Call1 -> Call2), (QCall1 -> QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'(\+ Call, \+ QCall) :-
	!,
	'$lgt_double_quote_atoms'(Call, QCall).

'$lgt_double_quote_atoms'([], []) :-
	!.

'$lgt_double_quote_atoms'([Arg| Args], [QArg| QArgs]) :-
	!,
	'$lgt_double_quote_atoms'(Arg, QArg),
	'$lgt_double_quote_atoms'(Args, QArgs).

'$lgt_double_quote_atoms'(Atom, QAtom) :-
	atom(Atom),
	!,
	('$lgt_atom_needs_quotes'(Atom) ->
		atom_concat('''', Atom, Aux),
		atom_concat(Aux, '''', QAtom)
		;
		Atom = QAtom).

'$lgt_double_quote_atoms'(Number, Number) :-
	number(Number),
	!.

'$lgt_double_quote_atoms'(Term, QTerm) :-
	Term =.. [Functor| Args],
	('$lgt_built_in'(Term) ->
		QFunctor = Functor
		;
		'$lgt_double_quote_atoms'(Functor, QFunctor)),
	'$lgt_double_quote_atoms'(Args, QArgs),
	QTerm =.. [QFunctor| QArgs].


'$lgt_atom_needs_quotes'(Atom) :-
	atom_chars(Atom, [First| Rest]),
	once((
		First @< a;
		First @> z;
		'$lgt_member'(Char, Rest),
		\+ (Char == '_'; Char @>= a, Char @=< z; Char @>= 'A', Char @=< 'Z'; Char @>= '0', Char @=< '9'))).



% '$lgt_binding_vars'(@nonvar, -list)
%
% returns a list of all binding variables

'$lgt_binding_vars'(Bindings, Vars) :-
	atom(Bindings) ->		% no bindings, just "no", "yes", or equivalent answers
		Vars = []
		;
		'$lgt_binding_vars_list'(Bindings, Vars).


'$lgt_binding_vars_list'((Var = _), [Var]).
	
'$lgt_binding_vars_list'(((Var = _), Bindings), [Var| Vars]) :-
	'$lgt_binding_vars_list'(Bindings, Vars).



% '$lgt_vars_to_atoms'(+list, +list, +list)
%
% instantiates the variables in the input list to either a name or the atom '_'

'$lgt_vars_to_atoms'([], _, []).

'$lgt_vars_to_atoms'([Arg| Args], Vars, [Name| Names]) :-
	(var(Arg) ->
		('$lgt_member_var'(Arg, Vars) ->
			Arg = Name
			;
			Arg = '_')
		;
		true),
	'$lgt_vars_to_atoms'(Args, Vars, Names).



% '$lgt_vars_to_underscore'(+list)
%
% instantiates the variables in the input list to the atom '_'

'$lgt_vars_to_underscore'([]).

'$lgt_vars_to_underscore'([Arg| Args]) :-
	(var(Arg) -> Arg = '_'; true),
	'$lgt_vars_to_underscore'(Args).



% '$lgt_relation_to_xml_filename'(+entity, -atom)
%
% needed to build filenames in links to parametric objects

'$lgt_relation_to_xml_filename'(Relation, File) :-
	functor(Relation, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, '_', Aux),
	atom_concat(Aux, Atom, File).



% '$lgt_write_xml_predicates'(+stream)
%
% writes the predicate documentation

'$lgt_write_xml_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, predicates, []),
	'$lgt_write_xml_public_predicates'(Stream),
	'$lgt_write_xml_protected_predicates'(Stream),
	'$lgt_write_xml_private_predicates'(Stream),
	'$lgt_write_xml_close_tag'(Stream, predicates).



% '$lgt_write_xml_public_predicates'(+stream)
%
% writes the documentation of public predicates

'$lgt_write_xml_public_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, (public), []),
	'$lgt_pp_public_'(Functor, Arity),
	('$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, (public))
		;
		'$lgt_write_xml_predicate'(Stream, Functor, Arity, (public))),
	fail.

'$lgt_write_xml_public_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, (public)).



% '$lgt_write_xml_protected_predicates'(+stream)
%
% writes the documentation protected predicates

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, protected, []),
	'$lgt_pp_protected_'(Functor, Arity),
	('$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, protected)
		;
		'$lgt_write_xml_predicate'(Stream, Functor, Arity, protected)),
	fail.

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, protected).



% '$lgt_write_xml_private_predicates'(+stream)
%
% writes the documentation of private predicates

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, private, []),
	'$lgt_pp_private_'(Functor, Arity),
	('$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, private)
		;
		'$lgt_write_xml_predicate'(Stream, Functor, Arity, private)),
	fail.

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, private).



% '$lgt_write_xml_predicate'(+stream, +atom, +integer, +term)
%
% writes the documentation of a predicate

'$lgt_write_xml_predicate'(Stream, Functor, Arity, Scope) :-
	'$lgt_write_xml_open_tag'(Stream, predicate, []),
	'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Functor/Arity, Scope),
	'$lgt_write_xml_predicate_meta'(Stream, Functor, Arity),
	'$lgt_write_xml_predicate_mode'(Stream, Functor, Arity),
	('$lgt_pp_info_'(Functor/Arity, Info) ->
		'$lgt_write_xml_predicate_info'(Stream, Functor, Arity, Info)
		;
		true),
	'$lgt_write_xml_close_tag'(Stream, predicate).


'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Name, Scope) :-
	'$lgt_write_xml_cdata_element'(Stream, name, [], Name),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	(('$lgt_pp_entity'(_, _, _, _, (dynamic)); '$lgt_pp_dynamic_'(Functor, Arity)) ->
		Compilation = (dynamic)
		;
		Compilation = static),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation).


'$lgt_write_xml_predicate_meta'(Stream, Functor, Arity) :-
	functor(Meta, Functor, Arity),
	('$lgt_pp_metapredicate_'(Meta) ->
		'$lgt_write_xml_cdata_element'(Stream, meta, [], Meta)
		;
		true).


'$lgt_write_xml_predicate_mode'(Stream, Functor, Arity) :-
	functor(Template, Functor, Arity),
	forall(
		'$lgt_pp_mode_'(Template, Solutions),
		('$lgt_write_xml_open_tag'(Stream, (mode), []),
		 '$lgt_write_xml_cdata_element'(Stream, template, [], Template),
		 '$lgt_write_xml_element'(Stream, solutions, [], Solutions),
		 '$lgt_write_xml_close_tag'(Stream, (mode)))).


'$lgt_write_xml_predicate_info'(Stream, Functor, Arity, Info) :-
	('$lgt_member'(comment is Comment, Info) ->
		'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
		;
		true),
	('$lgt_member'(arguments is Arguments, Info) ->
		findall(Name, '$lgt_member'(Name - _, Arguments), Names),
		Template =.. [Functor| Names],
		'$lgt_write_xml_cdata_element'(Stream, template, [], Template),
		'$lgt_write_xml_open_tag'(Stream, arguments, []),
		forall(
			'$lgt_member'(Name-Description, Arguments),
		 	('$lgt_write_xml_open_tag'(Stream, argument, []),
	 		 '$lgt_write_xml_cdata_element'(Stream, name, [], Name),
		 	 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
		 	 '$lgt_write_xml_close_tag'(Stream, argument))),
		 '$lgt_write_xml_close_tag'(Stream, arguments)
		;
		true),
	('$lgt_member'(argnames is Names, Info) ->
		Template =.. [Functor| Names],
		'$lgt_write_xml_cdata_element'(Stream, template, [], Template)
		;
		true),
	('$lgt_member'(exceptions is Exceptions, Info) ->
		'$lgt_write_xml_open_tag'(Stream, exceptions, []),
		forall(
			'$lgt_member'(Cond-Term, Exceptions),
		 	('$lgt_write_xml_open_tag'(Stream, exception, []),
		 	 '$lgt_write_xml_cdata_element'(Stream, condition, [], Cond),
		 	 '$lgt_write_xml_cdata_element'(Stream, term, [], Term),
			 '$lgt_write_xml_close_tag'(Stream, exception))),
		 '$lgt_write_xml_close_tag'(Stream, exceptions)
		;
		true),
	forall(
		('$lgt_member'(Key is Value, Info),
		 \+ '$lgt_member'(Key, [comment, arguments, argnames, exceptions, examples])),
		('$lgt_write_xml_open_tag'(Stream, info, []),
		 '$lgt_write_xml_element'(Stream, key, [], Key),
		 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
		 '$lgt_write_xml_close_tag'(Stream, info))),
	('$lgt_member'(examples is Examples, Info) ->
		'$lgt_write_xml_open_tag'(Stream, examples, []),
		forall(
			'$lgt_member'((Description - Call - {Bindings}), Examples),
			('$lgt_pred_call_to_xml_term'(Functor, Arity, Call, Bindings, QCall, QBindings),
			 '$lgt_write_xml_open_tag'(Stream, example, []),
			 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
			 '$lgt_write_xml_cdata_element'(Stream, call, [], QCall),
			 '$lgt_write_xml_cdata_element'(Stream, bindings, [], QBindings),
		 	 '$lgt_write_xml_close_tag'(Stream, example))),
		'$lgt_write_xml_close_tag'(Stream, examples)
		;
		true).



% '$lgt_write_xml_non_terminal'(+stream, +atom, +atom, +integer, +term)
%
% writes the documentation of a grammar rule non-terminal

'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, Scope) :-
	'$lgt_write_xml_open_tag'(Stream, predicate, []),
	'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Functor//Args, Scope),
	'$lgt_write_xml_predicate_mode'(Stream, Functor, Args),
	('$lgt_pp_info_'(Functor//Args, Info) ->
		'$lgt_write_xml_predicate_info'(Stream, Functor, Args, Info)
		;
		true),
	'$lgt_write_xml_close_tag'(Stream, predicate).



'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, relations, []),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_implements_protocol_'(Entity, Ptc, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, implements, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_imports_category_'(Entity, Ctg, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ctg, imports, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_extends_object_'(Entity, Parent, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Parent, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_instantiates_class_'(Entity, Class, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Class, instantiates, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_specializes_class_'(Entity, Superclass, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Superclass, specializes, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_rclause_'('$lgt_extends_protocol_'(Entity, Ptc, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
	'$lgt_pp_uses_'(Obj),
	'$lgt_write_xml_relation'(Stream, Entity, Obj, uses),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
	'$lgt_pp_calls_'(Ptc),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, calls),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, relations).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag, Scope) :-
	'$lgt_relation_to_xml_term'(Entity, Relation),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Relation),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag) :-
	'$lgt_relation_to_xml_term'(Entity, Relation),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Relation),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).


'$lgt_write_xml_remarks'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, remarks, []),
	(('$lgt_pp_info_'(Info), '$lgt_member'(remarks is Remarks, Info)) ->
		forall(
			'$lgt_member'((Topic - Text), Remarks),
			('$lgt_write_xml_open_tag'(Stream, remark, []),
			 '$lgt_write_xml_cdata_element'(Stream, topic, [], Topic),
			 '$lgt_write_xml_cdata_element'(Stream, text, [], Text),
		 	 '$lgt_write_xml_close_tag'(Stream, remark)))
		;
		true),
	'$lgt_write_xml_close_tag'(Stream, remarks).



% '$lgt_write_xml_open_tag'(@stream, @atom, @list)
%
% writes <Tag Att1="V1" Att2="V2" ...>

'$lgt_write_xml_open_tag'(Stream, Tag, Atts) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...>Text</Tag>

'$lgt_write_xml_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '>'),
	write(Stream, Text),
	write(Stream, '</'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_writeq_xml_cdata_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag> (quoted)

'$lgt_writeq_xml_cdata_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '><![CDATA['),
	'$lgt_pretty_print_vars_quoted'(Stream, Text),
	write(Stream, ']]></'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_cdata_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag>

'$lgt_write_xml_cdata_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '><![CDATA['),
	'$lgt_pretty_print_vars'(Stream, Text),
	write(Stream, ']]></'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_tag_attributes'(@stream, @list)

'$lgt_write_xml_tag_attributes'(_, []) :-
	!.

'$lgt_write_xml_tag_attributes'(Stream, [Attribute-Value| Rest]) :-
	write(Stream, ' '),
	write(Stream, Attribute),
	write(Stream, '="'),
	write(Stream, Value),
	write(Stream, '"'),
	'$lgt_write_xml_tag_attributes'(Stream, Rest).



% '$lgt_write_xml_close_tag'(@stream, @atom)
%
% writes </Tag>

'$lgt_write_xml_close_tag'(Stream, Tag) :-
	write(Stream, '</'),
	write(Stream, Tag),
	write(Stream, '>'),
	nl(Stream).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO specified predicates
%
%  used for portability checking
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_iso_spec_pred'(true).
'$lgt_iso_spec_pred'(fail).
'$lgt_iso_spec_pred'(call(_)).
'$lgt_iso_spec_pred'(!).
'$lgt_iso_spec_pred'((_; _)).
'$lgt_iso_spec_pred'((_, _)).
'$lgt_iso_spec_pred'((_ -> _)).
'$lgt_iso_spec_pred'((_ -> _; _)).
'$lgt_iso_spec_pred'(catch(_, _, _)).
'$lgt_iso_spec_pred'(throw(_)).

'$lgt_iso_spec_pred'((_ = _)).
'$lgt_iso_spec_pred'((_ \= _)).
'$lgt_iso_spec_pred'(unify_with_occurs_check(_, _)).

'$lgt_iso_spec_pred'(var(_)).
'$lgt_iso_spec_pred'(nonvar(_)).
'$lgt_iso_spec_pred'(atom(_)).
'$lgt_iso_spec_pred'(atomic(_)).
'$lgt_iso_spec_pred'(number(_)).
'$lgt_iso_spec_pred'(integer(_)).
'$lgt_iso_spec_pred'(float(_)).
'$lgt_iso_spec_pred'(compound(_)).

'$lgt_iso_spec_pred'((_ @=< _)).
'$lgt_iso_spec_pred'((_ @< _)).
'$lgt_iso_spec_pred'((_ @>= _)).
'$lgt_iso_spec_pred'((_ @> _)).
'$lgt_iso_spec_pred'((_ == _)).
'$lgt_iso_spec_pred'((_ \== _)).

'$lgt_iso_spec_pred'(functor(_, _, _)).
'$lgt_iso_spec_pred'(arg(_, _, _)).
'$lgt_iso_spec_pred'(_ =.. _).
'$lgt_iso_spec_pred'(copy_term(_, _)).

'$lgt_iso_spec_pred'(_ is _).

'$lgt_iso_spec_pred'((_ =< _)).
'$lgt_iso_spec_pred'((_ < _)).
'$lgt_iso_spec_pred'((_ >= _)).
'$lgt_iso_spec_pred'((_ > _)).
'$lgt_iso_spec_pred'((_ =:= _)).
'$lgt_iso_spec_pred'((_ =\= _)).

'$lgt_iso_spec_pred'(clause(_, _)).
'$lgt_iso_spec_pred'(current_predicate(_)).

'$lgt_iso_spec_pred'(asserta(_)).
'$lgt_iso_spec_pred'(assertz(_)).
'$lgt_iso_spec_pred'(retract(_)).
'$lgt_iso_spec_pred'(abolish(_)).

'$lgt_iso_spec_pred'(findall(_, _, _)).
'$lgt_iso_spec_pred'(bagof(_, _, _)).
'$lgt_iso_spec_pred'(setof(_, _, _)).

'$lgt_iso_spec_pred'(current_input(_)).
'$lgt_iso_spec_pred'(current_output(_)).
'$lgt_iso_spec_pred'(set_input(_)).
'$lgt_iso_spec_pred'(set_output(_)).
'$lgt_iso_spec_pred'(open(_, _, _, _)).
'$lgt_iso_spec_pred'(open(_, _, _)).
'$lgt_iso_spec_pred'(close(_, _)).
'$lgt_iso_spec_pred'(close(_)).
'$lgt_iso_spec_pred'(flush_output(_)).
'$lgt_iso_spec_pred'(flush_output).
'$lgt_iso_spec_pred'(stream_property(_, _)).
'$lgt_iso_spec_pred'(at_end_of_stream).
'$lgt_iso_spec_pred'(at_end_of_stream(_)).
'$lgt_iso_spec_pred'(set_stream_position(_, _)).

'$lgt_iso_spec_pred'(get_char(_, _)).
'$lgt_iso_spec_pred'(get_char(_)).
'$lgt_iso_spec_pred'(get_code(_, _)).
'$lgt_iso_spec_pred'(get_code(_)).
'$lgt_iso_spec_pred'(peek_char(_, _)).
'$lgt_iso_spec_pred'(peek_char(_)).
'$lgt_iso_spec_pred'(peek_code(_, _)).
'$lgt_iso_spec_pred'(peek_code(_)).
'$lgt_iso_spec_pred'(put_char(_, _)).
'$lgt_iso_spec_pred'(put_char(_)).
'$lgt_iso_spec_pred'(put_code(_, _)).
'$lgt_iso_spec_pred'(put_code(_)).
'$lgt_iso_spec_pred'(nl).
'$lgt_iso_spec_pred'(nl(_)).

'$lgt_iso_spec_pred'(get_byte(_, _)).
'$lgt_iso_spec_pred'(get_byte(_)).
'$lgt_iso_spec_pred'(peek_byte(_, _)).
'$lgt_iso_spec_pred'(peek_byte(_)).
'$lgt_iso_spec_pred'(put_byte(_, _)).
'$lgt_iso_spec_pred'(put_byte(_)).

'$lgt_iso_spec_pred'(read_term(_, _, _)).
'$lgt_iso_spec_pred'(read_term(_, _)).
'$lgt_iso_spec_pred'(read(_)).
'$lgt_iso_spec_pred'(read(_, _)).
'$lgt_iso_spec_pred'(write_term(_, _, _)).
'$lgt_iso_spec_pred'(write_term(_, _)).
'$lgt_iso_spec_pred'(write(_)).
'$lgt_iso_spec_pred'(write(_, _)).
'$lgt_iso_spec_pred'(writeq(_)).
'$lgt_iso_spec_pred'(writeq(_, _)).
'$lgt_iso_spec_pred'(write_canonical(_)).
'$lgt_iso_spec_pred'(write_canonical(_, _)).
'$lgt_iso_spec_pred'(op(_, _, _)).
'$lgt_iso_spec_pred'(current_op(_, _, _)).
'$lgt_iso_spec_pred'(char_conversion(_, _)).
'$lgt_iso_spec_pred'(current_char_conversion(_, _)).

'$lgt_iso_spec_pred'(\+ _).
'$lgt_iso_spec_pred'(once(_)).
'$lgt_iso_spec_pred'(repeat).

'$lgt_iso_spec_pred'(atom_length(_, _)).
'$lgt_iso_spec_pred'(atom_concat(_, _, _)).
'$lgt_iso_spec_pred'(sub_atom(_, _, _, _, _)).
'$lgt_iso_spec_pred'(atom_chars(_, _)).
'$lgt_iso_spec_pred'(atom_codes(_, _)).
'$lgt_iso_spec_pred'(char_code(_, _)).
'$lgt_iso_spec_pred'(number_chars(_, _)).
'$lgt_iso_spec_pred'(number_codes(_, _)).

'$lgt_iso_spec_pred'(set_prolog_flag(_, _)).
'$lgt_iso_spec_pred'(current_prolog_flag(_, _)).
'$lgt_iso_spec_pred'(halt).
'$lgt_iso_spec_pred'(halt(_)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk banner
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_startup_message' :-
	'$lgt_default_flag'(startup_message, Flag),
	'$lgt_startup_message'(Flag).


'$lgt_startup_message'(flags) :-
	'$lgt_banner',
	'$lgt_default_flags'.

'$lgt_startup_message'(banner) :-
	'$lgt_banner'.

'$lgt_startup_message'(none).



'$lgt_banner' :-
	current_logtalk_flag(version, version(Major, Minor, Patch)),
	nl, write('Logtalk '), write(Major), write('.'), write(Minor), write('.'), write(Patch), nl,
	write('Copyright (c) 1998-2005 Paulo Moura'), nl, nl.



'$lgt_default_flags' :-
	write('Default lint compilation flags:'), nl,
	'$lgt_default_flag'(unknown, Unknown),
	write('  Unknown entities (unknown):                               '), write(Unknown), nl,
	'$lgt_default_flag'(misspelt, Misspelt),
	write('  Misspelt predicate calls (misspelt):                      '), write(Misspelt), nl,
	'$lgt_default_flag'(lgtredef, Lgtredef),
	write('  Logtalk built-in predicates redefinition (lgtredef):      '), write(Lgtredef), nl,
	'$lgt_default_flag'(plredef, Plredef),
	write('  Prolog built-in predicates redefinition (plredef):        '), write(Plredef), nl,
	'$lgt_default_flag'(portability, Portability),
	write('  Non portable calls (portability):                         '), write(Portability), nl,
	'$lgt_default_flag'(singletons, Singletons),
	write('  Singletons variables (singletons):                        '), write(Singletons), nl,
	'$lgt_default_flag'(underscore_vars, Underscore),
	write('  Underscore variables interpretation (underscore_vars):    '), write(Underscore), nl,
	write('Default documenting compilation flags:'), nl,
	'$lgt_default_flag'(xml, XML),
	write('  XML documenting files (xml):                              '), write(XML), nl,
	'$lgt_default_flag'(xmlspec, XMLSpec),
	write('  XML specification file extension (xmlspec):               '), write(XMLSpec), nl,
	'$lgt_default_flag'(doctype, Doctype),
	write('  XML specification file location (doctype):                '), write(Doctype), nl,
	'$lgt_default_flag'(xsl, XSL),
	write('  XSL stylesheet (xsl):                                     '), write(XSL), nl,
	write('Other default compilation flags:'), nl,
	'$lgt_default_flag'(report, Report),
	write('  Compilation report (report):                              '), write(Report), nl,
	'$lgt_default_flag'(code_prefix, Code),
	write('  Compiled code functors prefix (code_prefix):              '), writeq(Code), nl,
	'$lgt_default_flag'(debug, Debug),
	write('  Compile entities in debug mode (debug):                   '), writeq(Debug), nl,
	'$lgt_default_flag'(smart_compilation, Smart),
	write('  Smart compilation (smart_compilation):                    '), write(Smart), nl,
	'$lgt_default_flag'(events, Events),
	write('  Event-driven programming support (events):                '), write(Events), nl,
	write('Read-only compilation flags:'), nl,
	'$lgt_default_flag'(supports_break_predicate, Break),
	write('  Support for break/0 predicate (supports_break_predicate): '), write(Break), nl,
	'$lgt_default_flag'(iso_initialization_dir, ISO),
	write('  ISO initialization/1 directive (iso_initialization_dir):  '), write(ISO), nl,
	'$lgt_default_flag'(startup_message, Startup),
	write('  Startup message (startup_message):                        '), write(Startup), nl,
	'$lgt_default_flag'(altdirs, Altdirs),
	write('  Alternative compilation directories (altdirs):            '), write(Altdirs), nl,
	'$lgt_default_flag'(supports_encoding_dir, Encodings),
	write('  Support for encoding directive (supports_encoding_dir):   '), write(Encodings), nl, nl.



:- initialization('$lgt_startup_message').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
