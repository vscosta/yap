
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.17.1
%
%  Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
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

:- dynamic('$lgt_current_protocol_'/3).			% '$lgt_current_protocol_'(Ptc, Prefix, Type)
:- dynamic('$lgt_current_category_'/3).			% '$lgt_current_category_'(Ctg, Prefix, Type)
:- dynamic('$lgt_current_object_'/6).			% '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, Type)

:- dynamic('$lgt_implements_protocol_'/3).		% '$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope)
:- dynamic('$lgt_imports_category_'/3).			% '$lgt_imports_category_'(Obj, Ctg, Scope)
:- dynamic('$lgt_instantiates_class_'/3).		% '$lgt_instantiates_class_'(Instance, Class, Scope)
:- dynamic('$lgt_specializes_class_'/3).		% '$lgt_specializes_class_'(Class, Superclass, Scope)
:- dynamic('$lgt_extends_protocol_'/3).			% '$lgt_extends_protocol_'(Ptc1, Ptc2, Scope)
:- dynamic('$lgt_extends_object_'/3).			% '$lgt_extends_object_'(Prototype, Parent, Scope)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic('$lgt_dcl_'/1).						% '$lgt_dcl_'(Clause)
:- dynamic('$lgt_ddcl_'/1).						% '$lgt_ddcl_'(Clause)
:- dynamic('$lgt_def_'/1).						% '$lgt_def_'(Clause)
:- dynamic('$lgt_ddef_'/1).						% '$lgt_ddef_'(Clause)
:- dynamic('$lgt_super_'/1).					% '$lgt_super_'(Clause)

:- dynamic('$lgt_dynamic_'/1).					% '$lgt_dynamic_'(Functor/Arity)
:- dynamic('$lgt_discontiguous_'/1).			% '$lgt_discontiguous_'(Functor/Arity)
:- dynamic('$lgt_mode_'/2).						% '$lgt_mode_'(Mode, Determinism)
:- dynamic('$lgt_public_'/1).					% '$lgt_public_'(Functor/Arity)
:- dynamic('$lgt_protected_'/1).				% '$lgt_protected_'(Functor/Arity)
:- dynamic('$lgt_private_'/1).					% '$lgt_private_'(Functor/Arity)
:- dynamic('$lgt_metapredicate_'/1).			% '$lgt_metapredicate_'(Pred)

:- dynamic('$lgt_object_'/9).					% '$lgt_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef)
:- dynamic('$lgt_category_'/4).					% '$lgt_category_'(Ctg, Prefix, Dcl, Def)
:- dynamic('$lgt_protocol_'/3).					% '$lgt_protocol_'(Ptc, Prefix, Dcl)

:- dynamic('$lgt_uses_'/1).						% '$lgt_uses_'(Entity)
:- dynamic('$lgt_calls_'/1).					% '$lgt_calls_'(Entity)
:- dynamic('$lgt_info_'/1).						% '$lgt_info_'(List)
:- dynamic('$lgt_info_'/2).						% '$lgt_info_'(Functor/Arity, List)

:- dynamic('$lgt_implemented_protocol_'/4).		% '$lgt_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)
:- dynamic('$lgt_imported_category_'/5).		% '$lgt_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_extended_object_'/10).			% '$lgt_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_instantiated_class_'/10).		% '$lgt_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_specialized_class_'/10).		% '$lgt_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_extended_protocol_'/4).		% '$lgt_extended_protocol_'(Ptc2, Prefix, Dcl, Scope)

:- dynamic('$lgt_entity_'/4).					% '$lgt_entity_'(Type, Entity, Prefix, Dcl)
:- dynamic('$lgt_entity_functors_'/1).			% '$lgt_entity_functors_'(Clause)
:- dynamic('$lgt_entity_init_'/1).				% '$lgt_entity_init_'(Goal)
:- dynamic('$lgt_fentity_init_'/1).				% '$lgt_fentity_init_'(Goal)
:- dynamic('$lgt_entity_comp_mode_'/1).			% '$lgt_entity_comp_mode_'(Type)

:- dynamic('$lgt_redefined_built_in_'/3).		% '$lgt_redefined_built_in_'(Head, Context, THead)

:- dynamic('$lgt_directive_'/1).				% '$lgt_directive_'(Dir)
:- dynamic('$lgt_rclause_'/1).					% '$lgt_rclause_'(Clause)
:- dynamic('$lgt_eclause_'/1).					% '$lgt_eclause_'(Clause)
:- dynamic('$lgt_feclause_'/1).					% '$lgt_feclause_'(Clause)

:- dynamic('$lgt_defs_pred_'/1).				% '$lgt_defs_pred_'(Functor/Arity)
:- dynamic('$lgt_calls_pred_'/1).				% '$lgt_calls_pred_'(Functor/Arity)

:- dynamic('$lgt_current_compiler_option_'/2).	% '$lgt_current_compiler_option_'(Option, Value)
:- dynamic('$lgt_flag_'/2).						% '$lgt_flag_'(Option, Value)

:- dynamic('$lgt_referenced_object_'/1).		% '$lgt_referenced_object_'(Object)
:- dynamic('$lgt_referenced_protocol_'/1).		% '$lgt_referenced_protocol_'(Protocol)
:- dynamic('$lgt_referenced_category_'/1).		% '$lgt_referenced_category_'(Category)

:- dynamic('$lgt_global_op_'/3).				% '$lgt_global_op_'(Priority, Specifier, Operator)
:- dynamic('$lgt_local_op_'/3).					% '$lgt_local_op_'(Priority, Specifier, Operator)

:- dynamic('$lgt_debugging_'/1).				% '$lgt_debugging_'(Entity)

:- dynamic('$lgt_dbg_debugging_'/0).			% '$lgt_dbg_debugging_'
:- dynamic('$lgt_dbg_tracing_'/0).				% '$lgt_dbg_tracing_'
:- dynamic('$lgt_dbg_skipping_'/0).				% '$lgt_dbg_skipping_'
:- dynamic('$lgt_dbg_spying_'/1).				% '$lgt_dbg_spying_'(Functor/Arity)
:- dynamic('$lgt_dbg_spying_'/4).				% '$lgt_dbg_spying_'(Sender, This, Self, Goal)
:- dynamic('$lgt_dbg_leashing_'/1).				% '$lgt_dbg_leashing_'(Port)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  top level runtime predicate for message sending: ::/2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



Obj::Pred :-
	var(Obj),
	throw(error(instantiation_error, Obj::Pred, user)).

Obj::Pred :-
	var(Pred),
	throw(error(instantiation_error, Obj::Pred, user)).

Obj::Pred :-
	'$lgt_sender'(Ctx, user),
	'$lgt_this'(Ctx, user),
	'$lgt_self'(Ctx, Obj),
	'$lgt_metavars'(Ctx, []),
	'$lgt_tr_msg'(Obj, Pred, Call, Ctx),
	(('$lgt_debugging_'(Obj), '$lgt_dbg_debugging_') ->
		catch(
			'$lgt_dbg_goal'(Obj::Pred, Call, Ctx),
			Error,
			(Error = error(logtalk_debugger_aborted) ->
				write('Debugging session aborted by user. Debugger still on.'), nl, fail
				;
				throw(Error)))
		;
		call(Call)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_object(?object_identifier)

current_object(Obj) :-
	nonvar(Obj),
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), current_object(Obj))).

current_object(Obj) :-
	'$lgt_current_object_'(Obj, _, _, _, _, _).



% current_protocol(?protocol_identifier)

current_protocol(Ptc) :-
	nonvar(Ptc),
	\+ '$lgt_valid_protocol_id'(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), current_protocol(Ptc))).

current_protocol(Ptc) :-
	'$lgt_current_protocol_'(Ptc, _, _).



% current_category(?category_identifier)

current_category(Ctg) :-
	nonvar(Ctg),
	\+ '$lgt_valid_category_id'(Ctg),
	throw(error(type_error(category_identifier, Ctg), current_category(Ctg))).

current_category(Ctg) :-
	'$lgt_current_category_'(Ctg, _, _).



% object_property(?object_identifier, ?object_property)

object_property(Obj, Prop) :-
	nonvar(Obj),
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), object_property(Obj, Prop))).

object_property(Obj, Prop) :-
	nonvar(Prop),
	\+ '$lgt_member'(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(object_property, Prop), object_property(Obj, Prop))).

object_property(user, built_in).
object_property(debugger, built_in).

object_property(Obj, Prop) :-
	'$lgt_current_object_'(Obj, _, _, _, _, Prop).



% category_property(?category_identifier, ?category_property)

category_property(Ctg, Prop) :-
	nonvar(Ctg),
	\+ '$lgt_valid_category_id'(Ctg),
	throw(error(type_error(category_identifier, Ctg), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	nonvar(Prop),
	\+ '$lgt_member'(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(category_property, Prop), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	'$lgt_current_category_'(Ctg, _, Prop).



% protocol_property(?protocol_identifier, ?protocol_property)

protocol_property(Ptc, Prop) :-
	nonvar(Ptc),
	\+ '$lgt_valid_protocol_id'(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	nonvar(Prop),
	\+ '$lgt_member'(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(protocol_property, Prop), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	'$lgt_current_protocol_'(Ptc, _, Prop).



% create_object(+object_identifier, +list, +list, +list)

create_object(Obj, Rels, Dirs, Clauses) :-
	var(Obj),
	throw(error(instantiation_error, create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ '$lgt_valid_object_id'(Obj),
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
	(var(Rels); \+ '$lgt_proper_list'(Rels)),
	throw(error(type_error(list, Rels), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Dirs); \+ '$lgt_proper_list'(Dirs)),
	throw(error(type_error(list, Dirs), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Clauses); \+ '$lgt_proper_list'(Clauses)),
	throw(error(type_error(list, Clauses), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_clean_up',
	'$lgt_tr_directive'(object, [Obj| Rels]),
	'$lgt_tr_directives'([(dynamic)| Dirs]),
	'$lgt_tr_clauses'(Clauses),
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_object_clauses',
	'$lgt_gen_object_directives',
	'$lgt_assert_tr_entity',
	'$lgt_report_unknown_entities',
	'$lgt_clean_up'.



% create_category(+category_identifier, +list, +list, +list)

create_category(Ctg, Rels, Dirs, Clauses) :-
	var(Ctg),
	throw(error(instantiation_error, create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ '$lgt_valid_category_id'(Ctg),
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
	(var(Rels); \+ '$lgt_proper_list'(Rels)),
	throw(error(type_error(list, Rels), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Dirs); \+ '$lgt_proper_list'(Dirs)),
	throw(error(type_error(list, Dirs), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Clauses); \+ '$lgt_proper_list'(Clauses)),
	throw(error(type_error(list, Clauses), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_clean_up',
	'$lgt_tr_directive'(category, [Ctg| Rels]),
	'$lgt_tr_directives'([(dynamic)| Dirs]),
	'$lgt_tr_clauses'(Clauses),
	'$lgt_fix_redef_built_ins',
	'$lgt_gen_category_clauses',
	'$lgt_gen_category_directives',
	'$lgt_assert_tr_entity',
	'$lgt_report_unknown_entities',
	'$lgt_clean_up'.



% create_protocol(+protocol_identifier, +list, +list)

create_protocol(Ptc, Rels, Dirs) :-
	var(Ptc),
	throw(error(instantiation_error, create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	\+ '$lgt_valid_protocol_id'(Ptc),
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
	(var(Rels); \+ '$lgt_proper_list'(Rels)),
	throw(error(type_error(list, Rels), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	(var(Dirs); \+ '$lgt_proper_list'(Dirs)),
	throw(error(type_error(list, Dirs), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_clean_up',
	'$lgt_tr_directive'(protocol, [Ptc| Rels]),
	'$lgt_tr_directives'([(dynamic)| Dirs]),
	'$lgt_gen_protocol_clauses',
	'$lgt_gen_protocol_directives',
	'$lgt_assert_tr_entity',
	'$lgt_report_unknown_entities',
	'$lgt_clean_up'.



% abolish_object(@object_identifier)

abolish_object(Obj) :-
	var(Obj),
	throw(error(instantiation_error, abolish_object(Obj))).

abolish_object(Obj) :-
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_object(Obj))).

abolish_object(Obj) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, Type) ->
		(Type = (dynamic) ->
			'$lgt_once'(Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			forall(
				'$lgt_call'(Def, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity), abolish(Functor/Arity))),
			forall(
				'$lgt_call'(DDef, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity), abolish(Functor/Arity))),
			abolish(Dcl/4),
			abolish(Dcl/6),
			abolish(Def/5),
			abolish(Def/6),
			abolish(Super/6),
			abolish(IDcl/6),
			abolish(IDef/6),
			abolish(DDcl/2),
			abolish(DDef/5),
			abolish(Prefix/7),
			retractall('$lgt_current_object_'(Obj, _, _, _, _, _)),
			retractall('$lgt_extends_object_'(Obj, _, _)),
			retractall('$lgt_instantiates_class_'(Obj, _, _)),
			retractall('$lgt_specializes_class_'(Obj, _, _)),
			retractall('$lgt_implements_protocol_'(Obj, _, _)),
			retractall('$lgt_imports_category_'(Obj, _, _))			
			;
			throw(error(permission_error(modify, static_object, Obj), abolish_object(Obj))))
		;
		throw(error(existence_error(object, Obj), abolish_object(Obj))).



% abolish_category(@category_identifier)

abolish_category(Ctg) :-
	var(Ctg),
	throw(error(instantiation_error, abolish_category(Ctg))).

abolish_category(Ctg) :-
	\+ '$lgt_valid_category_id'(Ctg),
	throw(error(type_error(category_identifier, Ctg), abolish_category(Ctg))).

abolish_category(Ctg) :-
	'$lgt_current_category_'(Ctg, Prefix, Type) ->
		(Type = (dynamic) ->
			'$lgt_once'(Prefix, Dcl, Def),
			forall(
				'$lgt_call'(Def, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity), abolish(Functor/Arity))),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Def/5),
			abolish(Prefix/2),
			retractall('$lgt_current_category_'(Ctg, _, _)),
			retractall('$lgt_implements_protocol_'(Ctg, _, _))
			;
			throw(error(permission_error(modify, static_category, Ctg), abolish_category(Ctg))))
		;
		throw(error(existence_error(category, Ctg), abolish_category(Ctg))).



% abolish_protocol(@protocol_identifier)

abolish_protocol(Ptc) :-
	var(Ptc),
	throw(error(instantiation_error, abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	\+ '$lgt_valid_protocol_id'(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	'$lgt_current_protocol_'(Ptc, Prefix, Type) ->
		(Type = (dynamic) ->
			'$lgt_once'(Prefix, Dcl),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Prefix/1),
			retractall('$lgt_current_protocol_'(Ptc, _, _)),
			retractall('$lgt_extends_protocol_'(Ptc, _, _))
			;
			throw(error(permission_error(modify, static_protocol, Ptc), abolish_protocol(Ptc))))
		;
		throw(error(existence_error(protocol, Ptc), abolish_protocol(Ptc))).



% implements_protocol(?term, ?atom)

implements_protocol(Entity, Ptc) :-
	catch(
		implements_protocol(Entity, Ptc, _),
		error(Error, _),
		throw(error(Error, implements_protocol(Entity, Ptc)))).



% implements_protocol(?term, ?atom, ?atom)

implements_protocol(Entity, Ptc, Scope) :-
	nonvar(Entity),
	\+ '$lgt_valid_object_id'(Entity),
	throw(error(type_error(object_identifier, Entity), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	nonvar(Ptc),
	\+ '$lgt_valid_protocol_id'(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	'$lgt_implements_protocol_'(Entity, Ptc, Scope).



% imports_category(?term, ?term)

imports_category(Obj, Ctg) :-
	catch(
		imports_category(Obj, Ctg, _),
		error(Error, _),
		throw(error(Error, imports_category(Obj, Ctg)))).



% imports_category(?term, ?term, ?atom)

imports_category(Obj, Ctg, Scope) :-
	nonvar(Obj),
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	nonvar(Ctg),
	\+ '$lgt_valid_category_id'(Ctg),
	throw(error(type_error(category_identifier, Ctg), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	'$lgt_imports_category_'(Obj, Ctg, Scope).



% instantiates_class(?term, ?term)

instantiates_class(Obj, Class) :-
	catch(
		instantiates_class(Obj, Class, _),
		error(Error, _),
		throw(error(Error, instantiates_class(Obj, Class)))).



% instantiates_class(?term, ?term, ?atom)

instantiates_class(Obj, Class, Scope) :-
	nonvar(Obj),
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Class),
	\+ '$lgt_valid_object_id'(Class),
	throw(error(type_error(object_identifier, Class), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	'$lgt_instantiates_class_'(Obj, Class, Scope).



% specializes_class(?term, ?term)

specializes_class(Class, Superclass) :-
	catch(
		specializes_class(Class, Superclass, _),
		error(Error, _),
		throw(error(Error, specializes_class(Class, Superclass)))).



% specializes_class(?term, ?term, ?atom)

specializes_class(Class, Superclass, Scope) :-
	nonvar(Class),
	\+ '$lgt_valid_object_id'(Class),
	throw(error(type_error(object_identifier, Class), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Superclass),
	\+ '$lgt_valid_object_id'(Superclass),
	throw(error(type_error(object_identifier, Superclass), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	'$lgt_specializes_class_'(Class, Superclass, Scope).



% extends_protocol(?atom, ?atom)

extends_protocol(Ptc1, Ptc2) :-
	catch(
		extends_protocol(Ptc1, Ptc2, _),
		error(Error, _),
		throw(error(Error, extends_protocol(Ptc1, Ptc2)))).



% extends_protocol(?atom, ?atom, ?atom)

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc1),
	\+ '$lgt_valid_protocol_id'(Ptc1),
	throw(error(type_error(protocol_identifier, Ptc1), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc2),
	\+ '$lgt_valid_protocol_id'(Ptc2),
	throw(error(type_error(protocol_identifier, Ptc2), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Scope),
	\+ '$lgt_member'(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	'$lgt_extends_protocol_'(Ptc1, Ptc2, Scope).



% extends_object(?term, ?term)

extends_object(Prototype, Parent) :-
	catch(
		extends_object(Prototype, Parent, _),
		error(Error, _),
		throw(error(Error, extends_object(Prototype, Parent)))).



% extends_object(?term, ?term, ?atom)

extends_object(Prototype, Parent, Scope) :-
	nonvar(Prototype),
	\+ '$lgt_valid_object_id'(Prototype),
	throw(error(type_error(object_identifier, Prototype), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	nonvar(Parent),
	\+ '$lgt_valid_object_id'(Parent),
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
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ '$lgt_callable'(Msg),
	throw(error(type_error(callable, Msg), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ '$lgt_valid_object_id'(Sender),
	throw(error(type_error(object_identifier, Sender), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ '$lgt_valid_object_id'(Monitor),
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
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ '$lgt_callable'(Msg),
	throw(error(type_error(callable, Msg), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ '$lgt_valid_object_id'(Sender),
	throw(error(type_error(object_identifier, Sender), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Monitor),
	throw(error(instantiation_error, define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ '$lgt_valid_object_id'(Monitor),
	throw(error(type_error(object_identifier, Monitor), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Event),
	!,
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_call'(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, BCall, _),
	'$lgt_call'(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, ACall, _),
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_before_'(Obj, Msg, Sender, Monitor, BCall)),
	retractall('$lgt_after_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_after_'(Obj, Msg, Sender, Monitor, ACall)).

define_events(before, Obj, Msg, Sender, Monitor) :-
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_call'(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
	retractall('$lgt_before_'(Obj, Msg, Sender, Monitor, _)),
	assertz('$lgt_before_'(Obj, Msg, Sender, Monitor, Call)).

define_events(after, Obj, Msg, Sender, Monitor) :-
	'$lgt_current_object_'(Monitor, _, _, Def, _, _),
	'$lgt_call'(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
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
	\+ '$lgt_valid_object_id'(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ '$lgt_callable'(Msg),
	throw(error(type_error(callable, Msg), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ '$lgt_valid_object_id'(Sender),
	throw(error(type_error(object_identifier, Sender), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ '$lgt_valid_object_id'(Monitor),
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


% '$lgt_compiler_option'(+atom, ?atom)
%
% gets/check the current value of a compiler option

'$lgt_compiler_option'(Option, Value) :-
	'$lgt_current_compiler_option_'(Option, Value2),
	!,
	Value = Value2.

'$lgt_compiler_option'(Option, Value) :-
	'$lgt_flag_'(Option, Value2),
	!,
	Value = Value2.

'$lgt_compiler_option'(Option, Value) :-
	'$lgt_default_flag'(Option, Value).



% logtalk_compile(@atom_or_atom_list)
%
% compiles to disk an entity or list of entities using default options

logtalk_compile(Entities) :-
	catch(
		logtalk_compile(Entities, []),
		error(Error, _),
		throw(error(Error, logtalk_compile(Entities)))).



% logtalk_compile(@atom_or_atom_list, @list)
%
% compiles to disk an entity or a list of entities using a list of options

logtalk_compile(Entity, Options) :-
	atom(Entity),
	!,
	catch(
		('$lgt_check_compiler_entity'(Entity),
		 '$lgt_check_compiler_options'(Options),
		 '$lgt_set_compiler_options'(Options),
		 '$lgt_compile_entity'(Entity)),
		Error,
		throw(error(Error, logtalk_compile(Entity, Options)))).

logtalk_compile(Entities, Options) :-
	catch(
		('$lgt_check_compiler_entities'(Entities),
		 '$lgt_check_compiler_options'(Options),
		 '$lgt_set_compiler_options'(Options),
		 '$lgt_compile_entities'(Entities)),
		Error,
		throw(error(Error, logtalk_compile(Entities, Options)))).



% '$lgt_check_compiler_entities'(@list)
%
% check if the entities names are valid and if the corresponding
% files exist in the current working directory

'$lgt_check_compiler_entities'(Entities) :-
	var(Entities),
	throw(instantiation_error).

'$lgt_check_compiler_entities'(Entities) :-
	\+ '$lgt_proper_list'(Entities),
	throw(type_error(atom_or_atom_list, Entities)).

'$lgt_check_compiler_entities'(Entities) :-
	'$lgt_check_compiler_entity_list'(Entities).



'$lgt_check_compiler_entity_list'([]).

'$lgt_check_compiler_entity_list'([Entity| Entities]) :-
	'$lgt_check_compiler_entity'(Entity),
	'$lgt_check_compiler_entity_list'(Entities).



'$lgt_check_compiler_entity'(Entity) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_check_compiler_entity'(Entity) :-
	\+ atom(Entity),
	throw(type_error(atom, Entity)).

'$lgt_check_compiler_entity'(Entity) :-
	'$lgt_file_name'(logtalk, Entity, File),
	\+ '$lgt_file_exists'(File),
	throw(existence_error(entity, Entity)).

'$lgt_check_compiler_entity'(_).



% '$lgt_check_compiler_options'(@list)
%
% check if the compiler options are valid

'$lgt_check_compiler_options'(Options) :-
	var(Options),
	throw(instantiation_error).

'$lgt_check_compiler_options'(Options) :-
	\+ '$lgt_proper_list'(Options),
	throw(type_error(list, Options)).

'$lgt_check_compiler_options'(Options) :-
	'$lgt_check_compiler_option_list'(Options).



'$lgt_check_compiler_option_list'([]).

'$lgt_check_compiler_option_list'([Option| Options]) :-
	'$lgt_check_compiler_option'(Option),
	'$lgt_check_compiler_option_list'(Options).



'$lgt_check_compiler_option'(Option) :-
	'$lgt_valid_compiler_option'(Option) ->
		true
		;
		throw(type_error(compiler_option, Option)).



% '$lgt_set_compiler_options'(@list)
%
% sets the compiler options

'$lgt_set_compiler_options'(Options) :-
	retractall('$lgt_current_compiler_option_'(_, _)),
	'$lgt_assert_compiler_options'(Options),
	('$lgt_current_compiler_option_'(debug, on) ->
		retractall('$lgt_current_compiler_option_'(smart_compilation, _)),
		asserta('$lgt_current_compiler_option_'(smart_compilation, off))
		;
		true).


'$lgt_assert_compiler_options'([]).

'$lgt_assert_compiler_options'([Option| Options]) :-
	Option =.. [Key, Value],
	asserta('$lgt_current_compiler_option_'(Key, Value)),
	'$lgt_assert_compiler_options'(Options).



% logtalk_load(@atom_or_atom_list)
%
% compiles to disk and then loads to memory an entity 
% or a list of entities using default options

logtalk_load(Entities) :-
	catch(
		logtalk_load(Entities, []),
		error(Error, _),
		throw(error(Error, logtalk_load(Entities)))).



% logtalk_load(@atom_or_atom_list, @list)
%
% compiles to disk and then loads to memory an entity 
% or a list of entities using a list of options

logtalk_load(Entity, Options) :-
	atom(Entity),
	!,
	catch(
		('$lgt_check_compiler_entity'(Entity),
		 '$lgt_check_compiler_options'(Options),
		 '$lgt_set_compiler_options'(Options),
		 '$lgt_load_entity'(Entity)),
		Error,
		throw(error(Error, logtalk_load(Entity, Options)))).

logtalk_load(Entities, Options) :-
	catch(
		('$lgt_check_compiler_entities'(Entities),
		 '$lgt_check_compiler_options'(Options),
		 '$lgt_set_compiler_options'(Options),
		 '$lgt_load_entities'(Entities)),
		Error,
		throw(error(Error, logtalk_load(Entities, Options)))).



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
	retractall('$lgt_flag_'(debug, _)),
	assertz('$lgt_flag_'(debug, on)),
	retractall('$lgt_flag_'(smart_compilation, _)),
	assertz('$lgt_flag_'(smart_compilation, off)).

set_logtalk_flag(Flag, Value) :-
	retractall('$lgt_flag_'(Flag, _)),
	assertz('$lgt_flag_'(Flag, Value)).



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
	'$lgt_flag_'(Flag, Value).

current_logtalk_flag(Flag, Value) :-
	'$lgt_default_flag'(Flag, Value),
	\+ '$lgt_flag_'(Flag, _).

current_logtalk_flag(version, version(2, 17, 1)).




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
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _),
	'$lgt_once'(Dcl, Pred, PScope, _, _, SCtn, _),
	once((\+ \+ PScope = Scope; Sender = SCtn)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _),
	findall(
		Functor/Arity - (PScope, SCtn),
		('$lgt_call'(Dcl, Pred, PScope, _, _, SCtn, _),
		 once((\+ \+ PScope = Scope; Sender = SCtn)),
		 functor(Pred, Functor, Arity)),
		Preds),
	'$lgt_cp_filter'(Preds, Filtered),
	'$lgt_member'(Functor/Arity - (PScope, SCtn), Filtered).


% '$lgt_cp_filter'(+list, -list)
%
% removes duplicated and redeclared predicates 

'$lgt_cp_filter'([], []).

'$lgt_cp_filter'([Data| Rest], [Data| Rest2]) :-
	'$lgt_cp_remove_all'(Rest, Data, Aux),
	'$lgt_cp_filter'(Aux, Rest2).


'$lgt_cp_remove_all'([], _, []).

'$lgt_cp_remove_all'([F/A-_| Rest], F/A-D, List) :-
	!,
	'$lgt_cp_remove_all'(Rest, F/A-D, List).

'$lgt_cp_remove_all'([Data| Rest], Filter, [Data| Rest2]) :-
	!,
	'$lgt_cp_remove_all'(Rest, Filter, Rest2).



% predicate_property/2 built-in method

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	nonvar(Prop),
	\+ '$lgt_member'(Prop, [(public), protected, private, static, (dynamic), declared_in(_), defined_in(_), metapredicate(_), built_in]),
	throw(error(domain_error(predicate_property, Prop), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ '$lgt_callable'(Pred),
	throw(error(type_error(callable, Pred), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _),
	'$lgt_once'(Dcl, Pred, PScope, Type, Meta, SCtn, TCtn),
	!,
	once((\+ \+ PScope = Scope; Sender = SCtn)),
	('$lgt_scope'(Prop, PScope);
	 Prop = Type;
	 Prop = declared_in(TCtn);
	 '$lgt_once'(Def, Pred, _, _, _, _, DCtn),
	 Prop = defined_in(DCtn);
	 Meta \= no, Prop = metapredicate(Meta)).

'$lgt_predicate_property'(_, Pred, Prop, _, Scope) :-
	'$lgt_built_in_method'(Pred, PScope),
	!,
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	\+ \+ PScope = Scope,
	('$lgt_scope'(Prop, PScope);
	 Prop = static;
	 Prop = built_in;
	 ('$lgt_metapredicate'(Meta) -> Prop = metapredicate(Meta))).

'$lgt_predicate_property'(_, Pred, Prop, _, _) :-
	'$lgt_built_in'(Pred),
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	(Prop = (public);
	 ('$lgt_predicate_property'(Pred, (dynamic)) -> Prop = (dynamic); Prop = static);
	 Prop = built_in;
	 ('$lgt_metapredicate'(Meta) -> Prop = metapredicate(Meta))).


% '$lgt_scope'(?atom, ?term).
%
% converts between user and system scope terms

'$lgt_scope'(private, p).
'$lgt_scope'(protected, p(p)).
'$lgt_scope'((public), p(p(p))).



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

'$lgt_abolish'(Obj, Functor/Arity, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, _, _, _) ->
		((functor(Pred, Functor, Arity),
		  '$lgt_once'(Dcl, Pred, PScope, Compilation, _, SCtn, _)) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				(Compilation = (dynamic) ->
					'$lgt_once'(Prefix, _, _, _, _, _, DDcl, DDef),
					('$lgt_once'(DDcl, Pred, _) ->
						Clause =.. [DDcl, Pred, _],
						retractall(Clause),
						('$lgt_once'(DDef, Pred, _, _, _, Call) ->
							functor(Call, CFunctor, CArity),
							abolish(CFunctor/CArity),
							Clause2 =.. [DDef, Pred, _, _, _, Call],
							retractall(Clause2)
							;
							true)
						;
						('$lgt_once'(Dcl, Pred, _, _, _) ->
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

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	\+ '$lgt_callable'(Head),
	throw(error(type_error(callable, Head), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _) :-
	\+ '$lgt_callable'(Body),
	throw(error(type_error(callable, Body), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::asserta(Clause), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, Scope) :-
	!,
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, Meta, SCtn, _) ->
	 	true
	 	;
	 	'$lgt_convert_test_scope'(Scope, Scope2),
	 	'$lgt_assert_ddcl_clause'(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SCtn)  ->
			(('$lgt_once'(Def, Head, Sender2, This, Self, Call); '$lgt_once'(DDef, Head, Sender2, This, Self, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				'$lgt_assert_ddef_clause'(Functor, Arity, Prefix, DDef, _),
				'$lgt_once'(DDef, Head, Sender2, This, Self, Call)),
			'$lgt_self'(Ctx, Self),
			'$lgt_this'(Ctx, This),
			'$lgt_sender'(Ctx, Sender2),
			'$lgt_prefix'(Ctx, Prefix),
			(compound(Meta) ->
				Head =.. [_| Args],
				Meta =.. [_| MArgs],
				'$lgt_extract_metavars'(Args, MArgs, Metavars)
				;
				Metavars = []),
			'$lgt_metavars'(Ctx, Metavars),
			'$lgt_tr_body'(Body, TBody, DBody, Ctx),
			('$lgt_debugging_'(Obj) ->
				asserta((Call :- ('$lgt_nop'(Body), DBody)))
				;
				asserta((Call :- ('$lgt_nop'(Body), TBody))))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta((Head:-Body)), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta((Head:-Body)), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta((Head:-Body)), Sender))).

'$lgt_asserta'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
	 	true
	 	;
	 	'$lgt_convert_test_scope'(Scope, Scope2),
	 	'$lgt_assert_ddcl_clause'(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SCtn)  ->
			(('$lgt_once'(Def, Head, _, _, _, Call); '$lgt_once'(DDef, Head, _, _, _, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				'$lgt_assert_ddef_clause'(Functor, Arity, Prefix, DDef, _),
				'$lgt_once'(DDef, Head, _, _, _, Call)),
			('$lgt_debugging_'(Obj) ->
				'$lgt_context'(Ctx),
				'$lgt_sender'(Ctx, Sender),
				'$lgt_this'(Ctx, Obj),
				'$lgt_self'(Ctx, Obj),
				asserta((Call :- '$lgt_dbg_fact'(Head, Ctx)))
				;
				asserta(Call))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta(Head), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta(Head), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta(Head), Sender))).



% assertz/1 built-in method

'$lgt_assertz'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::assertz(Clause), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	\+ '$lgt_callable'(Head),
	throw(error(type_error(callable, Head), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _) :-
	\+ '$lgt_callable'(Body),
	throw(error(type_error(callable, Body), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::assertz(Clause), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, Scope) :-
	!,
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, Meta, SCtn, _) ->
	 	true
	 	;
	 	'$lgt_convert_test_scope'(Scope, Scope2),
	 	'$lgt_assert_ddcl_clause'(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SCtn)  ->
			(('$lgt_once'(Def, Head, Sender2, This, Self, Call); '$lgt_once'(DDef, Head, Sender2, This, Self, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				'$lgt_assert_ddef_clause'(Functor, Arity, Prefix, DDef, _),
				'$lgt_once'(DDef, Head, Sender2, This, Self, Call)),
			'$lgt_self'(Ctx, Self),
			'$lgt_this'(Ctx, This),
			'$lgt_sender'(Ctx, Sender2),
			'$lgt_prefix'(Ctx, Prefix),
			(compound(Meta) ->
				Head =.. [_| Args],
				Meta =.. [_| MArgs],
				'$lgt_extract_metavars'(Args, MArgs, Metavars)
				;
				Metavars = []),
			'$lgt_metavars'(Ctx, Metavars),
			'$lgt_tr_body'(Body, TBody, DBody, Ctx),
			('$lgt_debugging_'(Obj) ->
				assertz((Call :- ('$lgt_nop'(Body), DBody)))
				;
				assertz((Call :- ('$lgt_nop'(Body), TBody))))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz((Head:-Body)), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz((Head:-Body)), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz((Head:-Body)), Sender))).

'$lgt_assertz'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
	 	true
	 	;
	 	'$lgt_convert_test_scope'(Scope, Scope2),
	 	'$lgt_assert_ddcl_clause'(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SCtn)  ->
			(('$lgt_once'(Def, Head, _, _, _, Call); '$lgt_once'(DDef, Head, _, _, _, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				'$lgt_assert_ddef_clause'(Functor, Arity, Prefix, DDef, _),
				'$lgt_once'(DDef, Head, _, _, _, Call)),
			('$lgt_debugging_'(Obj) ->
				'$lgt_context'(Ctx),
				'$lgt_sender'(Ctx, Sender),
				'$lgt_this'(Ctx, Obj),
				'$lgt_self'(Ctx, Obj),
				assertz((Call :- '$lgt_dbg_fact'(Head, Ctx)))
				;
				assertz(Call))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz(Head), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz(Head), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz(Head), Sender))).



% clause/2 built-in method

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	\+ '$lgt_callable'(Head),
	throw(error(type_error(callable, Head), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	nonvar(Body),
	\+ '$lgt_callable'(Body),
	throw(error(type_error(callable, Body), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, _, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				once(('$lgt_once'(Def, Head, _, _, _, Call); '$lgt_once'(DDef, Head, _, _, _, Call))),
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
		throw(error(existence_error(predicate_declaration, Head), Obj::clause(Head, Body), Sender))).



% retract/1 built-in method

'$lgt_retract'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::retract(Clause), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	\+ '$lgt_callable'(Head),
	throw(error(type_error(callable, Head), Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, Clause, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::retract(Clause), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, Scope) :-
	!,
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, _, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				('$lgt_once'(Def, Head, _, _, _, Call) ->
					retract((Call :- ('$lgt_nop'(Body), _)))
					;
					('$lgt_once'(DDef, Head, _, _, _, Call) ->
						retract((Call :- ('$lgt_nop'(Body), _))),
						'$lgt_update_ddef_table'(DDef, Call)
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
		throw(error(existence_error(predicate_declaration, Head), Obj::retract((Head:-Body)), Sender))).

'$lgt_retract'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, _, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				('$lgt_once'(Def, Head, _, _, _, Call) ->
					('$lgt_debugging_'(Obj) ->
						retract((Call :- '$lgt_dbg_fact'(_, _)))
						;
						retract(Call))
					;
					('$lgt_once'(DDef, Head, _, _, _, Call) ->
						('$lgt_debugging_'(Obj) ->
							retract((Call :- '$lgt_dbg_fact'(_, _)))
							;
							retract(Call)),
						'$lgt_update_ddef_table'(DDef, Call)
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
		throw(error(existence_error(predicate_declaration, Head), Obj::retract(Head), Sender))).



% retractall/1 built-in method

'$lgt_retractall'(Obj, Head, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retractall(Head), Sender)).

'$lgt_retractall'(Obj, Head, Sender, _) :-
	\+ '$lgt_callable'(Head),
	throw(error(type_error(callable, Head), Obj::retractall(Head), Sender)).

'$lgt_retractall'(Obj, Head, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::retractall(Head), Sender)).

'$lgt_retractall'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _),
	'$lgt_once'(Prefix, Dcl, Def, _, _, _, _, DDef),
	('$lgt_once'(Dcl, Head, PScope, Type, _, SCtn, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SCtn) ->
				('$lgt_once'(Def, Head, _, _, _, Call) ->
					retractall(Call)
					;
					('$lgt_once'(DDef, Head, _, _, _, Call) ->
						retractall(Call),
						'$lgt_update_ddef_table'(DDef, Call)
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
		throw(error(existence_error(predicate_declaration, Head), Obj::retractall(Head), Sender))).



% '$lgt_nop'(+goal)
%
% used in the implementation of the built-in  
% method clause/2 to store original clause body

'$lgt_nop'(_).



% '$lgt_phrase'(+ruleset, ?list)
%
% phrase/2 built-in method

'$lgt_phrase'(Obj, Ruleset, Input, Sender, Scope) :-
	catch(
		'$lgt_phrase'(Obj, Ruleset, Input, [], Sender, Scope),
		error(Error, _),
		throw(error(Error, Obj::phrase(Ruleset, Input), Sender))).



% '$lgt_phrase'(+ruleset, ?list, ?list)
%
% phrase/3 built-in method

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	var(Ruleset),
	throw(error(instantiation_error, Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	\+ '$lgt_callable'(Ruleset),
	throw(error(type_error(callable, Ruleset), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	nonvar(Input),
	\+ '$lgt_proper_list'(Input),
	throw(error(type_error(list, Input), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, _) :-
	nonvar(Rest),
	\+ '$lgt_proper_list'(Rest),
	throw(error(type_error(list, Rest), Obj::phrase(Ruleset, Input, Rest), Sender)).

'$lgt_phrase'(_, [], Input, Rest, _, _) :-
	!,
	Input = Rest.

'$lgt_phrase'(_, [Head| Tail], Input, Rest, _, _) :-
	!,
	'$lgt_append'([Head| Tail], Rest, Input).

'$lgt_phrase'(Obj, Ruleset, Input, Rest, Sender, Scope) :-
	Ruleset =.. [Functor| Args],
	'$lgt_append'(Args, [Input, Rest], Args2),
	Pred =.. [Functor| Args2],
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _),
	('$lgt_once'(Dcl, Pred, PScope, _, _, SCtn, _) ->
		((\+ \+ PScope = Scope; Sender = SCtn) ->
			'$lgt_once'(Def, Pred, Sender, Obj, Obj, Call, _),
			call(Call)
			;
			(PScope = p ->
				throw(error(permission_error(access, private_predicate, Pred), Obj::phrase(Ruleset, Input, Rest), Sender))
				;
				throw(error(permission_error(access, protected_predicate, Pred), Obj::phrase(Ruleset, Input, Rest), Sender))))
		;
		((Obj = Sender, '$lgt_once'(Def, Pred, Obj, Obj, Obj, Call, _)) ->
			call(Call)
			;
			throw(error(existence_error(procedure, Pred), Obj::phrase(Ruleset, Input, Rest), Sender)))).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message sending
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_send_to_self'(+object, ?term, +object)

'$lgt_send_to_self'(Self, Pred, This) :-
	nonvar(Pred) ->
		'$lgt_send_to_self_nv'(Self, Pred, This)
		;
		throw(error(instantiation_error, Self::Pred, This)).



% '$lgt_send_to_self_nv'(+object, +term, +object)

'$lgt_send_to_self_nv'(Self, Pred, This) :-
	'$lgt_current_object_'(Self, _, Dcl, Def, _, _),
	('$lgt_call'(Dcl, Pred, Scope, _, _, SCtn, _) ->
		((Scope = p(_); This = SCtn) ->
			'$lgt_once'(Def, Pred, This, Self, Self, Call, _),
			call(Call)
			;
			throw(error(permission_error(access, private_predicate, Pred), Self::Pred, This)))
		;
		('$lgt_built_in'(Pred) ->
			call(Pred)
			;
			throw(error(existence_error(predicate_declaration, Pred), Self::Pred, This)))).



% '$lgt_send_to_object'(@object, ?term, +object)

'$lgt_send_to_object'(Obj, Pred, Sender) :-
	nonvar(Obj) ->
		('$lgt_current_object_'(Obj, _, Dcl, Def, _, _) ->
			(nonvar(Pred) ->
			 	('$lgt_call'(Dcl, Pred, Scope, _, _, _, _) ->
			 		(Scope = p(p(_)) ->
						'$lgt_once'(Def, Pred, Sender, Obj, Obj, Call, _),
						\+ ('$lgt_before_'(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
						call(Call),
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
				throw(error(instantiation_error, Obj::Pred, Sender)))
			;
			throw(error(existence_error(object, Obj), Obj::Pred, Sender)))
		;
		throw(error(instantiation_error, Obj::Pred, Sender)).



% '$lgt_send_to_object_nv'(+object, +term, +object)

'$lgt_send_to_object_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _) ->
		('$lgt_call'(Dcl, Pred, Scope, _, _, _, _) ->
			(Scope = p(p(_)) ->
				'$lgt_once'(Def, Pred, Sender, Obj, Obj, Call, _),
				\+ ('$lgt_before_'(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
				call(Call),
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
		throw(error(existence_error(object, Obj), Obj::Pred, Sender)).



% '$lgt_send_to_super'(+object, ?term, +object, +object)

'$lgt_send_to_super'(Self, Pred, This, Sender) :-
	nonvar(Pred) ->
		'$lgt_send_to_super_nv'(Self, Pred, This, Sender)
		;
		throw(error(instantiation_error, ^^Pred, This)).



% '$lgt_send_to_super_nv'(+object, +term, +object, +object)

'$lgt_send_to_super_nv'(Self, Pred, This, Sender) :-
	'$lgt_current_object_'(Self, _, Dcl, _, _, _),
	('$lgt_call'(Dcl, Pred, Scope, _, _, SCtn, _) ->
	 	((Scope = p(_); This = SCtn) ->
			'$lgt_current_object_'(This, _, _, _, Super, _),
			'$lgt_once'(Super, Pred, Sender, This, Self, Call, Ctn),
			(Ctn \= This ->
				call(Call)
				;
				throw(error(endless_loop(Pred), ^^Pred, This)))
		;
		throw(error(permission_error(access, private_predicate, Pred), ^^Pred, This)))
	;
	('$lgt_built_in'(Pred) ->
		call(Pred)
		;
		throw(error(existence_error(predicate_declaration, Pred), ^^Pred, This)))).



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
			'$lgt_prefix'(Ctx, Prefix),
			'$lgt_sender'(Ctx, Sender),
			'$lgt_this'(Ctx, Obj),
			'$lgt_self'(Ctx, Obj),
			'$lgt_tr_body'(Pred, Call, _, Ctx),
			call(Call)).



% '$lgt_call_built_in'(+term, +term)
%
% needed for runtime translation of dynamic clauses

'$lgt_call_built_in'(Pred, Ctx) :-
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
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
	nonvar(Pred),
	\+ '$lgt_built_in'(Pred),
	functor(Pred, Functor, Arity),
	current_predicate(Functor/Arity),
	('$lgt_predicate_property'(Pred, (dynamic)) ->
		Type = (dynamic)
		;
		Type = static).

'$lgt_po_user0__dcl'(Pred, p(p(p)), Type, no) :-
	var(Pred),
	current_predicate(Functor/Arity),
	\+ '$lgt_hidden_functor'(Functor),
	functor(Pred, Functor, Arity),
	\+ '$lgt_built_in'(Pred),
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
	atom_concat('$lgt_', _, Functor).

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_category_'(_, Prefix, _),
	atom_concat(Prefix, _, Functor).

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_object_'(_, Prefix, _, _, _, _),
	atom_concat(Prefix, _, Functor).

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_protocol_'(_, Prefix, _),
	atom_concat(Prefix, _, Functor).




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
	('$lgt_dbg_spying_'(_) ->
		write('Defined predicate spy points (Functor/Arity):'), nl,
		forall(
			'$lgt_dbg_spying_'(Functor/Arity),
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


'$lgt_dbg_pretty_print_spypoint'(Sender, This, Self, Goal) :-
	current_ouput(Output),
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
	('$lgt_dbg_spying_'(Functor/Arity) ->
		true
		;
		assertz('$lgt_dbg_spying_'(Functor/Arity))),
	'$lgt_dbg_spy_aux'(Preds).

'$lgt_dbg_spy_aux'(Functor/Arity) :-
	nonvar(Functor),
	nonvar(Arity),
	('$lgt_dbg_spying_'(Functor/Arity) ->
		true
		;
		assertz('$lgt_dbg_spying_'(Functor/Arity))).


'$lgt_dbg_nospy'(Preds) :-
	'$lgt_dbg_nospy_aux'(Preds),
	write('All matching predicate spy points removed.'), nl.


'$lgt_dbg_nospy_aux'(Preds) :-
	var(Preds) ->
		retractall('$lgt_dbg_spying_'(_))
		;
		'$lgt_dbg_nospy_aux2'(Preds).


'$lgt_dbg_nospy_aux2'([]).

'$lgt_dbg_nospy_aux2'([Functor/Arity| Preds]) :-
	retractall('$lgt_dbg_spying_'(Functor/Arity)),
	'$lgt_dbg_nospy_aux2'(Preds).

'$lgt_dbg_nospy_aux2'(Functor/Arity) :-
	retractall('$lgt_dbg_spying_'(Functor/Arity)).


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
	retractall('$lgt_dbg_spying_'(_)),
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
	!,
	'$lgt_dbg_leash_shortand_ports'(Shorthand, Ports).

'$lgt_dbg_valid_leash_value'(Ports, Ports) :-
	'$lgt_proper_list'(Ports),
	'$lgt_dbg_valid_leash_ports'(Ports).


'$lgt_dbg_valid_leash_ports'([]).

'$lgt_dbg_valid_leash_ports'([Port| Ports]) :-
	atom(Port),
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
	\+ \+ '$lgt_dbg_spying_'(Functor/Arity).
	
'$lgt_dbg_spying'(_, Goal, Ctx, '*') :-
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
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
			 (TAction = fail -> fail; TAction = throw -> throw(Error)))),
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
	('$lgt_compiler_option'(supports_break_predicate, true) ->
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
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
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



% '$lgt_load_entities'(+list)
%
% compiles to disk and then loads to memory a list of entities

'$lgt_load_entities'([]).

'$lgt_load_entities'([Entity| Entities]) :-
	'$lgt_load_entity'(Entity),
	'$lgt_load_entities'(Entities).



% '$lgt_load_entity'(+atom)
%
% compiles to disk and then loads to memory an entity

'$lgt_load_entity'(Entity) :-
	'$lgt_compile_entity'(Entity),
	('$lgt_redefined_entity'(Entity, Type, Identifier) ->
		'$lgt_remove_old_entity'(Type, Identifier),
		'$lgt_report_redefined_entity'(Type, Identifier)
		;
		true),
	'$lgt_file_name'(prolog, Entity, File),
	'$lgt_load_prolog_code'(File),
	'$lgt_report_loaded_entity'(Entity).



% '$lgt_redefined_entity'(+atom, -atom)
%
% true if an entity of the same name is already loaded

'$lgt_redefined_entity'(Entity, object, Entity) :-
	'$lgt_current_object_'(Entity, _, _, _, _, _),
	!.

'$lgt_redefined_entity'(Entity, object, Identifier) :-		% parametric objects
	atom_codes(Entity, Codes),								% this is a quick and dirty hack
	'$lgt_compiler_option'(code_prefix, Atom),				% assuming that code_prefix does
	atom_codes(Atom, Code),									% not change between entity
	'$lgt_append'(Code, Codes, Codes2),						% compilations
	'$lgt_append'(Codes2, '_', Prefix),
	'$lgt_current_object_'(Identifier, Prefix, _, _, _, _),
	!.

'$lgt_redefined_entity'(Entity, protocol, Entity) :-
	'$lgt_current_protocol_'(Entity, _, _),
	!.

'$lgt_redefined_entity'(Entity, category, Entity) :-
	'$lgt_current_category_'(Entity, _, _).



% '$lgt_remove_old_entity'(+atom, +entity_identifier)
%
% remove old entity if dynamic otherwise retract all 
% clauses for all dynamic predicates

'$lgt_remove_old_entity'(object, Entity) :-
	object_property(Entity, (dynamic)) ->
		abolish_object(Entity)
		;
		forall(
			('$lgt_current_predicate'(Entity, Functor/Arity, Entity, _),
			 functor(Head, Functor, Arity),
			 '$lgt_predicate_property'(Entity, Head, (dynamic), Entity, _)), 
			'$lgt_retractall'(Entity, Head, Entity, _)).

'$lgt_remove_old_entity'(protocol, Entity) :-
	protocol_property(Entity, (dynamic)) ->
		abolish_protocol(Entity)
		;
		true.

'$lgt_remove_old_entity'(category, Entity) :-
	category_property(Entity, (dynamic)) ->
		abolish_category(Entity)
		;
		true.



% '$lgt_report_redefined_entity'(+atom, +entity_identifier)
%
% prints a warning for redefined entities

'$lgt_report_redefined_entity'(Type, Entity) :-
	'$lgt_compiler_option'(report, on) ->
		write('> WARNING!  redefining '), write(Type), write(' '), write(Entity), nl
		;
		true.


% '$lgt_report_up_to_date_entity'(+entity_identifier)
%
% prints a message that an entity is up-to-date

'$lgt_report_up_to_date_entity'(Entity) :-
	'$lgt_compiler_option'(report, on) ->
		nl, write('>>> compiling '), writeq(Entity), write('... up-to-date'), nl
		;
		true.



% '$lgt_report_compiling_entity'(+entity_identifier)
%
% prints a message that an entity is being compiled

'$lgt_report_compiling_entity'(Entity) :-
	'$lgt_compiler_option'(report, on) ->
		nl, write('>>> compiling '), writeq(Entity),
		('$lgt_compiler_option'(debug, on) ->
			write(' in debug mode...')
			;
			write('...')),
		nl
		;
		true.



% '$lgt_report_compiled_entity'(+entity_identifier)
%
% prints a message that an entity is finished compiling

'$lgt_report_compiled_entity'(Entity) :-
	'$lgt_compiler_option'(report, on) ->
		write('>>> '), writeq(Entity), write(' compiled'), nl
		;
		true.



% '$lgt_report_loaded_entity'(+entity_identifier)
%
% prints a message that an entity finished loading

'$lgt_report_loaded_entity'(Entity) :-
	'$lgt_compiler_option'(report, on) ->
		write('<<< '), writeq(Entity), write(' loaded'), nl
		;
		true.



% '$lgt_compile_entities'(+list)
%
% compiles to disk a list of entities

'$lgt_compile_entities'([]).

'$lgt_compile_entities'([Entity| Entities]) :-
	'$lgt_compile_entity'(Entity),
	'$lgt_compile_entities'(Entities).



% '$lgt_compile_entity'(+atom)
%
% compiles to disk an entity

'$lgt_compile_entity'(Entity) :-
	'$lgt_compiler_option'(smart_compilation, on),
	\+ '$lgt_needs_recompilation'(Entity),
	!,
	'$lgt_report_up_to_date_entity'(Entity).

'$lgt_compile_entity'(Entity) :-
	'$lgt_report_compiling_entity'(Entity),
	'$lgt_clean_up',
	'$lgt_tr_entity'(Entity),
	'$lgt_write_tr_entity'(Entity),
	'$lgt_write_entity_doc'(Entity),
	'$lgt_report_unknown_entities',
	'$lgt_clean_up',
	'$lgt_report_compiled_entity'(Entity).



% '$lgt_needs_recompilation'(+atom)
%
% source file needs recompilation

'$lgt_needs_recompilation'(Entity) :-
	'$lgt_file_name'(prolog, Entity, File),
	\+ '$lgt_file_exists'(File).

'$lgt_needs_recompilation'(Entity) :-
	'$lgt_file_name'(xml, Entity, File),
	'$lgt_compiler_option'(xml, on),
	\+ '$lgt_file_exists'(File).

'$lgt_needs_recompilation'(Entity) :-
	'$lgt_file_name'(logtalk, Entity, Source),
	'$lgt_file_name'(prolog, Entity, Object),
	('$lgt_compare_file_mtimes'(Result, Source, Object) ->
		Result = (>)
		;
		true).



% '$lgt_write_tr_entity'(+atom)
%
% writes to disk the entity compiled code

'$lgt_write_tr_entity'(Entity) :-
	'$lgt_file_name'(prolog, Entity, File),
	catch(
		open(File, write, Stream),
		Error,
		'$lgt_compiler_error_handler'(Stream, Error)),
	catch(
		('$lgt_write_directives'(Stream),
		 '$lgt_write_clauses'(Stream),
		 '$lgt_write_init_call'(Stream)),
		Error,
		'$lgt_compiler_error_handler'(Stream, Error)),
	close(Stream).



% '$lgt_write_entity_doc'(+atom)
%
% writes to disk the entity documentation in XML format

'$lgt_write_entity_doc'(Entity) :-
	'$lgt_compiler_option'(xml, on) ->
		'$lgt_file_name'(xml, Entity, File),
		catch(
			open(File, write, Stream),
			Error,
			'$lgt_compiler_error_handler'(Stream, Error)),
		catch(
			'$lgt_write_xml_file'(Stream),
			Error,
			'$lgt_compiler_error_handler'(Stream, Error)),
		close(Stream)
		;
		true.



% '$lgt_file_name'(+atom, +atom, -atom)
%
% constructs a filename given the type of file and the entity name

'$lgt_file_name'(Type, Entity, File) :-
	'$lgt_file_extension'(Type, Extension),
	atom_concat(Entity, Extension, File).



% '$lgt_tr_entity'(+atom)
%
% compiles an entity storing the resulting code in memory

'$lgt_tr_entity'(Entity) :-
	'$lgt_file_name'(logtalk, Entity, File),
	catch(
		open(File, read, Stream),
		Error,
		'$lgt_compiler_error_handler'(Stream, Error)),
	'$lgt_save_op_table',
	catch(
		(read_term(Stream, Term, [singletons(Singletons1)]),
		 '$lgt_filter_dont_care_vars'(Singletons1, Singletons2),
		 '$lgt_report_singletons'(Singletons2, Term),
		 '$lgt_tr_file'(Stream, Term)),
		Error,
		'$lgt_compiler_error_handler'(Stream, Error)),
	'$lgt_restores_op_table',
	close(Stream),
	'$lgt_fix_redef_built_ins',
	'$lgt_find_misspelt_calls',
	'$lgt_entity_'(Type, _, _, _),
	'$lgt_gen_clauses'(Type),
	'$lgt_gen_directives'(Type).



% '$lgt_tr_file'(+stream, +term)

'$lgt_tr_file'(_, end_of_file) :-
	!.

'$lgt_tr_file'(Stream, Term) :-
	'$lgt_tr_term'(Term),
	read_term(Stream, Next, [singletons(Singletons1)]),
	'$lgt_filter_dont_care_vars'(Singletons1, Singletons2),
	'$lgt_report_singletons'(Singletons2, Next),
	'$lgt_tr_file'(Stream, Next).



% '$lgt_filter_dont_care_vars'(+list, -list)
%
% filter variables whose name start with an underscore from a
% singletons list if the corresponding compiler option sets their
% interpretation to don't care variables

'$lgt_filter_dont_care_vars'(List, Result) :-
	'$lgt_compiler_option'(underscore_vars, dont_care) ->
		'$lgt_filter_dont_care_vars'(List, [], Result)
		;
		List = Result.


'$lgt_filter_dont_care_vars'([], Result, Result).

'$lgt_filter_dont_care_vars'([Atom = Var| List], Sofar, Result) :-
	sub_atom(Atom, 0, 1, _, '_') ->
		'$lgt_filter_dont_care_vars'(List, Sofar, Result)
		;
		'$lgt_filter_dont_care_vars'(List, [Atom = Var| Sofar], Result).



% '$lgt_report_singletons'(+list, +term)
%
% report the singleton variables found while compiling an entity term

'$lgt_report_singletons'([], _).

'$lgt_report_singletons'([Singleton| Singletons], Term) :-
	('$lgt_compiler_option'(singletons, warning),
	 '$lgt_compiler_option'(report, on)) ->
		write('> WARNING!'),
		\+ \+ ( '$lgt_report_singletons_aux'([Singleton| Singletons], Term, Names),
				write('  singleton variables: '), write(Names), nl,
				(Term = (:- _) ->
					write('>           in directive: ')
					;
					write('>           in clause: ')),
				write(Term), nl)
		;
		true.


'$lgt_report_singletons_aux'([], _, []).

'$lgt_report_singletons_aux'([Name = Var| Singletons], Term, [Name| Names]) :-
	Name = Var,
	'$lgt_report_singletons_aux'(Singletons, Term, Names).



% '$lgt_compiler_error_handler'(@term, +term)
%
% closes the stream opened for reading the entity file, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_error_handler'(Stream, Error) :-
	(nonvar(Stream) ->
		close(Stream)
		;
		true),
	'$lgt_restores_op_table',
	'$lgt_report_compiler_error'(Error),
	throw(Error).



% '$lgt_report_compiler_error'(+term)
%
% reports a compilation error

'$lgt_report_compiler_error'(error(Error, directive(Directive))) :-
	!,
	write('> ERROR!  '), writeq(Error), nl,
	write('>         in directive: '), write((:- Directive)), nl.

'$lgt_report_compiler_error'(error(Error, clause(Clause))) :-
	!,
	write('> ERROR!  '), writeq(Error), nl,
	write('>         in clause: '), write(Clause), nl.

'$lgt_report_compiler_error'(error(Error, dcgrule(Rule))) :-
	!,
	write('> ERROR!  '), writeq(Error), nl,
	write('>         in grammar rule: '), write((Rule)), nl.

'$lgt_report_compiler_error'(error(Error, Term)) :-
	!,
	write('> ERROR!  '), writeq(Error), nl,
	write('>         in: '), write(Term), nl.

'$lgt_report_compiler_error'(Error) :-
	write('> ERROR!  '), writeq(Error), nl.



% clean up all dynamic predicates used during entity compilation

'$lgt_clean_up' :-
	retractall('$lgt_object_'(_, _, _, _, _, _, _, _, _)),
	retractall('$lgt_protocol_'(_, _, _)),
	retractall('$lgt_category_'(_, _, _, _)),
	retractall('$lgt_implemented_protocol_'(_, _, _, _)),
	retractall('$lgt_imported_category_'(_, _, _, _, _)),
	retractall('$lgt_extended_object_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_instantiated_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_specialized_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_extended_protocol_'(_, _, _, _)),
	retractall('$lgt_uses_'(_)),
	retractall('$lgt_calls_'(_)),
	retractall('$lgt_info_'(_)),
	retractall('$lgt_info_'(_, _)),
	retractall('$lgt_directive_'(_)),
	retractall('$lgt_public_'(_)),
	retractall('$lgt_protected_'(_)),
	retractall('$lgt_private_'(_)),
	retractall('$lgt_dynamic_'(_)),
	retractall('$lgt_discontiguous_'(_)),
	retractall('$lgt_mode_'(_, _)),
	retractall('$lgt_metapredicate_'(_)),
	retractall('$lgt_entity_functors_'(_)),
	retractall('$lgt_entity_'(_, _, _, _)),
	retractall('$lgt_entity_init_'(_)),
	retractall('$lgt_fentity_init_'(_)),
	retractall('$lgt_entity_comp_mode_'(_)),
	retractall('$lgt_dcl_'(_)),
	retractall('$lgt_ddcl_'(_)),
	retractall('$lgt_def_'(_)),
	retractall('$lgt_ddef_'(_)),
	retractall('$lgt_super_'(_)),
	retractall('$lgt_rclause_'(_)),
	retractall('$lgt_eclause_'(_)),
	retractall('$lgt_feclause_'(_)),
	retractall('$lgt_redefined_built_in_'(_, _, _)),
	retractall('$lgt_defs_pred_'(_)),
	retractall('$lgt_calls_pred_'(_)),
	retractall('$lgt_referenced_object_'(_)),
	retractall('$lgt_referenced_protocol_'(_)),
	retractall('$lgt_referenced_category_'(_)),
	retractall('$lgt_global_op_'(_, _, _)),
	retractall('$lgt_local_op_'(_, _, _)).



% dump all dynamic predicates used during entity compilation
% in the current ouput stream (just a debugging utility)
%
% only works on Prolog compilers implementing listing/1

'$lgt_dump_all' :-
	listing('$lgt_object_'/9),
	listing('$lgt_protocol_'/3),
	listing('$lgt_category_'/4),
	listing('$lgt_implemented_protocol_'/4),
	listing('$lgt_imported_category_'/5),
	listing('$lgt_extended_object_'/10),
	listing('$lgt_instantiated_class_'/10),
	listing('$lgt_specialized_class_'/10),
	listing('$lgt_extended_protocol_'/4),
	listing('$lgt_uses_'/1),
	listing('$lgt_calls_'/1),
	listing('$lgt_info_'/1),
	listing('$lgt_info_'/2),
	listing('$lgt_directive_'/1),
	listing('$lgt_public_'/1),
	listing('$lgt_protected_'/1),
	listing('$lgt_private_'/1),
	listing('$lgt_dynamic_'/1),
	listing('$lgt_discontiguous_'/1),
	listing('$lgt_mode_'/2),
	listing('$lgt_metapredicate_'/1),
	listing('$lgt_entity_functors_'/1),
	listing('$lgt_entity_'/4),
	listing('$lgt_entity_init_'/1),
	listing('$lgt_fentity_init_'/1),
	listing('$lgt_entity_comp_mode_'/1),
	listing('$lgt_dcl_'/1),
	listing('$lgt_ddcl_'/1),
	listing('$lgt_def_'/1),
	listing('$lgt_ddef_'/1),
	listing('$lgt_super_'/1),
	listing('$lgt_rclause_'/1),
	listing('$lgt_eclause_'/1),
	listing('$lgt_feclause_'/1),
	listing('$lgt_redefined_built_in_'/3),
	listing('$lgt_defs_pred_'/1),
	listing('$lgt_calls_pred_'/1),
	listing('$lgt_current_compiler_option_'/2),
	listing('$lgt_flag_'/2),
	listing('$lgt_referenced_object_'/1),
	listing('$lgt_referenced_protocol_'/1),
	listing('$lgt_referenced_category_'/1),
	listing('$lgt_global_op_'/3),
	listing('$lgt_local_op_'/3).



% '$lgt_save_op_table'
%
% saves current operator table

'$lgt_save_op_table' :-
	forall(
		current_op(Pr, Spec, Op),
		asserta('$lgt_global_op_'(Pr, Spec, Op))).



% '$lgt_restores_op_table'
%
% restores current operator table

'$lgt_restores_op_table' :-
	forall(
		retract('$lgt_local_op_'(_, Spec, Op)),
		op(0, Spec, Op)),
	retractall('$lgt_global_op_'(_, _, ',')),	% ','/2 cannot be an argument to op/3
	forall(
		retract('$lgt_global_op_'(Pr2, Spec2, Op2)),
		op(Pr2, Spec2, Op2)).



% '$lgt_assert_local_ops'(+integer, +operator_specifier, +atom_or_atom_list)
%
% asserts local operators

'$lgt_assert_local_ops'(_, _, []) :-
	!.

'$lgt_assert_local_ops'(Pr, Spec, [Op| Ops]) :-
	!,
	asserta('$lgt_local_op_'(Pr, Spec, Op)),
	'$lgt_assert_local_ops'(Pr, Spec, Ops).

'$lgt_assert_local_ops'(Pr, Spec, Op) :-
	asserta('$lgt_local_op_'(Pr, Spec, Op)).



% '$lgt_tr_terms'(+list)
%
% translates a list of entity terms (clauses and/or directives)

'$lgt_tr_terms'([]).

'$lgt_tr_terms'([Term| Terms]) :-
	'$lgt_tr_term'(Term),
	'$lgt_tr_terms'(Terms).



% '$lgt_tr_term'(+term)
%
% translates an entity term (either a clause or a directive)

'$lgt_tr_term'((Head :- Body)) :-
	!,
	'$lgt_tr_clause'((Head :- Body)).

'$lgt_tr_term'((:- Directive)) :-
	!,
	'$lgt_tr_directive'(Directive).

'$lgt_tr_term'((Head --> Body)) :-
	!,
	'$lgt_dcgrule_to_clause'((Head --> Body), Clause),
	'$lgt_tr_clause'(Clause).

'$lgt_tr_term'(Fact) :-
	'$lgt_tr_clause'(Fact).



% '$lgt_tr_directives'(+list)
%
% translates a list of entity directives

'$lgt_tr_directives'([]).

'$lgt_tr_directives'([Dir| Dirs]) :-
	'$lgt_tr_directive'(Dir),
	'$lgt_tr_directives'(Dirs).



% '$lgt_tr_directive'(+term)
%
% translates an entity directive

'$lgt_tr_directive'(Dir) :-
	var(Dir),
	throw(error(instantiantion_error, directive(Dir))).

'$lgt_tr_directive'(Dir) :-
	\+ '$lgt_entity_'(_, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	'$lgt_lgt_closing_directive'(Functor/Arity),	% opening directive missing/missplet
	throw(error(unmatched_directive, directive(Dir))).

'$lgt_tr_directive'(Dir) :-
	\+ '$lgt_entity_'(_, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	\+ '$lgt_lgt_opening_directive'(Functor/Arity),
	!,
	assertz('$lgt_directive_'(Dir)).

'$lgt_tr_directive'(Dir) :-
	functor(Dir, Functor, Arity),
	'$lgt_lgt_directive'(Functor/Arity),
	Dir =.. [Functor| Args],
	catch(
		'$lgt_tr_directive'(Functor, Args),
		Error,
		throw(error(Error, directive(Dir)))),
	!.

'$lgt_tr_directive'(Dir) :-
	throw(error(domain_error(directive, Dir), directive(Dir))).



% '$lgt_tr_directive'(+atom, +list)
%
% translates a directive and its (possibly empty) list of arguments

'$lgt_tr_directive'(object, [Obj| Rels]) :-
	'$lgt_valid_object_id'(Obj) ->
		'$lgt_tr_object_id'(Obj),
		'$lgt_tr_object_relations'(Rels, Obj)
		;
		throw(type_error(object_identifier, Obj)).

'$lgt_tr_directive'(end_object, []) :-
	'$lgt_entity_'(object, _, _, _).


'$lgt_tr_directive'(protocol, [Ptc| Rels]) :-
	'$lgt_valid_protocol_id'(Ptc) ->
		'$lgt_tr_protocol_id'(Ptc),
		'$lgt_tr_protocol_relations'(Rels, Ptc)
		;
		throw(type_error(protocol_identifier, Ptc)).


'$lgt_tr_directive'(end_protocol, []) :-
	'$lgt_entity_'(protocol, _, _, _).


'$lgt_tr_directive'(category, [Ctg| Rels]) :-
	'$lgt_valid_category_id'(Ctg) ->
		'$lgt_tr_category_id'(Ctg),
		'$lgt_tr_category_relations'(Rels, Ctg)
		;
		throw(type_error(category_identifier, Ctg)).


'$lgt_tr_directive'(end_category, []) :-
	'$lgt_entity_'(category, _, _, _).


% dynamic entity directive

'$lgt_tr_directive'((dynamic), []) :-
	!,
	assertz('$lgt_entity_comp_mode_'((dynamic))),
	'$lgt_entity_'(Type, _, _, _),
	'$lgt_update_entity_comp_mode'(Type).


'$lgt_tr_directive'(initialization, [Goal]) :-
	'$lgt_callable'(Goal) ->
		'$lgt_entity_'(_, Entity, Prefix, _),
		'$lgt_prefix'(Ctx, Prefix),
		'$lgt_metavars'(Ctx, []),
		'$lgt_sender'(Ctx, Entity),
		'$lgt_this'(Ctx, Entity),
		'$lgt_self'(Ctx, Entity),
		'$lgt_tr_body'(Goal, TGoal, _, Ctx),
		assertz('$lgt_entity_init_'(TGoal))
		;
		throw(type_error(callable, Goal)).


'$lgt_tr_directive'(op, [Pr, Spec, Ops]) :-
	'$lgt_valid_op_priority'(Pr) ->
		('$lgt_valid_op_specifier'(Spec) ->
			('$lgt_valid_op_names'(Ops) ->
				op(Pr, Spec, Ops),
				'$lgt_assert_local_ops'(Pr, Spec, Ops)
				;
				throw(type_error(operator_name, Ops)))
			;
			throw(type_error(operator_specifier, Spec)))
		;
		throw(type_error(operator_priority, Pr)).


'$lgt_tr_directive'(uses, Objs) :-
	'$lgt_convert_to_list'(Objs, Objs2),
	forall(
		'$lgt_member'(Obj, Objs2),
		('$lgt_valid_object_id'(Obj) ->
			'$lgt_add_referenced_object'(Obj),
			assertz('$lgt_uses_'(Obj))
			;
			throw(type_error(object_identifier, Obj)))).


'$lgt_tr_directive'(calls, Ptcs) :-
	'$lgt_convert_to_list'(Ptcs, Ptcs2),
	forall(
		'$lgt_member'(Ptc, Ptcs2),
		('$lgt_valid_protocol_id'(Ptc) ->
			'$lgt_add_referenced_protocol'(Ptc),
			assertz('$lgt_calls_'(Ptc))
			;
			throw(type_error(protocol_identifier, Ptc)))).


'$lgt_tr_directive'(info, [List]) :-
	!,
	('$lgt_valid_info_list'(List) ->
		assertz('$lgt_info_'(List))
		;
		throw(type_error(info_list, List))).


'$lgt_tr_directive'(info, [Pred, List]) :-
	'$lgt_valid_pred_ind'(Pred) ->
		('$lgt_valid_info_list'(List) ->
			assertz('$lgt_info_'(Pred, List))
			;
			throw(type_error(info_list, List)))
		;
		throw(type_error(predicate_indicator, Pred)).



'$lgt_tr_directive'((public), Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred) ->
			assertz('$lgt_public_'(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


'$lgt_tr_directive'(protected, Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred) ->
			assertz('$lgt_protected_'(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


'$lgt_tr_directive'(private, Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred) ->
			assertz('$lgt_private_'(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


'$lgt_tr_directive'((dynamic), Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred) ->
			assertz('$lgt_dynamic_'(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


'$lgt_tr_directive'((discontiguous), Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_pred_ind'(Pred) ->
			assertz('$lgt_discontiguous_'(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


'$lgt_tr_directive'(metapredicate, Preds) :-
	'$lgt_convert_to_list'(Preds, Preds2),
	forall(
		'$lgt_member'(Pred, Preds2),
		('$lgt_valid_metapred_term'(Pred) ->
			assertz('$lgt_metapredicate_'(Pred))
			;
			throw(type_error(metapredicate_term, Pred)))).


'$lgt_tr_directive'((mode), [Mode, Solutions]) :-
	'$lgt_valid_mode_term'(Mode) ->
		('$lgt_valid_number_of_solutions'(Solutions) ->
			assertz('$lgt_mode_'(Mode, Solutions))
			;
			throw(type_error(number_of_solutions, Solutions)))
		;
		throw(type_error(mode_term, Mode)).



% '$lgt_tr_object_relations'(+list, +term)
%
% translates the relations of an object with other entities

'$lgt_tr_object_relations'([], _).

'$lgt_tr_object_relations'([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Args],
	('$lgt_tr_object_relation'(Functor, Args, Obj) ->
		'$lgt_tr_object_relations'(Clauses, Obj)
		;
		throw(type_error(relation_clause, Functor))).



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
	'$lgt_tr_protocol_relation'(Functor, Args, Obj),
	'$lgt_tr_protocol_relations'(Clauses, Obj).



% '$lgt_tr_protocol_relation'(+atom, +list, +term)
%
% translates a relation between a protocol (the last argument) with other entities

'$lgt_tr_protocol_relation'(extends, Ptcs, Ptc) :-
	!,
	'$lgt_convert_to_list'(Ptcs, List),
	'$lgt_tr_extends_protocol'(List, Ptc).

'$lgt_tr_protocol_relation'(Unknown, _, _) :-
	throw(directive_error(relation_clause, Unknown)).



% '$lgt_tr_category_relations'(+list, +term)
%
% translates the relations of a category with other entities

'$lgt_tr_category_relations'([], _).

'$lgt_tr_category_relations'([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Args],
	'$lgt_tr_category_relation'(Functor, Args, Obj),
	'$lgt_tr_category_relations'(Clauses, Obj).



% '$lgt_tr_category_relation'(+atom, +list, +term)
%
% translates a relation between a category (the last argument) with other entities

'$lgt_tr_category_relation'(implements, Ptcs, Ctg) :-
	!,
	'$lgt_convert_to_list'(Ptcs, List),
	'$lgt_tr_implements_protocol'(List, Ctg).

'$lgt_tr_category_relation'(Unknown, _, _) :-
	throw(directive_error(relation_clause, Unknown)).



% '$lgt_tr_clauses'(+list)

'$lgt_tr_clauses'([]).

'$lgt_tr_clauses'([Clause| Clauses]) :-
	'$lgt_tr_clause'(Clause),
	'$lgt_tr_clauses'(Clauses).



% '$lgt_tr_clause'(+clause)

'$lgt_tr_clause'(Clause) :-
	\+ '$lgt_entity_'(_, _, _, _),		% clause occurs before opening entity directive
	!,
	assertz('$lgt_feclause_'(Clause)).

'$lgt_tr_clause'(Clause) :-
	'$lgt_entity_'(Type, Entity, Prefix, _),
	((Type = object, compound(Entity)) ->	% if the entity is a parametric object we need
		'$lgt_this'(Ctx, Entity)		% "this" for inline compilation of parameter/2
		;
		true),
	'$lgt_prefix'(Ctx, Prefix),
	catch(
		'$lgt_tr_clause'(Clause, TClause, DClause, Ctx),
		Error,
		throw(error(Error, clause(Clause)))),
	('$lgt_compiler_option'(debug, on) ->
		assertz('$lgt_eclause_'(DClause))
		;
		assertz('$lgt_eclause_'(TClause))),
	!.

'$lgt_tr_clause'(Clause) :-
	throw(error(unknown_error, clause(Clause))).



% '$lgt_tr_clause'(+clause, -clause, -clause, +term)

'$lgt_tr_clause'((Head:-_), _, _, _) :-
	\+ '$lgt_callable'(Head),
	throw(type_error(callable, Head)).

'$lgt_tr_clause'((Head:-Body), TClause, (THead:-'$lgt_dbg_head'(Head, Ctx),DBody), Ctx) :-
	functor(Head, Functor, Arity),
	'$lgt_dynamic_'(Functor/Arity),
	!,
	'$lgt_extract_metavars'(Head, Metavars),
	'$lgt_metavars'(Ctx, Metavars),
	'$lgt_tr_head'(Head, THead, Ctx),
	'$lgt_tr_body'(Body, TBody, DBody, Ctx),
	'$lgt_simplify_body'(TBody, SBody),
	TClause = (THead:-'$lgt_nop'(Body), SBody).

'$lgt_tr_clause'((Head:-Body), TClause, (THead:-'$lgt_dbg_head'(Head, Ctx),DBody), Ctx) :-
	!,
	'$lgt_extract_metavars'(Head, Metavars),
	'$lgt_metavars'(Ctx, Metavars),
	'$lgt_tr_head'(Head, THead, Ctx),
	'$lgt_tr_body'(Body, TBody, DBody, Ctx),
	'$lgt_simplify_body'(TBody, SBody),
	(SBody == true ->
		TClause = THead
		;
		TClause = (THead:-SBody)).

'$lgt_tr_clause'(Fact, _, _, _) :-
	\+ '$lgt_callable'(Fact),
	throw(type_error(callable, Fact)).

'$lgt_tr_clause'(Fact, TFact, (TFact:-'$lgt_dbg_fact'(Fact, Ctx)), Ctx) :-
	'$lgt_tr_head'(Fact, TFact, Ctx).



% '$lgt_tr_head'(+callable, -callable, +term)
%
% translates an entity clause head


% definition of dynamic predicates inside categories

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_entity_'(category, _, _, _),
	functor(Head, Functor, Arity), 
	'$lgt_dynamic_'(Functor/Arity),
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


% redefinition of Logtalk built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_lgt_built_in'(Head),
	'$lgt_compiler_option'(lgtredef, warning),
	'$lgt_compiler_option'(report, on),
	\+ '$lgt_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	write('> WARNING!  redefining a Logtalk built-in predicate: '),
	writeq(Functor/Arity), nl,
	fail.


% redefinition of Prolog built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pl_built_in'(Head),
	'$lgt_compiler_option'(plredef, warning),
	'$lgt_compiler_option'(report, on),
	\+ '$lgt_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	write('> WARNING!  redefining a Prolog built-in predicate: '),
	writeq(Functor/Arity), nl,
	fail.


% translate the head of a clause of a user defined predicate

'$lgt_tr_head'(Head, THead, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_prefix'(Ctx, EPrefix),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	(('$lgt_dynamic_'(Functor/Arity),
	  \+ '$lgt_public_'(Functor/Arity),
	  \+ '$lgt_protected_'(Functor/Arity),
	  \+ '$lgt_private_'(Functor/Arity)) ->
		'$lgt_add_ddef_clause'(Functor, Arity, PPrefix, Ctx)
		;
		'$lgt_add_def_clause'(Functor, Arity, PPrefix, Ctx)),
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
	'$lgt_append'(Args, [Sender, This, Self], Args2),
	THead =.. [PPrefix| Args2].



% '$lgt_tr_body'(+callable, -callable, -callable, +term)
%
% translates an entity clause body


% meta-calls

'$lgt_tr_body'(Pred, TPred, '$lgt_dbg_goal'(Pred, TPred, Ctx), Ctx) :-
	var(Pred),
	!,
	'$lgt_metavars'(Ctx, Metavars),
	('$lgt_member_var'(Pred, Metavars) ->
		'$lgt_sender'(Ctx, Sender),
		TPred = '$lgt_metacall_in_object'(Sender, Pred, Sender)
		;
		'$lgt_this'(Ctx, This),
		TPred = '$lgt_metacall_in_object'(This, Pred, This)).


% pre-processor bypass (call of external code)

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
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_body'(::Pred, TPred, '$lgt_dbg_goal'(::Pred, TPred, Ctx), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_body'(^^Pred, TPred, '$lgt_dbg_goal'(^^Pred, TPred, Ctx), Ctx) :-
	!,
	'$lgt_tr_super_sending'(Pred, TPred, Ctx).


% "reflection" built-in predicates

'$lgt_tr_body'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, _), '$lgt_dbg_goal'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, _), '$lgt_dbg_goal'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% database handling built-in predicates

'$lgt_tr_body'(abolish(Pred), '$lgt_abolish'(This, Pred, This, _), '$lgt_dbg_goal'(abolish(Pred), '$lgt_abolish'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(asserta(Pred), '$lgt_asserta'(This, Pred, This, _), '$lgt_dbg_goal'(asserta(Pred), '$lgt_asserta'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(assertz(Pred), '$lgt_assertz'(This, Pred, This, _), '$lgt_dbg_goal'(assertz(Pred), '$lgt_assertz'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(clause(Head, Body), '$lgt_clause'(This, Head, Body, This, _), '$lgt_dbg_goal'(clause(Head, Body), '$lgt_clause'(This, Head, Body, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(retract(Pred), '$lgt_retract'(This, Pred, This, _), '$lgt_dbg_goal'(retract(Pred), '$lgt_retract'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(retractall(Pred), '$lgt_retractall'(This, Pred, This, _), '$lgt_dbg_goal'(retractall(Pred), '$lgt_retractall'(This, Pred, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% DCG predicates

'$lgt_tr_body'(phrase(Ruleset, List), '$lgt_phrase'(This, Ruleset, List, This, _), '$lgt_dbg_goal'(phrase(Ruleset, List), '$lgt_phrase'(This, Ruleset, List, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_body'(phrase(Ruleset, List, Rest), '$lgt_phrase'(This, Ruleset, List, Rest, This, _), '$lgt_dbg_goal'(phrase(Ruleset, List, Rest), '$lgt_phrase'(This, Ruleset, List, Rest, This, _), Ctx), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% inline methods (translated to a single unification with the corresponding context argument)

'$lgt_tr_body'(sender(Sender), true, '$lgt_dbg_goal'(sender(Temp), Sender=Temp, Ctx), Ctx) :-
	'$lgt_sender'(Ctx, Sender),
	!.

'$lgt_tr_body'(this(This), true, '$lgt_dbg_goal'(this(Temp), This=Temp, Ctx), Ctx) :-
	'$lgt_this'(Ctx, This),
	!.

'$lgt_tr_body'(self(Self), true, '$lgt_dbg_goal'(self(Temp), Self=Temp, Ctx), Ctx) :-
	'$lgt_self'(Ctx, Self),
	!.

'$lgt_tr_body'(parameter(Arg, Value), TPred, '$lgt_dbg_goal'(parameter(Arg, Temp), DPred, Ctx), Ctx) :-
	'$lgt_this'(Ctx, This),
	(var(This) ->
		TPred = arg(Arg, This, Value),	% when using parameter/2 in categories
		DPred = (TPred, Temp=Value)
		;
		arg(Arg, This, Value),
		TPred = true,
		DPred = (Temp=Value)),
	!.


% term input predicates that need to be operator aware

'$lgt_tr_body'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), '$lgt_dbg_goal'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), '$lgt_dbg_goal'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), '$lgt_dbg_goal'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(read(Term), '$lgt_iso_read'(Term, Ops), '$lgt_dbg_goal'(read(Term), '$lgt_iso_read'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.


% term output predicates that need to be operator aware

'$lgt_tr_body'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), '$lgt_dbg_goal'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), Ctx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), '$lgt_dbg_goal'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), Ctx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), '$lgt_dbg_goal'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(write(Term), '$lgt_iso_write'(Term, Ops), '$lgt_dbg_goal'(write(Term), '$lgt_iso_write'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), '$lgt_dbg_goal'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.

'$lgt_tr_body'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), '$lgt_dbg_goal'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), Ctx), Ctx) :-
	bagof(op(Pr, Spec, Op), '$lgt_local_op_'(Pr, Spec, Op), Ops),
	!.


% Logtalk and Prolog built-in predicates

'$lgt_tr_body'(Pred, _, _, _) :-
	'$lgt_pl_built_in'(Pred),
	\+ '$lgt_iso_def_pred'(Pred),
	'$lgt_compiler_option'(portability, warning),
	'$lgt_compiler_option'(report, on),
	functor(Pred, Functor, Arity),
	write('> WARNING!  non-ISO defined built-in predicate call: '),
	writeq(Functor/Arity), nl,
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
	\+ '$lgt_callable'(Pred),
	throw(type_error(callable, Pred)).


% goal is a call to a user predicate

'$lgt_tr_body'(Cond, TCond, '$lgt_dbg_goal'(Cond, TCond, Ctx), Ctx) :-
	Cond =.. [Functor| Args],
	functor(Cond, Functor, Arity),
	'$lgt_prefix'(Ctx, EPrefix),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
	'$lgt_append'(Args, [Sender, This, Self], Args2),
	TCond =.. [PPrefix| Args2],
	assertz('$lgt_calls_pred_'(Functor/Arity)).



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



% '$lgt_tr_msg'(@object, @term, -term, +term)
%
% translates the sending of a message to an object


% message broadcasting

'$lgt_tr_msg'(Obj, Pred, TPred, Ctx) :-
	nonvar(Obj),
	(Obj = (_, _); Obj = (_; _)),
	!,
	'$lgt_tr_msg_broadcasting'(Obj, Pred, TPred, Ctx).


% invalid object identifier

'$lgt_tr_msg'(Obj, _, _, _) :-
	nonvar(Obj),
	\+ '$lgt_valid_object_id'(Obj),
	!,
	throw(type_error(object_identifier, Obj)).


% remember the object receiving the message to later check if it's known

'$lgt_tr_msg'(Obj, _, _, Ctx) :-
	nonvar(Obj),
	\+ '$lgt_sender'(Ctx, user),	% not runtime msg translation
	\+ '$lgt_this'(Ctx, user),
	'$lgt_add_referenced_object'(Obj),
	fail.


% non-instantiated message: traslation performed at runtime

'$lgt_tr_msg'(Obj, Pred, '$lgt_send_to_object'(Obj, Pred, This), Ctx) :-
	var(Pred),
	!,
	'$lgt_this'(Ctx, This).


% control constructs

'$lgt_tr_msg'(Obj, (Pred1, Pred2), (TPred1, TPred2), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred1, TPred1, Ctx),
	'$lgt_tr_msg'(Obj, Pred2, TPred2, Ctx).

'$lgt_tr_msg'(Obj, (Pred1; Pred2), (TPred1; TPred2), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred1, TPred1, Ctx),
	'$lgt_tr_msg'(Obj, Pred2, TPred2, Ctx).

'$lgt_tr_msg'(Obj, (Pred1 -> Pred2), (TPred1 -> TPred2), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred1, TPred1, Ctx),
	'$lgt_tr_msg'(Obj, Pred2, TPred2, Ctx).

'$lgt_tr_msg'(Obj, \+ Pred, \+ TPred, Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_msg'(Obj, !, ('$lgt_obj_exists'(Obj, !, This), !), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, true, ('$lgt_obj_exists'(Obj, true, This), true), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, fail, ('$lgt_obj_exists'(Obj, fail, This), fail), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, repeat, ('$lgt_obj_exists'(Obj, repeat, This), repeat), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, call(Pred), TPred, Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_msg'(Obj, once(Pred), once(TPred), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_msg'(Obj, catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Goal, TGoal, Ctx),
	'$lgt_tr_msg'(Obj, Recovery, TRecovery, Ctx).

'$lgt_tr_msg'(Obj, throw(Error), ('$lgt_obj_exists'(Obj, throw(Error), This), throw(Error)), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% built-in metapredicates

'$lgt_tr_msg'(Obj, bagof(Term, Pred, List), bagof(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_msg'(Obj, findall(Term, Pred, List), findall(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).

'$lgt_tr_msg'(Obj, forall(Gen, Test), forall(TGen, TTest), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Gen, TGen, Ctx),
	'$lgt_tr_msg'(Obj, Test, TTest, Ctx).

'$lgt_tr_msg'(Obj, setof(Term, Pred, List), setof(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj, Pred, TPred, Ctx).


% "reflection" built-in predicates

'$lgt_tr_msg'(Obj, current_predicate(Pred), '$lgt_current_predicate'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, predicate_property(Pred, Prop), '$lgt_predicate_property'(Obj, Pred, Prop, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% database handling built-in predicates

'$lgt_tr_msg'(Obj, abolish(Pred), '$lgt_abolish'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, asserta(Pred), '$lgt_asserta'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, assertz(Pred), '$lgt_assertz'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, clause(Head, Body), '$lgt_clause'(Obj, Head, Body, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, retract(Pred), '$lgt_retract'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, retractall(Pred), '$lgt_retractall'(Obj, Pred, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% DCG predicates

'$lgt_tr_msg'(Obj, phrase(Ruleset, List), '$lgt_phrase'(Obj, Ruleset, List, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, phrase(Ruleset, List, Rest), '$lgt_phrase'(Obj, Ruleset, List, Rest, This, p(p(p))), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).


% invalid goal

'$lgt_tr_msg'(_, Pred, _, _) :-
	\+ '$lgt_callable'(Pred),
	throw(type_error(callable, Pred)).


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_msg'(Obj, Pred, '$lgt_send_to_object'(Obj, Pred, This), Ctx) :-
	var(Obj),
	!,
	'$lgt_this'(Ctx, This).

'$lgt_tr_msg'(Obj, Pred, '$lgt_send_to_object_nv'(Obj, Pred, This), Ctx) :-
	!,
	'$lgt_this'(Ctx, This).



% '$lgt_tr_self_msg'(@term, -term, +term)
%
% translates the sending of a message to self


% non-instantiated message: traslation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self'(Self, Pred, This), Ctx) :-
	var(Pred),
	!,
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self).


% control constructs

'$lgt_tr_self_msg'((Pred1, Pred2), (TPred1, TPred2), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, Ctx),
	'$lgt_tr_self_msg'(Pred2, TPred2, Ctx).

'$lgt_tr_self_msg'(((Pred1; Pred2)), (TPred1; TPred2), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, Ctx),
	'$lgt_tr_self_msg'(Pred2, TPred2, Ctx).

'$lgt_tr_self_msg'((Pred1 -> Pred2), (TPred1 -> TPred2), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, Ctx),
	'$lgt_tr_self_msg'(Pred2, TPred2, Ctx).

'$lgt_tr_self_msg'(\+ Pred, \+ TPred, Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_self_msg'(!, !, _) :-
	!.

'$lgt_tr_self_msg'(true, true, _) :-
	!.

'$lgt_tr_self_msg'(fail, fail, _) :-
	!.

'$lgt_tr_self_msg'(repeat, repeat, _) :-
	!.

'$lgt_tr_self_msg'(call(Pred), TPred, Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_self_msg'(once(Pred), once(TPred), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_self_msg'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Goal, TGoal, Ctx),
	'$lgt_tr_self_msg'(Recovery, TRecovery, Ctx).

'$lgt_tr_self_msg'(throw(Error), throw(Error), _) :-
	!.


% built-in metapredicates

'$lgt_tr_self_msg'(bagof(Term, Pred, List), bagof(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_self_msg'(findall(Term, Pred, List), findall(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).

'$lgt_tr_self_msg'(forall(Gen, Test), forall(TGen, TTest), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Gen, TGen, Ctx),
	'$lgt_tr_self_msg'(Test, TTest, Ctx).

'$lgt_tr_self_msg'(setof(Term, Pred, List), setof(Term, TPred, List), Ctx) :-
	!,
	'$lgt_tr_self_msg'(Pred, TPred, Ctx).


% "reflection" built-in predicates

'$lgt_tr_self_msg'(current_predicate(Pred), '$lgt_current_predicate'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(predicate_property(Pred, Prop), '$lgt_predicate_property'(Self, Pred, Prop, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).


% database handling built-in predicates

'$lgt_tr_self_msg'(abolish(Pred), '$lgt_abolish'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(asserta(Pred), '$lgt_asserta'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(assertz(Pred), '$lgt_assertz'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(clause(Head, Body), '$lgt_clause'(Self, Head, Body, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(retract(Pred), '$lgt_retract'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(retractall(Pred), '$lgt_retractall'(Self, Pred, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).


% DCG predicates

'$lgt_tr_self_msg'(phrase(Ruleset, List), '$lgt_phrase'(Self, Ruleset, List, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).

'$lgt_tr_self_msg'(phrase(Ruleset, List, Rest), '$lgt_phrase'(Self, Ruleset, List, Rest, This, p(_)), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).


% invalid goal

'$lgt_tr_self_msg'(Pred, _, _) :-
	\+ '$lgt_callable'(Pred),
	throw(type_error(callable, Pred)).


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self_nv'(Self, Pred, This), Ctx) :-
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This).



% message broadcasting

'$lgt_tr_msg_broadcasting'((Obj1, Obj2), Pred, (TP1, TP2), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj1, Pred, TP1, Ctx),
	'$lgt_tr_msg'(Obj2, Pred, TP2, Ctx).

'$lgt_tr_msg_broadcasting'((Obj1; Obj2), Pred, (TP1; TP2), Ctx) :-
	!,
	'$lgt_tr_msg'(Obj1, Pred, TP1, Ctx),
	'$lgt_tr_msg'(Obj2, Pred, TP2, Ctx).



% '$lgt_tr_super_sending'(@term, -term, +term)
%
% translates calling of redefined predicates (super calls)


% invalid goal

'$lgt_tr_super_sending'(Pred, _, _) :-
	nonvar(Pred),
	\+ '$lgt_callable'(Pred),
	throw(type_error(callable, Pred)).


% translation performed at runtime

'$lgt_tr_super_sending'(Pred, '$lgt_send_to_super'(Self, Pred, This, Sender), Ctx) :-
	var(Pred),
	!,
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This),
	'$lgt_sender'(Ctx, Sender).

'$lgt_tr_super_sending'(Pred, '$lgt_send_to_super_nv'(Self, Pred, This, Sender), Ctx) :-
	'$lgt_self'(Ctx, Self),
	'$lgt_this'(Ctx, This),
	'$lgt_sender'(Ctx, Sender).



% '$lgt_extract_metavars'(+callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

'$lgt_extract_metavars'(Pred, Metavars) :-
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	('$lgt_metapredicate_'(Meta) ->
		Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		'$lgt_extract_metavars'(Args, MArgs, Metavars)
		;
		Metavars = []).


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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
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
		 '$lgt_remove_operators'(Ops)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_save_operators'(@list, -list)
%
% save currently defined operators that might be
% redefined when a list of operators is added

'$lgt_save_operators'(Ops, Saved) :-
	findall(
		op(Pr, Spec, Op),
		('$lgt_member'(op(_, _, Op), Ops),
		 current_op(Pr, Spec, Op)),
		Saved).



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



% '$lgt_tr_object_id'(+object_identifier)
%
% from the object identifier construct the set of 
% functor prefixes used in the compiled code clauses

'$lgt_tr_object_id'(Obj) :-
	'$lgt_add_referenced_object'(Obj),
	'$lgt_construct_object_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
	assertz('$lgt_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef)),
	Term =.. [Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef],
	assertz('$lgt_entity_functors_'(Term)),
	assertz('$lgt_rclause_'('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, static))),
	assertz('$lgt_entity_'(object, Obj, Prefix, Dcl)).



% '$lgt_tr_category_id'(+category_identifier)
%
% from the category identifier construct the set of 
%  functor prefixes used in the compiled code clauses

'$lgt_tr_category_id'(Ctg) :-
	'$lgt_add_referenced_category'(Ctg),
	'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def),
	assertz('$lgt_category_'(Ctg, Prefix, Dcl, Def)),
	Term =.. [Prefix, Dcl, Def],
	assertz('$lgt_entity_functors_'(Term)),
	assertz('$lgt_rclause_'('$lgt_current_category_'(Ctg, Prefix, static))),
	assertz('$lgt_entity_'(category, Ctg, Prefix, Dcl)).



% '$lgt_tr_protocol_id'(+protocol_identifier)
%
% from the protocol identifier construct the set of  
% functor prefixes used in the compiled code clauses

'$lgt_tr_protocol_id'(Ptc) :-
	'$lgt_add_referenced_protocol'(Ptc),
	'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl),
	assertz('$lgt_protocol_'(Ptc, Prefix, Dcl)),
	Term =.. [Prefix, Dcl],
	assertz('$lgt_entity_functors_'(Term)),
	assertz('$lgt_rclause_'('$lgt_current_protocol_'(Ptc, Prefix, static))),
	assertz('$lgt_entity_'(protocol, Ptc, Prefix, Dcl)).



% '$lgt_update_entity_comp_mode'(+atom)
%
% updates the entity compilation mode to "dynamic"
% (entities are static by default)

'$lgt_update_entity_comp_mode'(object) :-
	retract('$lgt_rclause_'('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, _))),
	assertz('$lgt_rclause_'('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, (dynamic)))).

'$lgt_update_entity_comp_mode'(protocol) :-
	retract('$lgt_rclause_'('$lgt_current_protocol_'(Ptc, Prefix, _))),
	assertz('$lgt_rclause_'('$lgt_current_protocol_'(Ptc, Prefix, (dynamic)))).

'$lgt_update_entity_comp_mode'(category) :-
	retract('$lgt_rclause_'('$lgt_current_category_'(Ctg, Prefix, _))),
	assertz('$lgt_rclause_'('$lgt_current_category_'(Ctg, Prefix, (dynamic)))).



% '$lgt_tr_implements_protocol'(+list, +object_identifier)
% '$lgt_tr_implements_protocol'(+list, +category_identifier)
%
% translates an "implementents" relation between 
%  a category or an object and a list of protocols

'$lgt_tr_implements_protocol'([], _).

'$lgt_tr_implements_protocol'([Ref| Refs], ObjOrCtg) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Ptc),
		 ('$lgt_valid_protocol_id'(Ptc) ->
		 	'$lgt_add_referenced_protocol'(Ptc),
			assertz('$lgt_rclause_'('$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope))),
			'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl),
			assertz('$lgt_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)),
			'$lgt_tr_implements_protocol'(Refs, ObjOrCtg)
			;
			throw(type_error(protocol_identifier, Ptc))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_imports_category'(+list, +object_identifier)
%
% translates an "imports" relation between 
% an object and a list of categories 

'$lgt_tr_imports_category'([], _).

'$lgt_tr_imports_category'([Ref| Refs], Obj) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Ctg),
		 ('$lgt_valid_category_id'(Ctg) ->
		 	'$lgt_add_referenced_category'(Ctg),
			assertz('$lgt_rclause_'('$lgt_imports_category_'(Obj, Ctg, Scope))),
			'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def),
			assertz('$lgt_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)),
			'$lgt_tr_imports_category'(Refs, Obj)
			;
			throw(type_error(category_identifier, Ctg))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_instantiates_class'(+list, +object_identifier)
%
% translates an "instantiates" relation between 
% an instance and a list of classes

'$lgt_tr_instantiates_class'([], _).

'$lgt_tr_instantiates_class'([Ref| Refs], Obj) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Class),
		 ('$lgt_valid_object_id'(Class) ->
		 	'$lgt_add_referenced_object'(Class),
			assertz('$lgt_rclause_'('$lgt_instantiates_class_'(Obj, Class, Scope))),
			'$lgt_construct_object_functors'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz('$lgt_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_instantiates_class'(Refs, Obj)
			;
			throw(type_error(object_identifier, Class))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_specializes_class'(+list, +object_identifier)
%
% translates a "specializes" relation between 
% a class and a list of superclasses

'$lgt_tr_specializes_class'([], _).

'$lgt_tr_specializes_class'([Ref| Refs], Class) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Superclass),
		 ('$lgt_valid_object_id'(Superclass) ->
		 	'$lgt_add_referenced_object'(Superclass),
			assertz('$lgt_rclause_'('$lgt_specializes_class_'(Class, Superclass, Scope))),
			'$lgt_construct_object_functors'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz('$lgt_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_specializes_class'(Refs, Class)
			;
			throw(type_error(object_identifier, Superclass))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_extends_object'(+list, +object_identifier)
%
% translates an "extends" relation between 
% a prototype and a list of parents

'$lgt_tr_extends_object'([], _).

'$lgt_tr_extends_object'([Ref| Refs], Obj) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Parent),
		 ('$lgt_valid_object_id'(Parent) ->
		 	'$lgt_add_referenced_object'(Parent),
			assertz('$lgt_rclause_'('$lgt_extends_object_'(Obj, Parent, Scope))),
			'$lgt_construct_object_functors'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz('$lgt_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			'$lgt_tr_extends_object'(Refs, Obj)
			;
			throw(type_error(object_identifier, Parent))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_tr_extends_protocol'(+list, +protocol_identifier)
%
% translates an "extends" relation between 
% a protocol and a list of protocols

'$lgt_tr_extends_protocol'([], _).

'$lgt_tr_extends_protocol'([Ref| Refs], Ptc1) :-
	'$lgt_valid_scope'(Ref) ->
		('$lgt_scope_id'(Ref, Scope, Ptc2),
		 ('$lgt_valid_protocol_id'(Ptc2) ->
		 	'$lgt_add_referenced_protocol'(Ptc2),
			assertz('$lgt_rclause_'('$lgt_extends_protocol_'(Ptc1, Ptc2, Scope))),
			'$lgt_construct_protocol_functors'(Ptc2, Prefix, Dcl),
			assertz('$lgt_extended_protocol_'(Ptc2, Prefix, Dcl, Scope)),
			'$lgt_tr_extends_protocol'(Refs, Ptc1)
			;
			throw(type_error(protocol_identifier, Ptc2))))
		;
		throw(type_error(scope, Ref)).



% '$lgt_add_referenced_object'(+object_identifier)
%
% assert the name of an object referenced by the entity that we are compiling

'$lgt_add_referenced_object'(Obj) :-
	'$lgt_referenced_object_'(Obj) ->
		true
		;
		assertz('$lgt_referenced_object_'(Obj)).



% '$lgt_add_referenced_protocol'(+protocol_identifier)
%
% assert the name of a protocol referenced by the entity that we are compiling

'$lgt_add_referenced_protocol'(Ptc) :-
	'$lgt_referenced_protocol_'(Ptc) ->
		true
		;
		assertz('$lgt_referenced_protocol_'(Ptc)).



% '$lgt_add_referenced_category'(+category_identifier)
%
% assert the name of a category referenced by the entity that we are compiling

'$lgt_add_referenced_category'(Ctg) :-
	'$lgt_referenced_category_'(Ctg) ->
		true
		;
		assertz('$lgt_referenced_category_'(Ctg)).



% '$lgt_report_unknown_entities'
%
% report any unknown referenced entities found while compiling an entity
% (if the corresponding compiler option is not set to "silent")

'$lgt_report_unknown_entities' :-
	('$lgt_compiler_option'(unknown, warning),
	 '$lgt_compiler_option'(report, on)) ->
		'$lgt_report_unknown_objects',
		'$lgt_report_unknown_protocols',
		'$lgt_report_unknown_categories'
		;
		true.	



% '$lgt_report_unknown_objects'
%
% report any unknown referenced objects found while compiling an entity

'$lgt_report_unknown_objects' :-
	findall(
		Obj,
		('$lgt_referenced_object_'(Obj), \+ ('$lgt_current_object_'(Obj, _, _, _, _, _); '$lgt_entity_'(_, Obj, _, _))),
		Objs),
	(Objs \= [] ->
		write('> WARNING!  references to unknown objects:    '), writeq(Objs), nl
		;
		true).



% '$lgt_report_unknown_protocols'
%
% report any unknown referenced protocols found while compiling an entity

'$lgt_report_unknown_protocols' :-
	findall(
		Ptc,
		('$lgt_referenced_protocol_'(Ptc), \+ ('$lgt_current_protocol_'(Ptc, _, _); '$lgt_entity_'(_, Ptc, _, _))),
		Ptcs),
	(Ptcs \= [] ->
		write('> WARNING!  references to unknown protocols:  '), writeq(Ptcs), nl
		;
		true).



% '$lgt_report_unknown_categories'
%
% report any unknown referenced categories found while compiling an entity

'$lgt_report_unknown_categories' :-
	findall(
		Ctg,
		('$lgt_referenced_category_'(Ctg), \+ ('$lgt_current_category_'(Ctg, _, _); '$lgt_entity_'(_, Ctg, _, _))),
		Ctgs),
	(Ctgs \= [] ->
		write('> WARNING!  references to unknown categories: '), writeq(Ctgs), nl
		;
		true).



% '$lgt_add_def_clause'(+atom, +integer, +atom, +term)
%
% adds a "def clause" (used to translate a predicate call)

'$lgt_add_def_clause'(Functor, Arity, Prefix, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
	'$lgt_append'(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	once(
		('$lgt_object_'(_, _, _, Def, _, _, _, _, _);
		 '$lgt_category_'(_, _, _, Def))),
	Clause =.. [Def, Head, Sender, This, Self, THead],
	('$lgt_def_'(Clause) ->
		true
		;
		assertz('$lgt_def_'(Clause))),
	('$lgt_built_in'(Head) ->
		assertz('$lgt_redefined_built_in_'(Head, Ctx, THead))
		;
		true),
	('$lgt_defs_pred_'(Functor/Arity) ->
		true
		;
		assertz('$lgt_defs_pred_'(Functor/Arity))).



% '$lgt_add_ddef_clause'(+atom, +integer, +atom, +term)
%
% adds a "ddef clause" (used to translate a predicate call)

'$lgt_add_ddef_clause'(Functor, Arity, Prefix, Ctx) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	'$lgt_sender'(Ctx, Sender),
	'$lgt_this'(Ctx, This),
	'$lgt_self'(Ctx, Self),
	'$lgt_append'(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	'$lgt_object_'(_, _, _, _, _, _, _, _, DDef),
	Clause =.. [DDef, Head, Sender, This, Self, THead],
	('$lgt_ddef_'(Clause) ->
		true
		;
		assertz('$lgt_ddef_'(Clause))),
	('$lgt_built_in'(Head) ->
		assertz('$lgt_redefined_built_in_'(Head, Ctx, THead))
		;
		true),
	('$lgt_defs_pred_'(Functor/Arity) ->
		true
		;
		assertz('$lgt_defs_pred_'(Functor/Arity))).



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
	assertz(Clause).



% '$lgt_update_ddef_table'(+atom, +callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% if there are no more clauses for the predicate otherwise does nothing

'$lgt_update_ddef_table'(DDef, Call) :-
	functor(Call, Functor, Arity),
	functor(Call2, Functor, Arity),
	(clause(Call2, _) ->
		true
		;
		Clause =.. [DDef, _, _, _, _, Call2],
		retractall(Clause)).



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



% '$lgt_gen_directives'(+atom)
%
% generates entity directives

'$lgt_gen_directives'(object) :-
	'$lgt_gen_object_directives'.

'$lgt_gen_directives'(category) :-
	'$lgt_gen_category_directives'.

'$lgt_gen_directives'(protocol) :-
	'$lgt_gen_protocol_directives'.



'$lgt_gen_object_directives' :-
	'$lgt_gen_object_dynamic_directives',
	'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_directives' :-
	'$lgt_gen_category_dynamic_directives',
	'$lgt_gen_category_discontiguous_directives'.



'$lgt_gen_protocol_directives' :-
	'$lgt_entity_comp_mode_'((dynamic)) ->
		'$lgt_protocol_'(_, Prefix, Dcl),
		assertz('$lgt_directive_'(dynamic(Prefix/1))),
		assertz('$lgt_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_directive_'(dynamic(Dcl/5)))
		;
		true.



'$lgt_gen_object_dynamic_directives' :-
	'$lgt_entity_comp_mode_'((dynamic)) ->
		'$lgt_gen_dynamic_object_dynamic_directives'
		;
		'$lgt_gen_static_object_dynamic_directives'.



'$lgt_gen_dynamic_object_dynamic_directives' :-
	'$lgt_object_'(_, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
	assertz('$lgt_directive_'(dynamic(Prefix/7))),
	assertz('$lgt_directive_'(dynamic(Dcl/4))),
	assertz('$lgt_directive_'(dynamic(Dcl/6))),
	assertz('$lgt_directive_'(dynamic(Def/5))),
	assertz('$lgt_directive_'(dynamic(Def/6))),
	assertz('$lgt_directive_'(dynamic(Super/6))),
	assertz('$lgt_directive_'(dynamic(IDcl/6))),
	assertz('$lgt_directive_'(dynamic(IDef/6))),
	assertz('$lgt_directive_'(dynamic(DDcl/2))),
	assertz('$lgt_directive_'(dynamic(DDef/5))),
	forall(
		('$lgt_def_'(Clause), Clause \= (_ :- _)),
		(arg(5, Clause, Call), functor(Call, Functor, Arity),
		 assertz('$lgt_directive_'(dynamic(Functor/Arity))))).



'$lgt_gen_static_object_dynamic_directives' :-
	'$lgt_object_'(_, _, _, Def, _, _, _, DDcl, DDef),
	assertz('$lgt_directive_'(dynamic(DDcl/2))),
	assertz('$lgt_directive_'(dynamic(DDef/5))),
	'$lgt_dynamic_'(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	'$lgt_def_'(Clause),
	functor(TPred, TFunctor, TArity),
	assertz('$lgt_directive_'(dynamic(TFunctor/TArity))),
	fail.

'$lgt_gen_static_object_dynamic_directives'.



'$lgt_gen_object_discontiguous_directives' :-
	'$lgt_object_'(_, _, _, Def, _, _, _, _, _),
	'$lgt_discontiguous_'(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	'$lgt_def_'(Clause),
	functor(TPred, TFunctor, TArity),
	assertz('$lgt_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_dynamic_directives' :-
	'$lgt_entity_comp_mode_'((dynamic)) ->
		'$lgt_category_'(_, Prefix, Dcl, Def),
		assertz('$lgt_directive_'(dynamic(Prefix/2))),
		assertz('$lgt_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_directive_'(dynamic(Def/5))),
		forall(
			('$lgt_def_'(Clause), Clause \= (_ :- _)),
			(arg(5, Clause, Call), functor(Call, Functor, Arity),
		 	 assertz('$lgt_directive_'(dynamic(Functor/Arity)))))
		 ;
		 true.



'$lgt_gen_category_discontiguous_directives' :-
	'$lgt_category_'(_, _, _, Def),
	'$lgt_discontiguous_'(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	'$lgt_def_'(Clause),
	functor(TPred, TFunctor, TArity),
	assertz('$lgt_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_category_discontiguous_directives'.



% '$lgt_gen_clauses'(+atom)

'$lgt_gen_clauses'(object) :-
	'$lgt_gen_object_clauses'.

'$lgt_gen_clauses'(protocol) :-
	'$lgt_gen_protocol_clauses'.

'$lgt_gen_clauses'(category) :-
	'$lgt_gen_category_clauses'.



'$lgt_gen_object_clauses' :-
	('$lgt_rclause_'('$lgt_instantiates_class_'(_, _, _));
	 '$lgt_rclause_'('$lgt_specializes_class_'(_, _, _))) ->
		'$lgt_gen_ic_clauses'
		;
		'$lgt_gen_prototype_clauses'.



% '$lgt_gen_local_dcl_clauses'
%
% a (local) predicate declaration is only generated
% if there is a scope declaration for the predicate

'$lgt_gen_local_dcl_clauses' :-
	'$lgt_entity_'(_, _, _, Dcl),
	(('$lgt_public_'(Functor/Arity), Scope = p(p(p)));
	 ('$lgt_protected_'(Functor/Arity), Scope = p(p));
	 ('$lgt_private_'(Functor/Arity), Scope = p)),
	functor(Meta, Functor, Arity),
	('$lgt_metapredicate_'(Meta) ->
		Meta2 = Meta
		;
		Meta2 = no),
	functor(Pred, Functor, Arity),
	('$lgt_dynamic_'(Functor/Arity)->
		Compilation = (dynamic)
		;
		Compilation = static),
	Fact =.. [Dcl, Pred, Scope, Compilation, Meta2],
	assertz('$lgt_dcl_'(Fact)),
	fail.

'$lgt_gen_local_dcl_clauses'.



'$lgt_gen_catchall_dcl_clause' :-
	\+ '$lgt_dcl_'(_) ->
		'$lgt_entity_'(_, _, _, Dcl),
		Head =.. [Dcl, _, _, _, _],
		assertz('$lgt_dcl_'((Head:-fail)))
		;
		true.



% '$lgt_gen_local_def_clauses'
%
% generates local def clauses for undefined but declared (via scope or
% dynamic directives) predicates

'$lgt_gen_local_def_clauses' :-
	'$lgt_entity_'(_, _, EPrefix, _),
	'$lgt_dynamic_'(Functor/Arity),
	\+ '$lgt_defs_pred_'(Functor/Arity),
	'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix),
	'$lgt_context'(Ctx),
	((\+ '$lgt_public_'(Functor/Arity),
	  \+ '$lgt_protected_'(Functor/Arity),
	  \+ '$lgt_private_'(Functor/Arity)) ->
		'$lgt_add_ddef_clause'(Functor, Arity, PPrefix, Ctx)
		;
		'$lgt_add_def_clause'(Functor, Arity, PPrefix, Ctx)),
	fail.

'$lgt_gen_local_def_clauses'.



'$lgt_gen_obj_catchall_def_clause' :-
	\+ '$lgt_def_'(_) ->
		'$lgt_object_'(_, _, _, Def, _, _, _, _, _),
		Head =.. [Def, _, _, _, _, _],
		assertz('$lgt_def_'((Head:-fail)))
		;
		true.



'$lgt_gen_protocol_clauses' :-
	'$lgt_gen_protocol_local_clauses',
	'$lgt_gen_protocol_linking_clauses',
	'$lgt_gen_protocol_extend_clauses'.



'$lgt_gen_protocol_local_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_catchall_dcl_clause'.



'$lgt_gen_protocol_linking_clauses' :-
	'$lgt_protocol_'(Ptc, _, PDcl),
	Head =.. [PDcl, Pred, Scope, Compilation, Meta, Ptc],
	Body =.. [PDcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_dcl_'((Head:-Body))).



'$lgt_gen_protocol_extend_clauses' :-
	'$lgt_protocol_'(_, _, PDcl1),
	Head =.. [PDcl1, Pred, Scope, Compilation, Meta, Ctn],
	'$lgt_extended_protocol_'(_, _, PDcl2, EScope),
	(EScope = (public) ->
		Body =.. [PDcl2, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl2, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl2, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_protocol_extend_clauses'.



'$lgt_gen_category_clauses' :-
	'$lgt_gen_category_dcl_clauses',
	'$lgt_gen_category_def_clauses'.



'$lgt_gen_category_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_catchall_dcl_clause',
	'$lgt_gen_category_linking_dcl_clauses',
	'$lgt_gen_category_implements_dcl_clauses'.



'$lgt_gen_category_linking_dcl_clauses' :-
	'$lgt_category_'(Ctg, _, CDcl, _),
	Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctg],
	Body =.. [CDcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_dcl_'((Head:-Body))).



'$lgt_gen_category_implements_dcl_clauses' :-
	'$lgt_category_'(_, _, CDcl, _),
	Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn],
	'$lgt_implemented_protocol_'(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_category_implements_dcl_clauses'.



'$lgt_gen_category_def_clauses' :-
	'$lgt_gen_category_catchall_def_clause'.



'$lgt_gen_category_catchall_def_clause' :-
	\+ '$lgt_def_'(_) ->
		'$lgt_category_'(_, _, _, Def),
		Head =.. [Def, _, _, _, _, _],
		assertz('$lgt_def_'((Head:-fail)))
		;
		true.



'$lgt_gen_prototype_clauses' :-
	'$lgt_gen_prototype_dcl_clauses',
	'$lgt_gen_prototype_def_clauses',
	'$lgt_gen_prototype_super_clauses'.



'$lgt_gen_prototype_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses',
	'$lgt_gen_catchall_dcl_clause',
	'$lgt_gen_prototype_linking_dcl_clauses',
	'$lgt_gen_prototype_implements_dcl_clauses',
	'$lgt_gen_prototype_imports_dcl_clauses',
	'$lgt_gen_prototype_extends_dcl_clauses'.



'$lgt_gen_prototype_linking_dcl_clauses' :-
	'$lgt_object_'(Obj, _, Dcl, _, _, _, _, DDcl, _),
	Head =.. [Dcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_dcl_'((Head:-Body))),
	Head2 =.. [Dcl, Pred, Scope, (dynamic), no, Obj, Obj],
	Body2 =.. [DDcl, Pred, Scope],
	assertz('$lgt_dcl_'((Head2:-Body2))).



'$lgt_gen_prototype_implements_dcl_clauses' :-
	'$lgt_object_'(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
	'$lgt_implemented_protocol_'(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_prototype_implements_dcl_clauses'.



'$lgt_gen_prototype_imports_dcl_clauses' :-
	'$lgt_object_'(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
	'$lgt_imported_category_'(_, _, CDcl, _, EScope),
	(EScope = (public) ->
		Body =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [CDcl, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_prototype_imports_dcl_clauses'.



'$lgt_gen_prototype_extends_dcl_clauses' :-
	'$lgt_object_'(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
	'$lgt_extended_object_'(_, _, PDcl, _, _, _, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Body = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_prototype_extends_dcl_clauses'.



'$lgt_gen_prototype_def_clauses' :-
	'$lgt_gen_local_def_clauses',
	'$lgt_gen_obj_catchall_def_clause',
	'$lgt_gen_prototype_linking_def_clauses',
	'$lgt_gen_prototype_imports_def_clauses',
	'$lgt_gen_prototype_extends_def_clauses'.



'$lgt_gen_prototype_linking_def_clauses' :-
	'$lgt_object_'(Obj, _, _, Def, _, _, _, _, DDef),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body2))).



'$lgt_gen_prototype_imports_def_clauses' :-
	'$lgt_object_'(Obj, _, _, ODef, _, _, _, _, _),
	'$lgt_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctg],
	'$lgt_imported_category_'(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),	
	fail.

'$lgt_gen_prototype_imports_def_clauses'.



'$lgt_gen_prototype_extends_def_clauses' :-
	'$lgt_object_'(Obj, _, _, ODef, _, _, _, _, _),
	'$lgt_rclause_'('$lgt_extends_object_'(Obj, Parent, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
	'$lgt_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	Body =.. [PDef, Pred, Sender, Parent, Self, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
	fail.

'$lgt_gen_prototype_extends_def_clauses'.



% we can have a root object where super have nowhere to go ...

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_object_'(Obj, _, _, _, OSuper, _, _, _, _),
	\+ '$lgt_rclause_'('$lgt_extends_object_'(Obj, _, _)),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz('$lgt_def_'((Head:-fail))),
	!.

% ... or we may extends some objects

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_object_'(Obj, _, _, _, OSuper, _, _, _, _),
	'$lgt_rclause_'('$lgt_extends_object_'(Obj, Parent, _)),
	Head =.. [OSuper, Pred, Sender, Obj, Self, Call, Ctn],
	'$lgt_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	Body =.. [PDef, Pred, Sender, Parent, Self, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
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
	'$lgt_gen_catchall_dcl_clause',
	'$lgt_gen_ic_hierarchy_dcl_clauses'.



'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	\+ '$lgt_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	!,
	'$lgt_object_'(_, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, _, _, _, _, _, _],
	assertz('$lgt_dcl_'((Head:-fail))).

'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	'$lgt_object_'(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
	'$lgt_instantiated_class_'(_, _, _, _, _, CIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [CIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Body = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_ic_hierarchy_dcl_clauses'.



% generates instance/class inherited declaration clauses

'$lgt_gen_ic_idcl_clauses' :-
	'$lgt_gen_ic_linking_idcl_clauses',
	'$lgt_gen_ic_protocol_idcl_clauses',
	'$lgt_gen_ic_category_idcl_clauses',
	'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_linking_idcl_clauses' :-
	'$lgt_object_'(Obj, _, Dcl, _, _, IDcl, _, DDcl, _),
	Head =.. [IDcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz('$lgt_dcl_'((Head:-Body))),
	Head2 =.. [IDcl, Pred, Scope, (dynamic), no, Obj, Obj],
	Body2 =.. [DDcl, Pred, Scope],
	assertz('$lgt_dcl_'((Head2:-Body2))).




'$lgt_gen_ic_protocol_idcl_clauses' :-
	'$lgt_object_'(Obj, _, _, _, _, OIDcl, _, _, _),
	Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
	'$lgt_implemented_protocol_'(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_ic_protocol_idcl_clauses'.



'$lgt_gen_ic_category_idcl_clauses' :-
	'$lgt_object_'(Obj, _, _, _, _, OIDcl, _, _, _),
	Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Ctn],
	'$lgt_imported_category_'(_, _, CDcl, _, EScope),
	(EScope = (public) ->
		Body =.. [CDcl, Pred, Scope, Compilation, Meta, Ctn]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Ctn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [CDcl, Pred, _, Compilation, Meta, Ctn])),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_ic_category_idcl_clauses'.



'$lgt_gen_ic_hierarchy_idcl_clauses' :-
	'$lgt_object_'(Obj, _, _, _, _, CIDcl, _, _, _),
	Head =.. [CIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn],
	'$lgt_specialized_class_'(_, _, _, _, _, SIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [SIDcl, Pred, Scope, Compilation, Meta, SCtn, TCtn]
		;
		(EScope = protected ->
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SCtn, TCtn],
			Body = (Call, (Scope2 == p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SCtn2, TCtn],
			Body = (Call, (Scope2 == p -> SCtn = SCtn2; SCtn = Obj)))),
	assertz('$lgt_dcl_'((Head:-Body))),
	fail.

'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_def_clauses' :-
	'$lgt_gen_local_def_clauses',
	'$lgt_gen_obj_catchall_def_clause',
	'$lgt_gen_ic_linking_def_clauses',
	'$lgt_gen_ic_imports_def_clauses',
	'$lgt_gen_ic_hierarchy_def_clauses'.



'$lgt_gen_ic_linking_def_clauses' :-
	'$lgt_object_'(Obj, _, _, Def, _, _, _, _, DDef),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body2))).



'$lgt_gen_ic_imports_def_clauses' :-
	'$lgt_object_'(Obj, _, _, ODef, _, _, _, _, _),
	'$lgt_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctg],
	'$lgt_imported_category_'(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),	
	fail.

'$lgt_gen_ic_imports_def_clauses'.



'$lgt_gen_ic_hierarchy_def_clauses' :-
	'$lgt_object_'(Obj, _, _, ODef, _, _, _, _, _),
	'$lgt_rclause_'('$lgt_instantiates_class_'(Obj, Class, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctn],
	'$lgt_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	Body =.. [CIDef, Pred, Sender, Class, Self, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
	fail.

'$lgt_gen_ic_hierarchy_def_clauses'.




'$lgt_gen_ic_idef_clauses' :-
	'$lgt_gen_ic_linking_idef_clauses',
	'$lgt_gen_ic_category_idef_clauses',
	'$lgt_gen_ic_hierarchy_idef_clauses'.



'$lgt_gen_ic_linking_idef_clauses' :-
	'$lgt_object_'(Obj, _, _, Def, _, _, IDef, _, DDef),
	Head =.. [IDef, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz('$lgt_def_'((Head:-Body2))).



'$lgt_gen_ic_category_idef_clauses' :-
	'$lgt_object_'(Obj, _, _, _, _, _, OIDef, _, _),
	'$lgt_rclause_'('$lgt_imports_category_'(Obj, Ctg, _)),
	Head =.. [OIDef, Pred, Sender, Obj, Self, Call, Ctg],
	'$lgt_imported_category_'(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz('$lgt_def_'((Head:-Body))),	
	fail.


'$lgt_gen_ic_category_idef_clauses'.



'$lgt_gen_ic_hierarchy_idef_clauses' :-
	'$lgt_object_'(Class, _, _, _, _, _, CIDef, _, _),
	'$lgt_rclause_'('$lgt_specializes_class_'(Class, Super, _)),
	Head =.. [CIDef, Pred, Sender, Class, Self, Call, Ctn],
	'$lgt_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	Body =.. [SIDef, Pred, Sender, Super, Self, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
	fail.

'$lgt_gen_ic_hierarchy_idef_clauses'.



% we can have a root object where super have nowhere to go ...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_object_'(_, _, _, _, OSuper, _, _, _, _),
	\+ '$lgt_rclause_'('$lgt_instantiates_class_'(_, _, _)),
	\+ '$lgt_rclause_'('$lgt_specializes_class_'(_, _, _)),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz('$lgt_def_'((Head:-fail))),
	!.

% ... or predicates can be redefined in instances...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_object_'(Obj, _, _, _, OSuper, _, _, _, _),
	'$lgt_rclause_'('$lgt_instantiates_class_'(Obj, Class, _)),
	Head =.. [OSuper, Pred, Sender, Obj, Obj, Call, Ctn],
	'$lgt_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	Body =.. [CIDef, Pred, Sender, Class, Obj, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
	fail.

% ... or/and in subclasses...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_object_'(Class, _, _, _, CSuper, _, _, _, _),
	'$lgt_rclause_'('$lgt_specializes_class_'(Class, Super, _)),
	Head =.. [CSuper, Pred, Sender, Class, Self, Call, Ctn],
	'$lgt_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	Body =.. [SIDef, Pred, Sender, Super, Self, Call, Ctn],
	assertz('$lgt_def_'((Head:-Body))),
	fail.

'$lgt_gen_ic_super_clauses'.



% '$lgt_fix_redef_built_ins'
%
% fix the calls of any redefined built-in predicate in all entity clauses 
% and initialization goals

'$lgt_fix_redef_built_ins' :-
	retract('$lgt_eclause_'(Clause)),
	'$lgt_fix_redef_built_ins'(Clause, Fixed),
	assertz('$lgt_feclause_'(Fixed)),
	fail.

'$lgt_fix_redef_built_ins' :-
	retract('$lgt_entity_init_'(Call)),
	'$lgt_fix_redef_built_ins'(Call, Fixed),
	assertz('$lgt_fentity_init_'(Fixed)),
	fail.

'$lgt_fix_redef_built_ins'.



% '$lgt_fix_redef_built_ins'(+clause, -clause)
%
% fix calls to redefined built-in predicates

'$lgt_fix_redef_built_ins'(Pred, Pred) :-
	var(Pred),
	!.

'$lgt_fix_redef_built_ins'((Head:-Body), (Head:-Fixed)) :-
	!,
	'$lgt_fix_redef_built_ins'(Body, Fixed).

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
	'$lgt_pl_metapredicate'(Meta),
	!,
	Pred =.. [_| Args],
	Meta =.. [_| MArgs],
	'$lgt_fix_redef_built_ins_in_margs'(Args, MArgs, TArgs),
	TPred =.. [Functor| TArgs].

'$lgt_fix_redef_built_ins'('$lgt_call_built_in'(Pred, Ctx), TPred) :-
	!,
	('$lgt_redefined_built_in_'(Pred, Ctx, TPred) ->
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



% find and report misspelt predicate calls
% in the body of objects/cartegories predicates

'$lgt_find_misspelt_calls' :-
	setof(Pred,
		('$lgt_calls_pred_'(Pred), \+ '$lgt_defs_pred_'(Pred), \+ '$lgt_dynamic_'(Pred)),
		Preds) ->
		'$lgt_report_misspelt_calls'(Preds)
		;
		true.



% '$lgt_report_misspelt_calls'(+list)

'$lgt_report_misspelt_calls'([]).

'$lgt_report_misspelt_calls'([Pred| Preds]) :-
	('$lgt_compiler_option'(misspelt, warning),
	 '$lgt_compiler_option'(report, on)) ->
		write('> WARNING!  these static predicates are called but never defined: '),
		writeq([Pred| Preds]), nl
		;
		true.



% '$lgt_write_directives'(+stream)
%
% writes the directives

'$lgt_write_directives'(Stream) :-
	'$lgt_directive_'(Dir),
	write_canonical(Stream, (:- Dir)),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_directives'(_).



'$lgt_write_clauses'(Stream) :-
	'$lgt_write_functors_clause'(Stream),
	'$lgt_write_dcl_clauses'(Stream),
	'$lgt_write_def_clauses'(Stream),
	'$lgt_write_ddef_clauses'(Stream),
	'$lgt_write_super_clauses'(Stream),
	'$lgt_write_entity_clauses'(Stream).



'$lgt_write_functors_clause'(Stream) :-
	'$lgt_entity_functors_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream).



'$lgt_write_def_clauses'(Stream) :-
	'$lgt_def_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_def_clauses'(_).



'$lgt_write_ddef_clauses'(Stream) :-
	'$lgt_ddef_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'), nl(Stream),
	fail.

'$lgt_write_ddef_clauses'(_).



'$lgt_write_dcl_clauses'(Stream) :-
	'$lgt_dcl_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_dcl_clauses'(_).



'$lgt_write_super_clauses'(Stream) :-
	'$lgt_super_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_super_clauses'(_).



'$lgt_write_entity_clauses'(Stream) :-
	'$lgt_feclause_'(Clause),
	write_canonical(Stream, Clause),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_entity_clauses'(_).



% '$lgt_write_init_call'(+stream)
%
% writes the initialization call for the compiled entity that will assert 
% the relation clauses and call any declared initialization goal when the
% entity is loaded

'$lgt_write_init_call'(Stream) :-
	'$lgt_entity_'(_, Entity, _, _),
	findall(Clause, '$lgt_rclause_'(Clause), Clauses),
	('$lgt_compiler_option'(debug, on) ->
		Goal1 = ('$lgt_assert_relation_clauses'(Clauses), assertz('$lgt_debugging_'(Entity)))
		;
		Goal1 = '$lgt_assert_relation_clauses'(Clauses)),
	('$lgt_fentity_init_'(Goal2) ->
		Goal = (Goal1, Goal2)
		;
		Goal = Goal1),
	('$lgt_compiler_option'(iso_initialization_dir, true) ->
		write_canonical(Stream, (:- initialization(Goal)))
		;
		write_canonical(Stream, (:- Goal))),
	write(Stream, '.'), nl(Stream).



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
	'$lgt_assert_entity_clauses',
	'$lgt_assert_relation_clauses',
	'$lgt_assert_init'.



'$lgt_assert_directives' :-
	'$lgt_directive_'((dynamic(Functor/Arity))),
	functor(Pred, Functor, Arity),
	asserta(Pred),
	retractall(Pred),
	fail.

'$lgt_assert_directives' :-
	'$lgt_directive_'((op(Pr, Spec, Ops))),
	op(Pr, Spec, Ops),
	fail.
	
'$lgt_assert_directives'.



'$lgt_assert_functors_clause' :-
	'$lgt_entity_functors_'(Clause),
	assertz(Clause).



'$lgt_assert_dcl_clauses' :-
	'$lgt_dcl_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_dcl_clauses'.



'$lgt_assert_def_clauses' :-
	'$lgt_def_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_def_clauses'.



'$lgt_assert_ddef_clauses' :-
	'$lgt_ddef_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_ddef_clauses'.



'$lgt_assert_super_clauses' :-
	'$lgt_super_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_super_clauses'.



'$lgt_assert_entity_clauses' :-
	'$lgt_feclause_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_entity_clauses'.



'$lgt_assert_relation_clauses' :-
	'$lgt_rclause_'(Clause),
	assertz(Clause),
	fail.

'$lgt_assert_relation_clauses'.



% '$lgt_assert_init'
%
% call any defined initialization goal for a dynamically created entity

'$lgt_assert_init' :-
	'$lgt_fentity_init_'(Goal) ->
		once(Goal)
		;
		true.



% '$lgt_assert_relation_clauses'(+list)
%
% called when loading a compiled Logtalk entity to update Logtalk 
% internal tables
%
% we may be reloading the entity so we must first retract any old
% relation clauses before asserting the new ones

'$lgt_assert_relation_clauses'([Clause| Clauses]) :-
	arg(1, Clause, Entity),
	'$lgt_retract_old_relation_clauses'(Entity),
	'$lgt_assert_new_relation_clauses'([Clause| Clauses]).


'$lgt_retract_old_relation_clauses'(Entity) :-
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


'$lgt_assert_new_relation_clauses'([]).

'$lgt_assert_new_relation_clauses'([Clause| Clauses]) :-
	assertz(Clause),
	'$lgt_assert_new_relation_clauses'(Clauses).



% '$lgt_construct_object_functors'(+compound, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of an object

'$lgt_construct_object_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef) :-
	'$lgt_compiler_option'(code_prefix, Code),
	functor(Obj, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Code, Aux, Aux2),
	atom_concat(Aux2, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def),
	atom_concat(Prefix, '_super', Super),
	atom_concat(Prefix, '_idcl', IDcl),
	atom_concat(Prefix, '_idef', IDef),
	atom_concat(Prefix, '_ddcl', DDcl),
	atom_concat(Prefix, '_ddef', DDef).



% '$lgt_construct_protocol_functors'(+compound, -atom, -atom)
%
% constructs all the functors used in the compiled code of a protocol

'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl) :-
	'$lgt_compiler_option'(code_prefix, Code),
	functor(Ptc, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Code, Aux, Aux2),
	atom_concat(Aux2, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl).



% '$lgt_construct_category_functors'(+compound, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of a category

'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def) :-
	'$lgt_compiler_option'(code_prefix, Code),
	functor(Ctg, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Code, Aux, Aux2),
	atom_concat(Aux2, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def).



% '$lgt_construct_predicate_functor'(+atom, +atom, +integer, -atom)
%
% constructs the functor used for a compiled predicate

'$lgt_construct_predicate_functor'(EPrefix, Functor, Arity, PPrefix) :-
	atom_concat(EPrefix, Functor, Aux),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Aux, Atom, PPrefix).



% '$lgt_built_in'(+callable)
%
% checks if the argument is either a Prolog or Logtalk built-in predicate

'$lgt_built_in'(Pred) :-
	'$lgt_pl_built_in'(Pred).

'$lgt_built_in'(Pred) :-
	'$lgt_lgt_built_in'(Pred).



% '$lgt_pl_built_in'(+callable)
%
% either host Prolog native built-ins or missing ISO built-ins
% that we have defined in the correspondent config file

'$lgt_pl_built_in'(Pred) :-
	'$lgt_predicate_property'(Pred, built_in).

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

'$lgt_built_in_method'(phrase(_, _), p(p(p))).
'$lgt_built_in_method'(phrase(_, _, _), p(p(p))).



% Logtalk directives
%
% '$lgt_lgt_directive'(+atom/+integer)

'$lgt_lgt_directive'(Directive) :-
	'$lgt_lgt_opening_directive'(Directive).

'$lgt_lgt_directive'(Directive) :-
	'$lgt_lgt_closing_directive'(Directive).

'$lgt_lgt_directive'(Directive) :-
	'$lgt_lgt_entity_directive'(Directive).

'$lgt_lgt_directive'(Directive) :-
	'$lgt_lgt_predicate_directive'(Directive).


'$lgt_lgt_opening_directive'(object/1).
'$lgt_lgt_opening_directive'(object/2).
'$lgt_lgt_opening_directive'(object/3).
'$lgt_lgt_opening_directive'(object/4).
'$lgt_lgt_opening_directive'(object/5).

'$lgt_lgt_opening_directive'(category/1).
'$lgt_lgt_opening_directive'(category/2).

'$lgt_lgt_opening_directive'(protocol/1).
'$lgt_lgt_opening_directive'(protocol/2).



'$lgt_lgt_closing_directive'(end_object/0).

'$lgt_lgt_closing_directive'(end_category/0).

'$lgt_lgt_closing_directive'(end_protocol/0).



'$lgt_lgt_entity_directive'(calls/N) :-
	N >= 1.
'$lgt_lgt_entity_directive'(uses/N) :-
	N >= 1.

'$lgt_lgt_entity_directive'((initialization)/1).

'$lgt_lgt_entity_directive'((dynamic)/N) :-
	N =:= 0.

'$lgt_lgt_entity_directive'(op/3).

'$lgt_lgt_entity_directive'(info/1).



'$lgt_lgt_predicate_directive'((dynamic)/N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'(metapredicate/N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((discontiguous)/N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((public)/N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(protected/N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(private/N) :-
	N >= 1.

'$lgt_lgt_predicate_directive'((mode)/2).

'$lgt_lgt_predicate_directive'(info/2).



% built-in metapredicates

'$lgt_metapredicate'(Meta) :-
	'$lgt_lgt_metapredicate'(Meta).

'$lgt_metapredicate'(Meta) :-
	'$lgt_pl_metapredicate'(Meta).		% defined in the config files



% built-in Logtalk (and Prolog) metapredicates

'$lgt_lgt_metapredicate'(catch(::, *, ::)).

'$lgt_lgt_metapredicate'(bagof(*, ::, *)).
'$lgt_lgt_metapredicate'(setof(*, ::, *)).
'$lgt_lgt_metapredicate'(findall(*, ::, *)).

'$lgt_lgt_metapredicate'(forall(::, ::)).

'$lgt_lgt_metapredicate'(call(::)).
'$lgt_lgt_metapredicate'(once(::)).

'$lgt_lgt_metapredicate'(\+ (::)).



% utility predicates used during compilation of Logtalk 
% entities to store and access context information

'$lgt_context'(context(_, _, _, _, _)).

'$lgt_sender'(context(Sender, _, _, _, _), Sender).

'$lgt_this'(context(_, This, _, _, _), This).

'$lgt_self'(context(_, _, Self, _, _), Self).

'$lgt_prefix'(context(_, _, _, Prefix, _), Prefix).

'$lgt_metavars'(context(_, _, _, _, Metavars), Metavars).



% '$lgt_convert_to_list'(+pi_or_pi_list, -pi_list)

'$lgt_convert_to_list'([[A|B]], [A|B]) :-	% predicate indicator list
	!.

'$lgt_convert_to_list'([A|B], [A|B]) :-		% predicate indicator sequence
	!.

'$lgt_convert_to_list'(A, [A]).				% single predicate indicator



% '$lgt_valid_pred_ind(@term)

'$lgt_valid_pred_ind'(Term) :-
	nonvar(Term),
	Term = Functor/Arity,
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_pred_ind_list(@term)

'$lgt_valid_pred_ind_list'(Term) :-
	nonvar(Term),
	'$lgt_valid_pred_ind_list2'(Term).


'$lgt_valid_pred_ind_list2'([]).

'$lgt_valid_pred_ind_list2'([Term| Terms]) :-
	'$lgt_valid_pred_ind_list'(Term),
	'$lgt_valid_pred_ind_list2'(Terms).



% '$lgt_valid_object_id'(@term)

'$lgt_valid_object_id'(Term) :-
	once((atom(Term); compound(Term))).



% '$lgt_valid_category_id'(@term)

'$lgt_valid_category_id'(Term) :-
	atom(Term).



% '$lgt_valid_protocol_id'(@term)

'$lgt_valid_protocol_id'(Term) :-
	atom(Term).



% '$lgt_valid_scope'(@term)
	
'$lgt_valid_scope'(Term) :-
	nonvar(Term),	
	(Term = (Scope::_) ->
		nonvar(Scope),
		once('$lgt_member'(Scope, [(public), protected, private]))
		;
		true).



% '$lgt_scope_id'(+term, -atom, -term)

'$lgt_scope_id'(Scope::Entity, Scope, Entity) :-
	!.

'$lgt_scope_id'(Entity, (public), Entity).



% '$lgt_callable'(@term)

'$lgt_callable'(Term) :-
	nonvar(Term),
	functor(Term, Functor, _),
	atom(Functor).



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



% '$lgt_valid_info_list'(@list)
%
% true if the argument is a list of key-value pairs

'$lgt_valid_info_list'([]).

'$lgt_valid_info_list'([Head| Tail]) :-
	nonvar(Head),
	Head = (Key is Value),
	nonvar(Key),
	nonvar(Value),
	'$lgt_valid_info_list'(Tail).



% '$lgt_valid_compiler_options'(@list)
%
% true if all compiler options are valid

'$lgt_valid_compiler_options'([]).

'$lgt_valid_compiler_options'([Option| Options]) :-
	nonvar(Option),
	'$lgt_valid_compiler_option'(Option),
	'$lgt_valid_compiler_options'(Options).



% '$lgt_valid_compiler_option'(@nonvar)

'$lgt_valid_compiler_option'(iso_initialization_dir(Option)) :-
	once((Option == true; Option == false)).

'$lgt_valid_compiler_option'(xml(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_option'(xsl(File)) :-
	atom(File).

'$lgt_valid_compiler_option'(unknown(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(singletons(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(misspelt(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(lgtredef(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(plredef(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(portability(Option)) :-
	once((Option == silent; Option == warning)).

'$lgt_valid_compiler_option'(report(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_option'(smart_compilation(Option)) :-
	once((Option == on; Option == off)).

'$lgt_valid_compiler_option'(underscore_vars(Option)) :-
	once((Option == dont_care; Option == singletons)).

'$lgt_valid_compiler_option'(code_prefix(Prefix)) :-
	atom(Prefix).

'$lgt_valid_compiler_option'(doctype(Option)) :-
	once((Option == standalone; Option == (local); Option == web)).

'$lgt_valid_compiler_option'(xmlspec(Option)) :-
	once((Option == dtd; Option == xsd)).

'$lgt_valid_compiler_option'(debug(Option)) :-
	once((Option == on; Option == off)).



% '$lgt_valid_flag'(@nonvar)
%
% true if the argument is a valid Logtalk flag

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



% '$lgt_valid_flag'(@term, @term)
%
% true if the argument is a valid Logtalk flag-value pair

'$lgt_valid_flag'(Flag, Value) :-
	atom(Flag),
	Option =.. [Flag, Value],
	'$lgt_valid_compiler_option'(Option).



% '$lgt_read_only_flag'(@nonvar)
%
% true if the argument is a read only Logtalk flag

'$lgt_read_only_flag'(startup_message).
'$lgt_read_only_flag'(version).
'$lgt_read_only_flag'(supports_break_predicate).



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
	'$lgt_dcg_head'(RHead, CHead, Body, Body2, S0, S, S1),
	'$lgt_dcg_body'(RBody, Body, S0, S),
	'$lgt_dcg_fold_unifications'(Body2, CBody, S1).



% '$lgt_dcg_head'(@dcghead, -head, @goal, -goal, @var, @var, -var)
%
% translates DCG rule head to a Prolog clause head
% (the last argument returns the variable representing the ouput list)

'$lgt_dcg_head'(RHead, _, _, _, _, _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_head'((_, Terminals), _, _, _, _, _, _) :-
	\+ '$lgt_proper_list'(Terminals),
	throw(type_error(list, Terminals)).

'$lgt_dcg_head'((Nonterminal, Terminals), CHead, Body, (Body,Goal), S0, S, S1) :-
	!,
	'$lgt_dcg_terminals'(Terminals, Goal, S1, S),
	'$lgt_dcg_goal'(Nonterminal, CHead, S0, S1).

'$lgt_dcg_head'(Nonterminal, CHead, Body, Body, S0, S, S) :-
	'$lgt_dcg_goal'(Nonterminal, CHead, S0, S).



% '$lgt_dcg_body'(@dcgbody, -body, @var, @var)
%
% translates DCG rule body to a Prolog clause body

'$lgt_dcg_body'(Var, phrase(Var, S0, S), S0, S) :-
	var(Var),
	!.

'$lgt_dcg_body'(::Goal, ::phrase(Goal, S0, S), S0, S) :-
	!.

'$lgt_dcg_body'(Object::Goal, Object::phrase(Goal, S0, S), S0, S) :-
	!.

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

'$lgt_dcg_body'(\+ RGoal, CGoal, S0, S) :-
	!,
	'$lgt_dcg_body'((RGoal -> {fail};{true}), CGoal, S0, S).

'$lgt_dcg_body'([], (S0=S), S0, S) :-
	!.

'$lgt_dcg_body'([Terminal| Terminals], CGoal, S0, S) :-
	!,
	'$lgt_dcg_terminals'([Terminal| Terminals], CGoal, S0, S).

'$lgt_dcg_body'(Non_terminal, CGoal, S0, S) :-
	'$lgt_dcg_goal'(Non_terminal, CGoal, S0, S).



% '$lgt_dcg_goal'(@goal, -goal, @var, @var)
%
% translates DCG goal to Prolog goal

'$lgt_dcg_goal'(RGoal, _, _, _) :-
	\+ '$lgt_callable'(RGoal),
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



% '$lgt_dcg_fold_unifications'(+goal, -goal, @var)
%
% folds redundant calls to =/2 by calling the unification
% goals execept for output unifications

'$lgt_dcg_fold_unifications'((Goal1 -> Goal2), (SGoal1 -> SGoal2), S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S).

'$lgt_dcg_fold_unifications'((Goal1;Goal2), (SGoal1;SGoal2), S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S).

'$lgt_dcg_fold_unifications'((Goal1,Goal2), SGoal, S) :-
	!,
	'$lgt_dcg_fold_unifications'(Goal1, SGoal1, S),
	'$lgt_dcg_fold_unifications'(Goal2, SGoal2, S),
	'$lgt_dcg_simplify_and'((SGoal1,SGoal2), SGoal).

'$lgt_dcg_fold_unifications'(S1=S2, S1=S2, S) :-
	(S1 == S; S2 == S),		% avoid output unifications
	!.

'$lgt_dcg_fold_unifications'(S1=S2, true, _) :-
	var(S2),				% avoid unification with list of terminals
	!,
	S1 = S2.

'$lgt_dcg_fold_unifications'(Goal, Goal, _).



% '$lgt_dcg_simplify_and'(+goal, -goal)
%
% removes redundant calls to true/0 and flats a conjunction of goals

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
	'$lgt_write_xml_footer'(Stream).



'$lgt_write_xml_header'(Stream) :-
	'$lgt_compiler_option'(xmlspec, XMLSpec),
	'$lgt_compiler_option'(doctype, Doctype),
	'$lgt_write_xml_header'(Doctype, XMLSpec, Stream).



'$lgt_write_xml_header'(local, XMLSpec, Stream) :-
	'$lgt_write_xml_open_tag'(Stream, '?xml version="1.0" standalone="no"?', []),
	write(Stream, '<!DOCTYPE logtalk SYSTEM "logtalk.'),
	write(Stream, XMLSpec), write(Stream, '">'), nl(Stream),
	'$lgt_compiler_option'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	'$lgt_write_xml_open_tag'(Stream, logtalk, []).

'$lgt_write_xml_header'(web, XMLSpec, Stream) :-
	'$lgt_write_xml_open_tag'(Stream, '?xml version="1.0" standalone="no"?', []),
	write(Stream, '<!DOCTYPE logtalk SYSTEM "http://www.logtalk.org/xml/1.1/logtalk.'),
	write(Stream, XMLSpec), write(Stream, '">'), nl(Stream),
	'$lgt_compiler_option'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	'$lgt_write_xml_open_tag'(Stream, logtalk, []).

'$lgt_write_xml_header'(standalone, _, Stream) :-
	'$lgt_write_xml_open_tag'(Stream, '?xml version="1.0" standalone="yes"?', []),
	'$lgt_compiler_option'(xsl, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	'$lgt_write_xml_open_tag'(Stream, logtalk, []).



'$lgt_write_xml_footer'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, logtalk).



'$lgt_write_xml_entity'(Stream) :-
	'$lgt_entity_'(Type, Entity, _, _),
	('$lgt_entity_comp_mode_'((dynamic)) -> Compilation = (dynamic); Compilation = static),
	'$lgt_write_xml_open_tag'(Stream, entity, []),
	'$lgt_entity_to_xml_name'(Entity, Name),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Name),
	'$lgt_write_xml_element'(Stream, (type), [], Type),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation),
	('$lgt_info_'(List) ->
		('$lgt_member'(comment is Comment, List) ->
			'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
			;
			true), 
		('$lgt_member'(author is Author, List) ->
			'$lgt_write_xml_cdata_element'(Stream, author, [], Author)
			;
			true), 
		('$lgt_member'(version is Version, List) ->
			'$lgt_write_xml_element'(Stream, version, [], Version)
			;
			true), 
		('$lgt_member'(date is Date, List) ->
			'$lgt_write_xml_element'(Stream, date, [], Date)
			;
			true),
		forall(
			('$lgt_member'(Key is Value, List),
			 \+ '$lgt_member'(Key, [comment, author, version, date, parnames])),
			('$lgt_write_xml_open_tag'(Stream, info, []),
			 '$lgt_write_xml_element'(Stream, key, [], Key),
			 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
			 '$lgt_write_xml_close_tag'(Stream, info)))
		;
		true),
	'$lgt_write_xml_close_tag'(Stream, entity).



% '$lgt_entity_to_xml_name'(+entity, -nonvar)
%
% instantiates the parameters in a parametric object to
% user defined names or to the atom '_'

'$lgt_entity_to_xml_name'(Entity, Name) :-
	'$lgt_info_'(List),
	'$lgt_member'(parnames is Names, List),
	!,
	Entity =.. [Functor| Names],
	Name =.. [Functor| Names].

'$lgt_entity_to_xml_name'(Entity, Name) :-
	Entity =.. [Functor| Args],
	'$lgt_vars_to_underscore'(Args, Names),
	Name =.. [Functor| Names].



% '$lgt_relation_to_xml_name'(+entity, +entity, -atom)
%
% instantiates the parameters in a related entity taking
% in account the parameter sharing with the original entity

'$lgt_relation_to_xml_name'(Entity, Relation, Name) :-
	'$lgt_entity_to_xml_name'(Entity, _),
	Relation =.. [Functor| Args],
	'$lgt_vars_to_underscore'(Args, Names),
	Name =.. [Functor| Names].



% '$lgt_vars_to_underscore'(+list, -list)
%
% instantiates the variables in the input list to the atom '_'

'$lgt_vars_to_underscore'([], []).

'$lgt_vars_to_underscore'([Arg| Args], [Name| Names]) :-
	(var(Arg) -> Name = '_'; Name = Arg),
	'$lgt_vars_to_underscore'(Args, Names).



% '$lgt_relation_to_xml_filename'(+entity, -atom)
%
% needed to build filenames in links to parametric objects

'$lgt_relation_to_xml_filename'(Relation, File) :-
	atom(Relation) ->
		File = Relation
		;
		functor(Relation, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, Atom, File).



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
	'$lgt_public_'(Functor/Arity),
	'$lgt_write_xml_predicate'(Stream, Functor/Arity, (public)),
	fail.

'$lgt_write_xml_public_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, (public)).



% '$lgt_write_xml_protected_predicates'(+stream)
%
% writes the documentation protected predicates

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, protected, []),
	'$lgt_protected_'(Functor/Arity),
	'$lgt_write_xml_predicate'(Stream, Functor/Arity, protected),
	fail.

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, protected).



% '$lgt_write_xml_private_predicates'(+stream)
%
% writes the documentation of private predicates

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, private, []),
	'$lgt_private_'(Functor/Arity),
	'$lgt_write_xml_predicate'(Stream, Functor/Arity, private),
	fail.

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, private).



% '$lgt_write_xml_predicate'(+stream, +atom/+integer, +term)
%
% writes the documentation of a predicate

'$lgt_write_xml_predicate'(Stream, Functor/Arity, Scope) :-
	(('$lgt_entity_comp_mode_'((dynamic)); '$lgt_dynamic_'(Functor/Arity)) ->
		Compilation = (dynamic)
		;
		Compilation = static),
	'$lgt_write_xml_open_tag'(Stream, predicate, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Functor/Arity),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation),
	functor(Meta, Functor, Arity),
	('$lgt_metapredicate_'(Meta) ->
		'$lgt_write_xml_cdata_element'(Stream, meta, [], Meta)
		;
		true),
	functor(Template, Functor, Arity),
	forall(
		'$lgt_mode_'(Template, Solutions),
		('$lgt_write_xml_open_tag'(Stream, (mode), []),
		 '$lgt_write_xml_cdata_element'(Stream, template, [], Template),
		 '$lgt_write_xml_element'(Stream, solutions, [], Solutions),
		 '$lgt_write_xml_close_tag'(Stream, (mode)))),
	(('$lgt_info_'(Functor/Arity, List), '$lgt_member'(comment is Comment, List)) ->
		'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
		;
		true),
	(('$lgt_info_'(Functor/Arity, List), '$lgt_member'(argnames is Names, List)) ->
		Template =.. [Functor| Names],
		'$lgt_write_xml_cdata_element'(Stream, template, [], Template)
		;
		true),
	(('$lgt_info_'(Functor/Arity, List), '$lgt_member'(exceptions is Terms, List), Terms \= []) ->
		'$lgt_write_xml_open_tag'(Stream, exceptions, []),
		forall(
			'$lgt_member'(Cond-Term, Terms),
		 	('$lgt_write_xml_open_tag'(Stream, exception, []),
		 	 '$lgt_write_xml_cdata_element'(Stream, condition, [], Cond),
		 	 '$lgt_write_xml_cdata_element'(Stream, term, [], Term),
		 	 '$lgt_write_xml_close_tag'(Stream, exception))),
		 '$lgt_write_xml_close_tag'(Stream, exceptions)
		;
		true),
	forall(
		('$lgt_info_'(Functor/Arity, List),
		 '$lgt_member'(Key is Value, List),
		 \+ '$lgt_member'(Key, [comment, argnames, exceptions])),
		('$lgt_write_xml_open_tag'(Stream, info, []),
		 '$lgt_write_xml_element'(Stream, key, [], Key),
		 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
		 '$lgt_write_xml_close_tag'(Stream, info))),
	'$lgt_write_xml_close_tag'(Stream, predicate).



'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, relations, []),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_implements_protocol_'(Entity, Ptc, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, implements, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_imports_category_'(Entity, Ctg, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ctg, imports, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_extends_object_'(Entity, Parent, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Parent, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_instantiates_class_'(Entity, Class, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Class, instantiates, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_specializes_class_'(Entity, Superclass, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Superclass, specializes, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_rclause_'('$lgt_extends_protocol_'(Entity, Ptc, Scope)),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_entity_'(_, Entity, _, _),
	'$lgt_uses_'(Obj),
	'$lgt_write_xml_relation'(Stream, Entity, Obj, uses),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_entity_'(_, Entity, _, _),
	'$lgt_calls_'(Ptc),
	'$lgt_write_xml_relation'(Stream, Entity, Ptc, calls),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, relations).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag, Scope) :-
	'$lgt_relation_to_xml_name'(Entity, Relation, Name),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Name),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag) :-
	'$lgt_relation_to_xml_name'(Entity, Relation, Name),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Name),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).



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
%  table of ISO defined predicates
%
%  used for portability checking
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_iso_def_pred'(true).
'$lgt_iso_def_pred'(fail).
'$lgt_iso_def_pred'(call(_)).
'$lgt_iso_def_pred'(!).
'$lgt_iso_def_pred'((_; _)).
'$lgt_iso_def_pred'((_, _)).
'$lgt_iso_def_pred'((_ -> _)).
'$lgt_iso_def_pred'((_ -> _; _)).
'$lgt_iso_def_pred'(catch(_, _, _)).
'$lgt_iso_def_pred'(throw(_)).

'$lgt_iso_def_pred'((_ = _)).
'$lgt_iso_def_pred'((_ \= _)).
'$lgt_iso_def_pred'(unify_with_occurs_check(_, _)).

'$lgt_iso_def_pred'(var(_)).
'$lgt_iso_def_pred'(nonvar(_)).
'$lgt_iso_def_pred'(atom(_)).
'$lgt_iso_def_pred'(atomic(_)).
'$lgt_iso_def_pred'(number(_)).
'$lgt_iso_def_pred'(integer(_)).
'$lgt_iso_def_pred'(float(_)).
'$lgt_iso_def_pred'(compound(_)).

'$lgt_iso_def_pred'((_ @=< _)).
'$lgt_iso_def_pred'((_ @< _)).
'$lgt_iso_def_pred'((_ @>= _)).
'$lgt_iso_def_pred'((_ @> _)).
'$lgt_iso_def_pred'((_ == _)).
'$lgt_iso_def_pred'((_ \== _)).

'$lgt_iso_def_pred'(functor(_, _, _)).
'$lgt_iso_def_pred'(arg(_, _, _)).
'$lgt_iso_def_pred'(_ =.. _).
'$lgt_iso_def_pred'(copy_term(_, _)).

'$lgt_iso_def_pred'(_ is _).

'$lgt_iso_def_pred'((_ =< _)).
'$lgt_iso_def_pred'((_ < _)).
'$lgt_iso_def_pred'((_ >= _)).
'$lgt_iso_def_pred'((_ > _)).
'$lgt_iso_def_pred'((_ =:= _)).
'$lgt_iso_def_pred'((_ =\= _)).

'$lgt_iso_def_pred'(clause(_, _)).
'$lgt_iso_def_pred'(current_predicate(_)).

'$lgt_iso_def_pred'(asserta(_)).
'$lgt_iso_def_pred'(assertz(_)).
'$lgt_iso_def_pred'(retract(_)).
'$lgt_iso_def_pred'(abolish(_)).

'$lgt_iso_def_pred'(findall(_, _, _)).
'$lgt_iso_def_pred'(bagof(_, _, _)).
'$lgt_iso_def_pred'(setof(_, _, _)).

'$lgt_iso_def_pred'(current_input(_)).
'$lgt_iso_def_pred'(current_output(_)).
'$lgt_iso_def_pred'(set_input(_)).
'$lgt_iso_def_pred'(set_output(_)).
'$lgt_iso_def_pred'(open(_, _, _, _)).
'$lgt_iso_def_pred'(open(_, _, _)).
'$lgt_iso_def_pred'(close(_, _)).
'$lgt_iso_def_pred'(close(_)).
'$lgt_iso_def_pred'(flush_output(_)).
'$lgt_iso_def_pred'(flush_output).
'$lgt_iso_def_pred'(stream_property(_, _)).
'$lgt_iso_def_pred'(at_end_of_stream).
'$lgt_iso_def_pred'(at_end_of_stream(_)).
'$lgt_iso_def_pred'(set_stream_position(_, _)).

'$lgt_iso_def_pred'(get_char(_, _)).
'$lgt_iso_def_pred'(get_char(_)).
'$lgt_iso_def_pred'(get_code(_, _)).
'$lgt_iso_def_pred'(get_code(_)).
'$lgt_iso_def_pred'(peek_char(_, _)).
'$lgt_iso_def_pred'(peek_char(_)).
'$lgt_iso_def_pred'(peek_code(_, _)).
'$lgt_iso_def_pred'(peek_code(_)).
'$lgt_iso_def_pred'(put_char(_, _)).
'$lgt_iso_def_pred'(put_char(_)).
'$lgt_iso_def_pred'(put_code(_, _)).
'$lgt_iso_def_pred'(put_code(_)).
'$lgt_iso_def_pred'(nl).
'$lgt_iso_def_pred'(nl(_)).

'$lgt_iso_def_pred'(get_byte(_, _)).
'$lgt_iso_def_pred'(get_byte(_)).
'$lgt_iso_def_pred'(peek_byte(_, _)).
'$lgt_iso_def_pred'(peek_byte(_)).
'$lgt_iso_def_pred'(put_byte(_, _)).
'$lgt_iso_def_pred'(put_byte(_)).

'$lgt_iso_def_pred'(read_term(_, _, _)).
'$lgt_iso_def_pred'(read_term(_, _)).
'$lgt_iso_def_pred'(read(_)).
'$lgt_iso_def_pred'(read(_, _)).
'$lgt_iso_def_pred'(write_term(_, _, _)).
'$lgt_iso_def_pred'(write_term(_, _)).
'$lgt_iso_def_pred'(write(_)).
'$lgt_iso_def_pred'(write(_, _)).
'$lgt_iso_def_pred'(writeq(_)).
'$lgt_iso_def_pred'(writeq(_, _)).
'$lgt_iso_def_pred'(write_canonical(_)).
'$lgt_iso_def_pred'(write_canonical(_, _)).
'$lgt_iso_def_pred'(op(_, _, _)).
'$lgt_iso_def_pred'(current_op(_, _, _)).
'$lgt_iso_def_pred'(char_conversion(_, _)).
'$lgt_iso_def_pred'(current_char_conversion(_, _)).

'$lgt_iso_def_pred'(\+ _).
'$lgt_iso_def_pred'(once(_)).
'$lgt_iso_def_pred'(repeat).

'$lgt_iso_def_pred'(atom_length(_, _)).
'$lgt_iso_def_pred'(atom_concat(_, _, _)).
'$lgt_iso_def_pred'(sub_atom(_, _, _, _, _)).
'$lgt_iso_def_pred'(atom_chars(_, _)).
'$lgt_iso_def_pred'(atom_codes(_, _)).
'$lgt_iso_def_pred'(char_code(_, _)).
'$lgt_iso_def_pred'(number_chars(_, _)).
'$lgt_iso_def_pred'(number_codes(_, _)).

'$lgt_iso_def_pred'(set_prolog_flag(_, _)).
'$lgt_iso_def_pred'(current_prolog_flag(_, _)).
'$lgt_iso_def_pred'(halt).
'$lgt_iso_def_pred'(halt(_)).



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
	write('Copyright (c) 1998-2004 Paulo Moura'), nl, nl.



'$lgt_default_flags' :-
	write('Default compilation flags:'), nl,
	'$lgt_default_flag'(iso_initialization_dir, ISO),
	write('  ISO initialization/1 directive (iso_initialization_dir):  '), write(ISO), nl,
	'$lgt_default_flag'(xml, XML),
	write('  XML documenting files (xml):                              '), write(XML), nl,
	'$lgt_default_flag'(xmlspec, XMLSpec),
	write('  XML specification file extension (xmlspec):               '), write(XMLSpec), nl,
	'$lgt_default_flag'(doctype, Doctype),
	write('  XML specification file location (doctype):                '), write(Doctype), nl,
	'$lgt_default_flag'(xsl, XSL),
	write('  XSL stylesheet (xsl):                                     '), write(XSL), nl,
	'$lgt_default_flag'(unknown, Unknown),
	write('  Unknown entities (unknown):                               '), write(Unknown), nl,
	'$lgt_default_flag'(misspelt, Misspelt),
	write('  Misspelt predicate calls (misspelt):                      '), write(Misspelt), nl,
	'$lgt_default_flag'(singletons, Singletons),
	write('  Singletons variables (singletons):                        '), write(Singletons), nl,
	'$lgt_default_flag'(lgtredef, Lgtredef),
	write('  Logtalk built-ins redefinition (lgtredef):                '), write(Lgtredef), nl,
	'$lgt_default_flag'(plredef, Plredef),
	write('  Prolog built-ins redefinition (plredef):                  '), write(Plredef), nl,
	'$lgt_default_flag'(portability, Portability),
	write('  Non portable calls (portability):                         '), write(Portability), nl,
	'$lgt_default_flag'(report, Report),
	write('  Compilation report (report):                              '), write(Report), nl,
	'$lgt_default_flag'(underscore_vars, Underscore),
	write('  Underscore variables (underscore_vars):                   '), write(Underscore), nl,
	'$lgt_default_flag'(code_prefix, Code),
	write('  Compiled code functors prefix (code_prefix):              '), writeq(Code), nl,
	'$lgt_default_flag'(debug, Debug),
	write('  Compile entities in debug mode (debug):                   '), writeq(Debug), nl,
	'$lgt_default_flag'(supports_break_predicate, Break),
	write('  Support for break/0 predicate (supports_break_predicate): '), writeq(Break), nl,
	'$lgt_default_flag'(smart_compilation, Smart),
	write('  Smart compilation (smart_compilation):                    '), write(Smart), nl, nl.



:- initialization('$lgt_startup_message').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
