
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.12.0
%
%  Copyright (c) 1998-2002 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operators
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% message sending operators

:- op(600, xfy, ::).						% send to object
:- op(600,  fy, ::).						% send to self

:- op(600,  fx, ^^).						% super call


% mode operators

:- op(200, fy, +).							% input argument (instantiated)
:- op(200, fy, ?).							% input/output argument
:- op(200, fy, @).							% input argument (not modified by the call)
:- op(200, fy, -).							% output argument (not instantiated)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  runtime directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% tables of defined events and monitors

:- dynamic(lgt_before_/5).					% lgt_before_(Obj, Msg, Sender, Monitor, Call)
:- dynamic(lgt_after_/5).					% lgt_after_(Obj, Msg, Sender, Monitor, Call)



% tables of loaded entities and respective relationships

:- dynamic(lgt_current_protocol_/2).		% lgt_current_protocol_(Ptc, Prefix)
:- dynamic(lgt_current_category_/2).		% lgt_current_category_(Ctg, Prefix)
:- dynamic(lgt_current_object_/5).			% lgt_current_object_(Obj, Prefix, Dcl, Def, Super)

:- dynamic(lgt_implements_protocol_/3).		% lgt_implements_protocol_(ObjOrCtg, Ptc, Scope)
:- dynamic(lgt_imports_category_/3).		% lgt_imports_category_(Obj, Ctg, Scope)
:- dynamic(lgt_instantiates_class_/3).		% lgt_instantiates_class_(Instance, Class, Scope)
:- dynamic(lgt_specializes_class_/3).		% lgt_specializes_class_(Class, Superclass, Scope)
:- dynamic(lgt_extends_protocol_/3).		% lgt_extends_protocol_(Ptc1, Ptc2, Scope)
:- dynamic(lgt_extends_object_/3).			% lgt_extends_object_(Prototype, Parent, Scope)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor directives
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic(lgt_dcl_/1).						% lgt_dcl_(Clause)
:- dynamic(lgt_ddcl_/1).					% lgt_ddcl_(Clause)
:- dynamic(lgt_def_/1).						% lgt_def_(Clause)
:- dynamic(lgt_ddef_/1).					% lgt_ddef_(Clause)
:- dynamic(lgt_super_/1).					% lgt_super_(Clause)

:- dynamic(lgt_dynamic_/1).					% lgt_dynamic_(Functor/Arity)
:- dynamic(lgt_discontiguous_/1).			% lgt_discontiguous_(Functor/Arity)
:- dynamic(lgt_mode_/2).					% lgt_mode_(Mode, Determinism)
:- dynamic(lgt_public_/1).					% lgt_public_(Functor/Arity)
:- dynamic(lgt_protected_/1).				% lgt_protected_(Functor/Arity)
:- dynamic(lgt_private_/1).					% lgt_private_(Functor/Arity)
:- dynamic(lgt_metapredicate_/1).			% lgt_metapredicate_(Pred)

:- dynamic(lgt_object_/9).					% lgt_object_(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef)
:- dynamic(lgt_category_/4).				% lgt_category_(Ctg, Prefix, Dcl, Def)
:- dynamic(lgt_protocol_/3).				% lgt_protocol_(Ptc, Prefix, Dcl)

:- dynamic(lgt_uses_/1).					% lgt_uses_(Entity)
:- dynamic(lgt_calls_/1).					% lgt_calls_(Entity)
:- dynamic(lgt_info_/1).					% lgt_info_(List)
:- dynamic(lgt_info_/2).					% lgt_info_(Functor/Arity, List)

:- dynamic(lgt_implemented_protocol_/4).	% lgt_implemented_protocol_(Ptc, Prefix, Dcl, Scope)
:- dynamic(lgt_imported_category_/5).		% lgt_imported_category_(Ctg, Prefix, Dcl, Def, Scope)
:- dynamic(lgt_extended_object_/10).		% lgt_extended_object_(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic(lgt_instantiated_class_/10).		% lgt_instantiated_class_(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic(lgt_specialized_class_/10).		% lgt_specialized_class_(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic(lgt_extended_protocol_/4).		% lgt_extended_protocol_(Ptc2, Prefix, Dcl, Scope)

:- dynamic(lgt_entity_/4).					% lgt_entity_(Type, Entity, Prefix, Dcl)
:- dynamic(lgt_entity_functors_/1).			% lgt_entity_functors_(Clause)
:- dynamic(lgt_entity_init_/1).				% lgt_entity_init_(Goal)
:- dynamic(lgt_fentity_init_/1).			% lgt_fentity_init_(Goal)
:- dynamic(lgt_entity_comp_mode_/1).		% lgt_entity_comp_mode_(Type)

:- dynamic(lgt_redefined_built_in_/3).		% lgt_redefined_built_in_(Head, Context, THead)

:- dynamic(lgt_directive_/1).				% lgt_directive_(Dir)
:- dynamic(lgt_rclause_/1).					% lgt_rclause_(Clause)
:- dynamic(lgt_eclause_/1).					% lgt_eclause_(Clause)
:- dynamic(lgt_feclause_/1).				% lgt_feclause_(Clause)

:- dynamic(lgt_defs_pred_/1).				% lgt_defs_pred_(Functor/Arity)
:- dynamic(lgt_calls_pred_/1).				% lgt_calls_pred_(Functor/Arity)

:- dynamic(lgt_current_compiler_option_/2).	% lgt_current_compiler_option_(Option, Value)
:- dynamic(lgt_flag_/2).					% lgt_flag_(Option, Value)

:- dynamic(lgt_referenced_object_/1).		% lgt_referenced_object_(Object)
:- dynamic(lgt_referenced_protocol_/1).		% lgt_referenced_protocol_(Protocol)
:- dynamic(lgt_referenced_category_/1).		% lgt_referenced_object_(Category)




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
	lgt_sender(Context, user),
	lgt_this(Context, user),
	lgt_self(Context, Obj),
	lgt_tr_msg(Obj, Pred, Call, Context),
	call(Call).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_object(?object_identifier)

current_object(Obj) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), current_object(Obj))).

current_object(Obj) :-
	lgt_current_object_(Obj, _, _, _, _).



% current_protocol(?protocol_identifier)

current_protocol(Ptc) :-
	nonvar(Ptc),
	\+ lgt_valid_protocol_id(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), current_protocol(Ptc))).

current_protocol(Ptc) :-
	lgt_current_protocol_(Ptc, _).



% current_category(?category_identifier)

current_category(Ctg) :-
	nonvar(Ctg),
	\+ lgt_valid_category_id(Ctg),
	throw(error(type_error(category_identifier, Ctg), current_category(Ctg))).

current_category(Ctg) :-
	lgt_current_category_(Ctg, _).



% object_property(?object_identifier, ?object_property)

object_property(Obj, Prop) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), object_property(Obj, Prop))).

object_property(Obj, Prop) :-
	nonvar(Prop),
	\+ lgt_member(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(object_property, Prop), object_property(Obj, Prop))).

object_property(user, built_in).

object_property(Obj, Prop) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	functor(Pred, Prefix, 7),
	(lgt_predicate_property(Pred, (dynamic)) ->
		Prop = (dynamic)
		;
		Prop = static).



% category_property(?category_identifier, ?category_property)

category_property(Ctg, Prop) :-
	nonvar(Ctg),
	\+ lgt_valid_category_id(Ctg),
	throw(error(type_error(category_identifier, Ctg), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	nonvar(Prop),
	\+ lgt_member(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(category_property, Prop), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	lgt_current_category_(Ctg, Prefix),
	functor(Pred, Prefix, 2),
	(lgt_predicate_property(Pred, (dynamic)) ->
		Prop = (dynamic)
		;
		Prop = static).



% protocol_property(?protocol_identifier, ?protocol_property)

protocol_property(Ptc, Prop) :-
	nonvar(Ptc),
	\+ lgt_valid_protocol_id(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	nonvar(Prop),
	\+ lgt_member(Prop, [(dynamic), static, built_in]),
	throw(error(domain_error(protocol_property, Prop), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	lgt_current_protocol_(Ptc, Prefix),
	functor(Pred, Prefix, 1),
	(lgt_predicate_property(Pred, (dynamic)) ->
		Prop = (dynamic)
		;
		Prop = static).



% create_object(+object_identifier, +list, +list, +list)

create_object(Obj, Rels, Dirs, Clauses) :-
	var(Obj),
	throw(error(instantiation_error, create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	lgt_current_object_(Obj, _, _, _, _),
	throw(error(permission_error(replace, object, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	lgt_current_category_(Obj, _),
	throw(error(permission_error(replace, category, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	lgt_current_protocol_(Obj, _),
	throw(error(permission_error(replace, protocol, Obj), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Rels); \+ lgt_proper_list(Rels)),
	throw(error(type_error(list, Rels), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Dirs); \+ lgt_proper_list(Dirs)),
	throw(error(type_error(list, Dirs), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	(var(Clauses); \+ lgt_proper_list(Clauses)),
	throw(error(type_error(list, Clauses), create_object(Obj, Rels, Dirs, Clauses))).

create_object(Obj, Rels, Dirs, Clauses) :-
	lgt_clean_up,
	lgt_tr_directive(object, [Obj| Rels]),
	lgt_tr_directives([(dynamic)| Dirs]),
	lgt_tr_clauses(Clauses),
	lgt_fix_redef_built_ins,
	lgt_gen_object_clauses,
	lgt_gen_object_directives,
	lgt_assert_tr_entity,
	lgt_report_unknown_entities.



% create_category(+category_identifier, +list, +list, +list)

create_category(Ctg, Rels, Dirs, Clauses) :-
	var(Ctg),
	throw(error(instantiation_error, create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	\+ lgt_valid_category_id(Ctg),
	throw(error(type_error(category_identifier, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	lgt_current_category_(Ctg, _),
	throw(error(permission_error(replace, category, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	lgt_current_object_(Ctg, _, _, _, _),
	throw(error(permission_error(replace, object, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	lgt_current_protocol_(Ctg, _),
	throw(error(permission_error(replace, protocol, Ctg), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Rels); \+ lgt_proper_list(Rels)),
	throw(error(type_error(list, Rels), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Dirs); \+ lgt_proper_list(Dirs)),
	throw(error(type_error(list, Dirs), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	(var(Clauses); \+ lgt_proper_list(Clauses)),
	throw(error(type_error(list, Clauses), create_category(Ctg, Rels, Dirs, Clauses))).

create_category(Ctg, Rels, Dirs, Clauses) :-
	lgt_clean_up,
	lgt_tr_directive(category, [Ctg| Rels]),
	lgt_tr_directives([(dynamic)| Dirs]),
	lgt_tr_clauses(Clauses),
	lgt_fix_redef_built_ins,
	lgt_gen_category_clauses,
	lgt_gen_category_directives,
	lgt_assert_tr_entity,
	lgt_report_unknown_entities.



% create_protocol(+protocol_identifier, +list, +list)

create_protocol(Ptc, Rels, Dirs) :-
	var(Ptc),
	throw(error(instantiation_error, create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	\+ lgt_valid_protocol_id(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	lgt_current_protocol_(Ptc, _),
	throw(error(permission_error(replace, protocol, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	lgt_current_object_(Ptc, _, _, _, _),
	throw(error(permission_error(replace, object, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	lgt_current_category_(Ptc, _),
	throw(error(permission_error(replace, category, Ptc), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	(var(Rels); \+ lgt_proper_list(Rels)),
	throw(error(type_error(list, Rels), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	(var(Dirs); \+ lgt_proper_list(Dirs)),
	throw(error(type_error(list, Dirs), create_protocol(Ptc, Rels, Dirs))).

create_protocol(Ptc, Rels, Dirs) :-
	lgt_clean_up,
	lgt_tr_directive(protocol, [Ptc| Rels]),
	lgt_tr_directives([(dynamic)| Dirs]),
	lgt_gen_protocol_clauses,
	lgt_gen_protocol_directives,
	lgt_assert_tr_entity,
	lgt_report_unknown_entities.



% abolish_object(@object_identifier)

abolish_object(Obj) :-
	var(Obj),
	throw(error(instantiation_error, abolish_object(Obj))).

abolish_object(Obj) :-
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_object(Obj))).

abolish_object(Obj) :-
	lgt_current_object_(Obj, Prefix, _, _, _) ->
		(object_property(Obj, (dynamic)) ->
			lgt_once(Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			forall(
				lgt_call(Def, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity),
				 abolish(Functor/Arity))),
			forall(
				lgt_call(DDef, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity),
				 abolish(Functor/Arity))),
			abolish(Dcl/4),
			abolish(Dcl/6),
			abolish(Def/5),
			abolish(Def/6),
			abolish(Super/6),
			abolish(IDcl/6),
			abolish(IDef/6),
			abolish(DDcl/4),
			abolish(DDef/5),
			abolish(Prefix/7),
			retractall(lgt_current_object_(Obj, _, _, _, _)),
			retractall(lgt_extends_object_(Obj, _, _)),
			retractall(lgt_instantiates_class_(Obj, _, _)),
			retractall(lgt_specializes_class_(Obj, _, _)),
			retractall(lgt_implements_protocol_(Obj, _, _)),
			retractall(lgt_imports_category_(Obj, _, _))			
			;
			throw(error(permission_error(modify, static_object, Obj), abolish_object(Obj))))
		;
		throw(error(existence_error(object, Obj), abolish_object(Obj))).



% abolish_category(@category_identifier)

abolish_category(Ctg) :-
	var(Ctg),
	throw(error(instantiation_error, abolish_category(Ctg))).

abolish_category(Ctg) :-
	\+ lgt_valid_category_id(Ctg),
	throw(error(type_error(category_identifier, Ctg), abolish_category(Ctg))).

abolish_category(Ctg) :-
	lgt_current_category_(Ctg, Prefix) ->
		(category_property(Ctg, (dynamic)) ->
			lgt_once(Prefix, Dcl, Def),
			forall(
				lgt_call(Def, _, _, _, _, Pred),
				(functor(Pred, Functor, Arity),
				 abolish(Functor/Arity))),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Def/5),
			abolish(Prefix/2),
			retractall(lgt_current_category_(Ctg, _)),
			retractall(lgt_implements_protocol_(Ctg, _, _))
			;
			throw(error(permission_error(modify, static_category, Ctg), abolish_category(Ctg))))
		;
		throw(error(existence_error(category, Ctg), abolish_category(Ctg))).



% abolish_protocol(@protocol_identifier)

abolish_protocol(Ptc) :-
	var(Ptc),
	throw(error(instantiation_error, abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	\+ lgt_valid_protocol_id(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), abolish_protocol(Ptc))).

abolish_protocol(Ptc) :-
	lgt_current_protocol_(Ptc, Prefix) ->
		(protocol_property(Ptc, (dynamic)) ->
			lgt_once(Prefix, Dcl),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Prefix/1),
			retractall(lgt_current_protocol_(Ptc, _)),
			retractall(lgt_extends_protocol_(Ptc, _, _))
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
	\+ lgt_valid_object_id(Entity),
	throw(error(type_error(object_identifier, Entity), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	nonvar(Ptc),
	\+ lgt_valid_protocol_id(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), implements_protocol(Entity, Ptc, Scope))).

implements_protocol(Entity, Ptc, Scope) :-
	lgt_implements_protocol_(Entity, Ptc, Scope).



% imports_category(?term, ?term)

imports_category(Obj, Ctg) :-
	catch(
		imports_category(Obj, Ctg, _),
		error(Error, _),
		throw(error(Error, imports_category(Obj, Ctg)))).



% imports_category(?term, ?term, ?atom)

imports_category(Obj, Ctg, Scope) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	nonvar(Ctg),
	\+ lgt_valid_category_id(Ctg),
	throw(error(type_error(category_identifier, Ctg), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), imports_category(Obj, Ctg, Scope))).

imports_category(Obj, Ctg, Scope) :-
	lgt_imports_category_(Obj, Ctg, Scope).



% instantiates_class(?term, ?term)

instantiates_class(Obj, Class) :-
	catch(
		instantiates_class(Obj, Class, _),
		error(Error, _),
		throw(error(Error, instantiates_class(Obj, Class)))).



% instantiates_class(?term, ?term, ?atom)

instantiates_class(Obj, Class, Scope) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Class),
	\+ lgt_valid_object_id(Class),
	throw(error(type_error(object_identifier, Class), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), instantiates_class(Obj, Class, Scope))).

instantiates_class(Obj, Class, Scope) :-
	lgt_instantiates_class_(Obj, Class, Scope).



% specializes_class(?term, ?term)

specializes_class(Class, Superclass) :-
	catch(
		specializes_class(Class, Superclass, _),
		error(Error, _),
		throw(error(Error, specializes_class(Class, Superclass)))).



% specializes_class(?term, ?term, ?atom)

specializes_class(Class, Superclass, Scope) :-
	nonvar(Class),
	\+ lgt_valid_object_id(Class),
	throw(error(type_error(object_identifier, Class), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Superclass),
	\+ lgt_valid_object_id(Superclass),
	throw(error(type_error(object_identifier, Superclass), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), specializes_class(Class, Superclass, Scope))).

specializes_class(Class, Superclass, Scope) :-
	lgt_specializes_class_(Class, Superclass, Scope).



% extends_protocol(?atom, ?atom)

extends_protocol(Ptc1, Ptc2) :-
	catch(
		extends_protocol(Ptc1, Ptc2, _),
		error(Error, _),
		throw(error(Error, extends_protocol(Ptc1, Ptc2)))).



% extends_protocol(?atom, ?atom, ?atom)

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc1),
	\+ lgt_valid_protocol_id(Ptc1),
	throw(error(type_error(protocol_identifier, Ptc1), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Ptc2),
	\+ lgt_valid_protocol_id(Ptc2),
	throw(error(type_error(protocol_identifier, Ptc2), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), extends_protocol(Ptc1, Ptc2, Scope))).

extends_protocol(Ptc1, Ptc2, Scope) :-
	lgt_extends_protocol_(Ptc1, Ptc2, Scope).



% extends_object(?term, ?term)

extends_object(Prototype, Parent) :-
	catch(
		extends_object(Prototype, Parent, _),
		error(Error, _),
		throw(error(Error, extends_object(Prototype, Parent)))).



% extends_object(?term, ?term, ?atom)

extends_object(Prototype, Parent, Scope) :-
	nonvar(Prototype),
	\+ lgt_valid_object_id(Prototype),
	throw(error(type_error(object_identifier, Prototype), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	nonvar(Parent),
	\+ lgt_valid_object_id(Parent),
	throw(error(type_error(object_identifier, Parent), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	nonvar(Scope),
	\+ lgt_member(Scope, [(public), protected, private]),
	throw(error(type_error(scope, Scope), extends_object(Prototype, Parent, Scope))).

extends_object(Prototype, Parent, Scope) :-
	lgt_extends_object_(Prototype, Parent, Scope).



% current_event(?event, ?object_identifier, ?callable, ?object_identifier, ?object_identifier)

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ lgt_callable(Msg),
	throw(error(type_error(callable, Msg), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ lgt_valid_object_id(Sender),
	throw(error(type_error(object_identifier, Sender), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ lgt_valid_object_id(Monitor),
	throw(error(type_error(object_identifier, Monitor), current_event(Event, Obj, Msg, Sender, Monitor))).

current_event(before, Obj, Msg, Sender, Monitor) :-
	lgt_before_(Obj, Msg, Sender, Monitor, _).

current_event(after, Obj, Msg, Sender, Monitor) :-
	lgt_after_(Obj, Msg, Sender, Monitor, _).



%define_events(@event, @object_identifier, @callable, @object_identifier, +object_identifier)

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ lgt_callable(Msg),
	throw(error(type_error(callable, Msg), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ lgt_valid_object_id(Sender),
	throw(error(type_error(object_identifier, Sender), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Monitor),
	throw(error(instantiation_error, define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ lgt_valid_object_id(Monitor),
	throw(error(type_error(object_identifier, Monitor), define_events(Event, Obj, Msg, Sender, Monitor))).

define_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Event),
	!,
	lgt_current_object_(Monitor, _, _, Def, _),
	lgt_call(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, BCall, _),
	lgt_call(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, ACall, _),
	retractall(lgt_before_(Obj, Msg, Sender, Monitor, _)),
	assertz(lgt_before_(Obj, Msg, Sender, Monitor, BCall)),
	retractall(lgt_after_(Obj, Msg, Sender, Monitor, _)),
	assertz(lgt_after_(Obj, Msg, Sender, Monitor, ACall)).

define_events(before, Obj, Msg, Sender, Monitor) :-
	lgt_current_object_(Monitor, _, _, Def, _),
	lgt_call(Def, before(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
	retractall(lgt_before_(Obj, Msg, Sender, Monitor, _)),
	assertz(lgt_before_(Obj, Msg, Sender, Monitor, Call)).

define_events(after, Obj, Msg, Sender, Monitor) :-
	lgt_current_object_(Monitor, _, _, Def, _),
	lgt_call(Def, after(Obj, Msg, Sender), Monitor, Monitor, Monitor, Call, _),
	retractall(lgt_after_(Obj, Msg, Sender, Monitor, _)),
	assertz(lgt_after_(Obj, Msg, Sender, Monitor, Call)).



% abolish_events(@event, @object_identifier, @callable, @object_identifier, @object_identifier)

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Event),
	Event \= before,
	Event \= after,
	throw(error(type_error(event, Event), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	throw(error(type_error(object_identifier, Obj), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Msg),
	\+ lgt_callable(Msg),
	throw(error(type_error(callable, Msg), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Sender),
	\+ lgt_valid_object_id(Sender),
	throw(error(type_error(object_identifier, Sender), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	nonvar(Monitor),
	\+ lgt_valid_object_id(Monitor),
	throw(error(type_error(object_identifier, Monitor), abolish_events(Event, Obj, Msg, Sender, Monitor))).

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	var(Event),
	!,
	retractall(lgt_before_(Obj, Msg, Sender, Monitor, _)),
	retractall(lgt_after_(Obj, Msg, Sender, Monitor, _)).

abolish_events(before, Obj, Msg, Sender, Monitor) :-
	retractall(lgt_before_(Obj, Msg, Sender, Monitor, _)).

abolish_events(after, Obj, Msg, Sender, Monitor) :-
	retractall(lgt_after_(Obj, Msg, Sender, Monitor, _)).



% compiling and loading built-in predicates


% lgt_compiler_option(+atom, ?atom)
%
% gets/check the current value of a compiler option

lgt_compiler_option(Option, Value) :-
	lgt_current_compiler_option_(Option, Value2),
	!,
	Value = Value2.

lgt_compiler_option(Option, Value) :-
	lgt_flag_(Option, Value2),
	!,
	Value = Value2.

lgt_compiler_option(Option, Value) :-
	lgt_default_flag(Option, Value).



% logtalk_compile(+list)
%
% compiles to disk a list of entities using default options

logtalk_compile(Entities) :-
	catch(
		logtalk_compile(Entities, []),
		error(Error, _),
		throw(error(Error, logtalk_compile(Entities)))).



% logtalk_compile(+list, +list)
%
% compiles to disk a list of entities using a list of options

logtalk_compile(Entities, Options) :-
	catch(
		(lgt_check_compiler_entities(Entities),
		 lgt_check_compiler_options(Options),
		 lgt_set_compiler_options(Options),
		 lgt_compile_entities(Entities)),
		Error,
		throw(error(Error, logtalk_compile(Entities, Options)))).



% lgt_check_compiler_entities(+list)
%
% check if the entities names are valid and if the corresponding
% files exist in the current working directory

lgt_check_compiler_entities(Entities) :-
	var(Entities),
	throw(instantiation_error).

lgt_check_compiler_entities(Entities) :-
	\+ lgt_proper_list(Entities),
	throw(type_error(list, Entities)).

lgt_check_compiler_entities(Entities) :-
	lgt_check_compiler_entity_list(Entities).



lgt_check_compiler_entity_list([]).

lgt_check_compiler_entity_list([Entity| Entities]) :-
	lgt_check_compiler_entity(Entity),
	lgt_check_compiler_entity_list(Entities).



lgt_check_compiler_entity(Entity) :-
	\+ atom(Entity),
	throw(type_error(atom, Entity)).

lgt_check_compiler_entity(Entity) :-
	lgt_file_name(logtalk, Entity, File),
	\+ lgt_file_exists(File),
	throw(existence_error(entity, Entity)).

lgt_check_compiler_entity(_).



% lgt_check_compiler_options(+list)
%
% check if the compiler options are valid

lgt_check_compiler_options(Options) :-
	var(Options),
	throw(instantiation_error).

lgt_check_compiler_options(Options) :-
	\+ lgt_proper_list(Options),
	throw(type_error(list, Options)).

lgt_check_compiler_options(Options) :-
	lgt_check_compiler_option_list(Options).



lgt_check_compiler_option_list([]).

lgt_check_compiler_option_list([Option| Options]) :-
	lgt_check_compiler_option(Option),
	lgt_check_compiler_option_list(Options).



lgt_check_compiler_option(Option) :-
	lgt_valid_compiler_option(Option) ->
		true
		;
		throw(type_error(compiler_option, Option)).



% lgt_set_compiler_options(+list)
%
% sets the compiler options

lgt_set_compiler_options(Options) :-
	retractall(lgt_current_compiler_option_(_, _)),
	lgt_assert_compiler_options(Options).


lgt_assert_compiler_options([]).

lgt_assert_compiler_options([Option| Options]) :-
	Option =.. [Key, Value],
	asserta(lgt_current_compiler_option_(Key, Value)),
	lgt_assert_compiler_options(Options).



% logtalk_load(+list)
%
% compiles to disk and then loads to memory a 
% list of entities using default options

logtalk_load(Entities) :-
	catch(
		logtalk_load(Entities, []),
		error(Error, _),
		throw(error(Error, logtalk_load(Entities)))).



% logtalk_load(+list, +list)
%
% compiles to disk and then loads to memory a 
% list of entities using a list of options

logtalk_load(Entities, Options) :-
	catch(
		(lgt_check_compiler_entities(Entities),
		 lgt_check_compiler_options(Options),
		 lgt_set_compiler_options(Options),
		 lgt_load_entities(Entities)),
		Error,
		throw(error(Error, logtalk_load(Entities, Options)))).



% logtalk_version(?integer, ?integer, ?integer)

logtalk_version(Major, Minor, Patch) :-
	nonvar(Major),
	\+ integer(Major),
	throw(error(type_error(integer, Major), logtalk_version(Major, Minor, Patch))).

logtalk_version(Major, Minor, Patch) :-
	nonvar(Minor),
	\+ integer(Minor),
	throw(error(type_error(integer, Minor), logtalk_version(Major, Minor, Patch))).

logtalk_version(Major, Minor, Patch) :-
	nonvar(Patch),
	\+ integer(Patch),
	throw(error(type_error(integer, Patch), logtalk_version(Major, Minor, Patch))).

logtalk_version(2, 12, 0).



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
	\+ lgt_valid_flag(Flag),
	throw(error(domain_error(valid_flag, Flag), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	lgt_read_only_flag(Flag),
	throw(error(permission_error(modify, read_only_flag, Flag), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	\+ lgt_valid_flag(Flag, Value),
	throw(error(domain_error(valid_flag_value, Value), set_logtalk_flag(Flag, Value))).

set_logtalk_flag(Flag, Value) :-
	retractall(lgt_flag_(Flag, _)),
	assertz(lgt_flag_(Flag, Value)).



% current_logtalk_flag(?atom, ?nonvar)
%
% tests/gets Logtalk flags

current_logtalk_flag(Flag, Value) :-
	nonvar(Flag),
	\+ atom(Flag),
	throw(error(type_error(atom, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	atom(Flag),
	\+ lgt_valid_flag(Flag),
	throw(error(domain_error(valid_flag, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	lgt_flag_(Flag, Value).

current_logtalk_flag(Flag, Value) :-
	\+ lgt_flag_(Flag, _),
	lgt_default_flag(Flag, Value).

current_logtalk_flag(version, version(2, 12, 0)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in methods
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_predicate/1 built-in method

lgt_current_predicate(Obj, Pred, Sender, _) :-
	nonvar(Pred),
	Pred \= _/_,
	throw(error(type_error(predicate_indicator, Pred), Obj::current_predicate(Pred), Sender)).

lgt_current_predicate(Obj, Functor/Arity, Sender, _) :-
	nonvar(Functor),
	\+ atom(Functor),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

lgt_current_predicate(Obj, Functor/Arity, Sender, _) :-
	nonvar(Arity),
	\+ (integer(Arity), Arity >= 0),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

lgt_current_predicate(Obj, Functor/Arity, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::current_predicate(Functor/Arity), Sender)).

lgt_current_predicate(Obj, Functor/Arity, Sender, Scope) :-
	nonvar(Functor),
	nonvar(Arity),
	!,
	functor(Pred, Functor, Arity),
	lgt_current_object_(Obj, _, Dcl, _, _),
	lgt_once(Dcl, Pred, PScope, _, _, SContainer, _),
	once((\+ \+ PScope = Scope; Sender = SContainer)).

lgt_current_predicate(Obj, Functor/Arity, Sender, Scope) :-
	lgt_current_object_(Obj, _, Dcl, _, _),
	findall(
		Functor/Arity-(PScope, SContainer),
		(lgt_call(Dcl, Pred, PScope, _, _, SContainer, _),
		 functor(Pred, Functor, Arity)),
		Preds),
	lgt_cp_filter(Preds, Filtered),
	lgt_member(Functor/Arity-(PScope, SContainer), Filtered),
	once((\+ \+ PScope = Scope; Sender = SContainer)).


% lgt_cp_filter(+list, -list)
%
% removes duplicated and redeclared predicates 

lgt_cp_filter([], []).

lgt_cp_filter([Data| Rest], [Data| Rest2]) :-
	lgt_cp_remove_all(Rest, Data, Aux),
	lgt_cp_filter(Aux, Rest2).


lgt_cp_remove_all([], _, []).

lgt_cp_remove_all([F/A-_| Rest], F/A-D, List) :-
	!,
	lgt_cp_remove_all(Rest, F/A-D, List).

lgt_cp_remove_all([Data| Rest], Filter, [Data| Rest2]) :-
	!,
	lgt_cp_remove_all(Rest, Filter, Rest2).



% predicate_property/2 built-in method

lgt_predicate_property(Obj, Pred, Prop, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::predicate_property(Pred, Prop), Sender)).

lgt_predicate_property(Obj, Pred, Prop, Sender, _) :-
	nonvar(Prop),
	\+ lgt_member(Prop, [(public), protected, private, static, (dynamic), declared_in(_), defined_in(_), metapredicate(_), built_in]),
	throw(error(domain_error(predicate_property, Prop), Obj::predicate_property(Pred, Prop), Sender)).

lgt_predicate_property(Obj, Pred, Prop, Sender, _) :-
	\+ lgt_callable(Pred),
	throw(error(type_error(callable, Pred), Obj::predicate_property(Pred, Prop), Sender)).

lgt_predicate_property(Obj, Pred, Prop, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::predicate_property(Pred, Prop), Sender)).

lgt_predicate_property(Obj, Pred, Prop, Sender, Scope) :-
	lgt_current_object_(Obj, _, Dcl, Def, _),
	lgt_once(Dcl, Pred, PScope, Type, Meta, SContainer, TContainer),
	!,
	once((\+ \+ PScope = Scope; Sender = SContainer)),
	(lgt_scope(Prop, PScope);
	 Prop = Type;
	 Prop = declared_in(TContainer);
	 lgt_once(Def, Pred, _, _, _, _, DContainer),
	 Prop = defined_in(DContainer);
	 Meta \= no,
	 Prop = metapredicate(Meta)).

lgt_predicate_property(_, Pred, Prop, _, Scope) :-
	lgt_built_in_method(Pred, PScope),
	!,
	\+ \+ PScope = Scope,
	(lgt_scope(Prop, PScope);
	 Prop = static;
	 Prop = built_in).

lgt_predicate_property(_, Pred, Prop, _, _) :-
	lgt_built_in(Pred),
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	(Prop = (public);
	 (lgt_predicate_property(Pred, (dynamic)) -> Prop = (dynamic); Prop = static);
	 Prop = built_in;
	 (lgt_pl_metapredicate(Meta) -> Prop = metapredicate(Meta))).


% lgt_scope(?atom, ?term).
%
% converts between user and system scope terms

lgt_scope(private, p).
lgt_scope(protected, p(p)).
lgt_scope((public), p(p(p))).



% abolish/1 built-in method

lgt_abolish(Obj, Pred, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::abolish(Pred), Sender)).

lgt_abolish(Obj, Pred, Sender, _) :-
	Pred \= _/_,
	throw(error(type_error(predicate_indicator, Pred), Obj::abolish(predicate), Sender)).

lgt_abolish(Obj, Functor/Arity, Sender, _) :-
	(var(Functor); var(Arity)),
	throw(error(instantiation_error, Obj::abolish(Functor/Arity), Sender)).

lgt_abolish(Obj, Functor/Arity, Sender, _) :-
	\+ atom(Functor),
	throw(error(type_error(atom, Functor), Obj::abolish(Functor/Arity), Sender)).

lgt_abolish(Obj, Functor/Arity, Sender, _) :-
	\+ integer(Arity),
	throw(error(type_error(integer, Arity), Obj::abolish(Functor/Arity), Sender)).

lgt_abolish(Obj, Functor/Arity, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, Dcl, _, _) ->
		((functor(Pred, Functor, Arity),
		  lgt_once(Dcl, Pred, PScope, Compilation, _, SContainer, _)) ->
			((\+ \+ PScope = Scope; Sender = SContainer) ->
				(Compilation = (dynamic) ->
					lgt_once(Prefix, _, _, _, _, _, DDcl, DDef),
					(lgt_once(DDcl, Pred, _, _, _) ->
						Clause =.. [DDcl, Pred, _, _, _],
						retractall(Clause),
						(lgt_once(DDef, Pred, _, _, _, Call) ->
							functor(Call, CFunctor, CArity),
							abolish(CFunctor/CArity),
							Clause2 =.. [DDef, Pred, _, _, _, Call],
							retractall(Clause2)
							;
							true)
						;
						(lgt_once(Dcl, Pred, _, _, _) ->
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

lgt_asserta(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::asserta(Clause), Sender)).

lgt_asserta(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::asserta((Head:-Body)), Sender)).

lgt_asserta(Obj, (Head:-Body), Sender, _) :-
	\+ lgt_callable(Head),
	throw(error(type_error(callable, Head), Obj::asserta((Head:-Body)), Sender)).

lgt_asserta(Obj, (Head:-Body), Sender, _) :-
	\+ lgt_callable(Body),
	throw(error(type_error(callable, Body), Obj::asserta((Head:-Body)), Sender)).

lgt_asserta(Obj, Clause, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::asserta(Clause), Sender)).

lgt_asserta(Obj, (Head:-Body), Sender, Scope) :-
	!,
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	(lgt_once(Dcl, Head, PScope, Type, Meta, SContainer, _) ->
	 	true
	 	;
	 	lgt_convert_test_scope(Scope, Scope2),
	 	lgt_assert_ddcl_clause(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SContainer)  ->
			((lgt_once(Def, Head, Sender2, This, Self, Call); lgt_once(DDef, Head, Sender2, This, Self, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				lgt_assert_ddef_clause(Functor, Arity, Prefix, DDef, _),
				lgt_once(DDef, Head, Sender2, This, Self, Call)),
			lgt_self(Context, Self),
			lgt_this(Context, This),
			lgt_sender(Context, Sender2),
			lgt_prefix(Context, Prefix),
			(nonvar(Meta) ->
				Head =.. [_| Args],
				Meta =.. [_| MArgs],
				lgt_extract_metavars(Args, MArgs, Metavars)
				;
				Metavars = []),
			lgt_metavars(Context, Metavars),
			asserta((Call:-lgt_tr_body(Body, TBody, Context), call(TBody)))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta((Head:-Body)), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta((Head:-Body)), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta((Head:-Body)), Sender))).

lgt_asserta(Obj, Head, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
	 	true
	 	;
	 	lgt_convert_test_scope(Scope, Scope2),
	 	lgt_assert_ddcl_clause(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SContainer)  ->
			((lgt_once(Def, Head, _, _, _, Call); lgt_once(DDef, Head, _, _, _, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				lgt_assert_ddef_clause(Functor, Arity, Prefix, DDef, _),
				lgt_once(DDef, Head, _, _, _, Call)),
			asserta(Call)
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta(Head), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta(Head), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta(Head), Sender))).



% assertz/1 built-in method

lgt_assertz(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::assertz(Clause), Sender)).

lgt_assertz(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::assertz((Head:-Body)), Sender)).

lgt_assertz(Obj, (Head:-Body), Sender, _) :-
	\+ lgt_callable(Head),
	throw(error(type_error(callable, Head), Obj::assertz((Head:-Body)), Sender)).

lgt_assertz(Obj, (Head:-Body), Sender, _) :-
	\+ lgt_callable(Body),
	throw(error(type_error(callable, Body), Obj::assertz((Head:-Body)), Sender)).

lgt_assertz(Obj, Clause, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::assertz(Clause), Sender)).

lgt_assertz(Obj, (Head:-Body), Sender, Scope) :-
	!,
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	(lgt_once(Dcl, Head, PScope, Type, Meta, SContainer, _) ->
	 	true
	 	;
	 	lgt_convert_test_scope(Scope, Scope2),
	 	lgt_assert_ddcl_clause(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SContainer)  ->
			((lgt_once(Def, Head, Sender2, This, Self, Call); lgt_once(DDef, Head, Sender2, This, Self, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				lgt_assert_ddef_clause(Functor, Arity, Prefix, DDef, _),
				lgt_once(DDef, Head, Sender2, This, Self, Call)),
			lgt_self(Context, Self),
			lgt_this(Context, This),
			lgt_sender(Context, Sender2),
			lgt_prefix(Context, Prefix),
			(nonvar(Meta) ->
				Head =.. [_| Args],
				Meta =.. [_| MArgs],
				lgt_extract_metavars(Args, MArgs, Metavars)
				;
				Metavars = []),
			lgt_metavars(Context, Metavars),
			assertz((Call:-lgt_tr_body(Body, TBody, Context), call(TBody)))
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz((Head:-Body)), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz((Head:-Body)), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz((Head:-Body)), Sender))).

lgt_assertz(Obj, Head, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, DDcl, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
	 	true
	 	;
	 	lgt_convert_test_scope(Scope, Scope2),
	 	lgt_assert_ddcl_clause(DDcl, Head, Scope2)),
	(Type = (dynamic) ->
		((\+ \+ PScope = Scope; Sender = SContainer)  ->
			((lgt_once(Def, Head, _, _, _, Call); lgt_once(DDef, Head, _, _, _, Call)) ->
				true
				;
				functor(Head, Functor, Arity),
				lgt_assert_ddef_clause(Functor, Arity, Prefix, DDef, _),
				lgt_once(DDef, Head, _, _, _, Call)),
			assertz(Call)
			;
			(PScope = p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz(Head), Sender))
				;
				throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz(Head), Sender))))
		;
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz(Head), Sender))).



% clause/2 built-in method

lgt_clause(Obj, Head, Body, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::clause(Head, Body), Sender)).

lgt_clause(Obj, Head, Body, Sender, _) :-
	\+ lgt_callable(Head),
	throw(error(type_error(callable, Head), Obj::clause(Head, Body), Sender)).

lgt_clause(Obj, Head, Body, Sender, _) :-
	nonvar(Body),
	\+ lgt_callable(Body),
	throw(error(type_error(callable, Body), Obj::clause(Head, Body), Sender)).

lgt_clause(Obj, Head, Body, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::clause(Head, Body), Sender)).

lgt_clause(Obj, Head, Body, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, _, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SContainer) ->
				once((lgt_once(Def, Head, _, _, _, Call); lgt_once(DDef, Head, _, _, _, Call))),
				clause(Call, TBody),
				(TBody = (lgt_tr_body(Body, _, _), _) ->
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

lgt_retract(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::retract(Clause), Sender)).

lgt_retract(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retract((Head:-Body)), Sender)).

lgt_retract(Obj, (Head:-Body), Sender, _) :-
	\+ lgt_callable(Head),
	throw(error(type_error(callable, Head), Obj::retract((Head:-Body)), Sender)).

lgt_retract(Obj, Clause, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::retract(Clause), Sender)).

lgt_retract(Obj, (Head:-Body), Sender, Scope) :-
	!,
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, _, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SContainer) ->
				(lgt_once(Def, Head, _, _, _, Call) ->
					retract((Call:-(lgt_tr_body(Body, _, _), _)))
					;
					(lgt_once(DDef, Head, _, _, _, Call) ->
						retract((Call:-(lgt_tr_body(Body, _, _), _))),
						lgt_update_ddef_table(DDef, Call)
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

lgt_retract(Obj, Head, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, _, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SContainer) ->
				(lgt_once(Def, Head, _, _, _, Call) ->
					retract(Call)
					;
					(lgt_once(DDef, Head, _, _, _, Call) ->
						retract(Call),
						lgt_update_ddef_table(DDef, Call)
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

lgt_retractall(Obj, Head, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retractall(Head), Sender)).

lgt_retractall(Obj, Head, Sender, _) :-
	\+ lgt_callable(Head),
	throw(error(type_error(callable, Head), Obj::retractall(Head), Sender)).

lgt_retractall(Obj, Head, Sender, _) :-
	\+ lgt_current_object_(Obj, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::retractall(Head), Sender)).

lgt_retractall(Obj, Head, Sender, Scope) :-
	lgt_current_object_(Obj, Prefix, _, _, _),
	lgt_once(Prefix, Dcl, Def, _, _, _, _, DDef),
	(lgt_once(Dcl, Head, PScope, Type, _, SContainer, _) ->
		(Type = (dynamic) ->
			((\+ \+ PScope = Scope; Sender = SContainer) ->
				(lgt_once(Def, Head, _, _, _, Call) ->
					retractall(Call)
					;
					(lgt_once(DDef, Head, _, _, _, Call) ->
						retractall(Call),
						lgt_update_ddef_table(DDef, Call)
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message sending
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% lgt_send_to_self(+object, ?term, +object)

lgt_send_to_self(Self, Pred, This) :-
	nonvar(Pred) ->
		lgt_send_to_self_nv(Self, Pred, This)
		;
		throw(error(instantiation_error, Self::Pred, This)).



% lgt_send_to_self_nv(+object, +term, +object)

lgt_send_to_self_nv(Self, Pred, This) :-
	lgt_current_object_(Self, _, Dcl, Def, _),
	(lgt_call(Dcl, Pred, Scope, _, _, SContainer, _) ->
		((Scope = p(_); This = SContainer) ->
			lgt_once(Def, Pred, This, Self, Self, Call, _),
			call(Call)
			;
			throw(error(permission_error(access, private_predicate, Pred), Self::Pred, This)))
		;
		(lgt_built_in(Pred) ->
			call(Pred)
			;
			throw(error(existence_error(predicate_declaration, Pred), Self::Pred, This)))).



% lgt_send_to_object(@object, ?term, +object)

lgt_send_to_object(Obj, Pred, Sender) :-
	nonvar(Obj) ->
		(lgt_current_object_(Obj, _, Dcl, Def, _) ->
			(nonvar(Pred) ->
			 	(lgt_call(Dcl, Pred, Scope, _, _, _, _) ->
			 		(Scope = p(p(_)) ->
						lgt_once(Def, Pred, Sender, Obj, Obj, Call, _),
						\+ (lgt_before_(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
						call(Call),
						\+ (lgt_after_(Obj, Pred, Sender, _, ACall), \+ call(ACall))
						;
						(Scope = p ->
							throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
							;
							throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))))
					;
					(lgt_built_in(Pred) ->
						call(Pred)
						;
						throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))))
				;
				throw(error(instantiation_error, Obj::Pred, Sender)))
			;
			throw(error(existence_error(object, Obj), Obj::Pred, Sender)))
		;
		throw(error(instantiation_error, Obj::Pred, Sender)).



% lgt_send_to_object_nv(+object, +term, +object)

lgt_send_to_object_nv(Obj, Pred, Sender) :-
	lgt_current_object_(Obj, _, Dcl, Def, _) ->
		(lgt_call(Dcl, Pred, Scope, _, _, _, _) ->
			(Scope = p(p(_)) ->
				lgt_once(Def, Pred, Sender, Obj, Obj, Call, _),
				\+ (lgt_before_(Obj, Pred, Sender, _, BCall), \+ call(BCall)),
				call(Call),
				\+ (lgt_after_(Obj, Pred, Sender, _, ACall), \+ call(ACall))
				;
				(Scope = p ->
					throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
					;
					throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))))
			;
			(lgt_built_in(Pred) ->
				call(Pred)
				;
				throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))))
		;
		throw(error(existence_error(object, Obj), Obj::Pred, Sender)).



% lgt_send_to_super(+object, ?term, +object, +object)

lgt_send_to_super(Self, Pred, This, Sender) :-
	nonvar(Pred) ->
		lgt_send_to_super_nv(Self, Pred, This, Sender)
		;
		throw(error(instantiation_error, ^^Pred, This)).



% lgt_send_to_super_nv(+object, +term, +object, +object)

lgt_send_to_super_nv(Self, Pred, This, Sender) :-
	lgt_current_object_(Self, _, Dcl, _, _),
	(lgt_call(Dcl, Pred, Scope, _, _, SContainer, _) ->
	 	((Scope = p(_); This = SContainer) ->
			lgt_current_object_(This, _, _, _, Super),
			lgt_once(Super, Pred, Sender, This, Self, Call, Container),
			(Container \= This ->
				call(Call)
				;
				throw(error(endless_loop(Pred), ^^Pred, This)))
		;
		throw(error(permission_error(access, private_predicate, Pred), ^^Pred, This)))
	;
	(lgt_built_in(Pred) ->
		call(Pred)
		;
		throw(error(existence_error(predicate_declaration, Pred), ^^Pred, This)))).



% lgt_metacall_in_object(+object, ?term, +object)
%
% metacalls in predicate definitions

lgt_metacall_in_object(Obj, Pred, Sender) :-
	var(Pred) ->
		throw(error(instantiation_error, Obj::call(Pred), Sender))
		;
		(Obj = user ->
			call(Pred)
			;
			lgt_current_object_(Obj, Prefix, _, _, _),
			lgt_prefix(Context, Prefix),
			lgt_sender(Context, Sender),
			lgt_this(Context, Obj),
			lgt_self(Context, Obj),
			lgt_tr_body(Pred, Call, Context),
			call(Call)).



% lgt_call_built_in(+term, +term)
%
% needed for runtime translation of dynamic clauses

lgt_call_built_in(Pred, Context) :-
	lgt_sender(Context, Sender),
	lgt_this(Context, This),
	lgt_self(Context, Self),
	lgt_current_object_(This, _, _, Def, _),
	(lgt_call(Def, Pred, Sender, This, Self, Call) ->
		call(Call)
		;
		call(Pred)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in pseudo-object user
%
%  represents the Prolog database (excluding built-in predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% the following clauses correspond to a virtual 
% compilation of the pseudo-object user

lgt_current_object_(user, user0_, user0__dcl, user0__def, _).

user0_(user0__dcl, user0__def, _, _, _, _, _).

user0__dcl(Pred, p(p(p)), no, Type) :-
	nonvar(Pred),
	\+ lgt_built_in(Pred),
	functor(Pred, Functor, Arity),
	current_predicate(Functor/Arity),
	(lgt_predicate_property(Pred, (dynamic)) ->
		Type = (dynamic)
		;
		Type = static).

user0__dcl(Pred, p(p(p)), no, Type) :-
	var(Pred),
	current_predicate(Functor/Arity),
	\+ lgt_hidden_functor(Functor),
	functor(Pred, Functor, Arity),
	\+ lgt_built_in(Pred),
	(lgt_predicate_property(Pred, (dynamic)) ->
		Type = (dynamic)
		;
		Type = static).

user0__dcl(Pred, p(p(p)), Type, Meta, user, user) :-
	user0__dcl(Pred, p(p(p)), Type, Meta).

user0__def(Pred, _, _, _, Pred).

user0__def(Pred, _, _, _, Pred, user).



% lgt_hidden_functor(+atom)
%
% hidden functors include Logtalk pre-processor and runtime internal functors
% and those used in the compiled code of objects, protocols, and categories

lgt_hidden_functor(Functor) :-
	atom_concat(lgt_, _, Functor).

lgt_hidden_functor(Functor) :-
	lgt_current_category_(_, Prefix),
	atom_concat(Prefix, _, Functor).

lgt_hidden_functor(Functor) :-
	lgt_current_object_(_, Prefix, _, _, _),
	atom_concat(Prefix, _, Functor).

lgt_hidden_functor(Functor) :-
	lgt_current_protocol_(_, Prefix),
	atom_concat(Prefix, _, Functor).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor - compiles Logtalk source files to Prolog
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% lgt_load_entities(+list)
%
% compiles to disk and then loads to memory a list of entities

lgt_load_entities([]).

lgt_load_entities([Entity| Entities]) :-
	lgt_load_entity(Entity),
	lgt_load_entities(Entities).



% lgt_load_entity(+atom)
%
% compiles to disk and then loads to memory an entity

lgt_load_entity(Entity) :-
	lgt_compile_entity(Entity),
	(lgt_compiler_option(report, on) ->
		lgt_report_redefined_entity(Entity)
		;
		true),
	lgt_file_name(prolog, Entity, File),
	lgt_load_prolog_code(File),
	(lgt_compiler_option(report, on) ->
		write('<<<  '), writeq(Entity), write(' loaded'), nl
		;
		true).



% lgt_report_redefined_entity(+atom)
%
% prints a warning if an entity of the same name is already loaded
% does not work for parametric objects...

lgt_report_redefined_entity(Entity) :-
	lgt_current_object_(Entity, _, _, _, _),
	!,
	write('WARNING!  redefining '), write(Entity), write(' object'), nl.

lgt_report_redefined_entity(Entity) :-
	lgt_current_protocol_(Entity, _),
	!,
	write('WARNING!  redefining '), write(Entity), write(' protocol'), nl.

lgt_report_redefined_entity(Entity) :-
	lgt_current_category_(Entity, _),
	!,
	write('WARNING!  redefining '), write(Entity), write(' category'), nl.

lgt_report_redefined_entity(_).
	


% lgt_compile_entities(+list)
%
% compiles to disk a list of entities

lgt_compile_entities([]).

lgt_compile_entities([Entity| Entities]) :-
	lgt_compile_entity(Entity),
	lgt_compile_entities(Entities).



% lgt_compile_entity(+atom)
%
% compiles to disk an entity

lgt_compile_entity(Entity) :-
	lgt_compiler_option(smart_compilation, on),
	\+ lgt_needs_recompilation(Entity),
	!,
	(lgt_compiler_option(report, on) ->
		nl, write('>>>  '), writeq(Entity), write(' is up-to-date'), nl
		;
		true).

lgt_compile_entity(Entity) :-
	(lgt_compiler_option(report, on) ->
		nl, write('>>>  compiling '), writeq(Entity), nl	
		;
		true),
	lgt_tr_entity(Entity),
	lgt_write_tr_entity(Entity),
	lgt_write_entity_doc(Entity),
	lgt_report_unknown_entities,
	(lgt_compiler_option(report, on) ->
		write('>>>  '), writeq(Entity), write(' compiled'), nl
		;
		true).



% lgt_needs_recompilation(+atom)
%
% source file needs recompilation

lgt_needs_recompilation(Entity) :-
	lgt_file_name(prolog, Entity, File),
	\+ lgt_file_exists(File).

lgt_needs_recompilation(Entity) :-
	lgt_file_name(xml, Entity, File),
	lgt_compiler_option(xml, on),
	\+ lgt_file_exists(File).

lgt_needs_recompilation(Entity) :-
	lgt_file_name(logtalk, Entity, Source),
	lgt_file_name(prolog, Entity, Object),
	(lgt_compare_file_mtimes(Result, Source, Object) ->
		Result = '>'
		;
		true).



% lgt_write_tr_entity(+atom)
%
% writes to disk the entity compiled code

lgt_write_tr_entity(Entity) :-
	lgt_file_name(prolog, Entity, File),
	catch(
		open(File, write, Stream),
		Error,
		lgt_compiler_error_handler(Stream, Error)),
	catch(
		(lgt_write_directives(Stream),
		 lgt_write_clauses(Stream),
		 lgt_write_init_call(Stream)),
		Error,
		lgt_compiler_error_handler(Stream, Error)),
	close(Stream).



% lgt_write_entity_doc(+atom)
%
% writes to disk the entity documentation in XML format

lgt_write_entity_doc(Entity) :-
	lgt_compiler_option(xml, on) ->
		lgt_file_name(xml, Entity, File),
		catch(
			open(File, write, Stream),
			Error,
			lgt_compiler_error_handler(Stream, Error)),
		catch(
			lgt_write_xml_file(Stream),
			Error,
			lgt_compiler_error_handler(Stream, Error)),
		close(Stream)
		;
		true.



% lgt_file_name(+atom, +atom, -atom)
%
% constructs a filename given the type of file and the entity name

lgt_file_name(Type, Entity, File) :-
	lgt_file_extension(Type, Extension),
	atom_concat(Entity, Extension, File).



% lgt_tr_entity(+atom)
%
% compiles an entity storing the resulting code in memory

lgt_tr_entity(Entity) :-
	lgt_clean_up,
	lgt_file_name(logtalk, Entity, File),
	catch(
		open(File, read, Stream),
		Error,
		lgt_compiler_error_handler(Stream, Error)),
	catch(
		(read_term(Stream, Term, [singletons(Singletons)]),
		 lgt_report_singletons(Singletons, Term),
		 lgt_tr_file(Stream, Term)),
		Error,
		lgt_compiler_error_handler(Stream, Error)),
	close(Stream),
	lgt_fix_redef_built_ins,
	lgt_find_misspelt_calls,
	lgt_entity_(Type, _, _, _),
	lgt_gen_clauses(Type),
	lgt_gen_directives(Type).



% lgt_tr_file(+stream, +term)

lgt_tr_file(_, end_of_file) :-
	!.

lgt_tr_file(Stream, Term) :-
	lgt_tr_term(Term),
	read_term(Stream, Next, [singletons(Singletons)]),
	lgt_report_singletons(Singletons, Next),
	lgt_tr_file(Stream, Next).



% lgt_report_singletons(+list, +term)
%
% report the singleton variables found while compiling an entity term

lgt_report_singletons([], _).

lgt_report_singletons([Singleton| Singletons], Term) :-
	(lgt_compiler_option(singletons, warning),
	 lgt_compiler_option(report, on)) ->
		write('WARNING!'),
		\+ \+ ( lgt_report_singletons_aux([Singleton| Singletons], Term, Names),
				write('  singleton variables: '), write(Names), nl,
				(Term = (:- _) ->
					write('          in directive: ')
					;
					write('          in clause: ')),
				write(Term), nl)
		;
		true.


lgt_report_singletons_aux([], _, []).

lgt_report_singletons_aux([Name = Var| Singletons], Term, [Name| Names]) :-
	Name = Var,
	lgt_report_singletons_aux(Singletons, Term, Names).



% lgt_compiler_error_handler(@term, +term)
%
% close the stream opened for reading the entity 
% file and report the compilation error found

lgt_compiler_error_handler(Stream, Error) :-
	(nonvar(Stream) ->
		close(Stream)
		;
		true),
	lgt_report_compiler_error(Error),
	throw(Error).



% lgt_report_compiler_error(+term)
%
% reports a compilation error

lgt_report_compiler_error(error(Error, directive(Directive))) :-
	!,
	write('ERROR!    '), writeq(Error), nl,
	write('          in directive: '), write((:- Directive)), nl.

lgt_report_compiler_error(error(Error, clause(Clause))) :-
	!,
	write('ERROR!    '), writeq(Error), nl,
	write('          in clause: '), write(Clause), nl.

lgt_report_compiler_error(error(Error, Term)) :-
	!,
	write('ERROR!    '), writeq(Error), nl,
	write('          in: '), write(Term), nl.

lgt_report_compiler_error(Error) :-
	write('ERROR!    '), writeq(Error), nl.



% clean up all dynamic predicates used during entity compilation

lgt_clean_up :-
	retractall(lgt_object_(_, _, _, _, _, _, _, _, _)),
	retractall(lgt_protocol_(_, _, _)),
	retractall(lgt_category_(_, _, _, _)),
	retractall(lgt_implemented_protocol_(_, _, _, _)),
	retractall(lgt_imported_category_(_, _, _, _, _)),
	retractall(lgt_extended_object_(_, _, _, _, _, _, _, _, _, _)),
	retractall(lgt_instantiated_class_(_, _, _, _, _, _, _, _, _, _)),
	retractall(lgt_specialized_class_(_, _, _, _, _, _, _, _, _, _)),
	retractall(lgt_extended_protocol_(_, _, _, _)),
	retractall(lgt_uses_(_)),
	retractall(lgt_calls_(_)),
	retractall(lgt_info_(_)),
	retractall(lgt_info_(_, _)),
	retractall(lgt_directive_(_)),
	retractall(lgt_public_(_)),
	retractall(lgt_protected_(_)),
	retractall(lgt_private_(_)),
	retractall(lgt_dynamic_(_)),
	retractall(lgt_discontiguous_(_)),
	retractall(lgt_mode_(_, _)),
	retractall(lgt_metapredicate_(_)),
	retractall(lgt_entity_functors_(_)),
	retractall(lgt_entity_(_, _, _, _)),
	retractall(lgt_entity_init_(_)),
	retractall(lgt_fentity_init_(_)),
	retractall(lgt_entity_comp_mode_(_)),
	retractall(lgt_dcl_(_)),
	retractall(lgt_ddcl_(_)),
	retractall(lgt_def_(_)),
	retractall(lgt_ddef_(_)),
	retractall(lgt_super_(_)),
	retractall(lgt_rclause_(_)),
	retractall(lgt_eclause_(_)),
	retractall(lgt_feclause_(_)),
	retractall(lgt_redefined_built_in_(_, _, _)),
	retractall(lgt_defs_pred_(_)),
	retractall(lgt_calls_pred_(_)),
	retractall(lgt_referenced_object_(_)),
	retractall(lgt_referenced_protocol_(_)),
	retractall(lgt_referenced_category_(_)).



% dump all dynamic predicates used during entity compilation
% in the current ouput stream (just a debugging utility)
%
% only works on Prolog compilers implementing listing/1

lgt_dump_all :-
	listing(lgt_object_/9),
	listing(lgt_protocol_/3),
	listing(lgt_category_/4),
	listing(lgt_implemented_protocol_/4),
	listing(lgt_imported_category_/5),
	listing(lgt_extended_object_/10),
	listing(lgt_instantiated_class_/10),
	listing(lgt_specialized_class_/10),
	listing(lgt_extended_protocol_/4),
	listing(lgt_uses_/1),
	listing(lgt_calls_/1),
	listing(lgt_info_/1),
	listing(lgt_info_/2),
	listing(lgt_directive_/1),
	listing(lgt_public_/1),
	listing(lgt_protected_/1),
	listing(lgt_private_/1),
	listing(lgt_dynamic_/1),
	listing(lgt_discontiguous_/1),
	listing(lgt_mode_/2),
	listing(lgt_metapredicate_/1),
	listing(lgt_entity_functors_/1),
	listing(lgt_entity_/4),
	listing(lgt_entity_init_/1),
	listing(lgt_fentity_init_/1),
	listing(lgt_entity_comp_mode_/1),
	listing(lgt_dcl_/1),
	listing(lgt_ddcl_/1),
	listing(lgt_def_/1),
	listing(lgt_ddef_/1),
	listing(lgt_super_/1),
	listing(lgt_rclause_/1),
	listing(lgt_eclause_/1),
	listing(lgt_feclause_/1),
	listing(lgt_redefined_built_in_/3),
	listing(lgt_defs_pred_/1),
	listing(lgt_calls_pred_/1),
	listing(lgt_current_compiler_option_/2),
	listing(lgt_flag_/2),
	listing(lgt_referenced_object_/1),
	listing(lgt_referenced_protocol_/1),
	listing(lgt_referenced_category_/1).



% lgt_tr_terms(+list)
%
% translates a list of entity terms (clauses and/or directives)

lgt_tr_terms([]).

lgt_tr_terms([Term| Terms]) :-
	lgt_tr_term(Term),
	lgt_tr_terms(Terms).



% lgt_tr_term(+term)
%
% translates an entity term (either a clause or a directive)

lgt_tr_term((Head:-Body)) :-
	!,
	lgt_tr_clause((Head:-Body)).

lgt_tr_term((:-Dir)) :-
	!,
	lgt_tr_directive(Dir).

lgt_tr_term(Fact) :-
	lgt_tr_clause(Fact).



% lgt_tr_directives(+list)
%
% translates a list of entity directives

lgt_tr_directives([]).

lgt_tr_directives([Dir| Dirs]) :-
	lgt_tr_directive(Dir),
	lgt_tr_directives(Dirs).



% lgt_tr_directive(+term)
%
% translates an entity directive

lgt_tr_directive(Dir) :-
	var(Dir),
	throw(error(instantiantion_error, directive(Dir))).

lgt_tr_directive(Dir) :-
	\+ lgt_entity_(_, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	lgt_lgt_closing_directive(Functor/Arity),	% opening directive missing/missplet
	throw(error(unmatched_directive, directive(Dir))).

lgt_tr_directive(Dir) :-
	\+ lgt_entity_(_, _, _, _),		% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	\+ lgt_lgt_opening_directive(Functor/Arity),
	!,
	assertz(lgt_directive_(Dir)).

lgt_tr_directive(Dir) :-
	functor(Dir, Functor, Arity),
	lgt_lgt_directive(Functor/Arity),
	Dir =.. [Functor| Args],
	catch(
		lgt_tr_directive(Functor, Args),
		Error,
		throw(error(Error, directive(Dir)))),
	!.

lgt_tr_directive(Dir) :-
	throw(error(domain_error(directive, Dir), directive(Dir))).



% lgt_tr_directive(+atom, +list)
%
% translates a directive and its (possibly empty) list of arguments

lgt_tr_directive(object, [Obj| Rels]) :-
	lgt_valid_object_id(Obj) ->
		lgt_tr_object_id(Obj),
		lgt_tr_object_relations(Rels, Obj)
		;
		throw(type_error(object_identifier, Obj)).

lgt_tr_directive(end_object, []) :-
	lgt_entity_(object, _, _, _).


lgt_tr_directive(protocol, [Ptc| Rels]) :-
	lgt_valid_protocol_id(Ptc) ->
		lgt_tr_protocol_id(Ptc),
		lgt_tr_protocol_relations(Rels, Ptc)
		;
		throw(type_error(protocol_identifier, Ptc)).


lgt_tr_directive(end_protocol, []) :-
	lgt_entity_(protocol, _, _, _).


lgt_tr_directive(category, [Ctg| Rels]) :-
	lgt_valid_category_id(Ctg) ->
		lgt_tr_category_id(Ctg),
		lgt_tr_category_relations(Rels, Ctg)
		;
		throw(type_error(category_identifier, Ctg)).


lgt_tr_directive(end_category, []) :-
	lgt_entity_(category, _, _, _).


% dynamic entity directive

lgt_tr_directive((dynamic), []) :-
	assertz(lgt_entity_comp_mode_((dynamic))).


lgt_tr_directive(initialization, [Goal]) :-
	lgt_callable(Goal) ->
		lgt_entity_(_, Entity, Prefix, _),
		lgt_prefix(Context, Prefix),
		lgt_metavars(Context, []),
		lgt_sender(Context, Entity),
		lgt_this(Context, Entity),
		lgt_self(Context, Entity),
		lgt_tr_body(Goal, TGoal, Context),
		assertz(lgt_entity_init_(TGoal))
		;
		throw(type_error(callable, Goal)).


lgt_tr_directive(op, [Priority, Specifier, Operators]) :-
	lgt_valid_op_priority(Priority) ->
		(lgt_valid_op_specifier(Specifier) ->
			(lgt_valid_op_names(Operators) ->
				op(Priority, Specifier, Operators),
				assertz(lgt_directive_(op(Priority, Specifier, Operators)))
				;
				throw(type_error(operator_name, Operators)))
			;
			throw(type_error(operator_specifier, Specifier)))
		;
		throw(type_error(operator_priority, Priority)).


lgt_tr_directive(uses, Objs) :-
	lgt_convert_to_list(Objs, Objs2),
	forall(
		lgt_member(Obj, Objs2),
		(lgt_valid_object_id(Obj) ->
			lgt_add_referenced_object(Obj),
			assertz(lgt_uses_(Obj))
			;
			throw(type_error(object_identifier, Obj)))).


lgt_tr_directive(calls, Ptcs) :-
	lgt_convert_to_list(Ptcs, Ptcs2),
	forall(
		lgt_member(Ptc, Ptcs2),
		(lgt_valid_protocol_id(Ptc) ->
			lgt_add_referenced_protocol(Ptc),
			assertz(lgt_calls_(Ptc))
			;
			throw(type_error(protocol_identifier, Ptc)))).


lgt_tr_directive(info, [List]) :-
	!,
	(lgt_valid_info_list(List) ->
		assertz(lgt_info_(List))
		;
		throw(type_error(info_list, List))).


lgt_tr_directive(info, [Pred, List]) :-
	lgt_valid_pred_ind(Pred) ->
		(lgt_valid_info_list(List) ->
			assertz(lgt_info_(Pred, List))
			;
			throw(type_error(info_list, List)))
		;
		throw(type_error(predicate_indicator, Pred)).



lgt_tr_directive((public), Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_pred_ind(Pred) ->
			assertz(lgt_public_(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


lgt_tr_directive(protected, Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_pred_ind(Pred) ->
			assertz(lgt_protected_(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


lgt_tr_directive(private, Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_pred_ind(Pred) ->
			assertz(lgt_private_(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


lgt_tr_directive((dynamic), Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_pred_ind(Pred) ->
			assertz(lgt_dynamic_(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


lgt_tr_directive((discontiguous), Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_pred_ind(Pred) ->
			assertz(lgt_discontiguous_(Pred))
			;
			throw(type_error(predicate_indicator, Pred)))).


lgt_tr_directive(metapredicate, Preds) :-
	lgt_convert_to_list(Preds, Preds2),
	forall(
		lgt_member(Pred, Preds2),
		(lgt_valid_metapred_term(Pred) ->
			assertz(lgt_metapredicate_(Pred))
			;
			throw(type_error(metapredicate_term, Pred)))).


lgt_tr_directive((mode), [Mode, Solutions]) :-
	lgt_valid_mode_term(Mode) ->
		(lgt_valid_number_of_solutions(Solutions) ->
			assertz(lgt_mode_(Mode, Solutions))
			;
			throw(type_error(number_of_solutions, Solutions)))
		;
		throw(type_error(mode_term, Mode)).



% lgt_tr_object_relations(+list, +term)
%
% translates the relations of an object with other entities

lgt_tr_object_relations([], _).

lgt_tr_object_relations([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Arguments],
	(lgt_tr_object_relation(Functor, Arguments, Obj) ->
		lgt_tr_object_relations(Clauses, Obj)
		;
		throw(type_error(relation_clause, Functor))).



% lgt_tr_object_relation(+atom, +list, +term)
%
% translates a relation between an object (the last argument) with other entities

lgt_tr_object_relation(implements, Ptcs, Obj) :-
	lgt_convert_to_list(Ptcs, List),
	lgt_tr_implements_protocol(List, Obj).

lgt_tr_object_relation(imports, Ctgs, Obj) :-
	lgt_convert_to_list(Ctgs, List),
	lgt_tr_imports_category(List, Obj).

lgt_tr_object_relation(instantiates, Classes, Obj) :-
	lgt_convert_to_list(Classes, List),
	lgt_tr_instantiates_class(List, Obj).

lgt_tr_object_relation(specializes, Superclasses, Class) :-
	lgt_convert_to_list(Superclasses, List),
	lgt_tr_specializes_class(List, Class).

lgt_tr_object_relation(extends, Parents, Prototype) :-
	lgt_convert_to_list(Parents, List),
	lgt_tr_extends_object(List, Prototype).



% lgt_tr_protocol_relations(+list, +term)
%
% translates the relations of a protocol with other entities

lgt_tr_protocol_relations([], _).

lgt_tr_protocol_relations([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Arguments],
	lgt_tr_protocol_relation(Functor, Arguments, Obj),
	lgt_tr_protocol_relations(Clauses, Obj).



% lgt_tr_protocol_relation(+atom, +list, +term)
%
% translates a relation between a protocol (the last argument) with other entities

lgt_tr_protocol_relation(extends, Ptcs, Ptc) :-
	!,
	lgt_convert_to_list(Ptcs, List),
	lgt_tr_extends_protocol(List, Ptc).

lgt_tr_protocol_relation(Unknown, _, _) :-
	throw(directive_error(relation_clause, Unknown)).



% lgt_tr_category_relations(+list, +term)
%
% translates the relations of a category with other entities

lgt_tr_category_relations([], _).

lgt_tr_category_relations([Clause| Clauses], Obj) :-
	Clause =.. [Functor| Arguments],
	lgt_tr_category_relation(Functor, Arguments, Obj),
	lgt_tr_category_relations(Clauses, Obj).



% lgt_tr_category_relation(+atom, +list, +term)
%
% translates a relation between a category (the last argument) with other entities

lgt_tr_category_relation(implements, Ptcs, Ctg) :-
	!,
	lgt_convert_to_list(Ptcs, List),
	lgt_tr_implements_protocol(List, Ctg).

lgt_tr_category_relation(Unknown, _, _) :-
	throw(directive_error(relation_clause, Unknown)).



% lgt_tr_clauses(+list)

lgt_tr_clauses([]).

lgt_tr_clauses([Clause| Clauses]) :-
	lgt_tr_clause(Clause),
	lgt_tr_clauses(Clauses).



% lgt_tr_clause(+clause)

lgt_tr_clause(Clause) :-
	\+ lgt_entity_(_, _, _, _),		% clause occurs before opening entity directive
	!,
	assertz(lgt_feclause_(Clause)).

lgt_tr_clause(Clause) :-
	lgt_entity_(_, _, Prefix, _),
	lgt_prefix(Context, Prefix),
	catch(
		lgt_tr_clause(Clause, TClause, Context),
		Error,
		throw(error(Error, clause(Clause)))),
	assertz(lgt_eclause_(TClause)),
	!.

lgt_tr_clause(Clause) :-
	throw(error(unknown_error, clause(Clause))).



% lgt_tr_clause(+clause, +clause, +term)

lgt_tr_clause((Head:-_), _, _) :-
	\+ lgt_callable(Head),
	throw(type_error(callable, Head)).

lgt_tr_clause((Head:-Body), (THead:-TBody), Context) :-
	!,
	lgt_extract_metavars(Head, Metavars),
	lgt_metavars(Context, Metavars),
	lgt_tr_head(Head, THead, Context),
	lgt_tr_body(Body, Body2, Context),
	lgt_simplify_body(Body2, TBody).

lgt_tr_clause(Fact, _, _) :-
	\+ lgt_callable(Fact),
	throw(type_error(callable, Fact)).

lgt_tr_clause(Fact, TFact, Context) :-
	lgt_tr_head(Fact, TFact, Context).



% lgt_tr_head(+callable, -callable, +term)
%
% translates an entity clause head


% definition of dynamic predicates inside categories

lgt_tr_head(Head, _, _) :-
	lgt_entity_(category, _, _, _),
	functor(Head, Functor, Arity), 
	lgt_dynamic_(Functor/Arity),
	throw(permission_error(define, dynamic_predicate, Functor/Arity)).


% redefinition of built-in methods

lgt_tr_head(Head, _, _) :-
	lgt_built_in_method(Head, _),
	functor(Head, Functor, Arity), 
	throw(permission_error(modify, built_in_method, Functor/Arity)).


% redefinition of Logtalk built-in predicates

lgt_tr_head(Head, _, _) :-
	lgt_lgt_built_in(Head),
	lgt_compiler_option(lgtredef, warning),
	lgt_compiler_option(report, on),
	\+ lgt_redefined_built_in_(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	write('WARNING!  redefining a Logtalk built-in predicate: '),
	writeq(Functor/Arity), nl,
	fail.


% redefinition of Prolog built-in predicates

lgt_tr_head(Head, _, _) :-
	lgt_pl_built_in(Head),
	lgt_compiler_option(plredef, warning),
	lgt_compiler_option(report, on),
	\+ lgt_redefined_built_in_(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	write('WARNING!  redefining a Prolog built-in predicate: '),
	writeq(Functor/Arity), nl,
	fail.


% translate the head of a clause of a user defined predicate

lgt_tr_head(Head, THead, Context) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	lgt_prefix(Context, EPrefix),
	lgt_construct_predicate_functor(EPrefix, Functor, Arity, PPrefix),
	((lgt_dynamic_(Functor/Arity),
	  \+ lgt_public_(Functor/Arity),
	  \+ lgt_protected_(Functor/Arity),
	  \+ lgt_private_(Functor/Arity)) ->
		lgt_add_ddef_clause(Functor, Arity, PPrefix, Context)
		;
		lgt_add_def_clause(Functor, Arity, PPrefix, Context)),
	lgt_sender(Context, Sender),
	lgt_this(Context, This),
	lgt_self(Context, Self),
	lgt_append(Args, [Sender, This, Self], Args2),
	THead =.. [PPrefix| Args2].



% lgt_tr_body(+callable, -callable, +term)
%
% translates an entity clause body


% meta-calls

lgt_tr_body(Pred, TPred, Context) :-
	var(Pred),
	!,
	lgt_metavars(Context, Metavars),
	(lgt_member_var(Pred, Metavars) ->
		lgt_sender(Context, Sender),
		TPred = lgt_metacall_in_object(Sender, Pred, Sender)
		;
		lgt_this(Context, This),
		TPred = lgt_metacall_in_object(This, Pred, This)).


% pre-processor bypass (call of external code)

lgt_tr_body({Pred}, Pred, _) :-
	!.


% bagof/3 and setof/3 existential quantifiers

lgt_tr_body(Var^Pred, Var^TPred, Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).


% control constructs

lgt_tr_body((Pred1, Pred2), (TPred1, TPred2), Context) :-
	!,
	lgt_tr_body(Pred1, TPred1, Context),
	lgt_tr_body(Pred2, TPred2, Context).

lgt_tr_body((Pred1; Pred2), (TPred1; TPred2), Context) :-
	!,
	lgt_tr_body(Pred1, TPred1, Context),
	lgt_tr_body(Pred2, TPred2, Context).

lgt_tr_body((Pred1 -> Pred2), (TPred1 -> TPred2), Context) :-
	!,
	lgt_tr_body(Pred1, TPred1, Context),
	lgt_tr_body(Pred2, TPred2, Context).

lgt_tr_body(\+ Pred, \+ TPred, Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).

lgt_tr_body(!, !, _) :-
	!.

lgt_tr_body(true, true, _) :-
	!.

lgt_tr_body(fail, fail, _) :-
	!.

lgt_tr_body(repeat, repeat, _) :-
	!.

lgt_tr_body(call(Pred), TPred, Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).

lgt_tr_body(once(Pred), once(TPred), Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).

lgt_tr_body(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Context) :-
	!,
	lgt_tr_body(Goal, TGoal, Context),
	lgt_tr_body(Recovery, TRecovery, Context).

lgt_tr_body(throw(Error), throw(Error), _) :-
	!.


% built-in metapredicates

lgt_tr_body(bagof(Term, Pred, List), bagof(Term, TPred, List), Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).

lgt_tr_body(findall(Term, Pred, List), findall(Term, TPred, List), Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).

lgt_tr_body(forall(Generate, Test), forall(TGenerate, TTest), Context) :-
	!,
	lgt_tr_body(Generate, TGenerate, Context),
	lgt_tr_body(Test, TTest, Context).

lgt_tr_body(setof(Term, Pred, List), setof(Term, TPred, List), Context) :-
	!,
	lgt_tr_body(Pred, TPred, Context).


% message sending

lgt_tr_body(Obj::Pred, TPred, Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_body(::Pred, TPred, Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_body(^^Pred, TPred, Context) :-
	!,
	lgt_tr_super_sending(Pred, TPred, Context).


% "reflection" built-in predicates

lgt_tr_body(current_predicate(Pred), lgt_current_predicate(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(predicate_property(Pred, Property), lgt_predicate_property(This, Pred, Property, This, _), Context) :-
	!,
	lgt_this(Context, This).


% database handling built-in predicates

lgt_tr_body(abolish(Pred), lgt_abolish(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(asserta(Pred), lgt_asserta(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(assertz(Pred), lgt_assertz(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(clause(Head, Body), lgt_clause(This, Head, Body, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(retract(Pred), lgt_retract(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_body(retractall(Pred), lgt_retractall(This, Pred, This, _), Context) :-
	!,
	lgt_this(Context, This).


% inline methods (translated to a single unification with the corresponding context argument)

lgt_tr_body(sender(Sender), true, Context) :-
	lgt_sender(Context, Sender),
	!.

lgt_tr_body(this(This), true, Context) :-
	lgt_this(Context, This),
	!.

lgt_tr_body(self(Self), true, Context) :-
	lgt_self(Context, Self),
	!.


% pre-defined methods

lgt_tr_body(parameter(Arg, Value), arg(Arg, This, Value), Context) :-
	lgt_this(Context, This),
	!.


% Logtalk and Prolog built-in predicates

lgt_tr_body(Pred, _, _) :-
	lgt_built_in(Pred),
	\+ lgt_iso_def_pred(Pred),
	lgt_compiler_option(portability, warning),
	lgt_compiler_option(report, on),
	functor(Pred, Functor, Arity),
	write('WARNING!  non-ISO defined built-in predicate call: '),
	writeq(Functor/Arity), nl,
	fail.

lgt_tr_body(Pred, lgt_call_built_in(Pred, Context), Context) :-
	lgt_built_in(Pred),
	!.


% invalid goal

lgt_tr_body(Pred, _, _) :-
	\+ lgt_callable(Pred),
	throw(type_error(callable, Pred)).


% goal is a call to a user predicate

lgt_tr_body(Condition, TCondition, Context) :-
	Condition =.. [Functor|Args],
	functor(Condition, Functor, Arity),
	lgt_prefix(Context, EPrefix),
	lgt_construct_predicate_functor(EPrefix, Functor, Arity, PPrefix),
	lgt_sender(Context, Sender),
	lgt_this(Context, This),
	lgt_self(Context, Self),
	lgt_append(Args, [Sender, This, Self], Args2),
	TCondition =.. [PPrefix|Args2],
	assertz(lgt_calls_pred_(Functor/Arity)).



% lgt_tr_msg(@object, @term, -term, +term)
%
% translates the sending of a message to an object


% message broadcasting

lgt_tr_msg(Obj, Pred, TPred, Context) :-
	nonvar(Obj),
	(Obj = (_, _); Obj = (_; _)),
	!,
	lgt_tr_msg_broadcasting(Obj, Pred, TPred, Context).


% invalid object identifier

lgt_tr_msg(Obj, _, _, _) :-
	nonvar(Obj),
	\+ lgt_valid_object_id(Obj),
	!,
	throw(type_error(object_identifier, Obj)).


% remember the object receiving the message to later check if it's known

lgt_tr_msg(Obj, _, _, _) :-
	nonvar(Obj),
	lgt_add_referenced_object(Obj),
	fail.


% non-instantiated message: traslation performed at runtime

lgt_tr_msg(Obj, Pred, lgt_send_to_object(Obj, Pred, This), Context) :-
	var(Pred),
	!,
	lgt_this(Context, This).


% control constructs

lgt_tr_msg(Obj, (Pred1, Pred2), (TPred1, TPred2), Context) :-
	!,
	lgt_tr_msg(Obj, Pred1, TPred1, Context),
	lgt_tr_msg(Obj, Pred2, TPred2, Context).

lgt_tr_msg(Obj, (Pred1; Pred2), (TPred1; TPred2), Context) :-
	!,
	lgt_tr_msg(Obj, Pred1, TPred1, Context),
	lgt_tr_msg(Obj, Pred2, TPred2, Context).

lgt_tr_msg(Obj, (Pred1 -> Pred2), (TPred1 -> TPred2), Context) :-
	!,
	lgt_tr_msg(Obj, Pred1, TPred1, Context),
	lgt_tr_msg(Obj, Pred2, TPred2, Context).

lgt_tr_msg(Obj, \+ Pred, \+ TPred, Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_msg(_, !, !, _) :-
	!.

lgt_tr_msg(_, true, true, _) :-
	!.

lgt_tr_msg(_, fail, fail, _) :-
	!.

lgt_tr_msg(_, repeat, repeat, _) :-
	!.

lgt_tr_msg(Obj, call(Pred), TPred, Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_msg(Obj, once(Pred), once(TPred), Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_msg(Obj, catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Context) :-
	!,
	lgt_tr_msg(Obj, Goal, TGoal, Context),
	lgt_tr_msg(Obj, Recovery, TRecovery, Context).

lgt_tr_msg(_, throw(Error), throw(Error), _) :-
	!.


% built-in metapredicates

lgt_tr_msg(Obj, bagof(Term, Pred, List), bagof(Term, TPred, List), Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_msg(Obj, findall(Term, Pred, List), findall(Term, TPred, List), Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).

lgt_tr_msg(Obj, forall(Generate, Test), forall(TGenerate, TTest), Context) :-
	!,
	lgt_tr_msg(Obj, Generate, TGenerate, Context),
	lgt_tr_msg(Obj, Test, TTest, Context).

lgt_tr_msg(Obj, setof(Term, Pred, List), setof(Term, TPred, List), Context) :-
	!,
	lgt_tr_msg(Obj, Pred, TPred, Context).


% "reflection" built-in predicates

lgt_tr_msg(Obj, current_predicate(Pred), lgt_current_predicate(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, predicate_property(Pred, Property), lgt_predicate_property(Obj, Pred, Property, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).


% database handling built-in predicates

lgt_tr_msg(Obj, abolish(Pred), lgt_abolish(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, asserta(Pred), lgt_asserta(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, assertz(Pred), lgt_assertz(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, clause(Head, Body), lgt_clause(Obj, Head, Body, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, retract(Pred), lgt_retract(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, retractall(Pred), lgt_retractall(Obj, Pred, This, p(p(p))), Context) :-
	!,
	lgt_this(Context, This).


% invalid goal

lgt_tr_msg(_, Pred, _, _) :-
	\+ lgt_callable(Pred),
	throw(type_error(callable, Pred)).


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

lgt_tr_msg(Obj, Pred, lgt_send_to_object(Obj, Pred, This), Context) :-
	var(Obj),
	!,
	lgt_this(Context, This).

lgt_tr_msg(Obj, Pred, lgt_send_to_object_nv(Obj, Pred, This), Context) :-
	!,
	lgt_this(Context, This).



% lgt_tr_self_msg(@term, -term, +term)
%
% translates the sending of a message to self


% non-instantiated message: traslation performed at runtime

lgt_tr_self_msg(Pred, lgt_send_to_self(Self, Pred, This), Context) :-
	var(Pred),
	!,
	lgt_this(Context, This),
	lgt_self(Context, Self).


% control constructs

lgt_tr_self_msg((Pred1, Pred2), (TPred1, TPred2), Context) :-
	!,
	lgt_tr_self_msg(Pred1, TPred1, Context),
	lgt_tr_self_msg(Pred2, TPred2, Context).

lgt_tr_self_msg(((Pred1; Pred2)), (TPred1; TPred2), Context) :-
	!,
	lgt_tr_self_msg(Pred1, TPred1, Context),
	lgt_tr_self_msg(Pred2, TPred2, Context).

lgt_tr_self_msg((Pred1 -> Pred2), (TPred1 -> TPred2), Context) :-
	!,
	lgt_tr_self_msg(Pred1, TPred1, Context),
	lgt_tr_self_msg(Pred2, TPred2, Context).

lgt_tr_self_msg(\+ Pred, \+ TPred, Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_self_msg(!, !, _) :-
	!.

lgt_tr_self_msg(true, true, _) :-
	!.

lgt_tr_self_msg(fail, fail, _) :-
	!.

lgt_tr_self_msg(repeat, repeat, _) :-
	!.

lgt_tr_self_msg(call(Pred), TPred, Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_self_msg(once(Pred), once(TPred), Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_self_msg(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Context) :-
	!,
	lgt_tr_self_msg(Goal, TGoal, Context),
	lgt_tr_self_msg(Recovery, TRecovery, Context).

lgt_tr_self_msg(throw(Error), throw(Error), _) :-
	!.


% built-in metapredicates

lgt_tr_self_msg(bagof(Term, Pred, List), bagof(Term, TPred, List), Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_self_msg(findall(Term, Pred, List), findall(Term, TPred, List), Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).

lgt_tr_self_msg(forall(Generate, Test), forall(TGenerate, TTest), Context) :-
	!,
	lgt_tr_self_msg(Generate, TGenerate, Context),
	lgt_tr_self_msg(Test, TTest, Context).

lgt_tr_self_msg(setof(Term, Pred, List), setof(Term, TPred, List), Context) :-
	!,
	lgt_tr_self_msg(Pred, TPred, Context).


% "reflection" built-in predicates

lgt_tr_self_msg(current_predicate(Pred), lgt_current_predicate(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(predicate_property(Pred, Property), lgt_predicate_property(Self, Pred, Property, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).


% database handling built-in predicates

lgt_tr_self_msg(abolish(Pred), lgt_abolish(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(asserta(Pred), lgt_asserta(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(assertz(Pred), lgt_assertz(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(clause(Head, Body), lgt_clause(Self, Head, Body, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(retract(Pred), lgt_retract(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).

lgt_tr_self_msg(retractall(Pred), lgt_retractall(Self, Pred, This, p(_)), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).


% invalid goal

lgt_tr_self_msg(Pred, _, _) :-
	\+ lgt_callable(Pred),
	throw(type_error(callable, Pred)).


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

lgt_tr_self_msg(Pred, lgt_send_to_self_nv(Self, Pred, This), Context) :-
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This).



% message broadcasting

lgt_tr_msg_broadcasting((Obj1, Obj2), Pred, (TP1, TP2), Context) :-
	!,
	lgt_tr_msg(Obj1, Pred, TP1, Context),
	lgt_tr_msg(Obj2, Pred, TP2, Context).

lgt_tr_msg_broadcasting((Obj1; Obj2), Pred, (TP1; TP2), Context) :-
	!,
	lgt_tr_msg(Obj1, Pred, TP1, Context),
	lgt_tr_msg(Obj2, Pred, TP2, Context).



% lgt_tr_super_sending(@term, -term, +term)
%
% translates calling of redefined predicates (super calls)


% invalid goal

lgt_tr_super_sending(Pred, _, _) :-
	nonvar(Pred),
	\+ lgt_callable(Pred),
	throw(type_error(callable, Pred)).


% translation performed at runtime

lgt_tr_super_sending(Pred, lgt_send_to_super(Self, Pred, This, Sender), Context) :-
	var(Pred),
	!,
	lgt_self(Context, Self),
	lgt_this(Context, This),
	lgt_sender(Context, Sender).

lgt_tr_super_sending(Pred, lgt_send_to_super_nv(Self, Pred, This, Sender), Context) :-
	lgt_self(Context, Self),
	lgt_this(Context, This),
	lgt_sender(Context, Sender).



% lgt_extract_metavars(+callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

lgt_extract_metavars(Pred, Metavars) :-
	functor(Pred, Functor, Arity),
	functor(Meta, Functor, Arity),
	(lgt_metapredicate_(Meta) ->
		Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		lgt_extract_metavars(Args, MArgs, Metavars)
		;
		Metavars = []).


% lgt_extract_metavars(+list, +list, -list)

lgt_extract_metavars([], [], []).

lgt_extract_metavars([Var| Args], [MArg| MArgs], [Var| Metavars]) :-
	var(Var),
	MArg = (::),
	!,
	lgt_extract_metavars(Args, MArgs, Metavars).

lgt_extract_metavars([_| Args], [_| MArgs], Metavars) :-
	lgt_extract_metavars(Args, MArgs, Metavars).



% lgt_simplify_body(+callable, -callable)
%
% remove redundant calls to true/0 from a translated clause body

lgt_simplify_body((A;B), (SA;SB)) :-
	!,
	lgt_simplify_body(A, SA),
	lgt_simplify_body(B, SB).

lgt_simplify_body((A->B), (SA->SB)) :-
	!,
	lgt_simplify_body(A, SA),
	lgt_simplify_body(B, SB).

lgt_simplify_body((true, B), SB) :-
	!,
	lgt_simplify_body(B, SB).

lgt_simplify_body((B, true), SB) :-
	!,
	lgt_simplify_body(B, SB).

lgt_simplify_body((A, B), (SA, SB)) :-
	!,
	lgt_simplify_body(A, SA),
	lgt_simplify_body(B, SB).

lgt_simplify_body(B, B).



% lgt_tr_object_id(+object_identifier)
%
% from the object identifier construct the set of 
% functor prefixes used in the compiled code clauses

lgt_tr_object_id(Obj) :-
	lgt_add_referenced_object(Obj),
	lgt_construct_object_functors(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
	assertz(lgt_object_(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef)),
	Term =.. [Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef],
	assertz(lgt_entity_functors_(Term)),
	assertz(lgt_rclause_(lgt_current_object_(Obj, Prefix, Dcl, Def, Super))),
	assertz(lgt_entity_(object, Obj, Prefix, Dcl)).



% lgt_tr_category_id(+category_identifier)
%
% from the category identifier construct the set of 
%  functor prefixes used in the compiled code clauses

lgt_tr_category_id(Ctg) :-
	lgt_add_referenced_category(Ctg),
	lgt_construct_category_functors(Ctg, Prefix, Dcl, Def),
	assertz(lgt_category_(Ctg, Prefix, Dcl, Def)),
	Term =.. [Prefix, Dcl, Def],
	assertz(lgt_entity_functors_(Term)),
	assertz(lgt_rclause_(lgt_current_category_(Ctg, Prefix))),
	assertz(lgt_entity_(category, Ctg, Prefix, Dcl)).



% lgt_tr_protocol_id(+protocol_identifier)
%
% from the protocol identifier construct the set of  
% functor prefixes used in the compiled code clauses

lgt_tr_protocol_id(Ptc) :-
	lgt_add_referenced_protocol(Ptc),
	lgt_construct_protocol_functors(Ptc, Prefix, Dcl),
	assertz(lgt_protocol_(Ptc, Prefix, Dcl)),
	Term =.. [Prefix, Dcl],
	assertz(lgt_entity_functors_(Term)),
	assertz(lgt_rclause_(lgt_current_protocol_(Ptc, Prefix))),
	assertz(lgt_entity_(protocol, Ptc, Prefix, Dcl)).



% lgt_tr_implements_protocol(+list, +object_identifier)
% lgt_tr_implements_protocol(+list, +category_identifier)
%
% translates an "implementents" relation between 
%  a category or an object and a list of protocols

lgt_tr_implements_protocol([], _).

lgt_tr_implements_protocol([Ref| Refs], ObjOrCtg) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Ptc),
		 (lgt_valid_protocol_id(Ptc) ->
		 	lgt_add_referenced_protocol(Ptc),
			assertz(lgt_rclause_(lgt_implements_protocol_(ObjOrCtg, Ptc, Scope))),
			lgt_construct_protocol_functors(Ptc, Prefix, Dcl),
			assertz(lgt_implemented_protocol_(Ptc, Prefix, Dcl, Scope)),
			lgt_tr_implements_protocol(Refs, ObjOrCtg)
			;
			throw(type_error(protocol_identifier, Ptc))))
		;
		throw(type_error(scope, Ref)).



% lgt_tr_imports_category(+list, +object_identifier)
%
% translates an "imports" relation between 
% an object and a list of categories 

lgt_tr_imports_category([], _).

lgt_tr_imports_category([Ref| Refs], Obj) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Ctg),
		 (lgt_valid_category_id(Ctg) ->
		 	lgt_add_referenced_category(Ctg),
			assertz(lgt_rclause_(lgt_imports_category_(Obj, Ctg, Scope))),
			lgt_construct_category_functors(Ctg, Prefix, Dcl, Def),
			assertz(lgt_imported_category_(Ctg, Prefix, Dcl, Def, Scope)),
			lgt_tr_imports_category(Refs, Obj)
			;
			throw(type_error(category_identifier, Ctg))))
		;
		throw(type_error(scope, Ref)).



% lgt_tr_instantiates_class(+list, +object_identifier)
%
% translates an "instantiates" relation between 
% an instance and a list of classes

lgt_tr_instantiates_class([], _).

lgt_tr_instantiates_class([Ref| Refs], Obj) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Class),
		 (lgt_valid_object_id(Class) ->
		 	lgt_add_referenced_object(Class),
			assertz(lgt_rclause_(lgt_instantiates_class_(Obj, Class, Scope))),
			lgt_construct_object_functors(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz(lgt_instantiated_class_(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			lgt_tr_instantiates_class(Refs, Obj)
			;
			throw(type_error(object_identifier, Class))))
		;
		throw(type_error(scope, Ref)).



% lgt_tr_specializes_class(+list, +object_identifier)
%
% translates a "specializes" relation between 
% a class and a list of superclasses

lgt_tr_specializes_class([], _).

lgt_tr_specializes_class([Ref| Refs], Class) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Superclass),
		 (lgt_valid_object_id(Superclass) ->
		 	lgt_add_referenced_object(Superclass),
			assertz(lgt_rclause_(lgt_specializes_class_(Class, Superclass, Scope))),
			lgt_construct_object_functors(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz(lgt_specialized_class_(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			lgt_tr_specializes_class(Refs, Class)
			;
			throw(type_error(object_identifier, Superclass))))
		;
		throw(type_error(scope, Ref)).



% lgt_tr_extends_object(+list, +object_identifier)
%
% translates an "extends" relation between 
% a prototype and a list of parents

lgt_tr_extends_object([], _).

lgt_tr_extends_object([Ref| Refs], Obj) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Parent),
		 (lgt_valid_object_id(Parent) ->
		 	lgt_add_referenced_object(Parent),
			assertz(lgt_rclause_(lgt_extends_object_(Obj, Parent, Scope))),
			lgt_construct_object_functors(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
			assertz(lgt_extended_object_(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
			lgt_tr_extends_object(Refs, Obj)
			;
			throw(type_error(object_identifier, Parent))))
		;
		throw(type_error(scope, Ref)).



% lgt_tr_extends_protocol(+list, +protocol_identifier)
%
% translates an "extends" relation between 
% a protocol and a list of protocols

lgt_tr_extends_protocol([], _).

lgt_tr_extends_protocol([Ref| Refs], Ptc1) :-
	lgt_valid_scope(Ref) ->
		(lgt_scope_id(Ref, Scope, Ptc2),
		 (lgt_valid_protocol_id(Ptc2) ->
		 	lgt_add_referenced_protocol(Ptc2),
			assertz(lgt_rclause_(lgt_extends_protocol_(Ptc1, Ptc2, Scope))),
			lgt_construct_protocol_functors(Ptc2, Prefix, Dcl),
			assertz(lgt_extended_protocol_(Ptc2, Prefix, Dcl, Scope)),
			lgt_tr_extends_protocol(Refs, Ptc1)
			;
			throw(type_error(protocol_identifier, Ptc2))))
		;
		throw(type_error(scope, Ref)).



% lgt_add_referenced_object(+object_identifier)
%
% assert the name of an object referenced by the entity that we are compiling

lgt_add_referenced_object(Obj) :-
	lgt_referenced_object_(Obj) ->
		true
		;
		assertz(lgt_referenced_object_(Obj)).



% lgt_add_referenced_protocol(+protocol_identifier)
%
% assert the name of a protocol referenced by the entity that we are compiling

lgt_add_referenced_protocol(Ptc) :-
	lgt_referenced_protocol_(Ptc) ->
		true
		;
		assertz(lgt_referenced_protocol_(Ptc)).



% lgt_add_referenced_category(+category_identifier)
%
% assert the name of a category referenced by the entity that we are compiling

lgt_add_referenced_category(Ctg) :-
	lgt_referenced_category_(Ctg) ->
		true
		;
		assertz(lgt_referenced_category_(Ctg)).



% lgt_report_unknown_entities
%
% report any unknown referenced entities found while compiling an entity
% (if the corresponding compiler option is not set to "silent")

lgt_report_unknown_entities :-
	(lgt_compiler_option(unknown, warning),
	 lgt_compiler_option(report, on)) ->
		lgt_report_unknown_objects,
		lgt_report_unknown_protocols,
		lgt_report_unknown_categories
		;
		true.	



% lgt_report_unknown_objects
%
% report any unknown referenced objects found while compiling an entity

lgt_report_unknown_objects :-
	findall(
		Obj,
		(lgt_referenced_object_(Obj), \+ (lgt_current_object_(Obj, _, _, _, _); lgt_entity_(_, Obj, _, _))),
		Objs),
	(Objs \= [] ->
		write('WARNING!  references to unknown objects:    '), writeq(Objs), nl
		;
		true).



% lgt_report_unknown_protocols
%
% report any unknown referenced protocols found while compiling an entity

lgt_report_unknown_protocols :-
	findall(
		Ptc,
		(lgt_referenced_protocol_(Ptc), \+ (lgt_current_protocol_(Ptc, _); lgt_entity_(_, Ptc, _, _))),
		Ptcs),
	(Ptcs \= [] ->
		write('WARNING!  references to unknown protocols:  '), writeq(Ptcs), nl
		;
		true).



% lgt_report_unknown_categories
%
% report any unknown referenced categories found while compiling an entity

lgt_report_unknown_categories :-
	findall(
		Ctg,
		(lgt_referenced_category_(Ctg), \+ (lgt_current_category_(Ctg, _); lgt_entity_(_, Ctg, _, _))),
		Ctgs),
	(Ctgs \= [] ->
		write('WARNING!  references to unknown categories: '), writeq(Ctgs), nl
		;
		true).



% lgt_add_def_clause(+atom, +integer, +atom, +term)
%
% adds a "def clause" (used to translate a predicate call)

lgt_add_def_clause(Functor, Arity, Prefix, Context) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	lgt_sender(Context, Sender),
	lgt_this(Context, This),
	lgt_self(Context, Self),
	lgt_append(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	once(
		(lgt_object_(_, _, _, Def, _, _, _, _, _);
		 lgt_category_(_, _, _, Def))),
	Clause =.. [Def, Head, Sender, This, Self, THead],
	(lgt_def_(Clause) ->
		true
		;
		assertz(lgt_def_(Clause))),
	(lgt_built_in(Head) ->
		assertz(lgt_redefined_built_in_(Head, Context, THead))
		;
		true),
	(lgt_defs_pred_(Functor/Arity) ->
		true
		;
		assertz(lgt_defs_pred_(Functor/Arity))).



% lgt_add_ddef_clause(+atom, +integer, +atom, +term)
%
% adds a "ddef clause" (used to translate a predicate call)

lgt_add_ddef_clause(Functor, Arity, Prefix, Context) :-
	functor(Head, Functor, Arity),
	Head =.. [_| Args],
	lgt_sender(Context, Sender),
	lgt_this(Context, This),
	lgt_self(Context, Self),
	lgt_append(Args, [Sender, This, Self], TArgs),
	THead =.. [Prefix|TArgs],
	lgt_object_(_, _, _, _, _, _, _, _, DDef),
	Clause =.. [DDef, Head, Sender, This, Self, THead],
	(lgt_ddef_(Clause) ->
		true
		;
		assertz(lgt_ddef_(Clause))),
	(lgt_built_in(Head) ->
		assertz(lgt_redefined_built_in_(Head, Context, THead))
		;
		true),
	(lgt_defs_pred_(Functor/Arity) ->
		true
		;
		assertz(lgt_defs_pred_(Functor/Arity))).



% lgt_assert_ddef_clause(+atom, +integer, +atom, +atom, -callable)
%
% asserts a dynamic "ddef clause" (used to translate a predicate call)

lgt_assert_ddef_clause(Functor, Arity, OPrefix, DDef, Call) :-
	lgt_construct_predicate_functor(OPrefix, Functor, Arity, PPrefix),
	functor(Pred, Functor, Arity),
	Pred =.. [_| Args],
	lgt_append(Args, [Sender, This, Self], TArgs),
	Call =.. [PPrefix| TArgs],
	Clause =.. [DDef, Pred, Sender, This, Self, Call],
	assertz(Clause).



% lgt_update_ddef_table(+atom, +callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% if there are no more clauses for the predicate otherwise does nothing

lgt_update_ddef_table(DDef, Call) :-
	functor(Call, Functor, Arity),
	functor(Call2, Functor, Arity),
	(clause(Call2, _) ->
		true
		;
		Clause =.. [DDef, _, _, _, _, Call2],
		retractall(Clause)).



% lgt_convert_test_scope(@term, +term),
%
% convert asserta/z test scope to predicate declaration scope

lgt_convert_test_scope(Scope, Scope2) :-
	var(Scope) ->
		Scope2 = p
		;
		((Scope = p(V), var(V)) ->
			Scope2 = p(p)
			;
			Scope2 = p(p(p))).



% lgt_assert_ddcl_clause(+atom, +term, +term)
%
% asserts a dynamic predicate declaration

lgt_assert_ddcl_clause(DDcl, Pred, Scope) :-
	functor(Pred, Functor, Arity),
	functor(DPred, Functor, Arity),
	Clause =.. [DDcl, DPred, Scope, (dynamic), no],
	assertz(Clause).



% lgt_gen_directives(+atom)
%
% generates entity directives

lgt_gen_directives(object) :-
	lgt_gen_object_directives.

lgt_gen_directives(category) :-
	lgt_gen_category_directives.

lgt_gen_directives(protocol) :-
	lgt_gen_protocol_directives.



lgt_gen_object_directives :-
	lgt_gen_object_dynamic_directives,
	lgt_gen_object_discontiguous_directives.



lgt_gen_category_directives :-
	lgt_gen_category_dynamic_directives,
	lgt_gen_category_discontiguous_directives.



lgt_gen_protocol_directives :-
	lgt_entity_comp_mode_((dynamic)) ->
		lgt_protocol_(_, Prefix, Dcl),
		assertz(lgt_directive_(dynamic(Prefix/1))),
		assertz(lgt_directive_(dynamic(Dcl/4))),
		assertz(lgt_directive_(dynamic(Dcl/5)))
		;
		true.



lgt_gen_object_dynamic_directives :-
	lgt_entity_comp_mode_((dynamic)) ->
		lgt_gen_dynamic_object_dynamic_directives
		;
		lgt_gen_static_object_dynamic_directives.



lgt_gen_dynamic_object_dynamic_directives :-
	lgt_object_(_, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef),
	assertz(lgt_directive_(dynamic(Prefix/7))),
	assertz(lgt_directive_(dynamic(Dcl/4))),
	assertz(lgt_directive_(dynamic(Dcl/6))),
	assertz(lgt_directive_(dynamic(Def/5))),
	assertz(lgt_directive_(dynamic(Def/6))),
	assertz(lgt_directive_(dynamic(Super/6))),
	assertz(lgt_directive_(dynamic(IDcl/6))),
	assertz(lgt_directive_(dynamic(IDef/6))),
	assertz(lgt_directive_(dynamic(DDcl/4))),
	assertz(lgt_directive_(dynamic(DDef/5))),
	forall(
		(lgt_def_(Clause), Clause \= (_ :- _)),
		(arg(5, Clause, Call), functor(Call, Functor, Arity),
		 assertz(lgt_directive_(dynamic(Functor/Arity))))).



lgt_gen_static_object_dynamic_directives :-
	lgt_object_(_, _, _, Def, _, _, _, DDcl, DDef),
	assertz(lgt_directive_(dynamic(DDcl/4))),
	assertz(lgt_directive_(dynamic(DDef/5))),
	lgt_dynamic_(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	lgt_def_(Clause),
	functor(TPred, TFunctor, TArity),
	assertz(lgt_directive_(dynamic(TFunctor/TArity))),
	fail.

lgt_gen_static_object_dynamic_directives.



lgt_gen_object_discontiguous_directives :-
	lgt_object_(_, _, _, Def, _, _, _, _, _),
	lgt_discontiguous_(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	lgt_def_(Clause),
	functor(TPred, TFunctor, TArity),
	assertz(lgt_directive_(discontiguous(TFunctor/TArity))),
	fail.

lgt_gen_object_discontiguous_directives.



lgt_gen_category_dynamic_directives :-
	lgt_entity_comp_mode_((dynamic)) ->
		lgt_category_(_, Prefix, Dcl, Def),
		assertz(lgt_directive_(dynamic(Prefix/2))),
		assertz(lgt_directive_(dynamic(Dcl/4))),
		assertz(lgt_directive_(dynamic(Dcl/5))),
		assertz(lgt_directive_(dynamic(Def/5))),
		forall(
			(lgt_def_(Clause), Clause \= (_ :- _)),
			(arg(5, Clause, Call), functor(Call, Functor, Arity),
		 	 assertz(lgt_directive_(dynamic(Functor/Arity)))))
		 ;
		 true.



lgt_gen_category_discontiguous_directives :-
	lgt_category_(_, _, _, Def),
	lgt_discontiguous_(Functor/Arity),
	functor(Pred, Functor, Arity),
	Clause =.. [Def, Pred, _, _, _, TPred],
	lgt_def_(Clause),
	functor(TPred, TFunctor, TArity),
	assertz(lgt_directive_(discontiguous(TFunctor/TArity))),
	fail.

lgt_gen_category_discontiguous_directives.



% lgt_gen_clauses(+atom)

lgt_gen_clauses(object) :-
	lgt_gen_object_clauses.

lgt_gen_clauses(protocol) :-
	lgt_gen_protocol_clauses.

lgt_gen_clauses(category) :-
	lgt_gen_category_clauses.



lgt_gen_object_clauses :-
	(lgt_rclause_(lgt_instantiates_class_(_, _, _));
	 lgt_rclause_(lgt_specializes_class_(_, _, _))) ->
		lgt_gen_ic_clauses
		;
		lgt_gen_prototype_clauses.



% lgt_gen_local_dcl_clauses
%
% a (local) predicate declaration is only generated
% if there is a scope declaration for the predicate

lgt_gen_local_dcl_clauses :-
	lgt_entity_(_, _, _, Dcl),
	((lgt_public_(Functor/Arity), Scope = p(p(p)));
	 (lgt_protected_(Functor/Arity), Scope = p(p));
	 (lgt_private_(Functor/Arity), Scope = p)),
	functor(Meta, Functor, Arity),
	(lgt_metapredicate_(Meta) ->
		Meta2 = Meta
		;
		Meta2 = no),
	functor(Pred, Functor, Arity),
	(lgt_dynamic_(Functor/Arity)->
		Compilation = (dynamic)
		;
		Compilation = static),
	Fact =.. [Dcl, Pred, Scope, Compilation, Meta2],
	assertz(lgt_dcl_(Fact)),
	fail.

lgt_gen_local_dcl_clauses.



lgt_gen_catchall_dcl_clause :-
	\+ lgt_dcl_(_) ->
		lgt_entity_(_, _, _, Dcl),
		Head =.. [Dcl, _, _, _, _],
		assertz(lgt_dcl_((Head:-fail)))
		;
		true.



% lgt_gen_local_def_clauses
%
% generates local def clauses for undefined but declared (via scope or
% dynamic directives) predicates

lgt_gen_local_def_clauses :-
	lgt_entity_(_, _, EPrefix, _),
	lgt_dynamic_(Functor/Arity),
	\+ lgt_defs_pred_(Functor/Arity),
	lgt_construct_predicate_functor(EPrefix, Functor, Arity, PPrefix),
	lgt_context(Context),
	((\+ lgt_public_(Functor/Arity),
	  \+ lgt_protected_(Functor/Arity),
	  \+ lgt_private_(Functor/Arity)) ->
		lgt_add_ddef_clause(Functor, Arity, PPrefix, Context)
		;
		lgt_add_def_clause(Functor, Arity, PPrefix, Context)),
	fail.

lgt_gen_local_def_clauses.



lgt_gen_obj_catchall_def_clause :-
	\+ lgt_def_(_) ->
		lgt_object_(_, _, _, Def, _, _, _, _, _),
		Head =.. [Def, _, _, _, _, _],
		assertz(lgt_def_((Head:-fail)))
		;
		true.



lgt_gen_protocol_clauses :-
	lgt_gen_protocol_local_clauses,
	lgt_gen_protocol_linking_clauses,
	lgt_gen_protocol_extend_clauses.



lgt_gen_protocol_local_clauses :-
	lgt_gen_local_dcl_clauses,
	lgt_gen_catchall_dcl_clause.



lgt_gen_protocol_linking_clauses :-
	lgt_protocol_(Ptc, _, PDcl),
	Head =.. [PDcl, Pred, Scope, Compilation, Meta, Ptc],
	Body =.. [PDcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body))).



lgt_gen_protocol_extend_clauses :-
	lgt_protocol_(_, _, PDcl1),
	Head =.. [PDcl1, Pred, Scope, Compilation, Meta, Container],
	lgt_extended_protocol_(_, _, PDcl2, EScope),
	(EScope = (public) ->
		Body =.. [PDcl2, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [PDcl2, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl2, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_protocol_extend_clauses.



lgt_gen_category_clauses :-
	lgt_gen_category_dcl_clauses,
	lgt_gen_category_def_clauses.



lgt_gen_category_dcl_clauses :-
	lgt_gen_local_dcl_clauses,
	lgt_gen_catchall_dcl_clause,
	lgt_gen_category_linking_dcl_clauses,
	lgt_gen_category_implements_dcl_clauses.



lgt_gen_category_linking_dcl_clauses :-
	lgt_category_(Ctg, _, CDcl, _),
	Head =.. [CDcl, Pred, Scope, Compilation, Meta, Ctg],
	Body =.. [CDcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body))).



lgt_gen_category_implements_dcl_clauses :-
	lgt_category_(_, _, CDcl, _),
	Head =.. [CDcl, Pred, Scope, Compilation, Meta, Container],
	lgt_implemented_protocol_(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_category_implements_dcl_clauses.



lgt_gen_category_def_clauses :-
	lgt_gen_category_catchall_def_clause.



lgt_gen_category_catchall_def_clause :-
	\+ lgt_def_(_) ->
		lgt_category_(_, _, _, Def),
		Head =.. [Def, _, _, _, _, _],
		assertz(lgt_def_((Head:-fail)))
		;
		true.



lgt_gen_prototype_clauses :-
	lgt_gen_prototype_dcl_clauses,
	lgt_gen_prototype_def_clauses,
	lgt_gen_prototype_super_clauses.



lgt_gen_prototype_dcl_clauses :-
	lgt_gen_local_dcl_clauses,
	lgt_gen_catchall_dcl_clause,
	lgt_gen_prototype_linking_dcl_clauses,
	lgt_gen_prototype_implements_dcl_clauses,
	lgt_gen_prototype_imports_dcl_clauses,
	lgt_gen_prototype_extends_dcl_clauses.



lgt_gen_prototype_linking_dcl_clauses :-
	lgt_object_(Obj, _, Dcl, _, _, _, _, DDcl, _),
	Head =.. [Dcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body))),
	Body2 =.. [DDcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body2))).



lgt_gen_prototype_implements_dcl_clauses :-
	lgt_object_(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Container],
	lgt_implemented_protocol_(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_prototype_implements_dcl_clauses.



lgt_gen_prototype_imports_dcl_clauses :-
	lgt_object_(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, Obj, Container],
	lgt_imported_category_(_, _, CDcl, _, EScope),
	(EScope = (public) ->
		Body =.. [CDcl, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [CDcl, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_prototype_imports_dcl_clauses.



lgt_gen_prototype_extends_dcl_clauses :-
	lgt_object_(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, SContainer, TContainer],
	lgt_extended_object_(_, _, PDcl, _, _, _, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, SContainer, TContainer]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SContainer, TContainer],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, SContainer2, TContainer],
			Body = (Call, (Scope2 = p -> SContainer = SContainer2; SContainer = Obj)))),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_prototype_extends_dcl_clauses.



lgt_gen_prototype_def_clauses :-
	lgt_gen_local_def_clauses,
	lgt_gen_obj_catchall_def_clause,
	lgt_gen_prototype_linking_def_clauses,
	lgt_gen_prototype_imports_def_clauses,
	lgt_gen_prototype_extends_def_clauses.



lgt_gen_prototype_linking_def_clauses :-
	lgt_object_(Obj, _, _, Def, _, _, _, _, DDef),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body2))).



lgt_gen_prototype_imports_def_clauses :-
	lgt_object_(Obj, _, _, ODef, _, _, _, _, _),
	lgt_rclause_(lgt_imports_category_(Obj, Ctg, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctg],
	lgt_imported_category_(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz(lgt_def_((Head:-Body))),	
	fail.

lgt_gen_prototype_imports_def_clauses.



lgt_gen_prototype_extends_def_clauses :-
	lgt_object_(Obj, _, _, ODef, _, _, _, _, _),
	lgt_rclause_(lgt_extends_object_(Obj, Parent, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Container],
	lgt_extended_object_(Parent, _, _, PDef, _, _, _, _, _, _),
	Body =.. [PDef, Pred, Sender, Parent, Self, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

lgt_gen_prototype_extends_def_clauses.



% we can have a root object where super have nowhere to go ...

lgt_gen_prototype_super_clauses :-
	lgt_object_(Obj, _, _, _, OSuper, _, _, _, _),
	\+ lgt_rclause_(lgt_extends_object_(Obj, _, _)),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz(lgt_def_((Head:-fail))),
	!.

% ... or we may extends some objects

lgt_gen_prototype_super_clauses :-
	lgt_object_(Obj, _, _, _, OSuper, _, _, _, _),
	lgt_rclause_(lgt_extends_object_(Obj, Parent, _)),
	Head =.. [OSuper, Pred, Sender, Obj, Self, Call, Container],
	lgt_extended_object_(Parent, _, _, PDef, _, _, _, _, _, _),
	Body =.. [PDef, Pred, Sender, Parent, Self, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

lgt_gen_prototype_super_clauses.



lgt_gen_ic_clauses :-
	lgt_gen_ic_dcl_clauses,
	lgt_gen_ic_idcl_clauses,
	lgt_gen_ic_def_clauses,
	lgt_gen_ic_idef_clauses,
	lgt_gen_ic_super_clauses.



lgt_gen_ic_dcl_clauses :-
	lgt_gen_local_dcl_clauses,
	lgt_gen_catchall_dcl_clause,
	lgt_gen_ic_hierarchy_dcl_clauses.



lgt_gen_ic_hierarchy_dcl_clauses :-
	\+ lgt_instantiated_class_(_, _, _, _, _, _, _, _, _, _),
	!,
	lgt_object_(_, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, _, _, _, _, _, _],
	assertz(lgt_dcl_((Head:-fail))).

lgt_gen_ic_hierarchy_dcl_clauses :-
	lgt_object_(Obj, _, ODcl, _, _, _, _, _, _),
	Head =.. [ODcl, Pred, Scope, Compilation, Meta, SContainer, TContainer],
	lgt_instantiated_class_(_, _, _, _, _, CIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [CIDcl, Pred, Scope, Compilation, Meta, SContainer, TContainer]
		;
		(EScope = protected ->
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SContainer, TContainer],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [CIDcl, Pred, Scope2, Compilation, Meta, SContainer2, TContainer],
			Body = (Call, (Scope2 = p -> SContainer = SContainer2; SContainer = Obj)))),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_ic_hierarchy_dcl_clauses.



% generates instance/class inherited declaration clauses

lgt_gen_ic_idcl_clauses :-
	lgt_gen_ic_linking_idcl_clauses,
	lgt_gen_ic_protocol_idcl_clauses,
	lgt_gen_ic_category_idcl_clauses,
	lgt_gen_ic_hierarchy_idcl_clauses.



lgt_gen_ic_linking_idcl_clauses :-
	lgt_object_(Obj, _, Dcl, _, _, IDcl, _, DDcl, _),
	Head =.. [IDcl, Pred, Scope, Compilation, Meta, Obj, Obj],
	Body =.. [Dcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body))),
	Body2 =.. [DDcl, Pred, Scope, Compilation, Meta],
	assertz(lgt_dcl_((Head:-Body2))).




lgt_gen_ic_protocol_idcl_clauses :-
	lgt_object_(Obj, _, _, _, _, OIDcl, _, _, _),
	Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Container],
	lgt_implemented_protocol_(_, _, PDcl, EScope),
	(EScope = (public) ->
		Body =.. [PDcl, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [PDcl, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [PDcl, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_ic_protocol_idcl_clauses.



lgt_gen_ic_category_idcl_clauses :-
	lgt_object_(Obj, _, _, _, _, OIDcl, _, _, _),
	Head =.. [OIDcl, Pred, Scope, Compilation, Meta, Obj, Container],
	lgt_imported_category_(_, _, CDcl, _, EScope),
	(EScope = (public) ->
		Body =.. [CDcl, Pred, Scope, Compilation, Meta, Container]
		;
		(EScope = protected ->
			Call =.. [CDcl, Pred, Scope2, Compilation, Meta, Container],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Body =.. [CDcl, Pred, _, Compilation, Meta, Container])),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_ic_category_idcl_clauses.



lgt_gen_ic_hierarchy_idcl_clauses :-
	lgt_object_(Obj, _, _, _, _, CIDcl, _, _, _),
	Head =.. [CIDcl, Pred, Scope, Compilation, Meta, SContainer, TContainer],
	lgt_specialized_class_(_, _, _, _, _, SIDcl, _, _, _, EScope),
	(EScope = (public) ->
		Body =.. [SIDcl, Pred, Scope, Compilation, Meta, SContainer, TContainer]
		;
		(EScope = protected ->
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SContainer, TContainer],
			Body = (Call, (Scope2 = p -> Scope = p; Scope = p(p)))
			;
			Scope = p,
			Call =.. [SIDcl, Pred, Scope2, Compilation, Meta, SContainer2, TContainer],
			Body = (Call, (Scope2 = p -> SContainer = SContainer2; SContainer = Obj)))),
	assertz(lgt_dcl_((Head:-Body))),
	fail.

lgt_gen_ic_hierarchy_idcl_clauses.



lgt_gen_ic_def_clauses :-
	lgt_gen_local_def_clauses,
	lgt_gen_obj_catchall_def_clause,
	lgt_gen_ic_linking_def_clauses,
	lgt_gen_ic_imports_def_clauses,
	lgt_gen_ic_hierarchy_def_clauses.



lgt_gen_ic_linking_def_clauses :-
	lgt_object_(Obj, _, _, Def, _, _, _, _, DDef),
	Head =.. [Def, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body2))).



lgt_gen_ic_imports_def_clauses :-
	lgt_object_(Obj, _, _, ODef, _, _, _, _, _),
	lgt_rclause_(lgt_imports_category_(Obj, Ctg, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Ctg],
	lgt_imported_category_(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz(lgt_def_((Head:-Body))),	
	fail.

lgt_gen_ic_imports_def_clauses.



lgt_gen_ic_hierarchy_def_clauses :-
	lgt_object_(Obj, _, _, ODef, _, _, _, _, _),
	lgt_rclause_(lgt_instantiates_class_(Obj, Class, _)),
	Head =.. [ODef, Pred, Sender, Obj, Self, Call, Container],
	lgt_instantiated_class_(Class, _, _, _, _, _, CIDef, _, _, _),
	Body =.. [CIDef, Pred, Sender, Class, Self, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

lgt_gen_ic_hierarchy_def_clauses.




lgt_gen_ic_idef_clauses :-
	lgt_gen_ic_linking_idef_clauses,
	lgt_gen_ic_category_idef_clauses,
	lgt_gen_ic_hierarchy_idef_clauses.



lgt_gen_ic_linking_idef_clauses :-
	lgt_object_(Obj, _, _, Def, _, _, IDef, _, DDef),
	Head =.. [IDef, Pred, Sender, This, Self, Call, Obj],
	Body =.. [Def, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body))),
	Body2 =.. [DDef, Pred, Sender, This, Self, Call],
	assertz(lgt_def_((Head:-Body2))).



lgt_gen_ic_category_idef_clauses :-
	lgt_object_(Obj, _, _, _, _, _, OIDef, _, _),
	lgt_rclause_(lgt_imports_category_(Obj, Ctg, _)),
	Head =.. [OIDef, Pred, Sender, Obj, Self, Call, Ctg],
	lgt_imported_category_(Ctg, _, _, CDef, _),
	Body =.. [CDef, Pred, Sender, Obj, Self, Call],
	assertz(lgt_def_((Head:-Body))),	
	fail.


lgt_gen_ic_category_idef_clauses.



lgt_gen_ic_hierarchy_idef_clauses :-
	lgt_object_(Class, _, _, _, _, _, CIDef, _, _),
	lgt_rclause_(lgt_specializes_class_(Class, Super, _)),
	Head =.. [CIDef, Pred, Sender, Class, Self, Call, Container],
	lgt_specialized_class_(Super, _, _, _, _, _, SIDef, _, _, _),
	Body =.. [SIDef, Pred, Sender, Super, Self, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

lgt_gen_ic_hierarchy_idef_clauses.



% we can have a root object where super have nowhere to go ...

lgt_gen_ic_super_clauses :-
	lgt_object_(_, _, _, _, OSuper, _, _, _, _),
	\+ lgt_rclause_(lgt_instantiates_class_(_, _, _)),
	\+ lgt_rclause_(lgt_specializes_class_(_, _, _)),
	Head =.. [OSuper, _, _, _, _, _, _],
	assertz(lgt_def_((Head:-fail))),
	!.

% ... or predicates can be redefined in instances...

lgt_gen_ic_super_clauses :-
	lgt_object_(Obj, _, _, _, OSuper, _, _, _, _),
	lgt_rclause_(lgt_instantiates_class_(Obj, Class, _)),
	Head =.. [OSuper, Pred, Sender, Obj, Obj, Call, Container],
	lgt_instantiated_class_(Class, _, _, _, _, _, CIDef, _, _, _),
	Body =.. [CIDef, Pred, Sender, Class, Obj, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

% ... or/and in subclasses...

lgt_gen_ic_super_clauses :-
	lgt_object_(Class, _, _, _, CSuper, _, _, _, _),
	lgt_rclause_(lgt_specializes_class_(Class, Super, _)),
	Head =.. [CSuper, Pred, Sender, Class, Self, Call, Container],
	lgt_specialized_class_(Super, _, _, _, _, _, SIDef, _, _, _),
	Body =.. [SIDef, Pred, Sender, Super, Self, Call, Container],
	assertz(lgt_def_((Head:-Body))),
	fail.

lgt_gen_ic_super_clauses.



% lgt_fix_redef_built_ins
%
% fix the calls of any redefined built-in predicate in all entity clauses 
% and initialization goals

lgt_fix_redef_built_ins :-
	retract(lgt_eclause_(Clause)),
	lgt_fix_redef_built_ins(Clause, Fixed),
	assertz(lgt_feclause_(Fixed)),
	fail.

lgt_fix_redef_built_ins :-
	retract(lgt_entity_init_(Call)),
	lgt_fix_redef_built_ins(Call, Fixed),
	assertz(lgt_fentity_init_(Fixed)),
	fail.

lgt_fix_redef_built_ins.



% lgt_fix_redef_built_ins(+clause, -clause)

lgt_fix_redef_built_ins((Head:-Body), (Head:-Fixed)) :-
	!,
	lgt_fix_redef_built_ins(Body, Fixed).

lgt_fix_redef_built_ins((Pred1, Pred2), (TPred1, TPred2)) :-
	!,
	lgt_fix_redef_built_ins(Pred1, TPred1),
	lgt_fix_redef_built_ins(Pred2, TPred2).

lgt_fix_redef_built_ins((Pred1; Pred2), (TPred1; TPred2)) :-
	!,
	lgt_fix_redef_built_ins(Pred1, TPred1),
	lgt_fix_redef_built_ins(Pred2, TPred2).

lgt_fix_redef_built_ins((Pred1 -> Pred2), (TPred1 -> TPred2)) :-
	!,
	lgt_fix_redef_built_ins(Pred1, TPred1),
	lgt_fix_redef_built_ins(Pred2, TPred2).

lgt_fix_redef_built_ins(\+ Pred, \+ TPred) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(call(Pred), call(TPred)) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(once(Pred), once(TPred)) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery)) :-
	!,
	lgt_fix_redef_built_ins(Goal, TGoal),
	lgt_fix_redef_built_ins(Recovery, TRecovery).

lgt_fix_redef_built_ins(bagof(Term, Pred, List), bagof(Term, TPred, List)) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(findall(Term, Pred, List), findall(Term, TPred, List)) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(forall(Generate, Test), forall(TGenerate, TTest)) :-
	!,
	lgt_fix_redef_built_ins(Generate, TGenerate),
	lgt_fix_redef_built_ins(Test, TTest).

lgt_fix_redef_built_ins(setof(Term, Pred, List), setof(Term, TPred, List)) :-
	!,
	lgt_fix_redef_built_ins(Pred, TPred).

lgt_fix_redef_built_ins(lgt_call_built_in(Pred, Context), TPred) :-
	!,
	(lgt_redefined_built_in_(Pred, Context, TPred) ->
		true
		;
		lgt_fix_redef_built_ins(Pred, TPred)).

lgt_fix_redef_built_ins(Pred, Pred).



% find and report misspelt predicate calls
% in the body of objects/cartegories predicates

lgt_find_misspelt_calls :-
	setof(Pred,
		(lgt_calls_pred_(Pred), \+ lgt_defs_pred_(Pred), \+ lgt_dynamic_(Pred)),
		Preds) ->
		lgt_report_misspelt_calls(Preds)
		;
		true.



% lgt_report_misspelt_calls(+list)

lgt_report_misspelt_calls([]).

lgt_report_misspelt_calls([Pred| Preds]) :-
	(lgt_compiler_option(misspelt, warning),
	 lgt_compiler_option(report, on)) ->
		write('WARNING!  these static predicates are called but never defined: '),
		writeq([Pred| Preds]), nl
		;
		true.



% lgt_write_directives(+stream)
%
% writes the Logtalk and user directives

lgt_write_directives(Stream) :-
	lgt_write_lgt_directives(Stream),
	lgt_write_user_directives(Stream).



% lgt_write_lgt_directives(+stream)
%
% writes the Logtalk message sending operator directives

lgt_write_lgt_directives(Stream) :-
	write_term(Stream, ':- ', []),
	write_term(Stream, op(600, xfy, ::), [quoted(true)]),
	write_term(Stream, '.', []), nl(Stream),
	write_term(Stream, ':- ', []),
	write_term(Stream, op(600,  fy, ::), [quoted(true)]),
	write_term(Stream, '.', []), nl(Stream),
	write_term(Stream, ':- ', []),
	write_term(Stream, op(600,  fx, ^^), [quoted(true)]),
	write_term(Stream, '.', []), nl(Stream).



% lgt_write_user_directives(+stream)
%
% writes the user directives

lgt_write_user_directives(Stream) :-
	lgt_directive_(Dir),
	write_term(Stream, ':- ', []),
	write_term(Stream, Dir, [quoted(true)]),
	write_term(Stream, '.', []),
	nl(Stream),
	fail.

lgt_write_user_directives(_).



lgt_write_clauses(Stream) :-
	lgt_write_functors_clause(Stream),
	lgt_write_dcl_clauses(Stream),
	lgt_write_def_clauses(Stream),
	lgt_write_ddef_clauses(Stream),
	lgt_write_super_clauses(Stream),
	lgt_write_entity_clauses(Stream).



lgt_write_functors_clause(Stream) :-
	lgt_entity_functors_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []),
	nl(Stream).



lgt_write_def_clauses(Stream) :-
	lgt_def_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []), nl(Stream),
	fail.

lgt_write_def_clauses(_).



lgt_write_ddef_clauses(Stream) :-
	lgt_ddef_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []), nl(Stream),
	fail.

lgt_write_ddef_clauses(_).



lgt_write_dcl_clauses(Stream) :-
	lgt_dcl_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []),
	nl(Stream),
	fail.

lgt_write_dcl_clauses(_).



lgt_write_super_clauses(Stream) :-
	lgt_super_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []),
	nl(Stream),
	fail.

lgt_write_super_clauses(_).



lgt_write_entity_clauses(Stream) :-
	lgt_feclause_(Clause),
	write_term(Stream, Clause, [quoted(true)]),
	write_term(Stream, '.', []),
	nl(Stream),
	fail.

lgt_write_entity_clauses(_).



% lgt_write_init_call(+stream)
%
% writes the initialization call for the compiled entity that will assert 
% the relation clauses and call any declared initialization goal when the
% entity is loaded

lgt_write_init_call(Stream) :-
	lgt_compiler_option(iso_initialization_dir, true),
	!,
	findall(Clause, lgt_rclause_(Clause), Clauses),
	write_term(Stream, ':- initialization((lgt_assert_relation_clauses(', []),
	write_term(Stream, Clauses, [quoted(true)]),
	write_term(Stream, ')', []),
	(lgt_fentity_init_(Call) ->
		write_term(Stream, ', ', []),
		write_term(Stream, Call, [quoted(true)])
		;
		true),
	write_term(Stream, ')).', []), nl(Stream).

lgt_write_init_call(Stream) :-
	findall(Clause, lgt_rclause_(Clause), Clauses),
	write_term(Stream, ':- lgt_assert_relation_clauses(', []),
	write_term(Stream, Clauses, [quoted(true)]),
	write_term(Stream, ').', []), nl(Stream),
	(lgt_fentity_init_(Call) ->
		write_term(Stream, ':- ', []),
		write_term(Stream, Call, [quoted(true)]),
		write_term(Stream, '.', []),
		nl(Stream)
		;
		true).



% lgt_assert_tr_entity
%
% adds a dynamically created entity to memory

lgt_assert_tr_entity :-
	lgt_assert_directives,
	lgt_assert_functors_clause,
	lgt_assert_dcl_clauses,
	lgt_assert_def_clauses,
	lgt_assert_ddef_clauses,
	lgt_assert_super_clauses,
	lgt_assert_entity_clauses,
	lgt_assert_relation_clauses,
	lgt_assert_init.



lgt_assert_directives :-
	lgt_directive_((dynamic(Functor/Arity))),
	functor(Pred, Functor, Arity),
	asserta(Pred),
	retractall(Pred),
	fail.

lgt_assert_directives :-
	lgt_directive_((op(Priority, Specifier, Operators))),
	op(Priority, Specifier, Operators),
	fail.
	
lgt_assert_directives.



lgt_assert_functors_clause :-
	lgt_entity_functors_(Clause),
	assertz(Clause).



lgt_assert_dcl_clauses :-
	lgt_dcl_(Clause),
	assertz(Clause),
	fail.

lgt_assert_dcl_clauses.



lgt_assert_def_clauses :-
	lgt_def_(Clause),
	assertz(Clause),
	fail.

lgt_assert_def_clauses.



lgt_assert_ddef_clauses :-
	lgt_ddef_(Clause),
	assertz(Clause),
	fail.

lgt_assert_ddef_clauses.



lgt_assert_super_clauses :-
	lgt_super_(Clause),
	assertz(Clause),
	fail.

lgt_assert_super_clauses.



lgt_assert_entity_clauses :-
	lgt_feclause_(Clause),
	assertz(Clause),
	fail.

lgt_assert_entity_clauses.



lgt_assert_relation_clauses :-
	lgt_rclause_(Clause),
	assertz(Clause),
	fail.

lgt_assert_relation_clauses.



% lgt_assert_init
%
% call any defined initialization goal for a dynamically created entity

lgt_assert_init :-
	lgt_fentity_init_(Goal) ->
		once(Goal)
		;
		true.



% lgt_assert_relation_clauses(+list)
%
% called when loading a compiled Logtalk entity to update Logtalk 
% internal tables
%
% we may be reloading the entity so we must first retract any old
% relation clauses before asserting the new ones

lgt_assert_relation_clauses([Clause| Clauses]) :-
	arg(1, Clause, Entity),
	lgt_retract_old_relation_clauses(Entity),
	lgt_assert_new_relation_clauses([Clause| Clauses]).


lgt_retract_old_relation_clauses(Entity) :-
	retractall(lgt_current_object_(Entity, _, _, _, _)),
	retractall(lgt_current_protocol_(Entity, _)),
	retractall(lgt_current_category_(Entity, _)),
	retractall(lgt_implements_protocol_(Entity, _, _)),
	retractall(lgt_imports_category_(Entity, _, _)),
	retractall(lgt_instantiates_class_(Entity, _, _)),
	retractall(lgt_specializes_class_(Entity, _, _)),
	retractall(lgt_extends_protocol_(Entity, _, _)),
	retractall(lgt_extends_object_(Entity, _, _)).


lgt_assert_new_relation_clauses([]).

lgt_assert_new_relation_clauses([Clause| Clauses]) :-
	assertz(Clause),
	lgt_assert_new_relation_clauses(Clauses).



% lgt_construct_object_functors(+compound, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of an object

lgt_construct_object_functors(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef) :-
	functor(Obj, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Aux, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def),
	atom_concat(Prefix, '_super', Super),
	atom_concat(Prefix, '_idcl', IDcl),
	atom_concat(Prefix, '_idef', IDef),
	atom_concat(Prefix, '_ddcl', DDcl),
	atom_concat(Prefix, '_ddef', DDef).



% lgt_construct_protocol_functors(+compound, -atom, -atom)
%
% constructs all the functors used in the compiled code of a protocol

lgt_construct_protocol_functors(Ptc, Prefix, Dcl) :-
	functor(Ptc, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Aux, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl).



% lgt_construct_category_functors(+compound, -atom, -atom, -atom)
%
% constructs all the functors used in the compiled code of a category

lgt_construct_category_functors(Ctg, Prefix, Dcl, Def) :-
	functor(Ctg, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, Atom, Aux),
	atom_concat(Aux, '_', Prefix),
	atom_concat(Prefix, '_dcl', Dcl),
	atom_concat(Prefix, '_def', Def).



% lgt_construct_predicate_functor(+atom, +atom, +integer, -atom)
%
% constructs the functor used for a compiled predicate

lgt_construct_predicate_functor(EPrefix, Functor, Arity, PPrefix) :-
	atom_concat(EPrefix, Functor, Aux),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Aux, Atom, PPrefix).



% lgt_built_in(+callable)
%
% checks if the argument is either a Prolog or Logtalk built-in predicate

lgt_built_in(Pred) :-
	lgt_pl_built_in(Pred).

lgt_built_in(Pred) :-
	lgt_lgt_built_in(Pred).



% lgt_pl_built_in(+callable)
%
% either host Prolog native built-ins or missing ISO built-ins
% that we have defined in the correspondent config file

lgt_pl_built_in(Pred) :-
	lgt_predicate_property(Pred, built_in).

lgt_pl_built_in(Pred) :-
	lgt_iso_predicate(Pred).



% logtalk built-in methods
%
% lgt_built_in_method(?callable, ?scope)

lgt_built_in_method(parameter(_, _), p).
lgt_built_in_method(self(_), p).
lgt_built_in_method(sender(_), p).
lgt_built_in_method(this(_), p).

lgt_built_in_method(current_predicate(_), p(p(p))).
lgt_built_in_method(predicate_property(_, _), p(p(p))).

lgt_built_in_method(abolish(_), p(p(p))).
lgt_built_in_method(asserta(_), p(p(p))).
lgt_built_in_method(assertz(_), p(p(p))).
lgt_built_in_method(clause(_, _), p(p(p))).
lgt_built_in_method(retract(_), p(p(p))).
lgt_built_in_method(retractall(_), p(p(p))).

lgt_built_in_method(bagof(_, _, _), p(p(p))).
lgt_built_in_method(findall(_, _, _), p(p(p))).
lgt_built_in_method(forall(_, _), p(p(p))).
lgt_built_in_method(setof(_, _, _), p(p(p))).



% Logtalk directives
%
% lgt_lgt_directive(+atom/+integer)

lgt_lgt_directive(Directive) :-
	lgt_lgt_opening_directive(Directive).

lgt_lgt_directive(Directive) :-
	lgt_lgt_closing_directive(Directive).

lgt_lgt_directive(Directive) :-
	lgt_lgt_entity_directive(Directive).

lgt_lgt_directive(Directive) :-
	lgt_lgt_predicate_directive(Directive).


lgt_lgt_opening_directive(object/1).
lgt_lgt_opening_directive(object/2).
lgt_lgt_opening_directive(object/3).
lgt_lgt_opening_directive(object/4).

lgt_lgt_opening_directive(category/1).
lgt_lgt_opening_directive(category/2).

lgt_lgt_opening_directive(protocol/1).
lgt_lgt_opening_directive(protocol/2).



lgt_lgt_closing_directive(end_object/0).

lgt_lgt_closing_directive(end_category/0).

lgt_lgt_closing_directive(end_protocol/0).



lgt_lgt_entity_directive(calls/N) :-
	N >= 1.
lgt_lgt_entity_directive(uses/N) :-
	N >= 1.

lgt_lgt_entity_directive((initialization)/1).

lgt_lgt_entity_directive((dynamic)/N) :-
	N =:= 0.

lgt_lgt_entity_directive(op/3).

lgt_lgt_entity_directive(info/1).
lgt_lgt_entity_directive(info/2).



lgt_lgt_predicate_directive((dynamic)/N) :-
	N >= 1.

lgt_lgt_predicate_directive(metapredicate/N) :-
	N >= 1.

lgt_lgt_predicate_directive((discontiguous)/N) :-
	N >= 1.

lgt_lgt_predicate_directive((public)/N) :-
	N >= 1.
lgt_lgt_predicate_directive(protected/N) :-
	N >= 1.
lgt_lgt_predicate_directive(private/N) :-
	N >= 1.

lgt_lgt_predicate_directive((mode)/2).



% built-in Prolog metapredicates

lgt_pl_metapredicate(catch(::, *, ::)).

lgt_pl_metapredicate(bagof(*, ::, *)).
lgt_pl_metapredicate(setof(*, ::, *)).
lgt_pl_metapredicate(findall(*, ::, *)).

lgt_pl_metapredicate(forall(::, ::)).

lgt_pl_metapredicate(call(::)).
lgt_pl_metapredicate(once(::)).

lgt_pl_metapredicate(\+ (::)).



% utility predicates used during compilation of Logtalk 
% entities to store and access context information

lgt_context(context(_, _, _, _, _)).

lgt_sender(context(Sender, _, _, _, _), Sender).

lgt_this(context(_, This, _, _, _), This).

lgt_self(context(_, _, Self, _, _), Self).

lgt_prefix(context(_, _, _, Prefix, _), Prefix).

lgt_metavars(context(_, _, _, _, Metavars), Metavars).



% lgt_convert_to_list(+pi_or_pi_list, -pi_list)

lgt_convert_to_list([[A|B]], [A|B]) :-	% predicate indicator list
	!.

lgt_convert_to_list([A|B], [A|B]) :-	% predicate indicator sequence
	!.

lgt_convert_to_list(A, [A]).			% single predicate indicator



% lgt_valid_pred_ind(@term)

lgt_valid_pred_ind(Term) :-
	nonvar(Term),
	Term = Functor/Arity,
	atom(Functor),
	integer(Arity),
	Arity >= 0.


% lgt_valid_object_id(@term)

lgt_valid_object_id(Term) :-
	once((atom(Term); compound(Term))).



% lgt_valid_category_id(@term)

lgt_valid_category_id(Term) :-
	atom(Term).



% lgt_valid_protocol_id(@term)

lgt_valid_protocol_id(Term) :-
	atom(Term).



% lgt_valid_scope(@term)
	
lgt_valid_scope(Term) :-
	nonvar(Term),	
	(Term = (Scope::_) ->
		nonvar(Scope),
		once(lgt_member(Scope, [(public), protected, private]))
		;
		true).



% lgt_scope_id(+term, -atom, -term)

lgt_scope_id(Scope::Entity, Scope, Entity) :-
	!.

lgt_scope_id(Entity, (public), Entity).



% lgt_callable(@term)

lgt_callable(Term) :-
	nonvar(Term),
	functor(Term, Functor, _),
	atom(Functor).



% lgt_valid_op_priority(@term)

lgt_valid_op_priority(Priority) :-
	integer(Priority),
	Priority >= 0,
	Priority =< 1200.



% lgt_valid_op_specifier(@term)

lgt_valid_op_specifier(Specifier) :-
	nonvar(Specifier),
	once(lgt_member(Specifier, [fx, fy, xfx, xfy, yfx, xf, yf])).



% lgt_valid_op_names(@term)

lgt_valid_op_names(Operators) :-
	nonvar(Operators),
	lgt_convert_to_list(Operators, List),
	forall(lgt_member(Operator, List), atom(Operator)).



% lgt_valid_metapred_term(@term)

lgt_valid_metapred_term(Pred) :-
	nonvar(Pred),
	Pred =.. [_| Args],
	forall(lgt_member(Arg, Args), (nonvar(Arg), (Arg = (::); Arg = (*)))).



% lgt_valid_mode_term(@term)

lgt_valid_mode_term(Pred) :-
	nonvar(Pred),
	Pred =.. [_| Args],
	forall(
		lgt_member(Arg, Args),
		(nonvar(Arg), functor(Arg, Functor, Arity), Arity =< 1, lgt_member(Functor, [?, +, -, @]))).



% lgt_valid_number_of_solutions(@term)

lgt_valid_number_of_solutions(Solutions) :-
	nonvar(Solutions),
	once(lgt_member(Solutions, [zero, one, zero_or_one, zero_or_more, one_or_more, error])).



% lgt_valid_info_list(@list)
%
% true if the argument is a list of key-value pairs

lgt_valid_info_list([]).

lgt_valid_info_list([Head| Tail]) :-
	nonvar(Head),
	Head = (Key is Value),
	nonvar(Key),
	nonvar(Value),
	lgt_valid_info_list(Tail).



% lgt_valid_compiler_options(@list)
%
% true if all compiler options are valid

lgt_valid_compiler_options([]).

lgt_valid_compiler_options([Option| Options]) :-
	nonvar(Option),
	lgt_valid_compiler_option(Option),
	lgt_valid_compiler_options(Options).



% lgt_valid_compiler_option(@nonvar)

lgt_valid_compiler_option(iso_initialization_dir(Option)) :-
	once((Option == true; Option == false)).

lgt_valid_compiler_option(xml(Option)) :-
	once((Option == on; Option == off)).

lgt_valid_compiler_option(xsl(File)) :-
	atom(File).

lgt_valid_compiler_option(unknown(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(singletons(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(misspelt(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(lgtredef(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(plredef(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(portability(Option)) :-
	once((Option == silent; Option == warning)).

lgt_valid_compiler_option(report(Option)) :-
	once((Option == on; Option == off)).

lgt_valid_compiler_option(smart_compilation(Option)) :-
	once((Option == on; Option == off)).



% lgt_valid_flag(@nonvar)
%
% true if the argument is a valid Logtalk flag

lgt_valid_flag(iso_initialization_dir).
lgt_valid_flag(xml).
lgt_valid_flag(xsl).
lgt_valid_flag(unknown).
lgt_valid_flag(singletons).
lgt_valid_flag(misspelt).
lgt_valid_flag(lgtredef).
lgt_valid_flag(plredef).
lgt_valid_flag(portability).
lgt_valid_flag(report).
lgt_valid_flag(smart_compilation).
lgt_valid_flag(startup_message).
lgt_valid_flag(version).



% lgt_valid_flag(@term, @term)
%
% true if the argument is a valid Logtalk flag-value pair

lgt_valid_flag(Flag, Value) :-
	atom(Flag),
	Option =.. [Flag, Value],
	lgt_valid_compiler_option(Option).



% lgt_read_only_flag(@nonvar)
%
% true if the argument is a read only Logtalk flag

lgt_read_only_flag(startup_message).
lgt_read_only_flag(version).



% Logtalk built-in predicates
%
% lgt_lgt_built_in(?callable)

lgt_lgt_built_in(::(_, _)).

lgt_lgt_built_in(forall(_, _)).
lgt_lgt_built_in(retractall(_)).

lgt_lgt_built_in(logtalk_compile(_)).
lgt_lgt_built_in(logtalk_compile(_, _)).
lgt_lgt_built_in(logtalk_load(_)).
lgt_lgt_built_in(logtalk_load(_, _)).

lgt_lgt_built_in(logtalk_version(_, _, _)).

lgt_lgt_built_in(protocol_property(_, _)).
lgt_lgt_built_in(category_property(_, _)).
lgt_lgt_built_in(object_property(_, _)).

lgt_lgt_built_in(current_protocol(_)).
lgt_lgt_built_in(current_category(_)).
lgt_lgt_built_in(current_object(_)).

lgt_lgt_built_in(create_object(_, _, _, _)).
lgt_lgt_built_in(create_category(_, _, _, _)).
lgt_lgt_built_in(create_protocol(_, _, _)).

lgt_lgt_built_in(abolish_object(_)).
lgt_lgt_built_in(abolish_category(_)).
lgt_lgt_built_in(abolish_protocol(_)).

lgt_lgt_built_in(implements_protocol(_, _)).
lgt_lgt_built_in(implements_protocol(_, _, _)).
lgt_lgt_built_in(imports_category(_, _)).
lgt_lgt_built_in(imports_category(_, _, _)).
lgt_lgt_built_in(instantiates_class(_, _)).
lgt_lgt_built_in(instantiates_class(_, _, _)).
lgt_lgt_built_in(specializes_class(_, _)).
lgt_lgt_built_in(specializes_class(_, _, _)).
lgt_lgt_built_in(extends_protocol(_, _)).
lgt_lgt_built_in(extends_protocol(_, _, _)).
lgt_lgt_built_in(extends_object(_, _)).
lgt_lgt_built_in(extends_object(_, _, _)).

lgt_lgt_built_in(abolish_events(_, _, _, _, _)).
lgt_lgt_built_in(define_events(_, _, _, _, _)).
lgt_lgt_built_in(current_event(_, _, _, _, _)).

lgt_lgt_built_in(current_logtalk_flag(_, _)).
lgt_lgt_built_in(set_logtalk_flag(_, _)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  xml
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% lgt_write_xml_file(+stream)
%
% writes a XML file containing the documentation of a compiled entity

lgt_write_xml_file(Stream) :-
	lgt_write_xml_header(Stream),
	lgt_write_xml_entity(Stream),
	lgt_write_xml_relations(Stream),
	lgt_write_xml_predicates(Stream),
	lgt_write_xml_footer(Stream).



lgt_write_xml_header(Stream) :-
	lgt_write_xml_open_tag(Stream, '?xml version="1.0"?', []),
	write_term(Stream, '<!DOCTYPE logtalk SYSTEM "logtalk.dtd">', []), nl(Stream),
	lgt_compiler_option(xsl, XSL),
	write_term(Stream, '<?xml-stylesheet type="text/xsl" href="', []),
	write_term(Stream, XSL, []),
	write_term(Stream, '"?>', []), nl(Stream),
	lgt_write_xml_open_tag(Stream, logtalk, []).



lgt_write_xml_footer(Stream) :-
	lgt_write_xml_close_tag(Stream, logtalk).



lgt_write_xml_entity(Stream) :-
	lgt_entity_(Type, Entity, _, _),
	(lgt_entity_comp_mode_((dynamic)) -> Compilation = (dynamic); Compilation = static),
	lgt_write_xml_open_tag(Stream, entity, []),
	lgt_entity_to_xml_name(Entity, Name),
	lgt_write_xml_cdata_element(Stream, name, [], Name),
	lgt_write_xml_element(Stream, (type), [], Type),
	lgt_write_xml_element(Stream, compilation, [], Compilation),
	(lgt_info_(List) ->
		(lgt_member(comment is Comment, List) ->
			lgt_write_xml_cdata_element(Stream, comment, [], Comment)
			;
			true), 
		(lgt_member(authors is Authors, List) ->
			lgt_write_xml_cdata_element(Stream, authors, [], Authors)
			;
			true), 
		(lgt_member(version is Version, List) ->
			lgt_write_xml_element(Stream, version, [], Version)
			;
			true), 
		(lgt_member(date is Date, List) ->
			lgt_write_xml_element(Stream, date, [], Date)
			;
			true),
		forall(
			(lgt_member(Key is Value, List),
			 \+ lgt_member(Key, [comment, authors, version, date, parnames])),
			(lgt_write_xml_open_tag(Stream, info, []),
			 lgt_write_xml_element(Stream, key, [], Key),
			 lgt_write_xml_cdata_element(Stream, value, [], Value),
			 lgt_write_xml_close_tag(Stream, info)))
		;
		true),
	lgt_write_xml_close_tag(Stream, entity).



% lgt_entity_to_xml_name(+entity, -nonvar)
%
% instantiates the parameters in a parametric object to
% user defined names or to the atom '_'

lgt_entity_to_xml_name(Entity, Name) :-
	lgt_info_(List),
	lgt_member(parnames is Names, List),
	!,
	Entity =.. [Functor| Names],
	Name =.. [Functor| Names].

lgt_entity_to_xml_name(Entity, Name) :-
	Entity =.. [Functor| Args],
	lgt_vars_to_underscore(Args, Names),
	Name =.. [Functor| Names].



% lgt_relation_to_xml_name(+entity, +entity, -atom)
%
% instantiates the parameters in a related entity taking
% in account the parameter sharing with the original entity

lgt_relation_to_xml_name(Entity, Relation, Name) :-
	lgt_entity_to_xml_name(Entity, _),
	Relation =.. [Functor| Args],
	lgt_vars_to_underscore(Args, Names),
	Name =.. [Functor| Names].



% lgt_vars_to_underscore(+list, -list)
%
% instantiates the variables in the input list to the atom '_'

lgt_vars_to_underscore([], []).

lgt_vars_to_underscore([Arg| Args], [Name| Names]) :-
	(var(Arg) -> Name = '_'; Name = Arg),
	lgt_vars_to_underscore(Args, Names).



% lgt_relation_to_xml_filename(+entity, -atom)
%
% needed to build filenames in links to parametric objects

lgt_relation_to_xml_filename(Relation, File) :-
	atom(Relation) ->
		File = Relation
		;
		functor(Relation, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, Atom, File).



% lgt_write_xml_predicates(+stream)
%
% writes the predicate documentation

lgt_write_xml_predicates(Stream) :-
	lgt_write_xml_open_tag(Stream, predicates, []),
	lgt_write_xml_public_predicates(Stream),
	lgt_write_xml_protected_predicates(Stream),
	lgt_write_xml_private_predicates(Stream),
	lgt_write_xml_close_tag(Stream, predicates).



% lgt_write_xml_public_predicates(+stream)
%
% writes the documentation of public predicates

lgt_write_xml_public_predicates(Stream) :-
	lgt_write_xml_open_tag(Stream, (public), []),
	lgt_public_(Functor/Arity),
	lgt_write_xml_predicate(Stream, Functor/Arity, (public)),
	fail.

lgt_write_xml_public_predicates(Stream) :-
	lgt_write_xml_close_tag(Stream, (public)).



% lgt_write_xml_protected_predicates(+stream)
%
% writes the documentation protected predicates

lgt_write_xml_protected_predicates(Stream) :-
	lgt_write_xml_open_tag(Stream, protected, []),
	lgt_protected_(Functor/Arity),
	lgt_write_xml_predicate(Stream, Functor/Arity, protected),
	fail.

lgt_write_xml_protected_predicates(Stream) :-
	lgt_write_xml_close_tag(Stream, protected).



% lgt_write_xml_private_predicates(+stream)
%
% writes the documentation of private predicates

lgt_write_xml_private_predicates(Stream) :-
	lgt_write_xml_open_tag(Stream, private, []),
	lgt_private_(Functor/Arity),
	lgt_write_xml_predicate(Stream, Functor/Arity, private),
	fail.

lgt_write_xml_private_predicates(Stream) :-
	lgt_write_xml_close_tag(Stream, private).



% lgt_write_xml_predicate(+stream, +atom/+integer, +term)
%
% writes the documentation of a predicate

lgt_write_xml_predicate(Stream, Functor/Arity, Scope) :-
	((lgt_entity_comp_mode_((dynamic)); lgt_dynamic_(Functor/Arity)) ->
		Compilation = (dynamic)
		;
		Compilation = static),
	lgt_write_xml_open_tag(Stream, predicate, []),
	lgt_write_xml_cdata_element(Stream, name, [], Functor/Arity),
	lgt_write_xml_element(Stream, scope, [], Scope),
	lgt_write_xml_element(Stream, compilation, [], Compilation),
	functor(Meta, Functor, Arity),
	(lgt_metapredicate_(Meta) ->
		lgt_write_xml_cdata_element(Stream, meta, [], Meta)
		;
		true),
	functor(Template, Functor, Arity),
	forall(
		lgt_mode_(Template, Solutions),
		(lgt_write_xml_open_tag(Stream, (mode), []),
		 lgt_write_xml_cdata_element(Stream, template, [], Template),
		 lgt_write_xml_element(Stream, solutions, [], Solutions),
		 lgt_write_xml_close_tag(Stream, (mode)))),
	((lgt_info_(Functor/Arity, List), lgt_member(comment is Comment, List)) ->
		lgt_write_xml_cdata_element(Stream, comment, [], Comment)
		;
		true),
	((lgt_info_(Functor/Arity, List), lgt_member(argnames is Names, List)) ->
		Template =.. [Functor| Names],
		lgt_write_xml_cdata_element(Stream, template, [], Template)
		;
		true),
	forall(
		(lgt_info_(Functor/Arity, List),
		 lgt_member(Key is Value, List),
		 \+ lgt_member(Key, [comment, argnames])),
		(lgt_write_xml_open_tag(Stream, info, []),
		 lgt_write_xml_element(Stream, key, [], Key),
		 lgt_write_xml_cdata_element(Stream, value, [], Value),
		 lgt_write_xml_close_tag(Stream, info))),
	lgt_write_xml_close_tag(Stream, predicate).



lgt_write_xml_relations(Stream) :-
	lgt_write_xml_open_tag(Stream, relations, []),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_implements_protocol_(Entity, Ptc, Scope)),
	lgt_write_xml_relation(Stream, Entity, Ptc, implements, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_imports_category_(Entity, Ctg, Scope)),
	lgt_write_xml_relation(Stream, Entity, Ctg, imports, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_extends_object_(Entity, Parent, Scope)),
	lgt_write_xml_relation(Stream, Entity, Parent, extends, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_instantiates_class_(Entity, Class, Scope)),
	lgt_write_xml_relation(Stream, Entity, Class, instantiates, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_specializes_class_(Entity, Superclass, Scope)),
	lgt_write_xml_relation(Stream, Entity, Superclass, specializes, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_rclause_(lgt_extends_protocol_(Entity, Ptc, Scope)),
	lgt_write_xml_relation(Stream, Entity, Ptc, extends, Scope),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_entity_(_, Entity, _, _),
	lgt_uses_(Obj),
	lgt_write_xml_relation(Stream, Entity, Obj, uses),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_entity_(_, Entity, _, _),
	lgt_calls_(Ptc),
	lgt_write_xml_relation(Stream, Entity, Ptc, calls),
	fail.

lgt_write_xml_relations(Stream) :-
	lgt_write_xml_close_tag(Stream, relations).



lgt_write_xml_relation(Stream, Entity, Relation, Tag, Scope) :-
	lgt_relation_to_xml_name(Entity, Relation, Name),
	lgt_relation_to_xml_filename(Relation, File),
	lgt_write_xml_open_tag(Stream, Tag, []),
	lgt_write_xml_cdata_element(Stream, name, [], Name),
	lgt_write_xml_element(Stream, scope, [], Scope),
	lgt_write_xml_cdata_element(Stream, file, [], File),
	lgt_write_xml_close_tag(Stream, Tag).



lgt_write_xml_relation(Stream, Entity, Relation, Tag) :-
	lgt_relation_to_xml_name(Entity, Relation, Name),
	lgt_relation_to_xml_filename(Relation, File),
	lgt_write_xml_open_tag(Stream, Tag, []),
	lgt_write_xml_cdata_element(Stream, name, [], Name),
	lgt_write_xml_cdata_element(Stream, file, [], File),
	lgt_write_xml_close_tag(Stream, Tag).



% lgt_write_xml_open_tag(@stream, @atom, @list)
%
% writes <Tag Att1="V1" Att2="V2" ...>

lgt_write_xml_open_tag(Stream, Tag, Atts) :-
	write_term(Stream, '<', []),
	write_term(Stream, Tag, []),
	lgt_write_xml_tag_attributes(Stream, Atts),
	write_term(Stream, '>', []), nl(Stream).



% lgt_write_xml_element(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...>Text</Tag>

lgt_write_xml_element(Stream, Tag, Atts, Text) :-
	write_term(Stream, '<', []),
	write_term(Stream, Tag, []),
	lgt_write_xml_tag_attributes(Stream, Atts),
	write_term(Stream, '>', []),
	write_term(Stream, Text, []),
	write_term(Stream, '</', []),
	write_term(Stream, Tag, []),
	write_term(Stream, '>', []), nl(Stream).



% lgt_write_xml_cdata_element(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag>

lgt_write_xml_cdata_element(Stream, Tag, Atts, Text) :-
	write_term(Stream, '<', []),
	write_term(Stream, Tag, []),
	lgt_write_xml_tag_attributes(Stream, Atts),
	write_term(Stream, '><![CDATA[', []),
	write_term(Stream, Text, []),
	write_term(Stream, ']]></', []),
	write_term(Stream, Tag, []),
	write_term(Stream, '>', []), nl(Stream).



% lgt_write_xml_tag_attributes(@stream, @list)

lgt_write_xml_tag_attributes(_, []) :-
	!.

lgt_write_xml_tag_attributes(Stream, [Attribute-Value| Rest]) :-
	write_term(Stream, ' ', []),
	write_term(Stream, Attribute, []),
	write_term(Stream, '="', []),
	write_term(Stream, Value, []),
	write_term(Stream, '"', []),
	lgt_write_xml_tag_attributes(Stream, Rest).



% lgt_write_xml_close_tag(@stream, @atom)
%
% writes </Tag>

lgt_write_xml_close_tag(Stream, Tag) :-
	write_term(Stream, '</', []),
	write_term(Stream, Tag, []),
	write_term(Stream, '>', []),
	nl(Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO defined predicates
%
%  used in portability checking
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lgt_iso_def_pred(true).
lgt_iso_def_pred(fail).
lgt_iso_def_pred(call(_)).
lgt_iso_def_pred(!).
lgt_iso_def_pred((_; _)).
lgt_iso_def_pred((_, _)).
lgt_iso_def_pred((_ -> _)).
lgt_iso_def_pred((_ -> _; _)).
lgt_iso_def_pred(catch(_, _, _)).
lgt_iso_def_pred(throw(_)).

lgt_iso_def_pred((_ = _)).
lgt_iso_def_pred((_ \= _)).
lgt_iso_def_pred(unify_with_occurs_check(_, _)).

lgt_iso_def_pred(var(_)).
lgt_iso_def_pred(nonvar(_)).
lgt_iso_def_pred(atom(_)).
lgt_iso_def_pred(atomic(_)).
lgt_iso_def_pred(number(_)).
lgt_iso_def_pred(integer(_)).
lgt_iso_def_pred(float(_)).
lgt_iso_def_pred(compound(_)).

lgt_iso_def_pred((_ @=< _)).
lgt_iso_def_pred((_ @< _)).
lgt_iso_def_pred((_ @>= _)).
lgt_iso_def_pred((_ @> _)).
lgt_iso_def_pred((_ == _)).
lgt_iso_def_pred((_ \== _)).

lgt_iso_def_pred(functor(_, _, _)).
lgt_iso_def_pred(arg(_, _, _)).
lgt_iso_def_pred(_ =.. _).
lgt_iso_def_pred(copy_term(_, _)).

lgt_iso_def_pred(_ is _).

lgt_iso_def_pred((_ =< _)).
lgt_iso_def_pred((_ < _)).
lgt_iso_def_pred((_ >= _)).
lgt_iso_def_pred((_ > _)).
lgt_iso_def_pred((_ =:= _)).
lgt_iso_def_pred((_ =\= _)).

lgt_iso_def_pred(clause(_, _)).
lgt_iso_def_pred(current_predicate(_)).

lgt_iso_def_pred(asserta(_)).
lgt_iso_def_pred(assertz(_)).
lgt_iso_def_pred(retract(_)).
lgt_iso_def_pred(abolish(_)).

lgt_iso_def_pred(findall(_, _, _)).
lgt_iso_def_pred(bagof(_, _, _)).
lgt_iso_def_pred(setof(_, _, _)).

lgt_iso_def_pred(current_input(_)).
lgt_iso_def_pred(current_output(_)).
lgt_iso_def_pred(set_input(_)).
lgt_iso_def_pred(set_output(_)).
lgt_iso_def_pred(open(_, _, _, _)).
lgt_iso_def_pred(open(_, _, _)).
lgt_iso_def_pred(close(_, _)).
lgt_iso_def_pred(close(_)).
lgt_iso_def_pred(flush_output(_)).
lgt_iso_def_pred(flush_output).
lgt_iso_def_pred(stream_property(_, _)).
lgt_iso_def_pred(at_end_of_stream).
lgt_iso_def_pred(at_end_of_stream(_)).
lgt_iso_def_pred(set_stream_position(_, _)).

lgt_iso_def_pred(get_char(_, _)).
lgt_iso_def_pred(get_char(_)).
lgt_iso_def_pred(get_code(_, _)).
lgt_iso_def_pred(get_code(_)).
lgt_iso_def_pred(peek_char(_, _)).
lgt_iso_def_pred(peek_char(_)).
lgt_iso_def_pred(peek_code(_, _)).
lgt_iso_def_pred(peek_code(_)).
lgt_iso_def_pred(put_char(_, _)).
lgt_iso_def_pred(put_char(_)).
lgt_iso_def_pred(put_code(_, _)).
lgt_iso_def_pred(put_code(_)).
lgt_iso_def_pred(nl).
lgt_iso_def_pred(nl(_)).

lgt_iso_def_pred(get_byte(_, _)).
lgt_iso_def_pred(get_byte(_)).
lgt_iso_def_pred(peek_byte(_, _)).
lgt_iso_def_pred(peek_byte(_)).
lgt_iso_def_pred(put_byte(_, _)).
lgt_iso_def_pred(put_byte(_)).

lgt_iso_def_pred(read_term(_, _, _)).
lgt_iso_def_pred(read_term(_, _)).
lgt_iso_def_pred(read(_)).
lgt_iso_def_pred(read(_, _)).
lgt_iso_def_pred(write_term(_, _, _)).
lgt_iso_def_pred(write_term(_, _)).
lgt_iso_def_pred(write(_)).
lgt_iso_def_pred(write(_, _)).
lgt_iso_def_pred(writeq(_)).
lgt_iso_def_pred(writeq(_, _)).
lgt_iso_def_pred(write_canonical(_)).
lgt_iso_def_pred(write_canonical(_, _)).
lgt_iso_def_pred(op(_, _, _)).
lgt_iso_def_pred(current_op(_, _, _)).
lgt_iso_def_pred(char_conversion(_, _)).
lgt_iso_def_pred(current_char_conversion(_, _)).

lgt_iso_def_pred(\+ _).
lgt_iso_def_pred(once(_)).
lgt_iso_def_pred(repeat).

lgt_iso_def_pred(atom_length(_, _)).
lgt_iso_def_pred(atom_concat(_, _, _)).
lgt_iso_def_pred(sub_atom(_, _, _, _, _)).
lgt_iso_def_pred(atom_chars(_, _)).
lgt_iso_def_pred(atom_codes(_, _)).
lgt_iso_def_pred(char_code(_, _)).
lgt_iso_def_pred(number_chars(_, _)).
lgt_iso_def_pred(number_codes(_, _)).

lgt_iso_def_pred(set_prolog_flag(_, _)).
lgt_iso_def_pred(current_prolog_flag(_, _)).
lgt_iso_def_pred(halt).
lgt_iso_def_pred(halt(_)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk banner
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lgt_banner :-
	lgt_compiler_option(startup_message, on) ->
		logtalk_version(Major, Minor, Patch),
		write('Logtalk '), write(Major), write('.'), write(Minor), write('.'), write(Patch), nl,
		write('Copyright (c) 1998-2002 Paulo Moura'), nl
		;
		true.


:- initialization(lgt_banner).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
