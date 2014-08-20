%%% -*- Mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Author:        Vitor Santos Costa
%    E-mail:        vsc@dcc.fc.up.pt
%    Copyright (C): Universidade do Porto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of the YAP Python Interface
%  distributed according to Perl Artistic License
%  check LICENSE file for distribution license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** <module> python

A C-based  Prolog interface to python.

@author		Vitor Santos Costa
@version	0:0:5, 2012/10/8
@license	Perl Artistic License

*/

%%%

:- module(python, [
     init_python/0,
     end_python/0,
     python_command/1,
     python_assign/3,
     python_import/1,
     python/2,
     op(100,fy,$),
     op(950,fy,:=),
     op(950,yfx,:=),
     (:=)/2,
     (:=)/1
     ]).


/************************************************************************************************************

Python interface

Data types are

     Python		Prolog
     string             atoms
     numbers		numbers
     lists		lists
     tuples             t(...)
     generic objs	__pointer__(Address)

     $var refers to the attribute __main__.var

*************************************************************************************************************/

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- dynamic python_mref_cache/2, python_obj_cache/2.

:= F :- python(F,_).

V := F :- var(V), !, python(F,V).
A := F :- python(F, F1), python_assign(A, F1).

python_import(Module) :-
	python_do_import(Module, _).

python_do_import(Module, MRef) :-
	python_mref_cache(Module, MRef), !.
python_do_import(Module, MRef) :-
        python_import(Module, MRef),
	assert( python_mref_cache(Module, MRef) ).

fetch_module(M:E, M1, E1, MRef) :-
	atom(M),
	python_import(M, MRef0),
	module_extend(M, E, M1, E1, MRef0, MRef).

%
% extend the module as much as we can.
%
module_extend(M0, M:E, MF, EF, MRef0, MRef) :-
	atom(M),
	atom_concat([M0,'.',M], MM),
	python_import(MM, MRef1), !,
	module_extend(MM, E, MF, EF, MRef1, MRef).
module_extend(M, E, M, E, MRef, MRef).

object_prefix('__obj__'(_)).
object_prefix('$'(_)).
object_prefix('__obj__'(_):_).
object_prefix('$'(_):_).

% from an exp take an object, and its corresponding Prolog representation
descend_exp(V, _Obj, _F, _S) :-
	var(V), !,
	throw(error(instantiation_error,_)).
descend_exp(Exp, Obj, F, S) :-
	object_prefix(Exp),
	!,
	python_field(Exp, Obj, F, S).
descend_exp(Exp, Obj, F, S) :-
	python_mref_cache(_, MObj),
	python_field(MObj:Exp, Obj, F, S), !.
descend_exp(Mod:Exp, Obj, F, S) :-
	atom(Mod), 
	python_import(Mod, MObj),
	python_field(MObj:Exp, Obj, F, S), !.

python_class(Obj) :-
	python_obj_cache(inspect:isclass(_), F),
	python_apply(F, isclass(Obj), {}, true).

process_obj(Obj, _, S, Obj, NS, Dict) :-
	python_callable(Obj), !,
	python_check_args(S, NS, Dict).
process_obj(Obj, _, S, Obj, NS, Dict) :-
	python_class(Obj),
	descend_object(Obj:'__init__', FObj, _, _),
	python_check_args(S, NS, Dict).

python_eval_term(Obj, Obj) :-
	var(Obj), !.
python_eval_term('__obj__'(Obj), '__obj__'(Obj)) :- !.
python_eval_term($Name, Obj) :- !,
	python_is($Name, Obj).
python_eval_term([H|T], [NH|NT]) :- !,
	python_eval_term(H, NH),
	python_eval_term(T, NT).
python_eval_term(N, N) :- atomic(N), !.
python_eval_term(Exp, O) :-
	descend_exp(Exp, Obj, Old, S), !,
	(functor(S, _, 0) -> 
	   O = Obj
        ; 
	   python_check_args(S, NS, Dict),
	   python_apply(Obj, NS, Dict, O)
        ).
python_eval_term(S, O) :-
	python_check_args(S, NS, {}),
	python_is(NS, O).	

python_check_args(Exp, t, {}) :-
	Exp =.. [_,V], var(V), !.
python_check_args(Exp, NExp, Dict) :-
	functor(Exp, _, Arity),
	arg(Arity, Exp, A), nonvar(A), A = (_=_), !,
	Exp =.. [_F|LArgs],
	match_args(LArgs, NLArgs, Dict),
	NExp =.. [t|NLArgs].
python_check_args(Exp, NExp, {}) :-
	Exp =.. [F|L],
	maplist(python_eval_term, L, LF),
	NExp =.. [F|LF].

% in case it is __init__ from __new__
splice_class(Ref, Ref, ArgNames, ArgNames) :- !.
splice_class(_FRef, _Ref, [_|ArgNames], ArgNames).

match_args([], [], {}).
match_args([V=A|LArgs], [], Dict) :- !,
	match_named_args([V=A|LArgs], Map),
	map_to_dict(Map, Dict).
match_args([A|LArgs], [VA|NLArgs], Dict) :-
	python_eval_term(A, VA),
	match_args(LArgs, NLArgs, Dict).

match_named_args([], []).
match_named_args([K=A|LArgs], [K=VA|Map]) :-
	python_eval_term(A, VA),
	match_named_args(LArgs, Map).


map_to_dict([X=V], {X:V}) :- !.
map_to_dict([X=V|Map], {X:V,NDict}) :-
	map_to_dict(Map, {NDict}).

match_from_anames([K|_ArgNames], K, VA, [_|Defaults], [VA|Defaults]) :- !.
match_from_anames([_|ArgNames], K, VA, [V|Defaults], [V|NDefaults]) :-
	match_from_anames(ArgNames, K, VA, Defaults, NDefaults).

fetch_args(FRef, Args, Kwd, Defaults) :-
	FRef = '__obj__'(_), !,
	python_mref_cache('inspect', M),
	python_obj_cache(inspect:getargspec(_), F),
	python_apply(F, getargspec(FRef), {}, ExtraArgs),
	ExtraArgs=t(Args, _, Kwd, Defaults).
fetch_args(_, []).


python(Obj, Out) :-
	 python_eval_term(Obj, Out).

python_command(Cmd) :-
       python_run_command(Cmd).

start_python :-
	init_python,
	python_main_module(MRef),
	assert(python_mref_cache('__main__', MRef)),
	python_command('import sys'),
	python_import('inspect'),
	python_mref_cache(inspect, InspRef),
	python_field(InspRef:isclass(_), IsClass, _, _),
	assert(python_obj_cache(inspect:isclass(_), IsClass)),
	python_field(InspRef:getargspec(_), GetArgSpec, _, _),
	assert(python_obj_cache(inspect:getargspec(_), GetArgSpec)),
	at_halt(end_python).

add_cwd_to_python :-
	unix(getcwd(Dir)),
	atom_concat(['sys.path.append(\"',Dir,'\")'], Command),
	python_command(Command),
	python_command("sys.argv = [\"yap\"]").
	% done	

python_assign(Name, Exp, '$'(Name)) :-
	python_assign(Name, Exp).

:- initialization( use_foreign_library(foreign(python)), now ).

:- initialization(start_python, now).

:- initialization(add_cwd_to_python).

