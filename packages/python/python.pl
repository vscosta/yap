% % % -* - Mode : Prolog; -*-
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

%%%

:- module(python,
	  [
	   init_python/0,
	   end_python/0,
	   python_command/1,
	   python_run_file/1,
	   python_run_command/1,
	   python_run_script/2,
	   python_assign/3,
	   python_import/1,
	   array_to_python_list/4,
	   array_to_python_tuple/4,
	   array_to_python_view/5,
	   python/2,
	   acquire_GIL/0,
	   release_GIL/0,
	   (:=)/2,
	   (:=)/1,
	%   (<-)/2,
	%   (<-)/1,
	   op(100,fy,$),
	   op(950,fy,:=),
	   op(950,yfx,:=),
	   op(950,fx,<-),
	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	   op(100, fy, '.')
	  ]).


/** <module> python

  A C-based  Prolog interface to python.

  @author               Vitor Santos Costa
  @version      0:0:5, 2012/10/8
  @license      Perl Artistic License

This is an interface to allow calling Python from Prolog. Please look
at the SWIG package if you want to embedd Prolog with Python.

The interface should be activated by consulting the python lybrary. It
immediately boots a Python image.

To best define the interface, one has to address two opposite goals:
    - make it as similar to python as possible
    - make all embedded language interfaces (python, R, Java) as
similar as possible.

   YAP supports the following translation between Prolog and Python:

| *Prolog*      | *Pyhon*       |          *Prolog Examples*             |
|:-------------:|:-------------:|---------------------------------------:|
| Numbers       | Numbers       | 2.3
|               |               | 1545
|               |               |
| Atom          | Symbols       | var
| $Atom         |               | $var [ = var]
| `string`      | 'string'      | \`hello\`
| "string"      |        '      | "hello"
|               |               |
| Atom(...)     | Symb(...)     | f( a, b, named=v)
| E.F(...)      | E.F (...)     | mod.f( a) [ = [mod\|f(a)] ]
| Atom()        |               | f() [ = '()'(f) ]
| Lists         | Lists         | [1,2,3]
| t(....)       | Tuples        | t(1,2,3) to (1,2,3)
| (..., ...)    |               | (1,2,3)[ = (1,(2,3))]
| {.=., .=.}    | Dict          | {\`one\`: 1, \`two\`: 2, \`three\`: 3}

*/



/************************************************************************************************************


Python interface

Data types are

     Python                Prolog
     string                atoms
     numbers		       numbers
     lists		           lists
     tuples                t(...)
     generic objs	        __pointer__(Address)

     $var refers to the attribute __main__.var

*************************************************************************************************************/

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- dynamic python_mref_cache/2, python_obj_cache/2.

:= (P1,P2) :- !,
	:= P1,
	:= P2.
:= import( F ) :- !, python_import(F).
:= F :- python(F,_).

V := F :- var(V), !, python(F,V0),
	python_exports(V0,V).
A := F :-
	python_eval_term(F, EF),
	assign(A, EF, _).

( V <- F ) :-
	var(V), !,
	V0 := F,
	python_exports(V0,V).
( V <- F ) :-
	ground(V), !,
	V := F.
( V <- F ) :-
	copy_term(V, V0),
	V0 := F,
	python_exports(V0,V).

python_exports(V0, V0) :-
	var(V0), !.
python_exports(V0, V0) :-
	atomic(V0), !.
python_exports('__obj__'(T0), T) :-
	!,
	python_export('__obj__'(T0), T).
python_exports(V0, VF) :-
	V0 =.. [F|L],
	maplist(python_exports, L, LF),
	VF =.. [F|LF].

((<- F)) :-
    python( F, _).

python_import(Module) :-
	python_do_import(Module, _).

python_do_import(Module, MRef) :-
	python_mref_cache(Module, MRef), !.
python_do_import(Module, MRef) :-
	python_import(Module, MRef),
	assert( python_mref_cache(Module, MRef) ).

fetch_module(M:E, _M1, E, MRef) :-
	atom(M),
	python_import(M, MRef).

% from an exp take an object, and its corresponding Prolog representation
descend_exp(V, _Obj) :-
	var(V), !,
	throw(error(instantiation_error,_)).
descend_exp(Mod.Exp, Obj) :-
	atom(Mod),
	python_import(Mod, MObj),
	!,
	descend_exp(MObj.Exp, Obj).
descend_exp(C1.C2.E, Obj) :- !,
	python_eval_term(C1, O1),
	python_field(O1, C2, Obj0 ),
	descend_exp(Obj0.E, Obj).
descend_exp(C1.E, Obj) :-
	!,
	python_eval_term(C1, O1),
	python_field(O1, E, Obj0 ),
	python_check_args(E, NE, Dict),
	python_apply(Obj0, NE, Dict, Obj).
descend_exp(C, O) :-
	python_is(C, O).

python_class(Obj) :-
	python_obj_cache(inspect:isclass(_), F),
	python_apply(F, isclass(Obj), {}, true).

process_obj(Obj, _, S, Obj, NS, Dict) :-
	python_callable(Obj), !,
	python_check_args(S, NS, Dict).
process_obj(Obj, _, S, FObj, NS, Dict) :-
	descend_object(Obj.'__init__', FObj, _, _),
	descend_object(Obj.'__init__', FObj, _, _),
	python_check_args(S, NS, Dict).

%% @pred python_eval_term( + Obj, -Obj) is semi-det
%  It implements the Python interprter's evaluation loop.
%
python_eval_term(Obj, Obj) :-
	var(Obj), !,
	throw(error(instantiation_error, Obj) ).
	%% atom use basic evaluation of an atom
	%% check if a variable.
python_eval_term(Name, Obj) :-
	atom(Name),
	!,
	python_is(Name, Obj).
	%% numbers are evaluated
python_eval_term(N, N) :- number(N), !.
python_eval_term(N, N) :- string(N), !.
%% we assume objects are so yoo.
python_eval_term('__obj__'(Obj), '__obj__'(Obj)) :- !.
%% constant functions are useful.
python_eval_term('()'(X), NX) :- !,
	python_eval_term(X, NX).
%% $ -> compatibilty with older versions
python_eval_term($Name, Obj) :- !,
	python_is(Name, Obj).
	%% lists are collections of individuals
	%% that may need futrher processing
python_eval_term([H|T], NL) :-
	is_list(T), !,
	maplist( python_eval_term, [H|T], NL).
	%% array access, Python understands numeric
	% indices and slices.
python_eval_term(Exp[Min:Max:Step], NEl) :- !,
	python_eval_term(slice(Min,Max,Step), Slice),
	python_slice(Exp,Slice, NEl).
python_eval_term(Exp[Min:Max], NEl) :- !,
	python_eval_term(slice(Min,Max), Slice),
	python_slide(Exp,Min,Max, NEl).
python_eval_term(Exp[Index], O) :- !,
	python_item(Exp,Index,O).
% function or method call of the form
% a.b.f(...)
python_eval_term(Inp.Exp, Obj) :- !,
	%flatten_exp(Exp, Exp1, []),
	descend_exp(Inp.Exp, Obj).
python_eval_term((A,B), Obj) :- !,
	flatten_conj((A,B),Cs,[]),
	maplist( python_eval_term, Cs, NCs),
	Tuple =.. [t|NCs],
	python_is(Tuple, Obj).
% tuples and varyadic functions.
python_eval_term(Tuple, Obj) :-
	Tuple =.. [Name|TupleL],
	maplist( python_eval_term, TupleL, NewTupleL),
	(
	 Name == t
	->
	 !,
	 NewTuple =.. [t|NewTupleL],
	 python_is(NewTuple, Obj)
	;
	 Name == open	->
	 !,
	 % calls the file constructor
	 NewTuple =.. [open|NewTupleL],
	 python_builtin_eval( NewTuple, [], Obj )
	).
python_eval_term(Exp, Obj) :-
	python_is(Exp, Obj).

flatten_exp( V , V, V0) :-
	V0 == [],
	var( V ),
	!.
flatten_exp( V1 ) -->
	{ var( V1 ) },
	!,
	[V1].
flatten_exp( (V1.V2) ) -->
	!,
	flatten_exp( V1 ),   % propagte the RHS first.
	flatten_exp( V2 ).
flatten_exp( V1() ) -->
	!,
	flatten_exp( V1 ).
flatten_exp( V1, V1, V0 ) :- V0 == [], !.
flatten_exp( V1 ) -->
	[V1].

flatten_conj( V1 ) -->
	{ var( V1 ) },
	!,
	[V1].
flatten_conj( (V1,V2) ) -->
	!,
	flatten_conj( V1 ),   % propagte the RHS first.
	flatten_conj( V2 ).
flatten_conj( V1 ) -->
	[V1].

python_check_args(_Exp(), t, {}) :-
	!.
python_check_args(Exp, t, {}) :-
	Exp =.. [_,V], var(V), !.
python_check_args(Exp, NExp, Dict) :-
	Exp =.. [_F|LArgs], !,
	match_args(LArgs, NLArgs, Dict),
	NExp =.. [t|NLArgs].
python_check_args(Exp, Exp, {}).

python_build_tuple(V) -->
	{var(V) }, !,
	[V].
python_build_tuple((X,Y)) --> !,
	python_build_tuple(X),
	python_build_tuple(Y).
python_build_tuple(X) --> [X].

% in case it is __init__ from __new__
splice_class(Ref, Ref, ArgNames, ArgNames) :- !.
splice_class(_FRef, _Ref, [_|ArgNames], ArgNames).

match_args([], [], {}).
match_args([V=A|LArgs], [], Dict) :- !,
	python_eval_term(A, EvA),
	match_named_args([V=EvA|LArgs], Map),
	map_to_dict(Map, Dict).
match_args([A|LArgs], [VA|NLArgs], Dict) :-
	python_eval_term(A, VA),
	match_args(LArgs, NLArgs, Dict).

match_named_args([], []).
match_named_args([K=A|LArgs], [K=A|Map]) :-
	match_named_args(LArgs, Map).


map_to_dict([X=V], {X:V}) :- !.
map_to_dict([X=V|Map], {X:V,NDict}) :-
	map_to_dict(Map, {NDict}).

match_from_anames([K|_ArgNames], K, VA, [_|Defaults], [VA|Defaults]) :- !.
match_from_anames([_|ArgNames], K, VA, [V|Defaults], [V|NDefaults]) :-
	match_from_anames(ArgNames, K, VA, Defaults, NDefaults).

fetch_args(FRef, Args, Kwd, Defaults) :-
	FRef = '__obj__'(_), !,
	%python_mref_cache('inspect', M),
	python_obj_cache(inspect:getargspec(_), F),
	python_apply(F, getargspec(FRef), {}, ExtraArgs),
	ExtraArgs=t(Args, _, Kwd, Defaults).
fetch_args(_, []).


python(Obj, Out) :-
	 python_eval_term(Obj, Out).

python_command(Cmd) :-
       python_run_command(Cmd).

python_lhs(Obj,Obj) :-
	var(Obj), !.
python_lhs(Name,Name) :-
	atom(Name), !.
python_lhs(N, N) :-
	number(N), !,
	throw(error(type(evaluable, N)), "in left-hand-side of s").
python_lhs(N,N) :-
	string(N), !,
	throw(error(type(evaluable, N)), "in left-hand-side of s").
python_lhs('__obj__'(Obj), '__obj__'(Obj)) :- !.
python_lhs($Name, Name) :-
	!.
python_lhs([H|T], NL) :-
	is_list(T), !,
	maplist( python_lhs, [H|T], NL).
python_lhs((Exp1,Exp2), O) :- !,
	python_build_tuple((Exp1,Exp2), TupleL, []),
	Tuple =.. [t|TupleL], % <<<
	python_lhs( Tuple, O).
python_lhs(F, F).

start_python :-
	python_main_module(MRef),
	assert(python_mref_cache('__main__', MRef)),
	python_command('import sys'),
	python_import('inspect'),
	python_mref_cache(inspect, InspRef),
	python_field(InspRef, isclass(_), IsClass),
	assert(python_obj_cache(inspect:isclass(_), IsClass)),
	python_field(InspRef, getargspec(_), GetArgSpec),
	assert(python_obj_cache(inspect:getargspec(_), GetArgSpec)),
	at_halt(end_python).

add_cwd_to_python :-
	unix(getcwd(Dir)),
	atom_concat(['sys.path.append(\"',Dir,'\")'], Command),
	python_command(Command),
	python_command("sys.argv = [\"yap\"]").
	% done

assign( V, E, O ) :-
	var(V),
	!,
	E = V,
	O = V.
assign( EName, E, EName ) :-
	\+ atomic(EName),
	python_assign_tuple(EName, E),
	!.
assign(Name, Exp, Name) :-
	python_assign(Name, Exp).

% from an exp take an object, and its corresponding Prolog representation
python_assign_field(V, _Obj) :-
	var(V), !,
	throw(error(instantiation_error,_)).
python_assign_field(Mod.Exp, Obj) :-
	atom(Mod),
	python_import(Mod, MObj),
	!,
	python_assign_field(MObj.Exp, Obj).
python_assign_field(C1.C2.E, Obj) :- !,
	python_eval_term(C1, O1),
	python_field(O1, C2, Obj0 ),
	python_assign_field(Obj0.E, Obj).
python_assign_field(Exp, Obj) :-
	fail,
	python_mref_cache(_, MObj),
	python_field(MObj, Exp, Obj), !.
python_assign_field(C1.E, Obj) :-
	atom(E),
	!,
	python_eval_term(C1, O1),
	python_assign_field(O1, E, Obj ).

%:- initialization( load_foreign_files([libYAPPython], [], init_python), now ).

:- initialization( load_foreign_library(foreign(libYAPPython), init_python), now ).
