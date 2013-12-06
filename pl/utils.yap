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
 * File:		utils.yap						 *
 * Last rev:	8/2/88							 *
 * mods:									 *
 * comments:	Some utility predicates available in yap		 *
 *									 *
 *************************************************************************/

 op(P,T,V) :-
	 '$check_op'(P,T,V,op(P,T,V)),
	 '$op'(P, T, V).

% just check the operator declarations for correctness.
'$check_op'(P,T,Op,G) :-
	( var(P) ; var(T); var(Op)), !,
	'$do_error'(instantiation_error,G).
'$check_op'(P,_,_,G) :-
	\+ integer(P), !,
	'$do_error'(type_error(integer,P),G).
'$check_op'(P,_,_,G) :-
	P < 0, !,
	'$do_error'(domain_error(operator_priority,P),G).
'$check_op'(P,_,_,G) :-
	P > 1200, !,
	'$do_error'(domain_error(operator_priority,P),G).
'$check_op'(_,T,_,G) :-
	\+ atom(T), !,
	'$do_error'(type_error(atom,P),G).
'$check_op'(_,T,_,G) :-
	\+  '$associativity'(T), !,
	'$do_error'(domain_error(operator_specifier,T),G).
'$check_op'(P,T,V,G) :-
	'$check_module_for_op'(V, G, NV),
	'$check_top_op'(P, T, NV, G).

'$check_top_op'(_, _, [], _).
'$check_top_op'(P, T, Op.NV, G) :- !,
	'$check_ops'(P, T, Op.NV, G).
'$check_top_op'(P, T, V, G) :-
	atom(V), !,
	'$check_op_name'(P, T, V, G).
'$check_top_op'(P, T, V, G) :-
	'$do_error'(type_error(atom,V),G).

 '$associativity'(xfx).
 '$associativity'(xfy).
 '$associativity'(yfx).
 '$associativity'(yfy).
 '$associativity'(xf).
 '$associativity'(yf).
 '$associativity'(fx).
 '$associativity'(fy).

'$check_module_for_op'(MOp, G, _) :-
	var(MOp), !,
	'$do_error'(instantiation_error,G).
'$check_module_for_op'(M:V, G, _) :-
	var(M), !,
	'$do_error'(instantiation_error,G).
'$check_module_for_op'(M:V, G, NV) :-
	atom(M), !,
	'$check_module_for_op'(V, G, NV).
'$check_module_for_op'(M:V, G, _) :- !,
	'$do_error'(type_error(atom,P),G).
'$check_module_for_op'(V, G, V).

'$check_ops'(P, T, [], G) :- !.
'$check_ops'(P, T, Op.NV, G) :- !,
	(
	 var(NV)
	->
	 '$do_error'(instantiation_error,G)
	;
	 '$check_module_for_op'(Op, G, NOp),
	 '$check_op_name'(P, T, NOp, G),
	 '$check_ops'(P, T, NV, G)
	).	
'$check_ops'(P, T, Ops, G) :-
	'$do_error'(type_error(list,Ops),G).

'$check_op_name'(_,_,V,G) :-
	  var(V), !,
	  '$do_error'(instantiation_error,G).
 '$check_op_name'(_,_,',',G) :- !,
	  '$do_error'(permission_error(modify,operator,','),G).
'$check_op_name'(_,_,'[]',G) :-  T \= yf, T\= xf, !,
	  '$do_error'(permission_error(create,operator,'[]'),G).
'$check_op_name'(_,_,'{}',G) :- T \= yf, T\= xf, !,
	  '$do_error'(permission_error(create,operator,'{}'),G).
'$check_op_name'(P,T,'|',G) :-
	 (
	  integer(P),
	  P < 1001, P > 0
	 ;
	  atom_codes(T,[_,_])
	 ), !,
	 '$do_error'(permission_error(create,operator,'|'),G).
'$check_op_name'(_,_,V,_) :-
	 atom(V), !.
'$check_op_name'(_,_,A,G) :-
	 '$do_error'(type_error(atom,A),G).

'$op'(P, T, ML) :-
	strip_module(ML, M, [A|As]), !,
	'$opl'(P, T, M, [A|As]).
'$op'(P, T, A) :-
	'$op2'(P,T,A).

'$opl'(P, T, _, []).
'$opl'(P, T, M, [A|As]) :-
	'$op2'(P, T, M:A),
	'$opl'(P, T, M, As).

'$op2'(P,T,A) :-
	atom(A), !,
	'$opdec'(P,T,A,prolog).
'$op2'(P,T,A) :-
	strip_module(A,M,N),
	'$opdec'(P,T,N,M).

current_op(X,Y,V) :- var(V), !,
	'$current_module'(M),
	'$do_current_op'(X,Y,V,M).
current_op(X,Y,M:Z) :- !,
	'$current_opm'(X,Y,Z,M).
current_op(X,Y,Z) :-
	'$current_module'(M),
	'$do_current_op'(X,Y,Z,M).


'$current_opm'(X,Y,Z,M) :-
	nonvar(Y),
	\+ '$associativity'(Y),
	'$do_error'(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
'$current_opm'(X,Y,Z,M) :-
	var(Z), !,
	'$do_current_op'(X,Y,Z,M).
'$current_opm'(X,Y,M:Z,_) :- !,
	'$current_opm'(X,Y,Z,M).
'$current_opm'(X,Y,Z,M) :-
	'$do_current_op'(X,Y,Z,M).

'$do_current_op'(X,Y,Z,M) :-
	nonvar(Y),
	\+ '$associativity'(Y),
	'$do_error'(domain_error(operator_specifier,Y),current_op(X,Y,M:Z)).
'$do_current_op'(X,Y,Z,M) :-
	atom(Z), !,
	'$current_atom_op'(Z, M1, Prefix, Infix, Posfix),
	( M1 = prolog -> true ; M1 = M ),
	(
	 '$get_prefix'(Prefix, X, Y)
	;
	 '$get_infix'(Infix, X, Y)
	;
	 '$get_posfix'(Posfix, X, Y)
	).
'$do_current_op'(X,Y,Z,M) :-
	'$current_op'(Z, M1, Prefix, Infix, Posfix),
	( M1 = prolog -> true ; M1 = M ),
	(
	 '$get_prefix'(Prefix, X, Y)
	;
	 '$get_infix'(Infix, X, Y)
	;
	 '$get_posfix'(Posfix, X, Y)
	).

'$get_prefix'(Prefix, X, Y) :-
	Prefix > 0,
	X is Prefix /\ 0xfff,
	(
         0x2000 /\ Prefix =:= 0x2000
        ->
         Y = fx
        ;
         Y = fy
        ).

'$get_infix'(Infix, X, Y) :-
	Infix > 0,
	X is Infix  /\ 0xfff,
	(
         0x3000 /\ Infix =:= 0x3000
        ->
         Y = xfx
	;
         0x1000 /\ Infix =:= 0x1000
        ->
         Y = xfy
        ;
         Y = yfx
        ).

'$get_posfix'(Posfix, X, Y) :-
	Posfix > 0,
	X is Posfix /\ 0xfff,
	(
         0x1000 /\ Posfix =:= 0x1000
        ->
         Y = xf
	;
         Y = yf
        ).


%%% Operating System utilities

cd :-
	cd('~').

cd(F) :-
      absolute_file_name(F, Dir, [file_type(directory),file_errors(fail),access(execute),expand(true)]),
      working_directory(_, Dir).

getcwd(Dir) :- working_directory(Dir, Dir).

ls :-
	getcwd(X),
	'$load_system_ls'(X,L),
	'$do_print_files'(L).

'$load_system_ls'(X,L) :-
	'$undefined'(directory_files(X, L), operating_system_support),
	load_files(library(system),[silent(true)]),
	fail.
'$load_system_ls'(X,L) :-
	operating_system_support:directory_files(X, L).
	

'$do_print_files'([]) :-
	nl.
'$do_print_files'([F| Fs]) :-
	'$do_print_file'(F),
	'$do_print_files'(Fs).

'$do_print_file'('.') :- !.
'$do_print_file'('..') :- !.
'$do_print_file'(F) :- atom_concat('.', _, F), !.
'$do_print_file'(F) :-
	write(F), write('  ').

pwd :-
	getcwd(X),
	write(X), nl.

unix(V) :- var(V), !,
	'$do_error'(instantiation_error,unix(V)).
unix(argv(L)) :- '$is_list_of_atoms'(L,L), !, '$argv'(L).
unix(argv(V)) :-
	'$do_error'(type_error(atomic,V),unix(argv(V))).
unix(cd) :- cd('~').
unix(cd(A)) :- cd(A).
unix(environ(X,Y)) :- '$do_environ'(X,Y).
unix(getcwd(X)) :- getcwd(X).
unix(shell(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(shell(V))).
unix(shell(A)) :- atom(A), !, '$shell'(A).
unix(shell(V)) :-
	'$do_error'(type_error(atomic,V),unix(shell(V))).
unix(system(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(system(V))).
unix(system(A)) :- atom(A), !, system(A).
unix(system(V)) :-
	'$do_error'(type_error(atom,V),unix(system(V))).
unix(shell) :- sh.
unix(putenv(X,Y)) :- '$putenv'(X,Y).


'$is_list_of_atoms'(V,_) :- var(V),!.
'$is_list_of_atoms'([],_) :- !.
'$is_list_of_atoms'([H|L],L0) :- !,
	'$check_if_head_may_be_atom'(H,L0),
	'$is_list_of_atoms'(L,L0).
'$is_list_of_atoms'(H,L0) :-
	'$do_error'(type_error(list,H),unix(argv(L0))).

'$check_if_head_may_be_atom'(H,_) :-
	var(H), !.
'$check_if_head_may_be_atom'(H,_) :-
	atom(H), !.
'$check_if_head_may_be_atom'(H,L0) :-
	'$do_error'(type_error(atom,H),unix(argv(L0))).


'$do_environ'(X, Y) :-
	var(X), !,
	'$do_error'(instantiation_error,unix(environ(X,Y))).
'$do_environ'(X, Y) :- atom(X), !,
	'$getenv'(X,Y).
'$do_environ'(X, Y) :-
	'$do_error'(type_error(atom,X),unix(environ(X,Y))).
	

putenv(Na,Val) :-
	'$putenv'(Na,Val).

getenv(Na,Val) :-
	'$getenv'(Na,Val).

setenv(Na,Val) :-
	'$putenv'(Na,Val).

prolog :-
	'$live'.

%%% current ....

recordaifnot(K,T,R) :-
	recorded(K,T,R), % force non-det binding to R.
	'$still_variant'(R,T),
	!,
	fail.
recordaifnot(K,T,R) :-
	recorda(K,T,R).

recordzifnot(K,T,R) :-
	recorded(K,T,R),
	'$still_variant'(R,T),
	!,
	fail.
recordzifnot(K,T,R) :-
	recordz(K,T,R).

current_atom(A) :-				% check
	atom(A), !.
current_atom(A) :-				% generate
	'$current_atom'(A).
current_atom(A) :-				% generate
	'$current_wide_atom'(A).

atom_concat(Xs,At) :-
	( var(At) ->
	   '$atom_concat'(Xs, At )
        ;
	 '$atom_concat_constraints'(Xs, start, At, Unbound),
	 '$process_atom_holes'(Unbound)
        ).

% the constraints are of the form hole: HoleAtom, Begin, Atom, End
'$atom_concat_constraints'([At], 0, At, _, []) :- !.
'$atom_concat_constraints'([At0], mid(Next, At), At, [hole(At0, Next, At, end)]) :-  !.
% just slice first atom
'$atom_concat_constraints'([At0|Xs], 0, At, Unbound) :-
	atom(At0), !,
	sub_atom(At, 0, Sz, L, At0 ),
	sub_atom(At, _, L, 0, Atr ), %remainder
	'$atom_concat_constraints'(Xs, 0, Atr, Unbound).
% first hole: Follow says whether we have two holes in a row, At1 will be our atom
'$atom_concat_constraints'([At0|Xs], start, At, [hole(At0, 0, At, Next)|Unbound]) :-
	 '$atom_concat_constraints'(Xs, mid(Next,At1), At, Unbound).
% end of a run
'$atom_concat_constraints'([At0|Xs], mid(end, At1), At, Unbound) :-
	atom(At0), !,
	sub_atom(At, Next, Sz, L, At0),
	sub_atom(At, 0, Next, Next, At1),
	sub_atom(At, _, L, 0, Atr), %remainder
	'$atom_concat_constraints'(Xs, 0, Atr, _, Unbound).
'$atom_concat_constraints'([At0|Xs], mid(Next,At1), At, Next, [hole(At0, Next, At, Follow)|Unbound]) :-
	 '$atom_concat_constraints'(Xs, mid(NextFollow, At1), At, Unbound).

'$process_atom_holes'([]).
'$process_atom_holes'([hole(At0, Next, At1, End)|Unbound]) :- End == end, !,
	sub_atom(At1, Next, _, 0, At0),
	 '$process_atom_holes'(Unbound).
'$process_atom_holes'([hole(At0, Next, At1, Follow)|Unbound]) :-
	sub_atom(At1, Next, Sz, _Left, At0),
	Follow is Next+Sz,
	 '$process_atom_holes'(Unbound).

	  
callable(A) :-
	( var(A) -> fail ; number(A) -> fail ; true ).

atomic_list_concat(L,At) :-
	atomic_concat(L, At).
	
atomic_list_concat(L, El, At) :-
	var(El), !,
	'$do_error'(instantiation_error,atom_list_concat(L,El,At)).
atomic_list_concat(L, El, At) :-
	nonvar(L), !,
	'$add_els'(L,El,LEl),
	atomic_concat(LEl, At).
atomic_list_concat(L, El, At) :-
	nonvar(At), !,
	atom_codes(At, S),
	atom_codes(El, [ElS]),
	'$split_elements'(S, ElS, SubS),
	'$atomify_list'(SubS, L).

'$add_els'([A,B|L],El,[A,El|NL]) :- !,
	'$add_els'([B|L],El,NL).
'$add_els'(L,_,L).
	
'$split_elements'(E.S, E, SubS) :- !,
	'$split_elements'(S, E, SubS).
'$split_elements'(E1.S, E, [E1|L].SubS) :- !,
	'$split_elements'(S, E, L, SubS).
'$split_elements'([], _, []).

'$split_elements'([], _, [], []).
'$split_elements'(E.S, E, [], SubS) :- !,
	'$split_elements'(S, E, SubS).
'$split_elements'(E1.S, E, E1.L, SubS) :-
	'$split_elements'(S, E, L, SubS).

'$atomify_list'([], []).
'$atomify_list'(S.SubS, A.L) :-
	atom_codes(A, S),
	'$atomify_list'(SubS, L).
	

%
% small compatibility hack

'$singletons_in_term'(T,VL) :-
	'$variables_in_term'(T,[],V10),
	'$sort'(V10, V1),
	'$non_singletons_in_term'(T,[],V20),
	'$sort'(V20, V2),	
	'$subtract_lists_of_variables'(V2,V1,VL).

'$subtract_lists_of_variables'([],VL,VL).
'$subtract_lists_of_variables'([_|_],[],[]) :- !.
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],VL) :-
	V1 == V2, !,
	'$subtract_lists_of_variables'(VL1,VL2,VL).
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],[V2|VL]) :-
	'$subtract_lists_of_variables'([V1|VL1],VL2,VL).

simple(V) :- var(V), !.
simple(A) :- atom(A), !.
simple(N) :- number(N).

callable(V) :- var(V), !, fail.
callable(V) :- atom(V), !.
callable(V) :- functor(V,_,Ar), Ar > 0.

nth_instance(Key,Index,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,_,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,Ref) :-
	'$nth_instance'(Key,Index,Ref).

nth_instance(Key,Index,T,Ref) :-
	nonvar(Key), var(Index), var(Ref), !,
	recorded(Key,T,Ref),
	'$nth_instance'(_,Index,Ref).
nth_instance(Key,Index,T,Ref) :-
	'$nth_instance'(Key,Index,Ref),
	instance(Ref,T).

nb_current(GlobalVariable, Val) :-
	'$nb_current'(GlobalVariable),
	'$nb_getval'(GlobalVariable, Val, _).

'$getval_exception'(GlobalVariable, Val, Caller) :-
	user:exception(undefined_global_variable, GlobalVariable, Action),
	!,
	(
	 Action == fail
	->
	 fail
	;
	 Action == retry
	->
	 b_getval(GlobalVariable, Val)
	;
	 Action == error
	->
	 '$do_error'(existence_error(variable, GlobalVariable),Caller)
	;
	 '$do_error'(type_error(atom, Action),Caller)
	).


subsumes_term(A,B) :-
	\+ \+ terms:subsumes(A,B).


