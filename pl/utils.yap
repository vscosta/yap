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

'$check_op'(P,T,V,G) :-
	(
	 var(P) ->
	 '$do_error'(instantiation_error,G)
	;
	 var(T) ->
	 '$do_error'(instantiation_error,G)
	;
	 var(V) ->
	 '$do_error'(instantiation_error,G)
	;
	 \+ integer(P) ->
	 '$do_error'(type_error(integer,P),G)
	;
	 \+ atom(T) ->
	 '$do_error'(type_error(atom,T),G)
	;
	 P < 0 ->
	 '$do_error'(domain_error(out_of_range,P),G)
	;
	 P > 1200 ->
	 '$do_error'(domain_error(out_of_range,P),G)
	;
	 \+ '$associativity'(T) ->
	 '$do_error'(domain_error(operator_specifier,T),G)
	;
	 '$check_op_name'(V,G)
	).

'$associativity'(xfx).
'$associativity'(xfy).
'$associativity'(yfx).
'$associativity'(yfy).
'$associativity'(xf).
'$associativity'(yf).
'$associativity'(fx).
'$associativity'(fy).

'$check_op_name'(V,G) :-
	 var(V), !,
	 '$do_error'(instantiation_error,G).
'$check_op_name'(V,_) :-
	 atom(V), !.
'$check_op_name'(M:A, G) :-
	 (
	  var(M) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  var(A) ->
	 '$do_error'(instantiation_error,G)
	 ;
	  atom(M) ->
	  '$check_op_name'(A, G)
	 ;
	 '$do_error'(instantiation_error,G)
	 ).
 '$check_op_name'([A|As], G) :-
	  '$check_op_name'(A, G),
	  '$check_op_names'(As, G).

'$check_op_names'([], _).
'$check_op_names'([A|As], G) :-
	'$check_op_name'(A, G),
	'$check_op_names'(As, G).

	  
'$op'(P, T, M:[A|As]) :- !,
	'$current_module'(M),
	'$opl'(P, T, M, [A|As]).
'$op'(P, T, [A|As]) :- !,
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
	var(Z), !,
	'$do_current_op'(X,Y,Z,M).
'$current_opm'(X,Y,M:Z,_) :- !,
	'$current_opm'(X,Y,Z,M).
'$current_opm'(X,Y,Z,M) :-
	'$do_current_op'(X,Y,Z,M).

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

unix(V) :- var(V), !,
	'$do_error'(instantiation_error,unix(V)).
unix(argv(L)) :- '$is_list_of_atoms'(L,L), !, '$argv'(L).
unix(argv(V)) :-
	'$do_error'(type_error(atomic,V),unix(argv(V))).
unix(cd) :- cd('~').
unix(cd(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(cd(V))).
unix(cd(A)) :- atomic(A), !, cd(A).
unix(cd(V)) :-
	'$do_error'(type_error(atomic,V),unix(cd(V))).
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

%%% Saving and restoring a computation

save(A) :- var(A), !,
	'$do_error'(instantiation_error,save(A)).
save(A) :- atom(A), !, name(A,S), '$save'(S).
save(S) :- '$save'(S).

save(A,_) :- var(A), !,
	'$do_error'(instantiation_error,save(A)).
save(A,OUT) :- atom(A), !, name(A,S), '$save'(S,OUT).
save(S,OUT) :- '$save'(S,OUT).

save_program(A) :- var(A), !,
	'$do_error'(instantiation_error,save_program(A)).
save_program(A) :- atom(A), !, name(A,S), '$save_program'(S).
save_program(S) :- '$save_program'(S).

save_program(A, G) :- var(A), !,
	'$do_error'(instantiation_error,save_program(A,G)).
save_program(A, G) :- var(G), !,
	'$do_error'(instantiation_error,save_program(A,G)).
save_program(A, G) :- \+ callable(G), !,
	'$do_error'(type_error(callable,G),save_program(A,G)).
save_program(A, G) :-
	( atom(A) -> name(A,S) ; A = S),
	recorda('$restore_goal',G,R),
	'$save_program'(S),
	erase(R),
	fail.
save_program(_,_).

restore(A) :- var(A), !,
	'$do_error'(instantiation_error,restore(A)).
restore(A) :- atom(A), !, name(A,S), '$restore'(S).
restore(S) :- '$restore'(S).

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

atom_concat(X,Y,At) :-
	(
	  nonvar(X),  nonvar(Y)
	->
	  atom_concat([X,Y],At)
	;
	  atom(At) ->
	  '$atom_contact_split'(At,X,Y)
	;
	  var(At) ->
	  '$do_error'(instantiation_error,atom_concat(X,Y,At))
	;
	  '$do_error'(type_error(atom,At),atomic_concant(X,Y,At))
	).

'$atom_contact_split'(At,X,Y) :-
	nonvar(X), !,
	atom_codes(At, Codes),
	atom_codes(X, Xs),
	lists:append(Xs,Ys,Codes),
	atom_codes(Y, Ys).
'$atom_contact_split'(At,X,Y) :-
	nonvar(Y), !,
	atom_codes(At, Codes),
	atom_codes(Y, Ys),
	once(lists:append(Xs,Ys,Codes)),
	atom_codes(Y, Ys).
'$atom_contact_split'(At,X,Y) :-
	atom_codes(At, Codes),
	'$split_codes'(Codes, Xs, Ys),
	atom_codes(X, Xs),
	atom_codes(Y, Ys).


atomic_list_concat(L,At) :-
	atomic_concat(L, At).
	
atomic_concat(X,Y,At) :-
	(
	  nonvar(X),  nonvar(Y)
	->
	  atomic_concat([X,Y],At)
	;
	  atom(At) ->
	  atom_length(At,Len),
	  '$atom_contact_split'(At,0,Len,X,Y)
	;
	  number(At) ->
	  '$number_contact_split'(At,X,Y)
	;
	  var(At) ->
	  '$do_error'(instantiation_error,atomic_concat(X,Y,At))
	;
	  '$do_error'(type_error(atomic,At),atomic_concant(X,Y,At))
	).

'$number_contact_split'(At,X,Y) :-
	nonvar(X), !,
	number_codes(At, Codes),
	name(X, Xs),
	lists:append(Xs,Ys,Codes),
	name(Y, Ys).
'$number_contact_split'(At,X,Y) :-
	nonvar(Y), !,
	number_codes(At, Codes),
	name(Y, Ys),
	once(lists:append(Xs,Ys,Codes)),
	name(Y, Ys).
'$number_contact_split'(At,X,Y) :-
	number_codes(At, Codes),
	'$split_codes'(Codes, Xs, Ys),
	name(X, Xs),
	name(Y, Ys).

sub_atom(At, Bef, Size, After, SubAt) :-
	% extract something from an atom
	atom(At), integer(Bef), integer(Size), !,
	'$sub_atom_extract'(At, Bef, Size, After, SubAt).
sub_atom(At, Bef, Size, After, SubAt) :-
	% extract subatom from an atom
	atom(At), atom(SubAt), !,
	'$do_sub_atom_fetch'(At, Bef, Size, After, SubAt, 0).
sub_atom(At, Bef, Size, After, SubAt) :-
	atom(At), !,
	atom_codes(At, Atl),
	'$sub_atom2'(Bef, Atl, Size, After, SubAt, sub_atom(At, Bef, Size, After, SubAt)).
sub_atom(At, Bef, Size, After, SubAt) :-
	var(At), !,
	'$do_error'(instantiation_error,sub_atom(At, Bef, Size,After, SubAt)).
sub_atom(At, Bef, Size, After, SubAt) :-
	\+ atom(At), !,
	'$do_error'(type_error(atom,At),sub_atom(At, Bef, Size,After, SubAt)).


'$do_sub_atom_fetch'(At, Bef, Size, After, SubAt, I0) :-
	'$sub_atom_fetch'(At, Bef1, Size, After1, SubAt, I0),
	(
	  Bef = Bef1, After = After1
	;
	  Next is Bef1+1,
	  '$do_sub_atom_fetch'(At, Bef, Size, After, SubAt, Next)
	).
	    
	

'$sub_atom2'(Bef, Atl, Size, After, SubAt, ErrorTerm) :-
	var(Bef), !,
	'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm).
'$sub_atom2'(Bef, Atl, Size, After, SubAt, ErrorTerm) :-
	'$sub_atom_get_subchars'(Bef, Atl, NewAtl),
	'$sub_atom3'(Size, After, SubAt, NewAtl, ErrorTerm).

% if SubAt is bound, the rest is deterministic.
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(SubAt), !,
	'$sub_atom_needs_atom'(SubAt,ErrorTerm),
	'$sub_atom_needs_int'(Size,ErrorTerm),
	'$sub_atom_needs_int'(After,ErrorTerm),
	atom_codes(SubAt,Atls),
	'$$_length1'(Atls, 0, Size),
	'$sub_atom_get_subchars_and_match'(Size, Atl, Atls, NAtl),
	'$$_length1'(NAtl,0,After).
% SubAt is unbound, but Size is bound
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(Size), !,
	'$sub_atom_needs_int'(Size,ErrorTerm),
	'$sub_atom_needs_int'(After,ErrorTerm),
	'$sub_atom_get_subchars_and_match'(Size, Atl, SubAts, NAtl),
	'$$_length1'(NAtl,0,After),
	atom_codes(SubAt,SubAts).
% SubAt and Size are unbound, but After is bound.
'$sub_atom3'(Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(After), !,
	'$sub_atom_needs_int'(After,ErrorTerm),
	'$sub_atom_get_last_subchars'(Atl,SubAts,After,Total,Size),
	Total >= After,
	atom_codes(SubAt,SubAts).
% SubAt, Size, and After are unbound.
'$sub_atom3'(Size, After, SubAt, Atl, _) :-
	'$$_length1'(Atl,0,Len),
	'$sub_atom_split'(Atl,Len,SubAts,Size,_,After),
	atom_codes(SubAt,SubAts).

% Bef is unbound, so we've got three hypothesis
% ok: in the best case we just try to find SubAt in  the original atom.
'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(SubAt), !,
	'$sub_atom_needs_atom'(SubAt, ErrorTerm),
	atom_codes(SubAt,SubAts),
	'$sub_atom_search'(SubAts, Atl, 0, Bef, AfterS),
	'$$_length1'(SubAts, 0, Size),
	'$$_length1'(AfterS, 0, After).
% ok: in the second best case we just get rid of the tail
'$sub_atombv'(Bef, Size, After, SubAt, Atl, ErrorTerm) :-
	nonvar(After), !,
	'$sub_atom_needs_int'(After, ErrorTerm),
	'$sub_atom_get_last_subchars'(Atl,SubAt0,After,Total,Size0),
	Total >= After,
	'$sub_atom_split'(SubAt0,Size0,_,Bef,SubAts,Size),
	atom_codes(SubAt,SubAts).
% ok: just do everything
'$sub_atombv'(Bef, Size, After, SubAt, Atl, _) :-
	'$$_length1'(Atl, 0, Len),
	'$sub_atom_split'(Atl,Len,_,Bef,Atls2,Len2),
	'$sub_atom_split'(Atls2,Len2,SubAts,Size,_,After),
	atom_codes(SubAt,SubAts).

'$sub_atom_search'([], AfterS, BefSize, BefSize, AfterS).
'$sub_atom_search'([C|SubAts], [C|Atl], BefSize, BefSize, AfterS) :-
	'$sub_atom_search2'(SubAts, Atl, AfterS).
'$sub_atom_search'([C|SubAts], [_|Atl], BefSize, BefSizeF, AfterS) :-
	NBefSize is BefSize+1,
	'$sub_atom_search'([C|SubAts], Atl, NBefSize, BefSizeF, AfterS).

'$sub_atom_search2'([], AfterS, AfterS).
'$sub_atom_search2'([C|SubAts], [C|Atl], AfterS) :-
	'$sub_atom_search2'(SubAts, Atl, AfterS).

'$sub_atom_get_subchars'(0, Atl, Atl) :- !.
'$sub_atom_get_subchars'(I0, [_|Atl], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars'(I, Atl, NAtl).

'$sub_atom_get_subchars'(0, Atl, [], Atl) :- !.
'$sub_atom_get_subchars'(I0, [C|Atl], [C|L], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars'(I, Atl, L, NAtl).

'$sub_atom_get_subchars_and_match'(0, Atl, [], Atl) :- !.
'$sub_atom_get_subchars_and_match'(I0, [C|Atl], [C|Match], NAtl) :-
	I is I0-1,
	'$sub_atom_get_subchars_and_match'(I, Atl, Match, NAtl).

'$sub_atom_check_length'([],0).
'$sub_atom_check_length'([_|L],N1) :-
	N1 > 0,
	N is N1-1,
	'$sub_atom_check_length'(L,N).	

'$sub_atom_get_last_subchars'([],[],_,0,0).
'$sub_atom_get_last_subchars'([C|Atl],SubAt,After,Total,Size) :-
	'$sub_atom_get_last_subchars'(Atl,SubAt0,After,Total0,Size0),
	Total is Total0+1,
	( Total > After ->
	    Size is Size0+1, SubAt = [C|SubAt0]
	 ;
	    Size = Size0, SubAt = SubAt0
	).

'$sub_atom_split'(Atl,After,[],0,Atl,After).
'$sub_atom_split'([C|Atl],Len,[C|Atls],Size,NAtl,After) :-
	Len1 is Len-1,
	'$sub_atom_split'(Atl,Len1,Atls,Size0,NAtl,After),
	Size is Size0+1.
	
'$sub_atom_needs_int'(V,_) :- var(V), !.
'$sub_atom_needs_int'(I,_) :- integer(I), I >= 0, !.
'$sub_atom_needs_int'(I,ErrorTerm) :- integer(I), !,
	'$do_error'(domain_error(not_less_than_zero,I),ErrorTerm).
'$sub_atom_needs_int'(I,ErrorTerm) :-
	'$do_error'(type_error(integer,I),ErrorTerm).

'$sub_atom_needs_atom'(V,_) :- var(V), !.
'$sub_atom_needs_atom'(A,_) :- atom(A), !.
'$sub_atom_needs_atom'(A,ErrorTerm) :-
	'$do_error'(type_error(atom,A),ErrorTerm).

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
	var(GlobalVariable), !,
	'$nb_current'(GlobalVariable),
	nb_getval(GlobalVariable, Val).
nb_current(GlobalVariable, Val) :-
	nb_getval(GlobalVariable, Val).

