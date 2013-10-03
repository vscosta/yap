
:- module(bdd, [bdd_new/2,
	bdd_new/3,
	bdd_from_list/3,
	mtbdd_new/2,
	mtbdd_new/3,
	bdd_eval/2, 
	mtbdd_eval/2, 
	bdd_tree/2,
	bdd_size/2,
	bdd_print/2,
	bdd_to_probability_sum_product/2,
	bdd_to_probability_sum_product/3,
	bdd_close/1,
	mtbdd_close/1]).

:- use_module(library(lists)).

:- use_module(library(maplist)).

:- use_module(library(rbtrees)).

:- use_module(library(simpbool)).

tell_warning :-
	print_message(warning,functionality(cudd)).

:- catch(load_foreign_files([cudd], [], init_cudd),_,fail) -> true ; tell_warning.


% create a new BDD from a tree.
bdd_new(T, Bdd) :-
	term_variables(T, Vars),
	bdd_new(T, Vars, Bdd).

bdd_new(T, Vars, cudd(M,X,VS,TrueVars)) :-
	term_variables(Vars, TrueVars),
	VS =.. [vs|TrueVars],
	findall(Manager-Cudd, set_bdd(T, VS, Manager, Cudd), [M-X]).

% create a new BDD from a list.
bdd_from_list(List, Vars, cudd(M,X,VS,TrueVars)) :-
	term_variables(Vars, TrueVars),
	VS =.. [vs|TrueVars],
	findall(Manager-Cudd, set_bdd_from_list(List, VS, Manager, Cudd), [M-X]).

set_bdd(T, VS, Manager, Cudd) :-
	numbervars(VS,0,_),
	( ground(T)
        ->
	  term_to_cudd(T,Manager,Cudd)
        ;
	  writeln(throw(error(instantiation_error,T)))
        ).

set_bdd_from_list(T0, VS, Manager, Cudd) :-
	numbervars(VS,0,_),
	generate_releases(T0, Manager, T),
%	T0 = T,
%	writeln_list(T0),
	list_to_cudd(T,Manager,_Cudd0,Cudd).

generate_releases(T0, Manager, T) :-
	rb_empty(RB0),	
	reverse(T0, [H|R]),
	add_releases(R, RB0, [H], Manager, T).

add_releases([], _, RR, _M,  RR).
add_releases([(X = Ts)|R], RB0, RR0, M, RR) :-
	term_variables(Ts, Vs), !,
	add_variables(Vs, RB0, RR0, M, RBF, RRI),
	add_releases(R, RBF, [(X=Ts)|RRI], M, RR).

add_variables([], RB, RR, _M, RB, RR).
add_variables([V|Vs], RB0, RR0, M, RBF, RRF) :-
	rb_lookup(V, _, RB0), !,
	add_variables(Vs, RB0, RR0, M, RBF, RRF).
add_variables([V|Vs], RB0, RR0, M, RBF, RRF) :-
	rb_insert(RB0, V, _, RB1),
	add_variables(Vs, RB1, [release_node(M,V)|RR0], M, RBF, RRF).


writeln_list([]).
writeln_list([B|Bindings]) :-
	writeln(B),
	writeln_list(Bindings).

%list_to_cudd(H._List,_Manager,_Cudd0,_CuddF) :- writeln(l:H), fail.
list_to_cudd([],_Manager,Cudd,Cudd) :- writeln('X').
list_to_cudd([release_node(M,cudd(V))|T], Manager, Cudd0, CuddF) :- !,
	write('-'), flush_output,
	cudd_release_node(M,V),
	list_to_cudd(T, Manager, Cudd0, CuddF).
list_to_cudd([(V=0*_Par)|T], Manager, _Cudd0, CuddF) :- !,
	write('0'), flush_output,
	term_to_cudd(0, Manager, Cudd),
	V = cudd(Cudd),
	list_to_cudd(T, Manager, Cudd, CuddF).
list_to_cudd([(V=0)|T], Manager, _Cudd0, CuddF) :- !,
	write('0'), flush_output,
	term_to_cudd(0, Manager, Cudd),
	V = cudd(Cudd),
	list_to_cudd(T, Manager, Cudd, CuddF).
list_to_cudd([(V=_Tree*0)|T], Manager, _Cudd0, CuddF) :- !,
	write('0'), flush_output,
	term_to_cudd(0, Manager, Cudd),
	V = cudd(Cudd),
	list_to_cudd(T, Manager, Cudd, CuddF).
list_to_cudd([(V=Tree*1)|T], Manager, _Cudd0, CuddF) :- !,
	write('.'), flush_output,
	term_to_cudd(Tree, Manager, Cudd),
	V = cudd(Cudd),
	list_to_cudd(T, Manager, Cudd, CuddF).
list_to_cudd([(V=Tree)|T], Manager, _Cudd0, CuddF) :-
	write('.'), flush_output,
	( ground(Tree) -> true ; throw(error(instantiation_error(Tree))) ),
	term_to_cudd(Tree, Manager, Cudd),
	V = cudd(Cudd),
	list_to_cudd(T, Manager, Cudd, CuddF).

mtbdd_new(T, Mtbdd) :-
	term_variables(T, Vars),
	mtbdd_new(T, Vars, Mtbdd).

mtbdd_new(T, Vars, add(M,X,VS,Vars)) :-
	VS =.. [vs|Vars],
	functor(VS,vs,Sz),
	findall(Manager-Cudd, (numbervars(VS,0,_),term_to_add(T,Sz,Manager,Cudd)), [M-X]).

bdd_eval(cudd(M, X, Vars, _), Val) :-
	cudd_eval(M, X, Vars, Val).
bdd_eval(add(M, X, Vars, _), Val) :-
	add_eval(M, X, Vars, Val).

mtbdd_eval(add(M,X, Vars, _), Val) :-
	add_eval(M, X, Vars, Val).

% get the BDD as a Prolog list from the CUDD C object
bdd_tree(cudd(M, X, Vars, _Vs), bdd(Dir, List, Vars)) :-
	cudd_to_term(M, X, Vars, Dir, List).
bdd_tree(add(M, X, Vars, _), mtbdd(Tree, Vars)) :-
	add_to_term(M, X, Vars, Tree).

bdd_to_probability_sum_product(cudd(M,X,_,Probs), Prob) :-
	cudd_to_probability_sum_product(M, X, Probs, Prob).

bdd_to_probability_sum_product(cudd(M,X,_,_Probs), Probs, Prob) :-
	cudd_to_probability_sum_product(M, X, Probs, Prob).

bdd_close(cudd(M,_,_Vars, _)) :-
	cudd_die(M).
bdd_close(add(M,_,_Vars, _)) :-
	cudd_die(M).

bdd_size(cudd(M,Top,_Vars, _), Sz) :-
	cudd_size(M,Top,Sz).
bdd_size(add(M,Top,_Vars, _), Sz) :-
	cudd_size(M,Top,Sz).

bdd_print(cudd(M,Top,_Vars, _), File) :-
	cudd_print(M, Top, File).
bdd_print(add(M,Top,_Vars, _), File) :-
	cudd_print(M, Top, File).

mtbdd_close(add(M,_,_Vars,_)) :-
	cudd_die(M).

/* algorithm to compute probabilitie in Prolog */
bdd_to_sp(bdd(Dir, Tree, _Vars, IVars), Binds, Prob) :-
	findall(P, sp(Dir, Tree, IVars, Binds, P), [Prob]).

sp(Dir, Tree, Vars, Vars, P) :-
	run_sp(Tree),
	fetch(Tree, Dir, P).	

run_sp([]).
run_sp(pp(P,X,L,R).Tree) :-
	run_sp(Tree),
	P is X*L+(1-X)*R.
run_sp(pn(P,X,L,R).Tree) :-
	run_sp(Tree),
	P is X*L+(1-X)*(1-R).

fetch(pp(P,_,_,_)._Tree, 1, P).
fetch(pp(P,_,_,_)._Tree, -1, N) :- N is 1-P.
fetch(pn(P,_,_,_)._Tree, 1, P).
fetch(pn(P,_,_,_)._Tree, -1, N) :- N is 1-P.

