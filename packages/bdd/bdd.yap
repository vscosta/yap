
:- module(bdd, [bdd_new/2,
	bdd_new/3,
	mtbdd_new/2,
	mtbdd_new/3,
	bdd_eval/2, 
	mtbdd_eval/2, 
	bdd_tree/2,
	bdd_to_probability_sum_product/2,
	bdd_close/1,
	mtbdd_close/1]).

tell_warning :-
	print_message(warning,functionality(cudd)).

:- catch(load_foreign_files([cudd], [], init_cudd),_,fail) -> true ; tell_warning.

bdd_new(T, Bdd) :-
	term_variables(T, Vars),
	bdd_new(T, Vars, Bdd).

bdd_new(T, Vars, cudd(M,X,VS,TrueVars)) :-
	term_variables(Vars, TrueVars),
	VS =.. [vs|TrueVars],
	findall(Manager-Cudd, set_bdd(T, VS, Manager, Cudd), [M-X]).

set_bdd(T, VS, Manager, Cudd) :-
	numbervars(VS,0,_),
	( ground(T)
        ->
	  term_to_cudd(T,Manager,Cudd)
        ;
	  writeln(throw(error(instantiation_error,T)))
        ).

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

bdd_tree(cudd(M, X, Vars, _), bdd(Dir, Tree, Vars)) :-
	cudd_to_term(M, X, Vars, Dir, Tree).
bdd_tree(add(M, X, Vars, _), mtbdd(Tree, Vars)) :-
	add_to_term(M, X, Vars, Tree).

mtbdd_tree(add(M,X,Vars, _), mtbdd(Dir, Tree, Vars)) :-
	add_to_term(M, X, Vars, Dir, Tree).

bdd_to_probability_sum_product(cudd(M,X,_,Probs), Prob) :-
	cudd_to_probability_sum_product(M, X, Probs, Prob).

bdd_close(cudd(M,_,_Vars, _)) :-
	cudd_die(M).
bdd_close(add(M,_,_Vars, _)) :-
	cudd_die(M).

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

