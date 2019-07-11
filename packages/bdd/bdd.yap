

/**
 *   @file bdd/bdd.yap
 *   
 *   @defgroup BDDsPL Binary Decision Diagrams and Friends
 * @ingroup BDDs
 * @{
 * 
 * 
 * This library provides an interface to the BDD package CUDD. It requires
 * CUDD compiled as a dynamic library. In Linux this is available out of
 * box in Fedora, but can easily be ported to other Linux
 * distributions. CUDD is available in the ports OSX package, and in
 * cygwin. To use it, call
 * 
 * ~~~~~
 * :-use_module(library(bdd))`.
 * ~~~~~
 * 
 * The following predicates construct a BDD:
 * 
 * \toc
 */


:- module(bdd, [
	bdd_new/2,
	bdd_new/3,
	bdd_from_list/3,
	mtbdd_new/2,
	mtbdd_new/3,
	bdd_eval/2,
	mtbdd_eval/2,
	bdd_tree/2,
	bdd_size/2,
		bdd_print/2,
                bdd_print/3,
		bdd_to_probability_sum_product/2,
		bdd_to_probability_sum_product/3,
		bdd_reorder/2,
		bdd_close/1,
	mtbdd_close/1]).

:- use_module(library(lists)).

:- use_module(library(maplist)).

:- use_module(library(rbtrees)).

:- use_module(library(simpbool)).

tell_warning :-
	print_message(warning,functionality(cudd)).

:- catch(load_foreign_files([libcudd], [], init_cudd),_,fail) -> true ; tell_warning.

/**
@pred bdd_new(? _Exp_, - _BddHandle_)

create a new BDD from the logical expression  _Exp_. The expression
may include:

+ Logical Variables:

  a leaf-node can be a logical variable.

+ `0` and `1`

    a leaf-node can also be bound to the two boolean constants.

+ `or( _X_,  _Y_)`,  `_X_ \/  _Y_`,  `_X_ +  _Y_`

    disjunction

+ `and( _X_,  _Y_)`,  `_X_ /\  _Y_`,  `_X_ *  _Y_`

    conjunction

+ `nand( _X_,  _Y_)`

    negated conjunction

+ `nor( _X_,  _Y_)`

    negated disjunction

+ `xor( _X_,  _Y_)`

    exclusive or

+ `not( _X_)`, or `-_X_`

    negation.

*/
bdd_new(T, Bdd) :-
	term_variables(T, Vars),
	bdd_new(T, Vars, Bdd).

/**
@pred bdd_new(? _Exp_, +_Vars_, - _BddHandle_)

Same as bdd_new/2, but receives a term of the form
`vs(V1,....,Vn)`. This allows incremental construction of BDDs.

*/
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

/** @pred bdd_from_list(? _List_, ?_Vars_, - _BddHandle_)

Convert a _List_ of logical expressions of the form above, that
includes the set of free variables _Vars_, into a BDD accessible
through _BddHandle_.
*/
% create a new BDD from a list.
bdd_from_list(List, Vars, cudd(M,X,VS,TrueVars)) :-
	term_variables(Vars, TrueVars),
	VS =.. [vs|TrueVars],
	findall(Manager-Cudd, set_bdd_from_list(List, VS, Manager, Cudd), [M-X]).

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

/**
 * @pred list_to_cudd(+ _ListOfEquivalences, Manager, )Initial_ 
 */
list_to_cudd([],_Manager,Cudd,Cudd) :-
    writeln('X').
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

/**
 * @pred mtbdd_new(? _Exp_, - _BddHandle_)
 * 
 * create a new algebraic decision diagram (ADD) from the logical
 * expression  _Exp_. The expression may include:
 * 
 * + Logical Variables:
 * a leaf-node can be a logical variable, or <em>parameter</em>.
 * + Number
 * a leaf-node can also be any number
 * + _X_ \*  _Y_
 * product
 * + _X_ +  _Y_
 * sum
 * + _X_ -  _Y_
 * subtraction
 * + or( _X_,  _Y_),  _X_ \/  _Y_
 * logical or
 */

mtbdd_new(T, Mtbdd) :-
	term_variables(T, Vars),
	mtbdd_new(T, Vars, Mtbdd).

mtbdd_new(T, Vars, add(M,X,VS,Vars)) :-
	VS =.. [vs|Vars],
	functor(VS,vs,Sz),
	findall(Manager-Cudd, (numbervars(VS,0,_),term_to_add(T,Sz,Manager,Cudd)), [M-X]).

/** @pred bdd_eval(+ _BDDHandle_,  _Val_)

Unify  _Val_ with the value of the logical expression compiled in
 _BDDHandle_ given an assignment to its  variables.

~~~~~
bdd_new(X+(Y+X)*(-Z), BDD),
[X,Y,Z] = [0,0,0],
bdd_eval(BDD, V),
writeln(V).
~~~~~
would write 0 in the standard output stream.

The  Prolog code equivalent to <tt>bdd_eval/2</tt> is:

~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_bdd, RT, _, V).

eval_bdd(pp(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ R ).
eval_bdd(pn(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ (1-R) ).
~~~~~
First, the nodes are reversed to implement bottom-up evaluation. Then,
we use the `foldl` list manipulation predicate to walk every node,
computing the disjunction of the two cases and binding the output
variable. The top node gives the full expression value. Notice that
`(1- _X_)`  implements negation.


*/
bdd_eval(cudd(M, X, Vars, _), Val) :-
	cudd_eval(M, X, Vars, Val).
bdd_eval(add(M, X, Vars, _), Val) :-
	add_eval(M, X, Vars, Val).

mtbdd_eval(add(M,X, Vars, _), Val) :-
	add_eval(M, X, Vars, Val).

% get the BDD as a Prolog list from the CUDD C object
/** @pred bdd_tree(+ _BDDHandle_,  _Term_)

Convert the BDD or ADD represented by  _BDDHandle_ to a Prolog term
of the form `bdd( _Dir_,  _Nodes_,  _Vars_)` or `mtbdd( _Nodes_,  _Vars_)`, respectively. The arguments are:

+
 _Dir_ direction of the BDD, usually 1
+
 _Nodes_ list of nodes in the BDD or ADD.

In a BDD nodes may be <tt>pp</tt> (both terminals are positive) or <tt>pn</tt>
(right-hand-side is negative), and have four arguments: a logical
variable that will be bound to the value of the node, the logical
variable corresponding to the node, a logical variable, a 0 or a 1 with
the value of the left-hand side, and a logical variable, a 0 or a 1
with the right-hand side.

+
 _Vars_ are the free variables in the original BDD, or the parameters of the BDD/ADD.

As an example, the BDD for the expression `X+(Y+X)\*(-Z)` becomes:

~~~~~
bdd(1,[pn(N2,X,1,N1),pp(N1,Y,N0,1),pn(N0,Z,1,1)],vs(X,Y,Z))
~~~~~


*/
bdd_tree(cudd(M, X, Vars, _Vs), bdd(Dir, List, Vars)) :-
	cudd_to_term(M, X, Vars, Dir, List).
bdd_tree(add(M, X, Vars, _), mtbdd(Tree, Vars)) :-
	add_to_term(M, X, Vars, Tree).

/** @pred bdd_to_probability_sum_product(+ _BDDHandle_, - _Prob_)

Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_.


*/
bdd_to_probability_sum_product(cudd(M,X,_,Probs), Prob) :-
	cudd_to_probability_sum_product(M, X, Probs, Prob).

/** @pred bdd_to_probability_sum_product(+ _BDDHandle_, - _Probs_, - _Prob_)
Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_, and
the probabilities of the inner nodes are  _Probs_.

In Prolog, this predicate would correspond to computing the value of a
BDD. The input variables will be bound to probabilities, eg
`[ _X_, _Y_, _Z_] = [0.3.0.7,0.1]`, and the previous
`eval_bdd` would operate over real numbers:

~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_prob, RT, _, V).

eval_prob(pp(P,X,L,R), _, P) :-
    P is  X * L +  (1-X) * R.
eval_prob(pn(P,X,L,R), _, P) :-
    P is  X * L + (1-X) * (1-R).
~~~~~

*/
bdd_to_probability_sum_product(cudd(M,X,_,_Probs), Probs, Prob) :-
	cudd_to_probability_sum_product(M, X, Probs, Prob).


/** @pred bdd_close( _BDDHandle_)

close the BDD and release any resources it holds.

 */
bdd_close(cudd(M,_,_Vars, _)) :-
	cudd_die(M).
bdd_close(add(M,_,_Vars, _)) :-
	cudd_die(M).

/** @pred bdd_close( _BDDHandle_)

  close the BDD and release any resources it holds.

*/
bdd_reorder(cudd(M,Top,_Vars, _), How) :-
        cudd_reorder(M, Top,How).

/** @pred bdd_size(+ _BDDHandle_, - _Size_)

Unify  _Size_ with the number of nodes in  _BDDHandle_.


*/
bdd_size(cudd(M,Top,_Vars, _), Sz) :-
	cudd_size(M,Top,Sz).
bdd_size(add(M,Top,_Vars, _), Sz) :-
	cudd_size(M,Top,Sz).

/** @pred bdd_print(+ _BDDHandle_, + _File_)

Output bdd  _BDDHandle_ as a dot file to  _File_.


*/
bdd_print(cudd(M,Top,_Vars, _), File) :-
	absolute_file_name(File, AFile, []),
	cudd_print(M, Top, AFile).
bdd_print(add(M,Top,_Vars, _), File) :-
	 absolute_file_name(File, AFile, []),
	 cudd_print(M, Top, AFile).

bdd_print(cudd(M,Top, Vars, _), File, Names) :-
	Vars =.. [_|LVars],
	%trace,
	maplist( fetch_name(Names), LVars, Ss),
        absolute_file_name(File, AFile, []),
	cudd_print(M, Top, AFile, Ss).
bdd_print(add(M,Top, Vars, _), File, Names) :-
	Vars =.. [_|LVars],
	maplist( fetch_name(Names), LVars, Ss),
        absolute_file_name(File, AFile, []),
	cudd_print(M, Top, AFile, Ss).

fetch_name([S=V1|_], V2, SN) :- V1 ==  V2, !,
	( atom(S) -> SN = S ; format(atom(SN), '~w', [S]) ).
fetch_name([_|Y], V, S) :- !,
	fetch_name(Y, V, S).
fetch_name([], V, V).

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


%% @}

