%% -*- prolog -*-
%%=============================================================================
%% Copyright (C) 2011 by Denys Duchier
%%
%% This program is free software: you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published by the
%% Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%=============================================================================

:- module(gecode, [(:=)/2, op(500, xfx, ':='),
		   (+=)/2, op(500, xfx, '+=')]).


/** @addtogroup Gecode5 Gecode 5 Interface

@ingroup packages
@{


The gecode library intreface was designed and implemented by Denis
Duchier, with recent work by Vítor Santos Costa to port it to version 4
of gecode and to have an higher level interface,


 @addtogroup The_Gecode_Interface The Gecode Interface
@ingroup Gecode
@{

This text is due to Denys Duchier. The gecode interface requires

~~~~~{.prolog}
:- use_module(library(gecode)).
~~~~~
Several example programs are available with the distribution.

+ CREATING A SPACE

A space is gecodes data representation for a store of constraints:

~~~~~{.prolog}
    Space := space
~~~~~

+ CREATING VARIABLES

Unlike in Gecode, variable objects are not bound to a specific Space.  Each one
actually contains an index with which it is possible to access a Space-bound
Gecode variable.  Variables can be created using the following expressions:

~~~~~{.prolog}
   IVar := intvar(Space,SPEC...)
   BVar := boolvar(Space)
   SVar := setvar(Space,SPEC...)
~~~~~

where SPEC... is the same as in Gecode.  For creating lists of variables use
the following variants:

~~~~~{.prolog}
   IVars := intvars(Space,N,SPEC...)
   BVars := boolvars(Space,N,SPEC...)
   SVars := setvars(Space,N,SPEC...)
~~~~~

where N is the number of variables to create (just like for XXXVarArray in
Gecode).  Sometimes an IntSet is necessary:

~~~~~{.prolog}
   ISet := intset([SPEC...])
~~~~~

where each SPEC is either an integer or a pair (I,J) of integers.  An IntSet
describes a set of ints by providing either intervals, or integers (which stand
for an interval of themselves).  It might be tempting to simply represent an
IntSet as a list of specs, but this would be ambiguous with IntArgs which,
here, are represented as lists of ints.

~~~~~{.prolog}
   Space += keep(Var)
   Space += keep(Vars)
~~~~~

Variables can be marked as "kept".  In this case, only such variables will be
explicitly copied during search.  This could bring substantial benefits in
memory usage.  Of course, in a solution, you can then only look at variables
that have been "kept".  If no variable is marked as "kept", then they are all
kept.  Thus marking variables as "kept" is purely an optimization.

+ CONSTRAINTS AND BRANCHINGS

all constraint and branching posting functions are available just like in
Gecode.  Wherever a XXXArgs or YYYSharedArray is expected, simply use a list.
At present, there is no support for minimodel-like constraint posting.
Constraints and branchings are added to a space using:

~~~~~{.prolog}
    Space += CONSTRAINT
    Space += BRANCHING
~~~~~

For example:

~~~~~{.prolog}
    Space += rel(X,'IRT_EQ',Y)
~~~~~

arrays of variables are represented by lists of variables, and constants are
represented by atoms with the same name as the Gecode constant
(e.g. 'INT_VAR_SIZE_MIN').

+ SEARCHING FOR SOLUTIONS

~~~~~{.prolog}
    SolSpace := search(Space)
~~~~~

This is a backtrackable predicate that enumerates all solution spaces
(SolSpace).  It may also take options:

~~~~~{.prolog}
    SolSpace := search(Space,Options)
~~~~~

Options is a list whose elements maybe:

+ restart
to select the Restart search engine
+ threads=N
to activate the parallel search engine and control the number of
workers (see Gecode doc)
+ c_d=N
to set the commit distance for recomputation
+ a_d=N
to set the adaptive distance for recomputation



+ EXTRACTING INFO FROM A SOLUTION

An advantage of non Space-bound variables, is that you can use them both to
post constraints in the original space AND to consult their values in
solutions.  Below are methods for looking up information about variables.  Each
of these methods can either take a variable as argument, or a list of
variables, and returns resp. either a value, or a list of values:

~~~~~{.prolog}
    Val := assigned(Space,X)

    Val := min(Space,X)
    Val := max(Space,X)
    Val := med(Space,X)
    Val := val(Space,X)
    Val := size(Space,X)
    Val := width(Space,X)
    Val := regret_min(Space,X)
    Val := regret_max(Space,X)

    Val := glbSize(Space,V)
    Val := lubSize(Space,V)
    Val := unknownSize(Space,V)
    Val := cardMin(Space,V)
    Val := cardMax(Space,V)
    Val := lubMin(Space,V)
    Val := lubMax(Space,V)
    Val := glbMin(Space,V)
    Val := glbMax(Space,V)
    Val := glb_ranges(Space,V)
    Val := lub_ranges(Space,V)
    Val := unknown_ranges(Space,V)
    Val := glb_values(Space,V)
    Val := lub_values(Space,V)
    Val := unknown_values(Space,V)
~~~~~

+ DISJUNCTORS

Disjunctors provide support for disjunctions of clauses, where each clause is a
conjunction of constraints:

~~~~~{.prolog}
    C1 or C2 or ... or Cn
~~~~~

Each clause is executed "speculatively": this means it does not affect the main
space.  When a clause becomes failed, it is discarded.  When only one clause
remains, it is committed: this means that it now affects the main space.

Example:

Consider the problem where either X=Y=0 or X=Y+(1 or 2) for variable X and Y
that take values in 0..3.

~~~~~{.prolog}
    Space := space,
    [X,Y] := intvars(Space,2,0,3),
~~~~~

First, we must create a disjunctor as a manager for our 2 clauses:

~~~~~{.prolog}
    Disj := disjunctor(Space),
~~~~~

We can now create our first clause:

~~~~~{.prolog}
    C1 := clause(Disj),
~~~~~

This clause wants to constrain X and Y to 0.  However, since it must be
executed "speculatively", it must operate on new variables X1 and Y1 that
shadow X and Y:

~~~~~{.prolog}
    [X1,Y1] := intvars(C1,2,0,3),
    C1 += forward([X,Y],[X1,Y1]),
~~~~~

The forward(...) stipulation indicates which global variable is shadowed by
which clause-local variable.  Now we can post the speculative clause-local
constraints for X=Y=0:

~~~~~{.prolog}
    C1 += rel(X1,'IRT_EQ',0),
    C1 += rel(Y1,'IRT_EQ',0),
~~~~~

We now create the second clause which uses X2 and Y2 to shadow X and Y:

~~~~~{.prolog}
    C2 := clause(Disj),
    [X2,Y2] := intvars(C2,2,0,3),
    C2 += forward([X,Y],[X2,Y2]),
~~~~~

However, this clause also needs a clause-local variable Z2 taking values 1 or
2 in order to post the clause-local constraint X2=Y2+Z2:

~~~~~{.prolog}
    Z2 := intvar(C2,1,2),
    C2 += linear([-1,1,1],[X2,Y2,Z2],'IRT_EQ',0),
~~~~~

Finally, we can branch and search:

~~~~~{.prolog}
    Space += branch([X,Y],'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
    SolSpace := search(Space),
~~~~~

and lookup values of variables in each solution:

~~~~~{.prolog}
    [X_,Y_] := val(SolSpace,[X,Y]).
~~~~~




*/

:- use_module(library(debug)).

:- op(500, xfx, user:':=').
:- op(500, xfx, user:'+=').
:- load_foreign_files([gecode_yap],[],gecode_init).

is_int(X,Y) :- integer(X), Y=X.
is_int(X) :- integer(X).

is_bool_(true,true).
is_bool_(false,false).
is_bool(X,Y) :- nonvar(X), Y=X.
is_bool(X) :- is_bool(X,_).

is_IntVar_('IntVar'(I,K),N) :-
    integer(I),
    integer(K),
    nb_getval(gecode_space_use_keep_index,B),
    (B=true -> N=K ; N=I).
is_FloatVar_('FloatVar'(I,K),N) :-
    integer(I),
    integer(K),
    nb_getval(gecode_space_use_keep_index,B),
    (B=true -> N=K ; N=I).
is_BoolVar_('BoolVar'(I,K),N) :-
    integer(I),
    integer(K),
    nb_getval(gecode_space_use_keep_index,B),
    (B=true -> N=K ; N=I).
is_SetVar_('SetVar'(I,K),N) :-
    integer(I),
    integer(K),
    nb_getval(gecode_space_use_keep_index,B),
    (B=true -> N=K ; N=I).

is_IntVar(X,I) :- nonvar(X), is_IntVar_(X,I).
is_BoolVar(X,I) :- nonvar(X), is_BoolVar_(X,I).
is_FloatVar(X,I) :- nonvar(X), is_FloatVar_(X,I).
is_SetVar(X,I) :- nonvar(X), is_SetVar_(X,I).

is_IntVar(X) :- is_IntVar(X,_).
is_BoolVar(X) :- is_BoolVar(X,_).
is_FloatVar(X) :- is_FloatVar(X,_).
is_SetVar(X) :- is_SetVar(X,_).

is_IntVarArgs_([],[]).
is_IntVarArgs_([H|T],[H2|T2]) :- is_IntVar(H,H2), is_IntVarArgs(T,T2).
is_IntVarArgs(X,Y) :- nonvar(X), is_IntVarArgs_(X,Y).
is_IntVarArgs(X) :- \+ \+ is_IntVarArgs(X,_).

is_BoolVarArgs_([],[]).
is_BoolVarArgs_([H|T],[H2|T2]) :- is_BoolVar(H,H2), is_BoolVarArgs(T,T2).
is_BoolVarArgs(X,Y) :- nonvar(X), is_BoolVarArgs_(X,Y).
is_BoolVarArgs(X) :- \+ \+ is_BoolVarArgs(X,_).

is_FloatVarArgs_([],[]).
is_FloatVarArgs_([H|T],[H2|T2]) :- is_FloatVar(H,H2), is_FloatVarArgs(T,T2).
is_FloatVarArgs(X,Y) :- nonvar(X), is_FloatVarArgs_(X,Y).
is_FloatVarArgs(X) :- \+ \+ is_FloatVarArgs(X,_).

is_FloatValArgs_([],[]).
is_FloatValArgs_([H|T],[H2|T2]) :- is_FloatVar(H,H2), is_FloatValArgs(T,T2).
is_FloatValArgs(X,Y) :- nonvar(X), is_FloatValArgs_(X,Y).
is_FloatValArgs(X) :- \+ \+ is_FloatValArgs(X,_).

is_SetVarArgs_([],[]).
is_SetVarArgs_([H|T],[H2|T2]) :- is_SetVar(H,H2), is_SetVarArgs(T,T2).
is_SetVarArgs(X,Y) :- nonvar(X), is_SetVarArgs_(X,Y).
is_SetVarArgs(X) :- \+ \+ is_SetVarArgs(X,_).

is_IntArgs_([],[]).
is_IntArgs_([H|T],[H|T2]) :- integer(H), is_IntArgs(T,T2).
is_IntArgs(X,Y) :- nonvar(X), is_IntArgs_(X,Y).
is_IntArgs(X) :- \+ \+ is_IntArgs(X,_).

is_IntSharedArray(X) :- is_IntArgs(X).
is_IntSharedArray(X,Y) :- is_IntArgs(X,Y).

is_TaskTypeArgs_([],[]).
is_TaskTypeArgs_([H|T],[H2|T2]) :- is_TaskType(H,H2), is_TaskTypeArgs(T,T2).
is_TaskTypeArgs(X,Y) :- nonvar(X), is_TaskTypeArgs_(X,Y).
is_TaskTypeArgs(X) :- \+ \+ is_TaskTypeArgs(X,_).

is_IntSet_('IntSet'(L),L).
is_IntSet(X,Y) :- nonvar(X), is_IntSet_(X,Y).
is_IntSet(X) :- is_IntSet(X,_).

is_IntSetArgs_([],[]).
is_IntSetArgs_([H|T],[H2|T2]) :- is_IntSet(H,H2), is_IntSetArgs(T,T2).
is_IntSetArgs(X,Y) :- nonvar(X), is_IntSetArgs_(X,Y).
is_IntSetArgs(X) :- \+ \+ is_IntSetArgs(X,_).

is_TupleSet_('TupleSet'(TS),TS).
is_TupleSet(X,Y) :- nonvar(X), is_TupleSet_(X,Y).
is_TupleSet(X) :- is_TupleSet(X,_).

is_DFA_('DFA'(TS),TS).
is_DFA(X,Y) :- nonvar(X), is_DFA_(X,Y).
is_DFA(X) :- is_DFA(X,_).

new_intset(X,I,J) :- intset(X,I,J).
new_intset(X,L) :- intset(X,L).

intset(X, I, J) :-
	integer(I),
	integer(J),
	X='IntSet'([(I,J)]).
intset(X, L) :-
	is_list_of_intset_specs(L,L2),
	X='IntSet'(L2).

is_list_of_intset_specs(X,Y) :-
	nonvar(X), is_list_of_intset_specs_(X,Y).
is_list_of_intset_specs_([],[]).
is_list_of_intset_specs_([H|T],[H2|T2]) :-
	is_intset_spec(H,H2),
	is_list_of_intset_specs(T,T2).

is_intset_spec(X,Y) :- nonvar(X), is_intset_spec_(X,Y).
is_intset_spec_((I,J),(I,J)) :- !, integer(I), integer(J).
is_intset_spec_(I,(I,I)) :- integer(I).

assert_var(X,Y) :-
	var(X) -> X=Y; throw(gecode_error(expected(var))).
assert_is_int(X,Y) :-
	integer(X) -> X=Y ; throw(gecode_error(expected(int))).
assert_is_float(X,Y) :-
	float(X) -> X=Y ; throw(gecode_error(expected(int))).
assert_is_Space(X,Y) :-
	is_Space(X,Y) -> true ; throw(gecode_error(expected(space))).
assert_is_IntSet(X,Y) :-
	is_IntSet(X,Y) -> true ; throw(gecode_error(expected(intset))).
assert_is_TupleSet(X,Y) :-
	is_TupleSet(X,Y) -> true ; throw(gecode_error(expected(tupleset))).
assert_is_DFA(X,Y) :-
	is_DFA(X,Y) -> true ; throw(gecode_error(expected(dfa))).
assert_is_IntVar(X,Y) :-
	is_IntVar(X,Y) -> true ; throw(gecode_error(expected(intvar))).
assert_is_BoolVar(X,Y) :-
	is_BoolVar(X,Y) -> true ; throw(gecode_error(expected(boolvar))).
assert_is_FloatVar(X,Y) :-
	is_FloatVar(X,Y) -> true ; throw(gecode_error(expected(floatvar))).
assert_is_SetVar(X,Y) :-
	is_SetVar(X,Y) -> true ; throw(gecode_error(expected(setvar))).
assert_is_IntArgs(X,Y) :-
	is_IntArgs(X,Y) -> true ; throw(gecode_error(expected(intargs))).
assert_is_IntVarArgs(X,Y) :-
	is_IntVarArgs(X,Y) -> true ; throw(gecode_error(expected(intvarargs))).
assert_is_BoolVarArgs(X,Y) :-
	is_BoolVarArgs(X,Y) -> true ; throw(gecode_error(expected(boolvarargs))).
assert_is_FloatVarArgs(X,Y) :-
	is_FloatVarArgs(X,Y) -> true ; throw(gecode_error(expected(floatvarargs))).
assert_is_FloatValArgs(X,Y) :-
	is_FloatValArgs(X,Y) -> true ; throw(gecode_error(expected(floatvarargs))).
assert_is_SetVarArgs(X,Y) :-
	is_SetVarArgs(X,Y) -> true ; throw(gecode_error(expected(setvarargs))).
assert_is_ReifyMode(X,Y) :-
	is_ReifyMode(X,Y) -> true ; throw(gecode_error(expected(reifymode))).

assert_var(X) :- assert_var(X,_).
assert_is_int(X) :- assert_is_int(X,_).
assert_is_float(X) :- assert_is_float(X,_).
assert_is_Space(X) :- assert_is_Space(X,_).
assert_is_IntSet(X) :- assert_is_IntSet(X,_).
assert_is_IntVar(X) :- assert_is_IntVar(X,_).
assert_is_BoolVar(X) :- assert_is_BoolVar(X,_).
assert_is_FloatVar(X) :- assert_is_FloatVar(X,_).
assert_is_SetVar(X) :- assert_is_SetVar(X,_).
assert_is_IntArgs(X) :- assert_is_IntArgs(X,_).
assert_is_IntVarArgs(X) :- assert_is_IntVarArgs(X,_).
assert_is_BoolVarArgs(X) :- assert_is_BoolVarArgs(X,_).
assert_is_FloatVarArgs(X) :- assert_is_FloatVarArgs(X,_).
assert_is_FloatValArgs(X) :- assert_is_FloatValArgs(X,_).
assert_is_SetVarArgs(X) :- assert_is_SetVarArgs(X,_).

%% Var and Val Branching changed in Gecode 4 to be done as a set of functions,
%% not as an enum.

is_IntVarBranch_('INT_VAR_NONE').
is_IntVarBranch_('INT_VAR_RND'(_)).
is_IntVarBranch_('INT_VAR_MERIT_MIN'(_)).
is_IntVarBranch_('INT_VAR_MERIT_MAX'(_)).
is_IntVarBranch_('INT_VAR_DEGREE_MIN').
is_IntVarBranch_('INT_VAR_DEGREE_MAX').
is_IntVarBranch_('INT_VAR_AFC_MIN'(_)).
is_IntVarBranch_('INT_VAR_AFC_MAX'(_)).
is_IntVarBranch_('INT_VAR_ACTIVITY_MIN'(_)).
is_IntVarBranch_('INT_VAR_ACTIVITY_MAX'(_)).
is_IntVarBranch_('INT_VAR_MIN_MIN').
is_IntVarBranch_('INT_VAR_MIN_MAX').
is_IntVarBranch_('INT_VAR_MAX_MIN').
is_IntVarBranch_('INT_VAR_MAX_MAX').
is_IntVarBranch_('INT_VAR_SIZE_MIN').
is_IntVarBranch_('INT_VAR_SIZE_MAX').
is_IntVarBranch_('INT_VAR_DEGREE_SIZE_MIN').
is_IntVarBranch_('INT_VAR_DEGREE_SIZE_MAX').
is_IntVarBranch_('INT_VAR_AFC_SIZE_MIN'(_)).
is_IntVarBranch_('INT_VAR_AFC_SIZE_MAX'(_)).
is_IntVarBranch_('INT_VAR_ACTIVITY_SIZE_MIN'(_)).
is_IntVarBranch_('INT_VAR_ACTIVITY_SIZE_MAX'(_)).
is_IntVarBranch_('INT_VAR_REGRET_MIN_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MIN_MAX').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MIN').
is_IntVarBranch_('INT_VAR_REGRET_MAX_MAX').

is_IntVarBranch_(X, X) :-
	is_IntVarBranch_(X).

is_IntVarBranch(X,Y) :- nonvar(X), is_IntVarBranch_(X,Y).
is_IntVarBranch(X) :- is_IntVarBranch(X,_).

is_SetVarBranch_('SET_VAR_NONE').
is_SetVarBranch_('SET_VAR_RND'(_)).
is_SetVarBranch_('SET_VAR_MERIT_MIN'(_)).
is_SetVarBranch_('SET_VAR_MERIT_MAX'(_)).
is_SetVarBranch_('SET_VAR_DEGREE_MIN').
is_SetVarBranch_('SET_VAR_DEGREE_MAX').
is_SetVarBranch_('SET_VAR_AFC_MIN'(_)).
is_SetVarBranch_('SET_VAR_AFC_MAX'(_)).
is_SetVarBranch_('SET_VAR_ACTIVITY_MIN'(_)).
is_SetVarBranch_('SET_VAR_ACTIVITY_MAX'(_)).
is_SetVarBranch_('SET_VAR_MIN_MIN').
is_SetVarBranch_('SET_VAR_MIN_MAX').
is_SetVarBranch_('SET_VAR_MAX_MIN').
is_SetVarBranch_('SET_VAR_MAX_MAX').
is_SetVarBranch_('SET_VAR_SIZE_MIN').
is_SetVarBranch_('SET_VAR_SIZE_MAX').
is_SetVarBranch_('SET_VAR_DEGREE_SIZE_MIN').
is_SetVarBranch_('SET_VAR_DEGREE_SIZE_MAX').
is_SetVarBranch_('SET_VAR_AFC_SIZE_MIN'(_)).
is_SetVarBranch_('SET_VAR_AFC_SIZE_MAX'(_)).
is_SetVarBranch_('SET_VAR_ACTIVITY_SIZE_MIN'(_)).
is_SetVarBranch_('SET_VAR_ACTIVITY_SIZE_MAX'(_)).

is_SetVarBranch_(X, X) :-
	is_SetVarBranch_(X).

is_SetVarBranch(X,Y) :- nonvar(X), is_SetVarBranch_(X,Y).
is_SetVarBranch(X) :- is_SetVarBranch(X,_).

is_FloatVarBranch_('FLOAT_VAR_NONE').
is_FloatVarBranch_('FLOAT_VAR_RND'(_)).
is_FloatVarBranch_('FLOAT_VAR_MERIT_MIN'(_)).
is_FloatVarBranch_('FLOAT_VAR_MERIT_MAX'(_)).
is_FloatVarBranch_('FLOAT_VAR_DEGREE_MIN').
is_FloatVarBranch_('FLOAT_VAR_DEGREE_MAX').
is_FloatVarBranch_('FLOAT_VAR_AFC_MIN'(_)).
is_FloatVarBranch_('FLOAT_VAR_AFC_MAX'(_)).
is_FloatVarBranch_('FLOAT_VAR_ACTIVITY_MIN'(_)).
is_FloatVarBranch_('FLOAT_VAR_ACTIVITY_MAX'(_)).
is_FloatVarBranch_('FLOAT_VAR_MIN_MIN').
is_FloatVarBranch_('FLOAT_VAR_MIN_MAX').
is_FloatVarBranch_('FLOAT_VAR_MAX_MIN').
is_FloatVarBranch_('FLOAT_VAR_MAX_MAX').
is_FloatVarBranch_('FLOAT_VAR_SIZE_MIN').
is_FloatVarBranch_('FLOAT_VAR_SIZE_MAX').
is_FloatVarBranch_('FLOAT_VAR_DEGREE_SIZE_MIN').
is_FloatVarBranch_('FLOAT_VAR_DEGREE_SIZE_MAX').
is_FloatVarBranch_('FLOAT_VAR_AFC_SIZE_MIN'(_)).
is_FloatVarBranch_('FLOAT_VAR_AFC_SIZE_MAX'(_)).
is_FloatVarBranch_('FLOAT_VAR_ACTIVITY_SIZE_MIN'(_)).
is_FloatVarBranch_('FLOAT_VAR_ACTIVITY_SIZE_MAX'(_)).

is_FloatVarBranch_(X, X) :-
	is_FloatVarBranch_(X).

is_FloatVarBranch(X,Y) :- nonvar(X), is_FloatVarBranch_(X,Y).
is_FloatVarBranch(X) :- is_FloatVarBranch(X,_).

is_IntValBranch_('INT_VAL_RND'(_)).
is_IntValBranch_('INT_VAL'(_,_)).
is_IntValBranch_('INT_VAL_MIN').
is_IntValBranch_('INT_VAL_MED').
is_IntValBranch_('INT_VAL_MAX').
is_IntValBranch_('INT_VAL_SPLIT_MIN').
is_IntValBranch_('INT_VAL_SPLIT_MAX').
is_IntValBranch_('INT_VAL_RANGE_MIN').
is_IntValBranch_('INT_VAL_RANGE_MAX').
is_IntValBranch_('INT_VALUES_MIN').
is_IntValBranch_('INT_NEAR_MIN'(_)).
is_IntValBranch_('INT_NEAR_MAX'(_)).
is_IntValBranch_('INT_NEAR_INC'(_)).
is_IntValBranch_('INT_NEAR_DEC'(_)).

is_IntValBranch_(X,X) :- is_IntValBranch_(X).

is_IntValBranch(X,Y) :- nonvar(X), is_IntValBranch_(X,Y).
is_IntValBranch(X) :- is_IntValBranch(X,_).

is_SetValBranch_('SET_VAL_RND_INC'(_)).
is_SetValBranch_('SET_VAL_RND_EXC'(_)).
is_SetValBranch_('SET_VAL'(_,_)).
is_SetValBranch_('SET_VAL_MIN_INC').
is_SetValBranch_('SET_VAL_MIN_EXC').
is_SetValBranch_('SET_VAL_MED_INC').
is_SetValBranch_('SET_VAL_MED_EXC').
is_SetValBranch_('SET_VAL_MAX_INC').
is_SetValBranch_('SET_VAL_MAX_EXC').

is_SetValBranch_(X,X) :- is_SetValBranch_(X).

is_SetValBranch(X,Y) :- nonvar(X), is_SetValBranch_(X,Y).
is_SetValBranch(X) :- is_SetValBranch(X,_).

is_FloatValBranch_('FLOAT_VAL'(_,_)).
is_FloatValBranch_('FLOAT_VAL_SPLIT_RND'(_)).
is_FloatValBranch_('FLOAT_VAL_SPLIT_MIN').
is_FloatValBranch_('FLOAT_VAL_SLIT_MAX').

is_FloatValBranch_(X,X) :- is_FloatValBranch_(X).

is_FloatValBranch(X,Y) :- nonvar(X), is_FloatValBranch_(X,Y).
is_FloatValBranch(X) :- is_FloatValBranch(X,_).


new_space(Space) :-
	assert_var(Space),
	gecode_new_space(Space_),
	Space='Space'(Space_).

%% checking that an argument is a space sets a global variable
%% indicating whether variables need to be translated to their
%% original index or to their "keep" index.
%%
%% these bindings are going to take advantage of the fact that,
%% when a space is involved, it is checked first, thus setting
%% this global variable. subsequent accesses to variables are
%% then correctly translated.

is_Space_('Space'(X),X) :-
    gecode_space_use_keep_index(X,B),
    nb_setval(gecode_space_use_keep_index,B).
is_Space(X,Y) :- nonvar(X), is_Space_(X,Y).
is_Space(X) :- is_Space(X,_).

is_Reify_('Reify'(X),X).
is_Reify(X,Y) :- nonvar(X), is_Reify_(X,Y).
is_Reify(X) :- is_Reify(X,_).

%% AUTOGENERATE ALL VARIANTS LATER!

new_intvars([], _Space, _Lo, _Hi).
new_intvars([IVar|IVars], Space, Lo, Hi) :-
	new_intvar(IVar, Space, Lo, Hi),
	new_intvars(IVars, Space, Lo, Hi).

new_intvars([], _Space, _IntSet).
new_intvars([IVar|IVars], Space, IntSet) :-
	new_intvar(IVar, Space, IntSet),
	new_intvars(IVars, Space, IntSet).

new_boolvars([], _Space).
new_boolvars([BVar|BVars], Space) :-
	new_boolvar(BVar, Space),
	new_boolvars(BVars, Space).

new_setvars([], _Space, _X1, _X2, _X3, _X4, _X5, _X6).
new_setvars([SVar|SVars], Space, X1, X2, X3, X4, X5, X6) :-
	new_setvar(SVar, Space, X1, X2, X3, X4, X5, X6),
	new_setvars(SVars, Space, X1, X2, X3, X4, X5, X6).

new_setvars([], _Space, _X1, _X2, _X3, _X4, _X5).
new_setvars([SVar|SVars], Space, X1, X2, X3, X4, X5) :-
	new_setvar(SVar, Space, X1, X2, X3, X4, X5),
	new_setvars(SVars, Space, X1, X2, X3, X4, X5).

new_setvars([], _Space, _X1, _X2, _X3, _X4).
new_setvars([SVar|SVars], Space, X1, X2, X3, X4) :-
	new_setvar(SVar, Space, X1, X2, X3, X4),
	new_setvars(SVars, Space, X1, X2, X3, X4).

new_setvars([], _Space, _X1, _X2, _X3).
new_setvars([SVar|SVars], Space, X1, X2, X3) :-
	new_setvar(SVar, Space, X1, X2, X3),
	new_setvars(SVars, Space, X1, X2, X3).

new_setvars([], _Space, _X1, _X2).
new_setvars([SVar|SVars], Space, X1, X2) :-
	new_setvar(SVar, Space, X1, X2),
	new_setvars(SVars, Space, X1, X2).

%% AUTOGENERATE LATER

assert_integer(X) :- assert_is_int(X).

new_intvar(IVar, Space, Lo, Hi) :- !,
	assert_var(IVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_integer(Lo),
	assert_integer(Hi),
	gecode_new_intvar_from_bounds(Idx,Space_,Lo,Hi),
	IVar='IntVar'(Idx,-1).
new_intvar(IVar, Space, IntSet) :- !,
	assert_var(IVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_is_IntSet(IntSet, L),
	gecode_new_intvar_from_intset(Idx,Space_,L),
	IVar='IntVar'(Idx,-1).

new_floatvar(FVar, Space, Lo, Hi) :- !,
	assert_var(FVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_float(Lo),
	assert_float(Hi),
	gecode_new_floatvar_from_bounds(Idx,Space_,Lo,Hi),
	FVar='FloatVar'(Idx,-1).

new_boolvar(BVar, Space) :- !,
	assert_var(BVar),
	assert_is_Space_or_Clause(Space,Space_),
	gecode_new_boolvar(Idx,Space_),
	BVar='BoolVar'(Idx,-1).

%% (GlbMin,GlbMax,LubMin,LubMax,CardMin,CardMax) 6 new_setvar_1
%% (GlbMin,GlbMax,LubMin,LubMax,CardMin)         5 new_setvar_2
%% (GlbMin,GlbMax,LubMin,LubMax)                 4 new_setvar_3
%% (Glb,LubMin,LubMax,CardMin,CardMax)           5 new_setvar_4
%% (Glb,LubMin,LubMax,CardMin)                   4 new_setvar_5
%% (Glb,LubMin,LubMax)                           3 new_setvar_6
%% (GlbMin,GlbMax,Lub,CardMin,CardMax)           5 new_setvar_7
%% (GlbMin,GlbMax,Lub,CardMin)                   4 new_setvar_8
%% (GlbMin,GlbMax,Lub)                           3 new_setvar_9
%% (Glb,Lub,CardMin,CardMax)                     4 new_setvar_10
%% (Glb,Lub,CardMin)                             3 new_setvar_11
%% (Glb,Lub)                                     2 new_setvar_12

%% 6 arguments
%% (GlbMin,GlbMax,LubMin,LubMax,CardMin,CardMax) 6 new_setvar_1
new_setvar(SVar, Space, GlbMin, GlbMax, LubMin, LubMax, CardMin, CardMax) :-
	assert_var(SVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_integer(GlbMin),
	assert_integer(GlbMax),
	assert_integer(LubMin),
	assert_integer(LubMax),
	assert_integer(CardMin),
	assert_integer(CardMax),
	gecode_new_setvar(Idx, Space_, GlbMin, GlbMax, LubMin, LubMax, CardMin, CardMax),
	SVar='SetVar'(Idx,-1).

%% 5 arguments
%% (GlbMin,GlbMax,LubMin,LubMax,CardMin)         5 new_setvar_2
%% (Glb,LubMin,LubMax,CardMin,CardMax)           5 new_setvar_4
%% (GlbMin,GlbMax,Lub,CardMin,CardMax)           5 new_setvar_7
new_setvar(SVar, Space, X1, X2, X3, X4, X5) :-
	assert_var(SVar),
	assert_is_Space_or_Clause(Space,Space_),
	(integer(X1)
	->
	 assert_integer(X2),
	 assert_integer(X4),
	 assert_integer(X5),
	 (is_IntSet(X3,X3_)
	 -> gecode_new_setvar_7(Idx,Space_,X1,X2,X3_,X4,X5)
	 ;  gecode_new_setvar_2(Idx,Space_,X1,X2,X3,X4,X5))
	;
	 assert_is_IntSet(X1,X1_),
	 assert_integer(X2),
	 assert_integer(X3),
	 assert_integer(X4),
	 assert_integer(X5),
	 gecode_new_setvar_4(Idx,Space_,X1_,X2,X3,X4,X5)),
	SVar='SetVar'(Idx,-1).

%% 4 arguments
%% (GlbMin,GlbMax,LubMin,LubMax)                 4 new_setvar_3
%% (Glb,LubMin,LubMax,CardMin)                   4 new_setvar_5
%% (GlbMin,GlbMax,Lub,CardMin)                   4 new_setvar_8
%% (Glb,Lub,CardMin,CardMax)                     4 new_setvar_10
new_setvar(SVar,Space,X1,X2,X3,X4) :-
	assert_var(SVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_integer(X4),
	(is_IntSet(X1,X1_)
	-> (is_IntSet(X2,X2_)
	   ->
	    assert_integer(X3),
	    gecode_new_setvar_10(Idx,Space_,X1_,X2_,X3,X4)
	   ;
	    assert_integer(X2),
	    assert_integer(X3),
	    gecode_new_setvar_5(Idx,Space_,X1_,X2,X3,X4))
	;
	 assert_integer(X1),
	 assert_integer(X2),
	 (is_IntSet(X3,X3_)
	 ->
	  gecode_new_setvar_8(Idx,Space_,X1,X2,X3_,X4)
	 ;
	  assert_integer(X3),
	  gecode_new_setvar_3(Idx,Space_,X1,X2,X3,X4))),
	SVar='SetVar'(Idx,-1).

%% 3 arguments
%% (Glb,LubMin,LubMax)                           3 new_setvar_6
%% (GlbMin,GlbMax,Lub)                           3 new_setvar_9
%% (Glb,Lub,CardMin)                             3 new_setvar_11
new_setvar(SVar,Space,X1,X2,X3) :-
	assert_var(SVar),
	assert_is_Space_or_Clause(Space,Space_),
	(is_IntSet(X1,X1_)
	-> (is_IntSet(X2,X2_)
	   ->
	    assert_integer(X3),
	    gecode_new_setvar_11(Idx,Space_,X1_,X2_,X3)
	   ;
	    assert_integer(X2),
	    assert_integer(X3),
	    gecode_new_setvar_6(Idx,Space_,X1_,X2,X3))
	;
	 assert_integer(X1),
	 assert_integer(X2),
	 assert_is_IntSet(X3,X3_),
	 gecode_new_setvar_9(Idx,Space_,X1,X2,X3_)),
	SVar='SetVar'(Idx,-1).

%% 2 arguments
%% (Glb,Lub)                                     2 new_setvar_12
new_setvar(SVar,Space,X1,X2) :-
	assert_var(SVar),
	assert_is_Space_or_Clause(Space,Space_),
	assert_is_IntSet(X1,X1_),
	assert_is_IntSet(X2,X2_),
	gecode_new_setvar_12(Idx,Space_,X1_,X2_),
	SVar='SetVar'(Idx,-1).

new_tupleset( TupleSet, List  ) :-
	gecode_new_tupleset(List, TupleSet_),
	TupleSet = 'TupleSet'(TupleSet_).

new_dfa( DFA, S0,  List, Finals  ) :-
	gecode_new_dfa(DFA_, S0, List, Finals),
	DFA = 'DFA'(DFA_).


minimize(Space,IVar) :-
	assert_is_Space(Space,Space_),
	assert_is_IntVar(IVar,IVar_),
	gecode_space_minimize(Space_,IVar_).
maximize(Space,IVar) :-
	assert_is_Space(Space,Space_),
	assert_is_IntVar(IVar,IVar_),
	gecode_space_maximize(Space_,IVar_).
minimize(Space,IVar1,IVar2) :-
	assert_is_Space(Space,Space_),
	assert_is_IntVar(IVar1,IVar1_),
	assert_is_IntVar(IVar2,IVar2_),
	gecode_space_minimize_ratio(Space_,IVar1_,IVar2_).
maximize(Space,IVar1,IVar2) :-
	assert_is_Space(Space,Space_),
	assert_is_IntVar(IVar1,IVar1_),
	assert_is_IntVar(IVar2,IVar2_),
	gecode_space_maximize_ratio(Space_,IVar1_,IVar2_).

reify(Space,BVar,Mode,R) :-
	assert_is_Space(Space,Space_),
	assert_is_BoolVar(BVar,BVar_),
	assert_is_ReifyMode(Mode,Mode_),
	assert_var(R),
	gecode_new_reify(Space_,BVar_,Mode_,R_),
	R = 'Reify'(R_).

gecode_search_options_init(search_options(0,1.0,8,2,'RM_NONE',0,1,0)).
gecode_search_options_offset(restart,1).
gecode_search_options_offset(threads,2).
gecode_search_options_offset(c_d    ,3).
gecode_search_options_offset(a_d    ,4).
gecode_search_options_offset(cutoff, 5).
gecode_search_options_offset(nogoods_limit, 6).
gecode_search_options_offset(clone, 7).
gecode_search_options_offset(stop, 8). % unimplemented

gecode_search_option_set(O,V,R) :-
    gecode_search_options_offset(O,I),
    setarg(I,R,V).

gecode_search_options_from_alist(L,R) :-
    gecode_search_options_init(R),
    gecode_search_options_process_alist(L,R).

gecode_search_options_process_alist([], _R).
gecode_search_options_process_alist([H|T], R) :- !,
    gecode_search_options_process1(H, R),
    gecode_search_options_process_alist(T, R).

gecode_search_options_process1(restart,R) :- !,
    gecode_search_option_set(restart,1,R).
gecode_search_options_process1(threads=N,R) :- !,
    (integer(N) -> V is float(N)
    ; (float(N) -> V=N
      ; throw(bad_search_option_value(threads=N)))),
    gecode_search_option_set(threads,V,R).
gecode_search_options_process1(c_d=N,R) :- !,
    (integer(N) -> V=N
    ; throw(bad_search_option_value(c_d=N))),
    gecode_search_option_set(c_d,V,R).
gecode_search_options_process1(a_d=N,R) :- !,
    (integer(N) -> V=N
    ; throw(bad_search_option_value(a_d=N))),
    gecode_search_option_set(a_d,V,R).
gecode_search_options_process1(cutoff=C,R) :- !,
    (is_RestartMode(C,C_) -> V=C_
    ; throw(bad_search_option_value(cutoff=C))),
    gecode_search_option_set(cutoff,V,R).
gecode_search_options_process1(nogoods_limit=N,R) :- !,
    (integer(N), N >= 0 -> V=N
    ; throw(bad_search_option_value(nogoods_limit=N))),
    gecode_search_option_set(nogoods_limit,V,R).
gecode_search_options_process1(clone=N,R) :- !,
    ((N == 0 ; N == 1)-> V=N
    ; throw(bad_search_option_value(clone=N))),
    gecode_search_option_set(clone,V,R).
gecode_search_options_process1(O,_) :-
    throw(gecode_error(unrecognized_search_option(O))).

search(Space, Solution) :-
    search(Space, Solution, []).

search(Space, Solution, Alist) :-
	assert_is_Space(Space,Space_),
	assert_var(Solution),
	gecode_search_options_from_alist(Alist,O),
	gecode_new_engine(Space_,Engine_,O),
	gecode_engine_search(Engine_,Solution_),
	Solution='Space'(Solution_).


%% INSPECTING VARIABLES

get_for_vars([],_Space,[],_F).
get_for_vars([V|Vs],Space,[V2|V2s],F) :-
	call_with_args(F,V,Space,V2),
	get_for_vars(Vs,Space,V2s,F).

get_assigned(Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_assigned(Space_,Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_assigned(Space_,Var_)
	; is_FloatVar(Var,Var_)
	-> gecode_floatvar_assigned(Space_,Var_)
	; is_SetVar(Var,Var_)
	-> gecode_setvar_assigned(Space_,Var_)
	; throw(gecode_error(expected(variable)))).

get_min(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_min(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_min(X, Space_, Var_)
	; is_FloatVar(Var,Var_)
	-> gecode_floatvar_min(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_min)).

get_max(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_max(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_max(X, Space_, Var_)
	; is_FloatVar(Var,Var_)
	-> gecode_floatvar_max(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_max)).

get_med(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_med(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_med(X, Space_, Var_)
	; is_FloatVar(Var,Var_)
	-> gecode_floatvar_med(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_med)).

get_val(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_val(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_val(X, Space_, Var_)
	; is_FloatVar(Var,Var_)
	-> gecode_floatvar_val(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_val)).

get_size(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_size(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_size(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_size)).

get_width(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_width(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_width(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_width)).

get_regret_min(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_regret_min(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_regret_min(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_regret_min)).

get_regret_max(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_regret_max(X, Space_, Var_)
	; is_BoolVar(Var,Var_)
	-> gecode_boolvar_regret_max(X, Space_, Var_)
	; get_for_vars(X, Space, Var, gecode:get_regret_max)).

get_glbSize(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_glbSize(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_glbSize)).

get_lubSize(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_lubSize(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_lubSize)).

get_unknownSize(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_unknownSize(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_unknownSize)).

get_cardMin(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_cardMin(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_cardMin)).

get_cardMax(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_cardMax(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_cardMax)).

get_lubMin(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_lubMin(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_lubMin)).

get_lubMax(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_lubMax(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_lubMax)).

get_glbMin(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_glbMin(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_glbMin)).

get_glbMax(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_glbMax(X,Space_,Var_)
	; get_for_vars(X, Space, Var, gecode:get_glbMax)).

get_glb_ranges(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_glb_ranges(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_glb_ranges)).

get_lub_ranges(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_lub_ranges(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_lub_ranges)).

get_unknown_ranges(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_unknown_ranges(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_unknown_ranges)).

get_glb_values(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_glb_values(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_glb_values)).

get_lub_values(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_lub_values(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_lub_values)).

get_unknown_values(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_SetVar(Var,Var_)
	-> gecode_setvar_unknown_values(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_unknown_values)).

get_ranges(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_ranges(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_ranges)).

get_values(X, Space, Var) :-
	assert_is_Space(Space,Space_),
	(is_IntVar(Var,Var_)
	-> gecode_intvar_values(X,Space_,Var_)
	; get_for_vars(X,Space,Var,gecode:get_values)).

new_disjunctor(X, Space) :-
	assert_is_Space(Space,Space_),
	gecode_new_disjunctor(D,Space_),
	X='Disjunctor'(D).

is_Disjunctor_('Disjunctor'(D),D).
is_Disjunctor(X,Y) :- nonvar(X), is_Disjunctor_(X,Y).
is_Disjunctor(X) :- is_Disjunctor(X,_).

assert_is_Disjunctor(X,Y) :-
	is_Disjunctor(X,Y) -> true ; throw(gecode_error(expected(disjunctor))).

new_clause(X, Disj) :-
	assert_is_Disjunctor(Disj,Disj_),
	gecode_new_clause(C, Disj_),
	X='Clause'(C).

is_Clause_('Clause'(C),C) :-
    gecode_space_use_keep_index(C,B),
    nb_setval(gecode_space_use_keep_index,B).
is_Clause(X,Y) :- nonvar(X), is_Clause_(X,Y).
is_Clause(X) :- is_Clause(X,_).

assert_is_Clause(X,Y) :-
	is_Clause(X,Y) -> true ; throw(gecode_error(expected(clause))).

is_Space_or_Clause(X,Y) :-
	(is_Space(X,Y);is_Clause(X,Y)), !.
assert_is_Space_or_Clause(X,Y) :-
	is_Space_or_Clause(X,Y)	-> true
	; throw(gecode_error(expected(space,clause))).

new_forward(Clause, X, Y) :-
	assert_is_Clause(Clause, Clause_),
	(is_IntVar(X,X_)
	-> (is_IntVar(Y,Y_)
	   -> gecode_clause_intvar_forward(Clause_,X_,Y_)
	   ; throw(gecode_error(forward)))
	; (is_BoolVar(X,X_)
	  -> (is_BoolVar(Y,Y_)
	     -> gecode_clause_boolvar_forward(Clause_,X_,Y_)
	     ; throw(gecode_error(forward)))
	  ; (is_SetVar(X,X_)
	    -> (is_SetVar(Y,Y_)
	       -> gecode_clause_setvar_forward(Clause_,X_,Y_)
	       ; throw(gecode_error(forward)))
	    ; (X=[]
	      -> Y=[]
	      ;(X=[H1|T1],Y=[H2|T2])
	      -> (new_forward(Clause,H1,H2),
		  new_forward(Clause,T1,T2))
	      ; throw(gecode_error(forward)))))).

new_intvars_(L,Space,N,I,J) :- length(L,N), new_intvars(L,Space,I,J).
new_intvars_(L,Space,N,IntSet) :- length(L,N), new_intvars(L,Space,IntSet).
new_boolvars_(L,Space,N) :- length(L,N), new_boolvars(L,Space).
new_setvars_(L,Space,N,X1,X2,X3,X4,X5,X6) :- length(L,N), new_setvars(L,Space,X1,X2,X3,X4,X5,X6).
new_setvars_(L,Space,N,X1,X2,X3,X4,X5) :- length(L,N), new_setvars(L,Space,X1,X2,X3,X4,X5).
new_setvars_(L,Space,N,X1,X2,X3,X4) :- length(L,N), new_setvars(L,Space,X1,X2,X3,X4).
new_setvars_(L,Space,N,X1,X2,X3) :- length(L,N), new_setvars(L,Space,X1,X2,X3).
new_setvars_(L,Space,N,X1,X2) :- length(L,N), new_setvars(L,Space,X1,X2).

keep_(Space, Var) :-
    (Var = 'IntVar'(I,J)
    -> (J = -1 -> (gecode_intvar_keep(Space,I,K),setarg(2,Var,K))
       ; throw(gecode_error(variable_already_kept(Var))))
    ; (Var = 'BoolVar'(I,J)
      -> (J = -1 -> (gecode_boolvar_keep(Space,I,K),setarg(2,Var,K))
	 ; throw(gecode_error(variable_already_kept(Var))))
      ; (Var = 'SetVar'(I,J)
	-> (J = -1 -> (gecode_setvar_keep(Space,I,K),setarg(2,Var,K))
	   ; throw(gecode_error(variable_already_kept(Var))))
	; keep_list_(Space,Var)))).

keep_list_(_Space, []) :- !.
keep_list_(Space, [H|T]) :- !,
    keep_(Space,H), keep_list_(Space,T).
keep_list_(_, X) :-
    throw(gecode_error(not_a_variable(X))).

%% more concise interface:
(X := Y) :- var(Y), !, throw(gecode_error((X := Y))).
(X := intset(I,J)) :- !, new_intset(X,I,J).
(X := intset(L)) :- !, new_intset(X,L).
(X := space) :- !, new_space(X).
(X := intvar(Space,I,J)) :- !, new_intvar(X,Space,I,J).
(X := intvar(Space,IntSet)) :- !, new_intvar(X,Space,IntSet).
(X := boolvar(Space)) :- !, new_boolvar(X,Space).
(X := setvar(Space,X1,X2,X3,X4,X5,X6)) :- !, new_setvar(X,Space,X1,X2,X3,X4,X5,X6).
(X := setvar(Space,X1,X2,X3,X4,X5)) :- !, new_setvar(X,Space,X1,X2,X3,X4,X5).
(X := setvar(Space,X1,X2,X3,X4)) :- !, new_setvar(X,Space,X1,X2,X3,X4).
(X := setvar(Space,X1,X2,X3)) :- !, new_setvar(X,Space,X1,X2,X3).
(X := setvar(Space,X1,X2)) :- !, new_setvar(X,Space,X1,X2).
(X := intvars(Space,N,I,J)) :- !, new_intvars_(X,Space,N,I,J).
(X := intvars(Space,N,IntSet)) :- !, new_intvars_(X,Space,N,IntSet).
(X := boolvars(Space,N)) :- !, new_boolvars_(X,Space,N).
(X := setvars(Space,N,X1,X2,X3,X4,X5,X6)) :- !, new_setvars_(X,Space,N,X1,X2,X3,X4,X5,X6).
(X := setvars(Space,N,X1,X2,X3,X4,X5)) :- !, new_setvars_(X,Space,N,X1,X2,X3,X4,X5).
(X := setvars(Space,N,X1,X2,X3,X4)) :- !, new_setvars_(X,Space,N,X1,X2,X3,X4).
(X := setvars(Space,N,X1,X2,X3)) :- !, new_setvars_(X,Space,N,X1,X2,X3).
(X := setvars(Space,N,X1,X2)) :- !, new_setvars_(X,Space,N,X1,X2).
(X := tupleset(Set)) :- !, new_tupleset(X, Set).
(X := dfa(S0, Transitions, Finals)) :- !, new_dfa(X, S0, Transitions, Finals).

(X := min(Space,Var)) :- !, get_min(X,Space,Var).
(X := max(Space,Var)) :- !, get_max(X,Space,Var).
(X := med(Space,Var)) :- !, get_med(X,Space,Var).
(X := val(Space,Var)) :- !, get_val(X,Space,Var).
(X := size(Space,Var)) :- !, get_size(X,Space,Var).
(X := width(Space,Var)) :- !, get_width(X,Space,Var).
(X := regret_min(Space,Var)) :- !, get_regret_min(X,Space,Var).
(X := regret_max(Space,Var)) :- !, get_regret_max(X,Space,Var).
(X := ranges(Space,Var)) :- !, get_ranges(X,Space,Var).
(X := values(Space,Var)) :- !, get_values(X,Space,Var).

(X := glbSize(Space,Var)) :- !, get_glbSize(X,Space,Var).
(X := lubSize(Space,Var)) :- !, get_lubSize(X,Space,Var).
(X := unknownSize(Space,Var)) :- !, get_unknownSize(X,Space,Var).
(X := cardMin(Space,Var)) :- !, get_cardMin(X,Space,Var).
(X := cardMax(Space,Var)) :- !, get_cardMax(X,Space,Var).
(X := lubMin(Space,Var)) :- !, get_lubMin(X,Space,Var).
(X := lubMax(Space,Var)) :- !, get_lubMax(X,Space,Var).
(X := glbMin(Space,Var)) :- !, get_glbMin(X,Space,Var).
(X := glbMax(Space,Var)) :- !, get_glbMax(X,Space,Var).
(X := glb_ranges(Space,Var)) :- !, get_glb_ranges(X,Space,Var).
(X := lub_ranges(Space,Var)) :- !, get_lub_ranges(X,Space,Var).
(X := unknown_ranges(Space,Var)) :- !, get_unknown_ranges(X,Space,Var).
(X := glb_values(Space,Var)) :- !, get_glb_values(X,Space,Var).
(X := lub_values(Space,Var)) :- !, get_lub_values(X,Space,Var).
(X := unknown_values(Space,Var)) :- !, get_unknown_values(X,Space,Var).

(X := disjunctor(Space)) :- !, new_disjunctor(X,Space).
(X := clause(Disj)) :- !, new_clause(X,Disj).

(X := search(Y)) :- !, search(Y,X).
(X := search(Y,L)) :- !, search(Y,X,L).

% these should be autogenerated:
(C += forward(X,Y)) :- !, new_forward(C,X,Y).
(Space += abs(X1,X2)) :- !, abs(Space,X1,X2).
(Space += abs(X1,X2,X3)) :- !, abs(Space,X1,X2,X3).
(Space += assign(X1,X2)) :- !, assign(Space,X1,X2).
(Space += atmostOne(X1,X2)) :- !, atmostOne(Space,X1,X2).
(Space += binpacking(X1,X2,X3)) :- !, binpacking(Space,X1,X2,X3).
(Space += binpacking(X1,X2,X3,X4)) :- !, binpacking(Space,X1,X2,X3,X4).
(Space += branch(X1,X2)) :- !, branch(Space,X1,X2).
(Space += branch(X1,X2,X3)) :- !, branch(Space,X1,X2,X3).
(Space += cardinality(X1,X2)) :- !, cardinality(Space,X1,X2).
(Space += cardinality(X1,X2,X3)) :- !, cardinality(Space,X1,X2,X3).
(Space += channel(X1,X2)) :- !, channel(Space,X1,X2).
(Space += channel(X1,X2,X3)) :- !, channel(Space,X1,X2,X3).
(Space += channel(X1,X2,X3,X4)) :- !, channel(Space,X1,X2,X3,X4).
(Space += channel(X1,X2,X3,X4,X5)) :- !, channel(Space,X1,X2,X3,X4,X5).
(Space += channelSorted(X1,X2)) :- !, channelSorted(Space,X1,X2).
(Space += circuit(X1)) :- !, circuit(Space,X1).
(Space += circuit(X1,X2)) :- !, circuit(Space,X1,X2).
(Space += circuit(X1,X2,X3)) :- !, circuit(Space,X1,X2,X3).
(Space += circuit(X1,X2,X3,X4)) :- !, circuit(Space,X1,X2,X3,X4).
(Space += circuit(X1,X2,X3,X4,X5)) :- !, circuit(Space,X1,X2,X3,X4,X5).
(Space += circuit(X1,X2,X3,X4,X5,X6)) :- !, circuit(Space,X1,X2,X3,X4,X5,X6).
(Space += clause(X1,X2,X3,X4)) :- !, clause(Space,X1,X2,X3,X4).
(Space += clause(X1,X2,X3,X4,X5)) :- !, clause(Space,X1,X2,X3,X4,X5).
(Space += convex(X1)) :- !, convex(Space,X1).
(Space += convex(X1,X2)) :- !, convex(Space,X1,X2).
(Space += count(X1,X2)) :- !, count(Space,X1,X2).
(Space += count(X1,X2,X3)) :- !, count(Space,X1,X2,X3).
(Space += count(X1,X2,X3,X4)) :- !, count(Space,X1,X2,X3,X4).
(Space += count(X1,X2,X3,X4,X5)) :- !, count(Space,X1,X2,X3,X4,X5).
(Space += cumulative(X1,X2,X3,X4)) :- !, cumulative(Space,X1,X2,X3,X4).
(Space += cumulative(X1,X2,X3,X4,X5)) :- !, cumulative(Space,X1,X2,X3,X4,X5).
(Space += cumulative(X1,X2,X3,X4,X5,X6)) :- !, cumulative(Space,X1,X2,X3,X4,X5,X6).
(Space += cumulative(X1,X2,X3,X4,X5,X6,X7)) :- !, cumulative(Space,X1,X2,X3,X4,X5,X6,X7).
(Space += cumulatives(X1,X2,X3,X4,X5,X6,X7)) :- !, cumulatives(Space,X1,X2,X3,X4,X5,X6,X7).
(Space += cumulatives(X1,X2,X3,X4,X5,X6,X7,X8)) :- !, cumulatives(Space,X1,X2,X3,X4,X5,X6,X7,X8).
(Space += distinct(X1)) :- !, distinct(Space,X1).
(Space += distinct(X1,X2)) :- !, distinct(Space,X1,X2).
(Space += distinct(X1,X2,X3)) :- !, distinct(Space,X1,X2,X3).
(Space += div(X1,X2,X3)) :- !, div(Space,X1,X2,X3).
(Space += div(X1,X2,X3,X4)) :- !, div(Space,X1,X2,X3,X4).
(Space += divmod(X1,X2,X3,X4)) :- !, divmod(Space,X1,X2,X3,X4).
(Space += divmod(X1,X2,X3,X4,X5)) :- !, divmod(Space,X1,X2,X3,X4,X5).
(Space += dom(X1,X2)) :- !, dom(Space,X1,X2).
(Space += dom(X1,X2,X3)) :- !, dom(Space,X1,X2,X3).
(Space += dom(X1,X2,X3,X4)) :- !, dom(Space,X1,X2,X3,X4).
(Space += dom(X1,X2,X3,X4,X5)) :- !, dom(Space,X1,X2,X3,X4,X5).
(Space += element(X1,X2,X3)) :- !, element(Space,X1,X2,X3).
(Space += element(X1,X2,X3,X4)) :- !, element(Space,X1,X2,X3,X4).
(Space += element(X1,X2,X3,X4,X5)) :- !, element(Space,X1,X2,X3,X4,X5).
(Space += element(X1,X2,X3,X4,X5,X6)) :- !, element(Space,X1,X2,X3,X4,X5,X6).
(Space += element(X1,X2,X3,X4,X5,X6,X7)) :- !, element(Space,X1,X2,X3,X4,X5,X6,X7).
(Space += extensional(X1,X2)) :- !, extensional(Space,X1,X2).
(Space += extensional(X1,X2,X3)) :- !, extensional(Space,X1,X2,X3).
(Space += linear(X1,X2,X3)) :- !, linear(Space,X1,X2,X3).
(Space += linear(X1,X2,X3,X4)) :- !, linear(Space,X1,X2,X3,X4).
(Space += linear(X1,X2,X3,X4,X5)) :- !, linear(Space,X1,X2,X3,X4,X5).
(Space += linear(X1,X2,X3,X4,X5,X6)) :- !, linear(Space,X1,X2,X3,X4,X5,X6).
(Space += max(X1,X2)) :- !, max(Space,X1,X2).
(Space += max(X1,X2,X3)) :- !, max(Space,X1,X2,X3).
(Space += max(X1,X2,X3,X4)) :- !, max(Space,X1,X2,X3,X4).
(Space += min(X1,X2)) :- !, min(Space,X1,X2).
(Space += min(X1,X2,X3)) :- !, min(Space,X1,X2,X3).
(Space += min(X1,X2,X3,X4)) :- !, min(Space,X1,X2,X3,X4).
(Space += mod(X1,X2,X3)) :- !, mod(Space,X1,X2,X3).
(Space += mod(X1,X2,X3,X4)) :- !, mod(Space,X1,X2,X3,X4).
(Space += mult(X1,X2,X3)) :- !, mult(Space,X1,X2,X3).
(Space += mult(X1,X2,X3,X4)) :- !, mult(Space,X1,X2,X3,X4).
(Space += nooverlap(X1,X2,X3,X4)) :- !, nooverlap(Space,X1,X2,X3,X4).
(Space += nooverlap(X1,X2,X3,X4,X5)) :- !, nooverlap(Space,X1,X2,X3,X4,X5).
(Space += nooverlap(X1,X2,X3,X4,X5,X6)) :- !, nooverlap(Space,X1,X2,X3,X4,X5,X6).
(Space += nooverlap(X1,X2,X3,X4,X5,X6,X7)) :- !, nooverlap(Space,X1,X2,X3,X4,X5,X6,X7).
(Space += nooverlap(X1,X2,X3,X4,X5,X6,X7,X8)) :- !, nooverlap(Space,X1,X2,X3,X4,X5,X6,X7,X8).
(Space += notMax(X1,X2)) :- !, notMax(Space,X1,X2).
(Space += notMin(X1,X2)) :- !, notMin(Space,X1,X2).
(Space += path(X1,X2,X3)) :- !, path(Space,X1,X2,X3).
(Space += path(X1,X2,X3,X4)) :- !, path(Space,X1,X2,X3,X4).
(Space += path(X1,X2,X3,X4,X5)) :- !, path(Space,X1,X2,X3,X4,X5).
(Space += path(X1,X2,X3,X4,X5,X6)) :- !, path(Space,X1,X2,X3,X4,X5,X6).
(Space += path(X1,X2,X3,X4,X5,X6,X7)) :- !, path(Space,X1,X2,X3,X4,X5,X6,X7).
(Space += path(X1,X2,X3,X4,X5,X6,X7,X8)) :- !, path(Space,X1,X2,X3,X4,X5,X6,X7,X8).
(Space += precede(X1,X2)) :- !, precede(Space,X1,X2).
(Space += precede(X1,X2,X3)) :- !, precede(Space,X1,X2,X3).
(Space += precede(X1,X2,X3,X4)) :- !, precede(Space,X1,X2,X3,X4).
(Space += reify(X1,X2,X3)) :- !, reify(Space,X1,X2,X3).
(Space += rel(X1,X2)) :- !, rel(Space,X1,X2).
(Space += rel(X1,X2,X3)) :- !, rel(Space,X1,X2,X3).
(Space += rel(X1,X2,X3,X4)) :- !, rel(Space,X1,X2,X3,X4).
(Space += rel(X1,X2,X3,X4,X5)) :- !, rel(Space,X1,X2,X3,X4,X5).
(Space += rel(X1,X2,X3,X4,X5,X6)) :- !, rel(Space,X1,X2,X3,X4,X5,X6).
(Space += sequence(X1)) :- !, sequence(Space,X1).
(Space += sequence(X1,X2)) :- !, sequence(Space,X1,X2).
(Space += sequence(X1,X2,X3,X4,X5)) :- !, sequence(Space,X1,X2,X3,X4,X5).
(Space += sequence(X1,X2,X3,X4,X5,X6)) :- !, sequence(Space,X1,X2,X3,X4,X5,X6).
(Space += sorted(X1,X2)) :- !, sorted(Space,X1,X2).
(Space += sorted(X1,X2,X3)) :- !, sorted(Space,X1,X2,X3).
(Space += sorted(X1,X2,X3,X4)) :- !, sorted(Space,X1,X2,X3,X4).
(Space += sqr(X0,X1,X2,X3)) :- !, sqr(Space,X0,X1,X2,X3).
(Space += sqr(X1,X2)) :- !, sqr(Space,X1,X2).
(Space += sqrt(X1,X2)) :- !, sqrt(Space,X1,X2).
(Space += sqrt(X1,X2,X3)) :- !, sqrt(Space,X1,X2,X3).
(Space += unary(X1,X2)) :- !, unary(Space,X1,X2).
(Space += unary(X1,X2,X3)) :- !, unary(Space,X1,X2,X3).
(Space += unary(X1,X2,X3,X4)) :- !, unary(Space,X1,X2,X3,X4).
(Space += unary(X1,X2,X3,X4,X5)) :- !, unary(Space,X1,X2,X3,X4,X5).
(Space += unshare(X1)) :- !, unshare(Space,X1).
(Space += unshare(X1,X2)) :- !, unshare(Space,X1,X2).
(Space += weights(X1,X2,X3,X4)) :- !; weights(Space,X1,X2,X3,X4).

(Space += minimize(X)) :- !, minimize(Space,X).
(Space += maximize(X)) :- !, maximize(Space,X).
(Space += minimize(X,Y)) :- !, minimize(Space,X,Y).
(Space += maximize(X,Y)) :- !, maximize(Space,X,Y).

(Space += keep(X)) :- !, keep_(Space,X).

%! @}

