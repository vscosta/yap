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

/** @addtogroup Gecode4 Gecode Interface

@ingroup packages
@{


The gecode library intreface was designed and implemented by Denis
Duchier, with recent work by Vítor Santos Costa to port it to version 4
of gecode and to have an higher level interface,


 @addtogroup TheGecode4Interface The Gecode Interface
@ingroup Gecode4
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

:- module(gecode, [(:=)/2, op(500, xfx, ':='),
		   (+=)/2, op(500, xfx, '+=')]).


:- use_module(library(debug)).

:- op(500, xfx, ':=').
:- op(500, xfx, '+=').
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

is_RestartMode_('RM_NONE').
is_RestartMode_('RM_CONSTANT').
is_RestartMode_('RM_LINEAR').
is_RestartMode_('RM_LUBY').
is_RestartMode_('RM_GEOMETRIC').

is_RestartMode_('RM_NONE','RM_NONE').
is_RestartMode_('RM_CONSTANT','RM_CONSTANT').
is_RestartMode_('RM_LINEAR','RM_LINEAR').
is_RestartMode_('RM_LUBY','RM_LUBY').
is_RestartMode_('RM_GEOMETRIC','RM_GEOMETRIC').

is_RestartMode(X,Y) :- nonvar(X), is_RestartMode_(X,Y).
is_RestartMode(X) :- is_RestartMode(X,_).

is_FloatRelType_('FRT_EQ').
is_FloatRelType_('FRT_NQ').
is_FloatRelType_('FRT_LQ').
is_FloatRelType_('FRT_LE').
is_FloatRelType_('FRT_GQ').
is_FloatRelType_('FRT_GR').

is_FloatRelType_('FRT_EQ','FRT_EQ').
is_FloatRelType_('FRT_NQ','FRT_NQ').
is_FloatRelType_('FRT_LQ','FRT_LQ').
is_FloatRelType_('FRT_LE','FRT_LE').
is_FloatRelType_('FRT_GQ','FRT_GQ').
is_FloatRelType_('FRT_GR','FRT_GR').

is_FloatRelType(X,Y) :- nonvar(X), is_FloatRelType_(X,Y).
is_FloatRelType(X) :- is_FloatRelType(X,_).

is_ReifyMode_('RM_EQV').
is_ReifyMode_('RM_IMP').
is_ReifyMode_('RM_PMI').

is_ReifyMode_('RM_EQV','RM_EQV').
is_ReifyMode_('RM_IMP','RM_IMP').
is_ReifyMode_('RM_PMI','RM_PMI').

is_ReifyMode(X,Y) :- nonvar(X), is_ReifyMode_(X,Y).
is_ReifyMode(X) :- is_ReifyMode(X,_).

is_IntRelType_('IRT_EQ').
is_IntRelType_('IRT_NQ').
is_IntRelType_('IRT_LQ').
is_IntRelType_('IRT_LE').
is_IntRelType_('IRT_GQ').
is_IntRelType_('IRT_GR').

is_IntRelType_('IRT_EQ','IRT_EQ').
is_IntRelType_('IRT_NQ','IRT_NQ').
is_IntRelType_('IRT_LQ','IRT_LQ').
is_IntRelType_('IRT_LE','IRT_LE').
is_IntRelType_('IRT_GQ','IRT_GQ').
is_IntRelType_('IRT_GR','IRT_GR').

is_IntRelType(X,Y) :- nonvar(X), is_IntRelType_(X,Y).
is_IntRelType(X) :- is_IntRelType(X,_).

is_BoolOpType_('BOT_AND').
is_BoolOpType_('BOT_OR').
is_BoolOpType_('BOT_IMP').
is_BoolOpType_('BOT_EQV').
is_BoolOpType_('BOT_XOR').

is_BoolOpType_('BOT_AND','BOT_AND').
is_BoolOpType_('BOT_OR','BOT_OR').
is_BoolOpType_('BOT_IMP','BOT_IMP').
is_BoolOpType_('BOT_EQV','BOT_EQV').
is_BoolOpType_('BOT_XOR','BOT_XOR').

is_BoolOpType(X,Y) :- nonvar(X), is_BoolOpType_(X,Y).
is_BoolOpType(X) :- is_BoolOpType(X,_).

is_IntConLevel_('ICL_VAL').
is_IntConLevel_('ICL_BND').
is_IntConLevel_('ICL_DOM').
is_IntConLevel_('ICL_DEF').

is_IntConLevel_('ICL_VAL','ICL_VAL').
is_IntConLevel_('ICL_BND','ICL_BND').
is_IntConLevel_('ICL_DOM','ICL_DOM').
is_IntConLevel_('ICL_DEF','ICL_DEF').

is_IntConLevel(X,Y) :- nonvar(X), is_IntConLevel_(X,Y).
is_IntConLevel(X) :- is_IntConLevel(X,_).

is_TaskType_('TT_FIXP').
is_TaskType_('TT_FIXS').
is_TaskType_('TT_FIXE').

is_TaskType_('TT_FIXP','TT_FIXP').
is_TaskType_('TT_FIXS','TT_FIXS').
is_TaskType_('TT_FIXE','TT_FIXE').

is_TaskType(X,Y) :- nonvar(X), is_TaskType_(X,Y).
is_TaskType(X) :- is_TaskType(X,_).

is_ExtensionalPropKind_('EPK_DEF').
is_ExtensionalPropKind_('EPK_SPEED').
is_ExtensionalPropKind_('EPK_MEMORY').

is_ExtensionalPropKind_('EPK_DEF','EPK_DEF').
is_ExtensionalPropKind_('EPK_SPEED','EPK_SPEED').
is_ExtensionalPropKind_('EPK_MEMORY','EPK_MEMORY').

is_ExtensionalPropKind(X,Y) :- nonvar(X), is_ExtensionalPropKind_(X,Y).
is_ExtensionalPropKind(X) :- is_ExtensionalPropKind(X,_).

is_SetRelType_('SRT_EQ').
is_SetRelType_('SRT_NQ').
is_SetRelType_('SRT_SUB').
is_SetRelType_('SRT_SUP').
is_SetRelType_('SRT_DISJ').
is_SetRelType_('SRT_CMPL').
is_SetRelType_('SRT_LQ').
is_SetRelType_('SRT_LE').
is_SetRelType_('SRT_GQ').
is_SetRelType_('SRT_GR').

is_SetRelType_('SRT_EQ','SRT_EQ').
is_SetRelType_('SRT_NQ','SRT_NQ').
is_SetRelType_('SRT_SUB','SRT_SUB').
is_SetRelType_('SRT_SUP','SRT_SUP').
is_SetRelType_('SRT_DISJ','SRT_DISJ').
is_SetRelType_('SRT_CMPL','SRT_CMPL').
is_SetRelType_('SRT_LQ','SRT_LQ').
is_SetRelType_('SRT_LE','SRT_LE').
is_SetRelType_('SRT_GQ','SRT_GQ').
is_SetRelType_('SRT_GR','SRT_GR').

is_SetRelType(X,Y) :- nonvar(X), is_SetRelType_(X,Y).
is_SetRelType(X) :- is_SetRelType(X,_).

is_SetOpType_('SOT_UNION').
is_SetOpType_('SOT_DUNION').
is_SetOpType_('SOT_INTER').
is_SetOpType_('SOT_MINUS').

is_SetOpType_('SOT_UNION','SOT_UNION').
is_SetOpType_('SOT_DUNION','SOT_DUNION').
is_SetOpType_('SOT_INTER','SOT_INTER').
is_SetOpType_('SOT_MINUS','SOT_MINUS').

is_SetOpType(X,Y) :- nonvar(X), is_SetOpType_(X,Y).
is_SetOpType(X) :- is_SetOpType(X,_).

unary(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_BoolVarArgs(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_unary_458(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_BoolVarArgs(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_unary_454(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4,X5),arg=1))).

nvalues(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_nvalues_345(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_nvalues_347(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_nvalues_341(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_IntVar(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_nvalues_343(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3,X4),arg=1))).

max(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_max_302(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_max_300(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
                 ;  (is_FloatVar(X1,Y1)
                     -> (is_FloatVar(X2,Y2)
                         -> (is_FloatVar(X3,Y3)
                             -> gecode_constraint_max_301(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_Reify(X3,Y3)
                                 -> gecode_constraint_max_305(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=3)))
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=2))))))
         ;  throw(gecode_argument_error(max(X0,X1,X2,X3),arg=1))).

dom(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_dom_200(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_SetVar(X1,Y1)
                 -> (is_SetRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> gecode_constraint_dom_209(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4,X5),arg=1))).

convex(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_convex_91(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(convex(X0,X1,X2),arg=1))).

nooverlap(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> gecode_constraint_nooverlap_329(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4),arg=1))).

assign(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatAssign(X2,Y2)
                 -> gecode_constraint_assign_15(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatAssign(X2,Y2)
                     -> gecode_constraint_assign_6(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> gecode_constraint_assign_3(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntAssign(X2,Y2)
                             -> gecode_constraint_assign_1(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_IntAssign(X2,Y2)
                                 -> gecode_constraint_assign_9(Y0,Y1,Y2)
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_IntAssign(X2,Y2)
                                     -> gecode_constraint_assign_17(Y0,Y1,Y2)
                                     ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                                 ;  (is_SetVarArgs(X1,Y1)
                                     -> (is_SetAssign(X2,Y2)
                                         -> gecode_constraint_assign_12(Y0,Y1,Y2)
                                         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_SetAssign(X2,Y2)
                                             -> gecode_constraint_assign_19(Y0,Y1,Y2)
                                             ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=3)))
                                         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=2))))))))))
         ;  throw(gecode_argument_error(assign(X0,X1,X2),arg=1))).

element(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_element_220(Y0,Y1,Y2,Y3)
                     ;  (is_IntVar(X3,Y3)
                         -> gecode_constraint_element_224(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_element_214(Y0,Y1,Y2,Y3)
                         ;  (is_BoolVar(X3,Y3)
                             -> gecode_constraint_element_212(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                 ;  (is_IntSetArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> gecode_constraint_element_219(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> gecode_constraint_element_227(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntArgs(X1,Y1)
                             -> (is_IntVar(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> gecode_constraint_element_230(Y0,Y1,Y2,Y3)
                                     ;  (is_IntVar(X3,Y3)
                                         -> gecode_constraint_element_236(Y0,Y1,Y2,Y3)
                                         ;  (is_BoolVar(X3,Y3)
                                             -> gecode_constraint_element_228(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=4)))))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=3)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=2)))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3),arg=1))).

sequence(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> gecode_constraint_sequence_437(Y0,Y1)
             ;  throw(gecode_argument_error(sequence(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(sequence(X0,X1),arg=1))).

notMax(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_notMax_335(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(notMax(X0,X1,X2),arg=1))).

ite(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_ite_256(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4),arg=1))).

unary(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> gecode_constraint_unary_451(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(unary(X0,X1,X2),arg=1))).

nroot(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_nroot_339(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_circuit_85(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> gecode_constraint_circuit_76(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3),arg=1))).

dom(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> gecode_constraint_dom_199(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_198(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_Reify(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_202(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4))))
                 ;  (is_IntSet(X2,Y2)
                     -> (is_Reify(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_dom_194(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3))))
             ;  (is_SetVarArgs(X1,Y1)
                 -> (is_SetRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> gecode_constraint_dom_185(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_int(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_dom_181(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                     ;  (is_FloatVar(X1,Y1)
                         -> (is_FloatNum(X2,Y2)
                             -> (is_FloatNum(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> gecode_constraint_dom_187(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                         ;  (is_SetVar(X1,Y1)
                             -> (is_SetRelType(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> (is_int(X4,Y4)
                                         -> gecode_constraint_dom_208(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_Reify(X4,Y4)
                                             -> gecode_constraint_dom_210(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5))))
                                     ;  (is_IntSet(X3,Y3)
                                         -> (is_Reify(X4,Y4)
                                             -> gecode_constraint_dom_206(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=4))))
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=3)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=2)))))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3,X4),arg=1))).

channel(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_channel_66(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_channel_62(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
                 ;  (is_BoolVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_channel_60(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_BoolVarArgs(X7,Y7)
                                     -> gecode_constraint_nooverlap_331(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_nooverlap_334(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=8))))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

element(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> gecode_constraint_element_222(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_BoolVar(X6,Y6)
                                     -> gecode_constraint_element_216(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                 ;  (is_IntSetArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> (is_int(X5,Y5)
                                     -> (is_SetVar(X6,Y6)
                                         -> gecode_constraint_element_218(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_IntVar(X4,Y4)
                                     -> (is_int(X5,Y5)
                                         -> (is_SetVar(X6,Y6)
                                             -> gecode_constraint_element_226(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                         ;  (is_IntArgs(X1,Y1)
                             -> (is_IntVar(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> (is_IntVar(X4,Y4)
                                         -> (is_int(X5,Y5)
                                             -> (is_IntVar(X6,Y6)
                                                 -> gecode_constraint_element_234(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                                 ;  (is_BoolVar(X6,Y6)
                                                     -> gecode_constraint_element_232(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=2)))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6),arg=1))).

max(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatVar(X2,Y2)
                 -> gecode_constraint_max_298(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(max(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> gecode_constraint_max_299(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(max(X0,X1,X2),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> gecode_constraint_max_304(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(max(X0,X1,X2),arg=3)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2),arg=2)))))
         ;  throw(gecode_argument_error(max(X0,X1,X2),arg=1))).

unshare(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_unshare_463(Y0,Y1)
             ;  (is_BoolVarArgs(X1,Y1)
                 -> gecode_constraint_unshare_461(Y0,Y1)
                 ;  throw(gecode_argument_error(unshare(X0,X1),arg=2))))
         ;  throw(gecode_argument_error(unshare(X0,X1),arg=1))).

path(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_path_358(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_path_357(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4),arg=1))).

branch(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarBranch(X2,Y2)
                 -> (is_IntValBranch(X3,Y3)
                     -> (is_Symmetries(X4,Y4)
                         -> (is_IntBranchFilter(X5,Y5)
                             -> (is_IntVarValPrint(X6,Y6)
                                 -> gecode_constraint_branch_34(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVarBranch(X2,Y2)
                     -> (is_IntValBranch(X3,Y3)
                         -> (is_Symmetries(X4,Y4)
                             -> (is_BoolBranchFilter(X5,Y5)
                                 -> (is_BoolVarValPrint(X6,Y6)
                                     -> gecode_constraint_branch_28(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                 ;  (is_SetVarArgs(X1,Y1)
                     -> (is_SetVarBranch(X2,Y2)
                         -> (is_SetValBranch(X3,Y3)
                             -> (is_Symmetries(X4,Y4)
                                 -> (is_SetBranchFilter(X5,Y5)
                                     -> (is_SetVarValPrint(X6,Y6)
                                         -> gecode_constraint_branch_40(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=2)))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5,X6),arg=1))).

mult(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_mult_325(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatVar(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> gecode_constraint_mult_324(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3),arg=1))).

clause(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_clause_89(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_BoolVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_clause_87(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4,X5),arg=1))).

precede(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_precede_366(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3,X4),arg=1))).

distinct(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_distinct_160(Y0,Y1)
             ;  throw(gecode_argument_error(distinct(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(distinct(X0,X1),arg=1))).

member(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_Reify(X3,Y3)
                     -> gecode_constraint_member_312(Y0,Y1,Y2,Y3)
                     ;  (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_member_311(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_BoolVar(X2,Y2)
                     -> (is_Reify(X3,Y3)
                         -> gecode_constraint_member_308(Y0,Y1,Y2,Y3)
                         ;  (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_member_307(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(member(X0,X1,X2,X3),arg=1))).

mod(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_mod_323(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3,X4),arg=1))).

cardinality(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_cardinality_57(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(cardinality(X0,X1,X2),arg=1))).

atmostOne(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> gecode_constraint_atmostOne_53(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(atmostOne(X0,X1,X2),arg=1))).

channelSorted(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_channelSorted_73(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(channelSorted(X0,X1,X2),arg=1))).

extensional(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_TupleSet(X2,Y2)
                 -> (is_ExtensionalPropKind(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_extensional_253(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_TupleSet(X2,Y2)
                     -> (is_ExtensionalPropKind(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_extensional_248(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3,X4),arg=1))).

linear(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatRelType(X2,Y2)
                 -> (is_FloatVar(X3,Y3)
                     -> gecode_constraint_linear_272(Y0,Y1,Y2,Y3)
                     ;  (is_FloatNum(X3,Y3)
                         -> gecode_constraint_linear_270(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_linear_258(Y0,Y1,Y2,Y3)
                         ;  (is_IntVar(X3,Y3)
                             -> gecode_constraint_linear_262(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> gecode_constraint_linear_290(Y0,Y1,Y2,Y3)
                             ;  (is_IntVar(X3,Y3)
                                 -> gecode_constraint_linear_294(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=3)))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=2)))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3),arg=1))).

circuit(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> gecode_constraint_circuit_82(Y0,Y1)
             ;  throw(gecode_argument_error(circuit(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1),arg=1))).

rel(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatRelType(X2,Y2)
                 -> (is_FloatVal(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> gecode_constraint_rel_406(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                     ;  (is_FloatVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> gecode_constraint_rel_408(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_SetOpType(X1,Y1)
                 -> (is_SetVarArgs(X2,Y2)
                     -> (is_IntSet(X3,Y3)
                         -> (is_SetVar(X4,Y4)
                             -> gecode_constraint_rel_422(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntSet(X3,Y3)
                             -> (is_SetVar(X4,Y4)
                                 -> gecode_constraint_rel_420(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_rel_392(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                             ;  (is_BoolVarArgs(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_rel_388(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                 ;  (is_BoolVar(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_rel_386(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> gecode_constraint_rel_383(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_rel_382(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                 ;  (is_BoolVar(X3,Y3)
                                     -> (is_Reify(X4,Y4)
                                         -> gecode_constraint_rel_379(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_rel_378(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                             ;  (is_BoolOpType(X2,Y2)
                                 -> (is_BoolVar(X3,Y3)
                                     -> (is_int(X4,Y4)
                                         -> gecode_constraint_rel_375(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_BoolVar(X4,Y4)
                                             -> gecode_constraint_rel_373(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_rel_402(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                     ;  (is_IntVar(X3,Y3)
                                         -> (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_rel_404(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                         ;  (is_IntVarArgs(X3,Y3)
                                             -> (is_IntConLevel(X4,Y4)
                                                 -> gecode_constraint_rel_398(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_IntRelType(X2,Y2)
                                     -> (is_int(X3,Y3)
                                         -> (is_Reify(X4,Y4)
                                             -> gecode_constraint_rel_411(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_IntConLevel(X4,Y4)
                                                 -> gecode_constraint_rel_410(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                         ;  (is_IntVar(X3,Y3)
                                             -> (is_Reify(X4,Y4)
                                                 -> gecode_constraint_rel_415(Y0,Y1,Y2,Y3,Y4)
                                                 ;  (is_IntConLevel(X4,Y4)
                                                     -> gecode_constraint_rel_414(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5))))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                     ;  (is_SetRelType(X2,Y2)
                                         -> (is_SetVar(X3,Y3)
                                             -> (is_Reify(X4,Y4)
                                                 -> gecode_constraint_rel_419(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3))))
                                 ;  (is_SetVar(X1,Y1)
                                     -> (is_SetRelType(X2,Y2)
                                         -> (is_IntVar(X3,Y3)
                                             -> (is_Reify(X4,Y4)
                                                 -> gecode_constraint_rel_430(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                             ;  (is_SetVar(X3,Y3)
                                                 -> (is_Reify(X4,Y4)
                                                     -> gecode_constraint_rel_432(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                                     ;  (is_BoolOpType(X1,Y1)
                                         -> (is_BoolVarArgs(X2,Y2)
                                             -> (is_int(X3,Y3)
                                                 -> (is_IntConLevel(X4,Y4)
                                                     -> gecode_constraint_rel_372(Y0,Y1,Y2,Y3,Y4)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                                 ;  (is_BoolVar(X3,Y3)
                                                     -> (is_IntConLevel(X4,Y4)
                                                         -> gecode_constraint_rel_370(Y0,Y1,Y2,Y3,Y4)
                                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=5)))
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=4))))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=3)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=2))))))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4),arg=1))).

min(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_min_318(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_min_316(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
                 ;  (is_FloatVar(X1,Y1)
                     -> (is_FloatVar(X2,Y2)
                         -> (is_FloatVar(X3,Y3)
                             -> gecode_constraint_min_317(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_Reify(X3,Y3)
                                 -> gecode_constraint_min_321(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=3)))
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=2))))))
         ;  throw(gecode_argument_error(min(X0,X1,X2,X3),arg=1))).

cardinality(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_cardinality_56(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=3)))
             ;  (is_SetVar(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_cardinality_58(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(cardinality(X0,X1,X2,X3),arg=1))).

count(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_count_109(Y0,Y1,Y2,Y3)
                     ;  (is_IntArgs(X3,Y3)
                         -> gecode_constraint_count_106(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4))))
                 ;  (is_IntSet(X2,Y2)
                     -> (is_IntArgs(X3,Y3)
                         -> gecode_constraint_count_100(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntSetArgs(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_count_99(Y0,Y1,Y2,Y3)
                             ;  (is_IntArgs(X3,Y3)
                                 -> gecode_constraint_count_96(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=3)))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3),arg=1))).

sqrt(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_sqrt_447(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatVar(X2,Y2)
                     -> gecode_constraint_sqrt_446(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(sqrt(X0,X1,X2),arg=1))).

cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntArgs(X6,Y6)
                                 -> (is_bool(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_cumulatives_157(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  (is_IntArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_155(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  (is_IntArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_153(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_151(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4))))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> (is_IntConLevel(X8,Y8)
                                             -> gecode_constraint_cumulatives_149(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_147(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntVarArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> (is_IntConLevel(X8,Y8)
                                                 -> gecode_constraint_cumulatives_145(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                     ;  (is_IntArgs(X5,Y5)
                                         -> (is_IntArgs(X6,Y6)
                                             -> (is_bool(X7,Y7)
                                                 -> (is_IntConLevel(X8,Y8)
                                                     -> gecode_constraint_cumulatives_143(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6))))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4))))
                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2))))
         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

nvalues(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_nvalues_344(Y0,Y1,Y2,Y3)
                     ;  (is_IntVar(X3,Y3)
                         -> gecode_constraint_nvalues_346(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_nvalues_340(Y0,Y1,Y2,Y3)
                         ;  (is_IntVar(X3,Y3)
                             -> gecode_constraint_nvalues_342(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=4))))
                     ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(nvalues(X0,X1,X2,X3),arg=1))).

binpacking(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> gecode_constraint_binpacking_54(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3),arg=1))).

linear(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_Reify(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_linear_285(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  (is_IntVar(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_linear_289(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_BoolVarArgs(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_Reify(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_linear_277(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  (is_IntVar(X4,Y4)
                                 -> (is_Reify(X5,Y5)
                                     -> (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_linear_281(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=5))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5,X6),arg=1))).

abs(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_abs_52(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(abs(X0,X1,X2,X3),arg=1))).

convex(X0,X1) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> gecode_constraint_convex_90(Y0,Y1)
             ;  throw(gecode_argument_error(convex(X0,X1),arg=2)))
         ;  throw(gecode_argument_error(convex(X0,X1),arg=1))).

div(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_div_163(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatVar(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> gecode_constraint_div_162(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(div(X0,X1,X2,X3),arg=1))).

rel(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_rel_412(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_rel_416(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntSet(X1,Y1)
                 -> (is_SetOpType(X2,Y2)
                     -> (is_SetVar(X3,Y3)
                         -> (is_SetRelType(X4,Y4)
                             -> (is_IntSet(X5,Y5)
                                 -> gecode_constraint_rel_395(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  (is_SetVar(X5,Y5)
                                     -> gecode_constraint_rel_396(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  (is_BoolVar(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_rel_384(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  (is_BoolVar(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_rel_380(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  (is_BoolOpType(X2,Y2)
                             -> (is_BoolVar(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_rel_376(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  (is_BoolVar(X4,Y4)
                                         -> (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_rel_374(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6)))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3))))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetOpType(X2,Y2)
                             -> (is_IntSet(X3,Y3)
                                 -> (is_SetRelType(X4,Y4)
                                     -> (is_IntSet(X5,Y5)
                                         -> gecode_constraint_rel_425(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_SetVar(X5,Y5)
                                             -> gecode_constraint_rel_426(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  (is_SetVar(X3,Y3)
                                     -> (is_SetRelType(X4,Y4)
                                         -> (is_IntSet(X5,Y5)
                                             -> gecode_constraint_rel_427(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_SetVar(X5,Y5)
                                                 -> gecode_constraint_rel_428(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=5)))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=4))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=3)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=2))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3,X4,X5),arg=1))).

weights(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_SetVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_weights_465(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(weights(X0,X1,X2,X3,X4),arg=1))).

max(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_max_303(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(max(X0,X1,X2,X3,X4),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_IntVar(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_path_353(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

unary(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> gecode_constraint_unary_459(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> gecode_constraint_unary_455(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_BoolVarArgs(X3,Y3)
                             -> gecode_constraint_unary_449(Y0,Y1,Y2,Y3)
                             ;  (is_IntConLevel(X3,Y3)
                                 -> gecode_constraint_unary_452(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=3))))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3),arg=1))).

nroot(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_nroot_338(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> gecode_constraint_nroot_337(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(nroot(X0,X1,X2,X3),arg=1))).

sorted(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_sorted_440(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_circuit_80(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_circuit_77(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_IntVarArgs(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> gecode_constraint_circuit_74(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=3))))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4),arg=1))).

dom(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatVal(X2,Y2)
                 -> (is_Reify(X3,Y3)
                     -> gecode_constraint_dom_189(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                 ;  (is_FloatNum(X2,Y2)
                     -> (is_FloatNum(X3,Y3)
                         -> gecode_constraint_dom_186(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatNum(X2,Y2)
                     -> (is_FloatNum(X3,Y3)
                         -> gecode_constraint_dom_172(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_BoolVarArgs(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_dom_170(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_BoolVar(X2,Y2)
                             -> (is_IntConLevel(X3,Y3)
                                 -> gecode_constraint_dom_168(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_int(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> gecode_constraint_dom_180(Y0,Y1,Y2,Y3)
                                     ;  (is_IntConLevel(X3,Y3)
                                         -> gecode_constraint_dom_179(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                                 ;  (is_IntVarArgs(X2,Y2)
                                     -> (is_IntConLevel(X3,Y3)
                                         -> gecode_constraint_dom_177(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                                     ;  (is_IntSet(X2,Y2)
                                         -> (is_IntConLevel(X3,Y3)
                                             -> gecode_constraint_dom_175(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_int(X2,Y2)
                                     -> (is_int(X3,Y3)
                                         -> gecode_constraint_dom_197(Y0,Y1,Y2,Y3)
                                         ;  (is_Reify(X3,Y3)
                                             -> gecode_constraint_dom_201(Y0,Y1,Y2,Y3)
                                             ;  (is_IntConLevel(X3,Y3)
                                                 -> gecode_constraint_dom_196(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))))
                                     ;  (is_IntVar(X2,Y2)
                                         -> (is_IntConLevel(X3,Y3)
                                             -> gecode_constraint_dom_204(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4)))
                                         ;  (is_IntSet(X2,Y2)
                                             -> (is_Reify(X3,Y3)
                                                 -> gecode_constraint_dom_193(Y0,Y1,Y2,Y3)
                                                 ;  (is_IntConLevel(X3,Y3)
                                                     -> gecode_constraint_dom_192(Y0,Y1,Y2,Y3)
                                                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))))
                                 ;  (is_SetVarArgs(X1,Y1)
                                     -> (is_SetRelType(X2,Y2)
                                         -> (is_int(X3,Y3)
                                             -> gecode_constraint_dom_184(Y0,Y1,Y2,Y3)
                                             ;  (is_IntSet(X3,Y3)
                                                 -> gecode_constraint_dom_183(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_SetRelType(X2,Y2)
                                             -> (is_int(X3,Y3)
                                                 -> gecode_constraint_dom_207(Y0,Y1,Y2,Y3)
                                                 ;  (is_IntSet(X3,Y3)
                                                     -> gecode_constraint_dom_205(Y0,Y1,Y2,Y3)
                                                     ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=4))))
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=3)))
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=2))))))))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2,X3),arg=1))).

abs(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_abs_51(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatVar(X2,Y2)
                     -> gecode_constraint_abs_50(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(abs(X0,X1,X2),arg=1))).

channel(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_channel_68(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_channel_63(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4),arg=1))).

assign(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatAssign(X2,Y2)
                 -> (is_FloatBranchFilter(X3,Y3)
                     -> (is_FloatVarValPrint(X4,Y4)
                         -> gecode_constraint_assign_8(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntAssign(X2,Y2)
                     -> (is_BoolBranchFilter(X3,Y3)
                         -> (is_BoolVarValPrint(X4,Y4)
                             -> gecode_constraint_assign_5(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> (is_IntBranchFilter(X3,Y3)
                             -> (is_IntVarValPrint(X4,Y4)
                                 -> gecode_constraint_assign_11(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetAssign(X2,Y2)
                             -> (is_SetBranchFilter(X3,Y3)
                                 -> (is_SetVarValPrint(X4,Y4)
                                     -> gecode_constraint_assign_14(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=3)))
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=2))))))
         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3,X4),arg=1))).

rel(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> gecode_constraint_rel_399(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> gecode_constraint_rel_389(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2),arg=1))).

path(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_path_356(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3),arg=1))).

branch(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatValBranch(X2,Y2)
                 -> (is_FloatVarValPrint(X3,Y3)
                     -> gecode_constraint_branch_45(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarBranch(X2,Y2)
                     -> (is_FloatValBranch(X3,Y3)
                         -> gecode_constraint_branch_29(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntValBranch(X3,Y3)
                             -> gecode_constraint_branch_23(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntValBranch(X2,Y2)
                             -> (is_BoolVarValPrint(X3,Y3)
                                 -> gecode_constraint_branch_22(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_IntVarBranch(X2,Y2)
                                 -> (is_IntValBranch(X3,Y3)
                                     -> gecode_constraint_branch_35(Y0,Y1,Y2,Y3)
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_IntValBranch(X2,Y2)
                                     -> (is_IntVarValPrint(X3,Y3)
                                         -> gecode_constraint_branch_47(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                                 ;  (is_SetVarArgs(X1,Y1)
                                     -> (is_SetVarBranch(X2,Y2)
                                         -> (is_SetValBranch(X3,Y3)
                                             -> gecode_constraint_branch_41(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_SetValBranch(X2,Y2)
                                             -> (is_SetVarValPrint(X3,Y3)
                                                 -> gecode_constraint_branch_49(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=4)))
                                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=3)))
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=2))))))))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3),arg=1))).

mult(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_mult_326(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(mult(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_circuit_81(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_IntVarArgs(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> gecode_constraint_circuit_78(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_circuit_75(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5),arg=1))).

clause(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolOpType(X1,Y1)
             -> (is_BoolVarArgs(X2,Y2)
                 -> (is_BoolVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_clause_88(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_BoolVar(X4,Y4)
                             -> gecode_constraint_clause_86(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(clause(X0,X1,X2,X3,X4),arg=1))).

precede(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> gecode_constraint_precede_368(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> gecode_constraint_precede_365(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_precede_364(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=3))))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(precede(X0,X1,X2,X3),arg=1))).

channel(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_channel_69(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(channel(X0,X1,X2,X3,X4,X5),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> (is_BoolVarArgs(X6,Y6)
                                 -> gecode_constraint_cumulative_126(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_cumulative_129(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> gecode_constraint_cumulative_122(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_125(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntArgs(X4,Y4)
                                 -> (is_BoolVarArgs(X5,Y5)
                                     -> (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_119(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> gecode_constraint_cumulative_138(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  (is_IntConLevel(X6,Y6)
                                         -> gecode_constraint_cumulative_141(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> (is_BoolVarArgs(X6,Y6)
                                         -> gecode_constraint_cumulative_134(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  (is_IntConLevel(X6,Y6)
                                             -> gecode_constraint_cumulative_137(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7))))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                             ;  (is_IntArgs(X3,Y3)
                                 -> (is_IntArgs(X4,Y4)
                                     -> (is_BoolVarArgs(X5,Y5)
                                         -> (is_IntConLevel(X6,Y6)
                                             -> gecode_constraint_cumulative_131(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=4))))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6),arg=1))).

distinct(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntConLevel(X2,Y2)
                 -> gecode_constraint_distinct_161(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> gecode_constraint_distinct_158(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(distinct(X0,X1,X2),arg=1))).

member(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_Reify(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_member_313(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_BoolVar(X2,Y2)
                     -> (is_Reify(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_member_309(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(member(X0,X1,X2,X3,X4),arg=1))).

mod(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_mod_322(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(mod(X0,X1,X2,X3),arg=1))).

sqr(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_sqr_444(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_FloatVar(X2,Y2)
                     -> gecode_constraint_sqr_443(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(sqr(X0,X1,X2),arg=1))).

sequence(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntSet(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_sequence_436(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntSet(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_sequence_434(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=3)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=2))))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5,X6),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> gecode_constraint_path_354(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_path_351(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  (is_IntVarArgs(X5,Y5)
                                     -> (is_IntVar(X6,Y6)
                                         -> gecode_constraint_path_348(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=6))))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6),arg=1))).

divmod(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_divmod_166(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4,X5),arg=1))).

sorted(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_sorted_441(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2),arg=1))).

extensional(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_TupleSet(X2,Y2)
                 -> (is_ExtensionalPropKind(X3,Y3)
                     -> gecode_constraint_extensional_252(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=4)))
                 ;  (is_DFA(X2,Y2)
                     -> (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_extensional_255(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=3))))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_TupleSet(X2,Y2)
                     -> (is_ExtensionalPropKind(X3,Y3)
                         -> gecode_constraint_extensional_247(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=4)))
                     ;  (is_DFA(X2,Y2)
                         -> (is_IntConLevel(X3,Y3)
                             -> gecode_constraint_extensional_250(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=3))))
                 ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(extensional(X0,X1,X2,X3),arg=1))).

circuit(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_circuit_84(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntConLevel(X2,Y2)
                     -> gecode_constraint_circuit_83(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2),arg=1))).

channel(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_channel_71(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> gecode_constraint_channel_61(Y0,Y1,Y2)
                     ;  (is_SetVar(X2,Y2)
                         -> gecode_constraint_channel_64(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3))))
                 ;  (is_BoolVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> gecode_constraint_channel_59(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
                     ;  (is_IntVarArgs(X1,Y1)
                         -> (is_SetVarArgs(X2,Y2)
                             -> gecode_constraint_channel_67(Y0,Y1,Y2)
                             ;  (is_IntVarArgs(X2,Y2)
                                 -> gecode_constraint_channel_65(Y0,Y1,Y2)
                                 ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3))))
                         ;  (is_IntVar(X1,Y1)
                             -> (is_FloatVar(X2,Y2)
                                 -> gecode_constraint_channel_72(Y0,Y1,Y2)
                                 ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
                             ;  (is_SetVarArgs(X1,Y1)
                                 -> (is_SetVarArgs(X2,Y2)
                                     -> gecode_constraint_channel_70(Y0,Y1,Y2)
                                     ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=3)))
                                 ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=2))))))))
         ;  throw(gecode_argument_error(channel(X0,X1,X2),arg=1))).

count(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> gecode_constraint_count_92(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntVar(X4,Y4)
                             -> gecode_constraint_count_94(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                 ;  (is_int(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> gecode_constraint_count_110(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntVar(X4,Y4)
                                 -> gecode_constraint_count_112(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                     ;  (is_IntSet(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> gecode_constraint_count_102(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_IntVar(X4,Y4)
                                     -> gecode_constraint_count_104(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                             ;  (is_IntArgs(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_count_101(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4))))
                         ;  (is_IntSetArgs(X2,Y2)
                             -> (is_IntArgs(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_count_97(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                             ;  (is_IntVarArgs(X2,Y2)
                                 -> (is_IntArgs(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_count_107(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                                 ;  (is_IntVar(X2,Y2)
                                     -> (is_IntRelType(X3,Y3)
                                         -> (is_int(X4,Y4)
                                             -> gecode_constraint_count_114(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_IntVar(X4,Y4)
                                                 -> gecode_constraint_count_116(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=5))))
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=4)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=3))))))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4),arg=1))).

cumulatives(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntArgs(X6,Y6)
                                 -> (is_bool(X7,Y7)
                                     -> gecode_constraint_cumulatives_156(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  (is_IntArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_154(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  (is_IntArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_152(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_150(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=4))))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntArgs(X6,Y6)
                                     -> (is_bool(X7,Y7)
                                         -> gecode_constraint_cumulatives_148(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  (is_IntArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_146(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntVarArgs(X5,Y5)
                                     -> (is_IntArgs(X6,Y6)
                                         -> (is_bool(X7,Y7)
                                             -> gecode_constraint_cumulatives_144(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  (is_IntArgs(X5,Y5)
                                         -> (is_IntArgs(X6,Y6)
                                             -> (is_bool(X7,Y7)
                                                 -> gecode_constraint_cumulatives_142(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=6))))
                                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=4))))
                     ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                 ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=2))))
         ;  throw(gecode_argument_error(cumulatives(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

binpacking(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_binpacking_55(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(binpacking(X0,X1,X2,X3,X4),arg=1))).

extensional(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_TupleSet(X2,Y2)
                 -> gecode_constraint_extensional_251(Y0,Y1,Y2)
                 ;  (is_DFA(X2,Y2)
                     -> gecode_constraint_extensional_254(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(extensional(X0,X1,X2),arg=3))))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_TupleSet(X2,Y2)
                     -> gecode_constraint_extensional_246(Y0,Y1,Y2)
                     ;  (is_DFA(X2,Y2)
                         -> gecode_constraint_extensional_249(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(extensional(X0,X1,X2),arg=3))))
                 ;  throw(gecode_argument_error(extensional(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(extensional(X0,X1,X2),arg=1))).

linear(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_linear_293(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_linear_297(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntRelType(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_linear_261(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  (is_IntVar(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_linear_265(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  (is_FloatValArgs(X1,Y1)
                     -> (is_FloatVarArgs(X2,Y2)
                         -> (is_FloatRelType(X3,Y3)
                             -> (is_FloatVar(X4,Y4)
                                 -> (is_Reify(X5,Y5)
                                     -> gecode_constraint_linear_269(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  (is_FloatNum(X4,Y4)
                                     -> (is_Reify(X5,Y5)
                                         -> gecode_constraint_linear_267(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3)))
                     ;  (is_IntArgs(X1,Y1)
                         -> (is_IntVarArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> (is_Reify(X5,Y5)
                                         -> gecode_constraint_linear_284(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_linear_283(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  (is_IntVar(X4,Y4)
                                         -> (is_Reify(X5,Y5)
                                             -> gecode_constraint_linear_288(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_IntConLevel(X5,Y5)
                                                 -> gecode_constraint_linear_287(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  (is_BoolVarArgs(X2,Y2)
                                 -> (is_IntRelType(X3,Y3)
                                     -> (is_int(X4,Y4)
                                         -> (is_Reify(X5,Y5)
                                             -> gecode_constraint_linear_276(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  (is_IntConLevel(X5,Y5)
                                                 -> gecode_constraint_linear_275(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                         ;  (is_IntVar(X4,Y4)
                                             -> (is_Reify(X5,Y5)
                                                 -> gecode_constraint_linear_280(Y0,Y1,Y2,Y3,Y4,Y5)
                                                 ;  (is_IntConLevel(X5,Y5)
                                                     -> gecode_constraint_linear_279(Y0,Y1,Y2,Y3,Y4,Y5)
                                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=6))))
                                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=5))))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=4)))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=3))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=2))))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4,X5),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> gecode_constraint_nooverlap_333(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  (is_IntArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_BoolVarArgs(X5,Y5)
                                 -> (is_IntConLevel(X6,Y6)
                                     -> gecode_constraint_nooverlap_328(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=3))))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6),arg=1))).

div(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_div_164(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(div(X0,X1,X2,X3,X4),arg=1))).

sqr(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_sqr_445(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sqr(X0,X1,X2,X3),arg=1))).

path(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_path_355(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  (is_IntVarArgs(X6,Y6)
                                     -> (is_IntVar(X7,Y7)
                                         -> gecode_constraint_path_352(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=7))))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVarArgs(X5,Y5)
                                 -> (is_IntVar(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_path_349(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

unary(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_TaskTypeArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_BoolVarArgs(X4,Y4)
                         -> gecode_constraint_unary_457(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_unary_460(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5))))
                     ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_BoolVarArgs(X4,Y4)
                             -> gecode_constraint_unary_453(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_unary_456(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                     ;  (is_IntArgs(X2,Y2)
                         -> (is_BoolVarArgs(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_unary_450(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=3))))
                 ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(unary(X0,X1,X2,X3,X4),arg=1))).

sorted(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> gecode_constraint_sorted_439(Y0,Y1,Y2,Y3)
                     ;  (is_IntConLevel(X3,Y3)
                         -> gecode_constraint_sorted_442(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sorted(X0,X1,X2,X3),arg=1))).

element(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> (is_IntVar(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_element_223(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> (is_BoolVar(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_element_217(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                 ;  (is_IntArgs(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_IntVar(X4,Y4)
                                 -> (is_int(X5,Y5)
                                     -> (is_IntVar(X6,Y6)
                                         -> (is_IntConLevel(X7,Y7)
                                             -> gecode_constraint_element_235(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  (is_BoolVar(X6,Y6)
                                             -> (is_IntConLevel(X7,Y7)
                                                 -> gecode_constraint_element_233(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=7))))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=3)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=2)))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

assign(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatAssign(X2,Y2)
                 -> (is_FloatVarValPrint(X3,Y3)
                     -> gecode_constraint_assign_16(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatAssign(X2,Y2)
                     -> (is_FloatBranchFilter(X3,Y3)
                         -> gecode_constraint_assign_7(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntAssign(X2,Y2)
                         -> (is_BoolBranchFilter(X3,Y3)
                             -> gecode_constraint_assign_4(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_IntAssign(X2,Y2)
                             -> (is_BoolVarValPrint(X3,Y3)
                                 -> gecode_constraint_assign_2(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_IntAssign(X2,Y2)
                                 -> (is_IntBranchFilter(X3,Y3)
                                     -> gecode_constraint_assign_10(Y0,Y1,Y2,Y3)
                                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_IntAssign(X2,Y2)
                                     -> (is_IntVarValPrint(X3,Y3)
                                         -> gecode_constraint_assign_18(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                                     ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                                 ;  (is_SetVarArgs(X1,Y1)
                                     -> (is_SetAssign(X2,Y2)
                                         -> (is_SetBranchFilter(X3,Y3)
                                             -> gecode_constraint_assign_13(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_SetAssign(X2,Y2)
                                             -> (is_SetVarValPrint(X3,Y3)
                                                 -> gecode_constraint_assign_20(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=4)))
                                             ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=3)))
                                         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=2))))))))))
         ;  throw(gecode_argument_error(assign(X0,X1,X2,X3),arg=1))).

element(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_element_221(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_element_225(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_element_215(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_BoolVar(X3,Y3)
                             -> (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_element_213(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_SetOpType(X1,Y1)
                     -> (is_SetVarArgs(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> (is_SetVar(X4,Y4)
                                 -> gecode_constraint_element_244(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                         ;  (is_IntVarArgs(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> (is_SetVar(X4,Y4)
                                     -> gecode_constraint_element_242(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                             ;  (is_IntSetArgs(X2,Y2)
                                 -> (is_SetVar(X3,Y3)
                                     -> (is_SetVar(X4,Y4)
                                         -> gecode_constraint_element_240(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                                 ;  (is_IntArgs(X2,Y2)
                                     -> (is_SetVar(X3,Y3)
                                         -> (is_SetVar(X4,Y4)
                                             -> gecode_constraint_element_238(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3))))))
                     ;  (is_IntArgs(X1,Y1)
                         -> (is_IntVar(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_element_231(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                 ;  (is_IntVar(X3,Y3)
                                     -> (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_element_237(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                     ;  (is_BoolVar(X3,Y3)
                                         -> (is_IntConLevel(X4,Y4)
                                             -> gecode_constraint_element_229(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=5)))
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=4)))))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=3)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=2))))))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4),arg=1))).

sequence(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_SetVar(X2,Y2)
                 -> gecode_constraint_sequence_438(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2),arg=1))).

branch(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatVarBranch(X2,Y2)
                 -> (is_FloatValBranch(X3,Y3)
                     -> (is_FloatBranchFilter(X4,Y4)
                         -> gecode_constraint_branch_30(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVarBranch(X2,Y2)
                     -> (is_IntValBranch(X3,Y3)
                         -> (is_Symmetries(X4,Y4)
                             -> gecode_constraint_branch_26(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_BoolBranchFilter(X4,Y4)
                                 -> gecode_constraint_branch_24(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntValBranch(X3,Y3)
                             -> (is_Symmetries(X4,Y4)
                                 -> gecode_constraint_branch_32(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_IntBranchFilter(X4,Y4)
                                     -> gecode_constraint_branch_36(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5))))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetValBranch(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> gecode_constraint_branch_42(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_Symmetries(X4,Y4)
                                         -> gecode_constraint_branch_38(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=3)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=2))))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4),arg=1))).

circuit(X0,X1,X2,X3,X4,X5,X6) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVar(X5,Y5)
                             -> (is_IntConLevel(X6,Y6)
                                 -> gecode_constraint_circuit_79(Y0,Y1,Y2,Y3,Y4,Y5,Y6)
                                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=7)))
                             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=6)))
                         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=5)))
                     ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=4)))
                 ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=3)))
             ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=2)))
         ;  throw(gecode_argument_error(circuit(X0,X1,X2,X3,X4,X5,X6),arg=1))).

pow(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> gecode_constraint_pow_361(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=3)))
             ;  (is_FloatVar(X1,Y1)
                 -> (is_int(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> gecode_constraint_pow_360(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=4)))
                     ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=3)))
                 ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=2))))
         ;  throw(gecode_argument_error(pow(X0,X1,X2,X3),arg=1))).

precede(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> gecode_constraint_precede_367(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntArgs(X2,Y2)
                     -> gecode_constraint_precede_363(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(precede(X0,X1,X2),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> gecode_constraint_cumulative_128(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> gecode_constraint_cumulative_124(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  (is_IntArgs(X3,Y3)
                             -> (is_IntArgs(X4,Y4)
                                 -> (is_BoolVarArgs(X5,Y5)
                                     -> gecode_constraint_cumulative_118(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_cumulative_121(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6))))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4))))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> gecode_constraint_cumulative_140(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> gecode_constraint_cumulative_136(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  (is_IntArgs(X3,Y3)
                                 -> (is_IntArgs(X4,Y4)
                                     -> (is_BoolVarArgs(X5,Y5)
                                         -> gecode_constraint_cumulative_130(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_cumulative_133(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=6))))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=4))))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5),arg=1))).

distinct(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_distinct_159(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(distinct(X0,X1,X2,X3),arg=1))).

min(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatVar(X2,Y2)
                 -> gecode_constraint_min_314(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(min(X0,X1,X2),arg=3)))
             ;  (is_IntVarArgs(X1,Y1)
                 -> (is_IntVar(X2,Y2)
                     -> gecode_constraint_min_315(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(min(X0,X1,X2),arg=3)))
                 ;  (is_SetVar(X1,Y1)
                     -> (is_IntVar(X2,Y2)
                         -> gecode_constraint_min_320(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(min(X0,X1,X2),arg=3)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2),arg=2)))))
         ;  throw(gecode_argument_error(min(X0,X1,X2),arg=1))).

sqrt(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntConLevel(X3,Y3)
                     -> gecode_constraint_sqrt_448(Y0,Y1,Y2,Y3)
                     ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=4)))
                 ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=3)))
             ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=2)))
         ;  throw(gecode_argument_error(sqrt(X0,X1,X2,X3),arg=1))).

sequence(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntSet(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_int(X5,Y5)
                             -> gecode_constraint_sequence_435(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntSet(X2,Y2)
                     -> (is_int(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_int(X5,Y5)
                                 -> gecode_constraint_sequence_433(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(sequence(X0,X1,X2,X3,X4,X5),arg=1))).

unshare(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntConLevel(X2,Y2)
                 -> gecode_constraint_unshare_464(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntConLevel(X2,Y2)
                     -> gecode_constraint_unshare_462(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(unshare(X0,X1,X2),arg=1))).

path(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_path_359(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_IntArgs(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntVar(X3,Y3)
                         -> (is_IntVar(X4,Y4)
                             -> (is_IntVar(X5,Y5)
                                 -> gecode_constraint_path_350(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=2))))
         ;  throw(gecode_argument_error(path(X0,X1,X2,X3,X4,X5),arg=1))).

divmod(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> gecode_constraint_divmod_165(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(divmod(X0,X1,X2,X3,X4),arg=1))).

branch(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVarArgs(X1,Y1)
             -> (is_FloatVarBranch(X2,Y2)
                 -> (is_FloatValBranch(X3,Y3)
                     -> (is_FloatBranchFilter(X4,Y4)
                         -> (is_FloatVarValPrint(X5,Y5)
                             -> gecode_constraint_branch_31(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_IntVarBranch(X2,Y2)
                     -> (is_IntValBranch(X3,Y3)
                         -> (is_Symmetries(X4,Y4)
                             -> (is_BoolBranchFilter(X5,Y5)
                                 -> gecode_constraint_branch_27(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  (is_BoolBranchFilter(X4,Y4)
                                 -> (is_BoolVarValPrint(X5,Y5)
                                     -> gecode_constraint_branch_25(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3)))
                 ;  (is_IntVarArgs(X1,Y1)
                     -> (is_IntVarBranch(X2,Y2)
                         -> (is_IntValBranch(X3,Y3)
                             -> (is_Symmetries(X4,Y4)
                                 -> (is_IntBranchFilter(X5,Y5)
                                     -> gecode_constraint_branch_33(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  (is_IntBranchFilter(X4,Y4)
                                     -> (is_IntVarValPrint(X5,Y5)
                                         -> gecode_constraint_branch_37(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3)))
                     ;  (is_SetVarArgs(X1,Y1)
                         -> (is_SetVarBranch(X2,Y2)
                             -> (is_SetValBranch(X3,Y3)
                                 -> (is_SetBranchFilter(X4,Y4)
                                     -> (is_SetVarValPrint(X5,Y5)
                                         -> gecode_constraint_branch_43(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  (is_Symmetries(X4,Y4)
                                         -> (is_SetBranchFilter(X5,Y5)
                                             -> gecode_constraint_branch_39(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=6)))
                                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=3)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=2))))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2,X3,X4,X5),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntVarArgs(X4,Y4)
                         -> (is_IntVarArgs(X5,Y5)
                             -> (is_IntVarArgs(X6,Y6)
                                 -> (is_BoolVarArgs(X7,Y7)
                                     -> (is_IntConLevel(X8,Y8)
                                         -> gecode_constraint_nooverlap_332(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
                                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=9)))
                                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=8)))
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=7)))
                             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=6)))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5,X6,X7,X8),arg=1))).

cumulative(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> (is_IntArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> gecode_constraint_cumulative_120(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_IntVar(X1,Y1)
                 -> (is_IntVarArgs(X2,Y2)
                     -> (is_IntArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> gecode_constraint_cumulative_132(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=4)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=3)))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4),arg=1))).

member(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_member_310(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(member(X0,X1,X2),arg=3)))
             ;  (is_BoolVarArgs(X1,Y1)
                 -> (is_BoolVar(X2,Y2)
                     -> gecode_constraint_member_306(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(member(X0,X1,X2),arg=3)))
                 ;  throw(gecode_argument_error(member(X0,X1,X2),arg=2))))
         ;  throw(gecode_argument_error(member(X0,X1,X2),arg=1))).

count(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntRelType(X3,Y3)
                     -> (is_int(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_count_111(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  (is_IntVar(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_count_113(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVar(X2,Y2)
                     -> (is_IntRelType(X3,Y3)
                         -> (is_int(X4,Y4)
                             -> (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_count_115(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  (is_IntVar(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_count_117(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  (is_IntSet(X2,Y2)
                         -> (is_IntRelType(X3,Y3)
                             -> (is_int(X4,Y4)
                                 -> (is_IntConLevel(X5,Y5)
                                     -> gecode_constraint_count_103(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  (is_IntVar(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_count_105(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  (is_IntArgs(X2,Y2)
                             -> (is_IntRelType(X3,Y3)
                                 -> (is_int(X4,Y4)
                                     -> (is_IntConLevel(X5,Y5)
                                         -> gecode_constraint_count_93(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  (is_IntVar(X4,Y4)
                                         -> (is_IntConLevel(X5,Y5)
                                             -> gecode_constraint_count_95(Y0,Y1,Y2,Y3,Y4,Y5)
                                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=6)))
                                         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=5))))
                                 ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=3))))))
             ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2,X3,X4,X5),arg=1))).

pow(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_int(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_pow_362(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(pow(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(pow(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(pow(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(pow(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(pow(X0,X1,X2,X3,X4),arg=1))).

notMin(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> gecode_constraint_notMin_336(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=3)))
             ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(notMin(X0,X1,X2),arg=1))).

cumulative(X0,X1,X2,X3,X4,X5,X6,X7) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_int(X1,Y1)
             -> (is_TaskTypeArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_IntArgs(X5,Y5)
                             -> (is_BoolVarArgs(X6,Y6)
                                 -> (is_IntConLevel(X7,Y7)
                                     -> gecode_constraint_cumulative_127(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntVarArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_cumulative_123(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
             ;  (is_IntVar(X1,Y1)
                 -> (is_TaskTypeArgs(X2,Y2)
                     -> (is_IntVarArgs(X3,Y3)
                         -> (is_IntArgs(X4,Y4)
                             -> (is_IntArgs(X5,Y5)
                                 -> (is_BoolVarArgs(X6,Y6)
                                     -> (is_IntConLevel(X7,Y7)
                                         -> gecode_constraint_cumulative_139(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_IntVarArgs(X3,Y3)
                             -> (is_IntVarArgs(X4,Y4)
                                 -> (is_IntArgs(X5,Y5)
                                     -> (is_BoolVarArgs(X6,Y6)
                                         -> (is_IntConLevel(X7,Y7)
                                             -> gecode_constraint_cumulative_135(Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7)
                                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=8)))
                                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=7)))
                                     ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=6)))
                                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=5)))
                             ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=4)))
                         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=3))))
                 ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=2))))
         ;  throw(gecode_argument_error(cumulative(X0,X1,X2,X3,X4,X5,X6,X7),arg=1))).

branch(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntValBranch(X2,Y2)
                 -> gecode_constraint_branch_46(Y0,Y1,Y2)
                 ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
             ;  (is_BoolVar(X1,Y1)
                 -> (is_IntValBranch(X2,Y2)
                     -> gecode_constraint_branch_21(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
                 ;  (is_FloatVar(X1,Y1)
                     -> (is_FloatValBranch(X2,Y2)
                         -> gecode_constraint_branch_44(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
                     ;  (is_SetVar(X1,Y1)
                         -> (is_SetValBranch(X2,Y2)
                             -> gecode_constraint_branch_48(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=3)))
                         ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=2))))))
         ;  throw(gecode_argument_error(branch(X0,X1,X2),arg=1))).

dom(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatVal(X2,Y2)
                 -> gecode_constraint_dom_188(Y0,Y1,Y2)
                 ;  (is_FloatVar(X2,Y2)
                     -> gecode_constraint_dom_190(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3))))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatVarArgs(X2,Y2)
                     -> gecode_constraint_dom_171(Y0,Y1,Y2)
                     ;  (is_FloatVal(X2,Y2)
                         -> gecode_constraint_dom_173(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3))))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_BoolVarArgs(X2,Y2)
                         -> gecode_constraint_dom_169(Y0,Y1,Y2)
                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))
                     ;  (is_BoolVar(X1,Y1)
                         -> (is_BoolVar(X2,Y2)
                             -> gecode_constraint_dom_167(Y0,Y1,Y2)
                             ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))
                         ;  (is_IntVarArgs(X1,Y1)
                             -> (is_int(X2,Y2)
                                 -> gecode_constraint_dom_178(Y0,Y1,Y2)
                                 ;  (is_IntVarArgs(X2,Y2)
                                     -> gecode_constraint_dom_176(Y0,Y1,Y2)
                                     ;  (is_IntSet(X2,Y2)
                                         -> gecode_constraint_dom_174(Y0,Y1,Y2)
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))))
                             ;  (is_IntVar(X1,Y1)
                                 -> (is_int(X2,Y2)
                                     -> gecode_constraint_dom_195(Y0,Y1,Y2)
                                     ;  (is_IntVar(X2,Y2)
                                         -> gecode_constraint_dom_203(Y0,Y1,Y2)
                                         ;  (is_IntSet(X2,Y2)
                                             -> gecode_constraint_dom_191(Y0,Y1,Y2)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))))
                                 ;  (is_SetVarArgs(X1,Y1)
                                     -> (is_SetVarArgs(X2,Y2)
                                         -> gecode_constraint_dom_182(Y0,Y1,Y2)
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_SetVar(X2,Y2)
                                             -> gecode_constraint_dom_211(Y0,Y1,Y2)
                                             ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=3)))
                                         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=2))))))))))
         ;  throw(gecode_argument_error(dom(X0,X1,X2),arg=1))).

linear(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntRelType(X2,Y2)
                 -> (is_int(X3,Y3)
                     -> (is_Reify(X4,Y4)
                         -> gecode_constraint_linear_292(Y0,Y1,Y2,Y3,Y4)
                         ;  (is_IntConLevel(X4,Y4)
                             -> gecode_constraint_linear_291(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                     ;  (is_IntVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> gecode_constraint_linear_296(Y0,Y1,Y2,Y3,Y4)
                             ;  (is_IntConLevel(X4,Y4)
                                 -> gecode_constraint_linear_295(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
             ;  (is_FloatVarArgs(X1,Y1)
                 -> (is_FloatRelType(X2,Y2)
                     -> (is_FloatVar(X3,Y3)
                         -> (is_Reify(X4,Y4)
                             -> gecode_constraint_linear_273(Y0,Y1,Y2,Y3,Y4)
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))
                         ;  (is_FloatNum(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> gecode_constraint_linear_271(Y0,Y1,Y2,Y3,Y4)
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
                 ;  (is_BoolVarArgs(X1,Y1)
                     -> (is_IntRelType(X2,Y2)
                         -> (is_int(X3,Y3)
                             -> (is_Reify(X4,Y4)
                                 -> gecode_constraint_linear_260(Y0,Y1,Y2,Y3,Y4)
                                 ;  (is_IntConLevel(X4,Y4)
                                     -> gecode_constraint_linear_259(Y0,Y1,Y2,Y3,Y4)
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                             ;  (is_IntVar(X3,Y3)
                                 -> (is_Reify(X4,Y4)
                                     -> gecode_constraint_linear_264(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_IntConLevel(X4,Y4)
                                         -> gecode_constraint_linear_263(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4))))
                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
                     ;  (is_FloatValArgs(X1,Y1)
                         -> (is_FloatVarArgs(X2,Y2)
                             -> (is_FloatRelType(X3,Y3)
                                 -> (is_FloatVar(X4,Y4)
                                     -> gecode_constraint_linear_268(Y0,Y1,Y2,Y3,Y4)
                                     ;  (is_FloatNum(X4,Y4)
                                         -> gecode_constraint_linear_266(Y0,Y1,Y2,Y3,Y4)
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3)))
                         ;  (is_IntArgs(X1,Y1)
                             -> (is_IntVarArgs(X2,Y2)
                                 -> (is_IntRelType(X3,Y3)
                                     -> (is_int(X4,Y4)
                                         -> gecode_constraint_linear_282(Y0,Y1,Y2,Y3,Y4)
                                         ;  (is_IntVar(X4,Y4)
                                             -> gecode_constraint_linear_286(Y0,Y1,Y2,Y3,Y4)
                                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))
                                 ;  (is_BoolVarArgs(X2,Y2)
                                     -> (is_IntRelType(X3,Y3)
                                         -> (is_int(X4,Y4)
                                             -> gecode_constraint_linear_274(Y0,Y1,Y2,Y3,Y4)
                                             ;  (is_IntVar(X4,Y4)
                                                 -> gecode_constraint_linear_278(Y0,Y1,Y2,Y3,Y4)
                                                 ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=5))))
                                         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=4)))
                                     ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=3))))
                             ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=2)))))))
         ;  throw(gecode_argument_error(linear(X0,X1,X2,X3,X4),arg=1))).

nooverlap(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntArgs(X2,Y2)
                 -> (is_IntVarArgs(X3,Y3)
                     -> (is_IntArgs(X4,Y4)
                         -> (is_BoolVarArgs(X5,Y5)
                             -> gecode_constraint_nooverlap_327(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  (is_IntConLevel(X5,Y5)
                                 -> gecode_constraint_nooverlap_330(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=6))))
                         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(nooverlap(X0,X1,X2,X3,X4,X5),arg=1))).

element(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_SetOpType(X1,Y1)
             -> (is_SetVarArgs(X2,Y2)
                 -> (is_SetVar(X3,Y3)
                     -> (is_SetVar(X4,Y4)
                         -> (is_IntSet(X5,Y5)
                             -> gecode_constraint_element_245(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  (is_IntVarArgs(X2,Y2)
                     -> (is_SetVar(X3,Y3)
                         -> (is_SetVar(X4,Y4)
                             -> (is_IntSet(X5,Y5)
                                 -> gecode_constraint_element_243(Y0,Y1,Y2,Y3,Y4,Y5)
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                     ;  (is_IntSetArgs(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> (is_SetVar(X4,Y4)
                                 -> (is_IntSet(X5,Y5)
                                     -> gecode_constraint_element_241(Y0,Y1,Y2,Y3,Y4,Y5)
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                         ;  (is_IntArgs(X2,Y2)
                             -> (is_SetVar(X3,Y3)
                                 -> (is_SetVar(X4,Y4)
                                     -> (is_IntSet(X5,Y5)
                                         -> gecode_constraint_element_239(Y0,Y1,Y2,Y3,Y4,Y5)
                                         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=6)))
                                     ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=5)))
                                 ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=4)))
                             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=3))))))
             ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(element(X0,X1,X2,X3,X4,X5),arg=1))).

rel(X0,X1,X2,X3) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_FloatVar(X1,Y1)
             -> (is_FloatRelType(X2,Y2)
                 -> (is_FloatVal(X3,Y3)
                     -> gecode_constraint_rel_405(Y0,Y1,Y2,Y3)
                     ;  (is_FloatVar(X3,Y3)
                         -> gecode_constraint_rel_407(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
             ;  (is_SetOpType(X1,Y1)
                 -> (is_SetVarArgs(X2,Y2)
                     -> (is_SetVar(X3,Y3)
                         -> gecode_constraint_rel_423(Y0,Y1,Y2,Y3)
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                     ;  (is_IntVarArgs(X2,Y2)
                         -> (is_SetVar(X3,Y3)
                             -> gecode_constraint_rel_421(Y0,Y1,Y2,Y3)
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                 ;  (is_FloatVarArgs(X1,Y1)
                     -> (is_FloatRelType(X2,Y2)
                         -> (is_FloatVal(X3,Y3)
                             -> gecode_constraint_rel_393(Y0,Y1,Y2,Y3)
                             ;  (is_FloatVar(X3,Y3)
                                 -> gecode_constraint_rel_394(Y0,Y1,Y2,Y3)
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                     ;  (is_BoolVarArgs(X1,Y1)
                         -> (is_IntRelType(X2,Y2)
                             -> (is_int(X3,Y3)
                                 -> gecode_constraint_rel_391(Y0,Y1,Y2,Y3)
                                 ;  (is_BoolVarArgs(X3,Y3)
                                     -> gecode_constraint_rel_387(Y0,Y1,Y2,Y3)
                                     ;  (is_BoolVar(X3,Y3)
                                         -> gecode_constraint_rel_385(Y0,Y1,Y2,Y3)
                                         ;  (is_IntConLevel(X3,Y3)
                                             -> gecode_constraint_rel_390(Y0,Y1,Y2,Y3)
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))
                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                         ;  (is_BoolVar(X1,Y1)
                             -> (is_IntRelType(X2,Y2)
                                 -> (is_int(X3,Y3)
                                     -> gecode_constraint_rel_381(Y0,Y1,Y2,Y3)
                                     ;  (is_BoolVar(X3,Y3)
                                         -> gecode_constraint_rel_377(Y0,Y1,Y2,Y3)
                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                             ;  (is_IntVarArgs(X1,Y1)
                                 -> (is_IntRelType(X2,Y2)
                                     -> (is_int(X3,Y3)
                                         -> gecode_constraint_rel_401(Y0,Y1,Y2,Y3)
                                         ;  (is_IntVar(X3,Y3)
                                             -> gecode_constraint_rel_403(Y0,Y1,Y2,Y3)
                                             ;  (is_IntVarArgs(X3,Y3)
                                                 -> gecode_constraint_rel_397(Y0,Y1,Y2,Y3)
                                                 ;  (is_IntConLevel(X3,Y3)
                                                     -> gecode_constraint_rel_400(Y0,Y1,Y2,Y3)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))))
                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                                 ;  (is_IntVar(X1,Y1)
                                     -> (is_IntRelType(X2,Y2)
                                         -> (is_int(X3,Y3)
                                             -> gecode_constraint_rel_409(Y0,Y1,Y2,Y3)
                                             ;  (is_IntVar(X3,Y3)
                                                 -> gecode_constraint_rel_413(Y0,Y1,Y2,Y3)
                                                 ;  (is_SetVar(X3,Y3)
                                                     -> gecode_constraint_rel_417(Y0,Y1,Y2,Y3)
                                                     ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))))
                                         ;  (is_SetRelType(X2,Y2)
                                             -> (is_SetVar(X3,Y3)
                                                 -> gecode_constraint_rel_418(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                     ;  (is_SetVar(X1,Y1)
                                         -> (is_IntRelType(X2,Y2)
                                             -> (is_IntVar(X3,Y3)
                                                 -> gecode_constraint_rel_424(Y0,Y1,Y2,Y3)
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4)))
                                             ;  (is_SetRelType(X2,Y2)
                                                 -> (is_IntVar(X3,Y3)
                                                     -> gecode_constraint_rel_429(Y0,Y1,Y2,Y3)
                                                     ;  (is_SetVar(X3,Y3)
                                                         -> gecode_constraint_rel_431(Y0,Y1,Y2,Y3)
                                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3))))
                                         ;  (is_BoolOpType(X1,Y1)
                                             -> (is_BoolVarArgs(X2,Y2)
                                                 -> (is_int(X3,Y3)
                                                     -> gecode_constraint_rel_371(Y0,Y1,Y2,Y3)
                                                     ;  (is_BoolVar(X3,Y3)
                                                         -> gecode_constraint_rel_369(Y0,Y1,Y2,Y3)
                                                         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=4))))
                                                 ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=3)))
                                             ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=2)))))))))))
         ;  throw(gecode_argument_error(rel(X0,X1,X2,X3),arg=1))).

min(X0,X1,X2,X3,X4) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntConLevel(X4,Y4)
                         -> gecode_constraint_min_319(Y0,Y1,Y2,Y3,Y4)
                         ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=5)))
                     ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=4)))
                 ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=3)))
             ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=2)))
         ;  throw(gecode_argument_error(min(X0,X1,X2,X3,X4),arg=1))).

count(X0,X1,X2) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_IntVarArgs(X1,Y1)
             -> (is_IntVarArgs(X2,Y2)
                 -> gecode_constraint_count_108(Y0,Y1,Y2)
                 ;  (is_IntSetArgs(X2,Y2)
                     -> gecode_constraint_count_98(Y0,Y1,Y2)
                     ;  throw(gecode_argument_error(count(X0,X1,X2),arg=3))))
             ;  throw(gecode_argument_error(count(X0,X1,X2),arg=2)))
         ;  throw(gecode_argument_error(count(X0,X1,X2),arg=1))).

ite(X0,X1,X2,X3,X4,X5) :-
        (is_Space_or_Clause(X0,Y0)
         -> (is_BoolVar(X1,Y1)
             -> (is_IntVar(X2,Y2)
                 -> (is_IntVar(X3,Y3)
                     -> (is_IntVar(X4,Y4)
                         -> (is_IntConLevel(X5,Y5)
                             -> gecode_constraint_ite_257(Y0,Y1,Y2,Y3,Y4,Y5)
                             ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=6)))
                         ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=5)))
                     ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=4)))
                 ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=3)))
             ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=2)))
         ;  throw(gecode_argument_error(ite(X0,X1,X2,X3,X4,X5),arg=1))).

