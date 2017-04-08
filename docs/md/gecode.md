USING THE GECODE MODULE             {#gecode}
=======================

There are two ways to use the gecode interface from YAP. The original approach,
designed by Denys Duchier, requires loading the library:

:- use_module(library(gecode)).

A second approach is closer to CLP(FD), and is described in:

 - \ref Gecode_and_ClPbBFDbC

In what follows, we refer the reader to the~\cite{gecode} manual for the necessary background.

CREATING A SPACE
----------------

    Space := space

CREATING VARIABLES
-----------------

Unlike in Gecode, variable objects are not bound to a specific Space.  Each one
actually contains an index with which it is possible to access a Space-bound
Gecode variable.  Variables can be created using the following expressions:

   IVar := intvar(Space,SPEC...)
   BVar := boolvar(Space)
   SVar := setvar(Space,SPEC...)

where SPEC... is the same as in Gecode.  For creating lists of variables use
the following variants:

   IVars := intvars(Space,N,SPEC...)
   BVars := boolvars(Space,N,SPEC...)
   SVars := setvars(Space,N,SPEC...)

where N is the number of variables to create (just like for XXXVarArray in
Gecode).  Sometimes an IntSet is necessary:

   ISet := intset([SPEC...])

where each SPEC is either an integer or a pair (I,J) of integers.  An IntSet
describes a set of ints by providing either intervals, or integers (which stand
for an interval of themselves).  It might be tempting to simply represent an
IntSet as a list of specs, but this would be ambiguous with IntArgs which,
here, are represented as lists of ints.

   Space += keep(Var)
   Space += keep(Vars)

Variables can be marked as "kept".  In this case, only such variables will be
explicitly copied during search.  This could bring substantial benefits in
memory usage.  Of course, in a solution, you can then only look at variables
that have been "kept".  If no variable is marked as "kept", then they are all
kept.  Thus marking variables as "kept" is purely an optimization.


CONSTRAINTS AND BRANCHINGS
---------------------------

all constraint and branching posting functions are available just like in
Gecode.  Wherever a XXXArgs or YYYSharedArray is expected, simply use a list.
At present, there is no support for minimodel-like constraint posting.
Constraints and branchings are added to a space using:

    Space += CONSTRAINT
    Space += BRANCHING

For example:

    Space += rel(X,'IRT_EQ',Y)

arrays of variables are represented by lists of variables, and constants are
represented by atoms with the same name as the Gecode constant
(e.g. 'INT_VAR_SIZE_MIN').

SEARCHING FOR SOLUTIONS
--------------------

    SolSpace := search(Space)

This is a backtrackable predicate that enumerates all solution spaces
(SolSpace).  It may also take options:

    SolSpace := search(Space,Options)

Options is a list whose elements maybe:

restart
    to select the Restart search engine
threads=N
    to activate the parallel search engine and control the number of
    workers (see Gecode doc)
c_d=N
    to set the commit distance for recomputation
a_d=N
    to set the adaptive distance for recomputation

EXTRACTING INFO FROM A SOLUTION
------------------------------

An advantage of non Space-bound variables, is that you can use them both to
post constraints in the original space AND to consult their values in
solutions.  Below are methods for looking up information about variables.  Each
of these methods can either take a variable as argument, or a list of
variables, and returns resp. either a value, or a list of values:

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

DISJUNCTORS
-----------

Disjunctors provide support for disjunctions of clauses, where each clause is a
conjunction of constraints:

    C1 or C2 or ... or Cn

Each clause is executed "speculatively": this means it does not affect the main
space.  When a clause becomes failed, it is discarded.  When only one clause
remains, it is committed: this means that it now affects the main space.

Example:

Consider the problem where either X=Y=0 or X=Y+(1 or 2) for variable X and Y
that take values in 0..3.

    Space := space,
    [X,Y] := intvars(Space,2,0,3),

First, we must create a disjunctor as a manager for our 2 clauses:

    Disj := disjunctor(Space),

We can now create our first clause:

    C1 := clause(Disj),

This clause wants to constrain X and Y to 0.  However, since it must be
executed "speculatively", it must operate on new variables X1 and Y1 that
shadow X and Y:

    [X1,Y1] := intvars(C1,2,0,3),
    C1 += forward([X,Y],[X1,Y1]),

The forward(...) stipulation indicates which global variable is shadowed by
which clause-local variable.  Now we can post the speculative clause-local
constraints for X=Y=0:

    C1 += rel(X1,'IRT_EQ',0),
    C1 += rel(Y1,'IRT_EQ',0),

We now create the second clause which uses X2 and Y2 to shadow X and Y:

    C2 := clause(Disj),
    [X2,Y2] := intvars(C2,2,0,3),
    C2 += forward([X,Y],[X2,Y2]),

However, this clause also needs a clause-local variable Z2 taking values 1 or
2 in order to post the clause-local constraint X2=Y2+Z2:

    Z2 := intvar(C2,1,2),
    C2 += linear([-1,1,1],[X2,Y2,Z2],'IRT_EQ',0),

Finally, we can branch and search:

    Space += branch([X,Y],'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
    SolSpace := search(Space),

and lookup values of variables in each solution:

    [X_,Y_] := val(SolSpace,[X,Y]).
