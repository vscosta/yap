# Release notes - 3.0.0

This release is a work-in-progress, and is being made available only to a few enthusiasts who don't mind the likelihood that the API will change before 3.x becomes stable.

## Java API: new Variable semantics

* A Variable must be created with a name, e.g.
```prolog
new Variable("X")
```
 or as an anonymous variable
```prolog
new Variable("_")
```
 or as a dont-tell-me variable
```prolog
new Variable("_Q")
```
* Each binding within a solution is now indexed by the name of its associated Variable, hence
```prolog
solution.get("X")
```
* New variables returned in bindings are given new, sequential names, e.g. "_283".
 
* Each Variable instance within a Java application is just a lexical token in the alternative Prolog concrete syntax which Term and its subclasses comprise.  Two instances of Variable("X") are no different from one shared instance: you are free to reuse such lexical elements, but this has nothing to do with the sharing of variables which can occur within a Prolog engine.

* The bindings of anonymous and dont-tell-me variables (i.e. those whose names begin with an underscore character) are not returned to Java: use them to avoid the computational time and space costs of constructing Term representations of bindings in which you are not interested.

## Java API: easier Term and Query construction

Now that Variables are named, and bindings are keyed by the names of variables, it is easier to construct Term (and hence Query) instances.
     
This utility (NB liable to be renamed or moved into a different class) converts a valid Prolog source text representation of a term into a corresponding Term hierarchy:
```prolog
Term jpl.Util.textToTerm( String sourcetext)
```
A new (in JPL 3.0.0) Query constructor
```prolog
Query( String sourcetext)
```
allows queries to be created from source text, e.g.
```prolog
new Query("findall(_A,current_atom(_A),_As),length(_As,N)")
```
and oneSolution(), allSolutions() and nextSolution() will return bindings of N (but not of the dont-tell-me variables _A and _As), e.g.
```prolog
q.oneSolution().get("N")
```
returns a jpl.Integer representing the Prolog integer value to which N was bound by the successful call of the query.

## Java API: deprecated methods
```prolog
Query.query()
```
use Query.hasSolution() instead
```prolog
Query.rewind()
```
use Query.close() instead

## Java API: fixes

array methods inherited from java.lang.Object are now callable, e.g.
```prolog
jpl_new(array(int), [4,5,6], A),
jpl_call(A, hashCode, [], H).
```
## Java API: planned or under consideration

drop Term.display(), which cutely displays any Term in a Swing tree view in a new window.

support non-virtual method calls, e.g. by
```prolog
jpl_call(+Obj, +Class:Method, +Args, -Result)
```
finish the current tidy-up

passing (or returning) Prolog terms to Java by reference; we might stash them in Prolog's recorded database (see PL_record in the SWI-Prolog Reference Manual), and return an instance of some yet-to-be-designed JPL class which erases the recorded term when the referring object is garbage-collected

convenience constructs in Prolog akin to import in Java, allowing us to write e.g.
```prolog
jpl_new('Timestamp', X, R)
```
when we mean
```prolog
jpl_new('javax.sql.Timestamp', X, R)
```
renaming the package jpl more globally: unfortunately, org.jpl has already been taken :-)

ditching jpl.Util and moving its (static, utility) methods into jpl.JPL

deprecate all .args(), .arg0(), .arg1() methods and replace with
```prolog
public final Term[] args;
```
require any Variable's name to conform to Prolog source syntax, so that valid source texts can be reconstructed from Term instances by the toString() methods

Paul Singleton

Wednesday 4th February 2004
