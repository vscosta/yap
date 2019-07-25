# Release Notes - 3.0.2

## Changes within the distribution

### New classes folder

The root directory of the distribution now contains a classes folder, holding copies of the jpl.* and jpl.fli.* class files

### New demo

In demo/FamilyMT is a new variant of the Family demo which exercises multiple Prolog engines (from a  shared pool)
called by multiple Java threads.

## C library changes

Lots of refactoring and tidying in preparation for porting (to Linux+gcc initially).

Added Prolog "foreign" functions jpl_c_lib_version/1 and jpl_c_lib_version/4 for making library version identification
available to Prolog.

Added Java "native" method get_string_chars(), needed if Prolog returns a string to Java
(which it does sometime even if you don't want it to).

Commented out various unused functions.

Added Java "native" method action_abort() but it doesn't work yet...

Added support for new jpl.JRef type.

## Java API changes

Altered the semantics of Variable to be a purely lexical entity; a Variable instance should be created with a name
which is valid in Prolog source syntax;
the argumentless Variable() constructor is currently deprecated and constructs a variable with a "new" name
in the sequence "_1", "_2" etc. so as not to completely break older programs

Bindings from successful calls are now keyed by the names of the variables, rather than by Variable objects themselves;
this is part of a revamp to allow goals to be defined by source text fragments.

Implemented the following methods for all Term subclasses (to simplify coding of term checking and traversal etc.):

* type()

  returns jpl.fli.Prolog.ATOM, COMPOUND, FLOAT, INT or VARIABLE
 
* hasFunctor(String name, int arity)
 
  fails unless called appropriately;
  
* intValue(), longValue(), floatValue(), doubleValue()
  
  yield the Java int value (or long, float or double value respectively) of an Integer or Float instance;
  each will throw an exception for Atom, Compound and Variable instances;
  the names of these methods follow the precedent set by java.lang.Integer etc. and remember that a Prolog integer
  is not equivalent to a Java int (it may be longer or shorter), nor is a Prolog float equivalent to a Java float
  (it may have a different precision)
 
* arg(int argNo)
 
  calling arg() inappropriately (i.e. for jpl.Atom, jpl.Integer, jpl.Float and jpl.Variable instances)
  throws a runtime exception but is not a compile-time error;
  this method considers the args to be numbered from 1 upwards (i.e. Prolog convention, not Java array convention)
 
* name()

  Variable.name() returns the variable's lexical name, Atom.name() and Compound.name() behave as before,
  Integer.name() and Float.name() each throw an exception if called (but are valid at compile time)

Altered these methods for all Term subclasses:

* toString() now yields a valid (quoted if necessary) Prolog source text representation
* deprecated Compound.arg0() and all *.debugString() methods
* deprecated Float.value() (see floatValue() and doubleValue())
* jpl.Integer now holds a long value (to allow for other/future Prolog implementations with >32-bit integers) but this is backwards compatible if I understand Java correctly...
* deprecated jpl.Halt() pending a rethink about how best to clean up and terminate a hybrid Java+Prolog application
* added Query.abort() but it doesn't work yet...

Paul Singleton

Sunday 22nd February 2004
