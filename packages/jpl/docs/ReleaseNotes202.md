# Release notes - 2.0.2

## Java API: canonical representation of terms

### rationale
"List" and "Tuple" terms are not recognised as distinct types by the Prolog engine: they are just conventions: it doesn't follow that every ./2 or []/0 should be represented externally as instances of List or Nil, nor that {}/2 should be represented as Tuple.  There are many other informal types, and it's not clear which of them deserve the same treatment.  The simplest policy is to provide special support for none of them, and this is what JPL 2.x.x does.  This also ensures that there is only one valid representation of a Prolog term as JPL class instances (otherwise we would have to be careful to recognise every Atom whose name is "[]" as being equivalent to an instance of Nil).

 these classes have been dropped (sorry, not deprecated: they don't fit into the new scheme)
 * Tuple (see above)
 * List (see above)
 * Nil (see above)
 * String (these are obsolete and more-or-less deprecated in recent SWI-Prolog releases)
 * Long (this doesn't have a clear role)
 
 the Term class hierarchy has been rearranged thus:
'''
Term (abstract)
  |
  +--- Compound
  |      |
  |      +--- Atom (special case)
  |
  +--- Integer
  |
  +--- Float
  |
  +--- Variable

Query
'''
Note that an Atom is a Compound whose arity is zero.  It is naughty to construct a Compound with zero arity (this violates canonicity), and JPL code never does this when exporting terms from Prolog.  Application code written in Java, using JPL, should avoid doing this.  Maybe we should raise an exception if it is attempted (maybe we do, I can't remember :-)

Note also that, although a Query contains a Term (among other Prolog-related state), it is not related to it by inheritance.

## Java API: lazy initialisation

It is no longer necessary to explicitly initialise JPL before calling any of the methods which access the Prolog engine.  This allows you to develop Java classes which make use of JPL, and to make them available as "library" classes for use freely in applications, without placing a burden upon the application programmer to explicitly initialise JPL.

Instead, JPL (and, if necessary, the Prolog engine) is initialised "lazily", at the first attempt to invoke the Prolog engine.  At this point, a "default" sequence of initialisation parameters is used: initial values for these are compiled into JPL, but they can be redefined at any time up until initialisation occurs.

It is also possible for Java applications to discover (as a String[]) exactly what sequence of JPL initialisation parameters were actually used (this call returns null if Prolog is not yet initialised, and can thus be used as a test of this state).

If a Java application needs to use Prolog with, say, a larger than normal heap or stack, it should attempt to redefine the default initialisation parameters, and hope that the Prolog engine is not yet initialised (if it is, there's not much it can do about it) (in newer versions of SWI-Prolog it could restart it, but this is rather drastic, might disrupt other activities, and is not yet supported via JPL).

Finally, the JPL 1.0.1 static jpl.JPL.init() method is still supported, for backwards compatibility.

These changes are not only for convenience, and to allow development of easy-to-use library code, but are part of a plan to combine Fred Dushin's  Java-calls-Prolog interface with Paul Singleton's Prolog-calls-Java interface, to support hybrid Prolog+Java application programming in which either

* the JVM is alive before Prolog is started
* the Prolog engine is alive before a JVM is started
* a C or C++ main() starts both of them.

## Java API: miscellaneous changes

### new constructors:
```java
new jpl.Query( Term t)
```
### withdrawn constructors:
         
all the multi-argument convenience constructors for Compound etc., since Java 1.1 onwards supports "anonymous arrays" which can (fairly) conveniently be used to create Compounds of any arity, e.g.
```java
new Compound( "pair", new Term[] { new Atom("one"), new Atom("two") } )
```
### new accessor methods:
```java
String Compound.name()

int Compound.arity()
```
NB an Atom is a special case of a Compound, and inherits its  name() and an arity() accessors
     
### deprecated accessor methods:
```java
Compound.atom()
```
(although Prolog conventionally, and necessarily, returns the "name" of a term's principal functor as an atom, this "name" is really a string, and in Java we can represent it as such; the best Prolog can return is "the atom whose name is the same as the name of this compound", whereas we can simply return the name).
     
### deprecated method:

jpl.Query.query() is renamed jpl.Query.hasSolution()
for consistency with oneSolution() and allSolutions() 

## Java API: bug fixes

Only one "bug" has been fixed, and this was already flagged by Fred as an issue: it concerns the conversion, from Prolog into JPL, of terms which contain shared variables (i.e. several instances of the same variable).  Transput of any (non-cyclic) term from Prolog into Java and back, using JPL, should yield a new term which is identical to the original apart from having all new variables (but in a similar pattern of sharing).


Paul Singleton

reformatted Friday 19th October 2018

drafted Tuesday 20th February 2001

revised Thursday 19th April 2001
