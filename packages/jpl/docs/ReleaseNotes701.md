# Release notes - 7.0.1

SWI Prolog V7 maintains good source level compatibility with previous major versions,
but there are significant changes to the term model and foreign language interface (FLI),
and it is with these that JPL interfaces.

I have (eventually) prototyped a new version of JPL to serve SWI Prolog V7
in both its default and --traditional modes.

With Jan's encouragement, I have taken the opportunity to modernise JPL's APIs,
without unnecessarily complicating the migration of user applications from V6 (and earlier).

The Prolog API (jpl_call/4 etc.), which presents JVM classes and objects to Prolog,
has barely changed, as there have been no changes to the Java Native Interface (JNI)
and programming model which affect JPL's functionality. The Java language has evolved,
but most new features are compiled out, and JPL is an interface to the JVM, not to Java.

The Java API (Query.oneSolution etc.) has changed in some important ways,
and these are summarised below.

## Modern vs. traditional syntax

**summary:** *JPL7 supports --traditional mode too*

When started from Prolog, e.g. as a side-effect of
```prolog
jpl_call('java.lang.System', getProperty, ['os.arch'], X).
```
JPL7 respects the prevailing "syntax" option (modern or traditional) set when SWI Prolog was started; if you run
```
swipl --traditional ...
```
then JPL7 will automatically use traditional list syntax, otherwise it will use the (default) modern syntax.

If your application starts JPL from Java, e.g. as a side-effect of
```prolog
Query.oneSolution("current_prolog_flag(arch,A)").get("A").name()
```
then JPL7 initialises using modern syntax; you can override this by calling
```prolog
JPL.setTraditional()
```
before any JPL operation which starts or requires Prolog.

## A new Java package name

JPL7's Java API is in a new package, hence
```java
import org.jpl7.*;
```
instead of JPL 3.x's
```java
import jpl.*;
```
I am confident that this renaming will prevent much more confusion than it causes, by clearly distinguishing the old and new API and applications which use them.

The new package name is more idiomatic, and the domain has been secured for documentation etc.

## Unbounded integers

**summary:** *new backwards-compatible support for arbitrarily large integers*

JPL7 supports SWI Prolog's unbounded integers, backwards-compatibly with JPL 3.x's support for bounded
(max 64-bit twos complement) integers.

An `org.jpl7.Integer` instance can be constructed from a `java.math.BigInteger` instance
as well as from long, int, short, char or byte values.

If you do not expect Prolog to return "big" integer values (i.e. too large, +ve or -ve,
to fit into a Java long), then you can continue to use Integer as in JPL 3.x.

`Integer.longValue()` will throw a `JPLException` if its value won't fit in a long,
and you can test for and retrieve big values with
```java
i.isBig()
i.bigValue()
```
## Solutions as maps

**summary:** *Hashtable is replaced by Map<String, Term>*

In JPL7, a solution promises only to support this interface:
```java
java.util.Map<java.lang.String, org.jpl7.Term>
```
whereas in JPL 3.x it was explicitly a java.util.Hashtable (from Object to Object).

JPL7 actually returns instances of the more modern HashMap, but this is not formally documented, it could (in principle) change, and you shouldn't need to know this.

To migrate your JPL 3.x Java code to JPL7, you will need to replace
```java
Hashtable
```
by
```java
Map<String, Term>
```
if you use generics, or by
```java
Map
```
if you don't. Use of generic types will eliminate many coercions and enhance static type checking.

## Iterating one-at-a-time solutions

**summary:** *hasNext replaces hasMoreSolutions etc.*

`org.jpl7.Query` supports one-at-a-time solution fetching by implementing the `Iterable` and `Iterator` interfaces;
a `Query` instance is its own `Iterator`, hence
```java
for (Map soln : new Query("current_module(X)")) {
    ... soln.get("X").name() ...
}
```
or
```java
Query q = new Query(current_module(X)"));
while (q.hasNext()) {
    ... q.next().get("X").name() ...
}
```
The following JPL 3.x `Query` methods have been removed:
```java
q.hasMoreElements()
q.nextElement()

q.hasMoreSolutions()
q.nextSolution()
```
Use the corresponding methods from the `Iterator` interface instead:
```java
q.hasNext()
q.next()
```
In this respect, migrating JPL 3.x applications to JPL7 should require only shallow textual editing (flagged by the compiler), not structural changes.

## Iterating aggregated solutions

**summary:** *no change here*

JPL's `Query.allSolutions()` and `.nSolutions()` are analogous to SWI Prolog's `findall/3` and `findnsolns/3`.

Note that what JPL calls a *solution* is strictly a *substitution*, i.e. a map from variables to bindings.
Nevertheless, JPL7 and its documentation will continue to use *solution* everywhere.

In JPL7, these `Query` methods (still) return an array of solutions;
in current Java best practice they should arguably use `java.util.List` from the Collections Framework,
but I can see no overall advantage in changing this.

Note that Java arrays can be iterated with the *for-each* construct:
```java
for (Map soln : Query.allSolutions("current_module(X)")) {
    ... soln.get("X").name() ...
}
```

## Atom is a direct subclass of Term

**summary:** *Term class hierarchy flattened to reflect reality better*

In JPL 3.x, `Atom` extended `Compound`; it was a specialised compound with no args.
```
Term
 +-- Compound
 |    +-- Atom
```
in JPL7, `Atom` and `Compound` are sibling subclasses of `Term`,
because SWI Prolog V7 compounds may have zero args yet be distinct from similarly named atoms,
and JPL7 `Atom` instances have extra structure and functionality;
there is no longer any inheritance worth representing.
```
Term
 +-- Compound
 +-- Atom
 |
```
Migrating your JPL 3.x Java code to JPL7 requires recoding situations (if any) which exploit this inheritance.
The Java compiler should flag these for you, and the impact is local (albeit not a simple global edit).

## New types of Atom

**summary:** *atoms are backwards-compatibly typed, embracing strings and (some) blobs*

Classic Prolog atoms are equal iff their string names are equal, end of story (almost).

SWI Prolog has long had strings and blobs as well as classic atoms, and V7 builds on this.

JPL7 atoms have two internal components:
```java
String name; // as in JPL 3.x
String type; // new in JPL7
```
For traditional atoms, the type is `"text"`.

For out-of-band special atoms, such as the V7 empty list, the type is `"reserved_symbol"`, as in JPL7 source code
```java
protected static final Atom LIST_NIL_MODERN = new Atom("[]", "reserved_symbol");
public static Atom LIST_NIL = LIST_NIL_MODERN; // unless we switch to --traditional
```
SWI Prolog V7 strings are represented in JPL7 as `Atom` instances whose type is `"string"`;
if you don't bother to check this, strings appear to be returned from Prolog as "text" atoms
which may be convenient (or misleading).

Note that type is currently a `java.lang.String`, not a symbolic constant or enum.
This is because SWI Prolog supports user-defined blob types with arbitrary user-defined names,
although JPL7 doesn't (yet) handle these.

## List support

**summary:** *update explicit uses of ./2 and []/0, preferably using LIST_PAIR and LIST_NIL*

In SWI Prolog V7, lists are not an ADT. Although the empty list is denoted by an out-of-band atom
```java
new Atom("[]", "reserved_symbol")
```
the list pair constructor is a regular functor
```java
new Compound("[|]", new Term[] {....})
```
which (to me) means that V7 lists are still just a term encoding convention, as in classic Prolog.

Note also that
```prolog
[a|_]
[a|b]
```
are valid structures which do not map comfortably to instances of (say) `java.util.List<Term>`.

For these reasons taken together, JPL7 passes lists as regular terms,
and provides utilities to convert to and from various Java classes.

If your Java code manipulates lists, use the new symbols and methods e.g.
```java
new Compound(JPL.LIST_PAIR, new Term[],{ myTerm, JPL.LIST_NIL })

if (soln.get("X").isListNil()) {
```

## Dict support

**summary:** *not yet implemented or finally designed; aiming to be future-proof*

The SWI Prolog V7 *dict* is an abstract data type (ADT), currently implemented using an out-of-band functor
and a conventional, may-change internal structure.

JPL7 proposes to support dicts with a new subclass of `Term` containing a name and wrapping
(or otherwise impersonating) a map, hence (in pseudo code)
```java
new Dict(Map<Term, Term> map) // construct an anonymous dict
new Dict(String name, Map<Term, Term> map) // construct a named dict
dict.name() // null if dict is anonymous
dict.get(Term key)
```
Note that: dict keys must currently be either (textual) atoms or (small) integers; the map is necessarily from `Term` to `Term`; JPL7 must check (at run-time) that all keys are legitimate.

This is *not yet implemented*, and constructive discussion is welcomed.

## Replaced in JPL7

**summary:** *a few names and types have changed*

gone | replacement
---- | -----------
import jpl.*;	| import org.jpl7.*;
Hashtable	| Map<String, Term>
Query.hasMoreElements()	| Query.hasNext() or for-each
Query.nextElement()	| Query.next() or for-each
Query.hasMoreSolutions()| Query.hasNext() or for-each
Query.nextSolution()| Query.next() or for-each

## Absent from JPL7, deprecated in JPL 3.x

**summary:** *several deprecated methods have been removed*

gone | replacement
---- | -----------
Query.name() | Query.goal().name()
Query.args() | Query.goal().args()
Query.rewind() | Query.close()
Query.query() | Query.hasSolution()
Query.debugString() | 
Term.debugString() | (feel free to improvise)
Float.value() | Float.doubleValue()
Float.floatValue() | 
Integer.value() | Integer.longValue()
Integer.intValue() | 

## Absent from JPL7, disfunctional in JPL 3.x

**summary:** *Query.abort() has gone (for now)*

Some JPL 3.x features have simply been dropped, when arguably they should have been fixed.

gone | rationale
---- | ---------
Query.abort() | it never worked

## Term method orthogonality

**summary:** *all Term subclasses support all methods, reducing need for coercions*

For coding convenience, the major methods of `Term`'s concrete subclasses
(`Atom`, `Compound`, `Integer`, `Float`, `Variable` and (soon) `Dict`) are supported in JPL7
by all these classes; this reduces the need for coercions.

method | Atom | Compound | Float | Integer | Variable
------ | ---- | -------- | ----- | ------- | --------
arg(int) | | | | |
args() | | | | |
arity() | | | | |
bigValue() | | | | |
blobType() | | | | |
doubleValue() | | | | |
equals() | | | | |
floatValue() | | | | |
hasFunctor(double, int) | | | | |
hasFunctor(String, int) | | | | |
hasFunctor(int, int) | | | | |
isAtom() | | | | |
isCompound() | | | | |
isFloat() | | | | |
isInteger() | | | | |
isJFalse() | | | | |
isJNull() | | | | |
isJObject() | | | | |
isJRef() | | | | |
isJTrue() | | | | |
isJVoid() | | | | |
isListNil() | | | | |
isListPair() | | | | |
isVariable() | | | | |
jrefToObject() | | | | |
listLength() | | | | |
longValue() | | | | |
name() | | | | |
toString() | | | | |
toTermArray() | | | | |
type() | | | | |
typeName() | Atom | Compound | Float | Integer | Variable

## Some things which haven't been modernised

**summary:** *a few, harmless legacy oddities have been preserved*

JPL7's `Query.allSolutions()` should perhaps return a `java.util.List` of solutions (following the way of
the Collections Framework) but I've decided to maintain the JPL 1.x tradition of returning an array.
Note that arrays can be iterated with for-each
```java
for (Map soln : Query.allSolutions("p(X)")) {
    ... soln.get("X") ...
}
```

Term accessors should perhaps all have method names beginning with "get",
```java
t1.getArity()
t2.getName()
t3.getFloatValue()
```
but for now we keep the (concise but un-Java-idiomatic) naming from prior JPL. 
