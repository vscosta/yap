# Java API - Overview

The Java API comprises public Java classes which support:
    * constructing Java representations of Prolog terms and queries;
    * calling queries within SWI-Prolog engines; and
    * retrieving (as Java representations of Prolog terms) any bindings created by a call.


## The class hierarchy

The ***API*** consists of the following class hierarchy:

```java
org.jpl7
 +-- JPL
 +-- JPLException
 |    +-- PrologException
 +-- Query
 +-- Term
 |    +-- Atom
 |    +-- Compound
 |    +-- Float
 |    +-- Integer
 |    +-- Variable
 |    +-- JRef
 +-- Util
 +-- Version
```

`org.jpl7.Term` is an abstract class: only its subclasses can be instantiated.

Each instance of `org.jpl7.Query` contains a `Term` (denoting the goal which is to be proven), and much more besides.

Each instance of `org.jpl7.Compound` has a java.lang.String name and an array of `Term` arguments. For compatibility with SWI-Prolog version 7's extension [Compound terms with zero arguments](http://www.swi-prolog.org/pldoc/man?section=ext-compound-zero), the argument array can be of zero length.

## Initializing and terminating Prolog

Typically, this is automatic.
 
JPL lazily initializes the Prolog VM, if necessary, when the first query is activated, using default initialization arguments (command line options). Before initialization takes place, these default values can be read, and altered.

```java
public String[] getDefaultInitArgs();
public void setDefaultInitArgs(String[] args);
```

After initialization, the parameter values which were actually used can be read.

```java
public String[] getActualInitArgs();
```

This method returns `null` if initialization has not occurred, and thus it can be used as a test. This allows Java library classes to employ JPL without placing any burden of initialization upon the applications which use them. It can also ensure that the Prolog VM is initialized only if and when it is needed.

Explicit initialization is supported:

```java
public void init();
public void init(String args[]);
```

Java code which requires a Prolog VM to be initialized in a particular way can check whether initialization has already occurred: if not, it can specify parameters and force it to be attempted; if so, it can retrieve and check the initialisation parameters actually used, to determine whether the initialization meets its requirements.

JPL does not support reinitialization of a Prolog VM, but some command line options merely set flags, which can be altered later by calling **set\_prolog\_flag/2** via a JPL query.

For details about the legal parameter values, see [2.4 Command Line Options](http://www.swi-prolog.org/pldoc/man?section=cmdline) in the [SWI Prolog Reference Manual](http://www.swi-prolog.org/pldoc/doc_for?object=manual). Most users will rely on automatic initialization.

## Creating terms

The `Term`-based classes in the `org.jpl7` package are a structured concrete syntax for Prolog terms: they are not references to actual terms within the Prolog engine; rather, they are a means for
constructing queries which can be called within Prolog, and they are also a means for representing (and exploring) the results of such calls. In particular, instances of `org.jpl7.Variable` are never bound nor shared; they are merely tokens.

`Term` instances are never changed by any activity within the Prolog engine: indeed; it doesn't know of their existence.

The `Term` class is abstract, so it cannot be directly instantiated; to create a Term, create an instance of one of its subclasses.

### Atoms 

An `org.jpl7.Atom` instance represents a SWI Prolog text atom. To create an `Atom`, pass a (String) name to its constructor:

```java
Atom a1 = new Atom("aristotle");
Atom a2 = new Atom("alexander");
```

As with Java strings, SWI Prolog text atoms represent arbitrarily long sequences of Unicode characters (including ASCII's nul).

Two `Atom` instances with the same name are effectively identical. Feel free to reuse atoms when constructing compound terms.

The name of an atom need not be lower case: it can be any UCS string.

An atom's name is retrieved with its `name()` method, e.g.

```java
a1.name()
```

See [org.jpl7.Atom JavaDoc](http://192.168.1.49:8080/jpl7/doc/org/jpl7/Atom.html) for details of how SWI Prolog version 7's strings and blobs (including reserved symbols) are accommodated.

### Variables

`org.jpl7.Variable` instances have identifying names, which must comply with conventional Prolog source text syntax.

```java
Variable v1 = new Variable("X"); // a regular variable

Variable v2 = new Variable("_"); // an "anonymous" variable

Variable v3 = new Variable("_Y"); // a "dont-tell-me" variable, whose bindings we don't want to know
```

They are just tokens, and do not behave like Prolog variables.

### Integers

A `org.jpl7.Integer` is a specialized `org.jpl7.Term` which holds a Java long value or a `java.math.BigInteger` object. This class corresponds to the Prolog *integer* type.

```java
org.jpl7.Integer i = new org.jpl7.Integer(5);
```

Be careful to avoid confusion with `java.lang.Integer`, e.g. by qualifying the class name as in the example above.

The `org.jpl7.Integer` class has an `intValue()` accessor to obtain the `int` value of an instance, and also `longValue()`, `floatValue()` and `doubleValue()` (just like `java.lang.Integer` has).

If `isBig()` returns true, then the value is outside the range of a Java `long`, and is retrieved by `bigValue()`.

### Floats

A `org.jpl7.Float` is a specialized `org.jpl7.Term` which holds a Java `double` value. This class corresponds to the Prolog *float* type (64-bit ISO/IEC in SWI Prolog).

```java
org.jpl7.Float f = new org.jpl7.Float(3.14159265);
```

As with integers, take care to avoid confusion between `org.jpl7.Float` and `java.lang.Float`.

The `org.jpl7.Float` class has a `doubleValue()` accessor to obtain the `double` value of an instance, and also a `floatValue()` accessor.

### Compound terms

A `org.jpl7.Compound` is a specialized `org.jpl7.Term` which contains a name and an array of `org.jpl7.Term` arguments, and can be constructed e.g.

```java
Compound t1 = new Compound(
    "teacher_of",
    new Term[] {
        new Atom("aristotle"),
        new Atom("alexander")
    }
);
```

Note the use of Java's *anonymous array* syntax

```java
new Term[] {..., ...}
```

to specify any quantity (perhaps zero) of arguments.

In this example, the Java variable `t1` refers to a **Compound** instance, which represents the Prolog term *teacher\_of(aristotle, alexander)*.

To obtain the (String) name of a **Compound**, use the **name()** accessor method.

```java
public String name();
```

To obtain the arity of a **Compound**, use the **arity()** accessor method.

```java
public int arity();
```

To obtain an array of a **Compound**'s arguments, use the **args()** accessor method.

```java
public Term[] args();
```

To obtain the *ith* argument of a compound (numbered from 1), use the **arg()** accessor method (with an **int** parameter value between 1 and Arity inclusive).

```java
public Term arg(int i);
```

To obtain the *ith* argument of a compound (numbered from 0), use the **arg0()** accessor method (with an **int** parameter value between 0 and Arity-1 inclusive).

```java
public Term arg0(int i);
```

## Creating queries

A `Query` contains a `Term`, representing a Prolog goal:

```java
Term goal = new Compound("teacher_of", new Term[] {new Atom("aristotle"), new Atom("alexander")});
Query q = new Query(goal);
```

The `Query q` in this example represents the Prolog query

```prolog
?- teacher_of(aristotle, alexander).
```

`org.jpl7.Query` implements `java.util.Iterator`, allowing a query's solutions to be retrieved one at a time.

## Querying Prolog

To ask the Prolog engine a query, one first constructs a `Query` instance, as in the above example,
and then uses the `java.util.Iterator` interface, which the `Query` class implements,
to obtain solutions (where a "solution" is what is known in logic programming jargon as a *substitution*,
which is a collection of *bindings*, each of which relates one of the variables within the query's goal to a `Term` representation of the Prolog term to which the corresponding Prolog variable was bound by the proof).

```java
public interface Iterator {
    public boolean hasNext();
    public Object next();
}
```

The `hasNext()` method can be used to determine whether a query has any (or any further) solutions. In the above example, the method call

```java
q.hasNext()
```

returns `true` if the Prolog query *teaches(aristotle, alexander)* is provable, and `false` otherwise. In this example, the Prolog query is a ground term, so the "solution" to the query is merely a truth value, and is given by the `hasNext()` method.

Where a query contains variables, on the other hand, its execution yields a sequence of bindings
of the variables' names to `Term` instances.
JPL uses a `java.util.Map<String, Term>` (implemented as a `java.util.HashMap`) to represent these bindings;
the objects in the map are `org.jpl7.Term` instances, keyed (uniquely) by the `String` names of their associated variables.

For example, to print all of Aristotle's pupils, i.e. all the bindings of `X` which satisfy `teaches(aristotle,X)`,
one could write

```java
Query q = new Query("teaches", new Term[] {new Atom("aristotle"), new Variable("X")});
while (q.hasNext()) {
    Map binding = q.next();
    Term t = (Term) binding.get("X");
    System.out.println(t);
}
```

or, more concisely

```java
for (Map m : new Query("teaches", new Term[] {new Atom("aristotle"), new Variable("X")})) {
    System.out.println(m.get("X"));
}
```

or, using a convenience constructor which builds the term from Prolog source text

```java
for (Map m : new Query("teaches(aristotle,X)")) {
    System.out.println(m.get("X"));
}
```

If a query's goal contains no variables (i.e. it is ground), the `Query.next()`method will return an emnpty map for each solution.

If a query's goal contains more than one occurrence of some (named) variable, then each solution will have only one binding for that name.

### Obtaining one solution

Often, you'll want just the first solution to a query; `org.jpl7.Query` has a method for this:

```java
public final Map<String, Term> oneSolution();
```

If the query has no solutions, this method returns `null`; otherwise, a non-null return indicates success. If the query is ground (i.e. contains no variables), the returned map will be empty (i.e. will contain no bindings).

### Obtaining all solutions 

You may want all solutions to a query; `org.jpl7.Query` has a method for this:

```java
public final Map<String, Term>[] allSolutions();
```

The returned array will contain all the query's solutions, in the order they were obtained (as with Prolog's findall/3, duplicates are not removed). If the query has no solutions, this method returns an empty array.

### Ground queries

Sometimes an application is interested only in whether a query is provable, but not in any details of its possible solutions; `org.jpl7.Query` has a method for this common special case:

```java
public final boolean hasSolution();
```

This method is equivalent to calling **oneSolution** and asking whether the return value is non-**null** (i.e. whether the query succeeded).

### Terminating queries

Queries terminate automatically when `hasNext()` returns `false` (or when `next()` throws an exception), and once a query has terminated, its engine is returned to the pool for reuse (by any thread).

To terminate a query before all of its solutions have been exhausted, use its `close()` method:

```java
public final void close();
```

This method stops a query, setting it back into a state where it can be restarted. Here is an example in which the first three solutions to the query are obtained:

```java
Query query = // obtain Query somehow
for (int i = 0; i < 3 && query.hasNext(); ++i) {
    Map<String, Term> solution = query.next();
    // process solution...
}
query.close();
```

You may call `close()` on an inactive query without ill-effect, and you should *always* call close if you have not exhausted all solutions to a query, otherwise the associated Prolog engine will not be released.

If you are using the `allSolutions()`, `hasSolution()`, `nSolutions()`, or `oneSolution()` methods, you need not worry about closing the query; it is done automatically for you.

See [Types of Queries](TutorialTypesOfQueries.md) guide for further details and explanation on both one-shot and iterative queries that can be issued from Java.

## Queries from multi-threaded applications

JPL maintains a finite pool of Prolog engines, one of which is allocated to a query when it is activated (i.e. when, one way or another, a solution is requested). A query's engine is returned to the pool when it is closed (explicitly or automatically).

If no pool engine is available when a query is activated, the activation is blocked until an engine becomes available.

Each JVM thread can have at most one Prolog engine attached. A thread may nest (stack) two or more active queries, e.g. open and close a second query while a first is active, but it may not interleave the retrieval of solutions from two open queries.

Note that engines cannot communicate thread\_local predicates or global variables. That means that you can only use these from Java within a single query. If a thread activates two consecutive queries, it may get two different engines.

Note also that, once a thread has activated a query, it cannot pass it to another thread: solutions of a query must be retrieved by the thread which activated it.

See [Multi Threaded Queries](TutorialMultithreaded.md) guide for further details and subtle issues when potentially issuing multiple queries from various threads.

## Exceptions

JPL provides crude but adequate exception handling. The base class for all exceptions is `org.jpl7.JPLException`, which specialises `java.lang.RuntimeException` and hence is unchecked. Converting the
exception to a **java.lang.String** should provide some descriptive information about the reason for the error. JPL's only other exception class is `org.jpl7.PrologException`, which extends `org.jpl7.JPLException`.

A `org.jpl7.PrologException` is thrown either during execution of a Prolog built-in predicate or by an explicit call of `throw/1` by application code.

## Debugging

Each **Term** type (together with the **Query** class) supports an implementation of **toString()** which returns a more-or-less familiar Prolog textual representation of the **Term** or **Query**.

In general, **Term** and **Query** instances are represented in the form (*type data*), where *type* is the name of the type (e.g., **Atom**, **Compound**, **Tuple**, etc.), and *data* is a representation of the contents of the **Term**. For example, if the **Term** is an **Atom**,
the data is the **Atom**'s name. The arguments of **Compounds** are represented by comma-separated lists within square brackets ('\[' '\]').

Viewing the structure of a term or query can be useful in determining whether an error lies on the Prolog or Java side of your JPL applications.

## Version information

To obtain the current version of JPL you are using, you may obtain a static reference to the `org.jpl7.Version` class by calling the `org.jpl7.JPL#version` static method. This will return a `org.jpl7.Version` structure, which has the following final fields:

```java
package org.jpl7;
public class Version {
    public final int major;                // e.g. 7
    public final int minor;                // e.g. 4
    public final int patch;                // e.g. 0
    public final java.lang.String status;  // e.g. "alpha"
}
```

You may wish to use this class instance to obtain fine-grained information about the current JPL version, e.g.

```java
if (JPL.version().major == 7) {
```

You may also call the `version_string()` static method of the `org.jpl7.JPL` class. This will return a String representation of the current JPL version.

The version string can be written to the standard output stream by running the **main()** method of the `org.jpl7.JPL` class.

```bash
linux% java org.jpl7.JPL
JPL 7.4.0-alpha
```

## Gotchas

### Variables are named

Instances of `org.jpl7.Variable` have names, and bindings are retrieved by name, e.g.

```
Map m = org.jpl7.Query.oneSolution("statistics(heap, X)");
long heapsize = m.get("X");
```

### Argument numbering

The `Term[]` args of a `Compound` are indexed (like all Java arrays) from zero, whereas in Prolog the args of a structure are conventionally numbered from one.

### Representing `@(null)`

there is no `org.jpl7.JNull` class: instead, use `new JRef(null)` to represent `@(null)` (which itself represents Java's `null`). If you don't know what this all means, don't worry: it only affects those
writing hybrid Java+Prolog programs which call each other nestedly.

### All solutions of a Query with no solutions

`Query.allSolutions()` returns an empty array of `Map<String, Term>` if the query has no solutions (in 1.x versions it inconsistently returned null).
