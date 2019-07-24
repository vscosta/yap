# Tutorials - Getting started

Assume we have a testing Prolog file with this content:
```prolog
child_of(joe, ralf). 
child_of(mary, joe). 
child_of(steve, joe).
descendent_of(X, Y) :- 
    child_of(X, Y). 
descendent_of(X, Y) :- 
    child_of(Z, Y), 
    descendent_of(X, Z).
```
You may wish to load this database into an interactive Prolog session to experiment with the predicates in this database before experimenting with JPL.


## Initializing The Prolog engine

Although the `org.jpl7.JPL` class provides a number of methods for initializing the Prolog engine from within Java, their use is not usually necessary: Prolog will be automatically initialised with default parameters at the first attempt to use it.



## Consulting the Prolog database from its text file

In an ordinary interactive Prolog session, we would load the above Prolog database using the Prolog `consult/1` predicate, a built-in predicate in standard Prolog.  Note, however, that as a Prolog predicate, "calling" `consult/1` is just an example of making a Prolog query, and this is how we perform it with JPL.

First, we construct an instance of `Query`, whose name is consult and whose arguments (just one) comprise the atom 	`'test.pl'`:
```java
Query q1 = 
    new Query( 
	"consult", 
	new Term[] {new Atom("test.pl")} 
    );
```
    
Then, we call the `hasSolution()` method of this `Query` object, which returns a `boolean` value indicating its success:
```java
System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
```
At this point, this process may seem a bit long-winded; however, you should soon see that the classes are sufficiently general that they provide a robust and powerful interface into the Prolog engine. 


## Querying the database

Using the same technique, we can query the Prolog database about inferences it can make.  To ask whether the Prolog query `child_of(joe,ralf)` is true, given the above Prolog database, for example, we write:
```java
Query q2 = 
  new Query( 
      "child_of", 
      new Term[] {new Atom("joe"),new Atom("ralf")} 
  );
System.out.println( 
  "child_of(joe,ralf) is " + 
  ( q2.hasSolution() ? "provable" : "not provable" ) 
);
```
To take an example that requires a bit more work on the part of the Prolog engine, on the other hand, we can ask whether `descendent_of(steve,ralf)` is true:
```java
Query q3 = 
  new Query( 
      "descendent_of", 
      new Term[] {new Atom("steve"),new Atom("ralf")} 
  );
System.out.println( 
  "descendent_of(joe,ralf) is " + 
  ( q3.hasSolution() ? "provable" : "not provable" ) 
);
```

## Querying with variables

Ground queries (those without variables) like the above are relatively straightforward; they are essentially either provable or not, and there is typically no point in backtracking.  

Once we use variables, however, things get a bit more complicated. Using the `Variable` class, we can construct a non ground query; and using other methods of `Query` we can obtain a solution in the form of a `java.util.Map`.  If the `Query` has one or more solutions, then its `Query.oneSolution()` method returns a `Map<String,Term>` representing the first solution, otherwise it returns null:
```java
Variable X = new Variable("X");
Query q4 = 
  new Query( 
      "descendent_of", 
      new Term[] {X,new Atom("ralf")} 
  );

java.util.Map<String,Term> solution;

solution = q4.oneSolution();

System.out.println( "first solution of descendent_of(X, ralf)"); 
System.out.println( "X = " + solution.get("X"));
```
The `Map` contains **bindings** in the form of `Terms`, each of which is indexed by its corresponding `Variable` in the `Query`.
      
 
## Finding all solutions

The previous query finds only the _first_ solution.  Often, however, one wants all solutions, or at least more than just the first.  

The `Query` class also provides the `allSolutions()` method, which returns an array of zero or more `Map`, each of which represents a given solution.

In this example we reuse the query `q4`, which was reset to its initial state by the call of `oneSolution()`, and instead call `allSolutions()`, which returns an array of solutions:
```java
java.util.Map<String,Term>[] solutions = q4.allSolutions();
for ( int i=0 ; i < solutions.length ; i++ ) { 
  System.out.println( "X = " + solutions[i].get('X")); 
}
```

If one wants not all but, say, at most `n` solutions, one could use method `nSolutions(n)`. Again it will return an array of at most `n` solution bindings:

```java
java.util.Map<String,Term>[] solutions = q4.nSolutions(20);
for ( int i=0 ; i < solutions.length ; i++ ) { 
  System.out.println( "X = " + solutions[i].get('X")); 
}
```
Observe that even though we asked for at most twenty solutions, less could be returned since there may be less than such number of solutions.


## Iterating through solutions


Equivalently, one can obtain each solution by exploiting the `Iterator` interface, which the `Query` class implements.  This will allow a Java program to iteratively go through solutions one-by-one.

In this example, we iteratively call `hasMoreSolutions()` and `nextSolution()` to exhaustion:

```java
System.out.println( "each solution of descendent_of(X, ralf)");
while ( q4.hasMoreSolutions()) {    
	solution = q4.nextSolution();
    System.out.println( "X = " + solution.get("X"));
}
```
The `hasMoreSolutions()` method of the Query class returns a `boolean`, indicating whether there are any solutions "left" in the query. If the answer to this is 'yes', then the solution can be obtained in the form of a `Map<String,Term>` by the `nextSolution()` method.

As with any iterator in Java, it is safest to call `nextSolution()` only when it has been checked that there is indeed a "next" element in the iteration (via `hasMoreSolutions()`). Otherwise, we would run the risk of getting a `java.util.NoSuchElementException` exception.

In this final example, we reuse the previous variable `X` with a new variable `Y` in a new query `q5`:
```java
Variable Y = new Variable();
Query q5 = 
  new Query( 
      "descendent_of", 
      new Term[] {X,Y} 
  );

while ( q5.hasMoreSolutions() ){ 
  solution = q5.nextSolution(); 
  System.out.println( "X = " + solution.get("X") + ", Y = " + solution.get("Y")); 
}
```

**Note:** when using an iterator query, the query stays open until all solutions have been retrieved or the query is closed explicitly via the `close()` method. When open, the query is attached to a Prolog engine, and there are a finite number of them, so care must be taken in a multi-threaded application to avoid a deadlock situation. Please refer to the [Multi-Threaded-Queries](TutorialMultithreaded) for details.

Also, check to check all the high-level API provided to access Prolog from Java, see the entry [Types-of-Queries:-One-shot-vs-Iterative](TutorialTypesOfQueries).
