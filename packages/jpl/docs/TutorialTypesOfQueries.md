# Tutorials - Types of queries

The high-level Java API to interact with SWI Prolog can be divided into two classes.

## One-shot queries

For the majority of applications the following **one-shot** Queries mechanisms will be sufficient:

* `boolean hasSolution()`: returns true if the query can be proved true; no solution bindings are returned.
* `Map<String,Term> oneSolution()` returning the bindings for the first solution of the query.
* `Map<String,Term>[] nSolution(long n)`: an array (collection) of bindings for the first `n` solutions. 
* `Map<String,Term>[] allSolutions()`: an array (collection) of bindings containing all the solutions.

These methods will basically attach to a Prolog engine, run the goal, collect the solutions and build the bindings as Java `Term` objects, and close, thus releasing the Prolog engine (for other queries to use).

## Iterative queries

The second type of queries one can issue via JPL is the **iterative** type of query. This is useful when there are potentially infinite or too many number of solutions available, but one needs to process up to some solution and how many is unknown at the outset (otherwise one could use `nSolutions(n)`. For example, take a query of the sort "find an open shop in the city whose distance is not more than 10km", for which Prolog has all the information about shops, their locations and open hours, but Java has the information on distance between locations (e.g., via Google Maps API). Since there could be too many shops to consider and part of the task needs to be computed in Java, an iterative query would be the solution:
```java
Query query = Query("shop(ShopId, ShopLong, ShopLat, open)"); // get an open shop and its geo location
boolean found = false;
Map<String, Term> solution = new HashMap<String,Term>;
while (query.hasMoreSolutions() && !found) { // until a good sol is found
    solution = query.nextSolution();
    float distance = GoogleDistance(ShopLong, ShopLat, myLong, myLat); // call to external tool
    if (distance < 10km) {
        found = true;
    }
}
query.close(); // we close the query to release the prolog engine
```
Basically, this is analogous to issue a goal in Prolog and iterate through all solutions via entering `;` to get "the next solution".

The two methods for operating with iterative queries are:

* `boolean hasMoreSolutions()`: This will actually command the Prolog engine attached (if none yet, will attach one) to seek the _next_ solution. If one is found, `true` will be returned, and the solution bindings will be available if needed. If there are no more solutions, then the query will be immediately closed.
    * There are two other renamings of this method: `hasNext()` and `hasMoreElements()`.
* `Map<String, Term> nextSolution()`: This method will fetch (from the Prolog engine) and return the current solution binding found in the last `hasMoreSolutions()` called. Note that it _will not instruct Prolog to find the next solution_. So, if called multiple times without calling `hasMoreSolutions()` the same binding will be returned over and over.
    * There are two other renamings of this method: `next()` and `nextElement()`.


Finally, one has to have the following two things in mind when working with iterative queries:

1. Make sure you close the query, either by exhausting all solutions or by explicitly calling its `close()` method once you are finish with it. This is because an open query gets a hold on one of the few finite Prolog engines available, so it is good practice to release the engine as soon as possible, to avoid delays or even deadlock situations.
2. While one can nest iterative and one-shot queries, nested open queries form a kind of stack and it is only possible to operate on the query at the top of the stack. 

Please refer to the section [Multi-Threaded-Queries](TutorialMultithreaded) for details on both issues.
