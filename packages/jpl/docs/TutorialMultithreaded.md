
# Tutorials - Multi-threaded Java

In the past, JPL relied on a _single_ underlying Prolog engine that could only have _one query open_ at a time. This presented difficulties for multi-threaded programs in which the programmer had no control over when Queries are executed. While JPL made as much of the High-Level Interface thread-safe as it can, the user still had to make use of synchronization in a limited set of circumstances (namely when queries remained open for extended periods) to ensure that all calls to the High-Level Interface are thread safe.

Then, Jan Wielemaker implemented a much more powerful mechanism in which a **pool of underlying SWI Prolog engines** is set-up and maintained; queries can then attach, use and detach, and available engines re-used across different Queries.

In a nutshell, when a Query is activated (i.e., the first time a solution is searched for) in a given thread, an available SWI engine is taken from the pool and _attached to a thread_. While attached to an engine, a thread can issue any number of Queries (using the engine it is attached to) with the restriction not to nest retrieval of solutions (see below for details). When all Queries in the thread have been closed (explicitly via `Query.close()` or automatically by solution exhaustion), the engine is freed and returned to the pool (i.e., becomes available and able to attach to server another thread/queries). If no pool engine is available at the time a Query is activated in a thread, query activation (and the thread itself!) delays until one is freed. 

At this point, the pool contains ten (10) engines max (constant `JPL_MAX_POOL_ENGINES` in [`src/c/jpl.c`](https://github.com/SWI-Prolog/packages-jpl/blob/master/src/c/jpl.c). This means that there could be up to 10 "contemporaneously active queries" at any point. To handle more than that, one needs to make sure that queries are closed nicely and quickly,so that the Prolog engines can be returned to the pool and used them in other threads. We note that the first engine is "special" and once attached to a thread (the first one to issue a Query) it will remain attached to it forever; so practically one has 9 engines at disposal :-)

We discuss below a few subtle points on how the pool of engines and Queries operate that may be worth taking into account when using JPL in your application.


## Query opening & termination

First, a query is open and become active when the _first solution is required_. The creation itself of a `Query` object does not activate/open the query and hence no engine is needed or attached that point. 

Remember that JPL offers one-shot queries as well as iterative queries. See [this entry](Types-of-Queries:-One-shot-vs-Iterative) for details on them.

So, all the one-shot query methods (`hasSolution()`, `oneSolution()`, `nSolutions(n)` and `allSolutions()`), will close the query itself immediately after completion, thus releasing the engine used. For many programmers, these methods suffice. 

However, if a Query is processed as an iterator-based methods (using `hasMoreSolutions()`, `hasMoreElements()`, `hasNext()`, `nextSolution()`, `next()`, or `nextElement()`), then he corresponding Query will remain open, either until all solutions have been retrieved (solution exhaustion) or the Query is explicitly closed with the `close()` method. 
 
This means that if one is not careful enough when using a Query iteratively, the whole pool of SWI engines can be used and the application code get to a deadlock situation: any new Query issued will block waiting for some engine to become available in the pool (which may never happen!).

One way to achieve this is to process _all_ solutions, after which JPL will automatically close the query:

```java
Query query = // obtain Query
while (query.hasMoreSolutions()) {
    Map<String, Term> solution = query.nextSolution();
    // process solution as quickly as possible...
}
```
Another alternative is to explicitly close the Query once all required work has been done:

```java
Query query = // obtain Query
int x = 0;
while (query.hasMoreSolutions() && x < 100) {
    Map<String, Term> solution = query.nextSolution();
    // process solution as quickly as possible...
    x = solution.get("X").intValue;
}
query.close();
```

Observe that we may exit the loop without exhausting all solutions, so we make sure we close the query.


## Nesting queries in the same JVM thread

As stated, once a Query is activated in a JVM thread, a Prolog engine is attached to the thread (and the query). The thread itself can issue nested queries of different sort and they will all make use of the same engine without blocking. This is good. 

When using one-shot queries (e.g., `hasSolution()` or `oneSolution()`), then the query is opened, solved, and _closed immediately_, which means that the engine used for the query is returned back to the pool right away. If a next query in the thread is issued, it may then end up using another engine, if the engine used before is not available anymore (as some other thread-query has attached to it).

However, **each JVM thread can only attach to at most one SWI Prolog engine**. This implies that if one has to be a bit careful nesting iterator queries (e.g., `hasMoreSolution()` and `nextSolution()`). Basically,
a JVM thread may nest two or more active queries, thus forming a _stack_ of open queries. It is only possible to operate on the query at the top of the stack, the open query. Hence, the thread **may not interleave the retrieval of solutions** from two open queries. If you want to retrieve a new solution for a past iterator-type query that is still open in the thread, all queries done after that must be closed.

For example, this will work well:

```java
Query query1 = // obtain Query somehow``
for (int i = 0; i < 3 && query.hasMoreSolutions(); ++i) {
     Map<String, Term> solution = query1.nextSolution();
     // process solution...
 }
 Query query2 = // obtain new Query somehow
 while (query2.hasMoreSolutions()) { // process all query2: open, process fully, close
     Map<String, Term> solution = query2.nextSolution();
     // process solution...
 }
 while (query1.hasMoreSolutions()) { // finish processing query1
     Map<String, Term> solution = query2.nextSolution();
     // process solution...
 }
```

Here, while `query1` remains open while processing `query2`, new solutions for `query1` are only fetched _after `query2` has closed_ due to solution exhaustion. Observe that because  `query1` remains open, `query2` will run on the same attached engine.

On the other hand, suppose we try to fetch a next solution for `query1` while `query2` is still open and active:

```java
Query query1 = // obtain Query somehow
for (int i = 0; i < 3 && query.hasMoreSolutions(); ++i) {
     Map<String, Term> solution = query1.nextSolution();
     // process solution for query1
} 
Query query2 = // obtain new Query somehow
while (query2.hasMoreSolutions()) { // process all query2: open, process fully, close
    Map<String, Term> solution = query2.nextSolution();
    // process solution for query2

    if (query1.hasMoreSolutions()) {  // THIS MAY/WILL BREAK! nested query2 is still open/active
        Map<String, Term> solution = query1.nextSolution();
        // process new solution for query1...
    }
}
```

This will break if `query2` can yield more than one solution, because it will stay open at the point that we are trying to a new solutions for `query1`! Remember the open queries form a sort of _stack_ and hence one can only operate on the query at the top of the stack, the "active" one. This code will yield an error of this form:

```bash
[Thread 1 (main) at Thu Oct  4 19:42:08 2018] pl-vmi.c:2024: PL_next_solution: Assertion failed: FR == &QF>top_frame

 C-stack trace labeled "assert_fail":
  [0] PL_strtod() at ??:? [0x7fd791e00ff4]
  [1] __assert_fail() at ??:? [0x7fd791dd0127]
  [2] PL_next_solution() at ??:? [0x7fd791d4910a]
  [3] Java_org_jpl7_fli_Prolog_next_1solution() at ??:? [0x7fd7716142a1]
Aborted
```

For sure, you do not want to see this... :-)


## Passing queries between threads

Passing Queries around threads is dangerous, due to the way that these are associated to Prolog engines attached the the thread a query is running on. Unless really needed, I recommend not doing so; it is best to duplicate the query.

In principle, a `Query` object can be passed to another thread, as long as it has _not_ yet been opened. Trying to pass an open Query to another thread will result in segmentation fault:

```bash
JRE version: Java(TM) SE Runtime Environment (8.0_181-b13) (build 1.8.0_181-b13)
# Java VM: Java HotSpot(TM) 64-Bit Server VM (25.181-b13 mixed mode linux-amd64 compressed oops)
# Problematic frame:
# C  [libswipl.so+0x32a7a]  PL_next_solution+0x9a
```

Interestingly, one can pass a _closed_ query object to other threads, even to multiple ones. These threads can then "activate" the query (by fetching its next solution) and iterate through its solutions. Said so, things can become quite tricky when doing so. Since at any point in time:

1. an open query uses a Prolog engine from the pool; and
2. an engine can only be attached to one thread, 

it is not possible to run the same query object _simultaneously_ in different threads. You will get segmentation fault similar to the above. So, if one passes a query to threads `A` and `B`, and thread `A` opens it, then `B` has to hold of until either:

1. thread `A` closes the query all together. In this case  the query is reset and inactive, and any thread can re-activate and re-open it; or
2. thread `A` finishes completely, thus detaching from the engine and returning it to the pool.

Importantly, in the second case, when thread `B` tries to fetch the next solution, it will continue where thread `A` left, because the query remained open. That is, if thread `A` fetched the first 5 solutions and then died completely (the thread), then thread `B` will start from solution 6 onwards. Again, if thread `B` tries to fetch a solution while thread `A`  is still active with the query opened and an engine attached, segmentation fault will occur: the query is active in a Prolog engine, and that engine is already  attached to thread `A`.


## More information & acknowledgments

Much of the information reported here was obtained from @anionic and @JanWielemaker!

More information about this can be found in the following documentation written and recently updated by @anionic:

[Queries from multi-threaded applications](https://jpl7.org/JavaApiOverview#queries-from-multi-threaded-applications)

as well in this related issue at Github: https://github.com/SWI-Prolog/packages-jpl/issues/15


