package org.jpl7;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.NoSuchElementException;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.engine_t;
import org.jpl7.fli.fid_t;
import org.jpl7.fli.predicate_t;
import org.jpl7.fli.qid_t;
import org.jpl7.fli.term_t;

import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * A Query instance is created by an application in order to query the Prolog
 * database (or to invoke a built-in predicate). It is initialised with a
 * Compound (or Atom) denoting the goal which is to be called, and also contains
 * assorted private state relating to solutions. In some future version, it may
 * contain details of the module in which the goal is to be called.
 * <p>
 * A Query is either open or closed: when closed, it has no connection to the
 * Prolog system; when open, it is linked to an active goal within a Prolog
 * engine.
 * <p>
 * The Query class implements the Enumeration interface, through which one can
 * obtain successive solutions. The Enumeration hasMoreElements() method returns
 * true if the call or redo succeeded (otherwise false), and if the call or redo
 * did succeed, the nextElement() method returns a Map representing variable
 * bindings; the elements in the Map are Terms, indexed by the (String) names of
 * the Variables with which they are associated. For example, if <i>p(a)</i> and
 * <i>p(b)</i> are facts in the Prolog database, then the following is
 * equivalent to printing all the solutions to the Prolog query <i>p(X)</i>:
 *
 * <pre>
 * Variable X = new Variable(&quot;X&quot;);
 * Term arg[] = { X };
 * Query q = new Query(&quot;p&quot;, arg);
 *
 * while (q.hasMoreElements()) {
 *	Term bound_to_x = ((Map) q.nextElement()).get(&quot;X&quot;);
 *	System.out.println(bound_to_x);
 * }
 * </pre>
 *
 * Make sure to close the Query (using the close() method) if you do not need
 * any further solutions which it may have. It is safe (although redundant) to
 * close a Query whose solutions are already exhausted, or which is already
 * closed.
 *
 * To obtain just one solution from a Query, use the oneSolution() method.
 *
 * To obtain all solutions, use the allSolutions() method.
 *
 * To obtain at most N solutions, use the nSolutions() method.
 *
 * To determine merely whether the Query is provable, use the hasSolution()
 * method (i.e. has at least one solution).
 * <hr>
 * Copyright (C) 2007 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * <ol>
 * <li>Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * <li>Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 *
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 */
public class Query implements Iterable<Map<String, Term>>, Iterator<Map<String, Term>> { // was
																							// Enumeration<Object>

    private static final Logger LOGGER = Logger.getLogger( Query.class.getName() );
    private static final Level levelLog = Level.INFO;

	/**
	 * the Compound or Atom (but not Dict, Float, Integer or Variable)
	 * corresponding to the goal of this Query
	 */
	protected final Term goal_; // an Atom or Compound; set by all initialisers
	protected final String hostModule = "user"; // until revised constructors
												// allow this to be specified
	protected final String contextModule = "user"; // until revised constructors
													// allow this to be
													// specified

	/**
	 * Returns the Term (Atom or Compound) which is the goal of this Query
	 *
	 * @return a Term representing the goal of this Query
	 */
	public final Term goal() {
		return goal_;
	}

	/**
	 * Returns the context module for this Query
	 *
	 * @return a String representing the context in which the goal will be run
	 */
	public final String getContext() { return contextModule; }



	/**
	 * This constructor creates a Query whose goal is the specified Term. The
	 * Query is initially closed. <b>NB</b> Creating an instance of the Query
	 * class does not result in a call to a Prolog engine. <b>NB</b> The goal
	 * can be a Compound or an Atom (Atom extends Compound), but cannot be an
	 * instance of jpl.Float, jpl.Integer or jpl.Variable.
	 *
	 * @param t the goal of this Query; must be Atom or Compound
         * @throws JPLException  if term provided is not of right sort Atom or Compound
	 */
	public Query(Term t) {
	        LOGGER.setLevel(levelLog);  // Set the logging level for this query
		if ((t instanceof Atom) || (t instanceof Compound)) {
            		this.goal_ = t;
		} else if (t instanceof Float) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not a Float)");
		} else if (t instanceof Integer) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not an Integer)");
		} else if (t instanceof Variable) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not a Variable)");
		} else {
			throw new JPLException("a Query's goal must be an Atom or Compound");
		}
	}

    /**
     * This constructor builds a Query from the given Prolog source text, "as is".
     * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
     *
     * @param text the complete Prolog source text of this Query
     * @throws PrologException containing error(syntax_error(_),_) if text is invalid as a term.
     * @throws JPLException  if term provided is not of right sort Atom or Compound
     */
    public Query(String text) {
        this(Util.textToTerm(text));
    }


    /**
	 * If text denotes an atom, this constructor is shorthand for
	 * new Query(new Compound(name,args)).
	 *
	 * If text denotes a term containing N placehholder symbols ? and there are N args,
	 * each ? is replaced by its corresponding arg to provide the new Query's goal.
	 *
	 * @param text
	 *            the name of the principal functor of this Query's goal
	 * @param args
	 *            the arguments of this Query's goal
	 */
	public Query(String text, Term[] args) {
		this(buildQueryTerm(text, args));
	}

	// Just a convenience  shortcut case for a single arg; will build a monadic query "text(arg)"
	public Query(String text, Term arg) {
		this(text, new Term[] { arg });
	}


    /**
     * Builds a Term representing a query
     *
     * If text denotes an atom, it builds a Term of the form text(args)
     * If text denotes a compound term containing N placehholder symbols ?, then args
     * will be injected sequentially (has to be same number of ? as args)
     *
     * @param text
     *            the name of the principal functor of this Query's goal
     * @param args
     *            the arguments of this Query's goal
     */
	private static Term buildQueryTerm(String text, Term[] args) {
		Term t = Util.textToTerm(text);
		if (t instanceof Atom) {
			return new Compound(text, args);
		} else {
			return t.putParams(args);
		}
	}








	/**
         * This method is required by Iterator interface
	 * a Query is its own Iterator
	 *
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Map<String, Term>> iterator() {
		return this;
	}

    /**
     * This method is required by Iterator interface
     * It is a wrapper for {@link #hasMoreSolutions()}
     *
     * @see java.util.Iterator#hasNext()
     */
	public boolean hasNext() {
	    return hasMoreSolutions();
	}

	/**
         * This method is required by Iterator interface
         * It is a wrapper for {@link #nextSolution()}
	 *
	 * @see java.util.Iterator#next()
	 */
	public Map<String, Term> next() {
		return nextSolution();
	}

	/**
	 * This method (required by Iterator interface) is a no-op
	 *
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		// no op
	}

    /**
     * This method implements part of the java.util.Enumeration interface.
     * It is a wrapper for {@link #hasMoreSolutions()}.
     *
     * @return true if the Prolog query yields a (or another) solution, else
     *         false.
     */
    public final boolean hasMoreElements() {
        return hasMoreSolutions();
    }

    /**
     * This method implements part of the java.util.Enumeration interface.
     * It is a wrapper for {@link #nextSolution()}
     * <p>
     *
     * @return A Map representing a substitution.
     */
    public final Object nextElement() {
        return nextSolution();
    }


    /**
	 * These variables are used and set across the hasMoreElements and
	 * nextElement Enumeration interface implementation
	 */
	private boolean open = false;
	// the following state variables are used and defined only if this query is
	// open:
	private engine_t engine = null; // handle of attached Prolog engine iff
									// open, else null
	private predicate_t predicate = null; // handle of this Query's predicate
											// iff open, else undefined
	private fid_t fid = null; // id of current Prolog foreign frame iff open,
								// else null
	private term_t term0 = null; // term refs of this Query's args iff open,
									// else undefined
	private qid_t qid = null; // id of current Prolog query iff open, else null

        private Boolean hasNextSolution = null; // is there a next solution? null means "we do not know yet, haven't fetch"


	/**
	 * Whether the query is open.
	 *
	 * @return true if the query is open, otherwise false.
	 */
	public final boolean isOpen() {
		return open;
	}

	/**
	* Returns the engine attached to this query
	*
	* @return a long number representing the id of the SWI Prolog engine used in this query
	*/
	public final long getEngine() { return engine.value; }


        /**
	 * This method returns true if JPL was able to initiate a "call" of this
	 * Query within a Prolog engine. It is designed to be used with the
	 * nextSolution() method to retrieve one or more substitutions in the form
	 * of Maps. To iterate through all the solutions to a Query, for example,
	 * one might write
	 *
	 * <pre>
	 * Query q = // obtain Query reference
	 * while (q.hasMoreSolutions()) {
	 *     Map solution = q.nextSolution();
	 *     // process solution...
	 * }
	 * </pre>
	 * @return true if the Prolog query succeeds; otherwise false.
	 */
	public final boolean hasMoreSolutions() {
		if (hasNextSolution == null) {
	            if (!open) open();
	            hasNextSolution = fetchNextSolution();
	        }
	        return hasNextSolution;
	}

	/**
	 * This method returns true if JPL was able to initiate a "call" of this
	 * Query within the Prolog engine. It is designed to be used with the
	 * getSolution() and close() methods to retrieve one or more substitutions
	 * in the form of Maps.
	 *
	 * <pre>
	 * Query q = // obtain Query reference
	 * Map soln;
	 * q.open();
	 * while ((soln = q.getSolution()) != null) {
	 *      // process solution...
	 * }
	 * </pre>
	 * <p>
	 * If this method is called on an already-open Query, or if the query cannot
	 * be set up for whatever reason, then a JPLException will be thrown.
	 */
	public final void open() {
		if (open) {
			throw new JPLException("Query is already open");
		}
		if (Prolog.thread_self() == -1) { // this Java thread has no attached
											// Prolog engine?
			engine = Prolog.attach_pool_engine(); // may block for a while, or
													// fail
			//System.out.println("JPL attaching engine[" + engine.value + "] for " +
			//		   this.hashCode() + ":" + this.toString());
		} else { // this Java thread has an attached engine
			engine = Prolog.current_engine();
			//System.out.println("JPL reusing engine[" + engine.value + "] for " +
			//		   this.hashCode() + ":" + this.toString());
		}
		//
		// here, we must check for a module prefix, e.g.
		// jpl:jpl_modifier_bit(volatile,T)
		String module;
		Term goal;
		if (goal_.hasFunctor(":", 2)) {
			if (goal_.arg(1).isAtom()) {
				module = goal_.arg(1).name();
			} else if (goal_.arg(1).isVariable()) {
				throw new PrologException(Util.textParamsToTerm("error(instantiation_error,?)", new Term[] { goal_ }));
			} else {
				throw new PrologException(
						Util.textParamsToTerm("error(type_error(atom,?),?)", new Term[] { goal_.arg(1), goal_ }));
			}
			goal = goal_.arg(2);
		} else {
			module = contextModule;
			goal = goal_;
		}
		predicate = Prolog.predicate(goal.name(), goal.arity(), module); // was
																			// hostModule
		fid = Prolog.open_foreign_frame();
		Map<String, term_t> varnames_to_vars = new HashMap<String, term_t>();
		term0 = Term.putTerms(varnames_to_vars, goal.args());
		// THINKS: invert varnames_to_Vars and use it when getting
		// substitutions?
		qid = Prolog.open_query(Prolog.new_module(Prolog.new_atom(module)), Prolog.Q_CATCH_EXCEPTION, predicate,
				term0);
		open = true;
		hasNextSolution = null; // we have not fetch yet any solution
	}

    /**
     * Reset the query to its start: closed and no solution found so far. This will allow a query to re-start
     *
     */
    public final void reset() {
        close();
        hasNextSolution = null;
    }

    /**
     * Tell Prolog engine to fetch the next solution for the current active query (like hitting ;)
     * If there are no more solutions, then just close the query
     *
     * @return whether a new solutions was found or there are no more solutions
     * @throws PrologException with the term of the error from Prolog (e.g., syntax error in query or non existence of predicates)
     */
	private final boolean fetchNextSolution() { // try to get the next solution; if none,
									// close the query;
		if (Prolog.next_solution(qid)) {
			return true;
		} else { // if failure was due to throw/1, build exception term and
					// throw it
			term_t exception_term_t = Prolog.exception(qid);
			if (exception_term_t.value != 0L) {
				Term exception_term = Term.getTerm(new HashMap<term_t, Variable>(), exception_term_t);
				close();
				throw new PrologException(exception_term);
			} else {
				close();
				return false;
			}
		}
	}

	/**
     * Returns the next solution of the query
     *
     * @return the binding representing the next solution
	 * @deprecated use nextSolution()
	 */
	@Deprecated
	public final Map<String, Term> getSolution() {
	    return nextSolution();
	}

	public final Map<String, Term> getSubstWithNameVars() {
		// oughta check: thread has query's engine
		if (!open) {
			throw new JPLException("Query is not open");
		} else if (fetchNextSolution()) {
			return get2WithNameVars();
		} else {
			return null;
		}
	}

	/**
	 * This method returns a java.util.Map, which represents a binding from the
	 * names of query variables to terms within the next solution.
	 * <p>
	 * For example, if a Query has an occurrence of a jpl.Variable, say, named
	 * "X", one can obtain the Term bound to "X" in the solution by looking up
	 * "X" in the Map.
	 *
	 * <pre>
	 * Variable x = new Variable("X");
	 * Query q = // obtain Query reference (with x in the Term array)
	 * while (q.hasMoreSolutions()) {
	 *     Map solution = q.nextSolution();
	 *     // make t the Term bound to "X" in the solution
	 *     Term t = (Term) solution.get("X");
	 *     // ...
	 * }
	 * </pre>
	 *
	 * Programmers should be careful to call this method after checking that
	 * there is indeed a solution, via method hasMoreSolutions().
	 *
	 *  @return A Map representing a substitution of the next solution.
	 *  @throws JPLException if Query is not open.
	 *  @throws NoSuchElementException if there are no more new solutions.
	 */
	public final Map<String, Term> nextSolution() {
        if (hasMoreSolutions()) {
            hasNextSolution = null; // we reset this as we moved the pointer (we now do not know if there is a new one)
            return getCurrentSolutionBindings();
        } else
            throw new NoSuchElementException("Query has already yielded all solutions");
	}

	private final Map<String, Term> getCurrentSolutionBindings() {
		if (!open) {
			throw new JPLException("Query is not open, cannot retrive solution bindings.");
		} else {
			Map<String, Term> substitution = new HashMap<String, Term>();
			// TODO: getSubsts is in Term class, should it be there? Otherwise, where else?
			Term.getSubsts(substitution, new HashMap<term_t, Variable>(), goal_.args());

			return substitution;
		}
	}

	// assumes that Query's last arg is a Variable which will be bound to
	// [Name=Var,..]
	private final Map<String, Term> get2WithNameVars() {
		if (!open) {
			throw new JPLException("Query is not open");
		} else {
			Term[] args = goal_.args(); // for slight convenience below
			Term argNV = args[args.length - 1]; // the Query's last arg
			String nameNV = ((Variable) argNV).name; // its name
			// get [Name=Var,..] from the last arg
			Map<String, Term> varnames_to_Terms1 = new HashMap<String, Term>();
			Map<term_t, Variable> vars_to_Vars1 = new HashMap<term_t, Variable>();
			args[args.length - 1].getSubst(varnames_to_Terms1, vars_to_Vars1);
			Map<String, Term> varnames_to_Terms2 = new HashMap<String, Term>();
			Term nvs = varnames_to_Terms1.get(nameNV);
			Map<term_t, Variable> vars_to_Vars2 = Util.namevarsToMap(nvs);
			for (int i = 0; i < args.length - 1; ++i) {
				args[i].getSubst(varnames_to_Terms2, vars_to_Vars2);
			}
			return varnames_to_Terms2;
		}
	}

	/**
	 * This method can be used to close an open query before its solutions are
	 * exhausted. It is called automatically when solutions are exhausted.
	 * Calling close() on an already closed Query has no effect.
	 * <p>
	 *
	 * Here is one way to get the first three solutions to a Query:
	 *
	 * <pre>
	 * Query q = new Query(predicate, args);
	 * Map&lt;String, Term&gt; sub1 = q.nextSolution();
	 * Map&lt;String, Term&gt; sub2 = q.nextSolution();
	 * Map&lt;String, Term&gt; sub3 = q.nextSolution();
	 * q.close();
	 * </pre>
	 */
	public final void close() {
		if (!open) {
			return; // it is not an error to attempt to close a closed Query
		}
		if (Prolog.thread_self() == -1) {
			throw new JPLException("no engine is attached to this thread");
		}
		if (Prolog.current_engine().value != engine.value) {
			throw new JPLException("this Query's engine is not that which is attached to this thread");
		}
		qid_t topmost = Prolog.current_query();
		if (topmost.value != this.qid.value) {
			throw new JPLException("this Query (" + this.hashCode() + ":" + this.toString() + ") is not topmost ("
					+ topmost.hashCode() + ":" + topmost.toString() + ") within its engine[" + engine.value + "]");
		}
		Prolog.close_query(qid);
		qid = null; // for tidiness
		org.jpl7.fli.Prolog.discard_foreign_frame(fid);
		fid = null; // for tidiness
		if (Prolog.current_query() == null) { // only Query open in this engine?
			if (Prolog.current_engine_is_pool()) { // this (Query's) engine is
													// from the pool?
				Prolog.release_pool_engine();
				// System.out.println("JPL releasing engine[" + engine.value +
				// "]");
			} else {
				// System.out.println("JPL leaving engine[" + engine.value +
				// "]");
			}
		} else {
			// System.out.println("JPL retaining engine[" + engine.value + "]
		}
		open = false; // this Query is now closed
		engine = null; // this Query, being closed, is no longer associated with any Prolog engine
    }

	/**
	 * calls the Query's goal to exhaustion and returns an array of zero or more
	 * Maps of zero or more variablename-to-term bindings (each Map represents a
	 * solution, in the order in which they were found).
	 *
	 * @return an array of zero or more Maps of zero or more
	 *         variablename-to-term bindings (each Map represents a solution, in
	 *         the order in which they were found) <b>NB</b> in JPL 1.0.1, this
	 *         method (inconsistently) returned null when a Query had no
	 *         solutions; in JPL 2.x onwards it returns an empty array (thus the
	 *         length of the array is, in every case, the quantity of
	 *         solutions).
	 *         <p>
	 *         <b>NB</b> in JPL 1.0.1, bindings were keyed (awkwardly) by
	 *         Variable instances; in JPL 2.x onwards they are keyed by the
	 *         (String) names of variables, which is consistent with the Term
	 *         type being just a concrete syntax for terms (and hence queries).
	 */
	public final Map<String, Term>[] allSolutions() {
		if (open) {
			throw new JPLException("Query is already open");
		} else { // get a vector of solutions, then turn it into an array
			// Vector<Map<String, Term>> v = new Vector<Map<String, Term>>();
			// while (hasMoreSolutions()) {
			// v.addElement(nextSolution());
			// }
			// @SuppressWarnings("unchecked")
			// Map<String, Term> solutions[] = (Map<String, Term>[]) new
			// HashMap[v.size()]; // 0 solutions -> Map[0]
			// v.copyInto(solutions);
			// return solutions;
			// get a List of solutions:
			List<Map<String, Term>> l = new ArrayList<Map<String, Term>>();
			while (hasNext()) {
				l.add(next());
			}
			@SuppressWarnings("unchecked")
			Map<String, Term> t[] = (Map<String, Term>[]) new HashMap[0];
			return l.toArray(t);
		}
	}

	/**
	 * This static method creates a Query whose goal is the given Term, calls it
	 * to exhaustion, and returns an array of zero or more Maps of zero or more
	 * variablename-to-term bindings (each Map represents a solution, in the
	 * order in which they were found). Throws JPLException if goal is neither a
	 * jpl.Atom nor a jpl.Compound.
	 *
	 * @return an array of zero or more Maps of zero or more
	 *         variablename-to-term bindings (each Map represents a solution, in
	 *         the order in which they were found)
	 *
	 * @param goal
	 *            the goal of this Query
	 */
	public static final Map<String, Term>[] allSolutions(Term goal) {
		return (new Query(goal)).allSolutions();
	}

	/**
	 * This static method creates a Query from the given Prolog source text
	 * fragment, calls it to exhaustion, and returns an array of zero or more
	 * Maps of zero or more variablename-to-term bindings (each Map represents a
	 * solution, in the order in which they were found). Throws PrologException
	 * containing error(syntax_error(_),_) if text is invalid.
	 *
	 * @return an array of zero or more Maps of zero or more
	 *         variablename-to-term bindings (each Map represents a solution, in
	 *         the order in which they were found)
	 *
	 * @param text
	 *            a Prolog source text fragment denoting a goal
	 */
	public static final Map<String, Term>[] allSolutions(String text) {
		return (new Query(text)).allSolutions();
	}

	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N
	 * questionmark (?) symbols and there are N accompanying Term params, this
	 * static method replaces each questionmark symbol by its respective param,
	 * calls the resulting goal to exhaustion, and returns an array of zero or
	 * more Maps of zero or more variablename-to-term bindings (each Map
	 * represents a solution, in the order in which they were found).
	 *
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args; the resulting
	 * goal is then called as above. This letter mode is redundant, deprecated
	 * (informally), and retained only for backward compatibility.
	 *
	 * @return an array of zero or more Maps of zero or more
	 *         variablename-to-term bindings (each Map represents a solution, in
	 *         the order in which they were found)
	 *
	 * @param text
	 *            the Prolog source text of a goal, in which questionmarks are
	 *            regarded as substitutible parameters
	 * @param params
	 *            terms to be substituted for the respective questionmarks in
	 *            the query text
	 */
	public static final Map<String, Term>[] allSolutions(String text, Term[] params) {
		return (new Query(text, params)).allSolutions();
	}

	/**
	 * calls the Query's goal to exhaustion or until N solutions are found,
	 * whichever is sooner, and returns an array containing (as possibly empty
	 * Maps of variablename-to-term bindings) every found solution (in the order
	 * in which they were found).
	 *
     * @param n the number of solutions to cover
	 * @return an array of Maps (possibly none), each of which is a solution (in
	 *         the order in which they were found) of the Query; at most 'n'
	 *         solutions will be found and returned. <b>NB</b> in JPL 1.0.1,
	 *         this method (inconsistently) returned null when a Query had no
	 *         solutions; in JPL 2.x onwards it returns an empty array (thus the
	 *         length of the array is, in every case, the quantity of
	 *         solutions).
	 *         <p>
	 *         <b>NB</b> in JPL 1.0.1, bindings were keyed (awkwardly) by
	 *         Variable instances; in JPL 2.x onwards they are keyed by the
	 *         (String) names of variables, which is consistent with the Term
	 *         type being just a concrete syntax for terms (and hence queries).
	 */
	public final Map<String, Term>[] nSolutions(long n) {
		if (open) {
			throw new JPLException("Query is already open");
		} else { // get a vector of solutions, then turn it into an array
			// Vector<Map<String, Term>> v = new Vector<Map<String, Term>>();
			// for (long i = 0; i++ < n && hasMoreSolutions();) {
			// v.addElement(nextSolution());
			// }
			// @SuppressWarnings("unchecked")
			// Map<String, Term> solutions[] = (Map<String, Term>[]) new
			// Map[v.size()]; // 0 solutions -> Map[0]
			// v.copyInto(solutions);
			// return solutions;
			List<Map<String, Term>> l = new ArrayList<Map<String, Term>>();
			for (long i = 0; i++ < n && hasMoreSolutions();) {
				l.add(next());
			}
			@SuppressWarnings("unchecked")
			Map<String, Term> t[] = (Map<String, Term>[]) new HashMap[0];
			return l.toArray(t);
		}
	}

	/**
	 * This static method creates a Query whose goal is the given Term, calls it
	 * to exhaustion or until N solutions are found, whichever is sooner, and
	 * returns an array containing (as possibly empty Maps of
	 * variablename-to-term bindings) every found solution (in the order in
	 * which they were found). Throws JPLException if goal is neither a jpl.Atom
	 * nor a jpl.Compound.
	 *
	 * @param goal
	 *            the goal of this Query
     * @param n the number of solutions to cover
     * @return an array of up to the the first n binding solutions
	 */
	public static final Map<String, Term>[] nSolutions(Term goal, long n) {
		return (new Query(goal)).nSolutions(n);
	}

	/**
	 * This static method creates a Query from the given Prolog source text
	 * fragment, calls it to exhaustion or until N solutions are found,
	 * whichever is sooner, and returns an array containing (as possibly empty
	 * Maps of variablename-to-term bindings) every found solution (in the order
	 * in which they were found). Throws PrologException containing
	 * error(syntax_error(_),_) if text is invalid.
	 *
	 * @param text
	 *            a Prolog source text fragment denoting a goal
     * @param n the number of solutions to cover
     * @return an array of up to the the first n binding solutions
	 */
	public static final Map<String, Term>[] nSolutions(String text, long n) {
		return (new Query(text)).nSolutions(n);
	}

	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N
	 * questionmark (?) symbols and there are N accompanying params, this static
	 * method replaces each questionmark symbol by its respective param, calls
	 * the resulting goal to exhaustion or until N solutions are found,
	 * whichever is sooner, and returns an array containing (as possibly empty
	 * Maps of variablename-to-term bindings) every found solution (in the order
	 * in which they were found).
	 *
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args; the resulting
	 * goal is then called as above. This latter mode is redundant, deprecated
	 * (informally), and retained only for backward compatibility.
	 *
	 * @param text
	 *            the Prolog source text of a goal, in which questionmarks are
	 *            regarded as substitutible parameters
	 * @param params
	 *            terms to be substituted for the respective questionmarks in
	 *            the query text
     * @param n the number of solutions to cover
     * @return an array of up to the the first n binding solutions
	 */
	public static final Map<String, Term>[] nSolutions(String text, Term[] params, long n) {
		return (new Query(text, params)).nSolutions(n);
	}

	/**
	 * Returns the first solution, if any, as a (possibly empty) Map of
	 * variablename-to-term bindings, else null.
	 *
	 * This method will throw a JPLException if this Query is already open (and
	 * the Query will remain open as before). Otherwise, upon return, the Query
	 * will be closed.
	 *
	 * @return the first solution, if the query has one, as a (possibly empty)
	 *         Map. If the return value is null, this means that the Query has
	 *         no solutions.
	 */
	public final Map<String, Term> oneSolution() {
		if (open) {
			throw new JPLException("Query is already open");
		} else {
			Map<String, Term> solution;
			if (hasMoreSolutions()) {
				solution = nextSolution();
				close(); // safe, whether or not this is the only solution
			} else {
				solution = null;
			}
			return solution;
		}
	}

	/**
	 * This static method creates a Query (whose goal is the specified Term) and
	 * calls it at most once, returning the first solution, if there is one, as
	 * a (possibly empty) Map, else null. The goal can be a jpl.Atom or a
	 * jpl.Compound, but cannot be an instance of jpl.Float, jpl.Integer or
	 * jpl.Variable.
	 *
	 * @param goal
	 *            the goal of this Query
	 * @return binding of the first solution
	 */
	public static final Map<String, Term> oneSolution(Term goal) {
		return (new Query(goal)).oneSolution();
	}

	/**
	 * This static method creates a Query from the given Prolog source text
	 * fragment, and calls it at most once, returning the first solution, if
	 * there is one, as a (possibly empty) Map, else null. Throws
	 * PrologException containing error(syntax_error(_),_) if text is invalid.
	 *
	 * @param text
	 *            a Prolog source text fragment denoting a goal
	 * @return binding of the first solution
	 */
	public static final Map<String, Term> oneSolution(String text) {
		return (new Query(text)).oneSolution();
	}

	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N
	 * questionmark (?) symbols and there are N params, each questionmark symbol
	 * is replaced by its respective param to provide the goal of this query:
	 * the resulting goal is then called (at most once) and the first solution,
	 * if there is one, is returned as a (possibly empty) Map, else null.
	 *
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args; the resulting
	 * goal is then called as above. This latter mode is redundant, deprecated
	 * (informally), and retained only for backward compatibility.
	 *
	 * @param text
	 *            the Prolog source text of a goal, in which questionmarks are
	 *            regarded as substitutible parameters
	 * @param params
	 *            terms to be substituted for the respective questionmarks in
	 *            the query text
	 * @return binding of the first solution
	 */
	public static final Map<String, Term> oneSolution(String text, Term[] params) {
		return (new Query(text, params)).oneSolution();
	}

	/**
	 * This method will attempt to call this Query's goal within an available
	 * Prolog engine.
	 *
	 * @return the provability of the Query, i.e. 'true' if it has at least one
	 *         solution, 'false' if the call fails without finding a solution.
	 *         <p>
	 *
	 *         Only the first solution (if there is one) will be found; any
	 *         bindings will be discarded, and the Query will be closed.
	 *         <p>
	 *         This method will throw a JPLException if this Query is already
	 *         open.
	 */
	public final boolean hasSolution() {
		return oneSolution() != null;
	}

	/**
	 * This static method creates a Query (whose goal is the specified Term) and
	 * calls it at most once, returning true if a solution was found, else
	 * false. The goal can be a jpl.Atom or a jpl.Compound, but cannot be an
	 * instance of jpl.Float, jpl.Integer or jpl.Variable.
	 *
	 * @param goal
	 *            the goal of this Query
	 * @return true iff the query can be proved
	 */
	public static final boolean hasSolution(Term goal) {
		return (new Query(goal)).hasSolution();
	}

	/**
	 * This static method creates a Query from the given Prolog source text and
	 * calls it at most once, returning true if a solution was found, else
	 * false. Throws PrologException containing error(syntax_error(_),_) if text
	 * is invalid.
	 *
	 * @param text
	 *            the goal of this Query, as Prolog source text
	 * @return true iff the query can be proved
	 */
	public static final boolean hasSolution(String text) {
		return (new Query(text)).hasSolution();
	}

	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N
	 * questionmark (?) symbols and there are N params, each questionmark symbol
	 * is replaced by its corresponding arg to provide the new Query's goal: the
	 * resulting Query is called as described above.
	 *
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of its goal and args are its args; it then calls
	 * this goal (at most once) and returns true if a solution was found, else
	 * false. This latter mode is redundant, deprecated (informally), and
	 * retained only for backward compatibility.
	 *
	 * @param text
	 *            the Prolog source text of a goal, in which questionmarks are
	 *            regarded as substitutible parameters
	 * @param params
	 *            terms to be substituted for the respective questionmarks in
	 *            the query text
	 * @return true iff the query can be proved
	 */
	public static final boolean hasSolution(String text, Term[] params) {
		return (new Query(text, params)).hasSolution();
	}

	/**
	 * Returns a crude String representation of a Query.
	 *
	 * @return a crude String representation of a Query
	 */
	public String toString() {
		return goal_.name() + "( " + Term.toString(goal_.args()) + " )";
	}
}
