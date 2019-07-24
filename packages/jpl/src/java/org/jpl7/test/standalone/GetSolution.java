package org.jpl7.test.standalone;

import org.jpl7.*;

import java.util.Map;
import java.util.NoSuchElementException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GetSolution {
//    final Logger logger = LoggerFactory.getLogger(GetSolution.class);

	public static void main(String argv[]) {
    }

    @Before
    public void setUp() {
        // JPL.setTraditional();
        //
//		Query.hasSolution("use_module(library(jpl))"); // only because we call e.g. jpl_pl_syntax/1 below
//		Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
//		System.out.println("swipl.version = " + swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));
//		System.out.println("swipl.syntax = " + Query.oneSolution("jpl_pl_syntax(Syntax)").get("Syntax"));
//		System.out.println("swipl.home = " + Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
//		System.out.println("jpl.jar = " + JPL.version_string());
//		System.out.println("jpl.dll = " + org.jpl7.fli.Prolog.get_c_lib_version());
//		System.out.println("jpl.pl = " + Query.oneSolution("jpl_pl_lib_version(V)").get("V").name());
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
//            logger.info("{} being run...", description.getMethodName());

            System.out.println("Starting test: " + description.getMethodName());
        }
    };

    @Test
    public void testGetSolution1() {
        Query q = new Query("fail");
        q.open();
        if (q.hasMoreSolutions()) q.nextSolution();
        assertTrue("A query has exhausted all solutions but it is still open", !q.isOpen());
    }

    @Test
    public void testGetSolution2() {
        Query q = new Query("fail"); // this query has no solutions
        q.open(); // this opens the query
        try {
            q.nextSolution(); // this call is invalid, as the query is closed

            // shouldn't get to here
            fail("jpl.Query#nextSolution() should have thrown JPLException");
        } catch (NoSuchElementException e) {
            // all good, right exception threw
        } catch (Exception e) {
            fail("jpl.Query#nextSolution() threw wrong class of exception: " + e);
        }
    }


    @Test
    public void testOpenGetClose2() {
        Query q = new Query("dummy"); // we're not going to open this...
        try {
            q.nextSolution(); // should throw exception (query not open)
            fail("nextSolution() succeeds on unopened Query"); // shouldn't get
            // to here
        } catch (JPLException e) { // expected exception class
            if (e.getMessage().contains("existence_error")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("jpl.Query#nextSolution() threw wrong JPLException: " + e);
            }
        } catch (Exception e) {
            fail("jpl.Query#nextSolution() threw wrong exception class: " + e);
        }
    }

    @Test
    public void testOpenGetClose1() {
        StringBuffer sb = new StringBuffer();
        Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
        Map<String, Term> soln;
        q.open();
        while (q.hasMoreSolutions()) {
            sb.append(((Atom) q.nextSolution().get("C")).name());
        }
        q.close();
        assertEquals("prolog", sb.toString());
    }

    @Test
    public void testStackedQueries1() {
        StringBuffer sb = new StringBuffer();
        Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
        Map<String, Term> soln;
        q.open();
        while (q.hasMoreSolutions()) {
            soln = q.nextSolution();
            Atom a = (Atom) soln.get("C");
            if (Query.hasSolution("memberchk(?, [l,o,r])", new Term[] { a })) {
                // this query opens and closes while an earlier query is still open
                sb.append(((Atom) soln.get("C")).name());
            }
        }
        assertTrue(!q.isOpen()); // q will have been closed by solution exhaustion
        assertEquals("rolo", sb.toString());
    }


    @Test
    public void iterativeSol1() {
        final int[] expectedSolutions = { 1, 2, 3, 4, 5};

        Query q = new Query("between(1,5,N)");
        Map<String,Term> sol;
        int solutionIndex;

        solutionIndex = 0;
        while (q.hasMoreSolutions()) {
            sol = q.nextSolution();

            assertEquals(expectedSolutions[solutionIndex], sol.get("N").intValue());
            solutionIndex++;
        }

        assertFalse("There should not be more solutions for the query", q.hasSolution());
        assertFalse("Query should be closed due to sol exhaustion", q.isOpen());

        q.reset();
        assertFalse("Query is still close after reset", q.isOpen());
        assertTrue("Query again should have solutions after reset", q.hasSolution());

        solutionIndex = 0;
        while (q.hasMoreSolutions()) {
            sol = q.nextSolution();

            assertEquals(expectedSolutions[solutionIndex], sol.get("N").intValue());
            solutionIndex++;
        }
        assertFalse("There should not be more solutions for the query", q.hasSolution());
        assertFalse("Query should be closed due to sol exhaustion", q.isOpen());

        try {
            sol = q.next();
            fail("Query has been exhausted, there should be no solutions left");

        } catch (NoSuchElementException e) {
            // all good, this is what we expect as there are no more solutions
        } catch (Exception e) {
            fail("Wrong exception type: "  + e);
        }

    }

}
