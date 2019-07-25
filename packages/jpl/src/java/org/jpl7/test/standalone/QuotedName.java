package org.jpl7.test.standalone;

import org.jpl7.*;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;

public class QuotedName {
//    final Logger logger = LoggerFactory.getLogger(GetSolution.class);

	public static void main(String argv[]) {
    }

    @Before
    public void setUp() {
	    assertTrue("quotd_name.pl should loda well", Query.hasSolution("consult('src/java/org/jpl7/test/standalone/quoted_name.pl')"));
        System.out.println("\t quoted_name.pl loaded.");
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
//            logger.info("{} being run...", description.getMethodName());

            System.out.println("Starting test: " + description.getMethodName());
        }
    };

    @Test
    public void quotedName1() {
        String text_t;
        String name;
        Term t;

//        name = "org.jpl7.PrologException: PrologException: error(existence_error(procedure, '/'(pepe, 1)), context(':'(system, '/'('$c_call_prolog', 0)), _0))";
        name = "existence_error(procedure, '/'(pepe, 1))";
//        name = "'/'(pepe, 1)";
        name = "'$c_call_prolog'";
        t = Util.textToTerm(name);
        assertEquals("matching text-term", name, t.toString());
    }

    @Test
    public void errorTermTranslation1() {
        String text_t;
        String name;
        Term t;

        t = new Compound("error", new Term[] {
                new Atom("existence_error"),
                new Atom("context")});

        try {
            boolean x = Query.hasSolution(t);
        } catch (PrologException e) {
            assertTrue("PrologException has to be existence error", e.getMessage().contains("existence_error"));
        } catch (Exception e) {
            fail("Wrong exception type; should be PrologException but it is: " + e);
        }
    }


    private Term quotedQuery(String name) {
        return new Compound(":", new Term[]{
                new Atom("jpl"),
                new Compound("quoted_name", new Term[]{
                        new Atom(name),
                        new Variable("S")
                })
        });

    }

    @Test
    public void quotedName2() {
        String text_t;
        String name;
        Term t;
        Map<String, Term> sol;

        final String[] tests = { "hello", "hello(world)", "[1,2,3]" };
        final String[] expectedSolutions = { "hello", "\'hello(world)\'", "'[1,2,3]'" };


        int l = tests.length;
        for (int solutionIndex = 0; solutionIndex < l; solutionIndex++) {
//            t = quotedQuery(tests[solutionIndex]);
            sol = new Query(String.format("quoted_name(%s,S)", tests[solutionIndex])).oneSolution();

            System.out.println("\t The solution for S is: " + sol.get("S").toString());
            assertEquals(expectedSolutions[solutionIndex], sol.get("S").toString());
        }



//        String name2 = ((Atom) sol.get("S")).name();

//        t = Util.textToTerm(String.format("jpl:(quoted_name(%s, S))", name));

    }
}
