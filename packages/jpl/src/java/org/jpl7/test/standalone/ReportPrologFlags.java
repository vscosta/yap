package org.jpl7.test.standalone;

import org.jpl7.Atom;
import org.jpl7.JPLException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;

public class ReportPrologFlags {
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
    public void ReportPrologFlags() {
        StringBuffer sb = new StringBuffer();
        Query q = new Query("current_prolog_flag(X, Y)");
        q.open();
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            System.out.println(String.format("Value of %s: %s",   sol.get("X").toString(), sol.get(("Y").toString())));
        }
        q.close();
    }

}
