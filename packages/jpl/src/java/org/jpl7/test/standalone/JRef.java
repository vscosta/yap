package org.jpl7.test.standalone;

import org.jpl7.*;
import org.jpl7.Integer;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;


public class JRef {
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
    public void testJRef1() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", new Term[] { JPL.newJRef(I) });
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef2() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", JPL.newJRef(I));
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef3() {
        StringBuffer sb = new StringBuffer();
        Query.oneSolution("jpl_call(?,append,['xyz'],_)", new Term[] { JPL.newJRef(sb) });
        assertTrue(sb.toString().equals("xyz"));
    }

    @Test
    public void testJRef4() {
        Term jrefSB = Query.oneSolution("jpl_new('java.lang.StringBuffer',['abc'],SB)").get("SB");
        assertTrue(jrefSB.isJRef() && ((StringBuffer) jrefSB.object()).toString().equals("abc"));
    }

    @Test
    public void testJRef5() {
        String token = "foobar345";
        Term a = Query.oneSolution("jpl_new('java.lang.StringBuffer',[?],A)", new Term[] { new Atom(token) }).get("A");
        assertTrue(((java.lang.StringBuffer) (a.object())).toString().equals(token));
    }



    @Test
    public void testRef6() {
        Term nullJRef = JPL.newJRef(null);
        Object nullObject = nullJRef.object();
        assertNull("JPL null Term yields a null object", nullObject);
    }


    @Test
    public void testRef7() {
        Term badJRef = new Compound("hello", new Term[] { new Atom("foobar") }); // term hello(foobar)
        try {
            badJRef.object(); // should throw exception
            fail("@(foobar).object() should thrown JPLException"); // shouldn't get to here
        } catch (JPLException e) { // expected exception class
            if (e.getMessage().endsWith("term is not a JRef")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("hello(foobar).object() threw wrong JPLException: " + e);
            }
        } catch (Exception e) {
            fail("hello(foobar).object() threw wrong exception class: " + e);
        }
    }


}
