package org.jpl7.test;

import java.math.BigInteger;
import java.util.Map;
import java.util.NoSuchElementException;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Integer;
import org.jpl7.JPL;
import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;
import org.jpl7.Variable;
import org.jpl7.fli.Prolog;

// This class defines all the tests which are run from Java.
// It needs junit.framework.TestCase and junit.framework.TestSuite, which are not supplied with JPL.
public class TestJUnit extends TestCase {
	public static final String startup = (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
			: System.getenv("SWIPL_BOOT_FILE"));
	public static final String test_jpl = (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
			: System.getenv("TEST_JPL"));
	public static final String syntax = (System.getenv("SWIPL_SYNTAX") == null ? "modern"
			: System.getenv("SWIPL_SYNTAX"));
	public static final String home = (System.getenv("SWI_HOME_DIR") == null ? "../.."
			: System.getenv("SWI_HOME_DIR"));

	public TestJUnit(String name) { // called for each public void test*()
									// method
		super(name);
	}

	public static junit.framework.Test suite() {
		if (syntax.equals("traditional")) {
			JPL.setTraditional();
			Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
				"libswipl.dll", "-f", "none",
				"-g", "true", "--traditional", "-q",
				"--home="+home, "--nosignals" });
		} else {
			Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
					"libswipl.dll", "-f", "none",
				"-g", "true", "-q",
				"--home="+home, "--nosignals" });
		}
		assertTrue((new Query("consult", new Term[] { new Atom(test_jpl) })).hasSolution());
		assertTrue((new Query("use_module(library(jpl))")).hasSolution());
		return new TestSuite(TestJUnit.class);
	}

	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	protected void setUp() {
	}

	protected void tearDown() {
		// cleanup code
	}

	// supporting code:

	public static long fac(long n) { // complements
										// jpl:jpl_test_fac(+integer,-integer);
										// indirectly supports
										// testMutualRecursion
		if (n == 1) {
			return 1;
		} else if (n > 1) {
			return n * ((org.jpl7.Integer) Query
					.oneSolution("jpl_test_fac(?,F)", new Term[] { new org.jpl7.Integer(n - 1) }).get("F")).longValue();
		} else {
			return 0;
		}
	}

	// the tests; all public void test*()

//	public void testInfo() {
//		Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
//		System.out.println("version = " + swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));
//		System.out.println("syntax = " + Query.oneSolution("jpl:jpl_pl_syntax(Syntax)").get("Syntax"));
//		System.out.println("jpl.jar = " + JPL.version_string() + " " + JPL.jarPath());
//		System.out.println("jpl.dll = " + Prolog.get_c_lib_version());
//		System.out.println("jpl.pl = " + Query.oneSolution("jpl:jpl_pl_lib_version(V)").get("V").name() + " "
//				+ Query.oneSolution("module_property(jpl, file(F))").get("F").name());
//		System.out.println("home = " + Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
//	}

	public void testEmptyParentheses() {
		Term t = Query.oneSolution("T = a()").get("T"); // valid in both
														// traditional and
														// modern syntax in SWI
														// Prolog 7.x
		assertTrue("T is not bound to an atom", t.isAtom());
		assertTrue("the atom's name is not \"a\"", t.name().equals("a"));
	}

	public void testIntegerFromByte1() {
		byte b = (byte) 127; // -128..127
		Integer i = new Integer(b);
		assertTrue(i.intValue() == b);
	}

	public void testIntegerFromChar1() {
		char c = (char) 64; // 0..65535
		// System.out.println("c = " + c);
		Integer i = new Integer(c);
		assertTrue(i.intValue() == c);
	}

	public void testInteger1() {
		try {
			Term i = Query.oneSolution("I is 2**40").get("I"); // long but not
																// int
			i.intValue();
			fail("intValue() of bigger-than-int value failed to throw an exception");
		} catch (JPLException e) {
			if (e.getMessage().endsWith("cannot represent value as an int")) {
				// OK: an appropriate exception was thrown
			} else {
				fail("intValue() of bigger-than-int value threw incorrect JPLException: " + e);
			}
		} catch (Exception e) {
			fail("intValue() of bigger-than-int value threw unexpected class of exception: " + e);
		}
	}

	public void testIterable1() {
		// System.out.println("iterating over array of solutions");
		// for (Map<String, Term> m : Query.allSolutions("current_module(M)")) {
		// // iterating over array of solutions
		// System.out.println(m.get("M"));
		// }
		// System.out.println();
	}

	public void testIterable2() {
		// System.out.println("iterating over successively fetched solutions");
		// for (Map<String, Term> m : new Query("current_module(M)")) { //
		// iterating over successively fetched solutions
		// System.out.println(m.get("M"));
		// }
		// System.out.println();
	}

	public void testBigInteger1() {
		BigInteger a = new BigInteger(Long.toString(51L));
		BigInteger b = a.pow(51); // 51**51, too big for a long
		Term x = Query.oneSolution("X is 51**51").get("X");
		assertTrue("X is an org.jpl7.Integer", x.isInteger());
		// System.out.println("X.bigValue() = " + x.bigValue().toString());
		// System.out.println("b.bigValue() = " + b.toString());
		assertTrue("X is a big integer", x.isBigInteger());
		assertTrue("X's big value is 51**51", x.bigValue().equals(b));
	}

	public void testBigInteger2() {
		BigInteger b = new BigInteger("12345678901234567890123456789");
		Term i = new Integer(b); // too big for a long
		Term g = new Compound("is", new Term[] { new Variable("X"), i });
		Term x = Query.oneSolution(g).get("X");
		assertTrue("X is an org.jpl7.Integer", x.isInteger());
		assertTrue("X is a big org.jpl7.Integer", x.isBigInteger());
		assertTrue("X's value is as expected", x.bigValue().equals(b));
	}

	public void testCompoundZeroArity1() {
		Term t = new Compound("foo", new Term[] {});
		assertTrue(t.isCompound());
		assertFalse(t.isAtom());
		assertTrue(t.name().equals("foo"));
		assertTrue(t.arity() == 0);
	}

	public void testCompoundZeroArity2() {
		Term t = Query.oneSolution("T = foo()").get("T");
		// System.out.println("type = " + t.typeName());
		assertTrue(t.name().equals("foo"));
		assertTrue(t.arity() == 0);
	}

	// public void testCompoundZeroArity3() {
	// Term t = Query.oneSolution("T = foo()").get("T");
	// assertTrue("term is a compound", t.isCompound());
	// assertFalse("term is an atom", t.isAtom());
	// }

	public void testMap1() {
		Map<String, Term> h = Query.oneSolution("p(a,b) = p(X,Y)");
		assertTrue(h.get("X").name().equals("a"));
		assertTrue(h.get("Y").name().equals("b"));
	}

	public void testMap2() {
		Map<String, Term>[] hs = Query.allSolutions("p(a,b) = p(X,Y)");
		assertTrue(hs.length == 1);
		assertTrue(hs[0].get("X").name().equals("a"));
		assertTrue(hs[0].get("Y").name().equals("b"));
	}

	public void testSyntaxSet1() {
		if (syntax.equals("traditional")) {
			try {
				JPL.setTraditional(); // should succeed silently
			} catch (Exception e) {
				fail("setTraditional() under traditional syntax threw exception: " + e);
			}
		} else {
			try {
				JPL.setTraditional();
			} catch (JPLException e) { // expected exception class, but is it
										// correct in detail?
				if (e.getMessage().endsWith("traditional syntax after Prolog is initialised")) {
					// OK: an appropriate exception was thrown
				} else {
					fail("setTraditional() under modern syntax threw incorrect JPLException: " + e);
				}
			} catch (Exception e) {
				fail("setTraditional() under modern syntax threw unexpected class of exception: " + e);
			}
		}
	}

	public void testMasstest() {
		assertTrue((new Query("assert(diagnose_declaration(_,_,_,[not,a,real,error]))")).hasSolution());
	}

	public void testSameLibVersions1() {
		String java_lib_version = JPL.version_string();
		String c_lib_version = Prolog.get_c_lib_version();
		assertTrue("java_lib_version(" + java_lib_version + ") is same as c_lib_version(" + c_lib_version + ")",
				java_lib_version.equals(c_lib_version));
	}

	public void testSameLibVersions2() {
		String java_lib_version = JPL.version_string();
		String pl_lib_version = Query.oneSolution("jpl_pl_lib_version(V)").get("V").name();
		assertTrue("java_lib_version(" + java_lib_version + ") is same as pl_lib_version(" + pl_lib_version + ")",
				java_lib_version.equals(pl_lib_version));
	}

	public void testAtomName1() {
		String name = "fred";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}

	public void testAtomName2() {
		String name = "ha ha";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}

	public void testAtomName3() {
		String name = "3";
		Atom a = new Atom(name);
		assertEquals("an Atom's name is that with which it was created", a.name(), name);
	}

	public void testAtomToString1() {
		String name = "fred";
		String toString = "fred";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}

	public void testAtomToString2() {
		String name = "ha ha";
		String toString = "'ha ha'";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}

	public void testAtomToString3() {
		String name = "3";
		String toString = "'3'";
		Atom a = new Atom(name);
		assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
	}

	public void testAtomArity() {
		Atom a = new Atom("willy");
		assertEquals("an Atom has arity zero", a.arity(), 0);
	}

	public void testAtomEquality1() {
		String name = "fred";
		Atom a1 = new Atom(name);
		Atom a2 = new Atom(name);
		assertEquals("two Atoms created with the same name are equal", a1, a2);
	}

	public void testAtomIdentity() { // how could this fail?!
		String name = "fred";
		Atom a1 = new Atom(name);
		Atom a2 = new Atom(name);
		assertNotSame("two Atoms created with the same name are not identical", a1, a2);
	}

	public void testAtomHasFunctorNameZero() {
		String name = "sam";
		Atom a = new Atom(name);
		assertTrue("a text atom has a functor whose name is the name of the atom, and whose arity is zero",
				a.hasFunctor(name, 0));
	}

	public void testAtomHasFunctorWrongName() {
		assertFalse("an Atom does not have a functor whose name is other than that with which the Atom was created",
				new Atom("wally").hasFunctor("poo", 0));
	}

	public void testAtomHasFunctorWrongArity() {
		String name = "ted";
		assertFalse("an Atom does not have a functor whose arity is other than zero",
				new Atom(name).hasFunctor(name, 1));
	}

	public void testVariableBinding1() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("Y") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		Map<String, Term> soln = new Query(goal).oneSolution();
		assertTrue("two Variables with different names can bind to distinct atoms",
				soln != null && (soln.get("X")).name().equals("a") && (soln.get("Y")).name().equals("b"));
	}

	public void testVariableBinding2() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("X") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertFalse("two distinct Variables with same name cannot unify with distinct atoms",
				new Query(goal).hasSolution());
	}

	public void testVariableBinding3() {
		Variable X = new Variable("X");
		Term lhs = new Compound("p", new Term[] { X, X });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertFalse("two references to the same (named) Variable cannot unify with differing atoms",
				new Query(goal).hasSolution());
	}

	public void testVariableBinding4() {
		Term lhs = new Compound("p", new Term[] { new Variable("_"), new Variable("_") });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertTrue("two distinct anonymous Variables can unify with distinct atoms", new Query(goal).hasSolution());
	}

	public void testVariableBinding5() {
		Variable Anon = new Variable("_");
		Term lhs = new Compound("p", new Term[] { Anon, Anon });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b") });
		Term goal = new Compound("=", new Term[] { lhs, rhs });
		assertTrue("two references to an anonymous Variable can unify with differing atoms",
				new Query(goal).hasSolution());
	}

	public void testAtomEquality2() {
		Atom a = new Atom("a");
		assertTrue("two references to an Atom are equal by .equals()", a.equals(a));
	}

	public void testAtomEquality3() {
		assertTrue("two distinct, same-named Atoms are equal by .equals()", (new Atom("a")).equals(new Atom("a")));
	}

	public void testTextToTerm1() {
		String text = "fred(B,p(A))";
		Term t = Util.textToTerm(text);
		assertTrue("Util.textToTerm() converts \"fred(B,p(A))\" to a corresponding Term",
				t.hasFunctor("fred", 2) && t.arg(1).isVariable() && t.arg(1).name().equals("B")
						&& t.arg(2).hasFunctor("p", 1) && t.arg(2).arg(1).isVariable()
						&& t.arg(2).arg(1).name().equals("A"));
	}

	public void testArrayToList1() {
		Term l2 = Util.termArrayToList(
				new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e") });
		Query q9 = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2 }));
		assertTrue("append(Xs,Ys,[a,b,c,d,e]) has 6 solutions", q9.allSolutions().length == 6);
	}

	public void testArrayToList2() {
		String goal = "append(Xs,Ys,[a,b,c,d,e])";
		assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
	}

	public void testLength1() {
		Query q5 = new Query(new Compound("length", new Term[] { new Variable("Zs"), new org.jpl7.Integer(2) }));
		Term zs = q5.oneSolution().get("Zs");
		assertTrue("length(Zs,2) binds Zs to a list of two distinct variables " + zs.toString(),
				zs.isListPair() && zs.arg(1).isVariable() && zs.arg(2).isListPair() && zs.arg(2).arg(1).isVariable()
						&& zs.arg(2).arg(2).isListNil() && !zs.arg(1).name().equals(zs.arg(2).arg(1).name()));
	}

	public void testListNil1() {
		Term x = Query.oneSolution("X = []").get("X");
		if (syntax.equals("traditional")) {
			assertTrue("empty list is text atom []",
					x.isAtom() && x.atomType().equals("text") && x.name().equals("[]"));
		} else {
			assertTrue("empty list is reserved atom []",
					x.isAtom() && x.atomType().equals("reserved_symbol") && x.name().equals("[]"));
		}
	}

	public void testListCons1() {
		Term x = Query.oneSolution("X = [a]").get("X");
		if (syntax.equals("traditional")) {
			assertTrue("list constructor is ./2", x.isCompound() && x.name().equals("."));
		} else {
			assertTrue("list constructor is [|]/2", x.isCompound() && x.name().equals("[|]"));
		}
	}

	public void testGenerate1() { // we chickened out of verifying each solution
									// :-)
		String goal = "append(Xs,Ys,[_,_,_,_,_])";
		assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
	}

	public void testPrologException1() {
		try {
			new Query("p(]"); // writes junk to stderr and enters debugger
								// unless flag debug_on_error = false
		} catch (PrologException e) {
			assertTrue("new Query(\"p(]\") throws a PrologException " + e.toString(), true);
			return;
		}
		fail("new Query(\"p(]\") oughta throw a PrologException");
	}

	public void testAtom1() {
		assertTrue("new Atom(\"3 3\")" + (new Atom("3 3")).toString(), true);
	}

	public void testTextToTerm2() {
		String text1 = "fred(?,2,?)";
		String text2 = "[first(x,y),A]";
		Term plist = Util.textToTerm(text2);
		Term[] ps = plist.toTermArray();
		Term t = Util.textToTerm(text1).putParams(ps);
		assertTrue("fred(?,2,?) .putParams( [first(x,y),A] )",
				t.hasFunctor("fred", 3) && t.arg(1).hasFunctor("first", 2) && t.arg(1).arg(1).hasFunctor("x", 0)
						&& t.arg(1).arg(2).hasFunctor("y", 0) && t.arg(2).hasFunctor(2, 0) && t.arg(3).isVariable()
						&& t.arg(3).name().equals("A"));
	}

	public void testDontTellMeMode1() {
		final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
		JPL.setDTMMode(true);
		assertTrue(
				"in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for just one variable",
				q.oneSolution().keySet().size() == 1);
	}

	public void testDontTellMeMode2() {
		final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
		JPL.setDTMMode(false);
		assertTrue(
				"not in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for three variables",
				q.oneSolution().keySet().size() == 3);
	}

	public void testModulePrefix1() {
		assertTrue(Query.hasSolution("call(user:true)"));
	}

	private void testMutualRecursion(int n, long f) { // f is the expected
														// result for fac(n)
		try {
			assertEquals("mutual recursive Java<->Prolog factorial: fac(" + n + ") = " + f, fac(n), f);
		} catch (Exception e) {
			fail("fac(" + n + ") threw " + e);
		}
	}

	public void testMutualRecursion1() {
		testMutualRecursion(1, 1);
	}

	public void testMutualRecursion2() {
		testMutualRecursion(2, 2);
	}

	public void testMutualRecursion3() {
		testMutualRecursion(3, 6);
	}

	public void testMutualRecursion10() {
		testMutualRecursion(10, 3628800);
	}

	public void testIsJNull1() {
		Term atNull = new Compound("@", new Term[] { new Atom("null") });
		Query q = new Query("=", new Term[] { new Variable("X"), atNull });
		assertTrue(q.oneSolution().get("X").isJNull());
	}

	public void testIsJNull2() {
		Term t = Query.oneSolution("X = @(3)").get("X");
		assertFalse("@(3) . isJNull() fails", t.isJNull());
	}

	public void testIsJNull3() {
		Term t = Query.oneSolution("X = _").get("X");
		assertFalse("_ . isJNull() fails", t.isJNull());
	}

	public void testIsJNull4() {
		Term t = Query.oneSolution("X = @(true)").get("X");
		assertFalse("@(true) . isJNull() fails", t.isJNull());
	}

	public void testIsJNull5() {
		Term t = Query.oneSolution("X = @(false)").get("X");
		assertFalse("@(false) . isJNull() fails", t.isJNull());
	}

	public void testIsJTrue1() {
		Term t = Query.oneSolution("X = @(true)").get("X");
		assertTrue("@(true) . isJTrue() succeeds", t.isJTrue());
	}

	public void testIsJTrue2() {
		Term t = Query.oneSolution("X = @(3)").get("X");
		assertFalse("@(3) . isJTrue() fails", t.isJTrue());
	}

	public void testIsJTrue3() {
		Term t = Query.oneSolution("X = _").get("X");
		assertFalse("_ . isJTrue() fails", t.isJTrue());
	}

	public void testIsJTrue4() {
		Term t = Query.oneSolution("X = @(false)").get("X");
		assertFalse("@(false) . isJTrue() fails", t.isJTrue());
	}

	public void testIsJVoid1() {
		Term t = Query.oneSolution("X = @(void)").get("X");
		assertTrue("@(void) . isJVoid() succeeds", t.isJVoid());
	}

	public void testIsJVoid2() {
		Term t = Query.oneSolution("X = @(3)").get("X");
		assertFalse("@(3) . isJVoid() fails", t.isJVoid());
	}

	public void testIsJVoid3() {
		Term t = Query.oneSolution("X = _").get("X");
		assertFalse("_ . isJVoid() fails", t.isJVoid());
	}

	public void testTypeName1() {
		assertEquals("Y = foo binds Y to an Atom", Query.oneSolution("Y = foo").get("Y").typeName(), "Atom");
	}

	public void testTypeName2() {
		assertEquals("Y = 3.14159 binds Y to a Float", Query.oneSolution("Y = 3.14159").get("Y").typeName(), "Float");
	}

	public void testTypeName4() {
		assertEquals("Y = 6 binds Y to an Integer", Query.oneSolution("Y = 6").get("Y").typeName(), "Integer");
	}

	public void testTypeName5() {
		assertEquals("Y = _ binds Y to a Variable", Query.oneSolution("Y = _").get("Y").typeName(), "Variable");
	}

	public void testTypeName3() {
		assertEquals("Y = f(x) binds Y to a Compound", Query.oneSolution("Y = f(x)").get("Y").typeName(), "Compound");
	}

	public void testGoalWithModulePrefix1() {
		String goal = "jpl:jpl_modifier_bit(volatile,I)";
		assertTrue(goal + " binds I to an integer", Query.oneSolution(goal).get("I").isInteger());
	}

	public void testGoalWithModulePrefix2() {
		String goal = "user:length([],0)";
		assertTrue(goal + " succeeds", Query.hasSolution(goal));
	}

	public void testGoalWithModulePrefix3() {
		String goal = "3:length([],0)";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (numeric module prefix) didn't throw exception"); // shouldn't
																			// get
																			// to
																			// here
		} catch (PrologException e) { // expected exception class
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2)
					&& e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (numeric module prefix) threw incorrect PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (numeric module prefix) threw wrong class of exception: " + e);
		}
	}

	public void testGoalWithModulePrefix4() {
		String goal = "_:length([],0)";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (unbound module prefix) wrongly succeeded"); // shouldn't
																		// get
																		// to
																		// here
		} catch (PrologException e) { // expected exception class
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("instantiation_error", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (unbound module prefix) threw wrong PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (unbound module prefix) threw wrong exception class: " + e);
		}
	}

	public void testGoalWithModulePrefix5() {
		String goal = "f(x):length([],0)";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (compound module prefix) wrongly succeeded"); // shouldn't
																		// get
																		// to
																		// here
		} catch (PrologException e) { // correct exception class
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2)
					&& e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (compound module prefix) threw wrong PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (compound module prefix) threw wrong exception class: " + e);
		}
	}

	public void testGoalWithModulePrefix6() {
		String goal = "no_such_module:no_such_predicate(0)";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (nonexistent module prefix) wrongly succeeded"); // shouldn't
																			// get
																			// to
																			// here
		} catch (PrologException e) { // expected exception class
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("existence_error", 2)
					&& e.term().arg(1).arg(1).hasFunctor("procedure", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (nonexistent module prefix) threw wrong PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (nonexistent module prefix) threw wrong exception class: " + e);
		}
	}

	// public void testFetchCyclicTerm(){
	// assertTrue((new Query("X=f(X)")).hasSolution());
	// }
	public void testFetchLongList0() {
		assertTrue((new Query("findall(foo(N),between(0,10,N),L)")).hasSolution());
	}

	public void testFetchLongList1() {
		assertTrue((new Query("findall(foo(N),between(0,100,N),L)")).hasSolution());
	}

	public void testFetchLongList2() {
		assertTrue((new Query("findall(foo(N),between(0,1000,N),L)")).hasSolution());
	}

	public void testFetchLongList2c() {
		assertTrue((new Query("findall(foo(N),between(0,1023,N),L)")).hasSolution());
	}

	// public void testFetchLongList2a() { /* leads to stack overflow */
	// assertTrue((new
	// Query("findall(foo(N),between(0,2000,N),L)")).hasSolution());
	// }
	// public void testFetchLongList2b() {
	// assertTrue((new
	// Query("findall(foo(N),between(0,3000,N),L)")).hasSolution());
	// }
	// public void testFetchLongList3() {
	// assertTrue((new
	// Query("findall(foo(N),between(0,10000,N),L)")).hasSolution());
	// }
	public void testUnicode0() {
		assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[] { new Atom(" ") }));
	}

	public void testUnicode0a() {
		assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[] { new Atom("\u0020") }));
	}

	public void testUnicode0b() {
		assertTrue(Query.hasSolution("atom_codes(?,[0])", new Term[] { new Atom("\u0000") }));
	}

	public void testUnicode0c() {
		assertTrue(Query.hasSolution("atom_codes(?,[1])", new Term[] { new Atom("\u0001") }));
	}

	public void testUnicode0d() {
		assertTrue(Query.hasSolution("atom_codes(?,[127])", new Term[] { new Atom("\u007F") }));
	}

	public void testUnicode0e() {
		assertTrue(Query.hasSolution("atom_codes(?,[128])", new Term[] { new Atom("\u0080") }));
	}

	public void testUnicode0f() {
		assertTrue(Query.hasSolution("atom_codes(?,[255])", new Term[] { new Atom("\u00FF") }));
	}

	public void testUnicode0g() {
		assertTrue(Query.hasSolution("atom_codes(?,[256])", new Term[] { new Atom("\u0100") }));
	}

	public void testUnicode1() {
		assertTrue(Query.hasSolution("atom_codes(?,[0,127,128,255])",
				new Term[] { new Atom("\u0000\u007F\u0080\u00FF") }));
	}

	public void testUnicode2() {
		assertTrue(Query.hasSolution("atom_codes(?,[256,32767,32768,65535])",
				new Term[] { new Atom("\u0100\u7FFF\u8000\uFFFF") }));
	}

	public void testStringXput1() {
		Term a = Query.oneSolution("string_concat(foo,bar,S)").get("S");
		assertEquals("foobar", a.name());
		assertEquals("string", a.atomType());
	}

	public void testStringXput2() {
		String s1 = "\u0000\u007F\u0080\u00FF";
		String s2 = "\u0100\u7FFF\u8000\uFFFF";
		String s = s1 + s2; // concatenate in Java
		Term a = Query.oneSolution("string_concat(?,?,S)", new Term[] { new Atom(s1), new Atom(s2) }).get("S"); // concatenate
																												// in
																												// Prolog
		assertEquals(s, a.name());
		assertEquals("string", a.atomType());
	}

	// public void testMaxInteger1() {
	// assertEquals(Query.oneSolution("current_prolog_flag(max_integer,I)").get("I").longValue(),
	// java.lang.Long.MAX_VALUE); // i.e. 9223372036854775807L
	// }

	// public void testSingleton1() {
	// assertTrue(Query.hasSolution("style_check(-singleton),consult('test_singleton.pl')"));
	// }

	public void testStaticQueryInvalidSourceText2() {
		String goal = "p(]";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (bad syntax) succeeded"); // shouldn't get to here
		} catch (PrologException e) { // expected exception
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1)
					&& e.term().arg(1).arg(1).hasFunctor("cannot_start_term", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (bad syntax) threw wrong PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (bad syntax) threw wrong exception class: " + e);
		}
	}

	public void testStaticQueryInvalidSourceText1() {
		String goal = "bad goal";
		try {
			Query.hasSolution(goal); // should throw exception
			fail(goal + " (bad syntax) succeeded"); // shouldn't get to here
		} catch (PrologException e) { // expected exception
			if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1)
					&& e.term().arg(1).arg(1).hasFunctor("operator_expected", 0)) {
				// OK: an appropriate exception was thrown
			} else {
				fail(goal + " (bad syntax) threw wrong PrologException: " + e);
			}
		} catch (Exception e) {
			fail(goal + " (bad syntax) threw wrong exception class: " + e);
		}
	}

	public void testStaticQueryNSolutions1() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 5;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions",
				Query.nSolutions(goal, n).length == n);
	}

	public void testStaticQueryNSolutions2() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 0;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions",
				Query.nSolutions(goal, n).length == n);
	}

	public void testStaticQueryNSolutions3() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		int n = 20;
		assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns 10 solutions",
				Query.nSolutions(goal, n).length == 10);
	}

	public void testStaticQueryAllSolutions1() {
		String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
		assertTrue("Query.allSolutions(" + goal + ") returns 10 solutions", Query.allSolutions(goal).length == 10);
	}

	public void testStaticQueryHasSolution1() {
		String goal = "memberchk(13, [?,?,?])";
		Term[] params = new Term[] { new Integer(12), new Integer(13), new Integer(14) };
		assertTrue(Query.hasSolution(goal, params));
	}

	public void testStaticQueryHasSolution2() {
		String goal = "memberchk(23, [?,?,?])";
		Term[] params = new Term[] { new Integer(12), new Integer(13), new Integer(14) };
		assertFalse(Query.hasSolution(goal, params));
	}

	public void testUtilListToTermArray1() {
		String goal = "T = [a,b,c]";
		Term list = Query.oneSolution(goal).get("T");
		Term[] array = Util.listToTermArray(list);
		assertTrue(array[2].isAtom() && array[2].name().equals("c"));
	}

	public void testTermToTermArray1() {
		String goal = "T = [a,b,c]";
		Term list = Query.oneSolution(goal).get("T");
		Term[] array = list.toTermArray();
		assertTrue(array[2].isAtom() && array[2].name().equals("c"));
	}

	public void testJRef1() {
		int i = 76543;
		Integer I = new Integer(i);
		Query q = new Query("jpl_call(?,intValue,[],I2)", new Term[] { JPL.newJRef(I) });
		Term I2 = q.oneSolution().get("I2");
		assertTrue(I2.isInteger() && I2.intValue() == i);
	}

	public void testBerhhard1() {
		assertTrue(Query.allSolutions("consult(library('lists'))").length == 1);
	}

	public void testWouter1() { // Wouter says this fails under OS X Mavericks
								// 10.9 x86-64
		long n = 7381783232223l; // too big for an int
		Compound term = new Compound("is", new Term[] { new Variable("X"), new org.jpl7.Integer(n) });
		Map<String, Term>[] solutions = new Query(term).allSolutions();
		assertEquals(1, solutions.length);
		Map<String, Term> solution = solutions[0];
		assertTrue(solution.containsKey("X"));
		Object result = solution.get("X");
		assertTrue(result instanceof org.jpl7.Integer);
		assertEquals(n, ((org.jpl7.Integer) result).longValue());
	}

	public void testJRef2() {
		int i = 76543;
		Integer I = new Integer(i);
		Query q = new Query("jpl_call(?,intValue,[],I2)", JPL.newJRef(I));
		Term I2 = q.oneSolution().get("I2");
		assertTrue(I2.isInteger() && I2.intValue() == i);
	}

	public void testJRef3() {
		StringBuffer sb = new StringBuffer();
		Query.oneSolution("jpl_call(?,append,['xyz'],_)", new Term[] { JPL.newJRef(sb) });
		assertTrue(sb.toString().equals("xyz"));
	}

	public void testJRef4() {
		Term jrefSB = Query.oneSolution("jpl_new('java.lang.StringBuffer',['abc'],SB)").get("SB");
		assertTrue(jrefSB.isJRef() && ((StringBuffer) jrefSB.object()).toString().equals("abc"));
	}

	public void testJRef5() {
		String token = "foobar345";
		Term a = Query.oneSolution("jpl_new('java.lang.StringBuffer',[?],A)", new Term[] { new Atom(token) }).get("A");
		assertTrue(((java.lang.StringBuffer) (a.object())).toString().equals(token));
	}

	public void testRef6() {
		Term nullJRef = JPL.newJRef(null);
		Object nullObject = nullJRef.object();
		assertNull("JPL null Term yields a null object", nullObject);
	}

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


	public void testForeignFrame1() {
		int ls1 = Query.oneSolution("statistics(localused,LS)").get("LS").intValue();
		int ls2 = Query.oneSolution("statistics(localused,LS)").get("LS").intValue();
		assertTrue("local stack size unchanged after query", ls1 == ls2);
	}

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

	public void testOpen1() {
		Query q = new Query("dummy");
		assertTrue("a newly created query is not open", !q.isOpen());
	}

	public void testOpen2() {
		Query q = new Query("fail");
		q.open();
		assertTrue("a newly opened query which has no solutions is open", q.isOpen());
	}

	public void testGetSolution1() {
		Query q = new Query("fail");
		q.open();
		if (q.hasMoreSolutions()) q.nextSolution();
		assertTrue("A query has exhausted all solutions but it is still open", !q.isOpen());
	}

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

	public void testHasMoreSolutions1() {
		StringBuffer sb = new StringBuffer();
		Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
		Map<String, Term> soln;
		q.open();
		while (q.hasMoreSolutions()) {
			soln = q.nextSolution();
			sb.append(((Atom) soln.get("C")).name());
		}
		q.close();
		assertEquals("Query#hasMoreSolutions() + Query#nextSolution() work as intended", "prolog", sb.toString());
	}

	@SuppressWarnings("unchecked")
	public void testHasMoreElements1() {
		StringBuffer sb = new StringBuffer();
		Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
		Map<String, Term> soln;
		q.open();
		while (q.hasMoreElements()) {
			soln = (Map<String, Term>) q.nextElement();
			sb.append(((Atom) soln.get("C")).name());
		}
		q.close();
		assertEquals("Query#hasMoreElements() + Query#nextElement() work as intended", "prolog", sb.toString());
	}

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

}
