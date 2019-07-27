import java.util.Map;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;
import org.jpl7.Variable;

public class Test {
	public static void main(String argv[]) {
		run_tests();
	}

	static void run_tests() {
		test_0();
		test_1();
		test_2();
		test_3();
		test_4();
		test_5();
		test_6();
		test_7();
		test_8();
		test_9();
		test_10();
		test_11();
		test_101();
	}

	static void test_0() {
		System.out.print("test 0...");
		String t0 = "consult('test.pl')";
		if (!Query.hasSolution(t0)) {
			System.out.println(t0 + " failed");
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static Term a = new Atom("a");
	static Term b = new Atom("b");
	static Term f_a = new Compound("f", new Term[] { a });
	static Term pair_a_b = new Compound("-", new Term[] { a, b });

	static void test_1() {
		System.out.print("test 1...");
		String t1 = "p(a)";
		if (!Query.hasSolution(t1)) {
			System.out.println(t1 + " failed");
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_2() {
		System.out.print("test 2...");
		Query q2 = new Query("p", new Term[] { f_a });
		if (!q2.hasSolution()) {
			System.out.println("p(f(a)) failed");
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_3() {
		System.out.print("test 3...");
		Query q3 = new Query("p", new Term[] { pair_a_b });
		if (!q3.hasSolution()) {
			System.out.println("p(a-b) failed");
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_4() {
		System.out.print("test 4...");
		Variable X = new Variable("X");
		Query q4 = new Query("p", new Term[] { X });
		Term[] target = new Term[] { a, f_a, pair_a_b, new Variable("_") };
		Map<String, Term>[] solutions = q4.allSolutions();
		if (solutions.length != 4) {
			System.out.println("p(X) failed:");
			System.out.println("\tExpected: 4 solutions");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		for (int i = 0; i < solutions.length - 1; ++i) {
			Term binding = (Term) solutions[i].get("X");
			if (!binding.equals(target[i])) {
				System.out.println("p(X) failed");
				System.out.println("\tExpected: " + target[i]);
				System.out.println("\tGot:      " + binding);
				// System.exit(1);
			}
		}
		System.out.println("passed");
	}

	static void test_5() {
		System.out.print("test 5...");
		Variable X = new Variable("X");
		Variable Y = new Variable("Y");
		Query q5 = new Query("p", new Term[] { X, Y });
		Term[] x_target = new Term[] { a, a };
		Term[] y_target = new Term[] { a, b };
		Map<String, Term>[] solutions = q5.allSolutions();
		if (solutions.length != 2) {
			System.out.println("p(X, Y) failed:");
			System.out.println("\tExpected: 2 solutions");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		for (int i = 0; i < solutions.length; ++i) {
			Term x_binding = solutions[i].get("X");
			if (!x_binding.equals(x_target[i])) {
				System.out.println("p(X, Y) failed:");
				System.out.println("\tExpected: " + x_target[i]);
				System.out.println("\tGot:      " + x_binding);
				// System.exit(1);
			}
			Term y_binding = solutions[i].get("Y");
			if (!y_binding.equals(y_target[i])) {
				System.out.println("p( X, Y ) failed:");
				System.out.println("\tExpected: " + y_target[i]);
				System.out.println("\tGot:      " + y_binding);
				// System.exit(1);
			}
		}
		System.out.println("passed");
	}

	static void test_6() {
		System.out.print("test 6...");
		Variable X = new Variable("X");
		Query q6 = new Query("p", new Term[] { X, X });
		Term[] x_target = new Term[] { a };
		Map<String, Term>[] solutions = q6.allSolutions();
		if (solutions.length != 1) {
			System.out.println("p(X, X) failed:");
			System.out.println("\tExpected: 1 solution");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		for (int i = 0; i < solutions.length; ++i) {
			Object x_binding = solutions[i].get("X");
			if (!x_binding.equals(x_target[i])) {
				System.out.println("p(X, X) failed:");
				System.out.println("\tExpected: " + x_target[i]);
				System.out.println("\tGot:      " + x_binding);
				// System.exit(1);
			}
		}
		System.out.println("passed");
	}

	static void test_7() {
		System.out.print("test 7...");

		String t7 = "r(f(X,X), Y)";

		Variable vX = new Variable("X");
		Variable vY = new Variable("Y");
		Query q7 = new Query("r", new Term[] { new Compound("f", new Term[] { vX, vX }), vY });
		Map<String, Term>[] solutions = q7.allSolutions();

		// Hashtable<String, Term>[] solutions = Query.allSolutions(t7);

		if (solutions.length != 2) {
			System.out.println(t7 + " failed:");
			System.out.println("\tExpected: 2 solutions");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		Term X = solutions[0].get("X");
		Term Y = solutions[0].get("Y");
		if (X != Y) {
			System.out.println(t7 + " failed:");
			System.out.println(Util.toString(solutions[0]));
			System.out.println("\tThe variables to which X and Y are bound in the first solution should be identical.");
			// System.exit(1);
		}
		X = solutions[1].get("X");
		Y = solutions[1].get("Y");
		if (X == Y) {
			System.out.println(t7 + " failed:");
			System.out.println(Util.toString(solutions[1]));
			System.out.println("\tThe variables to which X and Y are bound in the second solution should be distinct.");
			// System.exit(1);
		}
		if (X.equals(Y)) {
			System.out.println(t7 + " failed:");
			System.out.println(Util.toString(solutions[1]));
			System.out.println("\tThe variables to which X and Y are bound in the second solution should not be \"equal\".");
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_8() {
		System.out.print("test 8...");
		// Variable X = new Variable("X");
		// Query query = new Query("r", new Term[] { new Compound("f", new Term[] { X, X }), X });
		String t8 = "r(f(X,X), X)";
		Map<String, Term>[] solutions = Query.allSolutions(t8);
		if (solutions.length != 2) {
			System.out.println(t8 + " failed:");
			System.out.println("\tExpected: 2 solutions");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		System.out.println("passed");
	}

	// corresponds with Prolog List: [a-a,a-b]
	static Term test_9_solution = Util.termArrayToList(new Term[] { new Compound("-", new Term[] { a, a }), new Compound("-", new Term[] { a, b }) });

	static void test_9() {
		System.out.print("test 9...");
		String t9 = "bagof(X-Y, p(X,Y), XYs)";
		Map<String, Term>[] solutions = Query.allSolutions(t9);
		if (solutions.length != 1) {
			System.out.println(t9 + " failed:");
			System.out.println("\tExpected: 1 solution");
			System.out.println("\tGot:      " + solutions.length);
			// System.exit(1);
		}
		Term term = solutions[0].get("XYs");
		if (!term.isListPair()) {
			System.out.println(t9 + " failed:");
			System.out.println("\tExpected: XYs to be a List");
			System.out.println("\tGot:      " + term);
			// System.exit(1);
		}
		if (!term.equals(test_9_solution)) {
			System.out.println(t9 + " failed:");
			System.out.println("\tExpected: " + test_9_solution);
			System.out.println("\tGot:      " + term);
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_10() {
		System.out.print("test 10...");
		String t10 = "t";
		try {
			boolean b = Query.hasSolution(t10);
			System.out.println(t10 + " failed:");
			System.out.println("\tExpected: PrologException");
			System.out.println("\tGot: " + b);
			// System.exit(1);
		} catch (PrologException e) {
			System.out.println("passed");
		} catch (Exception e) {
			System.out.println("wrong exception class thrown");
		}
	}

	static void test_11() {
		System.out.print("test 11...");
		Term tuple = new Compound("t", new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e") });
		try {
			Variable X = new Variable("X");
			Query q11 = new Query("tuple", new Term[] { X });
			Term result = q11.oneSolution().get("X");
			if (result == null || !result.equals(tuple)) {
				System.out.println("failed:");
				System.out.println("\tresult: " + result);
				System.out.println("\ttuple:  " + tuple);
				// System.exit(1);
			}
			if (result.arg(1) == null || !result.arg(1).equals(new Atom("a"))) {
				System.out.println("failed:");
				System.out.println("\tresult.arg(1): " + result.arg(1));
				// System.exit(1);
			}
			if (result.arg(2) == null || !result.arg(2).equals(new Atom("b"))) {
				System.out.println("failed:");
				System.out.println("\tresult.arg(2): " + result.arg(2));
				// System.exit(1);
			}
			if (result.arg(5) == null || !result.arg(5).equals(new Atom("e"))) {
				System.out.println("failed:");
				System.out.println("\tresult.arg(5): " + result.arg(5));
				// System.exit(1);
			}
			// arg0(6) throws an exception, as I'd expect it to...
			// if ( ((Compound)result).arg( 7 ) != null ){
			// System.out.println( "failed:" );
			// System.out.println( "\t((Compound)result).arg( 7 ): " + ((Compound)result).arg( 7 ) );
			// System.out.println( "\tshould be null" );
			// System.exit( 1 );
			// }
		} catch (PrologException e) {
			System.out.println("failed");
			e.printStackTrace();
			// System.exit(1);
		}
		System.out.println("passed");
	}

	static void test_101() {
		System.out.print("test 101...");
		Thread[] threads = new Thread[10];
		for (int i = 0; i < threads.length; ++i) {
			threads[i] = new QueryThread(i);
		}
		for (int i = 0; i < threads.length; ++i) {
			threads[i].start();
		}
		for (int i = 0; i < threads.length; ++i) {
			try {
				threads[i].join();
			} catch (InterruptedException ie) {
				ie.printStackTrace();
				// System.exit(1);
			}
		}
		System.out.println("passed");
	}

	private static class QueryThread extends Thread {
		private int id_ = -1;

		public QueryThread(int id) {
			this.id_ = id;
		}

		public java.lang.String toString() {
			return "(QueryThread id=" + id_ + ")";
		}

		public void run() {
			Query query = new Query("p", new Term[] { new Atom("a"), new Atom("a") });
			for (int i = 0; i < 10; ++i) {
				try {
					query.hasSolution();
				} catch (JPLException e) {
					System.out.println("Threaded p(a, a) threw exception: " + e);
					System.exit(1);
				}
				System.out.print(id_);
				Thread.yield();
			}
			for (int i = 0; i < 10; ++i) {
				try {
					while (query.hasMoreSolutions()) {
						Thread.yield();
						query.nextSolution();
					}
				} catch (JPLException e) {
					System.out.println("Threaded p(a, a) threw exception: " + e);
					System.exit(1);
				}
				System.out.print(id_);
			}
		}
	}
}
