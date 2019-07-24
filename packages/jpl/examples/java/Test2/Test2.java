import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

public class Test2 {
	public static int fac(int n) {
		if (n == 1) {
			return 1;
		} else {
			return n * ((org.jpl7.Integer) new Query(new Compound("jpl_test_fac", new Term[] { new org.jpl7.Integer(n - 1), new Variable("F") })).oneSolution().get("F")).intValue();
		}
	}

	public static void main(String argv[]) {
		Query.oneSolution("consult('test2.pl')");
		System.out.print("calling Prolog to call Java to call Prolog...\n");
		System.out.println("factorial(10) = " + fac(10));
	}
}
