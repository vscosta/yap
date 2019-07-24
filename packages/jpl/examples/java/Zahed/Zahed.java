import java.util.Map;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

public class Zahed {
	public static void main(java.lang.String argv[]) {
		System.out.println("starting...");
		Compound goal1 = new Compound("consult", new Term[] { new Atom("zahed.pl") });
		Query q1 = new Query(goal1);
		if (!q1.hasSolution()) {
			System.out.println("consult('zahed.pl') failed");
			return;
		}
		Term t2 = new Compound("t", new Term[] { new Atom("v"), new Atom("[]"), new Atom("a") });
		Compound list2 = new Compound(".", new Term[] { t2, new Atom("[]") });
		Compound t1 = new Compound("t", new Term[] { new Atom("c"), new Atom("q"), new Atom("[]") });
		Compound list1 = new Compound(".", new Term[] { t1, list2 });
		Variable answer = new Variable("A");
		Compound goal2 = new Compound("gen", new Term[] { list1, answer });
		Query q2 = new Query(goal2);
		Map<String, Term> solution = q2.oneSolution();
		if (solution == null) {
			System.out.println("failed");
		} else {
			System.out.println(solution.get("A").toString());
		}
		System.out.println("finished");
	}
}
