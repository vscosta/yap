import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;

public class SemWeb {
	public static void main(String argv[]) {
		String t1 = "use_module(library('semweb/rdf_db'))";
		System.out.println(t1 + " " + (Query.hasSolution(t1) ? "succeeded" : "failed"));
		//
		String t2 = "rdf_load('test.rdf')";
		System.out.println(Query.hasSolution(t2) ? "loaded" : "load failed");
		//
		String t3 = "rdf(S,P,O)";
		Query q3 = new Query(t3);
		while (q3.hasMoreSolutions()) {
			Map<String, Term> s3 = q3.nextSolution();
			System.out.println("{" + s3.get("S") + ", " + s3.get("P") + ", " + s3.get("O") + "}");
		}
	}
}
