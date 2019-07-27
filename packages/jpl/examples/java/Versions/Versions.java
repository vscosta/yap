import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

public class Versions {
	public static void main(String argv[]) {
		String pVersion = ((Term) (new Query("jpl_pl_lib_version(V)")).oneSolution().get("V")).name();
		String jVersion = JPL.version_string();
		String cVersion = Prolog.get_c_lib_version();
		System.out.println("prolog library version; " + pVersion);
		System.out.println("  java library version; " + jVersion);
		System.out.println("     c library version; " + cVersion);
		if (pVersion.equals(jVersion) && jVersion.equals(cVersion)) {
			System.out.println("BINGO! you appear to have the same version of each library installed");
		} else {
			System.out.println("WHOOPS! you appear not to have the same version of each library installed");
		}
	}
}
