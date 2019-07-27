package org.jpl7.test;

import java.net.URL;
import java.util.Map;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

public class Versions {
	public static void main(String argv[]) {

		System.out.println("command line args: (" + argv.length + ")");
		for (int i = 0; i < argv.length; i++) {
			System.out.println("  argv[" + i + "]: " + argv[i]);
		}
		System.out.println();

		System.out.println("old (built in) default init args:");
		String[] defaultInitArgsOld = org.jpl7.JPL.getDefaultInitArgs();
		for (int i = 0; i < defaultInitArgsOld.length; i++) {
			System.out.println("  arg[" + i + "]: " + defaultInitArgsOld[i]);
		}
		System.out.println();

		String[] defaultInitArgsNew1;
		if (argv.length == 1 && argv[0].equals("traditional")) {
			defaultInitArgsNew1 = new String[] {
				"swipl", "-g", "true", "--nosignals",
				"--traditional" };
		} else {
			defaultInitArgsNew1 = new String[] {
				"swipl", "-g", "true", "--nosignals" };
		}
		org.jpl7.JPL.setDefaultInitArgs(defaultInitArgsNew1);

		System.out.println("new (stashed) default init args:");
		String[] defaultInitArgsNew2 = org.jpl7.JPL.getDefaultInitArgs();
		for (int i = 0; i < defaultInitArgsNew2.length; i++) {
			System.out.println("  arg[" + i + "]: " + defaultInitArgsNew2[i]);
		}
		System.out.println();

		// if (!Query.hasSolution("consult('Versions.pl')")) {
		// System.out.println("Warning: failed to consult Versions.pl");
		// System.out.println();
		// }

		if (!(new Query("consult", new Atom("jpl/test/Versions.pl"))).hasSolution()) {
			System.out.println("Warning: failed to consult Versions.pl");
			System.out.println();
		}

		// String swiplHome = ((Term) (new
		// Query("current_prolog_flag(home,Home)")).oneSolution().get("Home")).name();
		// System.out.println(" SWI-Prolog home dir: " + swiplHome );

		System.out.println("home1 = " + (new Atom("c:/swipl-7.1.26")).toString());

		Query q1 = new Query("current_prolog_flag", new Term[] { new Atom("home"), new Variable("Home") });
		Map<String, Term> h1 = q1.oneSolution();
		Term home = (Term) h1.get("Home");
		// System.out.println("Home = " + home.debugString());
		System.out.println("Home = " + home.toString());

		try {
			URL jarPathJpl = Class.forName("org.jpl7.JPL").getProtectionDomain().getCodeSource().getLocation();
			System.out.println("package jpl loaded from: " + jarPathJpl);
		} catch (ClassNotFoundException e) {
			System.out.println("org.jpl7.JPL not found");
		}

		String prologVersion = ((Term) (new Query("jpl_pl_lib_version(V)")).oneSolution().get("V")).name();
		System.out.println(" prolog library version: " + prologVersion);
		String javaVersion = org.jpl7.JPL.version_string();
		System.out.println("   java library version: " + javaVersion);
		String cVersion = org.jpl7.fli.Prolog.get_c_lib_version();
		System.out.println("      c library version: " + cVersion);

		System.out.println("      SWI Prolog syntax: " + org.jpl7.fli.Prolog.get_syntax());

		// if ( prologVersion.equals(javaVersion) &&
		// javaVersion.equals(cVersion) ) {
		// System.out.println("BINGO! you appear to have the same version of
		// each library installed");
		// } else {
		// System.out.println("WHOOPS! you appear not to have the same version
		// of each library installed");
		// }

		System.out.println();
	}
}
