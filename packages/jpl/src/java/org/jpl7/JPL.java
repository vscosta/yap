package org.jpl7;

import java.io.File;

import org.jpl7.fli.Prolog;

/**
 * The jpl.JPL class contains static methods which allow (i) inspection and
 * alteration of the "default" initialisation arguments (ii) explicit
 * initialisation (iii) discovery of whether the Prolog engine is already
 * initialised, and if so, with what arguments. The Prolog engine must be
 * initialized before any queries are made, but this will happen automatically
 * (upon the first call to a Prolog FLI routine) if it has not already been done
 * explicitly.
 *
 * <hr>
 * Copyright (C) 2004 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * <ol>
 * <li>Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * <li>Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 *
 */
public class JPL {
	protected static final boolean DEBUG = false;

	public static final Term JFALSE = new Compound("@", new Term[] { new Atom("false") });
	public static final Term JTRUE = new Compound("@", new Term[] { new Atom("true") });
	public static final Term JNULL = new Compound("@", new Term[] { new Atom("null") });
	public static final Term JVOID = new Compound("@", new Term[] { new Atom("void") });

	protected static boolean modeDontTellMe = true;

	protected static final Atom LIST_NIL_MODERN = new Atom("[]", "reserved_symbol"); // NB
																						// an
																						// actual
																						// Atom
																						// (see
																						// LIST_PAIR_MODERN
																						// below)
	protected static final Atom LIST_NIL_TRADITIONAL = new Atom("[]", "text");
	public static Atom LIST_NIL = LIST_NIL_MODERN; // default unless/until
													// setTraditional() is
													// called successfully

	protected static final String LIST_PAIR_MODERN = "[|]"; // NB just the name
															// of the functor
															// (see
															// LIST_NIL_MODERN
															// above)
	protected static final String LIST_PAIR_TRADITIONAL = ".";
	public static String LIST_PAIR = LIST_PAIR_MODERN; // default unless/until
														// setTraditional() is
														// called successfully

	private static String nativeLibraryName = "jpl";
	private static String nativeLibraryDir = null;
	private static String nativeLibraryPath = null;

	public static String setNativeLibraryName(String newName) {
		if (newName == null) {
			throw new NullPointerException("newName cannot be null");
		} else {
			String oldName = nativeLibraryName;
			nativeLibraryName = newName;
			return oldName;
		}
	}

	public static String setNativeLibraryDir(String newDir) {
		String oldDir = nativeLibraryDir;
		nativeLibraryDir = newDir;
		return oldDir;
	}

	public static String setNativeLibraryPath(String newPath) {
		String oldPath = nativeLibraryPath;
		nativeLibraryPath = newPath;
		return oldPath;
	}

	public static void loadNativeLibrary() {
		if (nativeLibraryPath != null) {
			System.load((new File(nativeLibraryPath)).getAbsolutePath());
		} else if (nativeLibraryDir != null) {
			System.load((new File(nativeLibraryDir, System.mapLibraryName(nativeLibraryName))).getAbsolutePath());
		} else {
			System.loadLibrary(nativeLibraryName); // as resolved somewhere on
													// system property
													// 'java.library.path'
		}
	}

	public static String jarPath() {
		try {
			return Class.forName("org.jpl7.JPL").getProtectionDomain().getCodeSource().getLocation().toString();
		} catch (ClassNotFoundException e) {
			return "";
		}
	}

	/**
	 * Sets the global "dont-tell-me" mode (default value: true). When 'true',
	 * bindings will *not* be returned for any variable (in a Query's goal)
	 * whose name begins with an underscore character (except for "anonymous"
	 * variables, i.e. those whose name comprises just the underscore character,
	 * whose bindings are never returned). When 'false', bindings are returned
	 * for *all* variables except anonymous ones; this mode may be useful when
	 * traditional top-level interpreter behaviour is wanted, e.g. in a
	 * Java-based Prolog IDE or debugger.
	 * <p>
	 * This method should be regarded as experimental, and may subsequently be
	 * deprecated in favour of some more general mechanism for setting options,
	 * perhaps per-Query and per-call as well as globally.
	 *
	 * @param dtm
	 *            new "dont-tell-me" mode value
	 */
	public static void setDTMMode(boolean dtm) {
		modeDontTellMe = dtm;
	}

	public static void setTraditional() {
		if (getSyntax().equals("modern")) {
			if (getActualInitArgs() == null) { // i.e. Prolog is not yet
												// initialised
				LIST_NIL = LIST_NIL_TRADITIONAL;
				LIST_PAIR = LIST_PAIR_TRADITIONAL;
				setDefaultInitArgs(argsEnsureSyntax(getDefaultInitArgs())); // ensure
																			// that
																			// default
																			// init
																			// args
																			// contain
																			// "--traditional"
			} else {
				throw new JPLException("cannot switch to traditional syntax after Prolog is initialised");
			}
		} else {
			// benign attempt to set traditional when already using that syntax
		}
	}

	public static void setTraditionalAnyway() {
		LIST_NIL = LIST_NIL_TRADITIONAL;
		LIST_PAIR = LIST_PAIR_TRADITIONAL;
	}

	public static String getSyntax() {
		if (LIST_PAIR.equals(LIST_PAIR_MODERN)) {
			return "modern";
		} else if (LIST_PAIR.equals(LIST_PAIR_TRADITIONAL)) {
			return "traditional";
		} else {
			throw new JPLException("syntax is neither traditional nor modern");
		}
	}

	/**
	 * Returns, in an array of String, the sequence of command-line arguments
	 * that would be used if the Prolog engine were to be initialised now.
	 * Returns null if the Prolog VM has already been initialised (in which case
	 * the default init args are irrelevant and the actual init args are of
	 * interest)
	 * <p>
	 *
	 * @see org.jpl7.JPL#getActualInitArgs
	 * @return current default initialisation arguments, or null if already
	 *         initialised
	 */
	public static String[] getDefaultInitArgs() {
		return Prolog.get_default_init_args();
	}

	/**
	 * Specifies, in an array of String, the sequence of command-line arguments
	 * that should be used if the Prolog engine is subsequently initialised.
	 * <p>
	 *
	 * @param args
	 *            new default initialization arguments
	 */
	public static void setDefaultInitArgs(String[] args) {
		Prolog.set_default_init_args(argsEnsureSyntax(args));
	}

	private static String[] argsEnsureSyntax(String[] argsA) {
		String[] argsC; // return value
		int qtyTrad = 0; // provisional # occurrences of "--traditional" in
							// given args (oughta be 0 or 1, but we cope with
							// 2+)
		for (int iA = 0; iA < argsA.length; iA++) {
			if (argsA[iA].equals("--traditional")) {
				qtyTrad++;
			}
		}
		String argsB[] = new String[argsA.length - qtyTrad]; // for given args
																// stripped of
																// all
																// "--traditional"
		for (int iA = 0, iB = 0; iA < argsA.length; iA++) {
			if (!argsA[iA].equals("--traditional")) {
				argsB[iB++] = argsA[iA];
			}
		}
		if (getSyntax().equals("modern")) {
			argsC = argsB;
		} else {
			argsC = new String[argsB.length + 1];
			argsC[0] = argsB[0]; // e.g. "swipl"
			argsC[1] = "--traditional";
			for (int iB = 1, iC = 2; iB < argsB.length; iB++) {
				argsC[iC++] = argsB[iB];
			}
		}
		return argsC;
	}

	/**
	 * Returns, in an array of String, the sequence of command-line arguments
	 * that were actually used when the Prolog engine was formerly initialised.
	 *
	 * This method returns null if the Prolog engine has not yet been
	 * initialised, and thus may be used to test this condition.
	 *
	 * @return actual initialization arguments
	 */
	public static String[] getActualInitArgs() {
		return Prolog.get_actual_init_args();
	}

	/**
	 * Initializes the Prolog engine, using the String argument parameters
	 * passed. This method need be called only if you want to both (i)
	 * initialise the Prolog engine with parameters other than the default ones
	 * and (ii) force initialisation to occur (rather than allow it to occur
	 * automatically at the first query). For parameter options, consult your
	 * local Prolog documentation. The parameter values are passed directly to
	 * initialization routines for the Prolog environment.
	 * <p>
	 *
	 * This method must be called before making any queries.
	 *
	 * @param args Initialization parameter list
     * @return true iff initialization was new; false if already initialized
	 */
	public static boolean init(String[] args) {
		return Prolog.set_default_init_args(args) && init();
	}

	/**
	 * Initialises the Prolog engine using the current default initialisation
	 * parameters, and returns 'true' (or 'false' if already initialised).
     *
     * @return true if initialization was new, false if already initialized
	 */
	public static boolean init() {
		return Prolog.initialise();
	}

	/**
     * Checks if a string is a simple atom, with no quoting needed
     *
	 * @param s string to check if it is simple name (no quoting needed)
	 * @return whether s is a simple name, i.e. which needs no quoting in source text
	 */
	protected static boolean isSimpleName(String s) {
		int len;
		char c;
		if (s == null) {
			throw new java.lang.NullPointerException(); // JPL won't call it this way
		} else if ((len = s.length()) == 0) {
			return false;
		} else if ((c = s.charAt(0)) < 'a' || c > 'z') {
			return false;
		} else {
			for (int i = 1; i < len; i++) {
				c = s.charAt(i);
				if (!(c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9')) {
					return false;
				}
			}
			return true;
		}
	}

	// /**
	// * whether the String arg is a plausible tag, e.g. "J#0123456789".
	// */
	// public static boolean isTag(String s) {
	// return s.length() == 22 && s.charAt(0) == 'J' && s.charAt(1) == '#' &&
	// Character.isDigit(s.charAt(2)) && Character.isDigit(s.charAt(3)) &&
	// Character.isDigit(s.charAt(4))
	// && Character.isDigit(s.charAt(5)) && Character.isDigit(s.charAt(6)) &&
	// Character.isDigit(s.charAt(7)) && Character.isDigit(s.charAt(8)) &&
	// Character.isDigit(s.charAt(9))
	// && Character.isDigit(s.charAt(10)) && Character.isDigit(s.charAt(11)) &&
	// Character.isDigit(s.charAt(12)) && Character.isDigit(s.charAt(13)) &&
	// Character.isDigit(s.charAt(14))
	// && Character.isDigit(s.charAt(15)) && Character.isDigit(s.charAt(16)) &&
	// Character.isDigit(s.charAt(17)) && Character.isDigit(s.charAt(18)) &&
	// Character.isDigit(s.charAt(19))
	// && Character.isDigit(s.charAt(20)) && Character.isDigit(s.charAt(21));
	// }

	/**
	 * @param object the object of interest to get the JREF
	 * @return a new Term instance which canonically represents the given object
	 *         reference (concrete or null)
	 */
	public static Term newJRef(Object object) {
		if (object == null) {
			return JPL.JNULL;
		} else {
			return new JRef(object);
		}
	}

	/**
	 * Returns a quoted (iff necessary) form of the Atom's name, as understood
	 * by Prolog read/1
	 *
     * @param name  the name to quote if needed
	 * @return a quoted form of the Atom's name, as understood by Prolog read/1
	 */
	protected static String quotedName(String name) {
		return (isSimpleName(name) ? name : "'" + name + "'");
	}

	/**
	 * Terminates the Prolog session.
	 * <p>
	 *
	 * <b>Note.</b> This method calls the FLI halt() method with a status of 0,
	 * but the halt method currently is a no-op in SWI.
	 *
	 * @deprecated
	 */
	@Deprecated
	public static void halt() {
		Prolog.halt(0);
	}

	/**
	 * a static reference to the current Version
	 */
	private static final Version version_ = new Version();

	/**
	 * Returns (as a Version) an identification of this version of JPL.
	 *
	 * @return the running version of JPL.
	 */
	public static Version version() {
		return version_;
	}

	/**
	 * Returns a String (eg "3.0.0-alpha") identifying this version of JPL.
	 *
	 * @return a String (eg "3.0.0-alpha") identifying this version of JPL.
	 */
	public static String version_string() {
		return version_.major + "." + version_.minor + "." + version_.patch + "-" + version_.status;
	}

	public static void main(String[] args) {
		System.out.println(version_string());
	}
}
