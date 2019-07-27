package org.jpl7.fli;

/**
 * A term_t is a simple class which mirrors the term_t type in the Prolog FLI.
 * All it really does is hold a term reference, which is an internal
 * representation of a term in the Prolog Engine.
 * 
 * <hr>
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
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 */
public class term_t extends LongHolder {
	public static final long UNASSIGNED = -1L;

	public term_t() {
		value = UNASSIGNED;
	}

	/**
	 * This static method converts a term_t, which is assumed to contain a
	 * reference to a *consecutive* list of term_t references to a String
	 * representation of a list of terms, in this case, a comma separated list.
	 * 
	 * @param n
	 *            the number of consecutive term_ts
	 * @param term0
	 *            a term_t whose value is the 0th term_t.
	 * @return string representation of the term
	 */
	public static String toString(int n, term_t term0) {
		String s = "";
		int i;
		long ith_term_t;
		for (i = 0, ith_term_t = term0.value; i < n; ++i, ++ith_term_t) {
			term_t term = new term_t();
			term.value = ith_term_t;
			s += term.toString();
			if (i != n - 1) {
				s += ", ";
			}
		}
		return s;
	}

	/**
	 * Instances of term_ts are stored in Term objects (see jpl.Term), and these
	 * term_ts are in some cases stored in Hashtables. Supplying this predicate
	 * provides the right behavior in Hashtable lookup (see the rules for
	 * Hashtable lookup in java.util).
	 * <p>
	 * 
	 * Note. Two term_ts are *not* equal if their values have not been assigned.
	 * (Since Prolog FLI term_ts are unsigned values and the UNASSIGNED value is
	 * -1, this should work).
	 * 
	 * @param obj
	 *            the Object to comapre.
	 * @return true if the supplied object is a term_t instances and the long
	 *         values are the same
	 */
	public boolean equals(Object obj) {
		return (obj instanceof term_t) && this.value == ((term_t) obj).value && this.value != UNASSIGNED;
	}
}
