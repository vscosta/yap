package org.jpl7.test;

import org.jpl7.Query;

public class SyntaxError {
	public static void main(String argv[]) {
		Query q = new Query("syntax)error");
		System.err.println(q.hasSolution() ? "yes" : "no");
	}
}
