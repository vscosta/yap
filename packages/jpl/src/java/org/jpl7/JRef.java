package org.jpl7;

import java.util.Map;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;

/**
 * JRef is a specialised Term with a (non-null, non-String) Object field, representing JPL 7.4's Prolog references to Java objects, e.g.
 * &lt;jref&gt;(0x01D8000).
 *
 * <hr>
 * Copyright (C) 2004-2017 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are
 * met:
 *
 * <ol>
 * <li>Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * <li>Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 *
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 * @see org.jpl7.Term
 *
 */
public class JRef extends Term {

	/**
	 * the JRef's value (a non-null, non-String Object)
	 */
	protected final Object object;

	/**
	 * This constructor creates a JRef, initialized with the supplied (non-null, non-String) Object.
	 *
	 * @param object
	 *            this JRef's value (a non-null, non-String Object)
	 */
	public JRef(Object object) {
		if (object == null) {
			throw new JPLException("a JRef cannot reference null (use JPL.newJRef() or JPL.JNULL)");
		} else if (object instanceof String) {
			throw new JPLException("a JRef cannot reference a String (Strings are represented in Prolog by text atoms)");
		} else {
			this.object = object;
		}
	}

	public final String atomType() {
		return "jref";
	}

	/*
	 * Two JRefs are equal if their referenced objects are identical
	 *
	 * @param obj The Object to compare
	 *
	 * @return true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof JRef && ((JRef) obj).object == this.object);
	}

	public boolean hasFunctor(String name, int arity) {
		return false; // according to functor/3, a <jref>(0x01d8000)'s functor
						// name is the blob itself
	}

	/**
	 * @deprecated Use org.jpl7.JRef.object()
	 * @see org.jpl7.JRef#object()
	 */
	@Deprecated
	public Object jrefToObject() {
		return object;
	}

	/**
	 * Returns the actual object the JREF stands for
	 *
	 * @return the object of the JREF term
	 */
	public Object object() {
		return object;
	}

	/**
	 * To convert a JRef to a term, we put its Object field (.ref) into the term_t as a &lt;jref&gt;(0x01DF800) blob.
	 *
	 * @param varnames_to_vars
	 *            A Map from variable names to Prolog variables.
	 * @param term
	 *            A (newly created) term_t which is to be set to a Prolog &lt;jref&gt;(0x01DF800) blob denoting the .ref of this JRef
	 *            instance
	 */
	protected final void put(Map<String, term_t> varnames_to_vars, term_t term) {
		Prolog.put_jref(term, object);
	}

	/**
	 * @deprecated Use org.jpl7.JRef.object()
	 * @see org.jpl7.JRef#object()
	 */
	@Deprecated
	public Object ref() {
		return object;
	}

	/**
	 * Returns a Prolog source text representation of this JRef
	 *
	 * @return a Prolog source text representation of this JRef
	 */
	public String toString() {
		// org.jpl7.fli.Prolog.object_to_tag(ref)
		return "<jref>(0x???????)"; // WRONG
	}

	public final int type() {
		return Prolog.JREF;
	}

	public String typeName() {
		return "JRef";
	}

}
