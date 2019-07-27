package org.jpl7;

/**
 * PrologException instances wrap Prolog exceptions thrown (either by a Prolog
 * engine or by user code) in the course of finding a solution to a Query. See
 * JPLException for the handling of errors within the JPL Java-calls-Prolog
 * interface.
 * <p>
 * This class allows Java code which uses JPL's Java-calls-Prolog API to handle
 * Prolog exceptions, which is in general necessary for hybrid Java+Prolog
 * programming.
 * <p>
 * Use the term() accessor to obtain a Term representation of the term that was
 * thrown from within Prolog.
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
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 */
public final class PrologException extends JPLException {
	private static final long serialVersionUID = 1L;
	private Term term_ = null;

	protected PrologException(Term term) {
		super("PrologException: " + term.toString());
		this.term_ = term;
	}

	/**
	 * @return a reference to the Term thrown by the call to throw/1
	 */
	public Term term() {
		return this.term_;
	}
}
