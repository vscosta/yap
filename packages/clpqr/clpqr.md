Constraint Logic Programming over Rationals and Reals  {#clpqr}
=====================================================

YAP now uses the CLP(R) package developed by <em>Leslie De Koninck</em>,
K.U. Leuven as part of a thesis with supervisor Bart Demoen and daily
advisor Tom Schrijvers, and distributed with SWI-Prolog.

This CLP(R) system is a port of the CLP(Q,R) system of Sicstus Prolog
and YAP by Christian Holzbaur: Holzbaur C.: OFAI clp(q,r) Manual,
Edition 1.3.3, Austrian Research Institute for Artificial
Intelligence, Vienna, TR-95-09, 1995,
<http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09> This
port only contains the part concerning real arithmetics. This manual
is roughly based on the manual of the above mentioned  *CLP(QR)*
implementation.

Please note that the clpr library is <em>not</em> an
`autoload` library and therefore this library must be loaded
explicitely before using it:

~~~~~
:- use_module(library(clpr)).
~~~~~

###  Solver Predicates {#CLPQR_Solver_Predicates}

The following predicates are provided to work with constraints:


### Syntax of the predicate arguments {#CLPQR_Syntax}


The arguments of the predicates defined in the subsection above are
defined in the following table. Failing to meet the syntax rules will
result in an exception.

~~~~~
<Constraints> ---> <Constraint>				\ single constraint \
	      | <Constraint> , <Constraints>		\ conjunction \
	      | <Constraint> ; <Constraints>		\ disjunction \

<Constraint> ---> <Expression> {<} <Expression>		\ less than \
	     | <Expression> {>} <Expression>		\ greater than \
	     | <Expression> {=<} <Expression>	\ less or equal \
	     | {<=}(<Expression>, <Expression>)	\ less or equal \
	     | <Expression> {>=} <Expression>	\ greater or equal \
	     | <Expression> {=\=} <Expression>	\ not equal \
	     | <Expression> =:= <Expression>		\ equal \
	     | <Expression> = <Expression>		\ equal \

<Expression> --->  <Variable>				\ Prolog variable \
	     | <Number>				\ Prolog number (float, integer) \
	     | +<Expression>				\ unary plus \
	     | -<Expression>				\ unary minus \
	     | <Expression> + <Expression>		\ addition \
	     | <Expression> - <Expression>		\ substraction \
	     | <Expression> * <Expression>		\ multiplication \
	     | <Expression> / <Expression>		\ division \
	     | abs(<Expression>)			\ absolute value \
	     | sin(<Expression>)			\ sine \
	     | cos(<Expression>)			\ cosine \
	     | tan(<Expression>)			\ tangent \
	     | exp(<Expression>)			\ exponent \
	     | pow(<Expression>)			\ exponent \
	     | <Expression> {^} <Expression>		\ exponent \
	     | min(<Expression>, <Expression>)	\ minimum \
	     | max(<Expression>, <Expression>)	\ maximum \
~~~~~


###  Use of unification {#CLPQR_Unification}

Instead of using the `{}/1` predicate, you can also use the standard
unification mechanism to store constraints. The following code samples
are equivalent:

+ Unification with a variable

~~~~~
{X =:= Y}
{X = Y}
X = Y
~~~~~

+ Unification with a number

~~~~~
{X =:= 5.0}
{X = 5.0}
X = 5.0
~~~~~


####  Non-Linear Constraints {#CLPQR_NonhYlinear_Constraints}


In this version, non-linear constraints do not get solved until certain
conditions are satisfied. We call these conditions the _isolation_ axioms.
They are given in the following table.

~~~~~
A = B * C      when B or C is ground	or		 // A = 5 * C or A = B * 4 \\
	                A and (B or C) are ground	 // 20 = 5 * C or 20 = B * 4 \\

A = B / C      when C is ground or			// A = B / 3
	                A and B are ground		// 4 = 12 / C

X = min(Y,Z)   when Y and Z are ground or	// X = min(4,3)
X = max(Y,Z)        Y and Z are ground		// X = max(4,3)
X = abs(Y)          Y is ground			    // X = abs(-7)

X = pow(Y,Z)   when X and Y are ground or		// 8 = 2 ^ Z
X = exp(Y,Z)           X and Z are ground		// 8 = Y ^ 3
X = Y ^ Z              Y and Z are ground		// X = 2 ^ 3

X = sin(Y)	    when X is ground or			// 1 = sin(Y)
X = cos(Y)	         Y is ground			// X = sin(1.5707)
X = tan(Y)
~~~~~
