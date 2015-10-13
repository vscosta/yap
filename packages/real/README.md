
Real
---

Real is a c-based interface for connecting R to Prolog. 

YAP introduces a development version of real, developed to experiment
with the internals of the implementation of R. It includes major
changes and is likely to be much less stable than the version
maintained by Nicos ANgelopoulos. We refer to the version herein as
'realC' and describe the main novelties vs the version described
in~\cite{}. Their major differences:

   - Most of realC is written in `C`, instead of aa a Prolog string
   generator. The `C` code respects the SWI-Prolog fli interface and
   should work both in YAP and in SWI-Prolog.

   - realC uses Prolog atoms to represent real variables. R sequences
     of characters are represented as Prolog strings (not as lists of
     character codes). The atoms `true` and `false` indicate boolean
     constants.

     By default, YAP represents sequences of codes using double
     quotes, and strings by back quotes. Please consult the
     documentation o the ISO-Prolog flag `double_quotes` if you using
     prefer reading double-quote strings as Prolog string.

   - Free variables can be used to represent missing
     arguments,ie. `a[_,"G23"] would represent the column "G23".

   - All recent versions of real support the common syntax extensions
     for [], (), thus realC allows writing `a[[2]] <- f().

   - YAP allows A.B to be interpreted as [A|B]. This version takes
   advantage of this implementation quirk, and allows one to write
   expressions such as `a.b[2] <- f.g()`.

   - The left-hand side msy be:
     + a ground unary term, assumed to be an attribute
     + an index
     + an R variable
     + a logic variable, or other Prolog term: in this case it will be
     unified with the result of evaluating the right-hamd side.
 Yap
   ?- [examples/for_real].
   ?- for_real.


---
Nicos Angelopoulos and Vitor Santos Costa
December, 2012.

Updates: Nicos Angelopoulos
Dec. 2013,
March, 2014

Updates: Vitor Santos Costa
Dec. 2015

