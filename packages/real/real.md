
<!--- @@file real.md --->
<!--- @@author	Nicos Angelopoulos --->
<!--- @@author	Vitor Santos Costa --->
<!--- @@version	1:0:4, 2013/12/25, sinter_class --->
<!--- @@license	Perl Artistic License --->

@defgroup real The R Prolog Programming Interface
<!--- @@ingroup packages --->
<!--- @@{ --->

  + @tableofcontents

This library enables the communication with an R process started as a shared library.
It is the result of the efforts of two research groups that have worked in parallel.
The syntactic emphasis on a minimalistic interface.

In the doc/ directory of the distribution there is user's guide, a published paper
and html documentation from PlDoc (doc/html/real.html). There is large number
of examples in `examples/for_real.pl`.

A single predicate (<-/2,<-/1) channels
the bulk of the interactions. In addition to using R as a shared library, real uses
the c-interfaces of SWI/Yap and R to pass objects in both directions.
The usual mode of operation is to load Prolog values on to R variables and then call
R functions on these values. The return value of the called function can be either placed
on R variable or passed back to Prolog.  It has been tested extensively on current
SWI and YAP on Linux machines but it should also compile and work on MS operating systems and Macs.

The main modes for utilising the interface are
~~~~
	<- +Rexpr
	<- +Rvar
~~~~

     Print  Rvar or evaluate expression Rexpr in R
~~~~
	+Rvar   <- +PLdata
	+Rexpr  <- +PLdata
	-PLvar  <- +Rvar
	-PLvar  <- +Rexpr
	+Rexpr1 <- +Rexpr2
~~~~

Pass Prolog data to R, pass R data to Prolog or assign an R expression to
an assignable R expression.

@defgroup TestingR Testing Real
<!--- @@ingroup realmd --->
<!--- @@{ --->

There is a raft of examples packed in a single file that tests the library.

~~~~
	?- [pack(real/examples/for_real)].

	?- for_real.

	?- edit( pack(real/examples/for_real) ).
~~~~
<!--- @@} --->
@defgroup RSyntax Prolog and R Syntax
<!--- @@ingroup real --->
<!--- @@{ --->

There are syntactic conventions in R that make unparsable prolog code.
Notably function and variable names are allowed to contain dots, square brackets are used
to access parts of vectors and arrays and functions are allowed empty argument tuples.
We have introduced relevant syntax which allows for easy transition between prolog and R.
Prolog constructs are converted by the library as follows:


* =|..|= within atoms  ->  =|.|= (ex. =| as..integer(c(1,2,3)) ->  as.integer(c(1,2,3))|= )
* =|^[]|=  after atoms -> =|[]|= (ex. =|a^[2] -> a[2] |=)
* =|(.)|= at the end of atoms that are known R functions -> =|()|=  (ex. =|dev..off(.) -> dev.off()|= )
* =|[]|= -> c() (which equal to R's NULL value)
* ( f(x) :-  (..))   -> f(x) (...)
* Lists of lists are converted to matrices. All first level lists must have the same length.
* Filenames must be given as Prolog strings.
* R specific operators (eg. %*% should be quoted in Prolog.
* + prepends strings, for (Prolog) atoms: +'String'
* Expressions that pose difficulty in translation can always be passed as unquoted Prolog atoms or strings.
	]]* since  0:1:2  foo()  is valid syntax:  =|<- dev..off() |= works now (with no need for dev..off(.))
	* since  0:1:2  mat[1] is valid syntax:  =|m[1] <- 4|= works now (with no need for m^[...])
###  Mapping Data betweenn Prolog and R              {#RDataTransfer}

R vectors are mapped to prolog lists and matrices are mapped to nested lists.
The convention works the other way around too.

There are two ways to pass prolog data to R. The more efficient one is by using
~~~~
 Rvar <- PLdata
~~~~

Where Pldata is one of the basic data types (number,boolean) a list or a c/n term.
This transfers via C data between R and Prolog. In what follows atomic PLval data
are simply considered as singleton lists.
Flat Pldata lists are translated to R vectors and lists of one level of nesting to R matrices
(which are 2 dimensional arrays in R parlance). The type of values of the vector or matrice is
taken to be the type of the first data element of the Pldata according to the following :

     * integer -> integer
     * float   -> double
     * atom    -> char
     * boolean -> logical

Booleans are represented in prolog as true/false atoms.
Currently arrays of aribtrary dimensions are not supported in the low-level interface.
Note that in R a scalar is just a one element vector.  When passing non-scalars the
interface will assume the type of the object is that of the first scalar until it encounters
something different.
Real will currently re-start and repopulate partial integers for floats as illustrated
below:

~~~~
r <- [1,2,3].         % pass 1,2,3 to an R vector r
R <- r.               % pass contents of R vector r to Prolog variable R
R = [1, 2, 3].

i <- [1,2,3.1].       % r is now a vector of floats, rather than integers
I <- i.
I = [1.0, 2.0, 3.1].


~~~~

However, not all possible "corrections" are currently supported. For instance,

~~~~
?- c <- [a,b,c,1].
ERROR: real:set_R_variable/2: Type error: `boolean` expected, found `a`
~~~~

In the data passing mode we map Prolog atoms to R strings-

~~~~
?- x <- [abc,def].
true.

?- <- x.
[1] "abc" "def"
true.

?- X <- x.
X = [abc, def].

~~~~

In addition, Prolog data can be passed through the expression mechanism.
That is, data appearing in an arbitrary R expression will be parsed and be part of the long
string that will be passed from Prolog to R for evaluation.
This is only advisable for short data structures. For instance,

~~~~
     tut_4a :-
          state <- c(+"tas", +"sa",  +"qld", +"nsw", +"nsw"),
          <- state.

     tut_4b :-
          state <- c(+tas, +sa,  +qld, +nsw, +nsw),
          <- state.
~~~~

Through this interface it is more convenient to be explicit about R chars by Prolog prepending
atoms or codes with + as in the above example.

<!--- @@} --->
@defgroup RealExamples Examples
<!--- @@ingroup  real --->
<!--- @@{ --->

~~~~

?- e <- numeric(.).
yes
?- e^[3] <- 17.
yes
?- e[3] <- 17.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0]
?- e^[10] <- 12.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0,'$NaN','$NaN','$NaN','$NaN','$NaN','$NaN',12.0]

rtest :-
	y <- rnorm(50),               % get 50 random samples from normal distribution
	<- y,                         % print the values via R
	x <- rnorm(y),                % get an equal number of normal samples
     <- x11(width=5,height=3.5),   % create a plotting window
	<- plot(x,y)                  % plot the two samples
     r_wait,                       % wait for user to hit Enter
	% <- dev..off(.).             % old syntax, still supported
	<- dev.off().                % close the plotting window. foo() now acceptable in supported Prologs

tut6 :-
	d <- outer(0:9, 0:9),
	fr <- table(outer(d, d, "-")),
	<- plot(as..numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency").

tut4b :-
     state <- [tas,sa,qld,nsw,nsw,nt,wa],
     statef <- factor(state),
     incmeans <- tapply( c(60, 49, 40, 61, 64, 60, 59), statef, mean ),
     <- incmeans.

logical :-
     t <- [1,2,3,4,5,1],
     s <- t~~~~1,
     <- s,
     S <- s,
     write( s(S) ), nl.

~~~~


<!--- @@} --->
@defgroup RealInfo Real Information
<!--- @@ingroup  real --->
<!--- @@{ --->


@see		http://stoics.org.uk/~nicos/sware/real
@see		pack(real/examples/for_real)
@see		pack(real/doc/real.html)
@see		pack(real/doc/guide.pdf)
@see		pack(real/doc/padl2013-real.pdf)
@see		http://www.r-project.org/

Also @ref yap-real describes the YAP specfic details in real.

<!--- @@} --->
@defgroup yap_real Development of real in YAP
<!--- @@ingroup  real  --->
<!--- @@{ --->


YAP includes a development version of real, designed to experiment
with the internals of the implementation of R. It includes major
changes and is likely to be much less stable than the version
maintained by Nicos ANgelopoulos. We refer to the version herein as
'realC' and describe the main novelties vs the version described
in~\cite . Their major differences:

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
     arguments,ie. `a[_,"G23"]` would represent the column "G23".

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

<!--- @@} --->

---
 - Nicos Angelopoulos and Vitor Santos Costa, December, 2012.

 - Updates: Nicos Angelopoulos, Dec. 2013, March, 2014

 - Updates: Vitor Santos Costa Dec. 2015

<!--- @@} --->
