

## [The Foreign Code Interface](fli_c_cxx)
 <!--- {#$0} --->

@page  ChYInterface  YAP original C-interface


@defgroup chyinterface YAP original C-interface
@ingroup  fli_c_cxx

Before  describing in full detail how to interface to C code, we will examine
a brief example.

Assume the user requires a predicate `my_process_id(Id)` which succeeds
when  _Id_ unifies with the number of the process under which YAP is running.

In this case we will create a `my_process.c` file containing the
C-code described below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include "YAP/YapInterface.h"

static int my_process_id(void)
{
     YAP_Term pid = YAP_MkIntTerm(getpid());
     YAP_Term out = YAP_ARG1;
     return(YAP_Unify(out,pid));
}

void init_my_predicates()
{
     YAP_UserCPredicate("my_process_id",my_process_id,1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The commands to compile the above file depend on the operating
system.

	
@section CallYAP Using the compiler:


Under Linux you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -c -shared -fPIC my_process.c
      ld -shared -o my_process.so my_process.o
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under WIN32 in a MINGW/CYGWIN environment, using the standard
installation path you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -mno-cygwin  -I "c:/Yap/include" -c my_process.c
      gcc -mno-cygwin "c:/Yap/bin/yap.dll" --shared -o my_process.dll
my_process.o
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under WIN32 in a pure CYGWIN environment, using the standard
installation path, you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -I/usr/local -c my_process.c
      gcc -shared -o my_process.dll my_process.o /usr/local/bin/yap.dll
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


And could be loaded, under YAP, by executing the following Prolog goal

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      load_foreign_files(['my_process'],[],init_my_predicates).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that since YAP4.3.3 you should not give the suffix for object
files. YAP will deduce the correct suffix from the operating system it
is running under.

After loading that file the following Prolog goal

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       my_process_id(N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would unify N with the number of the process under which YAP is running.

Having presented a full example, we will now examine in more detail the
contents of the C source code file presented above.

The include statement is used to make available to the C source code the
macros for the handling of Prolog terms and also some YAP public
definitions.

The function `my_process_id` is the implementation, in C, of the
desired predicate.  Note that it returns an integer denoting the success
of failure of the goal and also that it has no arguments even though the
predicate being defined has one.
In fact the arguments of a Prolog predicate written in C are accessed
through macros, defined in the include file, with names  _YAP_ARG1_,
 _YAP_ARG2_, ...,  _YAP_ARG16_ or with  _YAP_A_( _N_)


where  _N_ is the argument number (starting with 1).  In the present
case the function uses just one local variable of type `YAP_Term`, the
type used for holding YAP terms, where the integer returned by the
standard unix function `getpid()` is stored as an integer term (the
conversion is done by `YAP_MkIntTerm(Int))`. Then it calls the
pre-defined routine `YAP_Unify(YAP_Term, YAP_Term)` which in turn returns an
integer denoting success or failure of the unification.

The role of the procedure `init_my_predicates` is to make known to
YAP, by calling YAP_UserCPredicate(), the predicates being
defined in the file.  This is in fact why, in the example above,
init_my_predicates() was passed as the third argument to
load_foreign_files/3.

The rest of this appendix describes exhaustively how to interface C to YAP.


@section Manipulating_Terms Terms


 <!-- @ingroup ChYInterface
@{ -->

This  section provides information about the primitives available to the C
programmer for manipulating Prolog terms.

Several C typedefs are included in the header file `yap/YAPInterface.h` to
describe, in a portable way, the C representation of Prolog terms.
The user should write is programs using this macros to ensure portability of
code across different versions of YAP.

The more important typedef is  _YAP_Term_ which is used to denote the
type of a Prolog term.

Terms, from a point of view of the C-programmer,  can be classified as
follows

* uninstantiated variables
* instantiated variables
* integers
* floating-point numbers
* database references
* atoms
* pairs (lists)
* compound terms

The primitive

YAP_Bool YAP_IsVarTerm(YAP_Term  _t_)

returns true iff its argument is an uninstantiated variable. Conversely the
primitive

+ YAP_Bool YAP_NonVarTerm(YAP_Term  _t_)

returns true iff its argument is not a variable.




The user can create a new uninstantiated variable using the primitive


+ YAP_Term  YAP_MkVarTerm()



The following primitives can be used to discriminate among the different types
of non-variable terms:


+ YAP_Bool YAP_IsIntTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsFloatTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsDbRefTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsAtomTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsPairTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsApplTerm(YAP_Term  _t_)


+ YAP_Bool YAP_IsCompoundTerm(YAP_Term  _t_)




The next primitive gives the type of a Prolog term:


+ YAP_tag_t YAP_TagOfTerm(YAP_Term  _t_)


The set of possible values is an enumerated type, with the following values:


+ `YAP_TAG_ATT`: an attributed variable

+ `YAP_TAG_UNBOUND`: an unbound variable

+ `YAP_TAG_REF`: a reference to a term

+ `YAP_TAG_PAIR`: a list

+ `YAP_TAG_ATOM`: an atom

+ `YAP_TAG_INT`: a small integer

+ `YAP_TAG_LONG_INT`: a word sized integer

+ `YAP_TAG_BIG_INT`: a very large integer

+ `YAP_TAG_RATIONAL`: a rational number

+ `YAP_TAG_FLOAT`: a floating point number

+ `YAP_TAG_OPAQUE`: an opaque term

+ `YAP_TAG_APPL`: a compound term



Next, we mention the primitives that allow one to destruct and construct
terms. All the above primitives ensure that their result is
a dereferenced, i.e. that it is not a pointer to another term.

The following primitives are provided for creating an integer term from an
integer and to access the value of an integer term.


+ YAP_Term YAP_MkIntTerm(YAP_Int   _i_)


+ YAP_Int  YAP_IntOfTerm(YAP_Term  _t_)



where `YAP_Int` is a typedef for the C integer type appropriate for
the machine or compiler in question (normally a long integer). The size
of the allowed integers is implementation dependent but is always
greater or equal to 24 bits: usually 32 bits on 32 bit machines, and 64
on 64 bit machines.

The two following primitives play a similar role for floating-point terms


+ YAP_Term YAP_MkFloatTerm(YAP_flt  _double_)



+ YAP_flt  YAP_FloatOfTerm(YAP_Term  _t_)



where `flt` is a typedef for the appropriate C floating point type,
nowadays a `double`

The following primitives are provided for verifying whether a term is
a big int, creating a term from a big integer and to access the value
of a big int from a term.


+ YAP_Bool YAP_IsBigNumTerm(YAP_Term  _t_)


+ YAP_Term YAP_MkBigNumTerm(void  \* _b_)


+ void \*YAP_BigNumOfTerm(YAP_Term  _t_, void \* _b_)



YAP must support bignum for the configuration you are using (check the
YAP configuration and setup). For now, YAP only supports the GNU GMP
library, and `void \*` will be a cast for `mpz_t`. Notice
that [YAP_BigNumOfTerm](@ref YAP_BigNumOfTerm) requires the number to be already
initialized. As an example, we show how to print a bignum:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int
p_print_bignum(void)
{
  mpz_t mz;
  if (!YAP_IsBigNumTerm(YAP_ARG1))
    return FALSE;

  mpz_init(mz);
  YAP_BigNumOfTerm(YAP_ARG1, mz);
  gmp_printf("Shows up as %Zd\n", mz);
  mpz_clear(mz);
  return TRUE;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, no primitives are supplied to users for manipulating data base
references.

A special typedef `YAP_Atom` is provided to describe Prolog
\a atoms (symbolic constants). The two following primitives can be used
to manipulate atom terms



+ YAP_Term YAP_MkAtomTerm(YAP_Atom at)


+ YAP_Atom YAP_AtomOfTerm(YAP_Term  _t_)



The following primitives are available for associating atoms with their
names


+ YAP_Atom  YAP_LookupAtom(char \*  _s_)


+ YAP_Atom  YAP_FullLookupAtom(char \*  _s_)


+ char     \*YAP_AtomName(YAP_Atom  _t_)



The function [YAP_LookupAtom](@ref YAP_LookupAtom) looks up an atom in the
standard hash
table. The function [YAP_FullLookupAtom](@ref YAP_FullLookupAtom) will also
search if the
atom had been "hidden": this is useful for system maintenance from C
code. The functor [YAP_AtomName](@ref YAP_AtomName) returns a pointer to the
string
for the atom.

The following primitives handle constructing atoms from strings with
wide characters, and vice-versa:


+ YAP_Atom  YAP_LookupWideAtom(wchar_t \*  _s_)


+ wchar_t  \*YAP_WideAtomName(YAP_Atom  _t_)




The following primitive tells whether an atom needs wide atoms in its
representation:


+ int  YAP_IsWideAtom(YAP_Atom  _t_)




The following primitive can be used to obtain the size of an atom in a
representation-independent way:


+ int      YAP_AtomNameLength(YAP_Atom  _t_)




The next routines give users some control over  the atom
garbage collector. They allow the user to guarantee that an atom is not
to be garbage collected (this is important if the atom is hold
externally to the Prolog engine, allow it to be collected, and call a
hook on garbage collection:


+ int  YAP_AtomGetHold(YAP_Atom  _at_)


+ int  YAP_AtomReleaseHold(YAP_Atom  _at_)


+ int  YAP_AGCRegisterHook(YAP_AGC_hook  _f_)




A \a pair is a Prolog term which consists of a tuple of two Prolog
terms designated as the \a head and the \a tail of the term. Pairs are
most often used to build <em>lists</em>. The following primitives can be
used to manipulate pairs:


+ YAP_Term  YAP_MkPairTerm(YAP_Term  _Head_, YAP_Term  _Tail_)


+ YAP_Term  YAP_MkNewPairTerm(void)


+ YAP_Term  YAP_HeadOfTerm(YAP_Term  _t_)


+ YAP_Term  YAP_TailOfTerm(YAP_Term  _t_)


+ YAP_Term  YAP_MkListFromTerms(YAP_Term \* _pt_, YAP_Int \* _sz_)



One can construct a new pair from two terms, or one can just build a
pair whose head and tail are new unbound variables. Finally, one can
fetch the head or the tail.

The last function supports the common operation of constructing a list from an
array of terms of size  _sz_ in a simple sweep.

Notice that the list constructors can call the garbage collector if
there is not enough space in the global stack.

A \a compound term consists of a \a functor and a sequence of terms with
length equal to the \a arity of the functor. A functor, described in C by
the typedef `Functor`, consists of an atom and of an integer.
The following primitives were designed to manipulate compound terms and
functors


+ YAP_Term     YAP_MkApplTerm(YAP_Functor  _f_, unsigned long int  _n_,
YAP_Term[]  _args_)


+ YAP_Term     YAP_MkNewApplTerm(YAP_Functor  _f_, int  _n_)


+ YAP_Term     YAP_ArgOfTerm(int argno,YAP_Term  _ts_)


+ YAP_Term    \*YAP_ArgsOfTerm(YAP_Term  _ts_)


+ YAP_Functor  YAP_FunctorOfTerm(YAP_Term  _ts_)



The [YAP_MkApplTerm() function constructs a new term, with functor
 _f_ (of arity  _n_), and using an array  _args_ of  _n_
terms with  _n_ equal to the arity of the
functor. YAP_MkNewApplTerm() builds up a compound term whose
arguments are unbound variables. [YAP_ArgOfTerm](@ref YAP_ArgOfTerm) gives an
argument
to a compound term. `argno` should be greater or equal to 1 and
less or equal to the arity of the functor.  [YAP_ArgsOfTerm](@ref
YAP_ArgsOfTerm)
returns a pointer to an array of arguments.

Notice that the compound term constructors can call the garbage
collector if there is not enough space in the global stack.

YAP allows one to manipulate the functors of compound term. The function
[YAP_FunctorOfTerm](@ref YAP_FunctorOfTerm) allows one to obtain a variable of
type
`YAP_Functor` with the functor to a term. The following functions
then allow one to construct functors, and to obtain their name and arity.


+ YAP_Functor  YAP_MkFunctor(YAP_Atom  _a_,unsigned long int  _arity_)

+ YAP_Atom     YAP_NameOfFunctor(YAP_Functor  _f_)

+ YAP_Int      YAP_ArityOfFunctor(YAP_Functor  _f_)



Note that the functor is essentially a pair formed by an atom, and
arity.

Constructing terms in the stack may lead to overflow. The routine


+ int          YAP_RequiresExtraStack(size_t  _min_)



verifies whether you have at least  _min_ cells free in the stack,
and it returns true if it has to ensure enough memory by calling the
garbage collector and or stack shifter. The routine returns false if no
memory is needed, and a negative number if it cannot provide enough
memory.

You can set  _min_ to zero if you do not know how much room you need
but you do know you do not need a big chunk at a single go. Usually, the routine
would usually be called together with a long-jump to restart the
code. Slots can also be used if there is small state.


@subsection  Unifying_Terms Unification

YAP  ---> --->provides a single routine to attempt the unification of two Prolog
terms. The routine may succeed or fail:


+ Int      YAP_Unify(YAP_Term  _a_, YAP_Term  _b_)



The routine attempts to unify the terms  _a_ and
 _b_ returning `TRUE` if the unification succeeds and `FALSE`
otherwise.

@subsection CText String Processing


The YAP C-interface now includes an utility routine to copy a string
represented as a list of a character codes to a previously allocated buffer


+ int YAP_StringToBuffer(YAP_Term  _String_, char \* _buf_, unsigned int
_bufsize_)



The routine copies the list of character codes  _String_ to a
previously allocated buffer  _buf_. The string including a
terminating null character must fit in  _bufsize_ characters,
otherwise the routine will simply fail. The  _StringToBuffer_ routine
fails and generates an exception if  _String_ is not a valid string.

The C-interface also includes utility routines to do the reverse, that
is, to copy a from a buffer to a list of character codes, to a
difference list,  or to a list of
character atoms. The routines work either on strings of characters or
strings of wide characters:


+ YAP_Term YAP_BufferToString(char \* _buf_)

+ YAP_Term YAP_NBufferToString(char \* _buf_, size_t  _len_)

+ YAP_Term YAP_WideBufferToString(wchar_t \* _buf_)

+ YAP_Term YAP_NWideBufferToString(wchar_t \* _buf_, size_t  _len_)

+ YAP_Term YAP_BufferToAtomList(char \* _buf_)

+ YAP_Term YAP_NBufferToAtomList(char \* _buf_, size_t  _len_)

+ YAP_Term YAP_WideBufferToAtomList(wchar_t \* _buf_)

+ YAP_Term YAP_NWideBufferToAtomList(wchar_t \* _buf_, size_t  _len_)



Users are advised to use the  _N_ version of the routines. Otherwise,
the user-provided string must include a terminating null character.

The C-interface function calls the parser on a sequence of characters
stored at  _buf_ and returns the resulting term.


+ YAP_Term YAP_ReadBuffer(char \* _buf_,YAP_Term \* _error_)



The user-provided string must include a terminating null
character. Syntax errors will cause returning `FALSE` and binding
 _error_ to a Prolog term.

These C-interface functions are useful when converting chunks of data to Prolog:


+ YAP_Term YAP_FloatsToList(double \* _buf_,size_t  _sz_)

+ YAP_Term YAP_IntsToList(YAP_Int \* _buf_,size_t  _sz_)



Notice that they are unsafe, and may call the garbage collector. They
return 0 on error.

These C-interface functions are useful when converting Prolog lists to arrays:


+ YAP_Int YAP_IntsToList(YAP_Term t, YAP_Int \* _buf_,size_t  _sz_)

+ YAP_Int YAP_FloatsToList(YAP_Term t, double \* _buf_,size_t  _sz_)



They return the number of integers scanned, up to a maximum of <tt>sz</tt>,
and <tt>-1</tt> on error.

@subsection Memory_Allocation Memory Allocation


The  ---> --->next routine can be used to ask space from the Prolog data-base:


+ void      \*YAP_AllocSpaceFromYAP(int  _size_)



The routine returns a pointer to a buffer allocated from the code area,
or `NULL` if sufficient space was not available.

The space allocated with [YAP_AllocSpaceFromYAP](@ref YAP_AllocSpaceFromYAP) can
be released
back to YAP by using:


+ void      YAP_FreeSpaceFromYAP(void \* _buf_)



The routine releases a buffer allocated from the code area. The system
may crash if `buf` is not a valid pointer to a buffer in the code
area.


@subsection Controlling_Streams Controlling YAP Streams from `C`

The  ---> --->C-Interface also provides the C-application with a measure of
control over the YAP Input/Output system. The first routine allows one
to find a file number given a current stream:


+ int      YAP_StreamToFileNo(YAP_Term  _stream_)



This function gives the file descriptor for a currently available
stream. Note that null streams and in memory streams do not have
corresponding open streams, so the routine will return a
negative. Moreover, YAP will not be aware of any direct operations on
this stream, so information on, say, current stream position, may become
stale.

A second routine that is sometimes useful is:


+ void      YAP_CloseAllOpenStreams(void)



This routine closes the YAP Input/Output system except for the first
three streams, that are always associated with the three standard Unix
streams. It is most useful if you are doing `fork()`.

Last, one may sometimes need to flush all streams:


+ void      YAP_CloseAllOpenStreams(void)



It is also useful before you do a `fork()`, or otherwise you may
have trouble with unflushed output.

The next routine allows a currently open file to become a stream. The
routine receives as arguments a file descriptor, the true file name as a
string, an atom with the user name, and a set of flags:


+ void      YAP_OpenStream(void \* _FD_, char \* _name_, YAP_Term  _t_, int
_flags_)



The available flags are `YAP_INPUT_STREAM`,
`YAP_OUTPUT_STREAM`, `YAP_APPEND_STREAM`,
`YAP_PIPE_STREAM`, `YAP_TTY_STREAM`, `YAP_POPEN_STREAM`,
`YAP_BINARY_STREAM`, and `YAP_SEEKABLE_STREAM`. By default, the
stream is supposed to be at position 0. The argument  _name_ gives
the name by which YAP should know the new stream.


@section Utility_Functions  Utility Functions in `C

The C-Interface  provides the C-application with a a number of utility
functions.

The first provides a way to insert a term into the data-base


+ void      \*YAP_Record(YAP_Term  _t_)



This function returns a pointer to a copy of the term in the database
(or to <tt>NULL</tt> if the operation fails.

The next functions provides a way to recover the term from the data-base:


+ YAP_Term      YAP_Recorded(void \* _handle_)



Notice that the semantics are the same as for recorded/3: this
function creates a new copy of the term in the stack, with fresh
variables. The function returns <tt>0L</tt> if it cannot create a new term.

Last, the next function allows one to recover space:


+ int      YAP_Erase(void \* _handle_)



Notice that any accesses using  _handle_ after this operation may
lead to a crash.

The following functions are often required to compare terms.

Succeed if two terms are actually the same term, as in
==/2:


+ int      YAP_ExactlyEqual(YAP_Term t1, YAP_Term t2)



The next function succeeds if two terms are variant terms, and returns
0 otherwise, as
=@=/2:


+ int      YAP_Variant(YAP_Term t1, YAP_Term t2)



The next functions deal with numbering variables in terms:


+ int      YAP_NumberVars(YAP_Term t, YAP_Int first_number)
+ YAP_Term YAP_UnNumberVars(YAP_Term t)
+ int      YAP_IsNumberedVariable(YAP_Term t)


The next one returns the length of a well-formed list  _t_, or
`-1` otherwise:


+ Int      YAP_ListLength(YAP_Term t)



Last, this function succeeds if two terms are unifiable:
=@=/2:


+ int      YAP_Unifiable(YAP_Term t1, YAP_Term t2)



The second function computes a hash function for a term, as in
`term_hash/4`.


+ YAP_Int    YAP_TermHash(YAP_Term t, YAP_Int range, YAP_Int depth, int
ignore_variables));



The first three arguments follow `term_has/4`. The last argument
indicates what to do if we find a variable: if `0` fail, otherwise
ignore the variable.

@subsection Calling_YAP_From_C From `C` back to Prolog


There  ---> --->are several ways to call Prolog code from C-code. By default, the
`YAP_RunGoal()` should be used for this task. It assumes the engine
has been initialized before:


+ YAP_Int YAP_RunGoal(YAP_Term Goal)


Execute query  _Goal_ and return 1 if the query succeeds, and 0
otherwise. The predicate returns 0 if failure, otherwise it will return
an  _YAP_Term_.

Quite often, one wants to run a query once. In this case you should use
 _Goal_:


+ YAP_Int YAP_RunGoalOnce(YAP_Term Goal)


The  `YAP_RunGoal()` function makes sure to recover stack space at
the end of execution.

Prolog terms are pointers: a problem users often find is that the term
 _Goal_ may actually <em>be moved around</em> during the execution of
`YAP_RunGoal()`, due to garbage collection or stack shifting. If
this is possible,  _Goal_ will become invalid after executing
`YAP_RunGoal()`. In this case, it is a good idea to save  _Goal_
<em>slots</em>, as shown next:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  long sl = YAP_InitSlot(scoreTerm);

  out = YAP_RunGoal(t);
  t = YAP_GetFromSlot(sl);
  YAP_RecoverSlots(1);
  if (out == 0) return FALSE;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following functions complement  _YAP_RunGoal_:

<ul>
+ `int` YAP_RestartGoal(`void`)

Look for the next solution to the current query by forcing YAP to
backtrack to the latest goal. Notice that slots allocated since the last
YAP_RunGoal() will become invalid.

+  `int` YAP_Reset(`yap_reset_t mode`)

Reset execution environment
(similar to the abort/0 built-in). This is useful when
you want to start a new query before asking all solutions to the
previous query. 'mode` specifies how deep the Reset will go and what
to do next. It will be most often set to `YAP_FULL_RESET`.


+ `int` YAP_ShutdownGoal(`int backtrack`)

Clean up the current goal. If
`backtrack` is true, stack space will be recovered and bindings
will be undone. In both cases, any slots allocated since the goal was
created will become invalid.


+ `YAP_Bool` YAP_GoalHasException(`YAP_Term \*tp`)

Check if the last goal generated an exception, and if so copy it to the
space pointed to by  _tp_


+ `void` YAP_ClearExceptions(`void`)

Reset any exceptions left over by the system.



The  YAP_RunGoal() interface is designed to be very robust, but may
not be the most efficient when repeated calls to the same goal are made
and when there is no interest in processing exception. The
 YAP_EnterGoal() interface should have lower-overhead:


+ `YAP_PredEntryPtr` YAP_FunctorToPred(`YAP_Functor`  _f_)
Return the predicate whose main functor is  _f_.


+ `YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_)

Return the arity 0 predicate whose name is  _at_.


+ `YAP_PredEntryPtr`
YAP_FunctorToPredInModule(`YAP_Functor`  _f_, `YAP_Module`  _m_),

Return the predicate in module  _m_ whose main functor is  _f_.


+ `YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_, `YAP_Module`  _m_),

Return the arity 0 predicate in module  _m_ whose name is  _at_.


+ `YAP_Bool` YAP_EnterGoal(`YAP_PredEntryPtr`  _pe_),

`YAP_Term \*`  _array_, `YAP_dogoalinfo \*`  _infop_)
Execute a  query for predicate  _pe_. The query is given as an
array of terms  _Array_.  _infop_ is the address of a goal
handle that can be used to backtrack and to recover space. Succeeds if
a solution was found.

Notice that you cannot create new slots if an YAP_ExnterGoal goal is open.

+ `YAP_Bool` YAP_RetryGoal(`YAP_dogoalinfo \*`  _infop_) @anchor YAP_RetryGoal


Backtrack to a query created by [YAP_EnterGoal](@ref YAP_EnterGoal). The query
is
given by the handle  _infop_. Returns whether a new solution could
be be found.

+ `YAP_Bool` YAP_LeaveGoal(`YAP_Bool`  _backtrack_, `YAP_dogoalinfo \*`  _infop_)
Exit a query query created by [YAP_EnterGoal](@ref YAP_EnterGoal). If
`backtrack` is `TRUE`, variable bindings are undone and Heap
space is recovered.  Otherwise, only stack space is recovered, ie,
`LeaveGoal` executes a cut.


Next, follows an example of how to use [YAP_EnterGoal](@ref YAP_EnterGoal):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
runall(YAP_Term g)
{
    YAP_dogoalinfo goalInfo;
    YAP_Term *goalArgs = YAP_ArraysOfTerm(g);
    YAP_Functor *goalFunctor = YAP_FunctorOfTerm(g);
    YAP_PredEntryPtr goalPred = YAP_FunctorToPred(goalFunctor);

    result = YAP_EnterGoal( goalPred, goalArgs, &goalInfo );
    while (result)
       result = YAP_RetryGoal( &goalInfo );
    YAP_LeaveGoal(TRUE, &goalInfo);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<YAP allows calling a  *new* Prolog interpreter from `C`. One
way is to first construct a goal `G`, and then it is sufficient to
perform:


+ YAP_Bool      YAP_CallProlog(YAP_Term  _G_)


the result will be `FALSE`, if the goal failed, or `TRUE`, if
the goal succeeded. In this case, the variables in  _G_ will store
the values they have been unified with. Execution only proceeds until
finding the first solution to the goal, but you can call
findall/3 or friends if you need all the solutions.


Notice that during execution, garbage collection or stack shifting may
have moved the terms

@subsection CAndModules Modules
YAP allows one to create a new module from C-code. To create the new
code it is sufficient to call:


+ YAP_Module      YAP_CreateModule(YAP_Atom  _ModuleName_)


Notice that the new module does not have any predicates associated and
that it is not the current module. To find the current module, you can call:


+ YAP_Module      YAP_CurrentModule()



Given a module, you may want to obtain the corresponding name. This is
possible by using:


+ YAP_Term      YAP_ModuleName(YAP_Module mod)


Notice that this function returns a term, and not an atom. You can
[YAP_AtomOfTerm](@ref YAP_AtomOfTerm) to extract the corresponding Prolog atom.

@subsection Miscellaneous_ChYFunctions  Miscellaneous C Functions


+ `void` YAP_Throw(`YAP_Term exception`)

+ `void` YAP_AsyncThrow(`YAP_Term exception`) @anchor YAP_Throw


Throw an exception with term   _exception_, just like if you called
`throw/2`. The function <tt>YAP_AsyncThrow</tt> is supposed to be used
from interrupt handlers.



+ `int` YAP_SetYAPFlag(`yap_flag_t flag, int value`) @anchor YAP_SetYAPFlag


This function allows setting some YAP flags from `C` .Currently,
only two boolean flags are accepted: `YAPC_ENABLE_GC` and
`YAPC_ENABLE_AGC`.  The first enables/disables the standard garbage
collector, the second does the same for the atom garbage collector.`


+ `YAP_TERM` YAP_AllocExternalDataInStack(`size_t bytes`)

+ `void \*` YAP_ExternalDataInStackFromTerm(`YAP_Term t`)

+ `YAP_Bool` YAP_IsExternalDataInStackTerm(`YAP_Term t`) 
 @anchor YAP_AllocExternalDataInStack


The next routines allow one to store external data in the Prolog
execution stack. The first routine reserves space for  _sz_ bytes
and returns an opaque handle. The second routines receives the handle
and returns a pointer to the data.  The last routine checks if a term
is an opaque handle.

Data will be automatically reclaimed during
backtracking. Also, this storage is opaque to the Prolog garbage compiler,
so it should not be used to store Prolog terms. On the other hand, it
may be useful to store arrays in a compact way, or pointers to external objects.


+ `int` YAP_HaltRegisterHook(`YAP_halt_hook f, void \*closure`) 
 @anchor YAP_HaltRegisterHook


Register the function  _f_ to be called if YAP is halted. The
function is called with two arguments: the exit code of the process
(`0` if this cannot be determined on your operating system) and
the closure argument  _closure_.



+ `int` YAP_Argv(`char \*\*\*argvp`) @anchor YAP_Argv

Return the number of arguments to YAP and instantiate argvp to point to the list
of such arguments.




@section Writing predicates in C


We will distinguish two kinds of predicates:


+ \a deterministic predicates which either fail or succeed but are not
backtrackable, like the one in the introduction;

+ \a backtrackable
predicates which can succeed more than once.



The first kind of predicates should be implemented as a C function with
no arguments which should return zero if the predicate fails and a
non-zero value otherwise. The predicate should be declared to
YAP, in the initialization routine, with a call to


+ void YAP_UserCPredicate(char \* _name_, YAP_Bool \* _fn_(), unsigned long
int  _arity_);
where  _name_ is a string with the name of the predicate,  _init_,
 _cont_,  _cut_ are the C functions used to start, continue and
when pruning the execution of the predicate,  _arity_ is the
predicate arity, and  _sizeof_ is the size of the data to be
preserved in the stack.

For the second kind of predicates we need three C functions. The first one
is called when the predicate is first activated; the second one
is called on backtracking to provide (possibly) other solutions; the
last one is called on pruning. Note
also that we normally also need to preserve some information to find out
the next solution.

In fact the role of the two functions can be better understood from the
following Prolog definition

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       p :- start.
       p :- repeat,
                continue.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `start` and `continue` correspond to the two C functions
described above.

The interface works as follows:


+ void YAP_UserBackCutCPredicate(char \* _name_, int \* _init_(), int \*
_cont_(), int \* _cut_(), unsigned long int  _arity_, unsigned int  _sizeof_)
@anchor YAP_UserBackCutCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_,  _cont_, and  _cut_ are the C functions that implement
the predicate and  _arity_ is the predicate's arity.


+ void YAP_UserBackCPredicate(char \* _name_, int \* _init_(), int \*
_cont_(), unsigned long int  _arity_, unsigned int  _sizeof_) 
@anchor YAP_UserBackCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_, and  _cont_ are the C functions that implement the
predicate and  _arity_ is the predicate's arity.


+ void YAP_PRESERVE_DATA( _ptr_,  _type_); @anchor YAP_PRESERVE_DATA



+ void YAP_PRESERVED_DATA( _ptr_,  _type_); @anchor YAP_PRESERVED_DATA



+ void YAP_PRESERVED_DATA_CUT( _ptr_,  _type_); 
 @anchor YAP_PRESERVED_DATA_CUT



+ void YAP_cut_succeed( void ); @anchor YAP_cut_succeed



+ void YAP_cut_fail( void ); @anchor YAP_cut_fail





As an example we will consider implementing in C a predicate `n100(N)`
which, when called with an instantiated argument should succeed if that
argument is a numeral less or equal to 100, and, when called with an
uninstantiated argument, should provide, by backtracking, all the positive
integers less or equal to 100.

To do that we first declare a structure, which can only consist
of Prolog terms, containing the information to be preserved on backtracking
and a pointer variable to a structure of that type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include "YAPInterface.h"

static int start_n100(void);
static int continue_n100(void);

typedef struct {
    YAP_Term next_solution;
   } n100_data_type;

n100_data_type *n100_data;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now write the `C` function to handle the first call:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int start_n100(void)
{
      YAP_Term t = YAP_ARG1;
      YAP_PRESERVE_DATA(n100_data,n100_data_type);
      if(YAP_IsVarTerm(t)) {
          n100_data->next_solution = YAP_MkIntTerm(0);
          return continue_n100();
       }
      if(!YAP_IsIntTerm(t) || YAP_IntOfTerm(t)<0 || YAP_IntOfTerm(t)>100) {
          YAP_cut_fail();
      } else {
          YAP_cut_succeed();
      }
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The routine starts by getting the dereference value of the argument.
The call to [YAP_PRESERVE_DATA](@ref YAP_PRESERVE_DATA) is used to initialize
the memory
which will hold the information to be preserved across
backtracking. The first argument is the variable we shall use, and the
second its type. Note that we can only use [YAP_PRESERVE_DATA](@ref
YAP_PRESERVE_DATA)
once, so often we will want the variable to be a structure. This data
is visible to the garbage collector, so it should consist of Prolog
terms, as in the example. It is also correct to store pointers to
objects external to YAP stacks, as the garbage collector will ignore
such references.

If the argument of the predicate is a variable, the routine initializes the
structure to be preserved across backtracking with the information
required to provide the next solution, and exits by calling
`continue_n100` to provide that solution.

If the argument was not a variable, the routine then checks if it was an
integer, and if so, if its value is positive and less than 100. In that
case it exits, denoting success, with [YAP_cut_succeed](@ref YAP_cut_succeed),
or
otherwise exits with [YAP_cut_fail](@ref YAP_cut_fail) denoting failure.

The reason for using for using the functions [YAP_cut_succeed](@ref
YAP_cut_succeed) and
[YAP_cut_fail](@ref YAP_cut_fail) instead of just returning a non-zero value in
the
first case, and zero in the second case, is that otherwise, if
backtracking occurred later, the routine `continue_n100` would be
called to provide additional solutions.

The code required for the second function is

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int continue_n100(void)
{
      int n;
      YAP_Term t;
      YAP_Term sol = YAP_ARG1;
      YAP_PRESERVED_DATA(n100_data,n100_data_type);
      n = YAP_IntOfTerm(n100_data->next_solution);
      if( n == 100) {
           t = YAP_MkIntTerm(n);
           YAP_Unify(sol,t);
           YAP_cut_succeed();
        }
       else {
           YAP_Unify(sol,n100_data->next_solution);
           n100_data->next_solution = YAP_MkIntTerm(n+1);
           return(TRUE);
        }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that again the macro [YAP_PRESERVED_DATA](@ref YAP_PRESERVED_DATA) is used
at the
beginning of the function to access the data preserved from the previous
solution.  Then it checks if the last solution was found and in that
case exits with [YAP_cut_succeed](@ref YAP_cut_succeed) in order to cut any
further
backtracking.  If this is not the last solution then we save the value
for the next solution in the data structure and exit normally with 1
denoting success. Note also that in any of the two cases we use the
function `YAP_unify` to bind the argument of the call to the value
saved in ` n100_state-\>next_solution`.

Note also that the only correct way to signal failure in a backtrackable
predicate is to use the [YAP_cut_fail](@ref YAP_cut_fail) macro.

Backtrackable predicates should be declared to YAP, in a way
similar to what happened with deterministic ones, but using instead a
call to

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this example, we would have something like

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
init_n100(void)
{
  YAP_UserBackCutCPredicate("n100", start_n100, continue_n100, cut_n100, 1, 1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The argument before last is the predicate's arity. Notice again the
last argument to the call. function argument gives the extra space we
want to use for `PRESERVED_DATA`. Space is given in cells, where
a cell is the same size as a pointer. The garbage collector has access
to this space, hence users should use it either to store terms or to
store pointers to objects outside the stacks.

The code for `cut_n100` could be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static int cut_n100(void)
{
  YAP_PRESERVED_DATA_CUT(n100_data,n100_data_type*);

  fprintf("n100 cut with counter %ld\n",
YAP_IntOfTerm(n100_data->next_solution));
  return TRUE;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we have to use [YAP_PRESERVED_DATA_CUT](@ref
YAP_PRESERVED_DATA_CUT): this is
because the Prolog engine is at a different state during cut.

If no work is required at cut, we can use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void
init_n100(void)
{
  YAP_UserBackCutCPredicate("n100", start_n100, continue_n100, NULL, 1, 1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
in this case no code is executed at cut time.

@subsection YAP4_Notes Changes to the C-Interface in YAP4


<!-- @ingroup ChYInterface
@{ -->

YAP4  includes several changes over the previous `load_foreign_files/3`
interface. These changes were required to support the new binary code
formats, such as ELF used in Solaris2 and Linux.

   * All Names of YAP objects now start with  _YAP__. This is
      designed to avoid clashes with other code. Use `YAPInterface.h` to
      take advantage of the new interface. `c_interface.h` is still
      available if you cannot port the code to the new interface.

   * Access to elements in the new interface always goes through
    <em>functions</em>. This includes access to the argument registers,
    `YAP_ARG1` to `YAP_ARG16`. This change breaks code such as
    `unify(\&ARG1,\&t)`, which is nowadays:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
   YAP_Unify(ARG1, t);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   * `cut_fail()` and `cut_succeed()` are now functions.

   * The use of `Deref` is deprecated. All functions that return
Prolog terms, including the ones that access arguments, already
dereference their arguments.

   * Space allocated with PRESERVE_DATA is ignored by garbage
collection and stack shifting. As a result, any pointers to a Prolog
stack object, including some terms, may be corrupted after garbage
collection or stack shifting. Prolog terms should instead be stored as
arguments to the backtrackable procedure.



								   

@section YAPAsLibrary Using YAP as a Library

YAP  can be used as a library to be called from other
programs. To do so, you must first create the YAP library:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make library
make install_library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This will install a file `libyap.a` in  _LIBDIR_ and the Prolog
headers in  _INCLUDEDIR_. The library contains all the functionality
available in YAP, except the foreign function loader and for
`YAP`'s startup routines.

To actually use this library you must follow a five step process:


You must initialize the YAP environment. A single function,
`YAP_FastInit` asks for a contiguous chunk in your memory space, fills
it in with the data-base, and sets up YAP's stacks and
execution registers. You can use a saved space from a standard system by
calling save_program/1.

   * You then have to prepare a query to give to
YAP. A query is a Prolog term, and you just have to use the same
functions that are available in the C-interface.

   * You can then use `YAP_RunGoal(query)` to actually evaluate your
query. The argument is the query term `query`, and the result is 1
if the query succeeded, and 0 if it failed.

   * You can use the term destructor functions to check how
arguments were instantiated.

   * If you want extra solutions, you can use
`YAP_RestartGoal()` to obtain the next solution.


The next program shows how to use this system. We assume the saved
program contains two facts for the procedure <tt>b</tt>:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include "YAP/YAPInterface.h"
#include <stdio.h>

int
main(int argc, char *argv[]) {
  if (YAP_FastInit("saved_state") == YAP_BOOT_ERROR)
    exit(1);
  if (YAP_RunGoal(YAP_MkAtomTerm(YAP_LookupAtom("do")))) {
    printf("Success\n");
    while (YAP_RestartGoal())
      printf("Success\n");
  }
  printf("NO\n");
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program first initializes YAP, calls the query for the
first time and succeeds, and then backtracks twice. The first time
backtracking succeeds, the second it fails and exits.

To compile this program it should be sufficient to do:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cc -o exem -I../YAP4.3.0 test.c -lYAP -lreadline -lm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may need to adjust the libraries and library paths depending on the
Operating System and your installation of YAP.

Note that YAP4.3.0 provides the first version of the interface. The
interface may change and improve in the future.

The following C-functions are available from YAP:

   * YAP_CompileClause(`YAP_Term`  _Clause_)
Compile the Prolog term  _Clause_ and assert it as the last clause
for the corresponding procedure.

   * YAP_MkExo(`YAP_PredEntryPtr` _pred_, `size_t` _sz_, `void *` _uid_)
Predicate _pred_ is an exo-predicate that needs _sz_ bytes of
contiguous storage. If _uid_ is non-null associate user-defined
code with _pred_.

   * YAP_AssertTuples(`YAP_PredEntryPtr` pred, `const YAP_Term *`  _Facts_,
`size_t` nb)
Add the array of _nb_ Prolog term `Facts` to the table
`Predicate`.

   * `int` YAP_ContinueGoal(`void`)
Continue execution from the point where it stopped.

   * `void` YAP_Error(`int`  _ID_,`YAP_Term`  _Cause_,`char \*`
_error_description_)
Generate an YAP System Error with description given by the string
 _error_description_.  _ID_ is the error ID, if known, or
`0`.  _Cause_ is the term that caused the crash.

   * `void` YAP_Exit(`int`  _exit_code_)
Exit YAP immediately. The argument  _exit_code_ gives the error code
and is supposed to be 0 after successful execution in Unix and Unix-like
systems.

   * `YAP_Term` YAP_GetValue(`Atom`  _at_)
Return the term  _value_ associated with the atom  _at_. If no
such term exists the function will return the empty list.

   * YAP_FastInit(`char \*`  _SavedState_)
Initialize a copy of YAP from  _SavedState_. The copy is
monolithic and currently must be loaded at the same address where it was
saved. `YAP_FastInit` is a simpler version of `YAP_Init`.

   * YAP_Init( _InitInfo_)
Initialize YAP. The arguments are in a `C`
structure of type `YAP_init_args`.

The fields of  _InitInfo_ are `char \*`  _SavedState_,
`int`  _HeapSize_, `int`  _StackSize_, `int`
 _TrailSize_, `int`  _NumberofWorkers_, `int`
 _SchedulerLoop_, `int`  _DelayedReleaseLoad_, `int`
 _argc_, `char \*\*`  _argv_, `int`  _ErrorNo_, and
`char \*`  _ErrorCause_. The function returns an integer, which
indicates the current status. If the result is `YAP_BOOT_ERROR`
booting failed.

If  _SavedState_ is not NULL, try to open and restore the file
 _SavedState_. Initially YAP will search in the current directory. If
the saved state does not exist in the current directory YAP will use
either the default library directory or the directory given by the
environment variable YAPLIBDIR. Note that currently
the saved state must be loaded at the same address where it was saved.

If  _HeapSize_ is different from 0 use  _HeapSize_ as the minimum
size of the Heap (or code space). If  _StackSize_ is different from 0
use  _HeapSize_ as the minimum size for the Stacks. If
 _TrailSize_ is different from 0 use  _TrailSize_ as the minimum
size for the Trails.

The  _NumberofWorkers_,  _NumberofWorkers_, and
 _DelayedReleaseLoad_ are only of interest to the or-parallel system.

The argument count  _argc_ and string of arguments  _argv_
arguments are to be passed to user programs as the arguments used to
call YAP.

If booting failed you may consult `ErrorNo` and `ErrorCause`
for the cause of the error, or call
`YAP_Error(ErrorNo,0L,ErrorCause)` to do default processing.

- `void` YAP_PutValue(`Atom`  _at_, `YAP_Term`  _value_)
Associate the term  _value_ with the atom  _at_. The term
 _value_ must be a constant. This functionality is used by YAP as a
simple way for controlling and communicating with the Prolog run-time.

- `YAP_Term` YAP_Read(`IOSTREAM \*Stream`)
Parse a  _Term_ from the stream  _Stream_.

- `YAP_Term` YAP_Write(`YAP_Term`  _t_)
Copy a Term  _t_ and all associated constraints. May call the garbage
collector and returns `0L` on error (such as no space being
available).

- `void` YAP_Write(`YAP_Term`  _t_, `IOSTREAM`  _stream_, `int`  _flags_)
Write a Term  _t_ using the stream  _stream_ to output
characters. The term is written according to a mask of the following
flags in the `flag` argument: `YAP_WRITE_QUOTED`,
`YAP_WRITE_HANDLE_VARS`, `YAP_WRITE_USE_PORTRAY`,  and `YAP_WRITE_IGNORE_OPS`.

- `int` YAP_WriteBuffer(`YAP_Term`  _t_, `char \*`  _buff_, `size_t`
_size_, `int`  _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The term is written
according to a mask of the following flags in the `flag`
argument: `YAP_WRITE_QUOTED`, `YAP_WRITE_HANDLE_VARS`,
`YAP_WRITE_USE_PORTRAY`, and `YAP_WRITE_IGNORE_OPS`. The
function will fail if it does not have enough space in the buffer.

- `char \*` YAP_WriteDynamicBuffer(`YAP_Term`  _t_, `char \*`  _buff_,
`size_t`  _size_, `size_t`  _\*lengthp_, `size_t`  _\*encodingp_, `int` _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The code will allocate an extra buffer if  _buff_ is
`NULL` or if `buffer` does not have enough room. The
variable `lengthp` is assigned the size of the resulting buffer,
and `encodingp` will receive the type of encoding (currently only
`PL_ENC_ISO_LATIN_1` and `PL_ENC_WCHAR` are supported)

- `void` YAP_InitConsult(`int`  _mode_, `char \*`  _filename_)
Enter consult mode on file  _filename_. This mode maintains a few
data-structures internally, for instance to know whether a predicate
before or not. It is still possible to execute goals in consult mode.

If  _mode_ is `TRUE` the file will be reconsulted, otherwise
just consulted. In practice, this function is most useful for
bootstrapping Prolog, as otherwise one may call the Prolog predicate
compile/1 or consult/1 to do compilation.

Note that it is up to the user to open the file  _filename_. The
`YAP_InitConsult` function only uses the file name for internal
bookkeeping.

- `void` YAP_EndConsult(`void`)
Finish consult mode.



Some observations:

* The system will core dump if you try to load the saved state in a
different address from where it was made. This may be a problem if
your program uses `mmap`. This problem will be addressed in future
versions of YAP.

* Currently, the YAP library will pollute the name
space for your program.

* The initial library includes the complete YAP system. In
the future we plan to split this library into several smaller libraries
(e.g. if you do not want to perform Input/Output).

* You can generate your own saved states. Look at  the
`boot.yap` and `init.yap` files.

