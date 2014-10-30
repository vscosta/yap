/*************************************************************************
*									 *
*	 YAP Prolog 	@(#)c_interface.h	2.2			 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapInterface.h						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	c_interface header file for YAP				 *
*									 *
*************************************************************************/

/**

@file YapInterface.h

*/


#ifndef _yap_c_interface_h

#define _yap_c_interface_h 1

#define __YAP_PROLOG__ 1

#ifndef YAPVERSION
#define YAPVERSION 60000   //> default versison
#endif

#include "YapDefs.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#include <wchar.h>

/**

@defgroup c-interface  YAP original C-interface
@Ingroup ChYInterface

Before describing in full detail how to interface to C code, we will examine 
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

@{

*/

/*
   __BEGIN_DECLS should be used at the beginning of the C declarations,
   so that C++ compilers don't mangle their names.  __END_DECLS is used
   at the end of C declarations.
*/
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif /* _cplusplus */

__BEGIN_DECLS

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

__END_DECLS

/**
 *
 * Using the compiler:

Under Linux you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -c -shared -fPIC my_process.c
      ld -shared -o my_process.so my_process.o
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under WIN32 in a MINGW/CYGWIN environment, using the standard
installation path you should use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gcc -mno-cygwin  -I "c:/Yap/include" -c my_process.c
      gcc -mno-cygwin "c:/Yap/bin/yap.dll" --shared -o my_process.dll my_process.o
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

This section provides information about the primitives available to the C
programmer for manipulating Prolog terms.

Several C typedefs are included in the header file `yap/YAPInterface.h` to
describe, in a portable way, the C representation of Prolog terms.
The user should write is programs using this macros to ensure portability of
code across different versions of YAP.

The more important typedef is  _YAP_Term_ which is used to denote the
type of a Prolog term.

Terms, from a point of view of the C-programmer,  can be classified as
follows

 + uninstantiated variables
 + instantiated variables
 + integers
 + floating-point numbers
 + database references
 + atoms
 + pairs (lists)
 + compound terms

The primitive

YAP_Bool YAP_IsVarTerm(YAP_Term  _t_) 

returns true iff its argument is an uninstantiated variable. Conversely the
primitive
</li>
 <li>YAP_Bool YAP_NonVarTerm(YAP_Term  _t_) 

returns true iff its argument is not a variable.
</li>
</ul>
 

The user can create a new uninstantiated variable using the primitive

<ul>
 <li>YAP_Term  YAP_MkVarTerm()
</li>
</ul>

The following primitives can be used to discriminate among the different types
of non-variable terms:

<ul>
 <li>YAP_Bool YAP_IsIntTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsFloatTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsDbRefTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsAtomTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsPairTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsApplTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Bool YAP_IsCompoundTerm(YAP_Term  _t_) 

</li>
</ul>

The next primitive gives the type of a Prolog term:

<ul>
 <li>YAP_tag_t YAP_TagOfTerm(YAP_Term  _t_)
</li>
</ul>
The set of possible values is an enumerated type, with the following values:

<ul>
 <li>`YAP_TAG_ATT`: an attributed variable
</li>
 <li>`YAP_TAG_UNBOUND`: an unbound variable
</li>
 <li>`YAP_TAG_REF`: a reference to a term
</li>
 <li>`YAP_TAG_PAIR`: a list
</li>
 <li>`YAP_TAG_ATOM`: an atom
</li>
 <li>`YAP_TAG_INT`: a small integer
</li>
 <li>`YAP_TAG_LONG_INT`: a word sized integer
</li>
 <li>`YAP_TAG_BIG_INT`: a very large integer
</li>
 <li>`YAP_TAG_RATIONAL`: a rational number
</li>
 <li>`YAP_TAG_FLOAT`: a floating point number
</li>
 <li>`YAP_TAG_OPAQUE`: an opaque term
</li>
 <li>`YAP_TAG_APPL`: a compound term
</li>
</ul>

Next, we mention the primitives that allow one to destruct and construct
terms. All the above primitives ensure that their result is
\a dereferenced, i.e. that it is not a pointer to another term.

The following primitives are provided for creating an integer term from an
integer and to access the value of an integer term.

<ul>
 <li>YAP_Term YAP_MkIntTerm(YAP_Int   _i_) 

</li>
 <li>YAP_Int  YAP_IntOfTerm(YAP_Term  _t_) 

</li>
</ul>
where `YAP_Int` is a typedef for the C integer type appropriate for
the machine or compiler in question (normally a long integer). The size
of the allowed integers is implementation dependent but is always
greater or equal to 24 bits: usually 32 bits on 32 bit machines, and 64
on 64 bit machines.

The two following primitives play a similar role for floating-point terms

<ul>
 <li>YAP_Term YAP_MkFloatTerm(YAP_flt  _double_) 


</li>
 <li>YAP_flt  YAP_FloatOfTerm(YAP_Term  _t_) 

</li>
</ul>
where `flt` is a typedef for the appropriate C floating point type,
nowadays a `double`

The following primitives are provided for verifying whether a term is
a big int, creating a term from a big integer and to access the value
of a big int from a term.

<ul>
 <li>YAP_Bool YAP_IsBigNumTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Term YAP_MkBigNumTerm(void  \* _b_) 

</li>
 <li>void \*YAP_BigNumOfTerm(YAP_Term  _t_, void \* _b_) 

</li>
</ul>
YAP must support bignum for the configuration you are using (check the
YAP configuration and setup). For now, YAP only supports the GNU GMP
library, and `void \*` will be a cast for `mpz_t`. Notice
that [YAP_BigNumOfTerm](@ref YAP_BigNumOfTerm) requires the number to be already
initialised. As an example, we show how to print a bignum:

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


<ul>
 <li>YAP_Term YAP_MkAtomTerm(YAP_Atom at) 

</li>
 <li>YAP_Atom YAP_AtomOfTerm(YAP_Term  _t_) 

</li>
</ul>
The following primitives are available for associating atoms with their
names 

<ul>
 <li>YAP_Atom  YAP_LookupAtom(char \*  _s_) 

</li>
 <li>YAP_Atom  YAP_FullLookupAtom(char \*  _s_) 

</li>
 <li>char     \*YAP_AtomName(YAP_Atom  _t_) 

</li>
</ul>
The function [YAP_LookupAtom](@ref YAP_LookupAtom) looks up an atom in the standard hash
table. The function [YAP_FullLookupAtom](@ref YAP_FullLookupAtom) will also search if the
atom had been "hidden": this is useful for system maintenance from C
code. The functor [YAP_AtomName](@ref YAP_AtomName) returns a pointer to the string
for the atom.

The following primitives handle constructing atoms from strings with
wide characters, and vice-versa:

<ul>
 <li>YAP_Atom  YAP_LookupWideAtom(wchar_t \*  _s_) 

</li>
 <li>wchar_t  \*YAP_WideAtomName(YAP_Atom  _t_) 

</li>
</ul>

The following primitive tells whether an atom needs wide atoms in its
representation:

<ul>
 <li>int  YAP_IsWideAtom(YAP_Atom  _t_) 

</li>
</ul>

The following primitive can be used to obtain the size of an atom in a
representation-independent way: 

<ul>
 <li>int      YAP_AtomNameLength(YAP_Atom  _t_) 

</li>
</ul>

The next routines give users some control over  the atom
garbage collector. They allow the user to guarantee that an atom is not
to be garbage collected (this is important if the atom is hold
externally to the Prolog engine, allow it to be collected, and call a
hook on garbage collection:

<ul>
 <li>int  YAP_AtomGetHold(YAP_Atom  _at_) 

</li>
 <li>int  YAP_AtomReleaseHold(YAP_Atom  _at_) 

</li>
 <li>int  YAP_AGCRegisterHook(YAP_AGC_hook  _f_) 

</li>
</ul>

A \a pair is a Prolog term which consists of a tuple of two Prolog
terms designated as the \a head and the \a tail of the term. Pairs are
most often used to build <em>lists</em>. The following primitives can be
used to manipulate pairs:

<ul>
 <li>YAP_Term  YAP_MkPairTerm(YAP_Term  _Head_, YAP_Term  _Tail_) 

</li>
 <li>YAP_Term  YAP_MkNewPairTerm(void) 

</li>
 <li>YAP_Term  YAP_HeadOfTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Term  YAP_TailOfTerm(YAP_Term  _t_) 

</li>
 <li>YAP_Term  YAP_MkListFromTerms(YAP_Term \* _pt_, YAP_Int \* _sz_) 

</li>
</ul>
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

<ul>
 <li>YAP_Term     YAP_MkApplTerm(YAP_Functor  _f_, unsigned long int  _n_, YAP_Term[]  _args_) 

</li>
 <li>YAP_Term     YAP_MkNewApplTerm(YAP_Functor  _f_, int  _n_) 

</li>
 <li>YAP_Term     YAP_ArgOfTerm(int argno,YAP_Term  _ts_) 

</li>
 <li>YAP_Term    \*YAP_ArgsOfTerm(YAP_Term  _ts_) 

</li>
 <li>YAP_Functor  YAP_FunctorOfTerm(YAP_Term  _ts_) 

</li>
</ul>
The [YAP_MkApplTerm() function constructs a new term, with functor
 _f_ (of arity  _n_), and using an array  _args_ of  _n_
terms with  _n_ equal to the arity of the
functor. YAP_MkNewApplTerm() builds up a compound term whose
arguments are unbound variables. [YAP_ArgOfTerm](@ref YAP_ArgOfTerm) gives an argument
to a compound term. `argno` should be greater or equal to 1 and
less or equal to the arity of the functor.  [YAP_ArgsOfTerm](@ref YAP_ArgsOfTerm)
returns a pointer to an array of arguments.

Notice that the compound term constructors can call the garbage
collector if there is not enough space in the global stack.

YAP allows one to manipulate the functors of compound term. The function
[YAP_FunctorOfTerm](@ref YAP_FunctorOfTerm) allows one to obtain a variable of type
`YAP_Functor` with the functor to a term. The following functions
then allow one to construct functors, and to obtain their name and arity. 

<ul>
 <li>YAP_Functor  YAP_MkFunctor(YAP_Atom  _a_,unsigned long int  _arity_)
</li>
 <li>YAP_Atom     YAP_NameOfFunctor(YAP_Functor  _f_)
</li>
 <li>YAP_Int      YAP_ArityOfFunctor(YAP_Functor  _f_)
</li>
</ul>

Note that the functor is essentially a pair formed by an atom, and
arity.

Constructing terms in the stack may lead to overflow. The routine

<ul>
 <li>int          YAP_RequiresExtraStack(size_t  _min_) 

</li>
</ul>
verifies whether you have at least  _min_ cells free in the stack,
and it returns true if it has to ensure enough memory by calling the
garbage collector and or stack shifter. The routine returns false if no
memory is needed, and a negative number if it cannot provide enough
memory.

You can set  _min_ to zero if you do not know how much room you need
but you do know you do not need a big chunk at a single go. Usually, the routine
would usually be called together with a long-jump to restart the
code. Slots can also be used if there is small state.

@section Unifying_Terms Unification

YAP provides a single routine to attempt the unification of two Prolog
terms. The routine may succeed or fail:

<ul>
 <li>Int      YAP_Unify(YAP_Term  _a_, YAP_Term  _b_) 

</li>
</ul>
The routine attempts to unify the terms  _a_ and
 _b_ returning `TRUE` if the unification succeeds and `FALSE`
otherwise.

@section Manipulating_Strings Strings

The YAP C-interface now includes an utility routine to copy a string
represented as a list of a character codes to a previously allocated buffer

<ul>
 <li>int YAP_StringToBuffer(YAP_Term  _String_, char \* _buf_, unsigned int  _bufsize_) 

</li>
</ul>
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

<ul>
 <li>YAP_Term YAP_BufferToString(char \* _buf_)
</li>
 <li>YAP_Term YAP_NBufferToString(char \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_WideBufferToString(wchar_t \* _buf_)
</li>
 <li>YAP_Term YAP_NWideBufferToString(wchar_t \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_BufferToAtomList(char \* _buf_)
</li>
 <li>YAP_Term YAP_NBufferToAtomList(char \* _buf_, size_t  _len_)
</li>
 <li>YAP_Term YAP_WideBufferToAtomList(wchar_t \* _buf_)
</li>
 <li>YAP_Term YAP_NWideBufferToAtomList(wchar_t \* _buf_, size_t  _len_) 

</li>
</ul>
Users are advised to use the  _N_ version of the routines. Otherwise,
the user-provided string must include a terminating null character.

The C-interface function calls the parser on a sequence of characters
stored at  _buf_ and returns the resulting term.

<ul>
 <li>YAP_Term YAP_ReadBuffer(char \* _buf_,YAP_Term \* _error_) 

</li>
</ul>
The user-provided string must include a terminating null
character. Syntax errors will cause returning `FALSE` and binding
 _error_ to a Prolog term.

These C-interface functions are useful when converting chunks of data to Prolog:

<ul>
 <li>YAP_Term YAP_FloatsToList(double \* _buf_,size_t  _sz_)
</li>
 <li>YAP_Term YAP_IntsToList(YAP_Int \* _buf_,size_t  _sz_) 

</li>
</ul>
Notice that they are unsafe, and may call the garbage collector. They
return 0 on error.

These C-interface functions are useful when converting Prolog lists to arrays:

<ul>
 <li>YAP_Int YAP_IntsToList(YAP_Term t, YAP_Int \* _buf_,size_t  _sz_)
</li>
 <li>YAP_Int YAP_FloatsToList(YAP_Term t, double \* _buf_,size_t  _sz_) 

</li>
</ul>
They return the number of integers scanned, up to a maximum of <tt>sz</tt>,
and <tt>-1</tt> on error.

@section Memory_Allocation Memory Allocation

The next routine can be used to ask space from the Prolog data-base:

<ul>
 <li>void      \*YAP_AllocSpaceFromYAP(int  _size_) 

</li>
</ul>
The routine returns a pointer to a buffer allocated from the code area,
or `NULL` if sufficient space was not available.

The space allocated with [YAP_AllocSpaceFromYAP](@ref YAP_AllocSpaceFromYAP) can be released
back to YAP by using:

<ul>
 <li>void      YAP_FreeSpaceFromYAP(void \* _buf_) 

</li>
</ul>
The routine releases a buffer allocated from the code area. The system
may crash if `buf` is not a valid pointer to a buffer in the code
area.

@section Controlling_Streams Controlling YAP Streams from `C`

The C-Interface also provides the C-application with a measure of
control over the YAP Input/Output system. The first routine allows one
to find a file number given a current stream:

<ul>
 <li>int      YAP_StreamToFileNo(YAP_Term  _stream_) 

</li>
</ul>
This function gives the file descriptor for a currently available
stream. Note that null streams and in memory streams do not have
corresponding open streams, so the routine will return a
negative. Moreover, YAP will not be aware of any direct operations on
this stream, so information on, say, current stream position, may become
stale.

A second routine that is sometimes useful is:

<ul>
 <li>void      YAP_CloseAllOpenStreams(void) 

</li>
</ul>
This routine closes the YAP Input/Output system except for the first
three streams, that are always associated with the three standard Unix
streams. It is most useful if you are doing `fork()`.

Last, one may sometimes need to flush all streams:

<ul>
 <li>void      YAP_CloseAllOpenStreams(void) 

</li>
</ul>
It is also useful before you do a `fork()`, or otherwise you may
have trouble with unflushed output.

The next routine allows a currently open file to become a stream. The
routine receives as arguments a file descriptor, the true file name as a
string, an atom with the user name, and a set of flags:

<ul>
 <li>void      YAP_OpenStream(void \* _FD_, char \* _name_, YAP_Term  _t_, int  _flags_) 

</li>
</ul>
The available flags are `YAP_INPUT_STREAM`,
`YAP_OUTPUT_STREAM`, `YAP_APPEND_STREAM`,
`YAP_PIPE_STREAM`, `YAP_TTY_STREAM`, `YAP_POPEN_STREAM`,
`YAP_BINARY_STREAM`, and `YAP_SEEKABLE_STREAM`. By default, the
stream is supposed to be at position 0. The argument  _name_ gives
the name by which YAP should know the new stream.

@section Utility_Functions Utility Functions in `C`

The C-Interface  provides the C-application with a a number of utility
functions that are useful.

The first provides a way to insert a term into the data-base

<ul>
 <li>void      \*YAP_Record(YAP_Term  _t_) 

</li>
</ul>
This function returns a pointer to a copy of the term in the database
(or to <tt>NULL</tt> if the operation fails.

The next functions provides a way to recover the term from the data-base:

<ul>
 <li>YAP_Term      YAP_Recorded(void \* _handle_) 

</li>
</ul>
Notice that the semantics are the same as for [recorded/3](@ref recorded): this
function creates a new copy of the term in the stack, with fresh
variables. The function returns <tt>0L</tt> if it cannot create a new term.

Last, the next function allows one to recover space:

<ul>
 <li>int      YAP_Erase(void \* _handle_) 

</li>
</ul>
Notice that any accesses using  _handle_ after this operation may
lead to a crash.

The following functions are often required to compare terms.

Succeed if two terms are actually the same term, as in
[==/2](@ref qQqQ):

<ul>
 <li>int      YAP_ExactlyEqual(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The next function succeeds if two terms are variant terms, and returns
0 otherwise, as
[=@=/2](@ref qQaAqQ):

<ul>
 <li>int      YAP_Variant(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The next functions deal with numbering variables in terms:

<ul>
 <li>int      YAP_NumberVars(YAP_Term t, YAP_Int first_number)
</li>
 <li>YAP_Term YAP_UnNumberVars(YAP_Term t)
</li>
 <li>int      YAP_IsNumberedVariable(YAP_Term t)
</li>
</ul>

The next one returns the length of a well-formed list  _t_, or
`-1` otherwise:

<ul>
 <li>Int      YAP_ListLength(YAP_Term t)
</li>
</ul>

Last, this function succeeds if two terms are unifiable:
[=@=/2](@ref qQaAqQ):

<ul>
 <li>int      YAP_Unifiable(YAP_Term t1, YAP_Term t2)
</li>
</ul>

The second function computes a hash function for a term, as in
`term_hash/4`.

<ul>
 <li>YAP_Int    YAP_TermHash(YAP_Term t, YAP_Int range, YAP_Int depth, int  ignore_variables)); 

</li>
</ul>
The first three arguments follow `term_has/4`. The last argument
indicates what to do if we find a variable: if `0` fail, otherwise
ignore the variable. 

@section Calling_YAP_From_C From `C` back to Prolog

There are several ways to call Prolog code from C-code. By default, the
`YAP_RunGoal()` should be used for this task. It assumes the engine
has been initialised before:

<ul>
 <li>YAP_Int YAP_RunGoal(YAP_Term Goal)
</li>
</ul>
Execute query  _Goal_ and return 1 if the query succeeds, and 0
otherwise. The predicate returns 0 if failure, otherwise it will return
an  _YAP_Term_. 

Quite often, one wants to run a query once. In this case you should use
 _Goal_:

<ul>
 <li>YAP_Int YAP_RunGoalOnce(YAP_Term Goal)
</li>
</ul>
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

@copydoc real

The following functions complement  _YAP_RunGoal_:

<ul>
 <li>`int` YAP_RestartGoal(`void`)

Look for the next solution to the current query by forcing YAP to
backtrack to the latest goal. Notice that slots allocated since the last
YAP_RunGoal() will become invalid.

@Item `int` YAP_Reset(`yap_reset_t mode`) 

Reset execution environment
(similar to the abort/0 built-in). This is useful when
you want to start a new query before asking all solutions to the
previous query. 'mode` specifies how deep the Reset will go and what
to do next. It will be most often set to `YAP_FULL_RESET`.

</li>
 <li>`int` YAP_ShutdownGoal(`int backtrack`)

Clean up the current goal. If
`backtrack` is true, stack space will be recovered and bindings
will be undone. In both cases, any slots allocated since the goal was
created will become invalid.

</li>
 <li>`YAP_Bool` YAP_GoalHasException(`YAP_Term \*tp`)

Check if the last goal generated an exception, and if so copy it to the
space pointed to by  _tp_

</li>
 <li>`void` YAP_ClearExceptions(`void`)

Reset any exceptions left over by the system.
</li>
</ul>

The  YAP_RunGoal() interface is designed to be very robust, but may
not be the most efficient when repeated calls to the same goal are made
and when there is no interest in processing exception. The
 YAP_EnterGoal() interface should have lower-overhead:

<ul>
 <li>`YAP_PredEntryPtr` YAP_FunctorToPred(`YAP_Functor`  _f_)

Return the predicate whose main functor is  _f_.

</li>
 <li>`YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_)

Return the arity 0 predicate whose name is  _at_.

</li>
 <li>`YAP_PredEntryPtr`
YAP_FunctorToPredInModule(`YAP_Functor`  _f_, `YAP_Module`  _m_),

Return the predicate in module  _m_ whose main functor is  _f_.

</li>
 <li>`YAP_PredEntryPtr` YAP_AtomToPred(`YAP_Atom`  _at_, `YAP_Module`  _m_), 

Return the arity 0 predicate in module  _m_ whose name is  _at_.

</li>
 <li>`YAP_Bool` YAP_EnterGoal(`YAP_PredEntryPtr`  _pe_), 

`YAP_Term \*`  _array_, `YAP_dogoalinfo \*`  _infop_)
Execute a  query for predicate  _pe_. The query is given as an
array of terms  _Array_.  _infop_ is the address of a goal
handle that can be used to backtrack and to recover space. Succeeds if
a solution was found.

Notice that you cannot create new slots if an YAP_ExnterGoal goal is open.

</li>
 <li>`YAP_Bool` YAP_RetryGoal(`YAP_dogoalinfo \*`  _infop_) @anchor YAP_RetryGoal


Backtrack to a query created by [YAP_EnterGoal](@ref YAP_EnterGoal). The query is
given by the handle  _infop_. Returns whether a new solution could
be be found.

</li>
 <li>`YAP_Bool` YAP_LeaveGoal(`YAP_Bool`  _backtrack_, @anchor YAP_LeaveGoal

`YAP_dogoalinfo \*`  _infop_)
Exit a query query created by [YAP_EnterGoal](@ref YAP_EnterGoal). If
`backtrack` is `TRUE`, variable bindings are undone and Heap
space is recovered.  Otherwise, only stack space is recovered, ie,
`LeaveGoal` executes a cut.
</li>
</ul>
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

YAP allows calling a  *new* Prolog interpreter from `C`. One
way is to first construct a goal `G`, and then it is sufficient to
perform:

<ul>
 <li>YAP_Bool      YAP_CallProlog(YAP_Term  _G_)
</li>
</ul>
the result will be `FALSE`, if the goal failed, or `TRUE`, if
the goal succeeded. In this case, the variables in  _G_ will store
the values they have been unified with. Execution only proceeds until
finding the first solution to the goal, but you can call
[findall/3](@ref findall) or friends if you need all the solutions.

Notice that during execution, garbage collection or stack shifting may
have moved the terms 

@section Module_Manipulation_in_C Module Manipulation in C

YAP allows one to create a new module from C-code. To create the new
code it is sufficient to call:

<ul>
 <li>YAP_Module      YAP_CreateModule(YAP_Atom  _ModuleName_)
</li>
</ul>
Notice that the new module does not have any predicates associated and
that it is not the current module. To find the current module, you can call:

<ul>
 <li>YAP_Module      YAP_CurrentModule()
</li>
</ul>

Given a module, you may want to obtain the corresponding name. This is
possible by using:

<ul>
 <li>YAP_Term      YAP_ModuleName(YAP_Module mod)
</li>
</ul>
Notice that this function returns a term, and not an atom. You can
[YAP_AtomOfTerm](@ref YAP_AtomOfTerm) to extract the corresponding Prolog atom.

@section Miscellaneous_ChYFunctions Miscellaneous C Functions

<ul>
 <li>`void` YAP_Throw(`YAP_Term exception`)
</li>
 <li>`void` YAP_AsyncThrow(`YAP_Term exception`) @anchor YAP_Throw


Throw an exception with term   _exception_, just like if you called
`throw/2`. The function <tt>YAP_AsyncThrow</tt> is supposed to be used
from interrupt handlers.


</li>
 <li>`int` YAP_SetYAPFlag(`yap_flag_t flag, int value`) @anchor YAP_SetYAPFlag


This function allows setting some YAP flags from `C` .Currently,
only two boolean flags are accepted: `YAPC_ENABLE_GC` and
`YAPC_ENABLE_AGC`.  The first enables/disables the standard garbage
collector, the second does the same for the atom garbage collector.`

</li>
 <li>`YAP_TERM` YAP_AllocExternalDataInStack(`size_t bytes`)
</li>
 <li>`void \*` YAP_ExternalDataInStackFromTerm(`YAP_Term t`)
</li>
 <li>`YAP_Bool` YAP_IsExternalDataInStackTerm(`YAP_Term t`) @anchor YAP_AllocExternalDataInStack


The next routines allow one to store external data in the Prolog
execution stack. The first routine reserves space for  _sz_ bytes
and returns an opaque handle. The second routines receives the handle
and returns a pointer to the data.  The last routine checks if a term
is an opaque handle.

Data will be automatically reclaimed during
backtracking. Also, this storage is opaque to the Prolog garbage compiler,
so it should not be used to store Prolog terms. On the other hand, it
may be useful to store arrays in a compact way, or pointers to external objects.

</li>
 <li>`int` YAP_HaltRegisterHook(`YAP_halt_hook f, void \*closure`) @anchor YAP_HaltRegisterHook


Register the function  _f_ to be called if YAP is halted. The
function is called with two arguments: the exit code of the process
(`0` if this cannot be determined on your operating system) and
the closure argument  _closure_.


</li>
 <li>`int` YAP_Argv(`char \*\*\*argvp`) @anchor YAP_Argv

Return the number of arguments to YAP and instantiate argvp to point to the list of such arguments.

</li>
</ul>

@section Writing_C Writing predicates in C

We will distinguish two kinds of predicates:

<ul>
 <li>\a deterministic predicates which either fail or succeed but are not
backtrackable, like the one in the introduction;
</li>
 <li>\a backtrackable
predicates which can succeed more than once.
</li>
</ul>

The first kind of predicates should be implemented as a C function with
no arguments which should return zero if the predicate fails and a
non-zero value otherwise. The predicate should be declared to
YAP, in the initialization routine, with a call to

<ul>
 <li>void YAP_UserCPredicate(char \* _name_, YAP_Bool \* _fn_(), unsigned long int  _arity_);
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

<ul>
 <li>void YAP_UserBackCutCPredicate(char \* _name_, int \* _init_(), int \* _cont_(), int \* _cut_(), unsigned long int  _arity_, unsigned int  _sizeof_) @anchor YAP_UserBackCutCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_,  _cont_, and  _cut_ are the C functions that implement
the predicate and  _arity_ is the predicate's arity.

</li>
 <li>void YAP_UserBackCPredicate(char \* _name_, int \* _init_(), int \* _cont_(), unsigned long int  _arity_, unsigned int  _sizeof_) @anchor YAP_UserBackCPredicate

describes a new predicate where  _name_ is the name of the predicate,
 _init_, and  _cont_ are the C functions that implement the
predicate and  _arity_ is the predicate's arity.

</li>
 <li>void YAP_PRESERVE_DATA( _ptr_,  _type_); @anchor YAP_PRESERVE_DATA


</li>
 <li>void YAP_PRESERVED_DATA( _ptr_,  _type_); @anchor YAP_PRESERVED_DATA


</li>
 <li>void YAP_PRESERVED_DATA_CUT( _ptr_,  _type_); @anchor YAP_PRESERVED_DATA_CUT


</li>
 <li>void YAP_cut_succeed( void ); @anchor YAP_cut_succeed


</li>
 <li>void YAP_cut_fail( void ); @anchor YAP_cut_fail


</li>
</ul>

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
The call to [YAP_PRESERVE_DATA](@ref YAP_PRESERVE_DATA) is used to initialize the memory
which will hold the information to be preserved across
backtracking. The first argument is the variable we shall use, and the
second its type. Note that we can only use [YAP_PRESERVE_DATA](@ref YAP_PRESERVE_DATA)
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
case it exits, denoting success, with [YAP_cut_succeed](@ref YAP_cut_succeed), or
otherwise exits with [YAP_cut_fail](@ref YAP_cut_fail) denoting failure.

The reason for using for using the functions [YAP_cut_succeed](@ref YAP_cut_succeed) and
[YAP_cut_fail](@ref YAP_cut_fail) instead of just returning a non-zero value in the
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

Note that again the macro [YAP_PRESERVED_DATA](@ref YAP_PRESERVED_DATA) is used at the
beginning of the function to access the data preserved from the previous
solution.  Then it checks if the last solution was found and in that
case exits with [YAP_cut_succeed](@ref YAP_cut_succeed) in order to cut any further
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

  fprintf("n100 cut with counter %ld\n", YAP_IntOfTerm(n100_data->next_solution));
  return TRUE;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we have to use [YAP_PRESERVED_DATA_CUT](@ref YAP_PRESERVED_DATA_CUT): this is
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


@section YAP4_Notes Changes to the C-Interface in YAP4

YAP4 includes several changes over the previous `load_foreign_files/3`
interface. These changes were required to support the new binary code
formats, such as ELF used in Solaris2 and Linux.

    + All Names of YAP objects now start with  _YAP__. This is
      designed to avoid clashes with other code. Use `YAPInterface.h` to
      take advantage of the new interface. `c_interface.h` is still
      available if you cannot port the code to the new interface.

    + Access to elements in the new interface always goes through
    <em>functions</em>. This includes access to the argument registers,
    `YAP_ARG1` to `YAP_ARG16`. This change breaks code such as
    `unify(\&ARG1,\&t)`, which is nowadays:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
   YAP_Unify(ARG1, t);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + `cut_fail()` and `cut_succeed()` are now functions.

    + The use of `Deref` is deprecated. All functions that return
Prolog terms, including the ones that access arguments, already
dereference their arguments.

    + Space allocated with PRESERVE_DATA is ignored by garbage
collection and stack shifting. As a result, any pointers to a Prolog
stack object, including some terms, may be corrupted after garbage
collection or stack shifting. Prolog terms should instead be stored as
arguments to the backtrackable procedure.



@defgroup YAPAsLibrary Using YAP as a Library
@subgroup  c-interface

YAP can be used as a library to be called from other
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

<ol>
You must initialize the YAP environment. A single function,
`YAP_FastInit` asks for a contiguous chunk in your memory space, fills
it in with the data-base, and sets up YAP's stacks and
execution registers. You can use a saved space from a standard system by
calling save_program/1.

    + You then have to prepare a query to give to
YAP. A query is a Prolog term, and you just have to use the same
functions that are available in the C-interface.

    + You can then use `YAP_RunGoal(query)` to actually evaluate your
query. The argument is the query term `query`, and the result is 1
if the query succeeded, and 0 if it failed.

    + You can use the term destructor functions to check how
arguments were instantiated.

    + If you want extra solutions, you can use
`YAP_RestartGoal()` to obtain the next solution.

</ol>

The next program shows how to use this system. We assume the saved
program contains two facts for the procedure <tt>b</tt>:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include <stdio.h>
#include "YAP/YAPInterface.h"

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

    + YAP_CompileClause(`YAP_Term`  _Clause_)
Compile the Prolog term  _Clause_ and assert it as the last clause
for the corresponding procedure.

    + YAP_MkExo(`YAP_PredEntryPtr` _pred_, `size_t` _sz_, `void *` _uid_) 
Predicate _pred_ is an exo-predicate that needs _sz_ bytes of
contiguous storage. If _uid_ is non-null associate user-defined 
code with _pred_.

    + YAP_AssertTuples(`YAP_PredEntryPtr` pred, `const YAP_Term *`  _Facts_, `size_t` nb)
Add the array of _nb_ Prolog term `Facts` to the table 
`Predicate`.

    + `int` YAP_ContinueGoal(`void`)
Continue execution from the point where it stopped.

    + `void` YAP_Error(`int`  _ID_,`YAP_Term`  _Cause_,`char \*`  _error_description_)
Generate an YAP System Error with description given by the string
 _error_description_.  _ID_ is the error ID, if known, or
`0`.  _Cause_ is the term that caused the crash.

    + `void` YAP_Exit(`int`  _exit_code_)
Exit YAP immediately. The argument  _exit_code_ gives the error code
and is supposed to be 0 after successful execution in Unix and Unix-like
systems.

    + `YAP_Term` YAP_GetValue(`Atom`  _at_)
Return the term  _value_ associated with the atom  _at_. If no
such term exists the function will return the empty list.

    + YAP_FastInit(`char \*`  _SavedState_)
Initialize a copy of YAP from  _SavedState_. The copy is
monolithic and currently must be loaded at the same address where it was
saved. `YAP_FastInit` is a simpler version of `YAP_Init`.

    + YAP_Init( _InitInfo_)
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

    + `void` YAP_PutValue(`Atom`  _at_, `YAP_Term`  _value_)
Associate the term  _value_ with the atom  _at_. The term
 _value_ must be a constant. This functionality is used by YAP as a
simple way for controlling and communicating with the Prolog run-time.

    + `YAP_Term` YAP_Read(`IOSTREAM \*Stream`)
Parse a  _Term_ from the stream  _Stream_.

    + `YAP_Term` YAP_Write(`YAP_Term`  _t_)
Copy a Term  _t_ and all associated constraints. May call the garbage
collector and returns `0L` on error (such as no space being
available).

    + `void` YAP_Write(`YAP_Term`  _t_, `IOSTREAM`  _stream_, `int`  _flags_)
Write a Term  _t_ using the stream  _stream_ to output
characters. The term is written according to a mask of the following
flags in the `flag` argument: `YAP_WRITE_QUOTED`,
`YAP_WRITE_HANDLE_VARS`, `YAP_WRITE_USE_PORTRAY`,  and `YAP_WRITE_IGNORE_OPS`.

    + `int` YAP_WriteBuffer(`YAP_Term`  _t_, `char \*`  _buff_, `size_t`  _size_, `int`  _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The term is written
according to a mask of the following flags in the `flag`
argument: `YAP_WRITE_QUOTED`, `YAP_WRITE_HANDLE_VARS`,
`YAP_WRITE_USE_PORTRAY`, and `YAP_WRITE_IGNORE_OPS`. The
function will fail if it does not have enough space in the buffer.

    + `char \*` YAP_WriteDynamicBuffer(`YAP_Term`  _t_, `char \*`  _buff_, `size_t`  _size_, `size_t`  _\*lengthp_, `size_t`  _\*encodingp_, `int`  _flags_)
Write a YAP_Term  _t_ to buffer  _buff_ with size
 _size_. The code will allocate an extra buffer if  _buff_ is
`NULL` or if `buffer` does not have enough room. The
variable `lengthp` is assigned the size of the resulting buffer,
and `encodingp` will receive the type of encoding (currently only `PL_ENC_ISO_LATIN_1` and `PL_ENC_WCHAR` are supported)

    + `void` YAP_InitConsult(`int`  _mode_, `char \*`  _filename_)
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

    + `void` YAP_EndConsult(`void`)
Finish consult mode.



Some observations:

    + The system will core dump if you try to load the saved state in a
different address from where it was made. This may be a problem if
your program uses `mmap`. This problem will be addressed in future
versions of YAP.

    + Currently, the YAP library will pollute the name
space for your program.

    + The initial library includes the complete YAP system. In
the future we plan to split this library into several smaller libraries
(e.g. if you do not want to perform Input/Output).

    + You can generate your own saved states. Look at  the
`boot.yap` and `init.yap` files.

*/

#define _yap_c_interface_h 1

#define __YAP_PROLOG__ 1

#ifndef YAPVERSION
#define YAPVERSION 60000
#endif

#include "YapDefs.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#include <wchar.h>

/*
   __BEGIN_DECLS should be used at the beginning of the C declarations,
   so that C++ compilers don't mangle their names.  __END_DECLS is used
   at the end of C declarations.
*/
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif /* _cplusplus */

__BEGIN_DECLS

#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif

#ifndef Int_FORMAT

#if _WIN64
#define Int_FORMAT "%I64d"
#define Int_ANYFORMAT "%I64i"
#define UInt_FORMAT "%I64u"
#else
#define Int_FORMAT "%ld"
#define Int_ANYFORMAT "%li"
#define UInt_FORMAT "%lu"
#endif

#endif /* portable form of formatted output for Prolog terms */

/* Primitive Functions */

#define YAP_Deref(t)  (t)

extern X_API YAP_Term YAP_A(int);
#define YAP_ARG1	YAP_A(1)
#define YAP_ARG2	YAP_A(2)
#define YAP_ARG3	YAP_A(3)
#define YAP_ARG4	YAP_A(4)
#define YAP_ARG5	YAP_A(5)
#define YAP_ARG6	YAP_A(6)
#define YAP_ARG7	YAP_A(7)
#define YAP_ARG8	YAP_A(8)
#define YAP_ARG9	YAP_A(9)
#define YAP_ARG10	YAP_A(10)
#define YAP_ARG11	YAP_A(11)
#define YAP_ARG12	YAP_A(12)
#define YAP_ARG13	YAP_A(13)
#define YAP_ARG14	YAP_A(14)
#define YAP_ARG15	YAP_A(15)
#define YAP_ARG16	YAP_A(16)

/*  YAP_Bool IsVarTerm(YAP_Term) */
extern X_API YAP_Bool YAP_IsVarTerm(YAP_Term);

/*  YAP_Bool IsNonVarTerm(YAP_Term) */
extern X_API YAP_Bool YAP_IsNonVarTerm(YAP_Term);

/*  YAP_Term  MkVarTerm()  */
extern X_API YAP_Term YAP_MkVarTerm(void);

/*  YAP_Bool IsIntTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsIntTerm(YAP_Term);

/*  YAP_Bool IsLongIntTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsLongIntTerm(YAP_Term);

/*  YAP_Bool IsBigNumTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsBigNumTerm(YAP_Term);

/*  YAP_Bool IsRationalTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsRationalTerm(YAP_Term);

/*  YAP_Bool IsFloatTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsFloatTerm(YAP_Term);

/*  YAP_Bool IsNumberTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsNumberTerm(YAP_Term);

/*  YAP_Bool IsDbRefTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsDbRefTerm(YAP_Term);

/*  YAP_Bool IsAtomTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsAtomTerm(YAP_Term);

/*  YAP_Bool IsPairTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsPairTerm(YAP_Term);

/*  YAP_Bool IsApplTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsApplTerm(YAP_Term);

/*  YAP_Bool IsCompoundTerm(YAP_Term)  */
extern X_API YAP_Bool YAP_IsCompoundTerm(YAP_Term);

/*    Term MkIntTerm(YAP_Int)  */
extern X_API YAP_Term YAP_MkIntTerm(YAP_Int);

/*    Term MkBigNumTerm(void *)  */
extern X_API YAP_Term YAP_MkBigNumTerm(void *);

/*    Term MkRationalTerm(void *)  */
extern X_API YAP_Term YAP_MkRationalTerm(void *);

/*    YAP_Int  IntOfTerm(Term) */
extern X_API YAP_Int YAP_IntOfTerm(YAP_Term);

/*    void *  BigNumOfTerm(Term) */
extern X_API YAP_Bool YAP_BigNumOfTerm(YAP_Term t, void *b);

/*    void *  RationalOfTerm(Term) */
extern X_API YAP_Bool YAP_RationalOfTerm(YAP_Term, void *);

/*    Term MkFloatTerm(YAP_Float)  */
extern X_API YAP_Term YAP_MkFloatTerm(YAP_Float);

/*    YAP_Float  FloatOfTerm(YAP_Term) */
// extract a floating point number for a term t. The type `flt` is a typedef for
// the appropriate C floating point type,
extern X_API YAP_Float YAP_FloatOfTerm(YAP_Term);

/*    Term MkAtomTerm(Atom)  */
extern X_API YAP_Term YAP_MkAtomTerm(YAP_Atom);

/*    YAP_Atom  AtomOfTerm(Term) */
extern X_API YAP_Atom YAP_AtomOfTerm(YAP_Term);

extern X_API YAP_Atom YAP_LookupAtom(const char *c);

extern X_API YAP_Atom YAP_LookupWideAtom(const wchar_t *c);

extern X_API YAP_Atom YAP_FullLookupAtom(const char *c);

/*    int  AtomNameLength(Atom) */
extern X_API size_t YAP_AtomNameLength(YAP_Atom);

extern X_API YAP_Bool YAP_IsWideAtom(YAP_Atom a);

extern X_API const char *YAP_AtomName(YAP_Atom a);

extern X_API const wchar_t *YAP_WideAtomName(YAP_Atom a);

/*    YAP_Term  MkPairTerm(YAP_Term Head, YAP_Term Tail) */
extern X_API YAP_Term YAP_MkPairTerm(YAP_Term,YAP_Term);

extern X_API YAP_Term YAP_MkListFromTerms(YAP_Term *,YAP_Int);

/*    YAP_Term  MkNewPairTerm(void) */
extern X_API YAP_Term YAP_MkNewPairTerm(void);

/*    Term  HeadOfTerm(Term)  */
extern X_API YAP_Term YAP_HeadOfTerm(YAP_Term);

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term YAP_TailOfTerm(YAP_Term);

/*    Int  AddressOfTailOfTerm(Term *, Term **)  */
extern X_API YAP_Int YAP_SkipList(YAP_Term *, YAP_Term **);

/*    Term  TailOfTerm(Term)  */
extern X_API YAP_Term YAP_TermNil(void);

extern X_API int YAP_IsTermNil(YAP_Term);

extern X_API YAP_Term YAP_MkApplTerm(YAP_Functor functor, YAP_UInt arity,YAP_Term args[]);

extern X_API YAP_Term YAP_MkNewApplTerm( YAP_Functor f, YAP_UInt arity);

extern X_API YAP_Functor YAP_FunctorOfTerm(YAP_Term t);

extern X_API YAP_Term YAP_ArgOfTerm(YAP_UInt n, YAP_Term t);

extern X_API YAP_Term *YAP_ArgsOfTerm(YAP_Term t);

extern X_API YAP_Functor YAP_MkFunctor(YAP_Atom a, YAP_UInt n);

extern X_API YAP_Atom YAP_NameOfFunctor(YAP_Functor g);

extern X_API YAP_UInt YAP_ArityOfFunctor(YAP_Functor f);

/*  void ExtraSpace(void) */
extern X_API void *YAP_ExtraSpace(void);
extern X_API void *YAP_ExtraSpaceCut(void);

#define YAP_PRESERVE_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA(ptr, type) (ptr = (type *)YAP_ExtraSpace())
#define YAP_PRESERVED_DATA_CUT(ptr,type) (ptr = (type *)YAP_ExtraSpaceCut())

extern X_API YAP_Bool YAP_Unify(YAP_Term t1, YAP_Term t2);

/*  void UserCPredicate(const char *name, int *fn(), int arity) */
extern X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, YAP_Arity arity);

/*  void UserCPredicateWithArgs(const char *name, int *fn(), unsigned int arity) */
extern X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, YAP_Arity, YAP_Term);

/*  void UserBackCPredicate(const char *name, int *init(), int *cont(), int
    arity, int extra) */
extern X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred, YAP_Arity, unsigned int);

/*   YAP_Int      YAP_ListLength(YAP_Term t) */
extern X_API YAP_Int YAP_ListLength(YAP_Term);

/*  void UserBackCPredicate(char *name, int *init(), int *cont(), int *cut(), int
    arity, int extra) */
extern X_API void YAP_UserBackCutCPredicate(const char *, YAP_UserCPred, YAP_UserCPred, YAP_UserCPred, YAP_Arity, unsigned int);

/*  void CallProlog(YAP_Term t) */
extern X_API YAP_Int YAP_CallProlog(YAP_Term t);

/*  void cut_fail(void) */
extern X_API void YAP_cut_up(void);

#define YAP_cut_succeed() do { YAP_cut_up(); return TRUE; } while(0)

#define YAP_cut_fail() do { YAP_cut_up(); return FALSE; } while(0)

/*  void *AllocSpaceFromYAP_(int) */
extern X_API void *YAP_AllocSpaceFromYap(size_t);

/*  void *ReallocSpaceFromYAP_(void*,int) */
extern X_API void *YAP_ReallocSpaceFromYap(void*,size_t);

/*  void FreeSpaceFromYAP_(void *) */
extern X_API void YAP_FreeSpaceFromYap(void *);

/*  int YAP_RunGoal(YAP_Term) */
extern X_API YAP_Int YAP_RunGoal(YAP_Term);

//extern X_API YAP_Int YAP_RunPredicate(YAP_PredEntryPtr, YAP_Term *);

/*  int YAP_RunGoalOnce(YAP_Term) */
extern X_API YAP_Int YAP_RunGoalOnce(YAP_Term);

/*  int YAP_RestartGoal(void) */
extern X_API YAP_Bool YAP_RestartGoal(void);

/*  int YAP_ShutdownGoal(int) */
extern X_API YAP_Bool YAP_ShutdownGoal(int);

/*  int YAP_ContinueGoal(void) */
extern X_API YAP_Bool YAP_ContinueGoal(void);


/*  void YAP_PruneGoal(void) */
extern X_API void YAP_PruneGoal(YAP_dogoalinfo *);

/*  int YAP_FunctorToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_FunctorToPred(YAP_Functor);

/*  int YAP_AtomToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_AtomToPred(YAP_Atom);

extern X_API YAP_PredEntryPtr YAP_FunctorToPredInModule(YAP_Functor, YAP_Module);

/*  int YAP_AtomToPred(struct pred_entry *, YAP_Term *) */
extern X_API YAP_PredEntryPtr YAP_AtomToPredInModule(YAP_Atom, YAP_Module);

/*  int YAP_EnterGoal(void) */
extern X_API YAP_Bool YAP_EnterGoal(YAP_PredEntryPtr, YAP_Term *, YAP_dogoalinfo *);

/*  int YAP_RetryGoal(void) */
extern X_API YAP_Bool YAP_RetryGoal(YAP_dogoalinfo *);

/*  int YAP_LeaveGoal(void) */
extern X_API YAP_Bool YAP_LeaveGoal(int, YAP_dogoalinfo *);

/*  int YAP_GoalHasException(YAP_Term *) */
extern X_API YAP_Bool YAP_GoalHasException(YAP_Term *);

/*  void YAP_ClearExceptions(void) */
extern X_API void YAP_ClearExceptions(void);

extern X_API int YAP_Reset(yap_reset_t reset);

extern X_API void YAP_Error(int myerrno, YAP_Term t, const char *buf, ...);

extern X_API int YAP_WriteBuffer(YAP_Term,char *,size_t,int);

extern X_API char* YAP_WriteDynamicBuffer(YAP_Term t,char *buf,size_t sze, size_t *lengthp, int *encp, int flags);

/*  void YAP_Term(YAP_Term) */
extern X_API YAP_Term YAP_CopyTerm(YAP_Term);

/*  char *YAP_CompileClause(YAP_Term) */
extern X_API char *YAP_CompileClause(YAP_Term);

extern X_API int YAP_NewExo( YAP_PredEntryPtr ap, size_t data, void *user_di);

extern X_API int YAP_AssertTuples( YAP_PredEntryPtr pred, const YAP_Term *ts, size_t sz);

/*  int YAP_Init(YAP_init_args *) */
extern X_API YAP_Int YAP_Init(YAP_init_args *);

/*  int YAP_FastInit(const char *) */
extern X_API YAP_Int YAP_FastInit(char saved_state[]);

#ifndef _PL_STREAM_H
// if we don't know what a stream is, just don't assume nothing about the pointer
#define IOSTREAM void
#endif /* FPL_STREAM_H */

extern X_API YAP_Term YAP_Read(IOSTREAM *s);

extern X_API void YAP_Write(YAP_Term t,IOSTREAM *s,int);

extern X_API  IOSTREAM * YAP_TermToStream(YAP_Term t);

extern X_API  IOSTREAM * YAP_InitConsult(int mode, const char *filename);

extern X_API void YAP_EndConsult(IOSTREAM *s);

#ifndef _PL_STREAM_H
// if we don't know what a stream is, just don't assume nothing about the pointer
#undef IOSTREAM
#endif /* FPL_STREAM_H */


extern X_API void YAP_Exit(int);

/*  void YAP_PutValue(YAP_Atom, YAP_Term) */
extern X_API void YAP_PutValue(YAP_Atom, YAP_Term);

/*  YAP_Term YAP_GetValue(YAP_Atom) */
extern X_API YAP_Term YAP_GetValue(YAP_Atom);

extern X_API YAP_Term YAP_FloatsToList(YAP_Float *, size_t);
extern X_API YAP_Int  YAP_ListToFloats(YAP_Term, YAP_Float *, size_t);

extern X_API YAP_Term YAP_IntsToList(YAP_Int *, size_t);
extern X_API YAP_Int  YAP_ListToInts(YAP_Term, YAP_Int *, size_t);

/*  int StringToBuffer(YAP_Term,char *,unsigned int) */
extern X_API int YAP_StringToBuffer(YAP_Term,char *,unsigned int);

extern X_API YAP_Term YAP_BufferToString(const char *s);

extern X_API YAP_Term YAP_NBufferToString(const char *s, size_t len);

/*  int BufferToString(const char *) */
extern X_API YAP_Term YAP_WideBufferToString(const wchar_t *);

extern X_API YAP_Term YAP_NWideBufferToString(const wchar_t *s, size_t len);

extern X_API YAP_Term YAP_BufferToAtomList(const char *s);

extern X_API YAP_Term YAP_NBufferToAtomList(const char *s, size_t len);

extern X_API YAP_Term YAP_WideBufferToAtomList(const wchar_t *s);

extern X_API YAP_Term YAP_NWideBufferToAtomList(const wchar_t *s, size_t len);

extern X_API YAP_Term YAP_NWideBufferToAtomDiffList(const wchar_t *s, YAP_Term t0, size_t len);

extern X_API YAP_Term YAP_BufferToDiffList(const char *s, YAP_Term t0);

extern X_API YAP_Term YAP_NBufferToDiffList(const char *s, YAP_Term t0, size_t len);

extern X_API YAP_Term YAP_WideBufferToDiffList(const wchar_t *s, YAP_Term t0);

extern X_API YAP_Term YAP_NWideBufferToDiffList(const wchar_t *s, YAP_Term t0, size_t len);

extern X_API YAP_Term YAP_ReadBuffer(const char *s,YAP_Term *tp);

extern X_API int YAP_InitSocks(const char *host,long port);

#ifdef  SFUNC

#define SFArity  0
extern X_API YAP_Term *ArgsOfSFTerm();

extern X_API YAP_Term MkSFTerm();

#endif /* SFUNC */


extern X_API void YAP_SetOutputMessage(void);

extern X_API int YAP_StreamToFileNo(YAP_Term);

extern X_API void YAP_CloseAllOpenStreams(void);

extern X_API void YAP_FlushAllStreams(void);

#define YAP_INPUT_STREAM	0x01
#define YAP_OUTPUT_STREAM	0x02
#define YAP_APPEND_STREAM	0x04
#define YAP_PIPE_STREAM 	0x08
#define YAP_TTY_STREAM	 	0x10
#define YAP_POPEN_STREAM	0x20
#define YAP_BINARY_STREAM	0x40
#define YAP_SEEKABLE_STREAM	0x80

/*  YAP_Term  *YAP_NewSlots()  */
extern X_API YAP_handle_t YAP_NewSlots(int);

/*  YAP_Int  YAP_CurrentSlot()  */
extern X_API YAP_handle_t YAP_CurrentSlot(void);

/*  YAP_Term  *YAP_InitSlot()  */
extern X_API YAP_handle_t YAP_InitSlot(YAP_Term);

/*  YAP_Term  YAP_GetFromSlots(t)  */
extern X_API YAP_Term YAP_GetFromSlot(YAP_handle_t);

/*  YAP_Term  *YAP_AddressFromSlots(t)  */
extern X_API YAP_Term *YAP_AddressFromSlot(YAP_handle_t);

/*  YAP_Term  *YAP_AddressOfTermInSlot(t)  */
extern X_API YAP_Term *YAP_AddressOfTermInSlot(YAP_handle_t);

/*  YAP_Term  YAP_PutInSlots(t)  */
extern X_API void YAP_PutInSlot(YAP_handle_t, YAP_Term);

extern X_API int YAP_RecoverSlots(int n, YAP_handle_t top_slot);

extern X_API YAP_handle_t YAP_ArgsToSlots(int);

extern X_API void YAP_SlotsToArgs(int, YAP_handle_t);

/*  void  YAP_Throw()  */
extern X_API void YAP_Throw(YAP_Term);

/*  void  YAP_AsyncThrow()  */
extern X_API void YAP_AsyncThrow(YAP_Term);

/*  int  YAP_LookupModule()  */
#define YAP_LookupModule(T)  (T)

#define YAP_ModuleName(mod) (mod)

/*  int  YAP_Halt()  */
extern X_API void  YAP_Halt(int);

/*  int  YAP_TopOfLocalStack()  */
extern X_API YAP_Term  *YAP_TopOfLocalStack(void);

/*  int  YAP_Predicate()  */
extern X_API void  *YAP_Predicate(YAP_Atom,YAP_Arity,YAP_Term);

/*  int  YAP_Predicate()  */
extern X_API void  YAP_PredicateInfo(void *,YAP_Atom *,YAP_Arity*,YAP_Module*);

/*  int  YAP_CurrentModule()  */
extern X_API YAP_Module  YAP_CurrentModule(void);

/*  int  YAP_SetCurrentModule()  */
extern X_API YAP_Module  YAP_SetCurrentModule(YAP_Module);

/*  int  YAP_CreateModule()  */
extern X_API YAP_Module  YAP_CreateModule(YAP_Atom);

/*  int  YAP_StripModule()  */
extern X_API YAP_Term  YAP_StripModule(YAP_Term, YAP_Module *);

/*  int  YAP_AtomGetHold(YAP_Atom)  */
extern X_API int  YAP_AtomGetHold(YAP_Atom);

/*  int  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int  YAP_AtomReleaseHold(YAP_Atom);

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API YAP_agc_hook  YAP_AGCRegisterHook(YAP_agc_hook hook);

/*  void  YAP_AtomReleaseHold(YAP_Atom)  */
extern X_API int  YAP_HaltRegisterHook(YAP_halt_hook, void *);

/*  char *YAP_cwd(void)  */
extern X_API char *  YAP_cwd(void);

/* thread stuff */
extern X_API int YAP_ThreadSelf(void);
extern X_API int YAP_ThreadCreateEngine(YAP_thread_attr *attr);
extern X_API int YAP_ThreadAttachEngine(int);
extern X_API int YAP_ThreadDetachEngine(int);
extern X_API int YAP_ThreadDestroyEngine(int);

/* blob stuff */
extern X_API YAP_Term YAP_MkBlobTerm(unsigned int);
extern X_API void    *YAP_BlobOfTerm(YAP_Term);

/*  term comparison */
extern X_API int  YAP_CompareTerms(YAP_Term, YAP_Term);

/*  list construction */
extern X_API YAP_Term     YAP_OpenList(int);
extern X_API YAP_Term     YAP_ExtendList(YAP_Term, YAP_Term);
extern X_API int          YAP_CloseList(YAP_Term, YAP_Term);

/*  attributed variables */
extern X_API int	YAP_IsAttVar(YAP_Term);
extern X_API YAP_Term	YAP_AttsOfVar(YAP_Term);

/*  stream info */
extern X_API void      *YAP_FileDescriptorFromStream(YAP_Term);
extern X_API int        YAP_FileNoFromStream(YAP_Term);

/*  store and recover terms */
extern X_API void      *YAP_Record(YAP_Term);
extern X_API YAP_Term   YAP_Recorded(void *);
extern X_API int        YAP_Erase(void *);

/*  term utilities */
extern X_API int        YAP_Variant(YAP_Term,YAP_Term);
extern X_API YAP_Int    YAP_NumberVars(YAP_Term,YAP_Int);
extern X_API YAP_Term   YAP_UnNumberVars(YAP_Term);
extern X_API int        YAP_IsNumberedVariable(YAP_Term);
extern X_API int        YAP_Unifiable(YAP_Term,YAP_Term);
extern X_API int        YAP_ExactlyEqual(YAP_Term,YAP_Term);
extern X_API YAP_Int    YAP_TermHash(YAP_Term, YAP_Int, YAP_Int, int);

extern X_API void       YAP_signal(int);

/*  stack expansion control */
extern X_API int        YAP_SetYAPFlag(yap_flag_t,int);

/*    void  *YAP_GlobalBase(Term)  */
extern X_API YAP_Int YAP_VarSlotToNumber(YAP_Int);

/*    Term  YAP_ModuleUser()  */
extern X_API YAP_Term YAP_ModuleUser(void);

/*    Int  YAP_NumberOfClausesForPredicate()  */
extern X_API YAP_Int YAP_NumberOfClausesForPredicate(YAP_PredEntryPtr);

/*    int  YAP_MaxOpPriority(Atom, Term)  */
extern X_API int YAP_MaxOpPriority(YAP_Atom, YAP_Term);

/*    int  YAP_OpInfo(Atom, Term, int, int *, int *)  */
extern X_API int  YAP_OpInfo(YAP_Atom, YAP_Term, int, int *, int *);

extern X_API YAP_Bool YAP_IsExternalDataInStackTerm(YAP_Term);

extern X_API YAP_Term YAP_AllocExternalDataInStack(size_t bytes);

extern X_API void *YAP_ExternalDataInStackFromTerm(YAP_Term t);

extern X_API YAP_Bool YAP_IsExternalDataInStackTerm(YAP_Term t);

extern X_API YAP_opaque_tag_t YAP_NewOpaqueType(struct YAP_opaque_handler_struct *f);

extern X_API YAP_Bool YAP_IsOpaqueObjectTerm(YAP_Term t, YAP_opaque_tag_t tag);

extern X_API YAP_Term YAP_NewOpaqueObject(YAP_opaque_tag_t tag, size_t bytes);

extern X_API void *YAP_OpaqueObjectFromTerm(YAP_Term t);

extern X_API YAP_CELL *YAP_HeapStoreOpaqueTerm(YAP_Term t);

extern X_API int  YAP_Argv(char ***);

extern X_API YAP_tag_t YAP_TagOfTerm(YAP_Term);

extern X_API size_t     YAP_ExportTerm(YAP_Term, char *, size_t);

extern X_API size_t   YAP_SizeOfExportedTerm(char *);

extern X_API YAP_Term     YAP_ImportTerm(char *);

extern X_API int      YAP_RequiresExtraStack(size_t);

extern X_API int
YAP_parse_yap_arguments(int argc, char *argv[], YAP_init_args *iap);

extern X_API YAP_Int      YAP_AtomToInt(YAP_Atom At);

extern X_API YAP_Atom     YAP_IntToAtom(YAP_Int i);

#define YAP_InitCPred(N,A,F)  YAP_UserCPredicate(N,F,A)

__END_DECLS

#endif
