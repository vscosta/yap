/** 

@defgroup YAPControl Control Predicates
@ingroup YAPBuiltins
@{

 
*/


/** @pred  true is iso 


Succeeds once.

 
*/

/** @pred  fail is iso 


Always fails.

 
*/

/** @pred  false is iso 


The same as fail.

 
*/

/** @pred  repeat is iso 
bprolqSucceeds repeatedly.

In the next example, `repeat` is used as an efficient way to implement
a loop. The next example reads all terms in a file:
~~~~~~~~~~~~~
 a :- repeat, read(X), write(X), nl, X=end_of_file, !.
~~~~~~~~~~~~~
the loop is effectively terminated by the cut-goal, when the test-goal
`X=end` succeeds. While the test fails, the goals `read(X)`,
`write(X)`, and `nl` are executed repeatedly, because
backtracking is caught by the `repeat` goal.

The built-in `repeat/0` could be defined in Prolog by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 repeat.
 repeat :- repeat.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The predicate between/3 can be used to iterate for a pre-defined
number of steps.
 
*/

/** @pred  call(+ _P_) is iso 
Meta-call predicate.

If _P_ is instantiated to an atom or a compound term, the goal `call(
_P_)` is executed as if the clause was originally written as _P_
instead as call( _P_ ), except that any "cut" occurring in _P_ only
cuts alternatives in the execution of _P_.

 
*/

/** @pred  incore(+ _P_) 


The same as call/1.

 
*/

/** @pred  call(+ _Closure_,...,? _Ai_,...) is iso 


Meta-call where  _Closure_ is a closure that is converted into a goal by 
appending the  _Ai_ additional arguments. The number of arguments varies 
between 0 and 10.

 
*/

/** @pred  call_with_args(+ _Name_,...,? _Ai_,...) 


Meta-call where  _Name_ is the name of the procedure to be called and
the  _Ai_ are the arguments. The number of arguments varies between 0
and 10. New code should use `call/N` for better portability.

If  _Name_ is a complex term, then call_with_args/n behaves as
call/n:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
call(p(X1,...,Xm), Y1,...,Yn) :- p(X1,...,Xm,Y1,...,Yn).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  + _P_ 

The same as `call( _P_)`. This feature has been kept to provide
compatibility with C-Prolog. When compiling a goal, YAP
generates a `call( _X_)` whenever a variable  _X_ is found as
a goal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 a(X) :- X.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is converted to:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 a(X) :- call(X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  if(? _G_,? _H_,? _I_)

Call goal  _H_ once per each solution of goal  _H_. If goal
 _H_ has no solutions, call goal  _I_.

The built-in `if/3` is similar to `-\>/3`, with the difference
that it will backtrack over the test goal. Consider the following
small data-base:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a(1).        b(a).          c(x).
a(2).        b(b).          c(y).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Execution of an `if/3` query will proceed as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- if(a(X),b(Y),c(Z)).

X = 1,
Y = a ? ;

X = 1,
Y = b ? ;

X = 2,
Y = a ? ;

X = 2,
Y = b ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The system will backtrack over the two solutions for `a/1` and the
two solutions for `b/1`, generating four solutions.

Cuts are allowed inside the first goal  _G_, but they will only prune
over  _G_.

If you want  _G_ to be deterministic you should use if-then-else, as
it is both more efficient and more portable.

 
*/

/** @pred  once(: _G_) is iso 


Execute the goal  _G_ only once. The predicate is defined by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 once(G) :- call(G), !.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that cuts inside once/1 can only cut the other goals inside
once/1.

 
*/

/** @pred  forall(: _Cond_,: _Action_) 


For all alternative bindings of  _Cond_  _Action_ can be
proven. The example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  ignore(: _Goal_) 


Calls  _Goal_ as once/1, but succeeds, regardless of whether
`Goal` succeeded or not. Defined as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ignore(Goal) :-
        Goal, !.
ignore(_).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  abort 


Abandons the execution of the current goal and returns to top level. All
break levels (see break/0 below) are terminated. It is mainly
used during debugging or after a serious execution error, to return to
the top-level.

 
*/

/** @pred  break 


Suspends the execution of the current goal and creates a new execution
level similar to the top level, displaying the following message:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [ Break (level <number>) ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
telling the depth of the break level just entered. To return to the
previous level just type the end-of-file character or call the
end_of_file predicate.  This predicate is especially useful during
debugging.

 
*/

/** @pred  halt is iso 


Halts Prolog, and exits to the calling application. In YAP,
halt/0 returns the exit code `0`.

 
*/

/** @pred  halt(+  _I_) is iso

Halts Prolog, and exits to the calling application returning the code
given by the integer  _I_.

 
*/

/** @pred  catch( : _Goal_,+ _Exception_,+ _Action_) is iso 


The goal `catch( _Goal_, _Exception_, _Action_)` tries to
execute goal  _Goal_. If during its execution,  _Goal_ throws an
exception  _E'_ and this exception unifies with  _Exception_, the
exception is considered to be caught and  _Action_ is executed. If
the exception  _E'_ does not unify with  _Exception_, control
again throws the exception.

The top-level of YAP maintains a default exception handler that
is responsible to capture uncaught exceptions.

 
*/

/** @pred  throw(+ _Ball_) is iso 


The goal `throw( _Ball_)` throws an exception. Execution is
stopped, and the exception is sent to the ancestor goals until reaching
a matching catch/3, or until reaching top-level.

 
*/

/** @pred  garbage_collect 


The goal `garbage_collect` forces a garbage collection.

 
*/

/** @pred  garbage_collect_atoms 


The goal `garbage_collect` forces a garbage collection of the atoms
in the data-base. Currently, only atoms are recovered.

 
*/

/** @pred  gc 


The goal `gc` enables garbage collection. The same as
`yap_flag(gc,on)`.

 
*/

/** @pred  nogc 


The goal `nogc` disables garbage collection. The same as
`yap_flag(gc,off)`.

 
*/

/** @pred  grow_heap(+ _Size_) 
Increase heap size  _Size_ kilobytes.

 
*/

/** @pred  grow_stack(+ _Size_) 


Increase stack size  _Size_ kilobytes


@} */

/** @defgroup Undefined_Procedures Handling Undefined Procedures
@ingroup YAPBuiltins
@{

A predicate in a module is said to be undefined if there are no clauses
defining the predicate, and if the predicate has not been declared to be
dynamic. What YAP does when trying to execute undefined predicates can
be specified in three different ways:

    + By setting an YAP flag, through the yap_flag/2 or
set_prolog_flag/2 built-ins. This solution generalizes the
ISO standard.
    + By using the unknown/2 built-in (this solution is
compatible with previous releases of YAP).
    + By defining clauses for the hook predicate
`user:unknown_predicate_handler/3`. This solution is compatible
with SICStus Prolog.


In more detail:


 
*/

/** @pred  unknown(- _O_,+ _N_) 


Specifies an handler to be called is a program tries to call an
undefined static procedure  _P_.

The arity of  _N_ may be zero or one. If the arity is `0`, the
new action must be one of `fail`, `warning`, or
`error`. If the arity is `1`,  _P_ is an user-defined
handler and at run-time, the argument to the handler  _P_ will be
unified with the undefined goal. Note that  _N_ must be defined prior
to calling unknown/2, and that the single argument to  _N_ must
be unbound.

In YAP, the default action is to `fail` (note that in the ISO
Prolog standard the default action is `error`).

After defining `undefined/1` by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
undefined(A) :- format('Undefined predicate: ~w~n',[A]), fail.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and executing the goal:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unknown(U,undefined(X)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a call to a predicate for which no clauses were defined will result in
the output of a message of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Undefined predicate: user:xyz(A1,A2)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
followed by the failure of that call.

 
*/

/** @pred  yap_flag(unknown,+ _SPEC_) 

Alternatively, one can use yap_flag/2,
current_prolog_flag/2, or set_prolog_flag/2, to set this
functionality. In this case, the first argument for the built-ins should
be `unknown`, and the second argument should be either
`error`, `warning`, `fail`, or a goal.

 
*/

/** @pred  user:unknown_predicate_handler(+G,+M,?NG) 


The user may also define clauses for
`user:unknown_predicate_handler/3` hook predicate. This
user-defined procedure is called before any system processing for the
undefined procedure, with the first argument  _G_ set to the current
goal, and the second  _M_ set to the current module. The predicate
 _G_ will be called from within the user module.

If `user:unknown_predicate_handler/3` succeeds, the system will
execute  _NG_. If  `user:unknown_predicate_handler/3` fails, the
system will execute default action as specified by unknown/2.

 
*/

/** @pred  exception(+ _Exception_, + _Context_, - _Action_) 


Dynamic predicate, normally not defined. Called by the Prolog system on run-time exceptions that can be repaired `just-in-time'. The values for  _Exception_ are described below. See also catch/3 and throw/1.
If this hook predicate succeeds it must instantiate the  _Action_ argument to the atom `fail` to make the operation fail silently, `retry` to tell Prolog to retry the operation or `error` to make the system generate an exception. The action `retry` only makes sense if this hook modified the environment such that the operation can now succeed without error.

    + undefined_predicate
 _Context_ is instantiated to a predicate-indicator ( _Module:Name/Arity_). If the predicate fails Prolog will generate an existence_error exception. The hook is intended to implement alternatives to the SWI built-in autoloader, such as autoloading code from a database. Do not use this hook to suppress existence errors on predicates. See also `unknown`.
    + undefined_global_variable
 _Context_ is instantiated to the name of the missing global variable. The hook must call nb_setval/2 or b_setval/2 before returning with the action retry.





@} */

/** @defgroup Messages Message Handling
@ingroup YAPBuiltins
@{

The interaction between YAP and the user relies on YAP's ability to
portray messages. These messages range from prompts to error
information. All message processing is performed through the builtin
print_message/2, in two steps:

    + The message is processed into a list of commands 
    + The commands in the list are sent to the `format/3` builtin
in sequence.


The first argument to print_message/2 specifies the importance of
the message. The options are:

    + error
error handling
    + warning
compilation and run-time warnings,
    + informational
generic informational messages
    + help 
help messages (not currently implemented in YAP)
    + query
query 	used in query processing (not currently implemented in YAP)
    + silent
messages that do not produce output but that can be intercepted by hooks.


The next table shows the main predicates and hooks associated to message
handling in YAP:


*/

/** @pred print_message(+ _Kind_,  _Term_) 

The predicate print_message/2 is used to print messages, notably from
exceptions in a human-readable format.  _Kind_ is one of
`informational`, `banner`, `warning`, `error`,
`help` or `silent`. A human-readable message is printed to
the stream user_error.

If the Prolog flag verbose is `silent`, messages with
 _Kind_ `informational`, or `banner` are treated as
silent.@c  See \\cmdlineoption{-q}.

This predicate first translates the  _Term_ into a list of `message
lines' (see print_message_lines/3 for details).  Next it will
call the hook message_hook/3 to allow the user intercepting the
message.  If message_hook/3 fails it will print the message unless
 _Kind_ is silent.

If you need to report errors from your own predicates, we advise you to
stick to the existing error terms if you can; but should you need to
invent new ones, you can define corresponding error messages by
asserting clauses for `prolog:message/2`. You will need to declare
the predicate as multifile.

 
*/

/** @pred  print_message_lines(+ _Stream_, + _Prefix_, + _Lines_) 


Print a message (see print_message/2) that has been translated to
a list of message elements.  The elements of this list are:

    + `\<Format\>`-`\<Args\>`
Where  _Format_ is an atom and  _Args_ is a list
of format argument.  Handed to `format/3`.
    + `flush`
If this appears as the last element,  _Stream_ is flushed
(see `flush_output/1`) and no final newline is generated.
    + `at_same_line`
If this appears as first element, no prefix is printed for
the first line and the line-position is not forced to 0
(see `format/1`, `~N`).
    + `\<Format\>`
Handed to `format/3` as `format(Stream, Format, [])`.
    + nl
A new line is started and if the message is not complete
the  _Prefix_ is printed too.


 
*/

/** @pred  user:message_hook(+ _Term_, + _Kind_, + _Lines_) 


Hook predicate that may be define in the module `user` to intercept
messages from print_message/2.  _Term_ and  _Kind_ are the
same as passed to print_message/2.  _Lines_ is a list of
format statements as described with print_message_lines/3.

This predicate should be defined dynamic and multifile to allow other
modules defining clauses for it too.

 
*/

/** @pred  message_to_string(+ _Term_, - _String_) 


Translates a message-term into a string object. Primarily intended for SWI-Prolog emulation.



@} */

/** @defgroup Testing_Terms Predicates on terms
@ingroup YAPBuiltins
@{



 
*/

/** @pred  var( _T_) is iso 


Succeeds if  _T_ is currently a free variable, otherwise fails. 

 
*/

/** @pred  atom( _T_) is iso 


Succeeds if and only if  _T_ is currently instantiated to an  atom.

 
*/

/** @pred  atomic(T) is iso 


Checks whether  _T_ is an atomic symbol (atom or number).

 
*/

/** @pred  compound( _T_) is iso 


Checks whether  _T_ is a compound term.

 
*/

/** @pred  db_reference( _T_) 


Checks whether  _T_ is a database reference.

 
*/

/** @pred  float( _T_) is iso 


Checks whether  _T_ is a floating point number.

 
*/

/** @pred  rational( _T_) 


Checks whether `T` is a rational number.

 
*/

/** @pred  integer( _T_) is iso 


Succeeds if and only if  _T_ is currently instantiated to an  integer.

 
*/

/** @pred  nonvar( _T_) is iso 


The opposite of `var( _T_)`.

 
*/

/** @pred  number( _T_) is iso 


Checks whether `T` is an integer, rational or a float.

 
*/

/** @pred  primitive( _T_) 


Checks whether  _T_ is an atomic term or a database reference.

 
*/

/** @pred  simple( _T_) 


Checks whether  _T_ is unbound, an atom, or a number.

 
*/

/** @pred  callable( _T_) is iso 


Checks whether  _T_ is a callable term, that is, an atom or a
compound term.

 
*/

/** @pred  numbervars( _T_,+ _N1_,- _Nn_) 


Instantiates each variable in term  _T_ to a term of the form:
`'$VAR'( _I_)`, with  _I_ increasing from  _N1_ to  _Nn_.

 
*/

/** @pred  unnumbervars( _T_,+ _NT_) 


Replace every `'$VAR'( _I_)` by a free variable.

 
*/

/** @pred  ground( _T_) is iso 


Succeeds if there are no free variables in the term  _T_.

 
*/

/** @pred  acyclic_term( _T_) is iso 


Succeeds if there are loops in the term  _T_, that is, it is an infinite term.

 
*/

/** @pred  arg(+ _N_,+ _T_, _A_) is iso 


Succeeds if the argument  _N_ of the term  _T_ unifies with
 _A_. The arguments are numbered from 1 to the arity of the term.

The current version will generate an error if  _T_ or  _N_ are
unbound, if  _T_ is not a compound term, of if  _N_ is not a positive
integer. Note that previous versions of YAP would fail silently
under these errors.

 
*/

/** @pred  functor( _T_, _F_, _N_) is iso 


The top functor of term  _T_ is named  _F_ and has  arity  _N_.

When  _T_ is not instantiated,  _F_ and  _N_ must be. If
 _N_ is 0,  _F_ must be an atomic symbol, which will be unified
with  _T_. If  _N_ is not 0, then  _F_ must be an atom and
 _T_ becomes instantiated to the most general term having functor
 _F_ and arity  _N_. If  _T_ is instantiated to a term then
 _F_ and  _N_ are respectively unified with its top functor name
and arity.

In the current version of YAP the arity  _N_ must be an
integer. Previous versions allowed evaluable expressions, as long as the
expression would evaluate to an integer. This feature is not available
in the ISO Prolog standard.

 
*/

/** @pred  _T_ =..  _L_ is iso 


The list  _L_ is built with the functor and arguments of the term
 _T_. If  _T_ is instantiated to a variable, then  _L_ must be
instantiated either to a list whose head is an atom, or to a list
consisting of just a number.

 
*/

/** @pred  _X_ =  _Y_ is iso 


Tries to unify terms  _X_ and  _Y_.

 
*/

/** @pred  _X_ \\=  _Y_ is iso 


Succeeds if terms  _X_ and  _Y_ are not unifiable.

 
*/

/** @pred  unify_with_occurs_check(?T1,?T2) is iso 


Obtain the most general unifier of terms  _T1_ and  _T2_, if there
is one.

This predicate implements the full unification algorithm. An example:n

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(B)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will succeed with the bindings `A = b` and `Z = f(B)`. On the
other hand:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unify_with_occurs_check(a(X,b,Z),a(X,A,f(Z)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would fail, because `Z` is not unifiable with `f(Z)`. Note that
`(=)/2` would succeed for the previous examples, giving the following
bindings `A = b` and `Z = f(Z)`.

 
*/

/** @pred  copy_term(? _TI_,- _TF_) is iso 


Term  _TF_ is a variant of the original term  _TI_, such that for
each variable  _V_ in the term  _TI_ there is a new variable  _V'_
in term  _TF_. Notice that:

  + suspended goals and attributes for attributed variables in _TI_ are also duplicated;
  + ground terms are shared between the new and the old term.

If you do not want any sharing to occur please use
duplicate_term/2.

 
*/

/** @pred  duplicate_term(? _TI_,- _TF_) 


Term  _TF_ is a variant of the original term  _TI_, such that
for each variable  _V_ in the term  _TI_ there is a new variable
 _V'_ in term  _TF_, and the two terms do not share any
structure. All suspended goals and attributes for attributed variables
in  _TI_ are also duplicated.

Also refer to copy_term/2.

 
*/

/** @pred  is_list(+ _List_) 


True when  _List_ is a proper list. That is,  _List_
is bound to the empty list (nil) or a term with functor '.' and arity 2.

 
*/

/** @pred  ? _Term1_ =@= ? _Term2_ 



Same as variant/2, succeeds if  _Term1_ and  _Term2_ are variant terms.

 
*/

/** @pred  subsumes_term(? _Subsumer_, ? _Subsumed_) 



Succeed if  _Submuser_ subsumes  _Subsuned_ but does not bind any
variable in  _Subsumer_.

 
*/

/** @pred  term_subsumer(? _T1_, ? _T2_, ? _Subsumer_) 



Succeed if  _Subsumer_ unifies with the least general
generalization over  _T1_ and
 _T2_.

 
*/

/** @pred  term_variables(? _Term_, - _Variables_) is iso 



Unify  _Variables_ with the list of all variables of term
 _Term_.  The variables occur in the order of their first
appearance when traversing the term depth-first, left-to-right.

 
*/

/** @pred  rational_term_to_tree(? _TI_,- _TF_) 


The term  _TF_ is a tree representation (without cycles) for the
Prolog term  _TI_. Loops are replaced by terms of the form
`_LOOP_( _LevelsAbove_)` where  _LevelsAbove_ is the size of
the loop.

 
*/

/** @pred  tree_to_rational_term(? _TI_,- _TF_) 


Inverse of rational_term_to_tree/2. The term  _TI_ is a tree representation (without
cycles) for the Prolog term  _TF_. Loops replace terms of the form
`_LOOP_( _LevelsAbove_)` where  _LevelsAbove_ is the size of
the loop.




@} */

/** @defgroup Predicates_on_Atoms Predicates on Atoms
@ingroup YAPBuiltins
@{

The following predicates are used to manipulate atoms:


 
*/

/** @pred  name( _A_, _L_) 


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ will
be unified with an atomic symbol and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 name(yap,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 L = [121,97,112].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 name(3,L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 L = [51].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  atom_chars(? _A_,? _L_) is iso 


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ must
be unifiable with an atom, and the argument  _L_ with the list of the
characters of  _A_.

 
*/

/** @pred  atom_codes(? _A_,? _L_) is iso 


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_ will
be unified with an atom and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

 
*/

/** @pred  atom_concat(+ _As_,? _A_) 


The predicate holds when the first argument is a list of atoms, and the
second unifies with the atom obtained by concatenating all the atoms in
the first list.

 
*/

/** @pred  atomic_concat(+ _As_,? _A_) 


The predicate holds when the first argument is a list of atomic terms, and
the second unifies with the atom obtained by concatenating all the
atomic terms in the first list. The first argument thus may contain
atoms or numbers.

 
*/

/** @pred  atomic_list_concat(+ _As_,? _A_) 


The predicate holds when the first argument is a list of atomic terms, and
the second unifies with the atom obtained by concatenating all the
atomic terms in the first list. The first argument thus may contain
atoms or numbers.

 
*/

/** @pred  atomic_list_concat(? _As_,+ _Separator_,? _A_)

Creates an atom just like atomic_list_concat/2, but inserts
 _Separator_ between each pair of atoms. For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- atomic_list_concat([gnu, gnat], ', ', A).

A = 'gnu, gnat'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

YAP emulates the SWI-Prolog version of this predicate that can also be
used to split atoms by instantiating  _Separator_ and  _Atom_ as
shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- atomic_list_concat(L, -, 'gnu-gnat').

L = [gnu, gnat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  atom_length(+ _A_,? _I_) is iso 


The predicate holds when the first argument is an atom, and the second
unifies with the number of characters forming that atom.

 
*/

/** @pred  atom_concat(? _A1_,? _A2_,? _A12_) is iso

The predicate holds when the third argument unifies with an atom, and
the first and second unify with atoms such that their representations
concatenated are the representation for  _A12_.

If  _A1_ and  _A2_ are unbound, the built-in will find all the atoms
that concatenated give  _A12_.

 
*/

/** @pred  number_chars(? _I_,? _L_) is iso 

The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _I_ must
be unifiable with a number, and the argument  _L_ with the list of the
characters of the external representation of  _I_.

 
*/

/** @pred  number_codes(? _A_,? _L_) is iso 


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _A_
will be unified with a number and  _L_ with the list of the ASCII
codes for the characters of the external representation of  _A_.

 
*/

/** @pred  atom_number(? _Atom_,? _Number_) 


The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). If the argument
 _Atom_ is an atom,  _Number_ must be the number corresponding
to the characters in  _Atom_, otherwise the characters in
 _Atom_ must encode a number  _Number_.

 
*/

/** @pred  number_atom(? _I_,? _L_) 



The predicate holds when at least one of the arguments is ground
(otherwise, an error message will be displayed). The argument  _I_ must
be unifiable with a number, and the argument  _L_ must be unifiable
with an atom representing the number.

 
*/

/** @pred  sub_atom(+ _A_,? _Bef_, ? _Size_, ? _After_, ? _At_out_) is iso 


True when  _A_ and  _At_out_ are atoms such that the name of
 _At_out_ has size  _Size_ and is a sub-string of the name of
 _A_, such that  _Bef_ is the number of characters before and
 _After_ the number of characters afterwards.

Note that  _A_ must always be known, but  _At_out_ can be unbound when
calling this built-in. If all the arguments for sub_atom/5 but  _A_
are unbound, the built-in will backtrack through all possible
sub-strings of  _A_.




@} */

/** @defgroup Predicates_on_Characters Predicates on Characters
@ingroup YAPBuiltins
@{

The following predicates are used to manipulate characters:


 
*/

/** @pred  char_code(? _A_,? _I_) is iso 


The built-in succeeds with  _A_ bound to character represented as an
atom, and  _I_ bound to the character code represented as an
integer. At least, one of either  _A_ or  _I_ must be bound before
the call.

 
*/

/** @pred  char_type(? _Char_, ? _Type_) 


Tests or generates alternative  _Types_ or  _Chars_. The
character-types are inspired by the standard `C`
`\<ctype.h\>` primitives.

  +  alnum
     _Char_ is a letter (upper- or lowercase) or digit.

  + alpha
    _Char_ is a letter (upper- or lowercase).

  + csym
    _Char_ is a letter (upper- or lowercase), digit or the underscore (_). These are valid C- and Prolog symbol characters.

  + csymf
    _Char_ is a letter (upper- or lowercase) or the underscore (_). These are valid first characters for C- and Prolog symbols

  + ascii
    _Char_ is a 7-bits ASCII character (0..127).

  + white
    _Char_ is a space or tab. E.i. white space inside a line.

  + cntrl
    _Char_ is an ASCII control-character (0..31).
 
  + digit
    _Char_ is a digit.

  + digit( _Weight_)
    _Char_ is a digit with value _Weight_. I.e. `char_type(X, digit(6))` yields `X =  '6'`. Useful for parsing numbers.

  + xdigit( _Weight_)
    _Char_ is a hexa-decimal digit with value  _Weight_. I.e. char_type(a, xdigit(X) yields X = '10'. Useful for parsing numbers.

  + graph
    _Char_ produces a visible mark on a page when printed. Note that the space is not included!

  + lower
    _Char_ is a lower-case letter.

  + lower(Upper)
    _Char_ is a lower-case version of  _Upper_. Only true if _Char_ is lowercase and  _Upper_ uppercase.

    + to_lower(Upper)
 _Char_ is a lower-case version of Upper. For non-letters, or letter without case,  _Char_ and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

    + upper
 _Char_ is an upper-case letter.

    + upper(Lower)
 _Char_ is an upper-case version of Lower. Only true if  _Char_ is uppercase and Lower lowercase.

    + to_upper(Lower)
 _Char_ is an upper-case version of Lower. For non-letters, or letter without case,  _Char_ and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

    + punct
 _Char_ is a punctuation character. This is a graph character that is not a letter or digit.

    + space
 _Char_ is some form of layout character (tab, vertical-tab, newline, etc.).

    + end_of_file
 _Char_ is -1.

    + end_of_line
 _Char_ ends a line (ASCII: 10..13).

    + newline
 _Char_ is a the newline character (10).

    + period
 _Char_ counts as the end of a sentence (.,!,?).

    + quote
 _Char_ is a quote-character (", ', `).

    + paren(Close)
 _Char_ is an open-parenthesis and Close is the corresponding close-parenthesis. 


    + code_type(? _Code_, ? _Type_) 


As char_type/2, but uses character-codes rather than
one-character atoms. Please note that both predicates are as
flexible as possible. They handle either representation if the
argument is instantiated and only will instantiate with an integer
code or one-character atom depending of the version used. See also
the prolog-flag double_quotes and the built-in predicates 
atom_chars/2 and atom_codes/2.




@} */

/** @defgroup Comparing_Terms Comparing Terms
@ingroup YAPBuiltins
@{

The following predicates are used to compare and order terms, using the
standard ordering:

    + 
variables come before numbers, numbers come before atoms which in turn
come before compound terms, i.e.: variables @\< numbers @\< atoms @\<
compound terms.
    + 
Variables are roughly ordered by "age" (the "oldest" variable is put
first);
    + 
Floating point numbers are sorted in increasing order;
    + 
Rational numbers are sorted in increasing order;
    + 
Integers are sorted in increasing order;
    + 
Atoms are sorted in lexicographic order;
    + 
Compound terms are ordered first by arity of the main functor, then by
the name of the main functor, and finally by their arguments in
left-to-right order.




 
*/

/** @pred  compare( _C_, _X_, _Y_) is iso 


As a result of comparing  _X_ and  _Y_,  _C_ may take one of
the following values:

    + 
`=` if  _X_ and  _Y_ are identical;
    + 
`\<` if  _X_ precedes  _Y_ in the defined order;
    + 
`\>` if  _Y_ precedes  _X_ in the defined order;


    + _X_ ==  _Y_ is iso 


Succeeds if terms  _X_ and  _Y_ are strictly identical. The
difference between this predicate and =/2 is that, if one of the
arguments is a free variable, it only succeeds when they have already
been unified.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X == Y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fails, but,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X = Y, X == Y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
succeeds.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X == 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fails, but,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- X = 2, X == 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
succeeds.

 
*/

/** @pred  _X_ \\==  _Y_ is iso 


Terms  _X_ and  _Y_ are not strictly identical.

 
*/

/** @pred  _X_ @\<  _Y_ is iso 


Term  _X_ precedes term  _Y_ in the standard order.

 
*/

/** @pred  _X_ @=\<  _Y_ is iso 


Term  _X_ does not follow term  _Y_ in the standard order.

 
*/

/** @pred  _X_ @\>  _Y_ is iso 


Term  _X_ follows term  _Y_ in the standard order.

 
*/

/** @pred  _X_ @\>=  _Y_ is iso 


Term  _X_ does not precede term  _Y_ in the standard order.

 
*/

/** @pred  sort(+ _L_,- _S_) is iso 


Unifies  _S_ with the list obtained by sorting  _L_ and  merging
identical (in the sense of `==`) elements.

 
*/

/** @pred  keysort(+ _L_, _S_) is iso 


Assuming L is a list of the form ` _Key_- _Value_`,
`keysort(+ _L_, _S_)` unifies  _S_ with the list obtained
from  _L_, by sorting its elements according to the value of
 _Key_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- keysort([3-a,1-b,2-c,1-a,1-b],S).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would return:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
S = [1-b,1-a,1-b,2-c,3-a]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  predsort(+ _Pred_, + _List_, - _Sorted_) 


Sorts similar to sort/2, but determines the order of two terms by
calling  _Pred_(- _Delta_, + _E1_, + _E2_) . This call must
unify  _Delta_ with one of `\<`, `\>` or `=`. If
built-in predicate compare/3 is used, the result is the same as
sort/2.

 
*/

/** @pred  length(? _L_,? _S_) 


Unify the well-defined list  _L_ with its length. The procedure can
be used to find the length of a pre-defined list, or to build a list
of length  _S_.




@} */

/** @defgroup Arithmetic Arithmetic
@ingroup YAPBuiltins
@{

@copydoc arithmetic 

  * See @ref arithmetic_preds for the predicates that implement arithment

  * See @ref arithmetic_cmps for the arithmetic comparisons supported in YAP

  * See @ref arithmetic_operators for how to call arithmetic operations in YAP 


@} */

/** @defgroup InputOutput Input/Output Predicates
@ingroup YAPBuiltins
@{

Some of the Input/Output predicates described below will in certain conditions
provide error messages and abort only if the file_errors flag is set.
If this flag is cleared the same predicates will just fail. Details on
setting and clearing this flag are given under 7.7.


@} */

/** @defgroup Streams_and_Files Handling Streams and Files
@ingroup YAPBuiltins
@{



 
*/

/** @pred  open(+ _F_,+ _M_,- _S_) is iso 


Opens the file with name  _F_ in mode  _M_ ('read', 'write' or
'append'), returning  _S_ unified with the stream name.

At most, there are 17 streams  opened at the same time. Each stream is
either an input or an output stream but not both. There are always 3
open streams:  user_input for reading, user_output for writing
and user_error for writing. If there is no  ambiguity, the atoms
user_input and user_output may be referred to as  `user`.

The `file_errors` flag controls whether errors are reported when in
mode 'read' or 'append' the file  _F_ does not exist or is not
readable, and whether in mode 'write' or 'append' the file is not
writable.

    + open(+ _F_,+ _M_,- _S_,+ _Opts_) is iso

Opens the file with name  _F_ in mode  _M_ ('read',  'write' or
'append'), returning  _S_ unified with the stream name, and following
these options:



    + type(+ _T_) is iso
Specify whether the stream is a `text` stream (default), or a
`binary` stream.

    + reposition(+ _Bool_) is iso
Specify whether it is possible to reposition the stream (`true`), or
not (`false`). By default, YAP enables repositioning for all
files, except terminal files and sockets.

    + eof_a
*/

/** @pred  n(+ _Action_) is iso
Specify the action to take if attempting to input characters from a
stream where we have previously found an `end_of_file`. The possible
actions are `error`, that raises an error, `reset`, that tries to
reset the stream and is used for `tty` type files, and `eof_code`,
which generates a new `end_of_file` (default for non-tty files).

    + alias(+ _Name_) is iso
Specify an alias to the stream. The alias <tt>Name</tt> must be an atom. The
alias can be used instead of the stream descriptor for every operation
concerning the stream.

The operation will fail and give an error if the alias name is already
in use. YAP allows several aliases for the same file, but only
one is returned by stream_property/2

    + bom(+ _Bool_)
If present and `true`, a BOM (<em>Byte Order Mark</em>) was
detected while opening the file for reading or a BOM was written while
opening the stream. See BOM for details.

    + encoding(+ _Encoding_)
Set the encoding used for text.  See Encoding for an overview of
wide character and encoding issues.

    + representation_errors(+ _Mode_)
Change the behaviour when writing characters to the stream that cannot
be represented by the encoding.  The behaviour is one of `error`
(throw and Input/Output error exception), `prolog` (write `\\u...\\`
escape code or `xml` (write `\&#...;` XML character entity).
The initial mode is `prolog` for the user streams and
`error` for all other streams. See also Encoding.

    + expand_filename(+ _Mode_)
If  _Mode_ is `true` then do filename expansion, then ask Prolog
to do file name expansion before actually trying to opening the file:
this includes processing `~` characters and processing `$`
environment variables at the beginning of the file. Otherwise, just try
to open the file using the given name.

The default behavior is given by the Prolog flag
open_expands_filename.



 
*/

/** @pred  close(+ _S_) is iso 


Closes the stream  _S_. If  _S_ does not stand for a stream
currently opened an error is reported. The streams user_input,
user_output, and user_error can never be closed.

 
*/

/** @pred  close(+ _S_,+ _O_) is iso

Closes the stream  _S_, following options  _O_. 

The only valid options are `force(true)` and `force(false)`.
YAP currently ignores these options.

 
*/

/** @pred  time_file(+ _File_,- _Time_) 


Unify the last modification time of  _File_ with
 _Time_.  _Time_ is a floating point number expressing the seconds
elapsed since Jan 1, 1970.

 
*/

/** @pred  access_file(+ _F_,+ _M_) 

Is the file accessible?

 
*/

/** @pred  file_base_name(+ _Name_,- _FileName_) 


Give the path a full path  _FullPath_ extract the  _FileName_.

 
*/

/** @pred  file_name_extension(? _Base_,? _Extension_, ? _Name_) 



This predicate is used to add, remove or test filename extensions. The
main reason for its introduction is to deal with different filename
properties in a portable manner. If the file system is
case-insensitive, testing for an extension will be done
case-insensitive too.  _Extension_ may be specified with or
without a leading dot (.). If an  _Extension_ is generated, it
will not have a leading dot.

 
*/

/** @pred  current_stream( _F_, _M_, _S_) 


Defines the relation: The stream  _S_ is opened on the file  _F_
in mode  _M_. It might be used to obtain all open streams (by
backtracking) or to access the stream for a file  _F_ in mode
 _M_, or to find properties for a stream  _S_. Notice that some
streams might not be associated to a file: in this case YAP tries to
return the file number. If that is not available, YAP unifies  _F_
with  _S_.

 
*/

/** @pred  is_stream( _S_) 


Succeeds if  _S_ is a currently open stream.

 
*/

/** @pred  flush_output is iso 


Send out all data in the output buffer of the current output stream.

 
*/

/** @pred  flush_output(+ _S_) is iso

Send all data in the output buffer for stream  _S_.

 
*/

/** @pred  set_input(+ _S_) is iso 


Set stream  _S_ as the current input stream. Predicates like read/1
and get/1 will start using stream  _S_.

 
*/

/** @pred  set_output(+ _S_) is iso 


Set stream  _S_ as the current output stream. Predicates like
write/1 and put/1 will start using stream  _S_.

 
*/

/** @pred  stream_select(+ _STREAMS_,+ _TIMEOUT_,- _READSTREAMS_) 


Given a list of open  _STREAMS_ opened in read mode and a  _TIMEOUT_
return a list of streams who are now available for reading. 

If the  _TIMEOUT_ is instantiated to `off`,
stream_select/3 will wait indefinitely for a stream to become
open. Otherwise the timeout must be of the form `SECS:USECS` where
`SECS` is an integer gives the number of seconds to wait for a timeout
and `USECS` adds the number of micro-seconds.

This built-in is only defined if the system call `select` is
available in the system.

 
*/

/** @pred  current_input(- _S_) is iso 


Unify  _S_ with the current input stream.

 
*/

/** @pred  current_output(- _S_) is iso 


Unify  _S_ with the current output stream.

 
*/

/** @pred  at_end_of_stream is iso 


Succeed if the current stream has stream position end-of-stream or
past-end-of-stream.

 
*/

/** @pred  at_end_of_stream(+ _S_) is iso

Succeed if the stream  _S_ has stream position end-of-stream or
past-end-of-stream. Note that  _S_ must be a readable stream.

 
*/

/** @pred  set_stream_position(+ _S_, + _POS_) is iso 


Given a stream position  _POS_ for a stream  _S_, set the current
stream position for  _S_ to be  _POS_.

 
*/

/** @pred  stream_property(? _Stream_,? _Prop_) is iso 



Obtain the properties for the open streams. If the first argument is
unbound, the procedure will backtrack through all open
streams. Otherwise, the first argument must be a stream term (you may
use `current_stream` to obtain a current stream given a file name).

The following properties are recognized:



    + file_name( _P_)
An atom giving the file name for the current stream. The file names are
user_input, user_output, and user_error for the
standard streams.

    + mode( _P_)
The mode used to open the file. It may be one of `append`,
`read`, or `write`.

    + input
The stream is readable.

    + output
The stream is writable.

    + alias( _A_)
ISO-Prolog primitive for stream aliases. <tt>YAP</tt> returns one of the
existing aliases for the stream.

    + position( _P_)
A term describing the position in the stream.

    + end_of_stream( _E_)
Whether the stream is `at` the end of stream, or it has found the
end of stream and is `past`, or whether it has `not` yet
reached the end of stream.

    + eof_action( _A_)
The action to take when trying to read after reaching the end of
stream. The action may be one of `error`, generate an error,
`eof_code`, return character code `-1`, or `reset` the
stream.

    + reposition( _B_)
Whether the stream can be repositioned or not, that is, whether it is
seekable.

    + type( _T_)
Whether the stream is a `text` stream or a `binary` stream.

    + bom(+ _Bool_)
If present and `true`, a BOM (<em>Byte Order Mark</em>) was
detected while opening the file for reading or a BOM was written while
opening the stream. See BOM for details.

    + encoding(+ _Encoding_)
Query the encoding used for text.  See Encoding for an
overview of wide character and encoding issues in YAP.

    + representation_errors(+ _Mode_)
Behaviour when writing characters to the stream that cannot be
represented by the encoding.  The behaviour is one of `error`
(throw and Input/Output error exception), `prolog` (write `\\u...\\`
escape code or `xml` (write `\&#...;` XML character entity).
The initial mode is `prolog` for the user streams and
`error` for all other streams. See also Encoding and
`open/4`.



    + current_line_number(- _LineNumber_) 


Unify  _LineNumber_ with the line number for the current stream.

 
*/

/** @pred  current_line_number(+ _Stream_,- _LineNumber_)

Unify  _LineNumber_ with the line number for the  _Stream_. 

 
*/

/** @pred  line_count(+ _Stream_,- _LineNumber_) 


Unify  _LineNumber_ with the line number for the  _Stream_.

 
*/

/** @pred  character_count(+ _Stream_,- _CharacterCount_) 


Unify  _CharacterCount_ with the number of characters written to or
read to  _Stream_.

 
*/

/** @pred  line_position(+ _Stream_,- _LinePosition_) 


Unify  _LinePosition_ with the position on current text stream
 _Stream_.

 
*/

/** @pred  stream_position(+ _Stream_,- _StreamPosition_) 


Unify  _StreamPosition_ with the packaged information of position on
current stream  _Stream_. Use stream_position_data/3 to
retrieve information on character or line count.

 
*/

/** @pred  stream_position_data(+ _Field_,+ _StreamPosition_,- _Info_) 


Given the packaged stream position term  _StreamPosition_, unify
 _Info_ with  _Field_ `line_count`, `byte_count`, or
`char_count`.




@} */

/** @defgroup ChYProlog_File_Handling C-Prolog File Handling
@ingroup YAPBuiltins
@{



 
*/

/** @pred  tell(+ _S_) 


If  _S_ is a currently opened stream for output, it becomes the
current output stream. If  _S_ is an atom it is taken to be a
filename.  If there is no output stream currently associated with it,
then it is opened for output, and the new output stream created becomes
the current output stream. If it is not possible to open the file, an
error occurs.  If there is a single opened output stream currently
associated with the file, then it becomes the current output stream; if
there are more than one in that condition, one of them is chosen.

Whenever  _S_ is a stream not currently opened for output, an error
may be reported, depending on the state of the file_errors flag. The
predicate just fails, if  _S_ is neither a stream nor an atom.

 
*/

/** @pred  telling(- _S_) 


The current output stream is unified with  _S_.

 
*/

/** @pred  told 


Closes the current output stream, and the user's terminal becomes again
the current output stream. It is important to remember to close streams
after having finished using them, as the maximum number of
simultaneously opened streams is 17.

 
*/

/** @pred  see(+ _S_) 


If  _S_ is a currently opened input stream then it is assumed to be
the current input stream. If  _S_ is an atom it is taken as a
filename. If there is no input stream currently associated with it, then
it is opened for input, and the new input stream thus created becomes
the current input stream. If it is not possible to open the file, an
error occurs.  If there is a single opened input stream currently
associated with the file, it becomes the current input stream; if there
are more than one in that condition, then one of them is chosen.

When  _S_ is a stream not currently opened for input, an error may be
reported, depending on the state of the `file_errors` flag. If
 _S_ is neither a stream nor an atom the predicates just fails.

 
*/

/** @pred  seeing(- _S_) 


The current input stream is unified with  _S_.

 
*/

/** @pred  seen 


Closes the current input stream (see 6.7.).




@} */

/** @defgroup InputOutput_of_Terms Handling Input/Output of Terms
@ingroup YAPBuiltins
@{



 
*/

/** @pred  read(- _T_) is iso 


Reads the next term from the current input stream, and unifies it with
 _T_. The term must be followed by a dot ('.') and any blank-character
as previously defined. The syntax of the term must match the current
declarations for operators (see op). If the end-of-stream is reached, 
 _T_ is unified with the atom `end_of_file`. Further reads from of 
the same stream may cause an error failure (see open/3).

    + read_term(- _T_,+ _Options_) is iso 


Reads term  _T_ from the current input stream with execution
controlled by the following options:



 
*/

/** @pred  term_position(- _Position_) 

Unify  _Position_ with a term describing the position of the stream
at the start of parse. Use stream_position_data/3 to obtain extra
information.

    + singletons(- _Names_) 

Unify  _Names_ with a list of the form  _Name=Var_, where
 _Name_ is the name of a non-anonymous singleton variable in the
original term, and `Var` is the variable's representation in
YAP.
The variables occur in left-to-right traversal order.

    + syntax_errors(+ _Val_) 

Control action to be taken after syntax errors. See yap_flag/2
for detailed information.

    + variable
*/

/** @pred  es(- _Names_) 

Unify  _Names_ with a list of the form  _Name=Var_, where  _Name_ is
the name of a non-anonymous variable in the original term, and  _Var_
is the variable's representation in YAP.
The variables occur in left-to-right traversal order.

    + variables(- _Names_) 

Unify  _Names_ with a list of the variables in term  _T_.
The variables occur in left-to-right traversal order.



    + char_conversion(+ _IN_,+ _OUT_) is iso 


While reading terms convert unquoted occurrences of the character
 _IN_ to the character  _OUT_. Both  _IN_ and  _OUT_ must be
bound to single characters atoms.

Character conversion only works if the flag `char_conversion` is
on. This is default in the `iso` and `sicstus` language
modes. As an example, character conversion can be used for instance to
convert characters from the ISO-LATIN-1 character set to ASCII.

If  _IN_ is the same character as  _OUT_, char_conversion/2
will remove this conversion from the table.

 
*/

/** @pred  current_char_conversion(? _IN_,? _OUT_) is iso 


If  _IN_ is unbound give all current character
translations. Otherwise, give the translation for  _IN_, if one
exists.

 
*/

/** @pred  write( _T_) is iso 


The term  _T_ is written to the current output stream according to
the operator declarations in force.

 
*/

/** @pred  writeln( _T_) is iso 


Same as write/1 followed by nl/0.

 
*/

/** @pred  display(+ _T_) 


Displays term  _T_ on the current output stream. All Prolog terms are
written in standard parenthesized prefix notation.

 
*/

/** @pred  write_canonical(+ _T_) is iso 


Displays term  _T_ on the current output stream. Atoms are quoted
when necessary, and operators are ignored, that is, the term is written
in standard parenthesized prefix notation.

 
*/

/** @pred  write_term(+ _T_, + _Opts_) is iso 


Displays term  _T_ on the current output stream, according to the
following options:

    + quoted(+ _Bool_) is iso
If `true`, quote atoms if this would be necessary for the atom to
be recognized as an atom by YAP's parser. The default value is
`false`.

    + ignore_ops(+ _Bool_) is iso
If `true`, ignore operator declarations when writing the term. The
default value is `false`.

    + numbervars(+ _Bool_) is iso
If `true`, output terms of the form
`'$VAR'(N)`, where  _N_ is an integer, as a sequence of capital
letters. The default value is `false`.

    + portrayed(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

    + portray(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

    + max_depth(+ _Depth_)
If `Depth` is a positive integer, use <tt>Depth</tt> as
the maximum depth to portray a term. The default is `0`, that is,
unlimited depth.

    + priority(+ _Piority_)
If `Priority` is a positive integer smaller than `1200`, 
give the context priority. The default is `1200`.

    + cycles(+ _Bool_)
Do not loop in rational trees (default).


 
*/

/** @pred  writeq( _T_) is iso 


Writes the term  _T_, quoting names to make the result acceptable to
the predicate 'read' whenever necessary.

 
*/

/** @pred  print( _T_) 


Prints the term  _T_ to the current output stream using write/1
unless T is bound and a call to the user-defined  predicate
`portray/1` succeeds. To do pretty  printing of terms the user should
define suitable clauses for `portray/1` and use print/1.

 
*/

/** @pred  format(+ _T_,+ _L_) 


Print formatted output to the current output stream. The arguments in
list  _L_ are output according to the string or atom  _T_.

A control sequence is introduced by a `w`. The following control
sequences are available in YAP:



    + '~~'
Print a single tilde.

    + '~a'
The next argument must be an atom, that will be printed as if by `write`.

    + '~Nc'
The next argument must be an integer, that will be printed as a
character code. The number  _N_ is the number of times to print the
character (default 1).

    + '~Ne'
    + '~NE'
    + '~Nf'
    + '~Ng'
    + '~NG'
The next argument must be a floating point number. The float  _F_, the number
 _N_ and the control code `c` will be passed to `printf` as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    printf("%s.Nc", F)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~8e, ~8E, ~8f, ~8g, ~8G~w",
          [3.14,3.14,3.14,3.14,3.14,3.14]).
3.140000e+00, 3.140000E+00, 3.140000, 3.14, 3.143.14
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~Nd'
The next argument must be an integer, and  _N_ is the number of digits
after the decimal point. If  _N_ is `0` no decimal points will be
printed. The default is  _N = 0_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2d, ~d",[15000, 15000]).
150.00, 15000
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~ND'
Identical to `'~Nd'`, except that commas are used to separate groups
of three digits.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2D, ~D",[150000, 150000]).
1,500.00, 150,000
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~i'
Ignore the next argument in the list of arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format('The ~i met the boregrove',[mimsy]).
The  met the boregrove
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~k'
Print the next argument with `write_canonical`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~k",a+[1,2]).
Good night +(a,[1,2])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~Nn'
Print  _N_ newlines (where  _N_ defaults to 1).

    + '~NN'
Print  _N_ newlines if at the beginning of the line (where  _N_
defaults to 1).

    + '~Nr'
The next argument must be an integer, and  _N_ is interpreted as a
radix, such that `2 \<= N \<= 36` (the default is 8).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249f0, 444760
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the letters `a-z` denote digits larger than 9.

    + '~NR'
Similar to '~NR'. The next argument must be an integer, and  _N_ is
interpreted as a radix, such that `2 \<= N \<= 36` (the default is 8).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249F0, 444760
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only difference is that letters `A-Z` denote digits larger than 9.

    + '~p'
Print the next argument with print/1:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~p",a+[1,2]).
Good night a+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~q'
Print the next argument with writeq/1:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~q",'Hello'+[1,2]).
Good night 'Hello'+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~Ns'
The next argument must be a list of character codes. The system then
outputs their representation as a string, where  _N_ is the maximum
number of characters for the string ( _N_ defaults to the length of the
string).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("The ~s are ~4s",["woods","lovely"]).
The woods are love
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + '~w'
Print the next argument with write/1:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- format("Good night ~w",'Hello'+[1,2]).
Good night Hello+[1,2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


The number of arguments, `N`, may be given as an integer, or it
may be given as an extra argument. The next example shows a small
procedure to write a variable number of `a` characters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_many_as(N) :-
        format("~*c",[N,0'a]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The format/2 built-in also allows for formatted output.  One can
specify column boundaries and fill the intermediate space by a padding
character: 

    + '~N|'
Set a column boundary at position  _N_, where  _N_ defaults to the
current position.

    + '~N+'
Set a column boundary at  _N_ characters past the current position, where
 _N_ defaults to `8`.

    + '~Nt'
Set padding for a column, where  _N_ is the fill code (default is
`SPC`).



The next example shows how to align columns and padding. We first show
left-alignment:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("~n*Hello~16+*~n",[]).
*Hello          *
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we reserve 16 characters for the column.

The following example shows how to do right-alignment:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("*~tHello~16+*~n",[]).
*          Hello*

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `~t` escape sequence forces filling before `Hello`. 

We next show how to do centering:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- format("*~tHello~t~16+*~n",[]).
*     Hello     *
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two `~t` escape sequence force filling both before and after
`Hello`. Space is then evenly divided between the right and the
left sides.

 
*/

/** @pred  format(+ _T_)

Print formatted output to the current output stream.

 
*/

/** @pred  format(+ _S_,+ _T_,+ _L_)

Print formatted output to stream  _S_.

 
*/

/** @pred  with_output_to(+ _Ouput_,: _Goal_) 


Run  _Goal_ as once/1, while characters written to the current
output are sent to  _Output_. The predicate is SWI-Prolog
specific.

Applications should generally avoid creating atoms by breaking and
concatenating other atoms as the creation of large numbers of
intermediate atoms generally leads to poor performance, even more so in
multi-threaded applications. This predicate supports creating
difference-lists from character data efficiently. The example below
defines the DCG rule `term/3` to insert a term in the output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 term(Term, In, Tail) :-
        with_output_to(codes(In, Tail), write(Term)).

?- phrase(term(hello), X).

X = [104, 101, 108, 108, 111]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + A Stream handle or alias
Temporary switch current output to the given stream. Redirection using with_output_to/2 guarantees the original output is restored, also if Goal fails or raises an exception. See also call_cleanup/2. 
    + atom(- _Atom_)
Create an atom from the emitted characters. Please note the remark above. 
    + string(- _String_)
Create a string-object (not supported in YAP). 
    + codes(- _Codes_)
Create a list of character codes from the emitted characters, similar to atom_codes/2. 
    + codes(- _Codes_, - _Tail_)
Create a list of character codes as a difference-list. 
    + chars(- _Chars_)
Create a list of one-character-atoms codes from the emitted characters, similar to atom_chars/2. 
    + chars(- _Chars_, - _Tail_)
Create a list of one-character-atoms as a difference-list. 





@} */

/** @defgroup InputOutput_of_Characters Handling Input/Output of Characters
@ingroup YAPBuiltins
@{



 
*/

/** @pred  put(+ _N_) 


Outputs to the current output stream the character whose ASCII code is
 _N_. The character  _N_ must be a legal ASCII character code, an
expression yielding such a code, or a list in which case only the first
element is used.

 
*/

/** @pred  put_byte(+ _N_) is iso 


Outputs to the current output stream the character whose code is
 _N_. The current output stream must be a binary stream.

 
*/

/** @pred  put_char(+ _N_) is iso 


Outputs to the current output stream the character who is used to build
the representation of atom `A`. The current output stream must be a
text stream.

 
*/

/** @pred  put_code(+ _N_) is iso 


Outputs to the current output stream the character whose ASCII code is
 _N_. The current output stream must be a text stream. The character
 _N_ must be a legal ASCII character code, an expression yielding such
a code, or a list in which case only the first element is used.

 
*/

/** @pred  get(- _C_) 


The next non-blank character from the current input stream is unified
with  _C_. Blank characters are the ones whose ASCII codes are not
greater than 32. If there are no more non-blank characters in the
stream,  _C_ is unified with -1. If `end_of_stream` has already
been reached in the previous reading, this call will give an error message.

 
*/

/** @pred  get0(- _C_) 


The next character from the current input stream is consumed, and then
unified with  _C_. There are no restrictions on the possible
values of the ASCII code for the character, but the character will be
internally converted by YAP.

 
*/

/** @pred  get_byte(- _C_) is iso 


If  _C_ is unbound, or is a character code, and the current stream is a
binary stream, read the next byte from the current stream and unify its
code with  _C_.

 
*/

/** @pred  get_char(- _C_) is iso 


If  _C_ is unbound, or is an atom representation of a character, and
the current stream is a text stream, read the next character from the
current stream and unify its atom representation with  _C_.

 
*/

/** @pred  get_code(- _C_) is iso 


If  _C_ is unbound, or is the code for a character, and
the current stream is a text stream, read the next character from the
current stream and unify its code with  _C_.

 
*/

/** @pred  peek_byte(- _C_) is iso 


If  _C_ is unbound, or is a character code, and the current stream is a
binary stream, read the next byte from the current stream and unify its
code with  _C_, while leaving the current stream position unaltered.

 
*/

/** @pred  peek_char(- _C_) is iso 


If  _C_ is unbound, or is an atom representation of a character, and
the current stream is a text stream, read the next character from the
current stream and unify its atom representation with  _C_, while
leaving the current stream position unaltered.

 
*/

/** @pred  peek_code(- _C_) is iso 


If  _C_ is unbound, or is the code for a character, and
the current stream is a text stream, read the next character from the
current stream and unify its code with  _C_, while
leaving the current stream position unaltered.

 
*/

/** @pred  skip(+ _N_) 


Skips input characters until the next occurrence of the character with
ASCII code  _N_. The argument to this predicate can take the same forms
as those for `put` (see 6.11).

 
*/

/** @pred  tab(+ _N_) 


Outputs  _N_ spaces to the current output stream.

 
*/

/** @pred  nl is iso 


Outputs a new line to the current output stream.




@} */

/** @defgroup InputOutput_for_Streams Input/Output Predicates applied to Streams
@ingroup YAPBuiltins
@{



 
*/

/** @pred  read(+ _S_,- _T_) is iso

Reads term  _T_ from the stream  _S_ instead of from the current input
stream.

 
*/

/** @pred  read_term(+ _S_,- _T_,+ _Options_) is iso

Reads term  _T_ from stream  _S_ with execution controlled by the
same options as read_term/2.

 
*/

/** @pred  write(+ _S_, _T_) is iso

Writes term  _T_ to stream  _S_ instead of to the current output
stream.

 
*/

/** @pred  write_canonical(+ _S_,+ _T_) is iso

Displays term  _T_ on the stream  _S_. Atoms are quoted when
necessary, and operators are ignored.

 
*/

/** @pred  write_term(+ _S_, + _T_, + _Opts_) is iso

Displays term  _T_ on the current output stream, according to the same
options used by `write_term/3`.

 
*/

/** @pred  writeq(+ _S_, _T_) is iso

As writeq/1, but the output is sent to the stream  _S_.

 
*/

/** @pred  display(+ _S_, _T_)

Like display/1, but using stream  _S_ to display the term.

 
*/

/** @pred  print(+ _S_, _T_)

Prints term  _T_ to the stream  _S_ instead of to the current output
stream.

 
*/

/** @pred  put(+ _S_,+ _N_)

As `put(N)`, but to stream  _S_.

 
*/

/** @pred  put_byte(+ _S_,+ _N_) is iso

As `put_byte(N)`, but to binary stream  _S_.

 
*/

/** @pred  put_char(+ _S_,+ _A_) is iso

As `put_char(A)`, but to text stream  _S_.

 
*/

/** @pred  put_code(+ _S_,+ _N_) is iso

As `put_code(N)`, but to text stream  _S_.

 
*/

/** @pred  get(+ _S_,- _C_)

The same as `get(C)`, but from stream  _S_.

 
*/

/** @pred  get0(+ _S_,- _C_)

The same as `get0(C)`, but from stream  _S_.

 
*/

/** @pred  get_byte(+ _S_,- _C_) is iso

If  _C_ is unbound, or is a character code, and the stream  _S_ is a
binary stream, read the next byte from that stream and unify its
code with  _C_.

 
*/

/** @pred  get_char(+ _S_,- _C_) is iso

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_.

 
*/

/** @pred  get_code(+ _S_,- _C_) is iso

If  _C_ is unbound, or is a character code, and the stream  _S_ is a
text stream, read the next character from that stream and unify its
code with  _C_.

 
*/

/** @pred  peek_byte(+ _S_,- _C_) is iso

If  _C_ is unbound, or is a character code, and  _S_ is a binary
stream, read the next byte from the current stream and unify its code
with  _C_, while leaving the current stream position unaltered.

 
*/

/** @pred  peek_char(+ _S_,- _C_) is iso

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_, while leaving
the current stream position unaltered.

 
*/

/** @pred  peek_code(+ _S_,- _C_) is iso

If  _C_ is unbound, or is an atom representation of a character, and
the stream  _S_ is a text stream, read the next character from that
stream and unify its representation as an atom with  _C_, while leaving
the current stream position unaltered.

 
*/

/** @pred  skip(+ _S_,- _C_)

Like skip/1, but using stream  _S_ instead of the current
input stream.

 
*/

/** @pred  tab(+ _S_,+ _N_)

The same as tab/1, but using stream  _S_.

 
*/

/** @pred  nl(+ _S_) is iso

Outputs a new line to stream  _S_.




@} */

/** @defgroup ChYProlog_to_Terminal Compatible C-Prolog predicates for Terminal Input/Output
@ingroup YAPBuiltins
@{



 
*/

/** @pred  ttyput(+ _N_) 


As `put(N)` but always to user_output.

 
*/

/** @pred  ttyget(- _C_) 


The same as `get(C)`, but from stream user_input.

 
*/

/** @pred  ttyget0(- _C_) 


The same as `get0(C)`, but from stream user_input.

 
*/

/** @pred  ttyskip(- _C_) 


Like skip/1, but always using stream user_input.
stream.

 
*/

/** @pred  ttytab(+ _N_) 


The same as tab/1, but using stream user_output.

 
*/

/** @pred  ttynl 


Outputs a new line to stream user_output.




@} */

/** @defgroup InputOutput_Control Controlling Input/Output
@ingroup YAPBuiltins
@{



 
*/

/** @pred  exists(+ _F_) 


Checks if file  _F_ exists in the current directory.

    + nofileerrors 


Switches off the file_errors flag, so that the predicates see/1,
tell/1, open/3 and close/1 just fail, instead of producing
an error message and aborting whenever the specified file cannot be
opened or closed.

    + fileerrors 


Switches on the file_errors flag so that in certain error conditions
Input/Output predicates will produce an appropriated message and abort.

    + always_prompt_user 


Force the system to prompt the user even if the user_input stream
is not a terminal. This command is useful if you want to obtain
interactive control from a pipe or a socket.




@} */

/** @defgroup Sockets Using Sockets From YAP
@ingroup YAPBuiltins
@{

YAP includes a SICStus Prolog compatible socket interface. In YAP-6.3
this uses the `clib` package to emulate the old low level interface that
provides direct access to the major socket system calls. These calls
can be used both to open a new connection in the network or connect to
a networked server. Socket connections are described as read/write
streams, and standard Input/Output built-ins can be used to write on or read
from sockets. The following calls are available:



 
*/

/** @pred  socket(+ _DOMAIN_,+ _TYPE_,+ _PROTOCOL_,- _SOCKET_) 


Corresponds to the BSD system call `socket`. Create a socket for
domain  _DOMAIN_ of type  _TYPE_ and protocol
 _PROTOCOL_. Both  _DOMAIN_ and  _TYPE_ should be atoms,
whereas  _PROTOCOL_ must be an integer.
The new socket object is
accessible through a descriptor bound to the variable  _SOCKET_.

The current implementation of YAP  accepts socket
domains `'AF_INET'` and `'AF_UNIX'`. 
Socket types depend on the
underlying operating system, but at least the following types are
supported: `'SOCK_STREAM'` and `'SOCK_DGRAM'` (untested in 6.3).

 
*/

/** @pred  socket(+ _DOMAIN_,- _SOCKET_)


Call socket/4 with  _TYPE_ bound to `'SOCK_STREAM'` and
 _PROTOCOL_ bound to `0`.

 
*/

/** @pred  socket_close(+ _SOCKET_) 



Close socket  _SOCKET_. Note that sockets used in
`socket_connect` (that is, client sockets) should not be closed with
`socket_close`, as they will be automatically closed when the
corresponding stream is closed with close/1 or `close/2`.

 
*/

/** @pred  socket_bind(+ _SOCKET_, ? _PORT_) 



Interface to system call `bind`, as used for servers: bind socket
to a port. Port information depends on the domain:

    + 'AF_UNIX'(+ _FILENAME_) (unsupported)
    + 'AF_FILE'(+ _FILENAME_)
use file name  _FILENAME_ for UNIX or local sockets.

    + 'AF_INET'(? _HOST_,?PORT)
If  _HOST_ is bound to an atom, bind to host  _HOST_, otherwise
if unbound bind to local host ( _HOST_ remains unbound). If port
 _PORT_ is bound to an integer, try to bind to the corresponding
port. If variable  _PORT_ is unbound allow operating systems to
choose a port number, which is unified with  _PORT_.



 
*/

/** @pred  socket_connect(+ _SOCKET_, + _PORT_, - _STREAM_) 



Interface to system call `connect`, used for clients: connect
socket  _SOCKET_ to  _PORT_. The connection results in the
read/write stream  _STREAM_.

Port information depends on the domain:

    + 'AF_UNIX'(+ _FILENAME_)
    + 'AF_FILE'(+ _FILENAME_)
connect to socket at file  _FILENAME_.

    + 'AF_INET'(+ _HOST_,+ _PORT_)
Connect to socket at host  _HOST_ and port  _PORT_.


 
*/

/** @pred  socket_listen(+ _SOCKET_, + _LENGTH_) 


Interface to system call `listen`, used for servers to indicate
willingness to wait for connections at socket  _SOCKET_. The
integer  _LENGTH_ gives the queue limit for incoming connections,
and should be limited to `5` for portable applications. The socket
must be of type `SOCK_STREAM` or `SOCK_SEQPACKET`.

 
*/

/** @pred  socket_accept(+ _SOCKET_, - _CLIENT_, - _STREAM_) 


Interface to system call `accept`, used for servers to wait for
connections at socket  _SOCKET_. The stream descriptor  _STREAM_
represents the resulting connection.  If the socket belongs to the
domain `'AF_INET'`,  _CLIENT_ unifies with an atom containing
the IP address for the client in numbers and dots notation.

 
*/

/** @pred  socket_accept(+ _SOCKET_, - _STREAM_)

Accept a connection but do not return client information.

 
*/

/** @pred  socket_buffering(+ _SOCKET_, - _MODE_, - _OLD_, + _NEW_) 


Set buffering for  _SOCKET_ in `read` or `write`
 _MODE_.  _OLD_ is unified with the previous status, and  _NEW_
receives the new status which may be one of `unbuf` or
`fullbuf`.

 
*/

/** @pred  socket_select(+ _SOCKETS_, - _NEWSTREAMS_, + _TIMEOUT_, 

+ _STREAMS_, - _READSTREAMS_) [unsupported in YAP-6.3]

Interface to system call `select`, used for servers to wait for
connection requests or for data at sockets. The variable
 _SOCKETS_ is a list of form  _KEY-SOCKET_, where  _KEY_ is
an user-defined identifier and  _SOCKET_ is a socket descriptor. The
variable  _TIMEOUT_ is either `off`, indicating execution will
wait until something is available, or of the form  _SEC-USEC_, where
 _SEC_ and  _USEC_ give the seconds and microseconds before
socket_select/5 returns. The variable  _SOCKETS_ is a list of
form  _KEY-STREAM_, where  _KEY_ is an user-defined identifier
and  _STREAM_ is a stream descriptor

Execution of socket_select/5 unifies  _READSTREAMS_ from
 _STREAMS_ with readable data, and  _NEWSTREAMS_ with a list of
the form  _KEY-STREAM_, where  _KEY_ was the key for a socket
with pending data, and  _STREAM_ the stream descriptor resulting
from accepting the connection.  

 
*/

/** @pred  current_host(? _HOSTNAME_) 

Unify  _HOSTNAME_ with an atom representing the fully qualified
hostname for the current host. Also succeeds if  _HOSTNAME_ is bound
to the unqualified hostname.

 
*/

/** @pred  hostname_address(? _HOSTNAME_,? _IP_ADDRESS_) 

 _HOSTNAME_ is an host name and  _IP_ADDRESS_ its IP
address in number and dots notation.




@} */

/** @defgroup Database Using the Clausal Data Base
@ingroup YAPBuiltins
@{

Predicates in YAP may be dynamic or static. By default, when
consulting or reconsulting, predicates are assumed to be static:
execution is faster and the code will probably use less space.
Static predicates impose some restrictions: in general there can be no 
addition or removal of  clauses for a procedure if it is being used in the
current execution.

Dynamic predicates allow programmers to change the Clausal Data Base with
the same flexibility as in C-Prolog. With dynamic predicates it is
always possible to add or remove clauses during execution and the
semantics will be the same as for C-Prolog. But the programmer should be
aware of the fact that asserting or retracting are still expensive operations, 
and therefore he should try to avoid them whenever possible.



 
*/

/** @pred  dynamic + _P_ 


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate.  _P_ must be written in form:
 _name/arity_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic god/1.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
a more convenient form can be used:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic son/3, father/2, mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

or, equivalently,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- dynamic [son/3, father/2, mother/2].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note:

a predicate is assumed to be dynamic when 
asserted before being defined.

 
*/

/** @pred  dynamic_predicate(+ _P_,+ _Semantics_) 


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate following either `logical` or
`immediate` semantics.

 
*/

/** @pred  compile_predicates(: _ListOfNameArity_) 



Compile a list of specified dynamic predicates (see dynamic/1 and
assert/1 into normal static predicates. This call tells the
Prolog environment the definition will not change anymore and further
calls to assert/1 or retract/1 on the named predicates
raise a permission error. This predicate is designed to deal with parts
of the program that is generated at runtime but does not change during
the remainder of the program execution.




@} */

/** @defgroup Modifying_the_Database Modification of the Data Base
@ingroup YAPBuiltins
@{

These predicates can be used either for static or for dynamic
predicates:



 
*/

/** @pred  assert(+ _C_) 


Same as assertz/1. Adds clause  _C_ to the program. If the predicate is undefined,
declare it as dynamic. New code should use assertz/1 for better portability.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates, as long as the predicate is not
in use and the language flag is <tt>cprolog</tt>. Note that this feature is
deprecated, if you want to assert clauses for static procedures you
should use assert_static/1.

 
*/

/** @pred  asserta(+ _C_) is iso 


Adds clause  _C_ to the beginning of the program. If the predicate is
undefined, declare it as dynamic.

 
*/

/** @pred  assertz(+ _C_) is iso 


Adds clause  _C_ to the end of the program. If the predicate is
undefined, declare it as dynamic.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates. The current version of YAP
supports this feature, but this feature is deprecated and support may go
away in future versions.

 
*/

/** @pred  abolish(+ _PredSpec_) is iso 


Deletes the predicate given by  _PredSpec_ from the database. If
 _PredSpec_ is an unbound variable, delete all predicates for the
current module. The
specification must include the name and arity, and it may include module
information. Under <tt>iso</tt> language mode this built-in will only abolish
dynamic procedures. Under other modes it will abolish any procedures. 

 
*/

/** @pred  abolish(+ _P_,+ _N_)

Deletes the predicate with name  _P_ and arity  _N_. It will remove
both static and dynamic predicates.

 
*/

/** @pred  assert_static(: _C_) 


Adds clause  _C_ to a static procedure. Asserting a static clause
for a predicate while choice-points for the predicate are available has
undefined results.

 
*/

/** @pred  asserta_static(: _C_) 


Adds clause  _C_ to the beginning of a static procedure. 

 
*/

/** @pred  assertz_static(: _C_) 


Adds clause  _C_ to the end of a static procedure.  Asserting a
static clause for a predicate while choice-points for the predicate are
available has undefined results.



The following predicates can be used for dynamic predicates and for
static predicates, if source mode was on when they were compiled:



 
*/

/** @pred  clause(+ _H_, _B_) is iso 


A clause whose head matches  _H_ is searched for in the
program. Its head and body are respectively unified with  _H_ and
 _B_. If the clause is a unit clause,  _B_ is unified with
 _true_.

This predicate is applicable to static procedures compiled with
`source` active, and to all dynamic procedures.

 
*/

/** @pred  clause(+ _H_, _B_,- _R_)

The same as clause/2, plus  _R_ is unified with the
reference to the clause in the database. You can use instance/2
to access the reference's value. Note that you may not use
erase/1 on the reference on static procedures.

 
*/

/** @pred  nth_clause(+ _H_, _I_,- _R_) 


Find the  _I_th clause in the predicate defining  _H_, and give
a reference to the clause. Alternatively, if the reference  _R_ is
given the head  _H_ is unified with a description of the predicate
and  _I_ is bound to its position.



The following predicates can only be used for dynamic predicates:



 
*/

/** @pred  retract(+ _C_) is iso 


Erases the first clause in the program that matches  _C_. This
predicate may also be used for the static predicates that have been
compiled when the source mode was `on`. For more information on
source/0 ( (see Setting the Compiler)).

 
*/

/** @pred  retractall(+ _G_) is iso 


Retract all the clauses whose head matches the goal  _G_. Goal
 _G_ must be a call to a dynamic predicate.




@} */

/** @defgroup Looking_at_the_Database Looking at the Data Base
@ingroup YAPBuiltins
@{



 
*/

/** @pred  listing 


Lists in the current output stream all the clauses for which source code
is available (these include all clauses for dynamic predicates and
clauses for static predicates compiled when source mode was `on`).

 
*/

/** @pred  listing(+ _P_)

Lists predicate  _P_ if its source code is available.

 
*/

/** @pred  portray_clause(+ _C_) 


Write clause  _C_ as if written by listing/0.

 
*/

/** @pred  portray_clause(+ _S_,+ _C_)

Write clause  _C_ on stream  _S_ as if written by listing/0.

 
*/

/** @pred  current_atom( _A_) 


Checks whether  _A_ is a currently defined atom. It is used to find all
currently defined atoms by backtracking.

 
*/

/** @pred  current_predicate( _F_) is iso 


 _F_ is the predicate indicator for a currently defined user or
library predicate.  _F_ is of the form  _Na/Ar_, where the atom
 _Na_ is the name of the predicate, and  _Ar_ its arity.

 
*/

/** @pred  current_predicate( _A_, _P_)

Defines the relation:  _P_ is a currently defined predicate whose
name is the atom  _A_.

 
*/

/** @pred  system_predicate( _A_, _P_) 


Defines the relation:   _P_ is a built-in predicate whose name
is the atom  _A_.

 
*/

/** @pred  predicate_property( _P_, _Prop_) is iso 


For the predicates obeying the specification  _P_ unify  _Prop_
with a property of  _P_. These properties may be:

    + built_in 

true for built-in predicates,
    + dynamic
true if the predicate is dynamic
    + static 

true if the predicate is static
    + meta_predicate( _M_) 

true if the predicate has a meta_predicate declaration  _M_.
    + multifile 

true if the predicate was declared to be multifile
    + imported_from( _Mod_) 

true if the predicate was imported from module  _Mod_.
    + exported 

true if the predicate is exported in the current module.
    + public
true if the predicate is public; note that all dynamic predicates are
public.
    + tabled 

true if the predicate is tabled; note that only static predicates can
be tabled in YAP.
    + source (predicate_property flag) 

true if source for the predicate is available.
    + number_of_clauses( _ClauseCount_) 

Number of clauses in the predicate definition. Always one if external
or built-in.


 
*/

/** @pred  predicate_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  


Given predicate  _P_,  _NCls_ is the number of clauses for
 _P_,  _Sz_ is the amount of space taken to store those clauses
(in bytes), and  _IndexSz_ is the amount of space required to store
indices to those clauses (in bytes).

 
*/

/** @pred  predicate_erased_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  


Given predicate  _P_,  _NCls_ is the number of erased clauses for
 _P_ that could not be discarded yet,  _Sz_ is the amount of space
taken to store those clauses (in bytes), and  _IndexSz_ is the amount
of space required to store indices to those clauses (in bytes).




@} */

/** @defgroup Database_References Using Data Base References
@ingroup YAPBuiltins
@{

Data Base references are a fast way of accessing terms. The predicates
erase/1 and `instance/1` also apply to these references and may
sometimes be used instead of retract/1 and clause/2.



 
*/

/** @pred  assert(+ _C_,- _R_)

The same as `assert(C)` ( (see Modifying the Database)) but
unifies  _R_ with the  database reference that identifies the new
clause, in a one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/

/** @pred  asserta(+ _C_,- _R_)

The same as `asserta(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/

/** @pred  assertz(+ _C_,- _R_)

The same as `assertz(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/

/** @pred  retract(+ _C_,- _R_)

Erases from the program the clause  _C_ whose 
database reference is  _R_. The predicate must be dynamic.




@} */

/** @defgroup Internal_Database Internal Data Base
@ingroup YAPBuiltins
@{

Some programs need global information for, e.g. counting or collecting 
data obtained by backtracking. As a rule, to keep this information, the
internal data base should be used instead of asserting and retracting
clauses (as most novice programmers  do), .
In YAP (as in some other Prolog systems) the internal data base (i.d.b. 
for short) is faster, needs less space and provides a better insulation of 
program and data than using asserted/retracted clauses.
The i.d.b. is implemented as a set of terms, accessed by keys that 
unlikely what happens in (non-Prolog) data bases are not part of the 
term. Under each key a list of terms is kept. References are provided so that 
terms can be identified: each term in the i.d.b. has a unique reference 
(references are also available for clauses of dynamic predicates).



 
*/

/** @pred  recorda(+ _K_, _T_,- _R_) 


Makes term  _T_ the first record under key  _K_ and  unifies  _R_
with its reference.

 
*/

/** @pred  recordz(+ _K_, _T_,- _R_) 


Makes term  _T_ the last record under key  _K_ and unifies  _R_
with its reference.

 
*/

/** @pred  recorda_at(+ _R0_, _T_,- _R_) 


Makes term  _T_ the record preceding record with reference
 _R0_, and unifies  _R_ with its reference.

 
*/

/** @pred  recordz_at(+ _R0_, _T_,- _R_) 


Makes term  _T_ the record following record with reference
 _R0_, and unifies  _R_ with its reference.

 
*/

/** @pred  recordaifnot(+ _K_, _T_,- _R_) 


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

 
*/

/** @pred  recordzifnot(+ _K_, _T_,- _R_) 


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

This predicate is YAP specific.

 
*/

/** @pred  recorded(+ _K_, _T_, _R_) 


Searches in the internal database under the key  _K_, a term that
unifies with  _T_ and whose reference matches  _R_. This
built-in may be used in one of two ways:

    + _K_ may be given, in this case the built-in will return all
elements of the internal data-base that match the key.
    + _R_ may be given, if so returning the key and element that
match the reference.


 
*/

/** @pred  erase(+ _R_) 


The term referred to by  _R_ is erased from the internal database. If
reference  _R_ does not exist in the database, `erase` just fails.

 
*/

/** @pred  erased(+ _R_) 


Succeeds if the object whose database reference is  _R_ has been
erased.

 
*/

/** @pred  instance(+ _R_,- _T_) 


If  _R_ refers to a clause or a recorded term,  _T_ is unified
with its most general instance. If  _R_ refers to an unit clause
 _C_, then  _T_ is unified with ` _C_ :- true`. When
 _R_ is not a reference to an existing clause or to a recorded term,
this goal fails.

 
*/

/** @pred  eraseall(+ _K_) 


All terms belonging to the key `K` are erased from the internal
database. The predicate always succeeds.

 
*/

/** @pred  current_key(? _A_,? _K_) 


Defines the relation:  _K_ is a currently defined database key whose
name is the atom  _A_. It can be used to generate all the keys for
the internal data-base.

 
*/

/** @pred  nth_instance(? _Key_,? _Index_,? _R_) 


Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.

 
*/

/** @pred  nth_instance(? _Key_,? _Index_, _T_,? _R_)

Fetches the  _Index_nth entry in the internal database under the key
 _Key_. Entries are numbered from one. If the key  _Key_ or the
 _Index_ are bound, a reference is unified with  _R_. Otherwise,
the reference  _R_ must be given, and YAP will find
the matching key and index.

 
*/

/** @pred  key_statistics(+ _K_,- _Entries_,- _Size_,- _IndexSize_) 


Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on entries,  _Size_, and what is the amount of
space spent in indices.

 
*/

/** @pred  key_statistics(+ _K_,- _Entries_,- _TotalSize_)

Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on this key.

 
*/

/** @pred  get_value(+ _A_,- _V_) 


In YAP, atoms can be associated with constants. If one such
association exists for atom  _A_, unify the second argument with the
constant. Otherwise, unify  _V_ with `[]`.

This predicate is YAP specific.

 
*/

/** @pred  set_value(+ _A_,+ _C_) 


Associate atom  _A_ with constant  _C_.

The `set_value` and `get_value` built-ins give a fast alternative to
the internal data-base. This is a simple form of implementing a global
counter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       read_and_increment_counter(Value) :-
                get_value(counter, Value),
                Value1 is Value+1,
                set_value(counter, Value1).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This predicate is YAP specific.



There is a strong analogy between the i.d.b. and the way dynamic 
predicates are stored. In fact, the main i.d.b. predicates might be 
implemented using dynamic predicates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
recorda(X,T,R) :- asserta(idb(X,T),R).
recordz(X,T,R) :- assertz(idb(X,T),R).
recorded(X,T,R) :- clause(idb(X,T),R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can take advantage of this, the other way around, as it is quite 
easy to write a simple Prolog interpreter, using the i.d.b.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
asserta(G) :- recorda(interpreter,G,_).
assertz(G) :- recordz(interpreter,G,_).
retract(G) :- recorded(interpreter,G,R), !, erase(R).
call(V) :- var(V), !, fail.
call((H :- B)) :- !, recorded(interpreter,(H :- B),_), call(B).
call(G) :- recorded(interpreter,G,_).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In YAP, much attention has been given to the implementation of the 
i.d.b., especially to the problem of accelerating the access to terms kept in 
a large list under the same key. Besides using the key, YAP uses an internal 
lookup function, transparent to the user, to find only the terms that might 
unify. For instance, in a data base containing the terms

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b
b(a)
c(d)
e(g)
b(X)
e(h)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stored under the key k/1, when executing the query 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- recorded(k(_),c(_),R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`recorded` would proceed directly to the third term, spending almost the 
time as if `a(X)` or `b(X)` was being searched.
The lookup function uses the functor of the term, and its first three
arguments (when they exist). So, `recorded(k(_),e(h),_)` would go
directly to the last term, while `recorded(k(_),e(_),_)` would find
first the fourth term, and then, after backtracking, the last one.

This mechanism may be useful to implement a sort of hierarchy, where 
the functors of the terms (and eventually the first arguments) work as 
secondary keys.

In the YAP's i.d.b. an optimized representation is used for 
terms without free variables. This results in a faster retrieval of terms 
and better space usage. Whenever possible, avoid variables in terms in terms stored in the  i.d.b.


@} */

/** @defgroup BlackBoard The Blackboard
@ingroup YAPBuiltins
@{

YAP implements a blackboard in the style of the SICStus Prolog
blackboard. The blackboard uses the same underlying mechanism as the
internal data-base but has several important differences:

    + It is module aware, in contrast to the internal data-base.
    + Keys can only be atoms or integers, and not compound terms.
    + A single term can be stored per key.
    + An atomic update operation is provided; this is useful for
parallelism.



 
*/

/** @pred  bb_put(+ _Key_,? _Term_) 


Store term table  _Term_ in the blackboard under key  _Key_. If a
previous term was stored under key  _Key_ it is simply forgotten.

 
*/

/** @pred  bb_get(+ _Key_,? _Term_) 


Unify  _Term_ with a term stored in the blackboard under key
 _Key_, or fail silently if no such term exists.

 
*/

/** @pred  bb_delete(+ _Key_,? _Term_) 


Delete any term stored in the blackboard under key  _Key_ and unify
it with  _Term_. Fail silently if no such term exists.

 
*/

/** @pred  bb_update(+ _Key_,? _Term_,? _New_) 


Atomically  unify a term stored in the blackboard under key  _Key_
with  _Term_, and if the unification succeeds replace it by
 _New_. Fail silently if no such term exists or if unification fails.




@} */

/** @defgroup Sets Collecting Solutions to a Goal
@ingroup YAPBuiltins
@{

When there are several solutions to a goal, if the user wants to collect all
the solutions he may be led to use the data base, because backtracking will
forget previous solutions.

YAP allows the programmer to choose from several system
predicates instead of writing his own routines.  findall/3 gives you
the fastest, but crudest solution. The other built-in predicates
post-process the result of the query in several different ways:



 
*/

/** @pred  findall( _T_,+ _G_,- _L_) is iso 


Unifies  _L_ with a list that contains all the instantiations of the
term  _T_ satisfying the goal  _G_.

With the following program:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a(2,1).
a(1,1).
a(2,2).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the answer to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
findall(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = _33
L = [2,1,2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  findall( _T_,+ _G_,+ _L_,- _L0_)

Similar to findall/3, but appends all answers to list  _L0_.

 
*/

/** @pred  all( _T_,+ _G_,- _L_) 


Similar to `findall( _T_, _G_, _L_)` but eliminate
repeated elements. Thus, assuming the same clauses as in the above
example, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = _33
L = [2,1];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that all/3 will fail if no answers are found.

 
*/

/** @pred  bagof( _T_,+ _G_,- _L_) is iso 


For each set of possible instances of the free variables occurring in
 _G_ but not in  _T_, generates the list  _L_ of the instances of
 _T_ satisfying  _G_. Again, assuming the same clauses as in the
examples above, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bagof(X,a(X,Y),L).

would be:
X = _32
Y = 1
L = [2,1];
X = _32
Y = 2
L = [2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  setof( _X_,+ _P_,- _B_) is iso 


Similar to `bagof( _T_, _G_, _L_)` but sorts list
 _L_ and keeping only one copy of each element.  Again, assuming the
same clauses as in the examples above, the reply to the query

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setof(X,a(X,Y),L).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would be:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X = _32
Y = 1
L = [1,2];
X = _32
Y = 2
L = [2];
no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup Grammars Grammar Rules
@ingroup YAPBuiltins
@{

Grammar rules in Prolog are both a convenient way to express definite
clause grammars and  an extension of the well known context-free grammars.

A grammar rule is of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head --> body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where both \a head and \a body are sequences of one or more items
linked by the standard conjunction operator ','.

<em>Items can be:</em>

    + 
a <em>non-terminal</em> symbol may be either a complex term or an atom.
    + 
a <em>terminal</em> symbol may be any Prolog symbol. Terminals are
written as Prolog lists.
    + 
an <em>empty body</em> is written as the empty list '[ ]'.
    + 
<em>extra conditions</em> may be inserted as Prolog procedure calls, by being
written inside curly brackets '{' and '}'.
    + 
the left side of a rule consists of a nonterminal and an optional list
of terminals.
    + 
alternatives may be stated in the right-hand side of the rule by using
the disjunction operator ';'.
    + 
the <em>cut</em> and <em>conditional</em> symbol ('-\>') may be inserted in the 
right hand side of a grammar rule


Grammar related built-in predicates:



 
*/

/** @pred  expand_term( _T_,- _X_) 



This predicate is used by YAP for preprocessing each top level
term read when consulting a file and before asserting or executing it.
It rewrites a term  _T_ to a term  _X_ according to the following
rules: first try term_expansion/2  in the current module, and then try to use the user defined predicate
`user:term_expansion/2`. If this call fails then the translating process
for DCG rules is applied, together with the arithmetic optimizer
whenever the compilation of arithmetic expressions is in progress.

 
*/

/** @pred  _CurrentModule_:term_expansion( _T_,- _X_),  user:term_expansion( _T_,- _X_) 


This user-defined predicate is called by `expand_term/3` to
preprocess all terms read when consulting a file. If it succeeds:

    + 
If  _X_ is of the form `:- G` or `?- G`, it is processed as
a directive.
    + 
If  _X_ is of the form `'$source_location'( _File_, _Line_): _Clause_` it is processed as if from `File` and line `Line`.

    + 
If  _X_ is a list, all terms of the list are asserted or processed
as directives.
    + The term  _X_ is asserted instead of  _T_.


 
*/

/** @pred  _CurrentModule_:goal_expansion(+ _G_,+ _M_,- _NG_), user:goal_expansion(+ _G_,+ _M_,- _NG_) 


YAP now supports goal_expansion/3. This is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If goal_expansion/3 succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
way. If goal_expansion/3 fails the system will use the default
rules.

 
*/

/** @pred  phrase(+ _P_, _L_, _R_) 


This predicate succeeds when the difference list ` _L_- _R_`
is a phrase of type  _P_.

 
*/

/** @pred  phrase(+ _P_, _L_)

This predicate succeeds when  _L_ is a phrase of type  _P_. The
same as `phrase(P,L,[])`.

Both this predicate and the previous are used as a convenient way to
start execution of grammar rules.

 
*/

/** @pred  'C'( _S1_, _T_, _S2_) 


This predicate is used by the grammar rules compiler and is defined as
`'C'([H|T],H,T)`.




@} */

/** @defgroup OS Access to Operating System Functionality
@ingroup YAPBuiltins
@{

The following built-in predicates allow access to underlying
Operating System functionality: 



 
*/

/** @pred  cd(+ _D_) 


Changes the current directory (on UNIX environments).

 
*/

/** @pred  cd

Changes the current directory (on UNIX environments) to the user's home directory.

 
*/

/** @pred  environ(+ _E_,- _S_) 





Given an environment variable  _E_ this predicate unifies the second argument  _S_ with its value.

 
*/

/** @pred  getcwd(- _D_) 


Unify the current directory, represented as an atom, with the argument
 _D_.

 
*/

/** @pred  pwd 


Prints the current directory.

 
*/

/** @pred  ls 


Prints a list of all files in the current directory.

 
*/

/** @pred  putenv(+ _E_,+ _S_) 


Set environment variable  _E_ to the value  _S_. If the
environment variable  _E_ does not exist, create a new one. Both the
environment variable and the value must be atoms.

 
*/

/** @pred  rename(+ _F_,+ _G_) 


Renames file  _F_ to  _G_.

 
*/

/** @pred  sh 


Creates a new shell interaction.

 
*/

/** @pred  system(+ _S_) 


Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.

 
*/

/** @pred  unix(+ _S_) 


Access to Unix-like functionality:

    + argv/1
Return a list of arguments to the program. These are the arguments that
follow a `--`, as in the usual Unix convention.
    + cd/0
Change to home directory.
    + cd/1
Change to given directory. Acceptable directory names are strings or
atoms.
    + environ/2
If the first argument is an atom, unify the second argument with the
value of the corresponding environment variable.
    + getcwd/1
Unify the first argument with an atom representing the current directory.
    + putenv/2
Set environment variable  _E_ to the value  _S_. If the
environment variable  _E_ does not exist, create a new one. Both the
environment variable and the value must be atoms.
    + shell/1
Execute command under current shell. Acceptable commands are strings or
atoms.
    + system/1
Execute command with `/bin/sh`. Acceptable commands are strings or
atoms.
    + shell/0
Execute a new shell.


 
*/

/** @pred  working_directory(- _CurDir_,? _NextDir_) 


Fetch the current directory at  _CurDir_. If  _NextDir_ is bound
to an atom, make its value the current working directory.

 
*/

/** @pred  alarm(+ _Seconds_,+ _Callable_,+ _OldAlarm_) 


Arranges for YAP to be interrupted in  _Seconds_ seconds, or in
[ _Seconds_| _MicroSeconds_]. When interrupted, YAP will execute
 _Callable_ and then return to the previous execution. If
 _Seconds_ is `0`, no new alarm is scheduled. In any event,
any previously set alarm is canceled.

The variable  _OldAlarm_ unifies with the number of seconds remaining
until any previously scheduled alarm was due to be delivered, or with
`0` if there was no previously scheduled alarm.

Note that execution of  _Callable_ will wait if YAP is
executing built-in predicates, such as Input/Output operations.

The next example shows how  _alarm/3_ can be used to implement a
simple clock:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop :- loop.

ticker :- write('.'), flush_output,
          get_value(tick, yes),
          alarm(1,ticker,_).

:- set_value(tick, yes), alarm(1,ticker,_), loop.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The clock, `ticker`, writes a dot and then checks the flag
`tick` to see whether it can continue ticking. If so, it calls
itself again. Note that there is no guarantee that the each dot
corresponds a second: for instance, if the YAP is waiting for
user input, `ticker` will wait until the user types the entry in.

The next example shows how alarm/3 can be used to guarantee that
a certain procedure does not take longer than a certain amount of time:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop :- loop.

:-   catch((alarm(10, throw(ball), _),loop),
        ball,
        format('Quota exhausted.~n',[])).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this case after `10` seconds our `loop` is interrupted,
`ball` is thrown,  and the handler writes `Quota exhausted`.
Execution then continues from the handler.

Note that in this case `loop/0` always executes until the alarm is
sent. Often, the code you are executing succeeds or fails before the
alarm is actually delivered. In this case, you probably want to disable
the alarm when you leave the procedure. The next procedure does exactly so:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
once_with_alarm(Time,Goal,DoOnAlarm) :-
   catch(execute_once_with_alarm(Time, Goal), alarm, DoOnAlarm).

execute_once_with_alarm(Time, Goal) :-
        alarm(Time, alarm, _),
        ( call(Goal) -> alarm(0, alarm, _) ; alarm(0, alarm, _), fail).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The procedure `once_with_alarm/3` has three arguments:
the  _Time_ to wait before the alarm is
sent; the  _Goal_ to execute; and the goal  _DoOnAlarm_ to execute
if the alarm is sent. It uses catch/3 to handle the case the
`alarm` is sent. Then it starts the alarm, calls the goal
 _Goal_, and disables the alarm on success or failure.

 
*/

/** @pred  on_signal(+ _Signal_,? _OldAction_,+ _Callable_) 


Set the interrupt handler for soft interrupt  _Signal_ to be
 _Callable_.  _OldAction_ is unified with the previous handler.

Only a subset of the software interrupts (signals) can have their
handlers manipulated through on_signal/3.
Their POSIX names, YAP names and default behavior is given below.
The "YAP name" of the signal is the atom that is associated with
each signal, and should be used as the first argument to
on_signal/3. It is chosen so that it matches the signal's POSIX
name.

on_signal/3 succeeds, unless when called with an invalid
signal name or one that is not supported on this platform. No checks
are made on the handler provided by the user.

    + sig_up (Hangup)
SIGHUP in Unix/Linux; Reconsult the initialization files
~/.yaprc, ~/.prologrc and ~/prolog.ini.
    + sig_usr1 and sig_usr2 (User signals)
SIGUSR1 and SIGUSR2 in Unix/Linux; Print a message and halt.


A special case is made, where if  _Callable_ is bound to
`default`, then the default handler is restored for that signal.

A call in the form `on_signal( _S_, _H_, _H_)` can be used
to retrieve a signal's current handler without changing it.

It must be noted that although a signal can be received at all times,
the handler is not executed while YAP is waiting for a query at the
prompt. The signal will be, however, registered and dealt with as soon
as the user makes a query.

Please also note, that neither POSIX Operating Systems nor YAP guarantee
that the order of delivery and handling is going to correspond with the
order of dispatch.




@} */

/** @defgroup Term_Modification Term Modification
@ingroup YAPBuiltins
@{

It is sometimes useful to change the value of instantiated
variables. Although, this is against the spirit of logic programming, it
is sometimes useful. As in other Prolog systems, YAP has
several primitives that allow updating Prolog terms. Note that these
primitives are also backtrackable.

The `setarg/3` primitive allows updating any argument of a Prolog
compound terms. The `mutable` family of predicates provides
<em>mutable variables</em>. They should be used instead of `setarg/3`,
as they allow the encapsulation of accesses to updatable
variables. Their implementation can also be more efficient for long
deterministic computations.


 
*/

/** @pred  setarg(+ _I_,+ _S_,? _T_) 


Set the value of the  _I_th argument of term  _S_ to term  _T_. 

 
*/

/** @pred  create_mutable(+ _D_,- _M_) 


Create new mutable variable  _M_ with initial value  _D_.

 
*/

/** @pred  is_mutable(? _D_) 


Holds if  _D_ is a mutable term.

 
*/

/** @pred  get_mutable(? _D_,+ _M_) 


Unify the current value of mutable term  _M_ with term  _D_.

 
*/

/** @pred  update_mutable(+ _D_,+ _M_) 


Set the current value of mutable term  _M_ to term  _D_.



@} */

/** @defgroup Global_Variables Global Variables
@ingroup YAPBuiltins
@{

Global variables are associations between names (atoms) and
terms. They differ in various ways from storing information using
assert/1 or recorda/3.

    + The value lives on the Prolog (global) stack. This implies that
lookup time is independent from the size of the term. This is
particularly interesting for large data structures such as parsed XML
documents or the CHR global constraint store. 

    + They support both global assignment using nb_setval/2 and
backtrackable assignment using b_setval/2.

    + Only one value (which can be an arbitrary complex Prolog term)
can be associated to a variable at a time. 

    + Their value cannot be shared among threads. Each thread has its own
namespace and values for global variables.


Currently global variables are scoped globally. We may consider module
scoping in future versions.   Both b_setval/2 and
nb_setval/2 implicitly create a variable if the referenced name
does not already refer to a variable.

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store
global variables, which implies they have to be declared with
initialization/1 to recreate them after loading the saved
state. Each thread has its own set of global variables, starting with
an empty set. Using `thread_initialization/1` to define a global
variable it will be defined, restored after reloading a saved state
and created in all threads that are created after the
registration. Finally, global variables can be initialised using the
exception hook called exception/3. The latter technique is used
by CHR.


 
*/

/** @pred  b_setval(+ _Name_, + _Value_)  


Associate the term  _Value_ with the atom  _Name_ or replaces
the currently associated value with  _Value_. If  _Name_ does
not refer to an existing global variable a variable with initial value
[] is created (the empty list). On backtracking the assignment is
reversed. 

 
*/

/** @pred  b_getval(+ _Name_, - _Value_)  


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further
instantiate the value of the global variable. If this is undesirable
the normal precautions (double negation or copy_term/2) must be
taken. The b_getval/2 predicate generates errors if  _Name_ is not
an atom or the requested variable does not exist. 

Notice that for compatibility with other systems  _Name_ <em>must</em> be already associated with a term: otherwise the system will generate an error.

 
*/

/** @pred  nb_setval(+ _Name_, + _Value_)  


Associates a copy of  _Value_ created with duplicate_term/2 with
the atom  _Name_. Note that this can be used to set an initial
value other than `[]` prior to backtrackable assignment.

 
*/

/** @pred  nb_getval(+ _Name_, - _Value_)  


The nb_getval/2 predicate is a synonym for b_getval/2,
introduced for compatibility and symmetry. As most scenarios will use
a particular global variable either using non-backtrackable or
backtrackable assignment, using nb_getval/2 can be used to
document that the variable is used non-backtrackable.

 
*/

/** @pred  nb_linkval(+ _Name_, + _Value_)  


Associates the term  _Value_ with the atom  _Name_ without
copying it. This is a fast special-purpose variation of nb_setval/2
intended for expert users only because the semantics on backtracking
to a point before creating the link are poorly defined for compound
terms. The principal term is always left untouched, but backtracking
behaviour on arguments is undone if the original assignment was
trailed and left alone otherwise, which implies that the history that
created the term affects the behaviour on backtracking. Please
consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
demo_nb_linkval :-
        T = nice(N),
        (   N = world,
            nb_linkval(myvar, T),
            fail
        ;   nb_getval(myvar, V),
            writeln(V)
        ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  nb_set_shared_val(+ _Name_, + _Value_)  


Associates the term  _Value_ with the atom  _Name_, but sharing
non-backtrackable terms. This may be useful if you want to rewrite a
global variable so that the new copy will survive backtracking, but
you want to share structure with the previous term.

The next example shows the differences between the three built-ins:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- nb_setval(a,a(_)),nb_getval(a,A),nb_setval(b,t(C,A)),nb_getval(b,B).
A = a(_A),
B = t(_B,a(_C)) ? 

?- nb_setval(a,a(_)),nb_getval(a,A),nb_set_shared_val(b,t(C,A)),nb_getval(b,B).

?- nb_setval(a,a(_)),nb_getval(a,A),nb_linkval(b,t(C,A)),nb_getval(b,B).
A = a(_A),
B = t(C,a(_A)) ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  nb_setarg(+{Arg], + _Term_, + _Value_) 



Assigns the  _Arg_-th argument of the compound term  _Term_ with
the given  _Value_ as setarg/3, but on backtracking the assignment
is not reversed. If  _Term_ is not atomic, it is duplicated using
duplicate_term/2. This predicate uses the same technique as
nb_setval/2. We therefore refer to the description of
nb_setval/2 for details on non-backtrackable assignment of
terms. This predicate is compatible to GNU-Prolog
`setarg(A,T,V,false)`, removing the type-restriction on
 _Value_. See also nb_linkarg/3. Below is an example for
counting the number of solutions of a goal. Note that this
implementation is thread-safe, reentrant and capable of handling
exceptions. Realising these features with a traditional implementation
based on assert/retract or flag/3 is much more complicated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    succeeds_n_times(Goal, Times) :-
            Counter = counter(0),
            (   Goal,
                arg(1, Counter, N0),
                N is N0 + 1,
                nb_setarg(1, Counter, N),
                fail
            ;   arg(1, Counter, Times)
            ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred  nb_set_shared_arg(+ _Arg_, + _Term_, + _Value_)  



As nb_setarg/3, but like nb_linkval/2 it does not
duplicate the global sub-terms in  _Value_. Use with extreme care
and consult the documentation of nb_linkval/2 before use.

 
*/

/** @pred  nb_linkarg(+ _Arg_, + _Term_, + _Value_)  



As nb_setarg/3, but like nb_linkval/2 it does not
duplicate  _Value_. Use with extreme care and consult the
documentation of nb_linkval/2 before use.

 
*/

/** @pred  nb_current(? _Name_, ? _Value_)  


Enumerate all defined variables with their value. The order of
enumeration is undefined. 

 
*/

/** @pred  nb_delete(+ _Name_)  


Delete the named global variable. 


Global variables have been introduced by various Prolog
implementations recently. We follow the implementation of them in
SWI-Prolog, itself based on hProlog by Bart Demoen.

GNU-Prolog provides a rich set of global variables, including
arrays. Arrays can be implemented easily in YAP and SWI-Prolog using
functor/3 and `setarg/3` due to the unrestricted arity of
compound terms.


@} */

/** @defgroup Profiling Profiling Prolog Programs
@ingroup YAPBuiltins
@{

YAP includes two profilers. The count profiler keeps information on the
number of times a predicate was called. This information can be used to
detect what are the most commonly called predicates in the program.  The
count profiler can be compiled by setting YAP's flag profiling
to `on`. The time-profiler is a `gprof` profiler, and counts
how many ticks are being spent on specific predicates, or on other
system functions such as internal data-base accesses or garbage collects.

The YAP profiling sub-system is currently under
development. Functionality for this sub-system will increase with newer
implementation.


@} */

/** @defgroup The_Count_Profiler The Count Profiler
@ingroup YAPBuiltins
@{

 *Notes:*

The count profiler works by incrementing counters at procedure entry or
backtracking. It provides exact information:

    + Profiling works for both static and dynamic predicates.
    + Currently only information on entries and retries to a predicate
are maintained. This may change in the future.
    + As an example, the following user-level program gives a list of
the most often called procedures in a program. The procedure
`list_profile` shows all procedures, irrespective of module, and
the procedure `list_profile/1` shows the procedures being used in
a specific module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_profile :-
        % get number of calls for each profiled procedure
        setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        write_profile_data(LP).

list_profile(Module) :-
        % get number of calls for each profiled procedure
        setof(D-[Module:P|D1],(profile_data(Module:P,calls,D),profile_data(Module:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        write_profile_data(LP).

write_profile_data([]).
write_profile_data([D-[M:P|R]|SLP]) :-
        % swap the two calls if you want the most often
        %  called predicates first.
        format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
        write_profile_data(SLP).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


These are  the current predicates to access and clear profiling data:


 
*/

/** @pred  profile_data(? _Na/Ar_, ? _Parameter_, - _Data_) 


Give current profile data on  _Parameter_ for a predicate described
by the predicate indicator  _Na/Ar_. If any of  _Na/Ar_ or
 _Parameter_ are unbound, backtrack through all profiled predicates
or stored parameters. Current parameters are:

    + calls
Number of times a procedure was called.

    + retries
Number of times a call to the procedure was backtracked to and retried.


    + profile_reset 


Reset all profiling information.




@} */

/** @defgroup Tick_Profiler Tick Profiler
@ingroup YAPBuiltins
@{

The tick profiler works by interrupting the Prolog code every so often
and checking at each point the code was. The profiler must be able to
retrace the state of the abstract machine at every moment. The major
advantage of this approach is that it gives the actual amount of time
being spent per procedure, or whether garbage collection dominates
execution time. The major drawback is that tracking down the state of
the abstract machine may take significant time, and in the worst case
may slow down the whole execution.

The following procedures are available:

    + profinit 


Initialise the data-structures for the profiler. Unnecessary for
dynamic profiler.

    + profon 


Start profiling.

    + profoff 


Stop profiling.

 
*/

/** @pred  showprofres 


Show profiling info.

 
*/

/** @pred  showprofres( _N_)

Show profiling info for the top-most  _N_ predicates.



The showprofres/0 and `showprofres/1` predicates call a user-defined multifile hook predicate, `user:prolog_predicate_name/2`, that can be used for converting a possibly explicitly-qualified callable term into an atom that will used when printing the profiling information.


@} */

/** @defgroup Call_Counting Counting Calls
@ingroup YAPBuiltins
@{

Predicates compiled with YAP's flag call_counting set to
`on` update counters on the numbers of calls and of
retries. Counters are actually decreasing counters, so that they can be
used as timers.  Three counters are available:

    + `calls`: number of predicate calls since execution started or since
system was reset; 
    + `retries`: number of retries for predicates called since
execution started or since counters were reset;
    + `calls_and_retries`: count both on predicate calls and
retries.

These counters can be used to find out how many calls a certain
goal takes to execute. They can also be used as timers.

The code for the call counters piggybacks on the profiling
code. Therefore, activating the call counters also activates the profiling
counters.

These are  the predicates that access and manipulate the call counters:


 
*/

/** @pred  call_count_data(- _Calls_, - _Retries_, - _CallsAndRetries_) 


Give current call count data. The first argument gives the current value
for the  _Calls_ counter, next the  _Retries_ counter, and last
the  _CallsAndRetries_ counter.

    + call_count_reset 


Reset call count counters. All timers are also reset.

 <li
*/

/** @pred  l_count(? _CallsMax_, ? _RetriesMax_, ? _CallsAndRetriesMax_) 


Set call count counter as timers. YAP will generate an exception
if one of the instantiated call counters decreases to 0. YAP will ignore
unbound arguments:

    + _CallsMax_: throw the exception `call_counter` when the
counter `calls` reaches 0;
    + _RetriesMax_: throw the exception `retry_counter` when the
counter `retries` reaches 0;
    + _CallsAndRetriesMax_: throw the exception
`call_and_retry_counter` when the counter `calls_and_retries`
reaches 0.



Next, we show a simple example of how to use call counters:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- yap_flag(call_counting,on), [-user]. l :- l. end_of_file. yap_flag(call_counting,off).

yes

yes
   ?- catch((call_count(10000,_,_),l),call_counter,format("limit_exceeded.~n",[])).

limit_exceeded.

yes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we first compile the looping predicate `l/0` with
call_counting `on`. Next, we catch/3 to handle an
exception when `l/0` performs more than 10000 reductions.


@} */

/** @defgroup Arrays Arrays
@ingroup YAPBuiltins
@{

The YAP system includes experimental support for arrays. The
support is enabled with the option `YAP_ARRAYS`.

There are two very distinct forms of arrays in YAP. The
<em>dynamic arrays</em> are a different way to access compound terms
created during the execution. Like any other terms, any bindings to
these terms and eventually the terms themselves will be destroyed during
backtracking. Our goal in supporting dynamic arrays is twofold. First,
they provide an alternative to the standard arg/3
built-in. Second, because dynamic arrays may have name that are globally
visible, a dynamic array can be visible from any point in the
program. In more detail, the clause

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g(X) :- array_element(a,2,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
will succeed as long as the programmer has used the built-in <tt>array/2</tt>
to create an array term with at least 3 elements in the current
environment, and the array was associated with the name `a`.  The
element `X` is a Prolog term, so one can bind it and any such
bindings will be undone when backtracking. Note that dynamic arrays do
not have a type: each element may be any Prolog term.

The <em>static arrays</em> are an extension of the database. They provide
a compact way for manipulating data-structures formed by characters,
integers, or floats imperatively. They can also be used to provide
two-way communication between YAP and external programs through
shared memory.

In order to efficiently manage space elements in a static array must
have a type. Currently, elements of static arrays in YAP should
have one of the following predefined types:

    + `byte`: an 8-bit signed character.
    + `unsigned_byte`: an 8-bit unsigned character.
    + `int`: Prolog integers. Size would be the natural size for
the machine's architecture.
    + `float`: Prolog floating point number. Size would be equivalent
to a double in `C`.
    + `atom`: a Prolog atom.
    + `dbref`: an internal database reference.
    + `term`: a generic Prolog term. Note that this will term will
not be stored in the array itself, but instead will be stored in the
Prolog internal database.


Arrays may be <em>named</em> or <em>anonymous</em>. Most arrays will be
<em>named</em>, that is associated with an atom that will be used to find
the array. Anonymous arrays do not have a name, and they are only of
interest if the `TERM_EXTENSIONS` compilation flag is enabled. In
this case, the unification and parser are extended to replace
occurrences of Prolog terms of the form `X[I]` by run-time calls to
array_element/3, so that one can use array references instead of
extra calls to arg/3. As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
g(X,Y,Z,I,J) :- X[I] is Y[J]+Z[I].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
should give the same results as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
G(X,Y,Z,I,J) :-
        array_element(X,I,E1),
        array_element(Y,J,E2),  
        array_element(Z,I,E3),  
        E1 is E2+E3.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the only limitation on array size are the stack size for
dynamic arrays; and, the heap size for static (not memory mapped)
arrays. Memory mapped arrays are limited by available space in the file
system and in the virtual memory space.

The following predicates manipulate arrays:



 
*/

/** @pred  array(+ _Name_, + _Size_) 


Creates a new dynamic array. The  _Size_ must evaluate to an
integer. The  _Name_ may be either an atom (named array) or an
unbound variable (anonymous array).

Dynamic arrays work as standard compound terms, hence space for the
array is recovered automatically on backtracking.

 
*/

/** @pred  static_array(+ _Name_, + _Size_, + _Type_) 


Create a new static array with name  _Name_. Note that the  _Name_
must be an atom (named array). The  _Size_ must evaluate to an
integer.  The  _Type_ must be bound to one of types mentioned
previously.

 
*/

/** @pred  reset_static_array(+ _Name_) 


Reset static array with name  _Name_ to its initial value.

 
*/

/** @pred  static_array_location(+ _Name_, - _Ptr_) 


Give the location for  a static array with name
 _Name_.

 
*/

/** @pred  static_array_properties(? _Name_, ? _Size_, ? _Type_) 


Show the properties size and type of a static array with name
 _Name_. Can also be used to enumerate all current
static arrays. 

This built-in will silently fail if the there is no static array with
that name.

 
*/

/** @pred  static_array_to_term(? _Name_, ? _Term_) 


Convert a static array with name
 _Name_ to a compound term of name  _Name_.

This built-in will silently fail if the there is no static array with
that name.

 
*/

/** @pred  mmapped_array(+ _Name_, + _Size_, + _Type_, + _File_) 


Similar to static_array/3, but the array is memory mapped to file
 _File_. This means that the array is initialized from the file, and
that any changes to the array will also be stored in the file. 

This built-in is only available in operating systems that support the
system call `mmap`. Moreover, mmapped arrays do not store generic
terms (type `term`).

 
*/

/** @pred  close_static_array(+ _Name_) 


Close an existing static array of name  _Name_. The  _Name_ must
be an atom (named array). Space for the array will be recovered and
further accesses to the array will return an error. 

 
*/

/** @pred  resize_static_array(+ _Name_, - _OldSize_, + _NewSize_) 


Expand or reduce a static array, The  _Size_ must evaluate to an
integer. The  _Name_ must be an atom (named array). The  _Type_
must be bound to one of `int`, `dbref`, `float` or
`atom`.

Note that if the array is a mmapped array the size of the mmapped file
will be actually adjusted to correspond to the size of the array.

 
*/

/** @pred  array_element(+ _Name_, + _Index_, ? _Element_) 


Unify  _Element_ with  _Name_[ _Index_]. It works for both
static and dynamic arrays, but it is read-only for static arrays, while
it can be used to unify with an element of a dynamic array.

 
*/

/** @pred  update_array(+ _Name_, + _Index_, ? _Value_)  


Attribute value  _Value_ to  _Name_[ _Index_]. Type
restrictions must be respected for static arrays. This operation is
available for dynamic arrays if `MULTI_ASSIGNMENT_VARIABLES` is
enabled (true by default). Backtracking undoes  _update_array/3_ for
dynamic arrays, but not for static arrays.

Note that update_array/3 actually uses `setarg/3` to update
elements of dynamic arrays, and `setarg/3` spends an extra cell for
every update. For intensive operations we suggest it may be less
expensive to unify each element of the array with a mutable terms and
to use the operations on mutable terms.

 
*/

/** @pred  add_to_array_element(+ _Name_, + _Index_, , + _Number_, ? _NewValue_)  


Add  _Number_  _Name_[ _Index_] and unify  _NewValue_ with
the incremented value. Observe that  _Name_[ _Index_] must be an
number. If  _Name_ is a static array the type of the array must be
`int` or `float`. If the type of the array is `int` you
only may add integers, if it is `float` you may add integers or
floats. If  _Name_ corresponds to a dynamic array the array element
must have been previously bound to a number and `Number` can be
any kind of number.

The `add_to_array_element/3` built-in actually uses
`setarg/3` to update elements of dynamic arrays. For intensive
operations we suggest it may be less expensive to unify each element
of the array with a mutable terms and to use the operations on mutable
terms.




@} */

/** @defgroup Preds Predicate Information
@ingroup YAPBuiltins
@{

Built-ins that return information on the current predicates and modules:



 
*/

/** @pred  current_module( _M_) 


Succeeds if  _M_ are defined modules. A module is defined as soon as some
predicate defined in the module is loaded, as soon as a goal in the
module is called, or as soon as it becomes the current type-in module.

 
*/

/** @pred  current_module( _M_, _F_)

Succeeds if  _M_ are current modules associated to the file  _F_.




@} */

/** @defgroup Misc Miscellaneous
@ingroup YAPBuiltins
@{



 
*/

/** @pred  statistics/0 


Send to the current user error stream general information on space used and time
spent by the system.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- statistics.
memory (total)        4784124 bytes
   program space      3055616 bytes:    1392224 in use,      1663392 free
                                                             2228132  max
   stack space        1531904 bytes:        464 in use,      1531440 free
     global stack:                           96 in use,       616684  max
      local stack:                          368 in use,       546208  max
   trail stack         196604 bytes:          8 in use,       196596 free

       0.010 sec. for 5 code, 2 stack, and 1 trail space overflows
       0.130 sec. for 3 garbage collections which collected 421000 bytes
       0.000 sec. for 0 atom garbage collections which collected 0 bytes
       0.880 sec. runtime
       1.020 sec. cputime
      25.055 sec. elapsed time

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The example shows how much memory the system spends. Memory is divided
into Program Space, Stack Space and Trail. In the example we have 3MB
allocated for program spaces, with less than half being actually
used. YAP also shows the maximum amount of heap space having been used
which was over 2MB.

The stack space is divided into two stacks which grow against each
other. We are in the top level so very little stack is being used. On
the other hand, the system did use a lot of global and local stack
during the previous execution (we refer the reader to a WAM tutorial in
order to understand what are the global and local stacks).

YAP also shows information on how many memory overflows and garbage
collections the system executed, and statistics on total execution
time. Cputime includes all running time, runtime excludes garbage
collection and stack overflow time.

 
*/

/** @pred  statistics(? _Param_,- _Info_)

Gives statistical information on the system parameter given by first
argument:



    + atoms 

`[ _NumberOfAtoms_, _SpaceUsedBy Atoms_]`


This gives the total number of atoms `NumberOfAtoms` and how much
space they require in bytes,  _SpaceUsedBy Atoms_.

    + cputime 

`[ _Time since Boot_, _Time From Last Call to Cputime_]`


This gives the total cputime in milliseconds spent executing Prolog code,
garbage collection and stack shifts time included.

    + dynamic_code 

`[ _Clause Size_, _Index Size_, _Tree Index Size_, _Choice Point Instructions Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into main tree,  _Tree Index Size_, 
tables that implement choice-point manipulation,  _Choice xsPoint Instructions Size_, tables that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and 
tables such as hash tables that select according to value,   _Index Switch Size_.

    + garbage_collection 

`[ _Number of GCs_, _Total Global Recovered_, _Total Time Spent_]`


Number of garbage collections, amount of space recovered in kbytes, and
total time spent doing garbage collection in milliseconds. More detailed
information is available using `yap_flag(gc_trace,verbose)`.

    + global_stack 

`[ _Global Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the global stack, and space available for
expansion by the local and global stacks.

    + local_stack 

`[ _Local Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the local stack, and space available for
expansion by the local and global stacks.

    + heap 

`[ _Heap Used_, _Heap Free_]`


Total space in kbytes not recoverable
in backtracking. It includes the program code, internal data base, and,
atom symbol table.

    + program 

`[ _Program Space Used_, _Program Space Free_]`


Equivalent to heap.

    + runtime 

`[ _Time since Boot_, _Time From Last Call to Runtime_]`


This gives the total cputime in milliseconds spent executing Prolog
code, not including garbage collections and stack shifts. Note that
until YAP4.1.2 the runtime statistics would return time spent on
garbage collection and stack shifting.

    + stack_shifts 

`[ _Number of Heap Shifts_, _Number of Stack Shifts_, _Number of Trail Shifts_]`


Number of times YAP had to
expand the heap, the stacks, or the trail. More detailed information is
available using `yap_flag(gc_trace,verbose)`.

    + static_code 

`[ _Clause Size_, _Index Size_, _Tree Index Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into a main tree,  _Tree Index Size_, table that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and and 
tables such as hash tables that select according to value,   _Index Switch Size_.

    + trail 

`[ _Trail Used_, _Trail Free_]`


Space in kbytes currently being used and still available for the trail.

    + walltime 

`[ _Time since Boot_, _Time From Last Call to Walltime_]`


This gives the clock time in milliseconds since starting Prolog.



 
*/

/** @pred  time(: _Goal_) 


Prints the CPU time and the wall time for the execution of  _Goal_.
Possible choice-points of  _Goal_ are removed. Based on the SWI-Prolog 
definition (minus reporting the number of inferences, which YAP currently
does not support).

 
*/

/** @pred  yap_flag(? _Param_,? _Value_) 


Set or read system properties for  _Param_:



    + argv 


Read-only flag. It unifies with a list of atoms that gives the
arguments to YAP after `--`.

    + agc_margin 

An integer: if this amount of atoms has been created since the last
atom-garbage collection, perform atom garbage collection at the first
opportunity. Initial value is 10,000. May be changed. A value of 0
(zero) disables atom garbage collection.

    + associate 



Read-write flag telling a suffix for files associated to Prolog
sources. It is `yap` by default.

    + bounded is iso 



Read-only flag telling whether integers are bounded. The value depends
on whether YAP uses the GMP library or not.

    + profiling 



If `off` (default) do not compile call counting information for
procedures. If `on` compile predicates so that they calls and
retries to the predicate may be counted. Profiling data can be read through the
call_count_data/3 built-in.

    + char_conversion is iso


Writable flag telling whether a character conversion table is used when
reading terms. The default value for this flag is `off` except in
`sicstus` and `iso` language modes, where it is `on`.

    + character_escapes is iso 


Writable flag telling whether a character escapes are enables,
`true`, or disabled, `false`. The default value for this flag is
`on`.

    + debug is iso 



If  _Value_ is unbound, tell whether debugging is `true` or
`false`. If  _Value_ is bound to `true` enable debugging, and if
it is bound to `false` disable debugging.

    + debugger_print_options 



If bound, set the argument to the `write_term/3` options the
debugger uses to write terms. If unbound, show the current options.

    + dialect 



Read-only flag that always returns `yap`.

    + discontiguous_warnings 



If  _Value_ is unbound, tell whether warnings for discontiguous
predicates are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

    + dollar_as_lower_case 



If `off` (default)  consider the character '$' a control character, if
`on` consider '$' a lower case character.

    + double_quotes is iso 



If  _Value_ is unbound, tell whether a double quoted list of characters
token is converted to a list of atoms, `chars`, to a list of integers,
`codes`, or to a single atom, `atom`. If  _Value_ is bound, set to
the corresponding behavior. The default value is `codes`.

    + executable 


Read-only flag. It unifies with an atom that gives the
original program path.

    + fast 



If `on` allow fast machine code, if `off` (default) disable it. Only
available in experimental implementations.

    + fileerrors


If `on` `fileerrors` is `on`, if `off` (default)
`fileerrors` is disabled.

    + float_format 


C-library `printf()` format specification used by write/1 and
friends to determine how floating point numbers are printed. The
default is `%.15g`. The specified value is passed to `printf()`
without further checking. For example, if you want less digits
printed, `%g` will print all floats using 6 digits instead of the
default 15.

    + gc


If `on` allow garbage collection (default), if `off` disable it.

    + gc_margin 



Set or show the minimum free stack before starting garbage
collection. The default depends on total stack size. 

    + gc_trace 


If `off` (default) do not show information on garbage collection
and stack shifts, if `on` inform when a garbage collection or stack
shift happened, if verbose give detailed information on garbage
collection and stack shifts. Last, if `very_verbose` give detailed
information on data-structures found during the garbage collection
process, namely, on choice-points.

    + generate_debugging_info 


If `true` (default) generate debugging information for
procedures, including source mode. If `false` predicates no
information is generated, although debugging is still possible, and
source mode is disabled.

    + host_type 


Return `configure` system information, including the machine-id
for which YAP was compiled and Operating System information. 

    + index 


If `on` allow indexing (default), if `off` disable it, if
`single` allow on first argument only.

    + index_sub_term_search_depth 



Maximum bound on searching sub-terms for indexing, if `0` (default) no bound.

    + informational_messages 



If `on` allow printing of informational messages, such as the ones
that are printed when consulting. If `off` disable printing
these messages. It is `on` by default except if YAP is booted with
the `-L` flag.

    + integer_rounding_function is iso 



Read-only flag telling the rounding function used for integers. Takes the value
`toward_zero` for the current version of YAP.

    + language 



Choose whether YAP is closer to C-Prolog, `cprolog`, iso-prolog,
`iso` or SICStus Prolog, `sicstus`. The current default is
`cprolog`. This flag affects update semantics, leashing mode,
style checking, handling calls to undefined procedures, how directives
are interpreted, when to use dynamic, character escapes, and how files
are consulted.

    + max_arity is iso 



Read-only flag telling the maximum arity of a functor. Takes the value
`unbounded` for the current version of YAP.

    + max_integer is iso 



Read-only flag telling the maximum integer in the
implementation. Depends on machine and Operating System
architecture, and on whether YAP uses the `GMP` multi-precision
library. If bounded is false, requests for max_integer
will fail.

    + max_tagged_integer  



Read-only flag telling the maximum integer we can store as a single
word. Depends on machine and Operating System
architecture. It can be used to find the word size of the current machine.

    + min_integer is iso 


Read-only flag telling the minimum integer in the
implementation. Depends on machine and Operating System architecture,
and on whether YAP uses the `GMP` multi-precision library. If
bounded is false, requests for min_integer will fail.

    + min_tagged_integer  



Read-only flag telling the minimum integer we can store as a single
word. Depends on machine and Operating System
architecture.

    + n_of_integer_keys_in_bb 



Read or set the size of the hash table that is used for looking up the
blackboard when the key is an integer.

    + occurs_check 



Current read-only and set to `false`.

    + n_of_integer_keys_in_db 



Read or set the size of the hash table that is used for looking up the
internal data-base when the key is an integer.

    + open_expands_filename 



If `true` the open/3 builtin performs filename-expansion
before opening a file (SICStus Prolog like). If `false` it does not
(SWI-Prolog like).

    + open_shared_object 



If true, `open_shared_object/2` and friends are implemented,
providing access to shared libraries (`.so` files) or to dynamic link
libraries (`.DLL` files).

    + profiling 



If `off` (default) do not compile profiling information for
procedures. If `on` compile predicates so that they will output
profiling information. Profiling data can be read through the
profile_data/3 built-in.

    + prompt_alternatives_on(atom, changeable) 

SWI-Compatible option, determines prompting for alternatives in the Prolog toplevel. Default is <tt>groundness</tt>, YAP prompts for alternatives if and only if the query contains variables. The alternative, default in SWI-Prolog is <tt>determinism</tt> which implies the system prompts for alternatives if the goal succeeded while leaving choicepoints.

    + redefine_warnings 



If  _Value_ is unbound, tell whether warnings for procedures defined
in several different files are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.

    + shared_object_search_path 

Name of the environment variable used by the system to search for shared
objects.

    + shared_object_extension 

Suffix associated with loadable code.

    + single_var_warnings 



If  _Value_ is unbound, tell whether warnings for singleton variables
are `on` or `off`. If  _Value_ is bound to `on` enable
these warnings, and if it is bound to `off` disable them. The
default for YAP is `off`, unless we are in `sicstus` or
`iso` mode.

    + strict_iso 



If  _Value_ is unbound, tell whether strict ISO compatibility mode
is `on` or `off`. If  _Value_ is bound to `on` set
language mode to `iso` and enable strict mode. If  _Value_ is
bound to `off` disable strict mode, and keep the current language
mode. The default for YAP is `off`.

Under strict ISO Prolog mode all calls to non-ISO built-ins generate an
error. Compilation of clauses that would call non-ISO built-ins will
also generate errors. Pre-processing for grammar rules is also
disabled. Module expansion is still performed.

Arguably, ISO Prolog does not provide all the functionality required
from a modern Prolog system. Moreover, because most Prolog
implementations do not fully implement the standard and because the
standard itself gives the implementor latitude in a few important
questions, such as the unification algorithm and maximum size for
numbers there is no guarantee that programs compliant with this mode
will work the same way in every Prolog and in every platform. We thus
believe this mode is mostly useful when investigating how a program
depends on a Prolog's platform specific features.

    + stack_dump_on_error 



If `on` show a stack dump when YAP finds an error. The default is
`off`.

    + syntax_errors


Control action to be taken after syntax errors while executing read/1,
`read/2`, or `read_term/3`:



    + dec10


Report the syntax error and retry reading the term.

    + fail


Report the syntax error and fail (default).

    + error


Report the syntax error and generate an error.

    + quiet


Just fail


    + system_options 


This read only flag tells which options were used to compile
YAP. Currently it informs whether the system supports `big_numbers`,
`coroutining`, `depth_limit`, `low_level_tracer`,
`or-parallelism`, `rational_trees`, `readline`, `tabling`,
`threads`, or the `wam_profiler`.

    + tabling_mode

Sets or reads the tabling mode for all tabled predicates. Please
 (see Tabling) for the list of options.

    + to_chars_mode 


Define whether YAP should follow `quintus`-like
semantics for the `atom_chars/1` or `number_chars/1` built-in,
or whether it should follow the ISO standard (`iso` option).

    + toplevel_hook 



+If bound, set the argument to a goal to be executed before entering the
top-level. If unbound show the current goal or `true` if none is
presented. Only the first solution is considered and the goal is not
backtracked into.

    + toplevel_print_options 



+If bound, set the argument to the `write_term/3` options used to write
terms from the top-level. If unbound, show the current options.

    + typein_module 



If bound, set the current working or type-in module to the argument,
which must be an atom. If unbound, unify the argument with the current
working module.

    + unix

Read-only Boolean flag that unifies with `true` if YAP is
running on an Unix system.  Defined if the C-compiler used to compile
this version of YAP either defines `__unix__` or `unix`.

    + unknown is iso


Corresponds to calling the unknown/2 built-in. Possible values 
are `error`, `fail`, and `warning`.

    + update_semantics 



Define whether YAP should follow `immediate` update
semantics, as in C-Prolog (default), `logical` update semantics,
as in Quintus Prolog, SICStus Prolog, or in the ISO standard. There is
also an intermediate mode, `logical_assert`, where dynamic
procedures follow logical semantics but the internal data base still
follows immediate semantics.

    + user_error 



If the second argument is bound to a stream, set user_error to
this stream. If the second argument is unbound, unify the argument with
the current user_error stream.

By default, the user_error stream is set to a stream
corresponding to the Unix `stderr` stream.

The next example shows how to use this flag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- open( '/dev/null', append, Error,
           [alias(mauri_tripa)] ).

Error = '$stream'(3) ? ;

no
   ?- set_prolog_flag(user_error, mauri_tripa).

close(mauri_tripa).

yes
   ?- 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We execute three commands. First, we open a stream in write mode and
give it an alias, in this case `mauri_tripa`. Next, we set
user_error to the stream via the alias. Note that after we did so
prompts from the system were redirected to the stream
`mauri_tripa`. Last, we close the stream. At this point, YAP
automatically redirects the user_error alias to the original
`stderr`.

    + user_flags 



Define the behaviour of set_prolog_flag/2 if the flag is not known. Values are `silent`, `warning` and `error`. The first two create the flag on-the-fly, with `warning` printing a message. The value `error` is consistent with ISO: it raises an existence error and does not create the flag. See also `create_prolog_flag/3`. The default is`error`, and developers are encouraged to use `create_prolog_flag/3` to create flags for their library.

    + user_input 



If the second argument is bound to a stream, set user_input to
this stream. If the second argument is unbound, unify the argument with
the current user_input stream.

By default, the user_input stream is set to a stream
corresponding to the Unix `stdin` stream.

    + user_output 



If the second argument is bound to a stream, set user_output to
this stream. If the second argument is unbound, unify the argument with
the current user_output stream.

By default, the user_output stream is set to a stream
corresponding to the Unix `stdout` stream.

    + verbose 



If `normal` allow printing of informational and banner messages,
such as the ones that are printed when consulting. If `silent`
disable printing these messages. It is `normal` by default except if
YAP is booted with the `-q` or `-L` flag.

    + verbose_load 


If `true` allow printing of informational messages when
consulting files. If `false` disable printing these messages. It
is `normal` by default except if YAP is booted with the `-L`
flag.

    + version 


Read-only flag that returns an atom with the current version of
YAP.

    + version_data 


Read-only flag that reads a term of the form
`yap`( _Major_, _Minor_, _Patch_, _Undefined_), where
 _Major_ is the major version,  _Minor_ is the minor version,
and  _Patch_ is the patch number.

    + windows 



Read-only boolean flag that unifies with tr `true` if YAP is
running on an Windows machine.

    + write_strings 


Writable flag telling whether the system should write lists of
integers that are writable character codes using the list notation. It
is `on` if enables or `off` if disabled. The default value for
this flag is `off`.

    + max_workers 


Read-only flag telling the maximum number of parallel processes.

    + max_threads 


Read-only flag telling the maximum number of Prolog threads that can 
be created.



 
*/

/** @pred current_prolog_flag(? _Flag_,- _Value_) is iso 



Obtain the value for a YAP Prolog flag. Equivalent to calling
yap_flag/2 with the second argument unbound, and unifying the
returned second argument with  _Value_.

 
*/

/** @pred prolog_flag(? _Flag_,- _OldValue_,+ _NewValue_) 



Obtain the value for a YAP Prolog flag and then set it to a new
value. Equivalent to first calling current_prolog_flag/2 with the
second argument  _OldValue_ unbound and then calling
set_prolog_flag/2 with the third argument  _NewValue_.

 
*/

/** @pred set_prolog_flag(+ _Flag_,+ _Value_) is iso 



Set the value for YAP Prolog flag `Flag`. Equivalent to
calling yap_flag/2 with both arguments bound.

 
*/

/** @pred create_prolog_flag(+ _Flag_,+ _Value_,+ _Options_) 



Create a new YAP Prolog flag.  _Options_ include `type(+Type)` and `access(+Access)` with  _Access_
one of `read_only` or `read_write` and  _Type_ one of `boolean`, `integer`, `float`, `atom`
and `term` (that is, no type).

 
*/

/** @pred op(+ _P_,+ _T_,+ _A_) is iso 


Defines the operator  _A_ or the list of operators  _A_ with type
 _T_ (which must be one of `xfx`, `xfy`,`yfx`,
`xf`, `yf`, `fx` or `fy`) and precedence  _P_
(see appendix iv for a list of predefined operators).

Note that if there is a preexisting operator with the same name and
type, this operator will be discarded. Also, `','` may not be defined
as an operator, and it is not allowed to have the same for an infix and
a postfix operator.

 
*/

/** @pred current_op( _P_, _T_, _F_) is iso 


Defines the relation:  _P_ is a currently defined  operator of type
 _T_ and precedence  _P_.

 
*/

/** @pred prompt(- _A_,+ _B_) 


Changes YAP input prompt from  _A_ to  _B_.

 
*/

/** @pred initialization

Execute the goals defined by initialization/1. Only the first answer is
considered.

 
*/

/** @pred prolog_initialization( _G_) 


Add a goal to be executed on system initialization. This is compatible
with SICStus Prolog's initialization/1.

 
*/

/** @pred version

Write YAP's boot message. 

 
*/

/** @pred version(- _Message_)

Add a message to be written when yap boots or after aborting. It is not
possible to remove messages.

 
*/

/** @pred prolog_load_context(? _Key_, ? _Value_) 


Obtain information on what is going on in the compilation process. The
following keys are available:



    + directory 



Full name for the directory where YAP is currently consulting the
file.

    + file 



Full name for the file currently being consulted. Notice that included
filed are ignored.

    + module 



Current source module.

    + source (prolog_load_context/2 option) 



Full name for the file currently being read in, which may be consulted,
reconsulted, or included.

    + stream 



Stream currently being read in.

    + term_position 



Stream position at the stream currently being read in. For SWI
compatibility, it is a term of the form
`'$stream_position'(0,Line,0,0,0)`.


    + source_location(? _FileName_, ? _Line_) 


SWI-compatible predicate. If the last term has been read from a physical file (i.e., not from the file user or a string), unify File with an absolute path to the file and Line with the line-number in the file. Please use prolog_load_context/2.

    + source_file(? _File_) 


SWI-compatible predicate. True if  _File_ is a loaded Prolog source file.

    + source_file(? _ModuleAndPred_,? _File_)

SWI-compatible predicate. True if the predicate specified by  _ModuleAndPred_ was loaded from file  _File_, where  _File_ is an absolute path name (see `absolute_file_name/2`).



@section YAPLibrary Library Predicates

Library files reside in the library_directory path (set by the
`LIBDIR` variable in the Makefile for YAP). Currently,
most files in the library are from the Edinburgh Prolog library. 


@} */

/** @defgroup Aggregate Aggregate
@ingroup YAPLibrary
@{

This is the SWI-Prolog library based on  the Quintus and SICStus 4
library.   @c To be done - Analysing the aggregation template.


This library provides aggregating operators over the solutions of a
predicate. The operations are a generalisation of the bagof/3,
setof/3 and findall/3 built-in predicates. The defined
aggregation operations are counting, computing the sum, minimum,
maximum, a bag of solutions and a set of solutions. We first give a
simple example, computing the country with the smallest area:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
smallest_country(Name, Area) :-
        aggregate(min(A, N), country(N, A), min(Area, Name)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are four aggregation predicates, distinguished on two properties.



 @pred aggregate vs. aggregate_all
The aggregate predicates use setof/3 (aggregate/4) or bagof/3
(aggregate/3), dealing with existential qualified variables
( _Var_/\\ _Goal_) and providing multiple solutions for the
remaining free variables in  _Goal_. The aggregate_all/3
predicate uses findall/3, implicitly qualifying all free variables
and providing exactly one solution, while aggregate_all/4 uses
sort/2 over solutions and Distinguish (see below) generated using
findall/3. 
    + The  _Distinguish_ argument
The versions with 4 arguments provide a  _Distinguish_ argument
that allow for keeping duplicate bindings of a variable in the
result. For example, if we wish to compute the total population of
all countries we do not want to lose results because two countries
have the same population. Therefore we use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
        aggregate(sum(P), Name, country(Name, P), Total)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



All aggregation predicates support the following operator below in
 _Template_. In addition, they allow for an arbitrary named compound
term where each of the arguments is a term from the list below. I.e. the
term `r(min(X), max(X))` computes both the minimum and maximum
binding for  _X_.



 @pred count
Count number of solutions. Same as `sum(1)`. 
    + sum( _Expr_)
Sum of  _Expr_ for all solutions. 
    + min( _Expr_)
Minimum of  _Expr_ for all solutions. 
    + min( _Expr_,  _Witness_)
A term min( _Min_,  _Witness_), where  _Min_ is the minimal version of  _Expr_
over all Solution and  _Witness_ is any other template applied to
Solution that produced  _Min_. If multiple solutions provide the same
minimum,  _Witness_ corresponds to the first solution. 
    + max( _Expr_)
Maximum of  _Expr_ for all solutions. 
    + max( _Expr_,  _Witness_)
As min( _Expr_,  _Witness_), but producing the maximum result. 
    + set( _X_)
An ordered set with all solutions for  _X_. 
    + bag( _X_)
A list of all solutions for  _X_. 


The predicates are:



 @pred [nondet]aggregate(+ _Template_, : _Goal_, - _Result_) 


Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate/3 version performs bagof/3 on  _Goal_.
 
*/

/** @pred [nondet]aggregate(+ _Template_, + _Discriminator_, : _Goal_, - _Result_)

Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate/3 version performs setof/3 on  _Goal_.
 
*/

/** @pred [semidet]aggregate_all(+ _Template_, : _Goal_, - _Result_) 


Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate_all/3 version performs findall/3 on  _Goal_.
 
*/

/** @pred [semidet]aggregate_all(+ _Template_, + _Discriminator_, : _Goal_, - _Result_)

Aggregate bindings in  _Goal_ according to  _Template_. The
aggregate_all/3 version performs findall/3 followed by sort/2 on
 _Goal_.
 
*/

/** @pred foreach(:Generator, : _Goal_) 


True if the conjunction of instances of  _Goal_ using the
bindings from Generator is true. Unlike forall/2, which runs a
failure-driven loop that proves  _Goal_ for each solution of
Generator, foreach creates a conjunction. Each member of the
conjunction is a copy of  _Goal_, where the variables it shares
with Generator are filled with the values from the corresponding
solution.

The implementation executes forall/2 if  _Goal_ does not contain
any variables that are not shared with Generator.

Here is an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    ?- foreach(between(1,4,X), dif(X,Y)), Y = 5.
    Y = 5
    ?- foreach(between(1,4,X), dif(X,Y)), Y = 3.
    No
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice that  _Goal_ is copied repeatedly, which may cause
problems if attributed variables are involved.

 
*/

/** @pred [det]free_variables(:Generator, + _Template_, +VarList0, -VarList) 


In order to handle variables properly, we have to find all the universally quantified variables in the Generator. All variables as yet unbound are universally quantified, unless

<ol>
    + they occur in the template
    + they are bound by X/\\P, setof, or bagof
</ol>

`free_variables(Generator, Template, OldList, NewList)` finds this set, using OldList as an accumulator.


The original author of this code was Richard O'Keefe. Jan Wielemaker
made some SWI-Prolog enhancements, sponsored by SecuritEase,
http://www.securitease.com. The code is public domain (from DEC10 library).





@} */

/** @defgroup Apply Apply Macros
@ingroup YAPLibrary
@{

This library provides a SWI-compatible set of utilities for applying a
predicate to all elements of a list. The library just forwards
definitions from the `maplist` library.


@} */

/** @defgroup Association_Lists Association Lists
@ingroup YAPLibrary
@{

The following association list manipulation predicates are available
once included with the `use_module(library(assoc))` command. The
original library used Richard O'Keefe's implementation, on top of
unbalanced binary trees. The current code utilises code from the
red-black trees library and emulates the SICStus Prolog interface.

 
*/

/** @pred assoc_to_list(+ _Assoc_,? _List_) 


Given an association list  _Assoc_ unify  _List_ with a list of
the form  _Key-Val_, where the elements  _Key_ are in ascending
order.

 
*/

/** @pred del_assoc(+ _Key_, + _Assoc_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the element with  _Key_ and  _Val_ from the list  _Assoc_.

 
*/

/** @pred del_max_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the largest element of the list, with  _Key_ and  _Val_ from the
list  _Assoc_.

 
*/

/** @pred del_min_assoc(+ _Assoc_, ? _Key_, ? _Val_, ? _NewAssoc_) 


Succeeds if  _NewAssoc_ is an association list, obtained by removing
the smallest element of the list, with  _Key_ and  _Val_
from the list  _Assoc_.

 
*/

/** @pred empty_assoc(+ _Assoc_) 


Succeeds if association list  _Assoc_ is empty.

 
*/

/** @pred gen_assoc(+ _Assoc_,? _Key_,? _Value_) 


Given the association list  _Assoc_, unify  _Key_ and  _Value_
with two associated elements. It can be used to enumerate all elements
in the association list.

 
*/

/** @pred get_assoc(+ _Key_,+ _Assoc_,? _Value_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value.

 
*/

/** @pred get_assoc(+ _Key_,+ _Assoc_,? _Value_,+ _NAssoc_,? _NValue_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the associated value  _Value_ and a new association list
 _NAssoc_ where  _Key_ is associated with  _NValue_.

 
*/

/** @pred get_prev_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_) 


If  _Key_ is one of the elements in the association list  _Assoc_,
return the previous key,  _Next_, and its value,  _Value_.

 
*/

/** @pred get_next_assoc(+ _Key_,+ _Assoc_,? _Next_,? _Value_)

If  _Key_ is one of the elements in the association list  _Assoc_,
return the next key,  _Next_, and its value,  _Value_.

 
*/

/** @pred is_assoc(+ _Assoc_) 


Succeeds if  _Assoc_ is an association list, that is, if it is a
red-black tree.

 
*/

/** @pred list_to_assoc(+ _List_,? _Assoc_) 


Given a list  _List_ such that each element of  _List_ is of the
form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

 
*/

/** @pred map_assoc(+ _Pred_,+ _Assoc_) 


Succeeds if the unary predicate name  _Pred_( _Val_) holds for every
element in the association list.

 
*/

/** @pred map_assoc(+ _Pred_,+ _Assoc_,? _New_)

Given the binary predicate name  _Pred_ and the association list
 _Assoc_,  _New_ in an association list with keys in  _Assoc_,
and such that if  _Key-Val_ is in  _Assoc_, and  _Key-Ans_ is in
 _New_, then  _Pred_( _Val_, _Ans_) holds.

 
*/

/** @pred max_assoc(+ _Assoc_,- _Key_,? _Value_) 


Given the association list
 _Assoc_,  _Key_ in the largest key in the list, and  _Value_
the associated value.

 
*/

/** @pred min_assoc(+ _Assoc_,- _Key_,? _Value_) 


Given the association list
 _Assoc_,  _Key_ in the smallest key in the list, and  _Value_
the associated value.

 
*/

/** @pred ord_list_to_assoc(+ _List_,? _Assoc_) 


Given an ordered list  _List_ such that each element of  _List_ is
of the form  _Key-Val_, and all the  _Keys_ are unique,  _Assoc_ is
the corresponding association list.

 
*/

/** @pred put_assoc(+ _Key_,+ _Assoc_,+ _Val_,+ _New_) 


The association list  _New_ includes and element of association
 _key_ with  _Val_, and all elements of  _Assoc_ that did not
have key  _Key_.




@} */

/** @defgroup AVL_Trees AVL Trees
@ingroup YAPLibrary
@{

AVL trees are balanced search binary trees. They are named after their
inventors, Adelson-Velskii and Landis, and they were the first
dynamically balanced trees to be proposed. The YAP AVL tree manipulation
predicates library uses code originally written by Martin van Emdem and
published in the Logic Programming Newsletter, Autumn 1981.  A bug in
this code was fixed by Philip Vasey, in the Logic Programming
Newsletter, Summer 1982. The library currently only includes routines to
insert and lookup elements in the tree. Please try red-black trees if
you need deletion.

 
*/

/** @pred avl_new(+ _T_) 


Create a new tree.

 
*/

/** @pred avl_insert(+ _Key_,? _Value_,+ _T0_,- _TF_) 


Add an element with key  _Key_ and  _Value_ to the AVL tree
 _T0_ creating a new AVL tree  _TF_. Duplicated elements are
allowed.

 
*/

/** @pred avl_lookup(+ _Key_,- _Value_,+ _T_) 


Lookup an element with key  _Key_ in the AVL tree
 _T_, returning the value  _Value_.




@} */

/** @defgroup Exo_Intervals Exo Intervals
@ingroup YAPLibrary
@{

This package assumes you use exo-compilation, that is, that you loaded
the pedicate using the `exo` option to load_files/2, In this
case, YAP includes a package for improved search on  intervals of
integers.

The package is activated by `udi` declarations that state what is
the argument of interest:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- udi(diagnoses(exo_interval,?,?)).

:- load_files(db, [consult(exo)]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is designed to optimise the following type of queries:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- max(X, diagnoses(X, 9, Y), X).

?- min(X, diagnoses(X, 9, 36211117), X).

?- X #< Y, min(X, diagnoses(X, 9, 36211117), X ), diagnoses(Y, 9, _).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The first argument gives the time, the second the patient, and the
third the condition code. The first query should find the last time
the patient 9 had any code reported, the second looks for the first
report of code 36211117, and the last searches for reports after this
one. All queries run in  constant or log(n) time.


@} */

/** @defgroup Gecode Gecode Interface
@ingroup YAPPackages
@{


The gecode library intreface was designed and implemented by Denis
Duchier, with recent work by Vítor Santos Costa to port it to version 4
of gecode and to have an higher level interface,


@} */

/** @defgroup The_Gecode_Interface The Gecode Interface
@ingroup Gecode
@{

This text is due to Denys Duchier. The gecode interface requires

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- use_module(library(gecode)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several example programs are available with the distribution.

    + CREATING A SPACE

A space is gecodes data representation for a store of constraints:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space := space
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + CREATING VARIABLES

Unlike in Gecode, variable objects are not bound to a specific Space.  Each one
actually contains an index with which it is possible to access a Space-bound
Gecode variable.  Variables can be created using the following expressions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   IVar := intvar(Space,SPEC...)
   BVar := boolvar(Space)
   SVar := setvar(Space,SPEC...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where SPEC... is the same as in Gecode.  For creating lists of variables use
the following variants:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   IVars := intvars(Space,N,SPEC...)
   BVars := boolvars(Space,N,SPEC...)
   SVars := setvars(Space,N,SPEC...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where N is the number of variables to create (just like for XXXVarArray in
Gecode).  Sometimes an IntSet is necessary:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   ISet := intset([SPEC...])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where each SPEC is either an integer or a pair (I,J) of integers.  An IntSet
describes a set of ints by providing either intervals, or integers (which stand
for an interval of themselves).  It might be tempting to simply represent an
IntSet as a list of specs, but this would be ambiguous with IntArgs which,
here, are represented as lists of ints.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   Space += keep(Var)
   Space += keep(Vars)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Variables can be marked as "kept".  In this case, only such variables will be
explicitly copied during search.  This could bring substantial benefits in
memory usage.  Of course, in a solution, you can then only look at variables
that have been "kept".  If no variable is marked as "kept", then they are all
kept.  Thus marking variables as "kept" is purely an optimization.

    + CONSTRAINTS AND BRANCHINGS

all constraint and branching posting functions are available just like in
Gecode.  Wherever a XXXArgs or YYYSharedArray is expected, simply use a list.
At present, there is no support for minimodel-like constraint posting.
Constraints and branchings are added to a space using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += CONSTRAINT
    Space += BRANCHING
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += rel(X,'IRT_EQ',Y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

arrays of variables are represented by lists of variables, and constants are
represented by atoms with the same name as the Gecode constant
(e.g. 'INT_VAR_SIZE_MIN').

    + SEARCHING FOR SOLUTIONS

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    SolSpace := search(Space)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a backtrackable predicate that enumerates all solution spaces
(SolSpace).  It may also take options:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    SolSpace := search(Space,Options)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Options is a list whose elements maybe:

    + restart
to select the Restart search engine
    + threads=N
to activate the parallel search engine and control the number of
workers (see Gecode doc)
    + c_d=N
to set the commit distance for recomputation
    + a_d=N
to set the adaptive distance for recomputation



    + EXTRACTING INFO FROM A SOLUTION

An advantage of non Space-bound variables, is that you can use them both to
post constraints in the original space AND to consult their values in
solutions.  Below are methods for looking up information about variables.  Each
of these methods can either take a variable as argument, or a list of
variables, and returns resp. either a value, or a list of values:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Val := assigned(Space,X)

    Val := min(Space,X)
    Val := max(Space,X)
    Val := med(Space,X)
    Val := val(Space,X)
    Val := size(Space,X)
    Val := width(Space,X)
    Val := regret_min(Space,X)
    Val := regret_max(Space,X)

    Val := glbSize(Space,V)
    Val := lubSize(Space,V)
    Val := unknownSize(Space,V)
    Val := cardMin(Space,V)
    Val := cardMax(Space,V)
    Val := lubMin(Space,V)
    Val := lubMax(Space,V)
    Val := glbMin(Space,V)
    Val := glbMax(Space,V)
    Val := glb_ranges(Space,V)
    Val := lub_ranges(Space,V)
    Val := unknown_ranges(Space,V)
    Val := glb_values(Space,V)
    Val := lub_values(Space,V)
    Val := unknown_values(Space,V)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + DISJUNCTORS

Disjunctors provide support for disjunctions of clauses, where each clause is a
conjunction of constraints:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 or C2 or ... or Cn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each clause is executed "speculatively": this means it does not affect the main
space.  When a clause becomes failed, it is discarded.  When only one clause
remains, it is committed: this means that it now affects the main space.

Example:

Consider the problem where either X=Y=0 or X=Y+(1 or 2) for variable X and Y
that take values in 0..3.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space := space,
    [X,Y] := intvars(Space,2,0,3),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, we must create a disjunctor as a manager for our 2 clauses:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Disj := disjunctor(Space),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now create our first clause:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 := clause(Disj),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This clause wants to constrain X and Y to 0.  However, since it must be
executed "speculatively", it must operate on new variables X1 and Y1 that
shadow X and Y:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    [X1,Y1] := intvars(C1,2,0,3),
    C1 += forward([X,Y],[X1,Y1]),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The forward(...) stipulation indicates which global variable is shadowed by
which clause-local variable.  Now we can post the speculative clause-local
constraints for X=Y=0:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C1 += rel(X1,'IRT_EQ',0),
    C1 += rel(Y1,'IRT_EQ',0),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now create the second clause which uses X2 and Y2 to shadow X and Y:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    C2 := clause(Disj),
    [X2,Y2] := intvars(C2,2,0,3),
    C2 += forward([X,Y],[X2,Y2]),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, this clause also needs a clause-local variable Z2 taking values 1 or
2 in order to post the clause-local constraint X2=Y2+Z2:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Z2 := intvar(C2,1,2),
    C2 += linear([-1,1,1],[X2,Y2,Z2],'IRT_EQ',0),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, we can branch and search:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Space += branch([X,Y],'INT_VAR_SIZE_MIN','INT_VAL_MIN'),
    SolSpace := search(Space),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and lookup values of variables in each solution:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    [X_,Y_] := val(SolSpace,[X,Y]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup Gecode_and_ClPbBFDbC Programming Finite Domain Constraints in YAP/Gecode
@ingroup Gecode
@{

The gecode/clp(fd) interface is designed to use the GECODE functionality
in a more CLP like style. It requires

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- use_module(library(gecode/clpfd)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Several example programs are available with the distribution.

Integer variables are declared as:

    + _V_ in  _A_.. _B_
declares an integer variable  _V_ with range  _A_ to  _B_.
    + _Vs_ ins  _A_.. _B_
declares a set of integer variabless  _Vs_ with range  _A_ to  _B_.
    + boolvar( _V_)
declares a  boolean variable.
    + boolvars( _Vs_)
declares a set of  boolean variable.


Constraints supported are:

 
*/

/** @pred _X_ #=  _Y_
equality
 
*/

/** @pred _X_ #\\=  _Y_
disequality
 
*/

/** @pred _X_ #\>  _Y_
larger
 
*/

/** @pred _X_ #\>=  _Y_
larger or equal
 
*/

/** @pred _X_ #=\<  _Y_
smaller
 
*/

/** @pred _X_ #\<  _Y_
smaller or equal

Arguments to this constraint may be an arithmetic expression with <tt>+</tt>,
<tt>-</tt>, <tt>\\\*</tt>, integer division <tt>/</tt>, <tt>min</tt>, <tt>max</tt>, <tt>sum</tt>,
<tt>count</tt>, and
<tt>abs</tt>. Boolean variables support conjunction (/\\), disjunction (\\/),
implication (=\>), equivalence (\<=\>), and xor. The <tt>sum</tt> constraint allows  a two argument version using the
`where` conditional, in Zinc style. 

The send more money equation may be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
          1000*S + 100*E + 10*N + D +
          1000*M + 100*O + 10*R + E #=
10000*M + 1000*O + 100*N + 10*E + Y,
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example uses `where` to select from
column  _I_ the elements that have value under  _M_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
OutFlow[I] #= sum(J in 1..N where D[J,I]<M, X[J,I])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The <tt>count</tt> constraint counts the number of elements that match a
certain constant or variable (integer sets are not available).

 
*/

/** @pred all_different( _Vs_    )
 
*/

/** @pred all_distinct( _Vs_)
 
*/

/** @pred all_different( _Cs_,  _Vs_)
 
*/

/** @pred all_distinct( _Cs_,  _Vs_)
verifies whether all elements of a list are different. In the second
case, tests if all the sums between a list of constants and a list of
variables are different.

This is a formulation of the queens problem that uses both versions of `all_different`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
queens(N, Queens) :-
    length(Queens, N),
    Queens ins 1..N,
    all_distinct(Queens),
    foldl(inc, Queens, Inc, 0, _), % [0, 1, 2, .... ]
    foldl(dec, Queens, Dec, 0, _), % [0, -1, -2, ... ]
    all_distinct(Inc,Queens),
    all_distinct(Dec,Queens),
    labeling([], Queens).

inc(_, I0, I0, I) :-
    I is I0+1.

dec(_, I0, I0, I) :-
    I is I0-1.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next example uses `all_different/1` and the functionality of the matrix package to verify that all squares in
sudoku have a different value:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    foreach( [I,J] ins 0..2 ,
           all_different(M[I*3+(0..2),J*3+(0..2)]) ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred scalar_product(+ _Cs_, + _Vs_, + _Rel_, ? _V_    )

The product of constant  _Cs_ by  _Vs_ must be in relation
 _Rel_ with  _V_ .

 
*/

/** @pred _X_ #= 
all elements of  _X_  must take the same value
 
*/

/** @pred _X_ #\\= 
not all elements of  _X_  take the same value
 
*/

/** @pred _X_ #\> 
elements of  _X_  must be increasing
 
*/

/** @pred _X_ #\>= 
elements of  _X_  must be increasinga or equal
 
*/

/** @pred _X_ #=\< 
elements of  _X_  must be decreasing
 
*/

/** @pred _X_ #\< 
elements of  _X_  must be decreasing or equal

 
*/

/** @pred _X_ #\<==\>  _B_
reified equivalence
 
*/

/** @pred _X_ #==\>  _B_
reified implication
 
*/

/** @pred _X_ #\<  _B_
reified implication 

As an example. consider finding out the people who wanted to sit
next to a friend and that are are actually sitting together:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
preference_satisfied(X-Y, B) :-
    abs(X - Y) #= 1 #<==> B.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that not all constraints may be reifiable.

 
*/

/** @pred element( _X_,  _Vs_    )
 _X_ is an element of list  _Vs_

 
*/

/** @pred clause( _Type_,  _Ps_ ,  _Ns_,  _V_     )
If  _Type_ is `and` it is the conjunction of boolean variables
 _Ps_ and the negation of boolean variables  _Ns_ and must have
result  _V_. If  _Type_ is `or` it is a disjunction.

    + DFA
the interface allows creating and manipulation deterministic finite
automata. A DFA has a set of states, represented as integers
and is initialised with an initial state, a set of transitions from the
first to the last argument emitting the middle argument, and a final
state.

The swedish-drinkers protocol is represented as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    A = [X,Y,Z],
    dfa( 0, [t(0,0,0),t(0,1,1),t(1,0,0),t(-1,0,0)], [0], C),
    in_dfa( A, C ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This code will enumeratae the valid tuples of three emissions.

    + extensional constraints
Constraints can also be represented as lists of tuples. 

The previous example
would be written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    extensional_constraint([[0,0,0],[0,1,0],[1,0,0]], C),
    in_relation( A, C ),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred minimum( _X_,  _Vs_)
 
*/

/** @pred min( _X_,  _Vs_)
First Argument is the least element of a list.

 
*/

/** @pred maximum( _X_,  _Vs_)
 
*/

/** @pred max( _X_,  _Vs_)
First Argument is the greatest element of a list.

    + lex_order( _Vs_)
All elements must be ordered.



The following predicates control search:

 
*/

/** @pred labeling( _Opts_,  _Xs_)
performs labeling, several variable and value selection options are
available. The defaults are `min` and `min_step`.

Variable selection options are as follows:

    + leftmost
choose the first variable
    + min
choose one of the variables with smallest minimum value
    + max
choose one of the variables with greatest maximum value
    + ff
choose one of the most constrained variables, that is, with the smallest
domain.


Given that we selected a variable, the values chosen for branching may
be:

    + min_step
smallest value
    + max_step
largest value
    + bisect
median
    + enum
all value starting from the minimum.


 
*/

/** @pred maximize( _V_)
maximise variable  _V_

 
*/

/** @pred minimize(<tt>V</tt>) 
minimise variable  _V_




@} */

/** @defgroup Heaps Heaps
@ingroup YAPLibrary
@{

A heap is a labelled binary tree where the key of each node is less than
or equal to the keys of its sons.  The point of a heap is that we can
keep on adding new elements to the heap and we can keep on taking out
the minimum element.  If there are N elements total, the total time is
O(NlgN).  If you know all the elements in advance, you are better off
doing a merge-sort, but this file is for when you want to do say a
best-first search, and have no idea when you start how many elements
there will be, let alone what they are.

The following heap manipulation routines are available once included
with the `use_module(library(heaps))` command. 



 @pred add_to_heap(+ _Heap_,+ _key_,+ _Datum_,- _NewHeap_) 


Inserts the new  _Key-Datum_ pair into the heap. The insertion is not
stable, that is, if you insert several pairs with the same  _Key_ it
is not defined which of them will come out first, and it is possible for
any of them to come out first depending on the  history of the heap.

 
*/

/** @pred empty_heap(? _Heap_) 


Succeeds if  _Heap_ is an empty heap.

 
*/

/** @pred get_from_heap(+ _Heap_,- _key_,- _Datum_,- _Heap_) 


Returns the  _Key-Datum_ pair in  _OldHeap_ with the smallest
 _Key_, and also a  _Heap_ which is the  _OldHeap_ with that
pair deleted.

 
*/

/** @pred heap_size(+ _Heap_, - _Size_) 


Reports the number of elements currently in the heap.

 
*/

/** @pred heap_to_list(+ _Heap_, - _List_) 


Returns the current set of  _Key-Datum_ pairs in the  _Heap_ as a
 _List_, sorted into ascending order of  _Keys_.

 
*/

/** @pred list_to_heap(+ _List_, - _Heap_) 


Takes a list of  _Key-Datum_ pairs (such as keysort could be used to sort)
and forms them into a heap.

 
*/

/** @pred min_of_heap(+ _Heap_,  - _Key_,  - _Datum_) 


Returns the Key-Datum pair at the top of the heap (which is of course
the pair with the smallest Key), but does not remove it from the heap.

 
*/

/** @pred min_of_heap(+ _Heap_,  - _Key1_,  - _Datum1_,
- _Key2_,  - _Datum2_)

Returns the smallest (Key1) and second smallest (Key2) pairs in the
heap, without deleting them.



@} */

/** @defgroup Lists List Manipulation
@ingroup YAPLibrary
@{

The following list manipulation routines are available once included
with the `use_module(library(lists))` command. 



 @pred append(? _Prefix_,? _Suffix_,? _Combined_) 


True when all three arguments are lists, and the members of
 _Combined_ are the members of  _Prefix_ followed by the members of  _Suffix_.
It may be used to form  _Combined_ from a given  _Prefix_,  _Suffix_ or to take
a given  _Combined_ apart.

 
*/

/** @pred append(? _Lists_,? _Combined_)

Holds if the lists of  _Lists_ can be concatenated as a
 _Combined_ list.

 
*/

/** @pred delete(+ _List_, ? _Element_, ? _Residue_) 


True when  _List_ is a list, in which  _Element_ may or may not
occur, and  _Residue_ is a copy of  _List_ with all elements
identical to  _Element_ deleted.

 
*/

/** @pred flatten(+ _List_, ? _FlattenedList_) 


Flatten a list of lists  _List_ into a single list
 _FlattenedList_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- flatten([[1],[2,3],[4,[5,6],7,8]],L).

L = [1,2,3,4,5,6,7,8] ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred last(+ _List_,? _Last_) 


True when  _List_ is a list and  _Last_ is identical to its last element.

 
*/

/** @pred list_concat(+ _Lists_,? _List_) 


True when  _Lists_ is a list of lists and  _List_ is the
concatenation of  _Lists_.

 
*/

/** @pred member(? _Element_, ? _Set_) 


True when  _Set_ is a list, and  _Element_ occurs in it.  It may be used
to test for an element or to enumerate all the elements by backtracking.

 
*/

/** @pred memberchk(+ _Element_, + _Set_) 


As member/2, but may only be used to test whether a known
 _Element_ occurs in a known Set.  In return for this limited use, it
is more efficient when it is applicable.

 
*/

/** @pred nth0(? _N_, ? _List_, ? _Elem_) 


True when  _Elem_ is the Nth member of  _List_,
counting the first as element 0.  (That is, throw away the first
N elements and unify  _Elem_ with the next.)  It can only be used to
select a particular element given the list and index.  For that
task it is more efficient than member/2

 
*/

/** @pred nth1(? _N_, ? _List_, ? _Elem_) 


The same as nth0/3, except that it counts from
1, that is `nth(1, [H|_], H)`.

 
*/

/** @pred nth(? _N_, ? _List_, ? _Elem_) 


The same as nth1/3.

 
*/

/** @pred nth0(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_,
counting from 0, and  _Rest_ with the other elements.  It can be used
to select the Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of  _Rest_, when
it yields  _List_, e.g. `nth0(2, List, c, [a,b,d,e])` unifies List with
`[a,b,c,d,e]`.  `nth/4` is the same except that it counts from 1.  `nth0/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.

 
*/

/** @pred nth1(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Unifies  _Elem_ with the Nth element of  _List_, counting from 1,
and  _Rest_ with the other elements.  It can be used to select the
Nth element of  _List_ (yielding  _Elem_ and  _Rest_), or to
insert  _Elem_ before the Nth (counting from 1) element of
 _Rest_, when it yields  _List_, e.g. `nth(3, List, c, [a,b,d,e])` unifies List with `[a,b,c,d,e]`.  `nth/4`
can be used to insert  _Elem_ after the Nth element of  _Rest_.

 
*/

/** @pred nth(? _N_, ? _List_, ? _Elem_, ? _Rest_)

Same as `nth1/4`.

 
*/

/** @pred permutation(+ _List_,? _Perm_) 


True when  _List_ and  _Perm_ are permutations of each other.

 
*/

/** @pred remove_duplicates(+ _List_, ? _Pruned_) 


Removes duplicated elements from  _List_.  Beware: if the  _List_ has
non-ground elements, the result may surprise you.

 
*/

/** @pred reverse(+ _List_, ? _Reversed_) 


True when  _List_ and  _Reversed_ are lists with the same elements
but in opposite orders. 

 
*/

/** @pred same_length(? _List1_, ? _List2_) 


True when  _List1_ and  _List2_ are both lists and have the same number
of elements.  No relation between the values of their elements is
implied.
Modes `same_length(-,+)` and `same_length(+,-)` generate either list given
the other; mode `same_length(-,-)` generates two lists of the same length,
in which case the arguments will be bound to lists of length 0, 1, 2, ...

 
*/

/** @pred select(? _Element_, ? _List_, ? _Residue_) 


True when  _Set_ is a list,  _Element_ occurs in  _List_, and
 _Residue_ is everything in  _List_ except  _Element_ (things
stay in the same order).

 
*/

/** @pred selectchk(? _Element_, ? _List_, ? _Residue_) 


Semi-deterministic selection from a list. Steadfast: defines as

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
selectchk(Elem, List, Residue) :-
        select(Elem, List, Rest0), !,
        Rest = Rest0.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred sublist(? _Sublist_, ? _List_) 


True when both `append(_,Sublist,S)` and `append(S,_,List)` hold.

 
*/

/** @pred suffix(? _Suffix_, ? _List_) 


Holds when `append(_,Suffix,List)` holds.

 
*/

/** @pred sum_list(? _Numbers_, ? _Total_) 


True when  _Numbers_ is a list of numbers, and  _Total_ is their sum.

 
*/

/** @pred sum_list(? _Numbers_, + _SoFar_, ? _Total_)

True when  _Numbers_ is a list of numbers, and  _Total_ is the sum of their total plus  _SoFar_.

 
*/

/** @pred sumlist(? _Numbers_, ? _Total_) 


True when  _Numbers_ is a list of integers, and  _Total_ is their
sum. The same as sum_list/2, please do use sum_list/2
instead.

 
*/

/** @pred max_list(? _Numbers_, ? _Max_) 


True when  _Numbers_ is a list of numbers, and  _Max_ is the maximum.

 
*/

/** @pred min_list(? _Numbers_, ? _Min_) 


True when  _Numbers_ is a list of numbers, and  _Min_ is the minimum.

 
*/

/** @pred numlist(+ _Low_, + _High_, + _List_) 


If  _Low_ and  _High_ are integers with  _Low_ =\<
 _High_, unify  _List_ to a list `[Low, Low+1, ...High]`. See
also between/3.

 
*/

/** @pred intersection(+ _Set1_, + _Set2_, + _Set3_) 


Succeeds if  _Set3_ unifies with the intersection of  _Set1_ and
 _Set2_.  _Set1_ and  _Set2_ are lists without duplicates. They
need not be ordered.

 
*/

/** @pred subtract(+ _Set_, + _Delete_, ? _Result_) 


Delete all elements from  _Set_ that   occur  in  _Delete_ (a set)
and unify the  result  with   _Result_.   Deletion  is  based  on
unification using memberchk/2. The complexity is
`|Delete|\*|Set|`.

See ord_subtract/3.



@} */

/** @defgroup LineUtilities Line Manipulation Utilities
@ingroup YAPLibrary
@{

This package provides a set of useful predicates to manipulate
sequences of characters codes, usually first read in as a line. It is
available by loading the library `library(lineutils)`.



 @pred search_for(+ _Char_,+ _Line_) 



Search for a character  _Char_ in the list of codes  _Line_.

 
*/

/** @pred search_for(+ _Char_,+ _Line_,- _RestOfine_)


Search for a character  _Char_ in the list of codes  _Line_,
 _RestOfLine_ has the line to the right.

 
*/

/** @pred scan_natural(? _Nat_,+ _Line_,+ _RestOfLine_) 



Scan the list of codes  _Line_ for a natural number  _Nat_, zero
or a positive integer, and unify  _RestOfLine_ with the remainder
of the line.

 
*/

/** @pred scan_integer(? _Int_,+ _Line_,+ _RestOfLine_) 



Scan the list of codes  _Line_ for an integer  _Nat_, either a
positive, zero, or negative integer, and unify  _RestOfLine_ with
the remainder of the line.

 
*/

/** @pred split(+ _Line_,+ _Separators_,- _Split_) 



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators. As an
example, consider:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- split("Hello * I am free"," *",S).

S = ["Hello","I","am","free"] ?

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred split(+ _Line_,- _Split_)


Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as separators.

 
*/

/** @pred fields(+ _Line_,+ _Separators_,- _Split_) 



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators for
fields. If two separators occur in a row, the field is considered
empty. As an example, consider:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- fields("Hello  I am  free"," *",S).

S = ["Hello","","I","am","","free"] ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred fields(+ _Line_,- _Split_)


Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as field separators.

 
*/

/** @pred glue(+ _Words_,+ _Separator_,- _Line_) 



Unify  _Line_ with  string obtained by glueing  _Words_ with
the character code  _Separator_.

 
*/

/** @pred copy_line(+ _StreamInput_,+ _StreamOutput_) 



Copy a line from  _StreamInput_ to  _StreamOutput_.

 
*/

/** @pred process(+ _StreamInp_, + _Goal_) 



For every line  _LineIn_ in stream  _StreamInp_, call
`call(Goal,LineIn)`.

 
*/

/** @pred filter(+ _StreamInp_, + _StreamOut_, + _Goal_) 



For every line  _LineIn_ in stream  _StreamInp_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to
stream  _StreamOut_.

 
*/

/** @pred file_filter(+ _FileIn_, + _FileOut_, + _Goal_) 



For every line  _LineIn_ in file  _FileIn_, execute
`call(Goal,LineIn,LineOut)`, and output  _LineOut_ to file
 _FileOut_.

 
*/

/** @pred file_filter(+ _FileIn_, + _FileOut_, + _Goal_, 

+ _FormatCommand_,   + _Arguments_)


Same as file_filter/3, but before starting the filter execute
`format/3` on the output stream, using  _FormatCommand_ and
 _Arguments_.




@} */

/** @defgroup matrix Matrix Library
@ingroup YAPLibrary
@{

This package provides a fast implementation of multi-dimensional
matrices of integers and floats. In contrast to dynamic arrays, these
matrices are multi-dimensional and compact. In contrast to static
arrays. these arrays are allocated in the stack. Matrices are available
by loading the library `library(matrix)`. They are multimensional
objects of type:

    + <tt>terms</tt>: Prolog terms
    + <tt>ints</tt>: bounded integers, represented as an opaque term. The
maximum integer depends on hardware, but should be obtained from the
natural size of the machine.
    + <tt>floats</tt>: floating-poiny numbers, represented as an opaque term.


Matrix elements can be accessed through the `matrix_get/2`
predicate or through an <tt>R</tt>-inspired access notation (that uses the ciao
style extension to `[]`.  Examples include:

 
*/

/** @pred _E_ \<==  _X_[2,3]
Access the second row, third column of matrix <tt>X</tt>. Indices start from
`0`,
 
*/

/** @pred _L_ \<==  _X_[2,_]
Access all the second row, the output is a list ofe elements.
 
*/

/** @pred _L_ \<==  _X_[2..4,_]
Access all the second, thrd and fourth rows, the output is a list of elements.
 
*/

/** @pred _L_ \<==  _X_[2..4+3,_]
Access all the fifth, sixth and eight rows, the output is a list of elements.


The matrix library also supports a B-Prolog/ECliPSe inspired `foreach` ITerator to iterate over
elements of a matrix:

 
*/

/** @pred foreach(I in 0..N1, X[I] \<== Y[I])
Copies a vector, element by element.
 
*/

/** @pred foreach([I in 0..N1, J in I..N1], Z[I,J] \<== X[I,J] - X[I,J])
The lower-triangular matrix  _Z_ is the difference between the
lower-triangular and upper-triangular parts of  _X_.
 
*/

/** @pred foreach([I in 0..N1, J in 0..N1], plus(X[I,J]), 0, Sum)
Add all elements of a matrix by using  _Sum_ as an accumulator.


Notice that the library does not support all known matrix operations. Please
contact the YAP maintainers if you require extra functionality.



    + _X_ = array[ _Dim1_,..., _Dimn_] of  _Objects_ 


The of/2 operator can be used to create a new array of
 _Objects_. The objects supported are:

    + Unbound Variable
create an array of free variables
    + ints 
create an array of integers
    + floats 
create an array of floating-point numbers
    + _I_: _J_
create an array with integers from  _I_ to  _J_
    + [..]
create an array from the values in a list


The dimensions can be given as an integer, and the matrix will be
indexed `C`-style from  `0..( _Max_-1)`, or can be given
as  an interval ` _Base_.. _Limit_`. In the latter case,
matrices of integers and of floating-point numbers should have the same
 _Base_ on every dimension.

 
*/

/** @pred ? _LHS_ \<==  _RHS_ 


General matrix assignment operation. It evaluates the right-hand side
and then acts different according to the
left-hand side and to the matrix:

    + if  _LHS_ is part of an integer or floating-point matrix,
perform non-backtrackable assignment.
    + other unify left-hand side and right-hand size.


The right-hand side supports the following operators: 

    + []/2
written as  _M_[ _Offset_]: obtain an element or list of elements
of matrix  _M_ at offset  _Offset_.
    + matrix/1
create a vector from a list
    + matrix/2
create a matrix from a list. Oprions are:

    + dim=
a list of dimensiona
    + type=
integers, floating-point or terms
    + base=
a list of base offsets per dimension (all must be the same for arrays of
integers and floating-points

    + matrix/3
create matrix giving two options
    + dim/1
list with matrix dimensions
    + nrow/1
number of rows in bi-dimensional matrix
    + ncol/1
number of columns in bi-dimensional matrix
    + length/1
size of a matrix
    + size/1
size of a matrix
    + max/1
maximum element of a numeric matrix
    + maxarg/1
argument of maximum element of a numeric matrix
    + min/1
minimum element of a numeric matrix
    + minarg/1
argument of minimum element of a numeric matrix
    + list/1
represent matrix as a list
    + lists/2
represent matrix as list of embedded lists
    + ../2
 _I_.. _J_ generates a list with all integers from  _I_ to
 _J_, included.
    + +/2
add two numbers, add two matrices element-by-element, or add a number to
all elements of a matrix or list
    + -/2 
subtract two numbers, subtract two matrices or lists element-by-element, or subtract a number from
all elements of a matrix or list
    + \* /2 
multiply two numbers, multiply two matrices or lists element-by-element, or multiply a number from
all elements of a matrix or list
    + log/1 
natural logarithm of a number, matrix or list
    + exp/1 
natural exponentiation of a number, matrix or list


 
*/

/** @pred foreach( _Sequence_,  _Goal_) 


Deterministic iterator. The ranges are given by  _Sequence_ that is
either ` _I_ in  _M_.. _N_`, or of the form 
`[ _I_, _J_] ins  _M_.. _N_`, or a list of the above conditions. 

Variables in the goal are assumed to be global, ie, share a single value
in the execution. The exceptions are the iteration indices. Moreover, if
the goal is of the form ` _Locals_^ _G_` all variables
occurring in  _Locals_ are marked as local. As an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
foreach([I,J] ins 1..N, A^(A <==M[I,J], N[I] <== N[I] + A*A) )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the variables  _I_,  _J_ and  _A_ are duplicated for every
call (local), whereas the matrices  _M_ and  _N_ are shared
throughout the execution (global).

 
*/

/** @pred foreach( _Sequence_,  _Goal_,  _Acc0_,  _AccF_)

Deterministic iterator with accumulator style arguments.

 
*/

/** @pred matrix_new(+ _Type_,+ _Dims_,- _Matrix_) 



Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, and with a list of dimensions  _Dims_.
The matrix will be initialised to zeros.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- matrix_new(ints,[2,3],Matrix).

Matrix = {..}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that currently YAP will always write a matrix of numbers as `{..}`.

 
*/

/** @pred matrix_new(+ _Type_,+ _Dims_,+ _List_,- _Matrix_)


Create a new matrix  _Matrix_ of type  _Type_, which may be one of
`ints` or `floats`, with dimensions  _Dims_, and
initialised from list  _List_.

 
*/

/** @pred matrix_new_set(? _Dims_,+ _OldMatrix_,+ _Value_,- _NewMatrix_) 



Create a new matrix  _NewMatrix_ of type  _Type_, with dimensions
 _Dims_. The elements of  _NewMatrix_ are set to  _Value_.

 
*/

/** @pred matrix_dims(+ _Matrix_,- _Dims_) 



Unify  _Dims_ with a list of dimensions for  _Matrix_.

 
*/

/** @pred matrix_ndims(+ _Matrix_,- _Dims_) 



Unify  _NDims_ with the number of dimensions for  _Matrix_.

 
*/

/** @pred matrix_size(+ _Matrix_,- _NElems_) 



Unify  _NElems_ with the number of elements for  _Matrix_.

 
*/

/** @pred matrix_type(+ _Matrix_,- _Type_) 



Unify  _NElems_ with the type of the elements in  _Matrix_.

 
*/

/** @pred matrix_to_list(+ _Matrix_,- _Elems_) 



Unify  _Elems_ with the list including all the elements in  _Matrix_.

 
*/

/** @pred matrix_get(+ _Matrix_,+ _Position_,- _Elem_) 



Unify  _Elem_ with the element of  _Matrix_ at position
 _Position_.

 
*/

/** @pred matrix_get(+ _Matrix_[+ _Position_],- _Elem_)


Unify  _Elem_ with the element  _Matrix_[ _Position_].

 
*/

/** @pred matrix_set(+ _Matrix_,+ _Position_,+ _Elem_) 



Set the element of  _Matrix_ at position
 _Position_ to   _Elem_.

 
*/

/** @pred matrix_set(+ _Matrix_[+ _Position_],+ _Elem_)


Set the element of  _Matrix_[ _Position_] to   _Elem_.

 
*/

/** @pred matrix_set_all(+ _Matrix_,+ _Elem_) 



Set all element of  _Matrix_ to  _Elem_.

 
*/

/** @pred matrix_add(+ _Matrix_,+ _Position_,+ _Operand_) 



Add  _Operand_ to the element of  _Matrix_ at position
 _Position_.

 
*/

/** @pred matrix_inc(+ _Matrix_,+ _Position_) 



Increment the element of  _Matrix_ at position  _Position_.

 
*/

/** @pred matrix_inc(+ _Matrix_,+ _Position_,- _Element_)


Increment the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.

 
*/

/** @pred matrix_dec(+ _Matrix_,+ _Position_) 



Decrement the element of  _Matrix_ at position  _Position_.

 
*/

/** @pred matrix_dec(+ _Matrix_,+ _Position_,- _Element_)


Decrement the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.

 
*/

/** @pred matrix_arg_to_offset(+ _Matrix_,+ _Position_,- _Offset_) 



Given matrix  _Matrix_ return what is the numerical  _Offset_ of
the element at  _Position_.

 
*/

/** @pred matrix_offset_to_arg(+ _Matrix_,- _Offset_,+ _Position_) 



Given a position  _Position _ for matrix  _Matrix_ return the
corresponding numerical  _Offset_ from the beginning of the matrix.

 
*/

/** @pred matrix_max(+ _Matrix_,+ _Max_) 



Unify  _Max_ with the maximum in matrix   _Matrix_.

 
*/

/** @pred matrix_maxarg(+ _Matrix_,+ _Maxarg_) 



Unify  _Max_ with the position of the maximum in matrix   _Matrix_.

 
*/

/** @pred matrix_min(+ _Matrix_,+ _Min_) 



Unify  _Min_ with the minimum in matrix   _Matrix_.

 
*/

/** @pred matrix_minarg(+ _Matrix_,+ _Minarg_) 



Unify  _Min_ with the position of the minimum in matrix   _Matrix_.

 
*/

/** @pred matrix_sum(+ _Matrix_,+ _Sum_) 



Unify  _Sum_ with the sum of all elements in matrix   _Matrix_.

 
*/

/** @pred matrix_agg_lines(+ _Matrix_,+ _Aggregate_) 



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the n-1 dimensional matrix where each element is obtained by adding all
Matrix elements with same last n-1 index.

 
*/

/** @pred matrix_agg_cols(+ _Matrix_,+ _Aggregate_) 



If  _Matrix_ is a n-dimensional matrix, unify  _Aggregate_ with
the one dimensional matrix where each element is obtained by adding all
Matrix elements with same  first index.

 
*/

/** @pred matrix_op(+ _Matrix1_,+ _Matrix2_,+ _Op_,- _Result_) 



 _Result_ is the result of applying  _Op_ to matrix  _Matrix1_
and  _Matrix2_. Currently, only addition (`+`) is supported.

 
*/

/** @pred matrix_op_to_all(+ _Matrix1_,+ _Op_,+ _Operand_,- _Result_) 



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with  _Operand_ as the second argument. Currently,
only addition (`+`), multiplication (`\*`), and division
(`/`) are supported.

 
*/

/** @pred matrix_op_to_lines(+ _Matrix1_,+ _Lines_,+ _Op_,- _Result_) 



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Lines_ as the
second argument. Currently, only division (`/`) is supported.

 
*/

/** @pred matrix_op_to_cols(+ _Matrix1_,+ _Cols_,+ _Op_,- _Result_) 



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Cols_ as the
second argument. Currently, only addition (`+`) is
supported. Notice that  _Cols_ will have n-1 dimensions.

 
*/

/** @pred matrix_shuffle(+ _Matrix_,+ _NewOrder_,- _Shuffle_) 



Shuffle the dimensions of matrix  _Matrix_ according to
 _NewOrder_. The list  _NewOrder_ must have all the dimensions of
 _Matrix_, starting from 0.

 
*/

/** @pred matrix_transpose(+ _Matrix_,- _Transpose_) 



Transpose matrix  _Matrix_ to   _Transpose_. Equivalent to:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matrix_transpose(Matrix,Transpose) :-
        matrix_shuffle(Matrix,[1,0],Transpose).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred matrix_expand(+ _Matrix_,+ _NewDimensions_,- _New_) 



Expand  _Matrix_ to occupy new dimensions. The elements in
 _NewDimensions_ are either 0, for an existing dimension, or a
positive integer with the size of the new dimension.

 
*/

/** @pred matrix_select(+ _Matrix_,+ _Dimension_,+ _Index_,- _New_) 



Select from  _Matrix_ the elements who have  _Index_ at
 _Dimension_.

 
*/

/** @pred matrix_row(+ _Matrix_,+ _Column_,- _NewMatrix_) 



Select from  _Matrix_ the row matching  _Column_ as new matrix  _NewMatrix_.  _Column_ must have one less dimension than the original matrix.
 _Dimension_.




@} */

/** @defgroup MATLAB MATLAB Package Interface
@ingroup YAPLibrary
@{

The MathWorks MATLAB is a widely used package for array
processing. YAP now includes a straightforward interface to MATLAB. To
actually use it, you need to install YAP calling `configure` with
the `--with-matlab=DIR` option, and you need to call
`use_module(library(lists))` command.

Accessing the matlab dynamic libraries can be complicated. In Linux
machines, to use this interface, you may have to set the environment
variable <tt>LD_LIBRARY_PATH</tt>. Next, follows an example using bash in a
64-bit Linux PC:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export LD_LIBRARY_PATH=''$MATLAB_HOME"/sys/os/glnxa64:''$MATLAB_HOME"/bin/glnxa64:''$LD_LIBRARY_PATH"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
where `MATLAB_HOME` is the directory where matlab is installed
at. Please replace `ax64` for `x86` on a 32-bit PC.



 @pred start_matlab(+ _Options_) 


Start a matlab session. The argument  _Options_ may either be the
empty string/atom or the command to call matlab. The command may fail.

 
*/

/** @pred close_matlab 


Stop the current matlab session.

 
*/

/** @pred matlab_on 


Holds if a matlab session is on.

 
*/

/** @pred matlab_eval_string(+ _Command_) 


Holds if matlab evaluated successfully the command  _Command_.

 
*/

/** @pred matlab_eval_string(+ _Command_, - _Answer_)

MATLAB will evaluate the command  _Command_ and unify  _Answer_
with a string reporting the result.

 
*/

/** @pred matlab_cells(+ _Size_, ? _Array_) 


MATLAB will create an empty vector of cells of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command `cells`.

 
*/

/** @pred matlab_cells(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an empty array of cells of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `cells`.

 
*/

/** @pred matlab_initialized_cells(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) 


MATLAB will create an array of cells of size  _SizeX_ and
 _SizeY_, initialized from the list  _List_, and if  _Array_
is bound to an atom, store the array in the matlab variable with name
 _Array_.

 
*/

/** @pred matlab_matrix(+ _SizeX_, + _SizeY_, + _List_, ? _Array_) 


MATLAB will create an array of floats of size  _SizeX_ and  _SizeY_,
initialized from the list  _List_, and if  _Array_ is bound to
an atom, store the array in the matlab variable with name  _Array_.

 
*/

/** @pred matlab_set(+ _MatVar_, + _X_, + _Y_, + _Value_) 


Call MATLAB to set element  _MatVar_( _X_,  _Y_) to
 _Value_. Notice that this command uses the MATLAB array access
convention.

 
*/

/** @pred matlab_get_variable(+ _MatVar_, - _List_) 


Unify MATLAB variable  _MatVar_ with the List  _List_.

 
*/

/** @pred matlab_item(+ _MatVar_, + _X_, ? _Val_) 


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

 
*/

/** @pred matlab_item(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
`C` notation for matrix access (ie, starting from 0).

 
*/

/** @pred matlab_item1(+ _MatVar_, + _X_, ? _Val_) 


Read or set MATLAB  _MatVar_( _X_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

 
*/

/** @pred matlab_item1(+ _MatVar_, + _X_, + _Y_, ? _Val_)

Read or set MATLAB  _MatVar_( _X_, _Y_) from/to  _Val_. Use
MATLAB notation for matrix access (ie, starting from 1).

 
*/

/** @pred matlab_sequence(+ _Min_, + _Max_, ? _Array_) 


MATLAB will create a sequence going from  _Min_ to  _Max_, and
if  _Array_ is bound to an atom, store the sequence in the matlab
variable with name  _Array_.

 
*/

/** @pred matlab_vector(+ _Size_, + _List_, ? _Array_) 


MATLAB will create a vector of floats of size  _Size_, initialized
from the list  _List_, and if  _Array_ is bound to an atom,
store the array in the matlab variable with name  _Array_.

 
*/

/** @pred matlab_zeros(+ _Size_, ? _Array_) 


MATLAB will create a vector of zeros of size  _Size_, and if
 _Array_ is bound to an atom, store the array in the matlab
variable with name  _Array_. Corresponds to the MATLAB command
`zeros`.

 
*/

/** @pred matlab_zeros(+ _SizeX_, + _SizeY_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_ and
 _SizeY_, and if  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.

 
*/

/** @pred matlab_zeros(+ _SizeX_, + _SizeY_, + _SizeZ_, ? _Array_)

MATLAB will create an array of zeros of size  _SizeX_,  _SizeY_,
and  _SizeZ_. If  _Array_ is bound to an atom, store the array
in the matlab variable with name  _Array_.  Corresponds to the
MATLAB command `zeros`.




@} */

/** @defgroup NonhYBacktrackable_Data_Structures Non-Backtrackable Data Structures
@ingroup YAPLibrary
@{

The following routines implement well-known data-structures using global
non-backtrackable variables (implemented on the Prolog stack). The
data-structures currently supported are Queues, Heaps, and Beam for Beam
search. They are allowed through `library(nb)`. 

 
*/

/** @pred nb_queue(- _Queue_) 


Create a  _Queue_.

 
*/

/** @pred nb_queue_close(+ _Queue_, - _Head_, ? _Tail_) 


Unify the queue   _Queue_ with a difference list
 _Head_- _Tail_. The queue will now be empty and no further
elements can be added.

 
*/

/** @pred nb_queue_enqueue(+ _Queue_, + _Element_) 


Add  _Element_ to the front of the queue   _Queue_.

 
*/

/** @pred nb_queue_dequeue(+ _Queue_, - _Element_) 


Remove  _Element_ from the front of the queue   _Queue_. Fail if
the queue is empty.

 
*/

/** @pred nb_queue_peek(+ _Queue_, - _Element_) 


 _Element_ is the front of the queue   _Queue_. Fail if
the queue is empty.

 
*/

/** @pred nb_queue_size(+ _Queue_, - _Size_) 


Unify  _Size_ with the number of elements in the queue   _Queue_.

 
*/

/** @pred nb_queue_empty(+ _Queue_) 


Succeeds if   _Queue_ is empty.

 
*/

/** @pred nb_heap(+ _DefaultSize_,- _Heap_) 


Create a  _Heap_ with default size  _DefaultSize_. Note that size
will expand as needed.

 
*/

/** @pred nb_heap_close(+ _Heap_) 


Close the heap  _Heap_: no further elements can be added.

 
*/

/** @pred nb_heap_add(+ _Heap_, + _Key_, + _Value_) 


Add  _Key_- _Value_ to the heap  _Heap_. The key is sorted on
 _Key_ only.

 
*/

/** @pred nb_heap_del(+ _Heap_, - _Key_, - _Value_) 


Remove element  _Key_- _Value_ with smallest  _Value_ in heap
 _Heap_. Fail if the heap is empty.

 
*/

/** @pred nb_heap_peek(+ _Heap_, - _Key_, - _Value_)) 


 _Key_- _Value_ is the element with smallest  _Key_ in the heap
 _Heap_. Fail if the heap is empty.

 
*/

/** @pred nb_heap_size(+ _Heap_, - _Size_) 


Unify  _Size_ with the number of elements in the heap   _Heap_.

 
*/

/** @pred nb_heap_empty(+ _Heap_) 


Succeeds if   _Heap_ is empty.

 
*/

/** @pred nb_beam(+ _DefaultSize_,- _Beam_) 


Create a  _Beam_ with default size  _DefaultSize_. Note that size
is fixed throughout.

 
*/

/** @pred nb_beam_close(+ _Beam_) 


Close the beam  _Beam_: no further elements can be added.

 
*/

/** @pred nb_beam_add(+ _Beam_, + _Key_, + _Value_) 


Add  _Key_- _Value_ to the beam  _Beam_. The key is sorted on
 _Key_ only.

 
*/

/** @pred nb_beam_del(+ _Beam_, - _Key_, - _Value_) 


Remove element  _Key_- _Value_ with smallest  _Value_ in beam
 _Beam_. Fail if the beam is empty.

 
*/

/** @pred nb_beam_peek(+ _Beam_, - _Key_, - _Value_)) 


 _Key_- _Value_ is the element with smallest  _Key_ in the beam
 _Beam_. Fail if the beam is empty.

 
*/

/** @pred nb_beam_size(+ _Beam_, - _Size_) 


Unify  _Size_ with the number of elements in the beam   _Beam_.

 
*/

/** @pred nb_beam_empty(+ _Beam_) 


Succeeds if   _Beam_ is empty.




@} */

/** @defgroup Ordered_Sets Ordered Sets
@ingroup YAPLibrary
@{

The following ordered set manipulation routines are available once
included with the `use_module(library(ordsets))` command.  An
ordered set is represented by a list having unique and ordered
elements. Output arguments are guaranteed to be ordered sets, if the
relevant inputs are. This is a slightly patched version of Richard
O'Keefe's original library.

 
*/

/** @pred list_to_ord_set(+ _List_, ? _Set_) 


Holds when  _Set_ is the ordered representation of the set
represented by the unordered representation  _List_.

 
*/

/** @pred merge(+ _List1_, + _List2_, - _Merged_) 


Holds when  _Merged_ is the stable merge of the two given lists.

Notice that merge/3 will not remove duplicates, so merging
ordered sets will not necessarily result in an ordered set. Use
`ord_union/3` instead.

 
*/

/** @pred ord_add_element(+ _Set1_, + _Element_, ? _Set2_) 


Inserting  _Element_ in  _Set1_ returns  _Set2_.  It should give
exactly the same result as `merge(Set1, [Element], Set2)`, but a
bit faster, and certainly more clearly. The same as ord_insert/3.

 
*/

/** @pred ord_del_element(+ _Set1_, + _Element_, ? _Set2_) 


Removing  _Element_ from  _Set1_ returns  _Set2_.

 
*/

/** @pred ord_disjoint(+ _Set1_, + _Set2_) 


Holds when the two ordered sets have no element in common.

 
*/

/** @pred ord_member(+ _Element_, + _Set_) 


Holds when  _Element_ is a member of  _Set_.

 
*/

/** @pred ord_insert(+ _Set1_, + _Element_, ? _Set2_) 


Inserting  _Element_ in  _Set1_ returns  _Set2_.  It should give
exactly the same result as `merge(Set1, [Element], Set2)`, but a
bit faster, and certainly more clearly. The same as ord_add_element/3.

 
*/

/** @pred ord_intersect(+ _Set1_, + _Set2_) 


Holds when the two ordered sets have at least one element in common.

 
*/

/** @pred ord_intersection(+ _Set1_, + _Set2_, ? _Intersection_)

Holds when Intersection is the ordered representation of  _Set1_
and  _Set2_.

 
*/

/** @pred ord_intersection(+ _Set1_, + _Set2_, ? _Intersection_, ? _Diff_)

Holds when Intersection is the ordered representation of  _Set1_
and  _Set2_.  _Diff_ is the difference between  _Set2_ and  _Set1_.

 
*/

/** @pred ord_seteq(+ _Set1_, + _Set2_) 


Holds when the two arguments represent the same set.

 
*/

/** @pred ord_setproduct(+ _Set1_, + _Set2_, - _Set_) 


If Set1 and Set2 are ordered sets, Product will be an ordered
set of x1-x2 pairs.

 
*/

/** @pred ord_subset(+ _Set1_, + _Set2_) 


Holds when every element of the ordered set  _Set1_ appears in the
ordered set  _Set2_.

 
*/

/** @pred ord_subtract(+ _Set1_, + _Set2_, ? _Difference_) 


Holds when  _Difference_ contains all and only the elements of  _Set1_
which are not also in  _Set2_.

 
*/

/** @pred ord_symdiff(+ _Set1_, + _Set2_, ? _Difference_) 


Holds when  _Difference_ is the symmetric difference of  _Set1_
and  _Set2_.

 
*/

/** @pred ord_union(+ _Sets_, ? _Union_) 


Holds when  _Union_ is the union of the lists  _Sets_.

 
*/

/** @pred ord_union(+ _Set1_, + _Set2_, ? _Union_)

Holds when  _Union_ is the union of  _Set1_ and  _Set2_.

 
*/

/** @pred ord_union(+ _Set1_, + _Set2_, ? _Union_, ? _Diff_)

Holds when  _Union_ is the union of  _Set1_ and  _Set2_ and
 _Diff_ is the difference.




@} */

/** @defgroup Pseudo_Random Pseudo Random Number Integer Generator
@ingroup YAPLibrary
@{

The following routines produce random non-negative integers in the range
0 .. 2^(w-1) -1, where w is the word size available for integers, e.g.
32 for Intel machines and 64 for Alpha machines. Note that the numbers
generated by this random number generator are repeatable. This generator
was originally written by Allen Van Gelder and is based on Knuth Vol 2.

 
*/

/** @pred rannum(- _I_) 


Produces a random non-negative integer  _I_ whose low bits are not
all that random, so it should be scaled to a smaller range in general.
The integer  _I_ is in the range 0 .. 2^(w-1) - 1. You can use:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rannum(X) :- yap_flag(max_integer,MI), rannum(R), X is R/MI.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
to obtain a floating point number uniformly distributed between 0 and 1.

 
*/

/** @pred ranstart 


Initialize the random number generator using a built-in seed. The
ranstart/0 built-in is always called by the system when loading
the package.

 
*/

/** @pred ranstart(+ _Seed_)

Initialize the random number generator with user-defined  _Seed_. The
same  _Seed_ always produces the same sequence of numbers.

 
*/

/** @pred ranunif(+ _Range_,- _I_) 


ranunif/2 produces a uniformly distributed non-negative random
integer  _I_ over a caller-specified range  _R_.  If range is  _R_,
the result is in 0 ..  _R_-1.




@} */

/** @defgroup Queues Queues
@ingroup YAPLibrary
@{

The following queue manipulation routines are available once
included with the `use_module(library(queues))` command. Queues are
implemented with difference lists.



 @pred make_queue(+ _Queue_) 


Creates a new empty queue. It should only be used to create a new queue.

 
*/

/** @pred join_queue(+ _Element_, + _OldQueue_, - _NewQueue_) 


Adds the new element at the end of the queue.

 
*/

/** @pred list_join_queue(+ _List_, + _OldQueue_, - _NewQueue_) 


Ads the new elements at the end of the queue.

 
*/

/** @pred jump_queue(+ _Element_, + _OldQueue_, - _NewQueue_) 


Adds the new element at the front of the list.

 
*/

/** @pred list_jump_queue(+ _List_, + _OldQueue_, + _NewQueue_) 


Adds all the elements of  _List_ at the front of the queue.

 
*/

/** @pred head_queue(+ _Queue_, ? _Head_) 


Unifies Head with the first element of the queue.

 
*/

/** @pred serve_queue(+ _OldQueue_, + _Head_, - _NewQueue_) 


Removes the first element of the queue for service.

 
*/

/** @pred empty_queue(+ _Queue_) 


Tests whether the queue is empty.

 
*/

/** @pred length_queue(+ _Queue_, - _Length_) 


Counts the number of elements currently in the queue.

 
*/

/** @pred list_to_queue(+ _List_, - _Queue_) 


Creates a new queue with the same elements as  _List._

 
*/

/** @pred queue_to_list(+ _Queue_, - _List_) 


Creates a new list with the same elements as  _Queue_.




@} */

/** @defgroup Random Random Number Generator
@ingroup YAPLibrary
@{

The following random number operations are included with the
`use_module(library(random))` command. Since YAP-4.3.19 YAP uses
the O'Keefe public-domain algorithm, based on the "Applied Statistics"
algorithm AS183.



 @pred getrand(- _Key_) 


Unify  _Key_ with a term of the form `rand(X,Y,Z)` describing the
current state of the random number generator.

 
*/

/** @pred random(- _Number_) 


Unify  _Number_ with a floating-point number in the range `[0...1)`.

 
*/

/** @pred random(+ _LOW_, + _HIGH_, - _NUMBER_)

Unify  _Number_ with a number in the range
`[LOW...HIGH)`. If both  _LOW_ and  _HIGH_ are
integers then  _NUMBER_ will also be an integer, otherwise
 _NUMBER_ will be a floating-point number.

 
*/

/** @pred randseq(+ _LENGTH_, + _MAX_, - _Numbers_) 


Unify  _Numbers_ with a list of  _LENGTH_ unique random integers
in the range `[1... _MAX_)`.

 
*/

/** @pred randset(+ _LENGTH_, + _MAX_, - _Numbers_) 


Unify  _Numbers_ with an ordered list of  _LENGTH_ unique random
integers in the range `[1... _MAX_)`.

 
*/

/** @pred setrand(+ _Key_) 


Use a term of the form `rand(X,Y,Z)` to set a new state for the
random number generator. The integer `X` must be in the range
`[1...30269)`, the integer `Y` must be in the range
`[1...30307)`, and the integer `Z` must be in the range
`[1...30323)`.




@} */

/** @defgroup Read_Utilities Read Utilities
@ingroup YAPLibrary
@{

The `readutil` library contains primitives to read lines, files,
multiple terms, etc.

 
*/

/** @pred read_line_to_codes(+ _Stream_, - _Codes_) 



Read the next line of input from  _Stream_ and unify the result with
 _Codes_ <em>after</em> the line has been read.  A line is ended by a
newline character or end-of-file. Unlike `read_line_to_codes/3`,
this predicate removes trailing newline character.

On end-of-file the atom `end_of_file` is returned.  See also
`at_end_of_stream/[0,1]`.

 
*/

/** @pred read_line_to_codes(+ _Stream_, - _Codes_, ? _Tail_)

Difference-list version to read an input line to a list of character
codes.  Reading stops at the newline or end-of-file character, but
unlike read_line_to_codes/2, the newline is retained in the
output.  This predicate is especially useful for reading a block of
lines upto some delimiter.  The following example reads an HTTP header
ended by a blank line:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_header_data(Stream, Header) :-
    read_line_to_codes(Stream, Header, Tail),
    read_header_data(Header, Stream, Tail).

read_header_data("\r\n", _, _) :- !.
read_header_data("\n", _, _) :- !.
read_header_data("", _, _) :- !.
read_header_data(_, Stream, Tail) :-
    read_line_to_codes(Stream, Tail, NewTail),
    read_header_data(Tail, Stream, NewTail).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred read_stream_to_codes(+ _Stream_, - _Codes_) 


Read all input until end-of-file and unify the result to  _Codes_.

 
*/

/** @pred read_stream_to_codes(+ _Stream_, - _Codes_, ? _Tail_)

Difference-list version of read_stream_to_codes/2.

 
*/

/** @pred read_file_to_codes(+ _Spec_, - _Codes_, + _Options_) 


Read a file to a list of character codes. Currently ignores
 _Options_.

 
*/

/** @pred read_file_to_terms(+ _Spec_, - _Terms_, + _Options_) 


Read a file to a list of Prolog terms (see read/1). @c  _Spec_ is a









@} */

/** @defgroup RedhYBlack_Trees Red-Black Trees
@ingroup YAPLibrary
@{

Red-Black trees are balanced search binary trees. They are named because
nodes can be classified as either red or black. The code we include is
based on "Introduction to Algorithms", second edition, by Cormen,
Leiserson, Rivest and Stein.  The library includes routines to insert,
lookup and delete elements in the tree.

 
*/

/** @pred rb_new(? _T_) 


Create a new tree.

 
*/

/** @pred rb_empty(? _T_) 


Succeeds if tree  _T_ is empty.

 
*/

/** @pred is_rbtree(+ _T_) 


Check whether  _T_ is a valid red-black tree.

 
*/

/** @pred rb_insert(+ _T0_,+ _Key_,? _Value_,+ _TF_) 


Add an element with key  _Key_ and  _Value_ to the tree
 _T0_ creating a new red-black tree  _TF_. Duplicated elements are not
allowed.

Add a new element with key  _Key_ and  _Value_ to the tree
 _T0_ creating a new red-black tree  _TF_. Fails is an element
with  _Key_ exists in the tree.

 
*/

/** @pred rb_lookup(+ _Key_,- _Value_,+ _T_) 


Backtrack through all elements with key  _Key_ in the red-black tree
 _T_, returning for each the value  _Value_.

 
*/

/** @pred rb_lookupall(+ _Key_,- _Value_,+ _T_) 


Lookup all elements with key  _Key_ in the red-black tree
 _T_, returning the value  _Value_.

 
*/

/** @pred rb_delete(+ _T_,+ _Key_,- _TN_) 


Delete element with key  _Key_ from the tree  _T_, returning a new
tree  _TN_.

 
*/

/** @pred rb_delete(+ _T_,+ _Key_,- _Val_,- _TN_)

Delete element with key  _Key_ from the tree  _T_, returning the
value  _Val_ associated with the key and a new tree  _TN_.

 
*/

/** @pred rb_del_min(+ _T_,- _Key_,- _Val_,- _TN_) 


Delete the least element from the tree  _T_, returning the key
 _Key_, the value  _Val_ associated with the key and a new tree
 _TN_.

 
*/

/** @pred rb_del_max(+ _T_,- _Key_,- _Val_,- _TN_) 


Delete the largest element from the tree  _T_, returning the key
 _Key_, the value  _Val_ associated with the key and a new tree
 _TN_.

 
*/

/** @pred rb_update(+ _T_,+ _Key_,+ _NewVal_,- _TN_) 


Tree  _TN_ is tree  _T_, but with value for  _Key_ associated
with  _NewVal_. Fails if it cannot find  _Key_ in  _T_.

 
*/

/** @pred rb_apply(+ _T_,+ _Key_,+ _G_,- _TN_) 


If the value associated with key  _Key_ is  _Val0_ in  _T_, and
if `call(G,Val0,ValF)` holds, then  _TN_ differs from
 _T_ only in that  _Key_ is associated with value  _ValF_ in
tree  _TN_. Fails if it cannot find  _Key_ in  _T_, or if
`call(G,Val0,ValF)` is not satisfiable.

 
*/

/** @pred rb_visit(+ _T_,- _Pairs_) 


 _Pairs_ is an infix visit of tree  _T_, where each element of
 _Pairs_ is of the form   _K_- _Val_.

 
*/

/** @pred rb_size(+ _T_,- _Size_) 


 _Size_ is the number of elements in  _T_.

 
*/

/** @pred rb_keys(+ _T_,+ _Keys_) 


 _Keys_ is an infix visit with all keys in tree  _T_. Keys will be
sorted, but may be duplicate.

 
*/

/** @pred rb_map(+ _T_,+ _G_,- _TN_) 


For all nodes  _Key_ in the tree  _T_, if the value associated with
key  _Key_ is  _Val0_ in tree  _T_, and if
`call(G,Val0,ValF)` holds, then the value associated with  _Key_
in  _TN_ is  _ValF_. Fails if or if `call(G,Val0,ValF)` is not
satisfiable for all  _Var0_.

 
*/

/** @pred rb_partial_map(+ _T_,+ _Keys_,+ _G_,- _TN_) 


For all nodes  _Key_ in  _Keys_, if the value associated with key
 _Key_ is  _Val0_ in tree  _T_, and if `call(G,Val0,ValF)`
holds, then the value associated with  _Key_ in  _TN_ is
 _ValF_. Fails if or if `call(G,Val0,ValF)` is not satisfiable
for all  _Var0_. Assumes keys are not repeated.

 
*/

/** @pred rb_fold(+ _T_,+ _G_,+ _Acc0_, - _AccF_) 


For all nodes  _Key_ in the tree  _T_, if the value
associated with key  _Key_ is  _V_ in tree  _T_, if
`call(G,V,Acc1,Acc2)` holds, then if  _VL_ is value of the
previous node in inorder, `call(G,VL,_,Acc0)` must hold, and if
 _VR_ is the value of the next node in inorder,
`call(G,VR,Acc1,_)` must hold.

 
*/

/** @pred rb_key_fold(+ _T_,+ _G_,+ _Acc0_, - _AccF_) 


For all nodes  _Key_ in the tree  _T_, if the value
associated with key  _Key_ is  _V_ in tree  _T_, if
`call(G,Key,V,Acc1,Acc2)` holds, then if  _VL_ is value of the
previous node in inorder, `call(G,KeyL,VL,_,Acc0)` must hold, and if
 _VR_ is the value of the next node in inorder,
`call(G,KeyR,VR,Acc1,_)` must hold.

 
*/

/** @pred rb_clone(+ _T_,+ _NT_,+ _Nodes_) 


``Clone'' the red-back tree into a new tree with the same keys as the
original but with all values set to unbound values. Nodes is a list
containing all new nodes as pairs  _K-V_.

 
*/

/** @pred rb_min(+ _T_,- _Key_,- _Value_) 


 _Key_  is the minimum key in  _T_, and is associated with  _Val_.

 
*/

/** @pred rb_max(+ _T_,- _Key_,- _Value_) 


 _Key_  is the maximal key in  _T_, and is associated with  _Val_.

 
*/

/** @pred rb_next(+ _T_, + _Key_,- _Next_,- _Value_) 


 _Next_ is the next element after  _Key_ in  _T_, and is
associated with  _Val_.

 
*/

/** @pred rb_previous(+ _T_, + _Key_,- _Previous_,- _Value_) 


 _Previous_ is the previous element after  _Key_ in  _T_, and is
associated with  _Val_.

 
*/

/** @pred ord_list_to_rbtree(+ _L_, - _T_) 


 _T_ is the red-black tree corresponding to the mapping in ordered
list  _L_.



@} */

/** @defgroup RegExp Regular Expressions
@ingroup YAPLibrary
@{

This library includes routines to determine whether a regular expression
matches part or all of a string. The routines can also return which
parts parts of the string matched the expression or subexpressions of
it. This library relies on Henry Spencer's `C`-package and is only
available in operating systems that support dynamic loading. The
`C`-code has been obtained from the sources of FreeBSD-4.0 and is
protected by copyright from Henry Spencer and from the Regents of the
University of California (see the file library/regex/COPYRIGHT for
further details).

Much of the description of regular expressions below is copied verbatim
from Henry Spencer's manual page.

A regular expression is zero or more branches, separated by ``|''.  It
matches anything that matches one of the branches.

A branch is zero or more pieces, concatenated.  It matches a match for
the first, followed by a match for the second, etc.

A piece is an atom possibly followed by ``\*'', ``+'', or ``?''.  An atom
followed by ``\*'' matches a sequence of 0 or more matches of the atom.
An atom followed by ``+'' matches a sequence of 1 or more matches of the
atom.  An atom followed by ``?'' matches a match of the atom, or the
null string.

An atom is a regular expression in parentheses (matching a match for the
regular expression), a range (see below), ``.''  (matching any single
character), ``^'' (matching the null string at the beginning of the
input string), ``$'' (matching the null string at the end of the input
string), a ``\\'' followed by a single character (matching that
character), or a single character with no other significance (matching
that character).

A range is a sequence of characters enclosed in ``[]''.  It normally
matches any single character from the sequence.  If the sequence begins
with ``^'', it matches any single character not from the rest of the
sequence.  If two characters in the sequence are separated by ``-'',
this is shorthand for the full list of ASCII characters between them
(e.g. ``[0-9]'' matches any decimal digit).  To include a literal ``]''
in the sequence, make it the first character (following a possible
``^'').  To include a literal ``-'', make it the first or last
character.



 @pred regexp(+ _RegExp_,+ _String_,+ _Opts_) 



Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The options may be:

    + `nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.


 
*/

/** @pred regexp(+ _RegExp_,+ _String_,+ _Opts_,? _SubMatchVars_)


Match regular expression  _RegExp_ to input string  _String_
according to options  _Opts_. The variable  _SubMatchVars_ should
be originally unbound or a list of unbound variables all will contain a
sequence of matches, that is, the head of  _SubMatchVars_ will
contain the characters in  _String_ that matched the leftmost
parenthesized subexpression within  _RegExp_, the next head of list
will contain the characters that matched the next parenthesized
subexpression to the right in  _RegExp_, and so on.

The options may be:

    + `nocase`: Causes upper-case characters  in   _String_ to
be treated  as  lower case during the matching process.
    + `indices`: Changes what  is  stored  in
 _SubMatchVars_. Instead  of storing the matching characters from
 _String_, each variable will contain a term of the form  _IO-IF_
giving the indices in  _String_ of the first and last characters  in
the  matching range of characters.



In general there may be more than one way to match a regular expression
to an input string.  For example,  consider the command

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexp("(a*)b*","aabaaabb", [], [X,Y])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Considering only the rules given so far,  _X_ and  _Y_ could end up
with the values `"aabb"` and `"aa"`, `"aaab"` and
`"aaa"`, `"ab"` and `"a"`, or any of several other
combinations.  To resolve this potential ambiguity `regexp` chooses among
alternatives using the rule ``first then longest''.  In other words, it
considers the possible matches in order working from left to right
across the input string and the pattern, and it attempts to match longer
pieces of the input string before shorter ones.  More specifically, the
following rules apply in decreasing order of priority:

<ol>
    + If a regular expression could match  two  different parts of an
input string then it will match the one that begins earliest.

    + If a regular expression contains "|"  operators  then the leftmost matching sub-expression is chosen.

    + In \*, +, and ? constructs, longer matches are chosen in preference to shorter ones.

    + In sequences of expression  components  the  components are considered from left to right.
</ol>

In the example from above, `"(a\*)b\*"` matches `"aab"`: the
`"(a\*)"` portion of the pattern is matched first and it consumes
the leading `"aa"`; then the `"b\*"` portion of the pattern
consumes the next `"b"`.  Or, consider the following example: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexp("(ab|a)(b*)c",  "abc", [], [X,Y,Z])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After this command  _X_ will be `"abc"`,  _Y_ will be
`"ab"`, and  _Z_ will be an empty string.  Rule 4 specifies that
`"(ab|a)"` gets first shot at the input string and Rule 2 specifies
that the `"ab"` sub-expression is checked before the `"a"`
sub-expression.  Thus the `"b"` has already been claimed before the
`"(b\*)"` component is checked and `(b\*)` must match an empty string.




@} */

/** @defgroup shlib SWI-Prolog's shlib library
@ingroup YAPLibrary
@{

This section discusses the functionality of the (autoload)
`library(shlib)`, providing an interface to manage shared
libraries.

One of the files provides a global function `install_mylib()` that
initialises the module using calls to `PL_register_foreign()`. Here is a
simple example file `mylib.c`, which creates a Windows MessageBox:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include <windows.h>
#include <SWI-Prolog.h>

static foreign_t
pl_say_hello(term_t to)
{ char *a;

  if ( PL_get_atom_chars(to, &a) )
  { MessageBox(NULL, a, "DLL test", MB_OK|MB_TASKMODAL);

    PL_succeed;
  }

  PL_fail;
}

install_t
install_mylib()
{ PL_register_foreign("say_hello", 1, pl_say_hello, 0);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now write a file mylib.pl:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(mylib, [ say_hello/1 ]).
:- use_foreign_library(foreign(mylib)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file mylib.pl can be loaded as a normal Prolog file and provides the predicate defined in C.

 
*/

/** @pred load_foreign_library(: _FileSpec_) is det 


 
*/

/** @pred load_foreign_library(: _FileSpec_, + _Entry_:atom) is det

Load a shared object or DLL. After loading the  _Entry_ function is
called without arguments. The default entry function is composed
from `install_`, followed by the file base-name. E.g., the
load-call below calls the function `install_mylib()`. If the platform
prefixes extern functions with `_`, this prefix is added before
calling.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ...
          load_foreign_library(foreign(mylib)),
          ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 _FileSpec_ is a specification for
absolute_file_name/3. If searching the file fails, the plain
name is passed to the OS to try the default method of the OS for
locating foreign objects. The default definition of
file_search_path/2 searches \<prolog home\>/lib/Yap.

See also
`use_foreign_library/1,2` are intended for use in
directives. 

 
*/

/** @pred [det] use_foreign_library(+ _FileSpec_),  use_foreign_library(+ _FileSpec_, + _Entry_:atom) 



Load and install a foreign library as load_foreign_library/1
and `load_foreign_library/2` and
register the installation using `initialization/2` with the option
now. This is similar to using:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    :- initialization(load_foreign_library(foreign(mylib))).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

but using the initialization/1 wrapper causes the library to
be loaded after loading of the file in which it appears is
completed, while use_foreign_library/1 loads the library
immediately. I.e. the difference is only relevant if the remainder
of the file uses functionality of the `C`-library. 

 
*/

/** @pred [det]unload_foreign_library(+ _FileSpec_)
 
*/

/** @pred [det]unload_foreign_library(+ _FileSpec_, + _Exit_:atom)  




Unload a shared
object or DLL. After calling the  _Exit_ function, the shared object is
removed from the process. The default exit function is composed from
`uninstall_`, followed by the file base-name.

 
*/

/** @pred current_foreign_library(? _File_, ? _Public_)  



Query currently
loaded shared libraries.  




@} */

/** @defgroup Splay_Trees Splay Trees
@ingroup YAPLibrary
@{

Splay trees are explained in the paper "Self-adjusting Binary Search
Trees", by D.D. Sleator and R.E. Tarjan, JACM, vol. 32, No.3, July 1985,
p. 668. They are designed to support fast insertions, deletions and
removals in binary search trees without the complexity of traditional
balanced trees. The key idea is to allow the tree to become
unbalanced. To make up for this, whenever we find a node, we move it up
to the top. We use code by Vijay Saraswat originally posted to the Prolog
mailing-list.



 @pred splay_access(- _Return_,+ _Key_,? _Val_,+ _Tree_,- _NewTree_) 


If item  _Key_ is in tree  _Tree_, return its  _Val_ and
unify  _Return_ with `true`. Otherwise unify  _Return_ with
`null`. The variable  _NewTree_ unifies with the new tree.

 
*/

/** @pred splay_delete(+ _Key_,? _Val_,+ _Tree_,- _NewTree_) 


Delete item  _Key_ from tree  _Tree_, assuming that it is present
already. The variable  _Val_ unifies with a value for key  _Key_,
and the variable  _NewTree_ unifies with the new tree. The predicate
will fail if  _Key_ is not present.

 
*/

/** @pred splay_init(- _NewTree_) 


Initialize a new splay tree.

 
*/

/** @pred splay_insert(+ _Key_,? _Val_,+ _Tree_,- _NewTree_) 


Insert item  _Key_ in tree  _Tree_, assuming that it is not
there already. The variable  _Val_ unifies with a value for key
 _Key_, and the variable  _NewTree_ unifies with the new
tree. In our implementation,  _Key_ is not inserted if it is
already there: rather it is unified with the item already in the tree.

 
*/

/** @pred splay_join(+ _LeftTree_,+ _RighTree_,- _NewTree_) 


Combine trees  _LeftTree_ and  _RighTree_ into a single
tree _NewTree_ containing all items from both trees. This operation
assumes that all items in  _LeftTree_ are less than all those in
 _RighTree_ and destroys both  _LeftTree_ and  _RighTree_.

 
*/

/** @pred splay_split(+ _Key_,? _Val_,+ _Tree_,- _LeftTree_,- _RightTree_) 


Construct and return two trees  _LeftTree_ and  _RightTree_,
where  _LeftTree_ contains all items in  _Tree_ less than
 _Key_, and  _RightTree_ contains all items in  _Tree_
greater than  _Key_. This operations destroys  _Tree_.




@} */

/** @defgroup String_InputOutput Reading From and Writing To Strings
@ingroup YAPLibrary
@{

From Version 4.3.2 onwards YAP implements SICStus Prolog compatible
String Input/Output. The library allows users to read from and write to a memory
buffer as if it was a file. The memory buffer is built from or converted
to a string of character codes by the routines in library. Therefore, if
one wants to read from a string the string must be fully instantiated
before the library built-in opens the string for reading. These commands
are available through the `use_module(library(charsio))` command.



 @pred format_to_chars(+ _Form_, + _Args_, - _Result_) 



Execute the built-in procedure format/2 with form  _Form_ and
arguments  _Args_ outputting the result to the string of character
codes  _Result_.

 
*/

/** @pred format_to_chars(+ _Form_, + _Args_, - _Result_, - _Result0_)


Execute the built-in procedure format/2 with form  _Form_ and
arguments  _Args_ outputting the result to the difference list of
character codes  _Result-Result0_.

 
*/

/** @pred write_to_chars(+ _Term_, - _Result_) 



Execute the built-in procedure write/1 with argument  _Term_
outputting the result to the string of character codes  _Result_.

 
*/

/** @pred write_to_chars(+ _Term_, - _Result0_, - _Result_)


Execute the built-in procedure write/1 with argument  _Term_
outputting the result to the difference list of character codes
 _Result-Result0_.

 
*/

/** @pred atom_to_chars(+ _Atom_, - _Result_) 



Convert the atom  _Atom_ to the string of character codes
 _Result_.

 
*/

/** @pred atom_to_chars(+ _Atom_, - _Result0_, - _Result_)


Convert the atom  _Atom_ to the difference list of character codes
 _Result-Result0_.

 
*/

/** @pred number_to_chars(+ _Number_, - _Result_) 



Convert the number  _Number_ to the string of character codes
 _Result_.

 
*/

/** @pred number_to_chars(+ _Number_, - _Result0_, - _Result_)


Convert the atom  _Number_ to the difference list of character codes
 _Result-Result0_.

 
*/

/** @pred atom_to_term(+ _Atom_, - _Term_, - _Bindings_) 


Use  _Atom_ as input to read_term/2 using the option `variable_names` and return the read term in  _Term_ and the variable bindings in  _Bindings_.  _Bindings_ is a list of `Name = Var` couples, thus providing access to the actual variable names. See also read_term/2. If Atom has no valid syntax, a syntax_error exception is raised.

 
*/

/** @pred term_to_atom(? _Term_, ? _Atom_) 


True if  _Atom_ describes a term that unifies with  _Term_. When
 _Atom_ is instantiated  _Atom_ is converted and then unified with
 _Term_. If  _Atom_ has no valid syntax, a syntax_error exception
is raised. Otherwise  _Term_ is ``written'' on  _Atom_ using
write_term/2 with the option quoted(true).

 
*/

/** @pred read_from_chars(+ _Chars_, - _Term_) 



Parse the list of character codes  _Chars_ and return the result in
the term  _Term_. The character codes to be read must terminate with
a dot character such that either (i) the dot character is followed by
blank characters; or (ii) the dot character is the last character in the
string.

 
*/

/** @pred open_chars_stream(+ _Chars_, - _Stream_) 



Open the list of character codes  _Chars_ as a stream  _Stream_.

 
*/

/** @pred with_output_to_chars(? _Goal_, - _Chars_) 



Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the list of character codes  _Chars_.

 
*/

/** @pred with_output_to_chars(? _Goal_, ? _Chars0_, - _Chars_)


Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_.

 
*/

/** @pred with_output_to_chars(? _Goal_, - _Stream_, ? _Chars0_, - _Chars_)


Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_ and  _Stream_ receives the stream corresponding to
the memory buffer.



The implementation of the character IO operations relies on three YAP
built-ins:



 @pred charsio:open_mem_read_stream(+ _String_, - _Stream_)
Store a string in a memory buffer and output a stream that reads from this
memory buffer.

 
*/

/** @pred charsio:open_mem_write_stream(- _Stream_)
Create a new memory buffer and output a stream that writes to  it.

 
*/

/** @pred charsio:peek_mem_write_stream(- _Stream_, L0, L)
Convert the memory buffer associated with stream  _Stream_ to the
difference list of character codes  _L-L0_.


These built-ins are initialized to belong to the module `charsio` in
`init.yap`. Novel procedures for manipulating strings by explicitly
importing these built-ins.

YAP does not currently support opening a `charsio` stream in
`append` mode, or seeking in such a stream.


@} */

/** @defgroup System Calling The Operating System from YAP
@ingroup YAPLibrary
@{

YAP now provides a library of system utilities compatible with the
SICStus Prolog system library. This library extends and to some point
replaces the functionality of Operating System access routines. The
library includes Unix/Linux and Win32 `C` code. They
are available through the `use_module(library(system))` command.



 @pred datime(datime(- _Year_, - _Month_, - _DayOfTheMonth_, 

- _Hour_, - _Minute_, - _Second_)

The datime/1 procedure returns the current date and time, with
information on  _Year_,  _Month_,  _DayOfTheMonth_,
 _Hour_,  _Minute_, and  _Second_. The  _Hour_ is returned
on local time. This function uses the WIN32
`GetLocalTime` function or the Unix `localtime` function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- datime(X).

X = datime(2001,5,28,15,29,46) ? 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred mktime(datime(+ _Year_, + _Month_, + _DayOfTheMonth_, 

+ _Hour_, + _Minute_, + _Second_), - _Seconds_)

The `mktime/1` procedure returns the number of  _Seconds_
elapsed since 00:00:00 on January 1, 1970, Coordinated Universal Time
(UTC).  The user provides information on  _Year_,  _Month_,
 _DayOfTheMonth_,  _Hour_,  _Minute_, and  _Second_. The
 _Hour_ is given on local time. This function uses the WIN32
`GetLocalTime` function or the Unix `mktime` function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- mktime(datime(2001,5,28,15,29,46),X).

X = 991081786 ? ;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred delete_file(+ _File_) 


The delete_file/1 procedure removes file  _File_. If
 _File_ is a directory, remove the directory <em>and all its subdirectories</em>.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- delete_file(x).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred delete_file(+ _File_,+ _Opts_)

The `delete_file/2` procedure removes file  _File_ according to
options  _Opts_. These options are `directory` if one should
remove directories, `recursive` if one should remove directories
recursively, and `ignore` if errors are not to be reported.

This example is equivalent to using the delete_file/1 predicate:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- delete_file(x, [recursive]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred directory_files(+ _Dir_,+ _List_) 


Given a directory  _Dir_,  directory_files/2 procedures a
listing of all files and directories in the directory:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ?- directory_files('.',L), writeq(L).
['Makefile.~1~','sys.so','Makefile','sys.o',x,..,'.']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The predicates uses the `dirent` family of routines in Unix
environments, and `findfirst` in WIN32.

 
*/

/** @pred file_exists(+ _File_) 


The atom  _File_ corresponds to an existing file.

 
*/

/** @pred file_exists(+ _File_,+ _Permissions_)

The atom  _File_ corresponds to an existing file with permissions
compatible with  _Permissions_. YAP currently only accepts for
permissions to be described as a number. The actual meaning of this
number is Operating System dependent.

 
*/

/** @pred file_property(+ _File_,? _Property_) 


The atom  _File_ corresponds to an existing file, and  _Property_
will be unified with a property of this file. The properties are of the
form `type( _Type_)`, which gives whether the file is a regular
file, a directory, a fifo file, or of unknown type;
`size( _Size_)`, with gives the size for a file, and
`mod_time( _Time_)`, which gives the last time a file was
modified according to some Operating System dependent
timestamp; `mode( _mode_)`, gives the permission flags for the
file, and `linkto( _FileName_)`, gives the file pointed to by a
symbolic link. Properties can be obtained through backtracking:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- file_property('Makefile',P).

P = type(regular) ? ;

P = size(2375) ? ;

P = mod_time(990826911) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred make_directory(+ _Dir_) 


Create a directory  _Dir_. The name of the directory must be an atom.

 
*/

/** @pred rename_file(+ _OldFile_,+ _NewFile_) 


Create file  _OldFile_ to  _NewFile_. This predicate uses the
`C` built-in function `rename`.

 
*/

/** @pred environ(? _EnvVar_,+ _EnvValue_) 


Unify environment variable  _EnvVar_ with its value  _EnvValue_,
if there is one. This predicate is backtrackable in Unix systems, but
not currently in Win32 configurations.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ?- environ('HOME',X).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred host_id(- _Id_) 



Unify  _Id_ with an identifier of the current host. YAP uses the
`hostid` function when available, 

 
*/

/** @pred host_name(- _Name_) 



Unify  _Name_ with a name for the current host. YAP uses the
`hostname` function in Unix systems when available, and the
`GetComputerName` function in WIN32 systems. 

 
*/

/** @pred kill( _Id_,+ _SIGNAL_) 



Send signal  _SIGNAL_ to process  _Id_. In Unix this predicate is
a direct interface to `kill` so one can send signals to groups of
processes. In WIN32 the predicate is an interface to
`TerminateProcess`, so it kills  _Id_ independently of  _SIGNAL_.

 
*/

/** @pred mktemp( _Spec_,- _File_) 



Direct interface to `mktemp`: given a  _Spec_, that is a file
name with six  _X_ to it, create a file name  _File_. Use
tmpnam/1 instead.

 
*/

/** @pred pid(- _Id_) 



Unify  _Id_ with the process identifier for the current
process. An interface to the <tt>getpid</tt> function.

 
*/

/** @pred tmpnam(- _File_) 



Interface with  _tmpnam_: obtain a new, unique file name  _File_.

 
*/

/** @pred tmp_file(- _File_) 



Create a name for a temporary file.  _Base_ is an user provided
identifier for the category of file. The  _TmpName_ is guaranteed to
be unique. If the system halts, it will automatically remove all created
temporary files.

 
*/

/** @pred exec(+ _Command_,[+ _InputStream_,+ _OutputStream_,+ _ErrorStream_],- _PID_) 


Execute command  _Command_ with its streams connected to
 _InputStream_,  _OutputStream_, and  _ErrorStream_. The
process that executes the command is returned as  _PID_. The
command is executed by the default shell `bin/sh -c` in Unix.

The following example demonstrates the use of exec/3 to send a
command and process its output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exec(ls,[std,pipe(S),null],P),repeat, get0(S,C), (C = -1, close(S) ! ; put(C)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The streams may be one of standard stream, `std`, null stream,
`null`, or `pipe(S)`, where  _S_ is a pipe stream. Note
that it is up to the user to close the pipe.

 
*/

/** @pred popen(+ _Command_, + _TYPE_, - _Stream_) 


Interface to the <tt>popen</tt> function. It opens a process by creating a
pipe, forking and invoking  _Command_ on the current shell. Since a
pipe is by definition unidirectional the  _Type_ argument may be
`read` or `write`, not both. The stream should be closed
using close/1, there is no need for a special `pclose`
command.

The following example demonstrates the use of popen/3 to process
the output of a command, as exec/3 would do:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
   ?- popen(ls,read,X),repeat, get0(X,C), (C = -1, ! ; put(C)).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The WIN32 implementation of popen/3 relies on exec/3.

 
*/

/** @pred shell 


Start a new shell and leave YAP in background until the shell
completes. YAP uses the shell given by the environment variable
`SHELL`. In WIN32 environment YAP will use `COMSPEC` if
`SHELL` is undefined.

 
*/

/** @pred shell(+ _Command_)

Execute command  _Command_ under a new shell. YAP will be in
background until the command completes. In Unix environments YAP uses
the shell given by the environment variable `SHELL` with the option
`" -c "`. In WIN32 environment YAP will use `COMSPEC` if
`SHELL` is undefined, in this case with the option `" /c "`.

 
*/

/** @pred shell(+ _Command_,- _Status_)

Execute command  _Command_ under a new shell and unify  _Status_
with the exit for the command. YAP will be in background until the
command completes. In Unix environments YAP uses the shell given by the
environment variable `SHELL` with the option `" -c "`. In
WIN32 environment YAP will use `COMSPEC` if `SHELL` is
undefined, in this case with the option `" /c "`.

 
*/

/** @pred sleep(+ _Time_) 


Block the current thread for  _Time_ seconds. When YAP is compiled 
without multi-threading support, this predicate blocks the YAP process. 
The number of seconds must be a positive number, and it may an integer 
or a float. The Unix implementation uses `usleep` if the number of 
seconds is below one, and `sleep` if it is over a second. The WIN32 
implementation uses `Sleep` for both cases.

 
*/

/** @pred system

Start a new default shell and leave YAP in background until the shell
completes. YAP uses `/bin/sh` in Unix systems and `COMSPEC` in
WIN32.

 
*/

/** @pred system(+ _Command_,- _Res_)

Interface to `system`: execute command  _Command_ and unify
 _Res_ with the result.

 
*/

/** @pred wait(+ _PID_,- _Status_) 


Wait until process  _PID_ terminates, and return its exits  _Status_.




@} */

/** @defgroup Terms Utilities On Terms
@ingroup YAPLibrary
@{

The next routines provide a set of commonly used utilities to manipulate
terms. Most of these utilities have been implemented in `C` for
efficiency. They are available through the
`use_module(library(terms))` command.



 @pred cyclic_term(? _Term_) 


Succeed if the argument  _Term_ is not a cyclic term.

 
*/

/** @pred term_hash(+ _Term_, ? _Hash_) 



If  _Term_ is ground unify  _Hash_ with a positive integer
calculated from the structure of the term. Otherwise the argument
 _Hash_ is left unbound. The range of the positive integer is from
`0` to, but not including, `33554432`.

 
*/

/** @pred term_hash(+ _Term_, + _Depth_, + _Range_, ? _Hash_)


Unify  _Hash_ with a positive integer calculated from the structure
of the term.  The range of the positive integer is from `0` to, but
not including,  _Range_. If  _Depth_ is `-1` the whole term
is considered. Otherwise, the term is considered only up to depth
`1`, where the constants and the principal functor have depth
`1`, and an argument of a term with depth  _I_ has depth  _I+1_. 

 
*/

/** @pred variables_within_term(+ _Variables_,? _Term_, - _OutputVariables_) 



Unify  _OutputVariables_ with the subset of the variables  _Variables_ that occurs in  _Term_.

 
*/

/** @pred new_variables_in_term(+ _Variables_,? _Term_, - _OutputVariables_) 



Unify  _OutputVariables_ with all variables occurring in  _Term_ that are not in the list  _Variables_.

 
*/

/** @pred variant(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ and  _Term2_ are variant terms.

 
*/

/** @pred subsumes(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ subsumes  _Term2_.  Variables in term
 _Term1_ are bound so that the two terms become equal.

 
*/

/** @pred subsumes_chk(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ subsumes  _Term2_ but does not bind any
variable in  _Term1_.

 
*/

/** @pred variable_in_term(? _Term_,? _Var_) 


Succeed if the second argument  _Var_ is a variable and occurs in
term  _Term_.

 
*/

/** @pred unifiable(? _Term1_, ? _Term2_, - _Bindings_) 



Succeed if  _Term1_ and  _Term2_ are unifiable with substitution
 _Bindings_.




@} */

/** @defgroup Tries Trie DataStructure
@ingroup YAPLibrary
@{

The next routines provide a set of utilities to create and manipulate
prefix trees of Prolog terms. Tries were originally proposed to
implement tabling in Logic Programming, but can be used for other
purposes. The tries will be stored in the Prolog database and can seen
as alternative to `assert` and `record` family of
primitives. Most of these utilities have been implemented in `C`
for efficiency. They are available through the
`use_module(library(tries))` command.

 
*/

/** @pred trie_open(- _Id_) 



Open a new trie with identifier  _Id_.

 
*/

/** @pred trie_close(+ _Id_) 



Close trie with identifier  _Id_.

 
*/

/** @pred trie_close_all 



Close all available tries.

 
*/

/** @pred trie_mode(? _Mode_) 



Unify  _Mode_ with trie operation mode. Allowed values are either
`std` (default) or `rev`.

 
*/

/** @pred trie_put_entry(+ _Trie_,+ _Term_,- _Ref_) 



Add term  _Term_ to trie  _Trie_. The handle  _Ref_ gives
a reference to the term.

 
*/

/** @pred trie_check_entry(+ _Trie_,+ _Term_,- _Ref_) 



Succeeds if a variant of term  _Term_ is in trie  _Trie_. An handle
 _Ref_ gives a reference to the term.

 
*/

/** @pred trie_get_entry(+ _Ref_,- _Term_) 


Unify  _Term_ with the entry for handle  _Ref_.

 
*/

/** @pred trie_remove_entry(+ _Ref_) 



Remove entry for handle  _Ref_.

 
*/

/** @pred trie_remove_subtree(+ _Ref_) 



Remove subtree rooted at handle  _Ref_.

 
*/

/** @pred trie_save(+ _Trie_,+ _FileName_) 


Dump trie  _Trie_ into file  _FileName_.

 
*/

/** @pred trie_load(+ _Trie_,+ _FileName_) 


Load trie  _Trie_ from the contents of file  _FileName_.

 
*/

/** @pred trie_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give generic statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/

/** @pred trie_max_stats(- _Memory_,- _Tries_,- _Entries_,- _Nodes_) 


Give maximal statistics on tries, including the amount of memory,
 _Memory_, the number of tries,  _Tries_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_.

 
*/

/** @pred trie_usage(+ _Trie_,- _Entries_,- _Nodes_,- _VirtualNodes_) 


Give statistics on trie  _Trie_, the number of entries,
 _Entries_, and the total number of nodes,  _Nodes_, and the
number of  _VirtualNodes_.

 
*/

/** @pred trie_print(+ _Trie_) 


Print trie  _Trie_ on standard output.




@} */

/** @defgroup Cleanup Call Cleanup
@ingroup YAPLibrary
@{

<tt>call_cleanup/1</tt> and <tt>call_cleanup/2</tt> allow predicates to register
code for execution after the call is finished. Predicates can be
declared to be <tt>fragile</tt> to ensure that <tt>call_cleanup</tt> is called
for any Goal which needs it. This library is loaded with the
`use_module(library(cleanup))` command.

 
*/

/** @pred :- fragile  _P_,...., _Pn_ 


Declares the predicate  _P_=<tt>[module:]name/arity</tt> as a fragile
predicate, module is optional, default is the current
typein_module. Whenever such a fragile predicate is used in a query
it will be called through call_cleanup/1.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- fragile foo/1,bar:baz/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred call_cleanup(: _Goal_) 


Execute goal  _Goal_ within a cleanup-context. Called predicates
might register cleanup Goals which are called right after the end of
the call to  _Goal_. Cuts and exceptions inside Goal do not prevent the
execution of the cleanup calls. <tt>call_cleanup</tt> might be nested.

 
*/

/** @pred call_cleanup(: _Goal_, : _CleanUpGoal_)

This is similar to <tt>call_cleanup/1</tt> with an additional
 _CleanUpGoal_ which gets called after  _Goal_ is finished.

 
*/

/** @pred setup_call_cleanup(: _Setup_,: _Goal_, : _CleanUpGoal_) 


Calls `(Setup, Goal)`. For each sucessful execution of  _Setup_, calling  _Goal_, the
cleanup handler  _Cleanup_ is guaranteed to be called exactly once.
This will happen after  _Goal_ completes, either through failure,
deterministic success, commit, or an exception.   _Setup_ will
contain the goals that need to be protected from asynchronous interrupts
such as the ones received from `call_with_time_limit/2` or thread_signal/2.  In
most uses,  _Setup_ will perform temporary side-effects required by
 _Goal_ that are finally undone by  _Cleanup_.

Success or failure of  _Cleanup_ is ignored and choice-points it
created are destroyed (as once/1). If  _Cleanup_ throws an exception,
this is executed as normal.

Typically, this predicate is used to cleanup permanent data storage
required to execute  _Goal_, close file-descriptors, etc. The example
below provides a non-deterministic search for a term in a file, closing
the stream as needed.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
term_in_file(Term, File) :-
    setup_call_cleanup(open(File, read, In),
               term_in_stream(Term, In),
               close(In) ).

term_in_stream(Term, In) :-
    repeat,
    read(In, T),
    (   T == end_of_file
    ->  !, fail
    ;   T = Term
    ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that it is impossible to implement this predicate in Prolog other than
by reading all terms into a list, close the file and call member/2.
Without setup_call_cleanup/3 there is no way to gain control if the
choice-point left by `repeat` is removed by a cut or an exception.

`setup_call_cleanup/2` can also be used to test determinism of a goal:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- setup_call_cleanup(true,(X=1;X=2), Det=yes).

X = 1 ;

X = 2,
Det = yes ;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This predicate is under consideration for inclusion into the ISO standard.
For compatibility with other Prolog implementations see `call_cleanup/2`.

 
*/

/** @pred setup_call_catcher_cleanup(: _Setup_,: _Goal_, + _Catcher_,: _CleanUpGoal_) 


Similar to `setup_call_cleanup( _Setup_,  _Goal_,  _Cleanup_)` with
additional information on the reason of calling  _Cleanup_.  Prior
to calling  _Cleanup_,  _Catcher_ unifies with the termination
code.  If this unification fails,  _Cleanup_ is
 *not* called.

 
*/

/** @pred on_cleanup(+ _CleanUpGoal_) 


Any Predicate might registers a  _CleanUpGoal_. The
 _CleanUpGoal_ is put onto the current cleanup context. All such
CleanUpGoals are executed in reverse order of their registration when
the surrounding cleanup-context ends. This call will throw an exception
if a predicate tries to register a  _CleanUpGoal_ outside of any
cleanup-context.

 
*/

/** @pred cleanup_all 


Calls all pending CleanUpGoals and resets the cleanup-system to an
initial state. Should only be used as one of the last calls in the
main program.



There are some private predicates which could be used in special
cases, such as manually setting up cleanup-contexts and registering
CleanUpGoals for other than the current cleanup-context.
Read the Source Luke.


@} */

/** @defgroup Timeout Calls With Timeout
@ingroup YAPLibrary
@{

The <tt>time_out/3</tt> command relies on the <tt>alarm/3</tt> built-in to
implement a call with a maximum time of execution. The command is
available with the `use_module(library(timeout))` command.



 @pred time_out(+ _Goal_, + _Timeout_, - _Result_) 


Execute goal  _Goal_ with time limited  _Timeout_, where
 _Timeout_ is measured in milliseconds. If the goal succeeds, unify
 _Result_ with success. If the timer expires before the goal
terminates, unify  _Result_ with <tt>time_out</tt>.

This command is implemented by activating an alarm at procedure
entry. If the timer expires before the goal completes, the alarm will
throw an exception  _timeout_.

One should note that time_out/3 is not reentrant, that is, a goal
called from `time_out` should never itself call
time_out/3. Moreover, time_out/3 will deactivate any previous
alarms set by alarm/3 and vice-versa, hence only one of these
calls should be used in a program.

Last, even though the timer is set in milliseconds, the current
implementation relies on <tt>alarm/3</tt>, and therefore can only offer
precision on the scale of seconds.




@} */

/** @defgroup Trees Updatable Binary Trees
@ingroup YAPLibrary
@{

The following queue manipulation routines are available once
included with the `use_module(library(trees))` command.



 @pred get_label(+ _Index_, + _Tree_, ? _Label_) 


Treats the tree as an array of  _N_ elements and returns the
 _Index_-th.

 
*/

/** @pred list_to_tree(+ _List_, - _Tree_) 


Takes a given  _List_ of  _N_ elements and constructs a binary
 _Tree_.

 
*/

/** @pred map_tree(+ _Pred_, + _OldTree_, - _NewTree_) 


Holds when  _OldTree_ and  _NewTree_ are binary trees of the same shape
and `Pred(Old,New)` is true for corresponding elements of the two trees.

 
*/

/** @pred put_label(+ _Index_, + _OldTree_, + _Label_, - _NewTree_) 


constructs a new tree the same shape as the old which moreover has the
same elements except that the  _Index_-th one is  _Label_.

 
*/

/** @pred tree_size(+ _Tree_, - _Size_) 


Calculates the number of elements in the  _Tree_.

 
*/

/** @pred tree_to_list(+ _Tree_, - _List_) 


Is the converse operation to list_to_tree.




@} */

/** @defgroup UGraphs Unweighted Graphs
@ingroup YAPLibrary
@{

The following graph manipulation routines are based in code originally
written by Richard O'Keefe. The code was then extended to be compatible
with the SICStus Prolog ugraphs library. The routines assume directed
graphs, undirected graphs may be implemented by using two edges. Graphs
are represented in one of two ways:

    + The P-representation of a graph is a list of (from-to) vertex
pairs, where the pairs can be in any old order.  This form is
convenient for input/output.

 
*/

/** @pred The S-representation of a graph is a list of (vertex-neighbors)
pairs, where the pairs are in standard order (as produced by keysort)
and the neighbors of each vertex are also in standard order (as
produced by sort).  This form is convenient for many calculations.


These built-ins are available once included with the
`use_module(library(ugraphs))` command.



 @pred vertices_edges_to_ugraph(+ _Vertices_, + _Edges_, - _Graph_) 


Given a graph with a set of vertices  _Vertices_ and a set of edges
 _Edges_,  _Graph_ must unify with the corresponding
s-representation. Note that the vertices without edges will appear in
 _Vertices_ but not in  _Edges_. Moreover, it is sufficient for a
vertex to appear in  _Edges_.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices_edges_to_ugraph([],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[]] ? 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this case all edges are defined implicitly. The next example shows
three unconnected edges:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices_edges_to_ugraph([6,7,8],[1-3,2-4,4-5,1-5],L).

L = [1-[3,5],2-[4],3-[],4-[5],5-[],6-[],7-[],8-[]] ? 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred vertices(+ _Graph_, - _Vertices_) 


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred edges(+ _Graph_, - _Edges_) 


Unify  _Edges_ with all edges appearing in graph
 _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- vertices([1-[3,5],2-[4],3-[],4-[5],5-[]], V).

L = [1,2,3,4,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- add_vertices([1-[3,5],2-[4],3-[],4-[5],
                 5-[],6-[],7-[],8-[]],
                [0,2,9,10,11],
                   NG).

NG = [0-[],1-[3,5],2-[4],3-[],4-[5],5-[],
      6-[],7-[],8-[],9-[],10-[],11-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- del_vertices([2,1],[1-[3,5],2-[4],3-[],
                 4-[5],5-[],6-[],7-[2,6],8-[]],NL).

NL = [3-[],4-[5],5-[],6-[],7-[6],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred add_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- add_edges([1-[3,5],2-[4],3-[],4-[5],5-[],6-[],
              7-[],8-[]],[1-6,2-3,3-2,5-7,3-2,4-5],NL).

NL = [1-[3,5,6],2-[3,4],3-[2],4-[5],5-[7],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred del_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- del_edges([1-[3,5],2-[4],3-[],4-[5],5-[],
              6-[],7-[],8-[]],
             [1-6,2-3,3-2,5-7,3-2,4-5,1-3],NL).

NL = [1-[5],2-[4],3-[],4-[],5-[],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred transpose(+ _Graph_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_. The cost is `O(|V|^2)`. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- transpose([1-[3,5],2-[4],3-[],
              4-[5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[],2-[],3-[1],4-[2],5-[1,4],6-[],7-[],8-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that an undirected graph is its own transpose.

 
*/

/** @pred neighbors(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail. In the next
example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- neighbors(4,[1-[3,5],2-[4],3-[],
                4-[1,2,7,5],5-[],6-[],7-[],8-[]],
             NL).

NL = [1,2,7,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred neighbours(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- neighbours(4,[1-[3,5],2-[4],3-[],
                 4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1,2,7,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred complement(+ _Graph_, - _NewGraph_) 


Unify  _NewGraph_ with the graph complementary to  _Graph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- complement([1-[3,5],2-[4],3-[],
               4-[1,2,7,5],5-[],6-[],7-[],8-[]], NL).

NL = [1-[2,4,6,7,8],2-[1,3,5,6,7,8],3-[1,2,4,5,6,7,8],
      4-[3,5,6,8],5-[1,2,3,4,6,7,8],6-[1,2,3,4,5,7,8],
      7-[1,2,3,4,5,6,8],8-[1,2,3,4,5,6,7]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred compose(+ _LeftGraph_, + _RightGraph_, - _NewGraph_) 


Compose the graphs  _LeftGraph_ and  _RightGraph_ to form  _NewGraph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- compose([1-[2],2-[3]],[2-[4],3-[1,2,4]],L).

L = [1-[4],2-[1,2,4],3-[]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred top_sort(+ _Graph_, - _Sort_) 


Generate the set of nodes  _Sort_ as a topological sorting of graph
 _Graph_, if one is possible.
In the next example we show how topological sorting works for a linear graph:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- top_sort([_138-[_219],_219-[_139], _139-[]],L).

L = [_138,_219,_139]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred top_sort(+ _Graph_, - _Sort0_, - _Sort_)

Generate the difference list  _Sort_- _Sort0_ as a topological
sorting of graph  _Graph_, if one is possible.

 
*/

/** @pred transitive_closure(+ _Graph_, + _Closure_) 


Generate the graph  _Closure_ as the transitive closure of graph
 _Graph_.
In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- transitive_closure([1-[2,3],2-[4,5],4-[6]],L).

L = [1-[2,3,4,5,6],2-[4,5,6],4-[6]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred reachable(+ _Node_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the set of all vertices in graph
 _Graph_ that are reachable from  _Node_. In the next example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- reachable(1,[1-[3,5],2-[4],3-[],4-[5],5-[]],V).

V = [1,3,5]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup DGraphs Directed Graphs
@ingroup YAPLibrary
@{

The following graph manipulation routines use the red-black tree library
to try to avoid linear-time scans of the graph for all graph
operations. Graphs are represented as a red-black tree, where the key is
the vertex, and the associated value is a list of vertices reachable
from that vertex through an edge (ie, a list of edges). 



 @pred dgraph_new(+ _Graph_) 


Create a new directed graph. This operation must be performed before
trying to use the graph.

 
*/

/** @pred dgraph_vertices(+ _Graph_, - _Vertices_) 


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

 
*/

/** @pred dgraph_edge(+ _N1_, + _N2_, + _Graph_) 


Edge  _N1_- _N2_ is an edge in directed graph  _Graph_.

 
*/

/** @pred dgraph_edges(+ _Graph_, - _Edges_) 


Unify  _Edges_ with all edges appearing in graph
 _Graph_.

 
*/

/** @pred dgraph_add_vertices(+ _Graph_, + _Vertex_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding
vertex  _Vertex_ to the graph  _Graph_.

 
*/

/** @pred dgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.

 
*/

/** @pred dgraph_del_vertex(+ _Graph_, + _Vertex_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by deleting vertex
 _Vertex_ and all the edges that start from or go to  _Vertex_ to
the graph  _Graph_.

 
*/

/** @pred dgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.

 
*/

/** @pred dgraph_add_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the edge
 _N1_- _N2_ to the graph  _Graph_.

 
*/

/** @pred dgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.

 
*/

/** @pred dgraph_del_edge(+ _Graph_, + _N1_, + _N2_, - _NewGraph_) 


Succeeds if  _NewGraph_ unifies with a new graph obtained by
removing the edge  _N1_- _N2_ from the graph  _Graph_. Notice
that no vertices are deleted.

 
*/

/** @pred dgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.

 
*/

/** @pred dgraph_to_ugraph(+ _Graph_, - _UGraph_) 


Unify  _UGraph_ with the representation used by the  _ugraphs_
unweighted graphs library, that is, a list of the form
 _V-Neighbors_, where  _V_ is a node and  _Neighbors_ the nodes
children.

 
*/

/** @pred ugraph_to_dgraph( + _UGraph_, - _Graph_) 


Unify  _Graph_ with the directed graph obtain from  _UGraph_,
represented in the form used in the  _ugraphs_ unweighted graphs
library.

 
*/

/** @pred dgraph_neighbors(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail.

 
*/

/** @pred dgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.

 
*/

/** @pred dgraph_complement(+ _Graph_, - _NewGraph_) 


Unify  _NewGraph_ with the graph complementary to  _Graph_.

 
*/

/** @pred dgraph_transpose(+ _Graph_, - _Transpose_) 


Unify  _NewGraph_ with a new graph obtained from  _Graph_ by
replacing all edges of the form  _V1-V2_ by edges of the form
 _V2-V1_. 

 
*/

/** @pred dgraph_compose(+ _Graph1_, + _Graph2_, - _ComposedGraph_) 


Unify  _ComposedGraph_ with a new graph obtained by composing
 _Graph1_ and  _Graph2_, ie,  _ComposedGraph_ has an edge
 _V1-V2_ iff there is a  _V_ such that  _V1-V_ in  _Graph1_
and  _V-V2_ in  _Graph2_.

 
*/

/** @pred dgraph_transitive_closure(+ _Graph_, - _Closure_) 


Unify  _Closure_ with the transitive closure of graph  _Graph_.

 
*/

/** @pred dgraph_symmetric_closure(+ _Graph_, - _Closure_) 


Unify  _Closure_ with the symmetric closure of graph  _Graph_,
that is,  if  _Closure_ contains an edge  _U-V_ it must also
contain the edge  _V-U_.

 
*/

/** @pred dgraph_top_sort(+ _Graph_, - _Vertices_) 


Unify  _Vertices_ with the topological sort of graph  _Graph_.

 
*/

/** @pred dgraph_top_sort(+ _Graph_, - _Vertices_, ? _Vertices0_)

Unify the difference list  _Vertices_- _Vertices0_ with the
topological sort of graph  _Graph_.

 
*/

/** @pred dgraph_min_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_) 


Unify the list  _Path_ with the minimal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.

 
*/

/** @pred dgraph_max_path(+ _V1_, + _V1_, + _Graph_, - _Path_, ? _Costt_) 


Unify the list  _Path_ with the maximal cost path between nodes
 _N1_ and  _N2_ in graph  _Graph_. Path  _Path_ has cost
 _Cost_.

 
*/

/** @pred dgraph_min_paths(+ _V1_, + _Graph_, - _Paths_) 


Unify the list  _Paths_ with the minimal cost paths from node
 _N1_ to the nodes in graph  _Graph_.

 
*/

/** @pred dgraph_isomorphic(+ _Vs_, + _NewVs_, + _G0_, - _GF_) 


Unify the list  _GF_ with the graph isomorphic to  _G0_ where 
vertices in  _Vs_ map to vertices in  _NewVs_.

 
*/

/** @pred dgraph_path(+ _Vertex_, + _Graph_, ? _Path_) 


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.

 
*/

/** @pred dgraph_path(+ _Vertex_, + _Vertex1_, + _Graph_, ? _Path_)

The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_ and ending at path  _Vertex2_.

 
*/

/** @pred dgraph_reachable(+ _Vertex_, + _Graph_, ? _Edges_) 


The path  _Path_ is a path starting at vertex  _Vertex_ in graph
 _Graph_.

 
*/

/** @pred dgraph_leaves(+ _Graph_, ? _Vertices_) 


The vertices  _Vertices_ have no outgoing edge in graph
 _Graph_.




@} */

/** @defgroup UnDGraphs Undirected Graphs
@ingroup YAPLibrary
@{

The following graph manipulation routines use the red-black tree graph
library to implement undirected graphs. Mostly, this is done by having
two directed edges per undirected edge.



 @pred undgraph_new(+ _Graph_) 


Create a new directed graph. This operation must be performed before
trying to use the graph.

 
*/

/** @pred undgraph_vertices(+ _Graph_, - _Vertices_) 


Unify  _Vertices_ with all vertices appearing in graph
 _Graph_.

 
*/

/** @pred undgraph_edge(+ _N1_, + _N2_, + _Graph_) 


Edge  _N1_- _N2_ is an edge in undirected graph  _Graph_.

 
*/

/** @pred undgraph_edges(+ _Graph_, - _Edges_) 


Unify  _Edges_ with all edges appearing in graph
 _Graph_.

 
*/

/** @pred undgraph_add_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
vertices  _Vertices_ to the graph  _Graph_.

 
*/

/** @pred undgraph_del_vertices(+ _Graph_, + _Vertices_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by deleting the list of
vertices  _Vertices_ and all the edges that start from or go to a
vertex in  _Vertices_ to the graph  _Graph_.

 
*/

/** @pred undgraph_add_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by adding the list of
edges  _Edges_ to the graph  _Graph_.

 
*/

/** @pred undgraph_del_edges(+ _Graph_, + _Edges_, - _NewGraph_) 


Unify  _NewGraph_ with a new graph obtained by removing the list of
edges  _Edges_ from the graph  _Graph_. Notice that no vertices
are deleted.

 
*/

/** @pred undgraph_neighbors(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbors of vertex  _Vertex_
in  _Graph_. If the vertice is not in the graph fail.

 
*/

/** @pred undgraph_neighbours(+ _Vertex_, + _Graph_, - _Vertices_) 


Unify  _Vertices_ with the list of neighbours of vertex  _Vertex_
in  _Graph_.

 
*/

/** @pred undgraph_complement(+ _Graph_, - _NewGraph_) 


Unify  _NewGraph_ with the graph complementary to  _Graph_.

 
*/

/** @pred dgraph_to_undgraph( + _DGraph_, - _UndGraph_) 


Unify  _UndGraph_ with the undirected graph obtained from the
directed graph  _DGraph_.




@} */

/** @defgroup DBUsage Memory Usage in Prolog Data-Base
@ingroup YAPLibrary
@{

This library provides a set of utilities for studying memory usage in YAP.
The following routines are available once included with the
`use_module(library(dbusage))` command.

 
*/

/** @pred db_usage 


Give general overview of data-base usage in the system.

 
*/

/** @pred db_static 


List memory usage for every static predicate.

 
*/

/** @pred db_static(+ _Threshold_)

List memory usage for every static predicate. Predicate must use more
than  _Threshold_ bytes.

 
*/

/** @pred db_dynamic 


List memory usage for every dynamic predicate.

 
*/

/** @pred db_dynamic(+ _Threshold_)

List memory usage for every dynamic predicate. Predicate must use more
than  _Threshold_ bytes.




@} */

/** @defgroup Lambda Lambda Expressions
@ingroup YAPLibrary
@{

This library, designed and implemented by Ulrich Neumerkel, provides
lambda expressions to simplify higher order programming based on `call/N`.

Lambda expressions are represented by ordinary Prolog terms.  There are
two kinds of lambda expressions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
    Free+\X1^X2^ ..^XN^Goal

         \X1^X2^ ..^XN^Goal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The second is a shorthand for` t+\\X1^X2^..^XN^Goal`, where `Xi` are the parameters.

 _Goal_ is a goal or continuation (Syntax note:  _Operators_ within  _Goal_
require parentheses due to the low precedence of the `^` operator).

Free contains variables that are valid outside the scope of the lambda
expression. They are thus free variables within.

All other variables of  _Goal_ are considered local variables. They must
not appear outside the lambda expression. This restriction is
currently not checked. Violations may lead to unexpected bindings.

In the following example the parentheses around `X\>3` are necessary.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- use_module(library(lambda)).
?- use_module(library(apply)).

?- maplist(\X^(X>3),[4,5,9]).
true.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following  _X_ is a variable that is shared by both instances
of the lambda expression. The second query illustrates the cooperation
of continuations and lambdas. The lambda expression is in this case a
continuation expecting a further argument.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- Xs = [A,B], maplist(X+\Y^dif(X,Y), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

?- Xs = [A,B], maplist(X+\dif(X), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following queries are all equivalent. To see this, use
the fact `f(x,y)`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.prolog}
?- call(f,A1,A2).
?- call(\X^f(X),A1,A2).
?- call(\X^Y^f(X,Y), A1,A2).                                                                                                            
?- call(\X^(X+\Y^f(X,Y)), A1,A2).
?- call(call(f, A1),A2).
?- call(f(A1),A2).
?- f(A1,A2).
A1 = x,
A2 = y.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Further discussions
at Ulrich Neumerker's page in <http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord>.


@} */

/** @defgroup LAM LAM
@ingroup YAPPackages
@{

This library provides a set of utilities for interfacing with LAM MPI.
The following routines are available once included with the
`use_module(library(lam_mpi))` command. The yap should be
invoked using the LAM mpiexec or mpirun commands (see LAM manual for
more details).

 
*/

/** @pred mpi_init 


Sets up the mpi environment. This predicate should be called before any other MPI predicate.

 
*/

/** @pred mpi_finalize 


Terminates the MPI execution environment. Every process must call this predicate before  exiting.

 
*/

/** @pred mpi_comm_size(- _Size_) 


Unifies  _Size_ with the number of processes in the MPI environment.

 
*/

/** @pred mpi_comm_rank(- _Rank_) 


Unifies  _Rank_ with the rank of the current process in the MPI environment.

 
*/

/** @pred mpi_version(- _Major_,- _Minor_) 


Unifies  _Major_ and  _Minor_ with, respectively, the major and minor version of the MPI.

 
*/

/** @pred mpi_send(+ _Data_,+ _Dest_,+ _Tag_) 



Blocking communication predicate. The message in  _Data_, with tag
 _Tag_, is sent immediately to the processor with rank  _Dest_.
The predicate succeeds after the message being sent.

 
*/

/** @pred mpi_isend(+ _Data_,+ _Dest_,+ _Tag_,- _Handle_) 



Non blocking communication predicate. The message in  _Data_, with
tag  _Tag_, is sent whenever possible to the processor with rank
 _Dest_. An  _Handle_ to the message is returned to be used to
check for the status of the message, using the `mpi_wait` or
`mpi_test` predicates. Until `mpi_wait` is called, the
memory allocated for the buffer containing the message is not
released.

 
*/

/** @pred mpi_recv(? _Source_,? _Tag_,- _Data_) 



Blocking communication predicate. The predicate blocks until a message
is received from processor with rank  _Source_ and tag  _Tag_.
The message is placed in  _Data_.

 
*/

/** @pred mpi_irecv(? _Source_,? _Tag_,- _Handle_) 



Non-blocking communication predicate. The predicate returns an
 _Handle_ for a message that will be received from processor with
rank  _Source_ and tag  _Tag_. Note that the predicate succeeds
immediately, even if no message has been received. The predicate
`mpi_wait_recv` should be used to obtain the data associated to
the handle.

 
*/

/** @pred mpi_wait_recv(? _Handle_,- _Status_,- _Data_) 



Completes a non-blocking receive operation. The predicate blocks until
a message associated with handle  _Hanlde_ is buffered. The
predicate succeeds unifying  _Status_ with the status of the
message and  _Data_ with the message itself. 

 
*/

/** @pred mpi_test_recv(? _Handle_,- _Status_,- _Data_) 



Provides information regarding a handle. If the message associated
with handle  _Hanlde_ is buffered then the predicate succeeds
unifying  _Status_ with the status of the message and  _Data_
with the message itself. Otherwise, the predicate fails.

 
*/

/** @pred mpi_wait(? _Handle_,- _Status_) 



Completes a non-blocking operation. If the operation was a
`mpi_send`, the predicate blocks until the message is buffered
or sent by the runtime system. At this point the send buffer is
released. If the operation was a `mpi_recv`, it waits until the
message is copied to the receive buffer.  _Status_ is unified with
the status of the message.

 
*/

/** @pred mpi_test(? _Handle_,- _Status_) 



Provides information regarding the handle  _Handle_, ie., if a
communication operation has been completed.  If the operation
associate with  _Hanlde_ has been completed the predicate succeeds
with the completion status in  _Status_, otherwise it fails.

 
*/

/** @pred mpi_barrier 



Collective communication predicate.  Performs a barrier
synchronization among all processes. Note that a collective
communication means that all processes call the same predicate. To be
able to use a regular `mpi_recv` to receive the messages, one
should use `mpi_bcast2`.

 
*/

/** @pred mpi_bcast2(+ _Root_, ? _Data_) 



Broadcasts the message  _Data_ from the process with rank  _Root_
to all other processes.

 
*/

/** @pred mpi_bcast3(+ _Root_, + _Data_, + _Tag_)


Broadcasts the message  _Data_ with tag  _Tag_ from the process with rank  _Root_
to all other processes.

 
*/

/** @pred mpi_ibcast(+ _Root_, + _Data_, + _Tag_) 



Non-blocking operation. Broadcasts the message  _Data_ with tag  _Tag_
from the process with rank  _Root_ to all other processes.

 
*/

/** @pred mpi_default_buffer_size(- _OldBufferSize_, ? _NewBufferSize_) 



The  _OldBufferSize_ argument unifies with the current size of the
MPI communication buffer size and sets the communication buffer size
 _NewBufferSize_. The buffer is used for assynchronous waiting and
for broadcast receivers. Notice that buffer is local at each MPI
process.

 
*/

/** @pred mpi_msg_size( _Msg_, - _MsgSize_) 


Unify  _MsgSize_ with the number of bytes YAP would need to send the
message  _Msg_.

 
*/

/** @pred mpi_gc 



Attempts to perform garbage collection with all the open handles
associated with send and non-blocking broadcasts. For each handle it
tests it and the message has been delivered the handle and the buffer
are released.




@} */

/** @defgroup BDDs Binary Decision Diagrams and Friends
@ingroup YAPPackages
@{

This library provides an interface to the BDD package CUDD. It requires
CUDD compiled as a dynamic library. In Linux this is available out of
box in Fedora, but can easily be ported to other Linux
distributions. CUDD is available in the ports OSX package, and in
cygwin. To use it, call `:-use_module(library(bdd))`.

The following predicates construct a BDD:

 
*/

/** @pred bbd_new(? _Exp_, - _BddHandle_) 

create a new BDD from the logical expression  _Exp_. The expression
may include:

    + Logical Variables:
a leaf-node can be a logical variable.
    + Constants 0 and 1
a leaf-node can also be one of these two constants.
    + or( _X_,  _Y_),  _X_ \\/  _Y_,  _X_ +  _Y_
disjunction
    + and( _X_,  _Y_),  _X_ /\\  _Y_,  _X_ \*  _Y_
conjunction
    + nand( _X_,  _Y_)
negated conjunction@
    + nor( _X_,  _Y_)
negated disjunction
    + xor( _X_,  _Y_)
exclusive or
    + not( _X_), - _X_
negation


 
*/

/** @pred bdd_from_list(? _List_, - _BddHandle_) 

Convert a  _List_ of logical expressions of the form above into a BDD
accessible through  _BddHandle_.

 
*/

/** @pred mtbdd_new(? _Exp_, - _BddHandle_) 

create a new algebraic decision diagram (ADD) from the logical
expression  _Exp_. The expression may include:

    + Logical Variables:
a leaf-node can be a logical variable, or <em>parameter</em>.
    + Number
a leaf-node can also be any number
    + _X_ \*  _Y_
product
    + _X_ +  _Y_
sum
    + _X_ -  _Y_
subtraction
    + or( _X_,  _Y_),  _X_ \\/  _Y_
logical or


 
*/

/** @pred bdd_tree(+ _BDDHandle_,  _Term_) 

Convert the BDD or ADD represented by  _BDDHandle_ to a Prolog term
of the form `bdd( _Dir_,  _Nodes_,  _Vars_)` or `mtbdd( _Nodes_,  _Vars_)`, respectively. The arguments are:

    + 
 _Dir_ direction of the BDD, usually 1
    + 
 _Nodes_ list of nodes in the BDD or ADD. 

In a BDD nodes may be <tt>pp</tt> (both terminals are positive) or <tt>pn</tt>
(right-hand-side is negative), and have four arguments: a logical
variable that will be bound to the value of the node, the logical
variable corresponding to the node, a logical variable, a 0 or a 1 with
the value of the left-hand side, and a logical variable, a 0 or a 1
with the right-hand side.

    + 
 _Vars_ are the free variables in the original BDD, or the parameters of the BDD/ADD.

As an example, the BDD for the expression `X+(Y+X)\*(-Z)` becomes:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdd(1,[pn(N2,X,1,N1),pp(N1,Y,N0,1),pn(N0,Z,1,1)],vs(X,Y,Z))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred bdd_eval(+ _BDDHandle_,  _Val_) 

Unify  _Val_ with the value of the logical expression compiled in
 _BDDHandle_ given an assignment to its  variables.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdd_new(X+(Y+X)*(-Z), BDD), 
[X,Y,Z] = [0,0,0], 
bdd_eval(BDD, V), 
writeln(V).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
would write 0 in the standard output stream.

The  Prolog code equivalent to <tt>bdd_eval/2</tt> is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_bdd, RT, _, V).

eval_bdd(pp(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ R ).
eval_bdd(pn(P,X,L,R), _, P) :-
    P is ( X/\L ) \/ ( (1-X) /\ (1-R) ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First, the nodes are reversed to implement bottom-up evaluation. Then,
we use the `foldl` list manipulation predicate to walk every node,
computing the disjunction of the two cases and binding the output
variable. The top node gives the full expression value. Notice that
`(1- _X_)`  implements negation.

 
*/

/** @pred bdd_size(+ _BDDHandle_, - _Size_) 

Unify  _Size_ with the number of nodes in  _BDDHandle_.

 
*/

/** @pred bdd_print(+ _BDDHandle_, + _File_) 

Output bdd  _BDDHandle_ as a dot file to  _File_.

 
*/

/** @pred bdd_to_probability_sum_product(+ _BDDHandle_, - _Prob_) 

Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_.

 
*/

/** @pred bdd_to_probability_sum_product(+ _BDDHandle_, - _Probs_, - _Prob_)
Each node in a BDD is given a probability  _Pi_. The total
probability of a corresponding sum-product network is  _Prob_, and
the probabilities of the inner nodes are  _Probs_.

In Prolog, this predicate would correspond to computing the value of a
BDD. The input variables will be bound to probabilities, eg
`[ _X_, _Y_, _Z_] = [0.3.0.7,0.1]`, and the previous
`eval_bdd` would operate over real numbers:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Tree = bdd(1, T, _Vs),
    reverse(T, RT),
    foldl(eval_prob, RT, _, V).

eval_prob(pp(P,X,L,R), _, P) :-
    P is  X * L +  (1-X) * R.
eval_prob(pn(P,X,L,R), _, P) :-
    P is  X * L + (1-X) * (1-R).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
*/

/** @pred bdd_close( _BDDHandle_) 

close the BDD and release any resources it holds.




@} */

/** @defgroup Block_Diagram Block Diagram
@ingroup YAPLibrary
@{

This library provides a way of visualizing a prolog program using
modules with blocks.  To use it use:
`:-use_module(library(block_diagram))`.

 
*/

/** @pred make_diagram(+inputfilename, +ouputfilename) 



This will crawl the files following the use_module, ensure_loaded directives withing the inputfilename.
The result will be a file in dot format.
You can make a pdf at the shell by asking `dot -Tpdf filename \> output.pdf`.

 
*/

/** @pred make_diagram(+inputfilename, +ouputfilename, +predicate, +depth, +extension)


The same as make_diagram/2 but you can define how many of the imported/exporeted predicates will be shown with predicate, and how deep the crawler is allowed to go with depth. The extension is used if the file use module directives do not include a file extension.



*/

/** @page SWIhYProlog_Emulation SWI-Prolog Emulation

This library provides a number of SWI-Prolog builtins that are not by
default in YAP. This support is loaded with the
`expects_dialect(swi)` command.


*/

/**  @pred append(? _List1_,? _List2_,? _List3_) 


Succeeds when  _List3_ unifies with the concatenation of  _List1_
and  _List2_. The predicate can be used with any instantiation
pattern (even three variables).

 
*/

/** @pred between(+ _Low_,+ _High_,? _Value_) 



 _Low_ and  _High_ are integers,  _High_ less or equal than
 _Low_. If  _Value_ is an integer,  _Low_ less or equal than
 _Value_ less or equal than  _High_.  When  _Value_ is a
variable it is successively bound to all integers between  _Low_ and
 _High_.  If  _High_ is `inf`, between/3 is true iff
 _Value_ less or equal than  _Low_, a feature that is particularly
interesting for generating integers from a certain value.

 
*/

/** @pred chdir(+ _Dir_) 



Compatibility predicate.  New code should use working_directory/2.

 
*/

/** @pred concat_atom(+ _List_,- _Atom_) 



 _List_ is a list of atoms, integers or floating point numbers. Succeeds
if  _Atom_ can be unified with the concatenated elements of  _List_. If
 _List_ has exactly 2 elements it is equivalent to `atom_concat/3`,
allowing for variables in the list.

 
*/

/** @pred concat_atom(? _List_,+ _Separator_,? _Atom_)


Creates an atom just like concat_atom/2, but inserts  _Separator_
between each pair of atoms.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- concat_atom([gnu, gnat], ', ', A).

A = 'gnu, gnat'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Unimplemented) This predicate can also be used to split atoms by
instantiating  _Separator_ and  _Atom_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- concat_atom(L, -, 'gnu-gnat').

L = [gnu, gnat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred nth1(+ _Index_,? _List_,? _Elem_) 


Succeeds when the  _Index_-th element of  _List_ unifies with
 _Elem_. Counting starts at 1.

Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence expand_file_name/2.

 
*/

/** @pred setenv(+ _Name_,+ _Value_) 


Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence expand_file_name/2.

 
*/

/** @pred term_to_atom(? _Term_,? _Atom_) 


Succeeds if  _Atom_ describes a term that unifies with  _Term_. When
 _Atom_ is instantiated  _Atom_ is converted and then unified with
 _Term_.  If  _Atom_ has no valid syntax, a `syntax_error`
exception is raised. Otherwise  _Term_ is ``written'' on  _Atom_
using write/1.

 
*/

/** @pred working_directory(- _Old_,+ _New_) 



Unify  _Old_ with an absolute path to the current working directory
and change working directory to  _New_.  Use the pattern
`working_directory(CWD, CWD)` to get the current directory.  See
also `absolute_file_name/2` and chdir/1.

 
*/

/** @pred @ _Term1_ =@= @ _Term2_ 



True iff  _Term1_ and  _Term2_ are structurally equivalent. I.e. if  _Term1_ and  _Term2_ are variants of each other.




@} */

/** @defgroup Invoking_Predicates_on_all_Members_of_a_List Invoking Predicates on all Members of a List
@ingroup YAPLibrary
@{


All the predicates in this section call a predicate on all members of a
list or until the predicate called fails.  The predicate is called via
`call/[2..]`, which implies common arguments can be put in
front of the arguments obtained from the list(s). For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- maplist(plus(1), [0, 1, 2], X).

X = [1, 2, 3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we will phrase this as `` _Predicate_ is applied on ...''



 @pred maplist(+ _Pred_,+ _List_) 


 _Pred_ is applied successively on each element of  _List_ until
the end of the list or  _Pred_ fails. In the latter case 
`maplist/2` fails.

 
*/

/** @pred maplist(+ _Pred_,+ _List1_,+ _List2_)

Apply  _Pred_ on all successive pairs of elements from
 _List1_ and
 _List2_. Fails if  _Pred_ can not be applied to a
pair. See the example above.

 
*/

/** @pred maplist(+ _Pred_,+ _List1_,+ _List2_,+ _List4_)

Apply  _Pred_ on all successive triples of elements from  _List1_,
 _List2_ and  _List3_. Fails if  _Pred_ can not be applied to a
triple. See the example above.




@} */

/** @defgroup Forall Forall			
@ingroup YAPPackages
@{


 
*/

/** @pred forall(+ _Cond_,+ _Action_) 




For all alternative bindings of  _Cond_  _Action_ can be proven.
The next example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



*/

/** @page SWIhYProlog_Global_Variables SWI Global variables


SWI-Prolog global variables are associations between names (atoms) and
terms.  They differ in various ways from storing information using
assert/1 or recorda/3.

    + The value lives on the Prolog (global) stack.  This implies 
that lookup time is independent from the size of the term.
This is particulary interesting for large data structures
such as parsed XML documents or the CHR global constraint
store.

 
*/

/** @pred They support both global assignment using nb_setval/2 and
backtrackable assignment using b_setval/2.

    + Only one value (which can be an arbitrary complex Prolog
term) can be associated to a variable at a time.

    + Their value cannot be shared among threads.  Each thread
has its own namespace and values for global variables.

    + Currently global variables are scoped globally.  We may
consider module scoping in future versions.


Both b_setval/2 and nb_setval/2 implicitly create a variable if the
referenced name does not already refer to a variable.

Global variables may be initialised from directives to make them
available during the program lifetime, but some considerations are
necessary for saved-states and threads. Saved-states to not store global
variables, which implies they have to be declared with initialization/1
to recreate them after loading the saved state.  Each thread has
its own set of global variables, starting with an empty set.  Using
`thread_inititialization/1` to define a global variable it will be
defined, restored after reloading a saved state and created in all
threads that are created <em>after</em> the registration.

 
*/

/** @pred b_setval(+ _Name_,+ _Value_) 


Associate the term  _Value_ with the atom  _Name_ or replaces
the currently associated value with  _Value_.  If  _Name_ does
not refer to an existing global variable a variable with initial value
`[]` is created (the empty list).  On backtracking the
assignment is reversed.

 
*/

/** @pred b_getval(+ _Name_,- _Value_) 


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further instantiate
the value of the global variable. If this is undesirable the normal
precautions (double negation or copy_term/2) must be taken. The
b_getval/2 predicate generates errors if  _Name_ is not an atom or
the requested variable does not exist.

 
*/

/** @pred nb_setval(+ _Name_,+ _Value_) 


Associates a copy of  _Value_ created with duplicate_term/2
with the atom  _Name_.  Note that this can be used to set an
initial value other than `[]` prior to backtrackable assignment.

 
*/

/** @pred nb_getval(+ _Name_,- _Value_) 


The nb_getval/2 predicate is a synonym for b_getval/2, introduced for
compatibility and symmetry.  As most scenarios will use a particular
global variable either using non-backtrackable or backtrackable
assignment, using nb_getval/2 can be used to document that the 
variable is used non-backtrackable.

 
*/

/** @pred nb_current(? _Name_,? _Value_) 


Enumerate all defined variables with their value. The order of
enumeration is undefined.

 
*/

/** @pred nb_delete(? _Name_)

Delete the named global variable.



@} */

/** @defgroup Compatibility_of_Global_Variables Compatibility of Global Variables
@ingroup YAPPackages
@{

Global variables have been introduced by various Prolog
implementations recently.  YAP follows their implementation in SWI-Prolog, itself
based on hProlog by Bart Demoen. Jan and Bart
decided that the semantics if hProlog nb_setval/2, which is
equivalent to nb_linkval/2 is not acceptable for normal Prolog
users as the behaviour is influenced by how builtin predicates
constructing terms (read/1, =../2, etc.) are implemented.

GNU-Prolog provides a rich set of global variables, including arrays.
Arrays can be implemented easily in SWI-Prolog using functor/3 and
`setarg/3` due to the unrestricted arity of compound terms.

*/

/** @page Extensions Extensions to Prolog

YAP includes a number of extensions over the original Prolog
language. Next, we discuss support to the most important ones.


@} */

/** @defgroup Rational_Trees Rational Trees
@ingroup YAPPackages
@{

Prolog unification is not a complete implementation. For efficiency
considerations, Prolog systems do not perform occur checks while
unifying terms. As an example, `X = a(X)` will not fail but instead
will create an infinite term of the form `a(a(a(a(a(...)))))`, or
<em>rational tree</em>.

Rational trees are now supported by default in YAP. In previous
versions, this was not the default and these terms could easily lead
to infinite computation. For example, `X = a(X), X = X` would
enter an infinite loop.

The `RATIONAL_TREES` flag improves support for these
terms. Internal primitives are now aware that these terms can exist, and
will not enter infinite loops. Hence, the previous unification will
succeed. Another example, `X = a(X), ground(X)` will succeed
instead of looping. Other affected built-ins include the term comparison
primitives, numbervars/3, copy_term/2, and the internal
data base routines. The support does not extend to Input/Output routines
or to assert/1 YAP does not allow directly reading
rational trees, and you need to use `write_depth/2` to avoid
entering an infinite cycle when trying to write an infinite term.


@} */

/** @defgroup CohYroutining Co-routining
@ingroup YAPPackages
@{

Prolog uses a simple left-to-right flow of control. It is sometimes
convenient to change this control so that goals will only be executed
when conditions are fulfilled. This may result in a more "data-driven"
execution, or may be necessary to correctly implement extensions such as
negation by default.

The `COROUTINING` flag enables this option. Note that the support for
coroutining  will in general slow down execution.

The following declaration is supported:

    + block/1
The argument to `block/1` is a condition on a goal or a conjunction
of conditions, with each element separated by commas. Each condition is
of the form `predname( _C1_,..., _CN_)`, where  _N_ is the
arity of the goal, and each  _CI_ is of the form `-`, if the
argument must suspend until the first such variable is bound, or
`?`, otherwise.

    + wait/1
The argument to `wait/1` is a predicate descriptor or a conjunction
of these predicates. These predicates will suspend until their first
argument is bound.


The following primitives are supported:

 
*/

/** @pred dif( _X_, _Y_) 


Succeed if the two arguments do not unify. A call to dif/2 will
suspend if unification may still succeed or fail, and will fail if they
always unify.

 
*/

/** @pred freeze(? _X_,: _G_) 


Delay execution of goal  _G_ until the variable  _X_ is bound.

 
*/

/** @pred frozen( _X_, _G_) 


Unify  _G_ with a conjunction of goals suspended on variable  _X_,
or `true` if no goal has suspended.

 
*/

/** @pred when(+ _C_,: _G_) 


Delay execution of goal  _G_ until the conditions  _C_ are
satisfied. The conditions are of the following form:

    + _C1_, _C2_
Delay until both conditions  _C1_ and  _C2_ are satisfied.
    + _C1_; _C2_
Delay until either condition  _C1_ or condition  _C2_ is satisfied.
    + ?=( _V1_, _C2_)
Delay until terms  _V1_ and  _V1_ have been unified.
    + nonvar( _V_)
Delay until variable  _V_ is bound.
    + ground( _V_)
Delay until variable  _V_ is ground.


Note that when/2 will fail if the conditions fail.

 
*/

/** @pred call_residue(: _G_, _L_) 



Call goal  _G_. If subgoals of  _G_ are still blocked, return
a list containing these goals and the variables they are blocked in. The
goals are then considered as unblocked. The next example shows a case
where dif/2 suspends twice, once outside call_residue/2,
and the other inside:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- dif(X,Y),
       call_residue((dif(X,Y),(X = f(Z) ; Y = f(Z))), L).

X = f(Z),
L = [[Y]-dif(f(Z),Y)],
dif(f(Z),Y) ? ;

Y = f(Z),
L = [[X]-dif(X,f(Z))],
dif(X,f(Z)) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The system only reports one invocation of dif/2 as having
suspended. 

 
*/

/** @pred call_residue_vars(: _G_, _L_) 



Call goal  _G_ and unify  _L_ with a list of all constrained variables created <em>during</em> execution of  _G_:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ?- dif(X,Z), call_residue_vars(dif(X,Y),L).
dif(X,Z), call_residue_vars(dif(X,Y),L).
L = [Y],
dif(X,Z),
dif(X,Y) ? ;

no
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup Attributed_Variables Attributed Variables
@ingroup YAPPackages
@{

YAP supports attributed variables, originally developed at OFAI by
Christian Holzbaur. Attributes are a means of declaring that an
arbitrary term is a property for a variable. These properties can be
updated during forward execution. Moreover, the unification algorithm is
aware of attributed variables and will call user defined handlers when
trying to unify these variables.

Attributed variables provide an elegant abstraction over which one can
extend Prolog systems. Their main application so far has been in
implementing constraint handlers, such as Holzbaur's CLPQR, Fruewirth
and Holzbaur's CHR, and CLP(BN). 

Different Prolog systems implement attributed variables in different
ways. Traditionally, YAP has used the interface designed by SICStus
Prolog. This interface is still
available in the <tt>atts</tt> library, but from YAP-6.0.3 we recommend using
the hProlog, SWI style interface. The main reason to do so is that 
most packages included in YAP that use attributed variables, such as CHR, CLP(FD), and CLP(QR),
rely on the SWI-Prolog interface.


@} */

/** @defgroup New_Style_Attribute_Declarations hProlog and SWI-Prolog style Attribute Declarations
@ingroup YAPPackages
@{

The following documentation is taken from the SWI-Prolog manual.

Binding an attributed variable schedules a goal to be executed at the
first possible opportunity. In the current implementation the hooks are
executed immediately after a successful unification of the clause-head
or successful completion of a foreign language (built-in) predicate. Each
attribute is associated to a module and the hook attr_unify_hook/2 is
executed in this module.  The example below realises a very simple and
incomplete finite domain reasoner.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(domain,
      [ domain/2            % Var, ?Domain
      ]).
:- use_module(library(ordsets)).

domain(X, Dom) :-
    var(Dom), !,
    get_attr(X, domain, Dom).
domain(X, List) :-
    list_to_ord_set(List, Domain),
    put_attr(Y, domain, Domain),
    X = Y.

%    An attributed variable with attribute value Domain has been
%    assigned the value Y

attr_unify_hook(Domain, Y) :-
    (   get_attr(Y, domain, Dom2)
    ->  ord_intersection(Domain, Dom2, NewDomain),
        (   NewDomain == []
        ->    fail
        ;    NewDomain = [Value]
        ->    Y = Value
        ;    put_attr(Y, domain, NewDomain)
        )
    ;   var(Y)
    ->  put_attr( Y, domain, Domain )
    ;   ord_memberchk(Y, Domain)
    ).

%    Translate attributes from this module to residual goals

attribute_goals(X) -->
    { get_attr(X, domain, List) },
    [domain(X, List)].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before explaining the code we give some example queries:

The predicate `domain/2` fetches (first clause) or assigns
(second clause) the variable a <em>domain</em>, a set of values it can
be unified with.  In the second clause first associates the domain
with a fresh variable and then unifies X to this variable to deal
with the possibility that X already has a domain. The
predicate attr_unify_hook/2 is a hook called after a variable with
a domain is assigned a value.  In the simple case where the variable
is bound to a concrete value we simply check whether this value is in
the domain. Otherwise we take the intersection of the domains and either
fail if the intersection is empty (first example), simply assign the
value if there is only one value in the intersection (second example) or
assign the intersection as the new domain of the variable (third
example). The nonterminal `attribute_goals/3` is used to translate
remaining attributes to user-readable goals that, when executed, reinstate
these attributes.



 @pred put_attr(+ _Var_,+ _Module_,+ _Value_) 



If  _Var_ is a variable or attributed variable, set the value for the
attribute named  _Module_ to  _Value_. If an attribute with this
name is already associated with  _Var_, the old value is replaced.
Backtracking will restore the old value (i.e., an attribute is a mutable
term. See also `setarg/3`). This predicate raises a representation error if
 _Var_ is not a variable and a type error if  _Module_ is not an atom.

 
*/

/** @pred get_attr(+ _Var_,+ _Module_,- _Value_) 



Request the current  _value_ for the attribute named  _Module_.  If
 _Var_ is not an attributed variable or the named attribute is not
associated to  _Var_ this predicate fails silently.  If  _Module_
is not an atom, a type error is raised.

 
*/

/** @pred del_attr(+ _Var_,+ _Module_) 



Delete the named attribute.  If  _Var_ loses its last attribute it
is transformed back into a traditional Prolog variable.  If  _Module_
is not an atom, a type error is raised. In all other cases this
predicate succeeds regardless whether or not the named attribute is
present.

 
*/

/** @pred attr_unify_hook(+ _AttValue_,+ _VarValue_) 



Hook that must be defined in the module an attributed variable refers
to. Is is called <em>after</em> the attributed variable has been
unified with a non-var term, possibly another attributed variable.
 _AttValue_ is the attribute that was associated to the variable
in this module and  _VarValue_ is the new value of the variable.
Normally this predicate fails to veto binding the variable to
 _VarValue_, forcing backtracking to undo the binding.  If
 _VarValue_ is another attributed variable the hook often combines
the two attribute and associates the combined attribute with
 _VarValue_ using put_attr/3.

 
*/

/** @pred attr_portray_hook(+ _AttValue_,+ _Var_) 



Called by write_term/2 and friends for each attribute if the option
`attributes(portray)` is in effect.  If the hook succeeds the
attribute is considered printed.  Otherwise  `Module = ...` is
printed to indicate the existence of a variable.

 
*/

/** @pred attribute_goals(+ _Var_,- _Gs_,+ _GsRest_) 



This nonterminal, if it is defined in a module, is used by  _copy_term/3_
to project attributes of that module to residual goals. It is also
used by the toplevel to obtain residual goals after executing a query.


Normal user code should deal with put_attr/3, get_attr/3 and del_attr/2.
The routines in this section fetch or set the entire attribute list of a
variables. Use of these predicates is anticipated to be restricted to
printing and other special purpose operations.



 @pred get_attrs(+ _Var_,- _Attributes_) 



Get all attributes of  _Var_.  _Attributes_ is a term of the form
`att( _Module_,  _Value_,  _MoreAttributes_)`, where  _MoreAttributes_ is
`[]` for the last attribute.

 
*/

/** @pred put_attrs(+ _Var_,+ _Attributes_) 


Set all attributes of  _Var_.  See get_attrs/2 for a description of
 _Attributes_.

 
*/

/** @pred del_attrs(+ _Var_) 


If  _Var_ is an attributed variable, delete <em>all</em> its
attributes.  In all other cases, this predicate succeeds without
side-effects.

 
*/

/** @pred term_attvars(+ _Term_,- _AttVars_) 


 _AttVars_ is a list of all attributed variables in  _Term_ and
its attributes. I.e., term_attvars/2 works recursively through
attributes.  This predicate is Cycle-safe.

 
*/

/** @pred copy_term(? _TI_,- _TF_,- _Goals_) 

Term  _TF_ is a variant of the original term  _TI_, such that for
each variable  _V_ in the term  _TI_ there is a new variable  _V'_
in term  _TF_ without any attributes attached.  Attributed
variables are thus converted to standard variables.   _Goals_ is
unified with a list that represents the attributes.  The goal
`maplist(call, _Goals_)` can be called to recreate the
attributes.

Before the actual copying, `copy_term/3` calls
`attribute_goals/1` in the module where the attribute is
defined.

 
*/

/** @pred copy_term_nat(? _TI_,- _TF_)  


As copy_term/2.  Attributes however, are <em>not</em> copied but replaced
by fresh variables.




@} */

/** @defgroup Old_Style_Attribute_Declarations SICStus Prolog style Attribute Declarations
@ingroup YAPLibrary
@{

Old style attribute declarations are activated through loading the library <tt>atts</tt> . The command

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(library(atts)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enables this form of use of attributed variables. The package provides the
following functionality:

    + Each attribute must be declared first. Attributes are described by a functor
and are declared per module. Each Prolog module declares its own sets of
attributes. Different modules may have different functors with the same
module.
    + The built-in put_atts/2 adds or deletes attributes to a
variable. The variable may be unbound or may be an attributed
variable. In the latter case, YAP discards previous values for the
attributes.
    + The built-in get_atts/2 can be used to check the values of
an attribute associated with a variable.
    + The unification algorithm calls the user-defined predicate
<tt>verify_attributes/3</tt> before trying to bind an attributed
variable. Unification will resume after this call.
    + The user-defined predicate
<tt>attribute_goal/2</tt> converts from an attribute to a goal.
    + The user-defined predicate
<tt>project_attributes/2</tt> is used from a set of variables into a set of
constraints or goals. One application of <tt>project_attributes/2</tt> is in
the top-level, where it is used to output the set of
floundered constraints at the end of a query.



@} */

/** @defgroup Attribute_Declarations Attribute Declarations
@ingroup Old_Style_Attribute_Declarations
@{

Attributes are compound terms associated with a variable. Each attribute
has a <em>name</em> which is <em>private</em> to the module in which the
attribute was defined. Variables may have at most one attribute with a
name. Attribute names are defined with the following declaration:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- attribute AttributeSpec, ..., AttributeSpec.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where each  _AttributeSpec_ has the form ( _Name_/ _Arity_).
One single such declaration is allowed per module  _Module_.

Although the YAP module system is predicate based, attributes are local
to modules. This is implemented by rewriting all calls to the
built-ins that manipulate attributes so that attribute names are
preprocessed depending on the module.  The `user:goal_expansion/3`
mechanism is used for this purpose.


@} */

/** @defgroup Attribute_Manipulation Attribute Manipulation
@ingroup Old_Style_Attribute_Declarations
@{

The  attribute manipulation predicates always work as follows:

<ol>
    + The first argument is the unbound variable associated with
attributes,
    + The second argument is a list of attributes. Each attribute will
be a Prolog term or a constant, prefixed with the <tt>+</tt> and <tt>-</tt> unary
operators. The prefix <tt>+</tt> may be dropped for convenience.
</ol>

The following three procedures are available to the user. Notice that
these built-ins are rewritten by the system into internal built-ins, and
that the rewriting process <em>depends</em> on the module on which the
built-ins have been invoked.

 
*/

/** @pred _Module_:get_atts( _-Var_, _?ListOfAttributes_) 


Unify the list  _?ListOfAttributes_ with the attributes for the unbound
variable  _Var_. Each member of the list must be a bound term of the
form `+( _Attribute_)`, `-( _Attribute_)` (the <tt>kbd</tt>
prefix may be dropped). The meaning of <tt>+</tt> and <tt>-</tt> is:
    + +( _Attribute_)
Unifies  _Attribute_ with a corresponding attribute associated with
 _Var_, fails otherwise.

    + -( _Attribute_)
Succeeds if a corresponding attribute is not associated with
 _Var_. The arguments of  _Attribute_ are ignored.

 
*/

/** @pred _Module_:put_atts( _-Var_, _?ListOfAttributes_) 


Associate with or remove attributes from a variable  _Var_. The
attributes are given in  _?ListOfAttributes_, and the action depends
on how they are prefixed:
    + +( _Attribute_)
Associate  _Var_ with  _Attribute_. A previous value for the
attribute is simply replace (like with `set_mutable/2`).

    + -( _Attribute_)
Remove the attribute with the same name. If no such attribute existed,
simply succeed.



@} */

/** @defgroup Attributed_Unification Attributed Unification
@ingroup Old_Style_Attribute_Declarations
@{

The user-predicate predicate verify_attributes/3 is called when
attempting to unify an attributed variable which might have attributes
in some  _Module_.

 
*/

/** @pred _Module_:verify_attributes( _-Var_,  _+Value_,  _-Goals_) 



The predicate is called when trying to unify the attributed variable
 _Var_ with the Prolog term  _Value_. Note that  _Value_ may be
itself an attributed variable, or may contain attributed variables.  The
goal <tt>verify_attributes/3</tt> is actually called before  _Var_ is
unified with  _Value_.

It is up to the user to define which actions may be performed by
<tt>verify_attributes/3</tt> but the procedure is expected to return in
 _Goals_ a list of goals to be called <em>after</em>  _Var_ is
unified with  _Value_. If <tt>verify_attributes/3</tt> fails, the
unification will fail.

Notice that the <tt>verify_attributes/3</tt> may be called even if  _Var_\<
has no attributes in module <tt>Module</tt>. In this case the routine should
simply succeed with  _Goals_ unified with the empty list.

 
*/

/** @pred attvar( _-Var_) 


Succeed if  _Var_ is an attributed variable.



@} */

/** @defgroup Displaying_Attributes Displaying Attributes
@ingroup Old_Style_Attribute_Declarations
@{

Attributes are usually presented as goals. The following routines are
used by built-in predicates such as call_residue/2 and by the
Prolog top-level to display attributes:

 
*/

/** @pred _Module_:attribute_goal( _-Var_,  _-Goal_)
User-defined procedure, called to convert the attributes in  _Var_ to
a  _Goal_. Should fail when no interpretation is available.




@} */

/** @defgroup Projecting_Attributes Projecting Attributes
@ingroup Old_Style_Attribute_Declarations
@{

Constraint solvers must be able to project a set of constraints to a set
of variables. This is useful when displaying the solution to a goal, but
may also be used to manipulate computations. The user-defined
project_attributes/2 is responsible for implementing this
projection.

 
*/

/** @pred _Module_:project_attributes( _+QueryVars_,  _+AttrVars_) 


Given a list of variables  _QueryVars_ and list of attributed
variables  _AttrVars_, project all attributes in  _AttrVars_ to
 _QueryVars_. Although projection is constraint system dependent,
typically this will involve expressing all constraints in terms of
 _QueryVars_ and considering all remaining variables as existentially
quantified.


Projection interacts with attribute_goal/2 at the Prolog top
level. When the query succeeds, the system first calls
project_attributes/2. The system then calls
attribute_goal/2 to get a user-level representation of the
constraints. Typically, attribute_goal/2 will convert from the
original constraints into a set of new constraints on the projection,
and these constraints are the ones that will have an
attribute_goal/2 handler.


@} */

/** @defgroup Attribute_Examples Attribute Examples
@ingroup Old_Style_Attribute_Declarations
@{

The following two examples example is taken from the SICStus Prolog manual. It
sketches the implementation of a simple finite domain ``solver''.  Note
that an industrial strength solver would have to provide a wider range
of functionality and that it quite likely would utilize a more efficient
representation for the domains proper.  The module exports a single
predicate `domain( _-Var_, _?Domain_)` which associates
 _Domain_ (a list of terms) with  _Var_.  A variable can be
queried for its domain by leaving  _Domain_ unbound.

We do not present here a definition for project_attributes/2.
Projecting finite domain constraints happens to be difficult.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(domain, [domain/2]).

:- use_module(library(atts)).
:- use_module(library(ordsets), [
        ord_intersection/3,
        ord_intersect/2,
        list_to_ord_set/2
   ]).

:- attribute dom/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, dom(Da)), !,          % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, dom(Db)) -> %   has a domain?
                ord_intersection(Da, Db, Dc),
                Dc = [El|Els],              % at least one element
                (   Els = [] ->             % exactly one element
                    Goals = [Other=El]      % implied binding
                ;   Goals = [],
                    put_atts(Other, dom(Dc))% rescue intersection
                )
            ;   Goals = [],
                put_atts(Other, dom(Da))    % rescue the domain
            )
        ;   Goals = [],
            ord_intersect([Other], Da)      % value in domain?
        ).
verify_attributes(_, _, []).                % unification triggered
                                            % because of attributes
                                            % in other modules

attribute_goal(Var, domain(Var,Dom)) :-     % interpretation as goal
        get_atts(Var, dom(Dom)).

domain(X, Dom) :-
        var(Dom), !,
        get_atts(X, dom(Dom)).
domain(X, List) :-
        list_to_ord_set(List, Set),
        Set = [El|Els],                     % at least one element
        (   Els = [] ->                     % exactly one element
            X = El                          % implied binding
        ;   put_atts(Fresh, dom(Set)),
            X = Fresh                       % may call
                                            % verify_attributes/3
        ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the ``implied binding'' `Other=El` was deferred until after
the completion of `verify_attribute/3`.  Otherwise, there might be a
danger of recursively invoking `verify_attribute/3`, which might bind
`Var`, which is not allowed inside the scope of `verify_attribute/3`.
Deferring unifications into the third argument of `verify_attribute/3`
effectively serializes the calls to `verify_attribute/3`.

Assuming that the code resides in the file domain.yap, we
can use it via:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(domain).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's test it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]).

domain(X,[1,5,6,7]),
domain(Y,[3,4,5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]), 
     X=Y.

Y = X,
domain(X,[5,6]),
domain(Z,[1,6,7,8]) ? 

yes
| ?- domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]),
     X=Y, Y=Z.

X = 6,
Y = 6,
Z = 6
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To demonstrate the use of the  _Goals_ argument of
verify_attributes/3, we give an implementation of
freeze/2.  We have to name it `myfreeze/2` in order to
avoid a name clash with the built-in predicate of the same name.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(myfreeze, [myfreeze/2]).

:- use_module(library(atts)).

:- attribute frozen/1.

verify_attributes(Var, Other, Goals) :-
        get_atts(Var, frozen(Fa)), !,       % are we involved?
        (   var(Other) ->                   % must be attributed then
            (   get_atts(Other, frozen(Fb)) % has a pending goal?
            ->  put_atts(Other, frozen((Fa,Fb))) % rescue conjunction
            ;   put_atts(Other, frozen(Fa)) % rescue the pending goal
            ),
            Goals = []
        ;   Goals = [Fa]
        ).
verify_attributes(_, _, []).

attribute_goal(Var, Goal) :-                % interpretation as goal
        get_atts(Var, frozen(Goal)).

myfreeze(X, Goal) :-
        put_atts(Fresh, frozen(Goal)),
        Fresh = X.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assuming that this code lives in file myfreeze.yap,
we would use it via:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- use_module(myfreeze).
| ?- myfreeze(X,print(bound(x,X))), X=2.

bound(x,2)                      % side effect
X = 2                           % bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two solvers even work together:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
| ?- myfreeze(X,print(bound(x,X))), domain(X,[1,2,3]),
     domain(Y,[2,10]), X=Y.

bound(x,2)                      % side effect
X = 2,                          % bindings
Y = 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two example solvers interact via bindings to shared attributed
variables only.  More complicated interactions are likely to be found
in more sophisticated solvers.  The corresponding
verify_attributes/3 predicates would typically refer to the
attributes from other known solvers/modules via the module prefix in
` _Module_:get_atts/2`.


@} */

/** @defgroup CLPR Constraint Logic Programming over Reals
@ingroup YAPPackages
@{

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- use_module(library(clpr)).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


@} */

/** @defgroup CLPR_Solver_Predicates Solver Predicates
@ingroup CLPR
@{


The following predicates are provided to work with constraints:

 
*/

/** @pred {+ _Constraints_}
Adds the constraints given by  _Constraints_ to the constraint store.

 
*/

/** @pred entailed(+ _Constraint_)
Succeeds if  _Constraint_ is necessarily true within the current
constraint store. This means that adding the negation of the constraint
to the store results in failure.

 
*/

/** @pred inf(+ _Expression_,- _Inf_)
Computes the infimum of  _Expression_ within the current state of the
constraint store and returns that infimum in  _Inf_. This predicate
does not change the constraint store.

 
*/

/** @pred inf(+ _Expression_,- _Sup_)
Computes the supremum of  _Expression_ within the current state of
the constraint store and returns that supremum in  _Sup_. This
predicate does not change the constraint store.

 
*/

/** @pred min(+ _Expression_)
Minimizes  _Expression_ within the current constraint store. This is
the same as computing the infimum and equation the expression to that
infimum.

 
*/

/** @pred max(+ _Expression_)
Maximizes  _Expression_ within the current constraint store. This is
the same as computing the supremum and equating the expression to that
supremum.

 
*/

/** @pred bb_inf(+ _Ints_,+ _Expression_,- _Inf_,- _Vertext_,+ _Eps_)
Computes the infimum of  _Expression_ within the current constraint
store, with the additional constraint that in that infimum, all
variables in  _Ints_ have integral values.  _Vertex_ will contain
the values of  _Ints_ in the infimum.  _Eps_ denotes how much a
value may differ from an integer to be considered an integer. E.g. when
 _Eps_ = 0.001, then X = 4.999 will be considered as an integer (5 in
this case).  _Eps_ should be between 0 and 0.5.

 
*/

/** @pred bb_inf(+ _Ints_,+ _Expression_,- _Inf_)
The same as bb_inf/5 but without returning the values of the integers
and with an eps of 0.001.

 
*/

/** @pred dump(+ _Target_,+ _Newvars_,- _CodedAnswer_)
Returns the constraints on  _Target_ in the list  _CodedAnswer_
where all variables of  _Target_ have veen replaced by  _NewVars_.
This operation does not change the constraint store. E.g. in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dump([X,Y,Z],[x,y,z],Cons)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 _Cons_ will contain the constraints on  _X_,  _Y_ and
 _Z_ where these variables have been replaced by atoms `x`, `y` and `z`.




@} */

/** @defgroup CLPR_Syntax Syntax of the predicate arguments
@ingroup YAPPackages
@{


The arguments of the predicates defined in the subsection above are
defined in the following table. Failing to meet the syntax rules will
result in an exception.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<Constraints> ---> <Constraint>				\\ single constraint \\
	      | <Constraint> , <Constraints>		\\ conjunction \\
	      | <Constraint> ; <Constraints>		\\ disjunction \\

<Constraint> ---> <Expression> {<} <Expression>		\\ less than \\
	     | <Expression> {>} <Expression>		\\ greater than \\
	     | <Expression> {=<} <Expression>	\\ less or equal \\
	     | {<=}(<Expression>, <Expression>)	\\ less or equal \\
	     | <Expression> {>=} <Expression>	\\ greater or equal \\
	     | <Expression> {=\=} <Expression>	\\ not equal \\
	     | <Expression> =:= <Expression>		\\ equal \\
	     | <Expression> = <Expression>		\\ equal \\

<Expression> --->  <Variable>				\\ Prolog variable \\
	     | <Number>				\\ Prolog number (float, integer) \\
	     | +<Expression>				\\ unary plus \\
	     | -<Expression>				\\ unary minus \\
	     | <Expression> + <Expression>		\\ addition \\
	     | <Expression> - <Expression>		\\ substraction \\
	     | <Expression> * <Expression>		\\ multiplication \\
	     | <Expression> / <Expression>		\\ division \\
	     | abs(<Expression>)			\\ absolute value \\
	     | sin(<Expression>)			\\ sine \\
	     | cos(<Expression>)			\\ cosine \\
	     | tan(<Expression>)			\\ tangent \\
	     | exp(<Expression>)			\\ exponent \\
	     | pow(<Expression>)			\\ exponent \\
	     | <Expression> {^} <Expression>		\\ exponent \\
	     | min(<Expression>, <Expression>)	\\ minimum \\
	     | max(<Expression>, <Expression>)	\\ maximum \\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


@} */

/** @defgroup CLPR_Unification Use of unification
@ingroup CLPR
@{

Instead of using the `{}/1` predicate, you can also use the standard
unification mechanism to store constraints. The following code samples
are equivalent:

    + Unification with a variable

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{X =:= Y}
{X = Y}
X = Y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + Unification with a number

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{X =:= 5.0}
{X = 5.0}
X = 5.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup CLPR_NonhYlinear_Constraints Non-Linear Constraints
@ingroup CLPR
@{


In this version, non-linear constraints do not get solved until certain
conditions are satisfied. We call these conditions the isolation axioms.
They are given in the following table.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A = B * C         when B or C is ground	or		 // A = 5 * C or A = B * 4 \\
	                      A and (B or C) are ground	 // 20 = 5 * C or 20 = B * 4 \\

A = B / C      when C is ground or			// A = B / 3 
	                      A and B are ground		// 4 = 12 / C 

X = min(Y,Z)     when Y and Z are ground or	// X = min(4,3) 
X = max(Y,Z)         Y and Z are ground		// X = max(4,3) 
X = abs(Y)                Y is ground			// X = abs(-7) 

X = pow(Y,Z)   when X and Y are ground or		// 8 = 2 ^ Z 
X = exp(Y,Z)           X and Z are ground		// 8 = Y ^ 3 
X = Y ^ Z            Y and Z are ground		// X = 2 ^ 3 

X = sin(Y)	    when X is ground or			// 1 = sin(Y) 
X = cos(Y)	               Y is ground			// X = sin(1.5707) 
X = tan(Y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section CHR CHR: Constraint Handling Rules 
@ingroup YAPPackages

This chapter is written by Tom Schrijvers, K.U. Leuven for the hProlog
system. Adjusted by Jan Wielemaker to fit the SWI-Prolog documentation
infrastructure and remove hProlog specific references.

The CHR system of SWI-Prolog is the K.U.Leuven CHR system.  The runtime
environment is written by Christian Holzbaur and Tom Schrijvers while the
compiler is written by Tom Schrijvers. Both are integrated with SWI-Prolog
and licenced under compatible conditions with permission from the authors.

The main reference for SWI-Prolog's CHR system is:

    + T. Schrijvers, and B. Demoen, <em>The K.U.Leuven CHR System: Implementation and Application</em>, First Workshop on Constraint Handling Rules: Selected
Contributions (Fruwirth, T. and Meister, M., eds.), pp. 1--5, 2004.



@} */

/** @defgroup CHR_Introduction Introduction
@ingroup CHR
@{


Constraint Handling Rules (CHR) is a committed-choice bottom-up language
embedded in Prolog. It is designed for writing constraint solvers and is
particularily useful for providing application-specific constraints.
It has been used in many kinds of applications, like scheduling,
model checking, abduction, type checking among many others.

CHR has previously been implemented in other Prolog systems (SICStus,
Eclipse, Yap), Haskell and Java. This CHR system is based on the
compilation scheme and runtime environment of CHR in SICStus.

In this documentation we restrict ourselves to giving a short overview
of CHR in general and mainly focus on elements specific to this
implementation. For a more thorough review of CHR we refer the reader to
[Freuhwirth:98]. More background on CHR can be found at the CHR web site.


@} */

/** @defgroup CHR_Syntax_and_Semantics Syntax and Semantics
@ingroup YAPPackages
@{




@} */

/** @defgroup CHR_Syntax CHR Syntax
Wingroup CHR
@{

The syntax of CHR rules in hProlog is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rules --> rule, rules.
rules --> [].

rule --> name, actual_rule, pragma, [atom('.')].

name --> atom, [atom('@')].
name --> [].

actual_rule --> simplification_rule.
actual_rule --> propagation_rule.
actual_rule --> simpagation_rule.

simplification_rule --> constraints, [atom('<=>')], guard, body.
propagation_rule --> constraints, [atom('==>')], guard, body.
simpagation_rule --> constraints, [atom('\')], constraints, [atom('<=>')], 
                     guard, body.

constraints --> constraint, constraint_id.
constraints --> constraint, [atom(',')], constraints.

constraint --> compound_term.

constraint_id --> [].
constraint_id --> [atom('#')], variable.

guard --> [].
guard --> goal, [atom('|')].

body --> goal.

pragma --> [].
pragma --> [atom('pragma')], actual_pragmas.

actual_pragmas --> actual_pragma.
actual_pragmas --> actual_pragma, [atom(',')], actual_pragmas.

actual_pragma --> [atom('passive(')], variable, [atom(')')].

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Additional syntax-related terminology:

    + *head:* the constraints in an `actual_rule` before
the arrow (either `\<=\>` or `==\>`)



@} */

/** @defgroup Semantics Semantics
@ingroup CHR
@{


In this subsection the operational semantics of CHR in Prolog are presented
informally. They do not differ essentially from other CHR systems.

When a constraint is called, it is considered an active constraint and
the system will try to apply the rules to it. Rules are tried and executed
sequentially in the order they are written. 

A rule is conceptually tried for an active constraint in the following
way. The active constraint is matched with a constraint in the head of
the rule. If more constraints appear in the head they are looked for
among the suspended constraints, which are called passive constraints in
this context. If the necessary passive constraints can be found and all
match with the head of the rule and the guard of the rule succeeds, then
the rule is committed and the body of the rule executed. If not all the
necessary passive constraint can be found, the matching fails or the
guard fails, then the body is not executed and the process of trying and
executing simply continues with the following rules. If for a rule,
there are multiple constraints in the head, the active constraint will
try the rule sequentially multiple times, each time trying to match with
another constraint.

This process ends either when the active constraint disappears, i.e. it
is removed by some rule, or after the last rule has been processed. In
the latter case the active constraint becomes suspended.

A suspended constraint is eligible as a passive constraint for an active
constraint. The other way it may interact again with the rules, is when
a variable appearing in the constraint becomes bound to either a nonvariable
or another variable involved in one or more constraints. In that case the
constraint is triggered, i.e. it becomes an active constraint and all
the rules are tried.


@} */

/** @defgroup Rule_Types
@ingroup CHR
@{

There are three different kinds of rules, each with their specific semantics:

    + simplification
The simplification rule removes the constraints in its head and calls its body.

    + propagation
The propagation rule calls its body exactly once for the constraints in
its head.

    + simpagation
The simpagation rule removes the constraints in its head after the
`\\` and then calls its body. It is an optimization of
simplification rules of the form: \\[constraints_1, constraints_2 \<=\>
constraints_1, body \\] Namely, in the simpagation form: 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
constraints1 \ constraints2 <=> body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 _constraints1_
constraints are not called in the body.



@} */

/** @defgroup CHR_Rule_Names Rule Names
@ingroup CHR
@{

Naming a rule is optional and has no semantical meaning. It only functions
as documentation for the programmer.


@} */

/** @defgroup CHRPragmas Pragmas
@ingroup CHR_Rule_Names
@{

The semantics of the pragmas are:

    + passive(Identifier)
The constraint in the head of a rule  _Identifier_ can only act as a
passive constraint in that rule.


Additional pragmas may be released in the future.


@} */

/** @defgroup CHR_Options Options
@ingroup CHR_Rule_Names
@{

It is possible to specify options that apply to all the CHR rules in the module.
Options are specified with the `option/2` declaration:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                option(Option,Value).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Available options are:

    + check_guard_bindings
This option controls whether guards should be checked for illegal
variable bindings or not. Possible values for this option are
`on`, to enable the checks, and `off`, to disable the
checks.

    + optimize
This is an experimental option controlling the degree of optimization.
Possible values are `full`, to enable all available
optimizations, and `off` (default), to disable all optimizations.  
The default is derived from the SWI-Prolog flag `optimise`, where
`true` is mapped to `full`.  Therefore the commandline
option `-O` provides full CHR optimization.
If optimization is enabled, debugging should be disabled.

    + debug
This options enables or disables the possibility to debug the CHR code.
Possible values are `on` (default) and `off`. See
`debugging` for more details on debugging.  The default is
derived from the prolog flag `generate_debug_info`, which
is `true` by default.  See `-nodebug`.
If debugging is enabled, optimization should be disabled.

    + mode
This option specifies the mode for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be one of `-`, `+` or `?`.
The latter is the default. The meaning is the following:

    + -
The corresponding argument of every occurrence
of the constraint is always unbound.
    + + 
The corresponding argument of every occurrence
of the constraint is always ground.
    + ?
The corresponding argument of every occurrence
of the constraint can have any instantiation, which may change
over time. This is the default value.

The declaration is used by the compiler for various optimizations. 
Note that it is up to the user the ensure that the mode declaration
is correct with respect to the use of the constraint.
This option may occur once for each constraint.

    + type_declaration
This option specifies the argument types for a particular constraint. The
value is a term with functor and arity equal to that of a constraint.
The arguments can be a user-defined type or one of
the built-in types:

    + int
The corresponding argument of every occurrence
of the constraint is an integer number.
    + float
...{} a floating point number.
    + number
...{} a number.
    + natural
...{} a positive integer.
    + any
The corresponding argument of every occurrence
of the constraint can have any type. This is the default value.


Currently, type declarations are only used to improve certain
optimizations (guard simplification, occurrence subsumption, ...{}).

    + type_definition
This option defines a new user-defined type which can be used in
type declarations. The value is a term of the form
`type(` _name_`,` _list_`)`, where
 _name_ is a term and  _list_ is a list of alternatives.
Variables can be used to define generic types. Recursive definitions
are allowed. Examples are 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(bool,[true,false]).
type(complex_number,[float + float * i]).
type(binary_tree(T),[ leaf(T) | node(binary_tree(T),binary_tree(T)) ]).
type(list(T),[ [] | [T | list(T)]).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



The mode, type_declaration and type_definition options are provided
for backward compatibility. The new syntax is described below.


@} */

/** @defgroup CHR_in_YAP_Programs CHR in YAP Programs	
@ingroup CHR
@{


The CHR constraints defined in a particulary chr file are
associated with a module. The default module is `user`. One should
never load different chr files with the same CHR module name.


@} */

/** @defgroup Constraint_declaration Constraint declaration
@ingroup CHR_in_YAP_Programs
@{


Every constraint used in CHR rules has to be declared.
There are two ways to do this. The old style is as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
option(type_definition,type(list(T),[ [] , [T|list(T)] ]).
option(mode,foo(+,?)).
option(type_declaration,foo(list(int),float)).
:- constraints foo/2, bar/0.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The new style is as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- chr_type list(T) ---> [] ; [T|list(T)].
:- constraints foo(+list(int),?float), bar.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


@} */

/** @defgroup Compilation Compilation

The@{
 SWI-Prolog CHR compiler exploits term_expansion/2 rules to translate
the constraint handling rules to plain Prolog. These rules are loaded
from the library chr.   They are activated if the compiled file
has the chr extension or after finding a declaration of the
format below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- constraints ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is adviced to define CHR rules in a module file, where the module
declaration is immediately followed by including the chr
library as examplified below:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(zebra, [ zebra/0 ]).
:- use_module(library(chr)).

:- constraints ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using this style CHR rules can be defined in ordinary Prolog
pl files and the operator definitions required by CHR do not
leak into modules where they might cause conflicts.


@} */

/** @defgroup CHR_Debugging Debugging
@ingroup CHR
@{



The CHR debugging facilities are currently rather limited. Only tracing
is currently available. To use the CHR debugging facilities for a CHR
file it must be compiled for debugging. Generating debug info is
controlled by the CHR option debug, whose default is derived
from the SWI-Prolog flag `generate_debug_info`.  Therefore debug
info is provided unless the `-nodebug` is used.


@} */

/** @defgroup Ports Ports
@ingroup CHR
@{


For CHR constraints the four standard ports are defined:

    + call
A new constraint is called and becomes active.
    + exit
An active constraint exits: it has either been inserted in the store after
trying all rules or has been removed from the constraint store.
    + fail
An active constraint fails.
    + redo
An active constraint starts looking for an alternative solution.


In addition to the above ports, CHR constraints have five additional
ports:

    + wake
A suspended constraint is woken and becomes active.
    + insert
An active constraint has tried all rules and is suspended in
the constraint store.
    + remove
An active or passive constraint is removed from the constraint
store, if it had been inserted.
    + try
An active constraints tries a rule with possibly
some passive constraints. The try port is entered
just before committing to the rule.
    + apply
An active constraints commits to a rule with possibly
some passive constraints. The apply port is entered
just after committing to the rule.



@} */

/** @defgroup Tracing Tracing
@ingroup CHR
@{


Tracing is enabled with the chr_trace/0 predicate
and disabled with the chr_notrace/0 predicate.

When enabled the tracer will step through the `call`,
`exit`, `fail`, `wake` and `apply` ports,
accepting debug commands, and simply write out the other ports.

The following debug commans are currently supported:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CHR debug options:

                <cr>    creep           c       creep
		s	skip
		g	ancestors
                n       nodebug
		b	break
                a       abort
                f       fail
                ?       help            h       help
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Their meaning is:

    + creep
Step to the next port.
    + skip
Skip to exit port of this call or wake port.
    + ancestors
Print list of ancestor call and wake ports.
    + nodebug
Disable the tracer.
    + break
Enter a recursive Prolog toplevel.  See break/0.
    + abort
Exit to the toplevel.  See abort/0.
    + fail
Insert failure in execution.
    + help
Print the above available debug options.



@} */

/** @defgroup CHR_Debugging_Predicates CHR Debugging Predicates
@ingroup CHR
@{


The chr module contains several predicates that allow
inspecting and printing the content of the constraint store.

    + chr_trace/0
Activate the CHR tracer.  By default the CHR tracer is activated and
deactivated automatically by the Prolog predicates trace/0 and
notrace/0.

 
*/

/** @pred chr_notrace/0
De-activate the CHR tracer.  By default the CHR tracer is activated and
deactivated automatically by the Prolog predicates trace/0 and
notrace/0.

    + chr_leash/0 

Define the set of CHR ports on which the CHR
tracer asks for user intervention (i.e. stops).  _Spec_ is either a
list of ports or a predefined `alias'. Defined aliases are:
`full` to stop at all ports, `none` or `off` to never
stop, and `default` to stop at the `call`, `exit`,
`fail`, `wake` and `apply` ports.  See also leash/1.

 
*/

/** @pred chr_show_store(+ _Mod_)
Prints all suspended constraints of module  _Mod_ to the standard
output. This predicate is automatically called by the SWI-Prolog toplevel at
the end of each query for every CHR module currently loaded.  The prolog-flag
`chr_toplevel_show_store` controls whether the toplevel shows the
constraint stores. The value `true` enables it.  Any other value
disables it.




@} */

/** @defgroup CHR_Examples Examples
@ingroup CHR
@{



Here are two example constraint solvers written in CHR.

    + 
The program below defines a solver with one constraint, 
`leq/2`, which is a less-than-or-equal constraint.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(leq,[cycle/3, leq/2]).
:- use_module(library(chr)).

:- constraints leq/2.
reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

cycle(X,Y,Z):-
        leq(X,Y),
        leq(Y,Z),
        leq(Z,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + 
The program below implements a simple finite domain
constraint solver.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- module(dom,[dom/2]).
:- use_module(library(chr)).

:- constraints dom/2. 

dom(X,[]) <=> fail.
dom(X,[Y]) <=> X = Y.
dom(X,L1), dom(X,L2) <=> intersection(L1,L2,L3), dom(X,L3).

intersection([],_,[]).
intersection([H|T],L2,[H|L3]) :-
        member(H,L2), !,
        intersection(T,L2,L3).
intersection([_|T],L2,L3) :-
        intersection(T,L2,L3).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup CHR_Compatibility Compatibility with SICStus CHR
@ingroup YAPPackages
@{



There are small differences between CHR in SWI-Prolog and newer
YAPs and SICStus and older versions of YAP.  Besides differences in
available options and pragmas, the following differences should be
noted:

    + [The handler/1 declaration]
In SICStus every CHR module requires a `handler/1`
declaration declaring a unique handler name. This declaration is valid
syntax in SWI-Prolog, but will have no effect. A warning will be given
during compilation.

    + [The rules/1 declaration]
In SICStus, for every CHR module it is possible to only enable a subset
of the available rules through the `rules/1` declaration. The
declaration is valid syntax in SWI-Prolog, but has no effect. A
warning is given during compilation.

    + [Sourcefile naming]
SICStus uses a two-step compiler, where chr files are
first translated into pl files.  For SWI-Prolog CHR
rules may be defined in a file with any extension.



@} */

/** @defgroup CHR_Guidelines Guidelines
@ingroup YAPPackages
@{



In this section we cover several guidelines on how to use CHR to write
constraint solvers and how to do so efficiently.

    + [Set semantics]
The CHR system allows the presence of identical constraints, i.e.
multiple constraints with the same functor, arity and arguments. For
most constraint solvers, this is not desirable: it affects efficiency
and possibly termination. Hence appropriate simpagation rules should be
added of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{constraint \ constraint <=> true}.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    + [Multi-headed rules]
Multi-headed rules are executed more efficiently when the constraints
share one or more variables.

    + [Mode and type declarations]
Provide mode and type declarations to get more efficient program execution.
Make sure to disable debug (`-nodebug`) and enable optimization
(`-O`).



@} */

/** @defgroup Logtalk Logtalk
@ingroup YAPPackages
@{

The Logtalk object-oriented extension is available after running its 
standalone installer by using the `yaplgt` command in POSIX 
systems or by using the `Logtalk - YAP` shortcut in the Logtalk 
program group in the Start Menu on Windows systems. For more information 
please see the URL <http://logtalk.org/>.


\copydoc real


@} */

/** @defgroup Threads Threads
@ingroup YAPBuiltins
@{

YAP implements a SWI-Prolog compatible multithreading
library. Like in SWI-Prolog, Prolog threads have their own stacks and
only share the Prolog <em>heap</em>: predicates, records, flags and other
global non-backtrackable data.  The package is based on the POSIX thread
standard (Butenhof:1997:PPT) used on most popular systems except
for MS-Windows.


@} */

/** @defgroup Creating_and_Destroying_Prolog_Threads Creating and Destroying Prolog Threads
@ingroup Threads
@{



 @pred thread_create(: _Goal_, - _Id_, + _Options_) 



Create a new Prolog thread (and underlying C-thread) and start it
by executing  _Goal_.  If the thread is created successfully, the
thread-identifier of the created thread is unified to  _Id_.
 _Options_ is a list of options.  Currently defined options are:

    + stack
Set the limit in K-Bytes to which the Prolog stacks of
this thread may grow.  If omitted, the limit of the calling thread is
used.  See also the  commandline `-S` option.

    + trail
Set the limit in K-Bytes to which the trail stack of this thread may
grow.  If omitted, the limit of the calling thread is used. See also the
commandline option `-T`.

    + alias
Associate an alias-name with the thread.  This named may be used to
refer to the thread and remains valid until the thread is joined
(see thread_join/2).

    + at_exit
Define an exit hook for the thread.  This hook is called when the thread
terminates, no matter its exit status.

    + detached
If `false` (default), the thread can be waited for using
thread_join/2. thread_join/2 must be called on this thread
to reclaim the all resources associated to the thread. If `true`,
the system will reclaim all associated resources automatically after the
thread finishes. Please note that thread identifiers are freed for reuse
after a detached thread finishes or a normal thread has been joined.
See also thread_join/2 and thread_detach/1.


The  _Goal_ argument is <em>copied</em> to the new Prolog engine.
This implies further instantiation of this term in either thread does
not have consequences for the other thread: Prolog threads do not share
data from their stacks.

 
*/

/** @pred thread_create(: _Goal_, - _Id_)


Create a new Prolog thread using default options. See thread_create/3.

 
*/

/** @pred thread_create(: _Goal_)


Create a new Prolog detached thread using default options. See thread_create/3.

 
*/

/** @pred thread_self(- _Id_) 


Get the Prolog thread identifier of the running thread.  If the thread
has an alias, the alias-name is returned.

 
*/

/** @pred thread_join(+ _Id_, - _Status_) 


Wait for the termination of thread with given  _Id_.  Then unify the
result-status of the thread with  _Status_.  After this call,
 _Id_ becomes invalid and all resources associated with the thread
are reclaimed.  Note that threads with the attribute `detached`
`true` cannot be joined.  See also current_thread/2.

A thread that has been completed without thread_join/2 being
called on it is partly reclaimed: the Prolog stacks are released and the
C-thread is destroyed. A small data-structure representing the
exit-status of the thread is retained until thread_join/2 is called on
the thread.  Defined values for  _Status_ are:

    + true
The goal has been proven successfully.

    + false
The goal has failed.

    + exception( _Term_)
The thread is terminated on an
exception.  See print_message/2 to turn system exceptions into
readable messages.

    + exited( _Term_)
The thread is terminated on thread_exit/1 using the argument  _Term_.


    + thread_detach(+ _Id_) 


Switch thread into detached-state (see `detached` option at
thread_create/3 at runtime.   _Id_ is the identifier of the thread
placed in detached state.

One of the possible applications is to simplify debugging. Threads that
are created as `detached` leave no traces if they crash. For
not-detached threads the status can be inspected using
current_thread/2.  Threads nobody is waiting for may be created
normally and detach themselves just before completion.  This way they
leave no traces on normal completion and their reason for failure can be
inspected.

 
*/

/** @pred thread_yield 


Voluntarily relinquish the processor.

 
*/

/** @pred thread_exit(+ _Term_) 


Terminates the thread immediately, leaving `exited( _Term_)` as
result-state for thread_join/2.  If the thread has the attribute
`detached` `true` it terminates, but its exit status cannot be
retrieved using thread_join/2 making the value of  _Term_
irrelevant.  The Prolog stacks and C-thread are reclaimed.

 
*/

/** @pred thread_at_exit(: _Term_) 


Run  _Goal_ just before releasing the thread resources. This is to
be compared to `at_halt/1`, but only for the current
thread. These hooks are ran regardless of why the execution of the
thread has been completed. As these hooks are run, the return-code is
already available through thread_property/2 using the result of
thread_self/1 as thread-identifier. If you want to guarantee the 
execution of an exit hook no matter how the thread terminates (the thread 
can be aborted before reaching the thread_at_exit/1 call), consider
using instead the `at_exit/1` option of thread_create/3. 

 
*/

/** @pred thread_setconcurrency(+ _Old_, - _New_) 


Determine the concurrency of the process, which is defined as the
maximum number of concurrently active threads. `Active' here means
they are using CPU time. This option is provided if the
thread-implementation provides
`pthread_setconcurrency()`. Solaris is a typical example of this
family. On other systems this predicate unifies  _Old_ to 0 (zero)
and succeeds silently.

 
*/

/** @pred thread_sleep(+ _Time_) 


Make current thread sleep for  _Time_ seconds.  _Time_ may be an
integer or a floating point number. When time is zero or a negative value 
the call succeeds and returns immediately. This call should not be used if
alarms are also being used.



@} */

/** @defgroup Monitoring_Threads Monitoring Threads
@ingroup Threads
@{

Normal multi-threaded applications should not need these the predicates
from this section because almost any usage of these predicates is
unsafe. For example checking the existence of a thread before signalling
it is of no use as it may vanish between the two calls. Catching
exceptions using catch/3 is the only safe way to deal with
thread-existence errors.

These predicates are provided for diagnosis and monitoring tasks.

 
*/

/** @pred thread_property(? _Id_, ? _Property_) 


Enumerates the properties of the specified thread.
Calling thread_property/2 does not influence any thread.  See also
thread_join/2.  For threads that have an alias-name, this name can
be used in  _Id_ instead of the numerical thread identifier.
 _Property_ is one of:

    + status( _Status_)
The thread status of a thread (see below).

    + alias( _Alias_)
The thread alias, if it exists.

    + at_exit( _AtExit_)
The thread exit hook, if defined (not available if the thread is already terminated).

    + detached( _Boolean_)
The detached state of the thread.

    + stack( _Size_)
The thread stack data-area size.

    + trail( _Size_)
The thread trail data-area size.

    + system( _Size_)
The thread system data-area size.


 
*/

/** @pred current_thread(+ _Id_, - _Status_) 


Enumerates identifiers and status of all currently known threads.
Calling current_thread/2 does not influence any thread.  See also
thread_join/2.  For threads that have an alias-name, this name is
returned in  _Id_ instead of the numerical thread identifier.
 _Status_ is one of:

    + running
The thread is running.  This is the initial status of a thread.  Please
note that threads waiting for something are considered running too.

    + false
The  _Goal_ of the thread has been completed and failed.

    + true
The  _Goal_ of the thread has been completed and succeeded.

    + exited( _Term_)
The  _Goal_ of the thread has been terminated using thread_exit/1
with  _Term_ as argument.  If the underlying native thread has
exited (using pthread_exit())  _Term_ is unbound.

    + exception( _Term_)
The  _Goal_ of the thread has been terminated due to an uncaught
exception (see throw/1 and catch/3).


 
*/

/** @pred thread_statistics(+ _Id_, + _Key_, - _Value_) 


Obtains statistical information on thread  _Id_ as `statistics/2`
does in single-threaded applications.  This call returns all keys
of `statistics/2`, although only information statistics about the
stacks and CPU time yield different values for each thread.

    + mutex_statistics 


Print usage statistics on internal mutexes and mutexes associated
with dynamic predicates.  For each mutex two numbers are printed:
the number of times the mutex was acquired and the number of
collisions: the number times the calling thread has to
wait for the mutex.  The collision-count is not available on
Windows as this would break portability to Windows-95/98/ME or
significantly harm performance.  Generally collision count is
close to zero on single-CPU hardware.

    + threads 


Prints a table of current threads and their status.



@} */

/** @defgroup Thread_Communication Thread communication
@ingroup Threads
@{

Prolog threads can exchange data using dynamic predicates, database
records, and other globally shared data. These provide no suitable means
to wait for data or a condition as they can only be checked in an
expensive polling loop. <em>Message queues</em> provide a means for
threads to wait for data or conditions without using the CPU.

Each thread has a message-queue attached to it that is identified
by the thread. Additional queues are created using
`message_queue_create/2`.



 @pred thread_send_message(+ _Term_) 


Places  _Term_ in the message-queue of the thread running the goal. 
Any term can be placed in a message queue, but note that the term is 
copied to the receiving thread and variable-bindings are thus lost. 
This call returns immediately.

 
*/

/** @pred thread_send_message(+ _QueueOrThreadId_, + _Term_)

Place  _Term_ in the given queue or default queue of the indicated
thread (which can even be the message queue of itself (see
thread_self/1). Any term can be placed in a message queue, but note that
the term is copied to the receiving thread and variable-bindings are
thus lost. This call returns immediately.

If more than one thread is waiting for messages on the given queue and
at least one of these is waiting with a partially instantiated
 _Term_, the waiting threads are <em>all</em> sent a wakeup signal,
starting a rush for the available messages in the queue.  This behaviour
can seriously harm performance with many threads waiting on the same
queue as all-but-the-winner perform a useless scan of the queue. If
there is only one waiting thread or all waiting threads wait with an
unbound variable an arbitrary thread is restarted to scan the queue.




 
*/

/** @pred thread_get_message(? _Term_) 


Examines the thread message-queue and if necessary blocks execution
until a term that unifies to  _Term_ arrives in the queue.  After
a term from the queue has been unified unified to  _Term_, the
term is deleted from the queue and this predicate returns.

Please note that not-unifying messages remain in the queue.  After
the following has been executed, thread 1 has the term `gnu`
in its queue and continues execution using  _A_ is `gnat`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   <thread 1>
   thread_get_message(a(A)),

   <thread 2>
   thread_send_message(b(gnu)),
   thread_send_message(a(gnat)),
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See also thread_peek_message/1.

 
*/

/** @pred message_queue_create(? _Queue_) 


If  _Queue_ is an atom, create a named queue.  To avoid ambiguity
on `thread_send_message/2`, the name of a queue may not be in use
as a thread-name.  If  _Queue_ is unbound an anonymous queue is
created and  _Queue_ is unified to its identifier.

 
*/

/** @pred message_queue_destroy(+ _Queue_) 


Destroy a message queue created with message_queue_create/1.  It is
<em>not</em> allows to destroy the queue of a thread.  Neither is it
allowed to destroy a queue other threads are waiting for or, for
anonymous message queues, may try to wait for later.

 
*/

/** @pred thread_get_message(+ _Queue_, ? _Term_)

As thread_get_message/1, operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.

 
*/

/** @pred thread_peek_message(? _Term_) 


Examines the thread message-queue and compares the queued terms
with  _Term_ until one unifies or the end of the queue has been
reached.  In the first case the call succeeds (possibly instantiating
 _Term_.  If no term from the queue unifies this call fails.

 
*/

/** @pred thread_peek_message(+ _Queue_, ? _Term_)

As thread_peek_message/1, operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.



Explicit message queues are designed with the <em>worker-pool</em> model
in mind, where multiple threads wait on a single queue and pick up the
first goal to execute.  Below is a simple implementation where the
workers execute arbitrary Prolog goals.  Note that this example provides
no means to tell when all work is done. This must be realised using
additional synchronisation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%    create_workers(+Id, +N)
%    
%    Create a pool with given Id and number of workers.

create_workers(Id, N) :-
    message_queue_create(Id),
    forall(between(1, N, _),
           thread_create(do_work(Id), _, [])).

do_work(Id) :-
    repeat,
      thread_get_message(Id, Goal),
      (   catch(Goal, E, print_message(error, E))
      ->  true
      ;   print_message(error, goal_failed(Goal, worker(Id)))
      ),
    fail.

%    work(+Id, +Goal)
%    
%    Post work to be done by the pool

work(Id, Goal) :-
    thread_send_message(Id, Goal).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


@} */

/** @defgroup Signalling_Threads Signalling Threads
@ingroup Threadas
@{

These predicates provide a mechanism to make another thread execute some
goal as an <em>interrupt</em>.  Signalling threads is safe as these
interrupts are only checked at safe points in the virtual machine.
Nevertheless, signalling in multi-threaded environments should be
handled with care as the receiving thread may hold a <em>mutex</em>
(see with_mutex/2).  Signalling probably only makes sense to start
debugging threads and to cancel no-longer-needed threads with throw/1,
where the receiving thread should be designed carefully do handle
exceptions at any point.

 
*/

/** @pred thread_signal(+ _ThreadId_, : _Goal_) 


Make thread  _ThreadId_ execute  _Goal_ at the first
opportunity.  In the current implementation, this implies at the first
pass through the <em>Call-port</em>. The predicate thread_signal/2
itself places  _Goal_ into the signalled-thread's signal queue
and returns immediately.

Signals (interrupts) do not cooperate well with the world of
multi-threading, mainly because the status of mutexes cannot be
guaranteed easily.  At the call-port, the Prolog virtual machine
holds no locks and therefore the asynchronous execution is safe.

 _Goal_ can be any valid Prolog goal, including throw/1 to make
the receiving thread generate an exception and trace/0 to start
tracing the receiving thread.




@} */

/** @defgroup Threads_and_Dynamic_Predicates Threads and Dynamic Predicates
@ingroup Threads
@{

Besides queues threads can share and exchange data using dynamic
predicates. The multi-threaded version knows about two types of
dynamic predicates. By default, a predicate declared <em>dynamic</em>
(see dynamic/1) is shared by all threads. Each thread may
assert, retract and run the dynamic predicate. Synchronisation inside
Prolog guarantees the consistency of the predicate. Updates are
<em>logical</em>: visible clauses are not affected by assert/retract
after a query started on the predicate. In many cases primitive from
thread synchronisation should be used to ensure application invariants on
the predicate are maintained.

Besides shared predicates, dynamic predicates can be declared with the
thread_local/1 directive. Such predicates share their
attributes, but the clause-list is different in each thread.

 
*/

/** @pred thread_local( _+Functor/Arity_)  


related to the dynamic/1 directive.  It tells the system that the
predicate may be modified using assert/1, retract/1,
etc, during execution of the program.  Unlike normal shared dynamic
data however each thread has its own clause-list for the predicate.
As a thread starts, this clause list is empty.  If there are still
clauses as the thread terminates these are automatically reclaimed by
the system.  The `thread_local` property implies
the property `dynamic`.

Thread-local dynamic predicates are intended for maintaining
thread-specific state or intermediate results of a computation.

It is not recommended to put clauses for a thread-local predicate into
a file as in the example below as the clause is only visible from the
thread that loaded the source-file.  All other threads start with an
empty clause-list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- thread_local
    foo/1.

foo(gnat).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup Thread_Synchronisation Thread Synchronisation

All@{
 internal Prolog operations are thread-safe. This implies two Prolog
threads can operate on the same dynamic predicate without corrupting the
consistency of the predicate. This section deals with user-level
<em>mutexes</em> (called <em>monitors</em> in ADA or
<em>critical-sections</em> by Microsoft).  A mutex is a
<em>MUT</em>ual <em>EX</em>clusive device, which implies at most one thread
can <em>hold</em> a mutex.

Mutexes are used to realise related updates to the Prolog database.
With `related', we refer to the situation where a `transaction' implies
two or more changes to the Prolog database.  For example, we have a
predicate `address/2`, representing the address of a person and we want
to change the address by retracting the old and asserting the new
address.  Between these two operations the database is invalid: this
person has either no address or two addresses, depending on the
assert/retract order.

Here is how to realise a correct update:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- initialization
    mutex_create(addressbook).

change_address(Id, Address) :-
    mutex_lock(addressbook),
    retractall(address(Id, _)),
    asserta(address(Id, Address)),
    mutex_unlock(addressbook).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred mutex_create(? _MutexId_) 


Create a mutex.  if  _MutexId_ is an atom, a <em>named</em> mutex is
created.  If it is a variable, an anonymous mutex reference is returned.
There is no limit to the number of mutexes that can be created.

 
*/

/** @pred mutex_destroy(+ _MutexId_) 


Destroy a mutex.  After this call,  _MutexId_ becomes invalid and
further references yield an `existence_error` exception.

 
*/

/** @pred with_mutex(+ _MutexId_, : _Goal_) 


Execute  _Goal_ while holding  _MutexId_.  If  _Goal_ leaves
choicepoints, these are destroyed (as in once/1).  The mutex is unlocked
regardless of whether  _Goal_ succeeds, fails or raises an exception.
An exception thrown by  _Goal_ is re-thrown after the mutex has been
successfully unlocked.  See also `mutex_create/2`.

Although described in the thread-section, this predicate is also
available in the single-threaded version, where it behaves simply as
once/1.

 
*/

/** @pred mutex_lock(+ _MutexId_) 


Lock the mutex.  Prolog mutexes are <em>recursive</em> mutexes: they
can be locked multiple times by the same thread.  Only after unlocking
it as many times as it is locked, the mutex becomes available for
locking by other threads. If another thread has locked the mutex the
calling thread is suspended until to mutex is unlocked.

If  _MutexId_ is an atom, and there is no current mutex with that
name, the mutex is created automatically using mutex_create/1.  This
implies named mutexes need not be declared explicitly.

Please note that locking and unlocking mutexes should be paired
carefully. Especially make sure to unlock mutexes even if the protected
code fails or raises an exception. For most common cases use
with_mutex/2, which provides a safer way for handling Prolog-level
mutexes.

 
*/

/** @pred mutex_trylock(+ _MutexId_) 


As mutex_lock/1, but if the mutex is held by another thread, this
predicates fails immediately.

 
*/

/** @pred mutex_unlock(+ _MutexId_) 


Unlock the mutex. This can only be called if the mutex is held by the
calling thread. If this is not the case, a `permission_error`
exception is raised.

 
*/

/** @pred mutex_unlock_all 


Unlock all mutexes held by the current thread.  This call is especially
useful to handle thread-termination using abort/0 or exceptions.  See
also thread_signal/2.

 
*/

/** @pred current_mutex(? _MutexId_, ? _ThreadId_, ? _Count_) 


Enumerates all existing mutexes.  If the mutex is held by some thread,
 _ThreadId_ is unified with the identifier of the holding thread and
 _Count_ with the recursive count of the mutex. Otherwise,
 _ThreadId_ is `[]` and  _Count_ is 0.



@} */

/** @defgroup Parallelism Parallelism
@ingroup YAPPackages
@{

There has been a sizeable amount of work on an or-parallel
implementation for YAP, called  *YAPOr*. Most of this work has
been performed by Ricardo Rocha. In this system parallelism is exploited
implicitly by running several alternatives in or-parallel. This option
can be enabled from the `configure` script or by checking the
system's `Makefile`.

 *YAPOr* is still a very experimental system, going through rapid
development. The following restrictions are of note:

    + *YAPOr* currently only supports the Linux/X86 and SPARC/Solaris
platforms. Porting to other Unix-like platforms should be straightforward.

    + *YAPOr* does not support parallel updates to the
data-base.

    + *YAPOr* does not support opening or closing of streams during
parallel execution.

    + Garbage collection and stack shifting are not supported in
 *YAPOr*.  

    + Built-ins that cause side-effects can only be executed when
left-most in the search-tree. There are no primitives to provide
asynchronous or cavalier execution of these built-ins, as in Aurora or
Muse.

    + YAP does not support voluntary suspension of work.


We expect that some of these restrictions will be removed in future
releases.


@} */

/** @defgroup Tabling Tabling
@ingroup YAPBuiltins
@{

 *YAPTab* is the tabling engine that extends YAP's execution
model to support tabled evaluation for definite programs. YAPTab was
implemented by Ricardo Rocha and its implementation is largely based
on the ground-breaking design of the XSB Prolog system, which
implements the SLG-WAM. Tables are implemented using tries and YAPTab
supports the dynamic intermixing of batched scheduling and local
scheduling at the subgoal level. Currently, the following restrictions
are of note:

    + YAPTab does not handle tabled predicates with loops through negation (undefined behaviour).
    + YAPTab does not handle tabled predicates with cuts (undefined behaviour).
    + YAPTab does not support coroutining (configure error).
    + YAPTab does not support tabling dynamic predicates (permission error).


To experiment with YAPTab use `--enable-tabling` in the configure
script or add `-DTABLING` to `YAP_EXTRAS` in the system's
`Makefile`. We next describe the set of built-ins predicates
designed to interact with YAPTab and control tabled execution:

 
*/

/** @pred table + _P_ 


Declares predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]) as a tabled
predicate.  _P_ must be written in the form
 _name/arity_. Examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table son/3.
:- table father/2.
:- table mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 or

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table son/3, father/2, mother/2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 or

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- table [son/3, father/2, mother/2].
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
*/

/** @pred is_tabled(+ _P_) 


Succeeds if the predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]), of the form
 _name/arity_, is a tabled predicate.

 
*/

/** @pred tabling_mode(+ _P_,? _Mode_) 


Sets or reads the default tabling mode for a tabled predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The list of  _Mode_ options includes:

    + batched
Defines that, by default, batched scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.
    + local
Defines that, by default, local scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.
    + exec_answers
Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained by executing
compiled WAM-like code directly from the trie data
structure. This reduces the loading time when backtracking, but
the order in which answers are obtained is undefined.
    + load_answers
Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained (as a
consumer) by loading them from the trie data structure. This
guarantees that answers are obtained in the same order as they
were found. Somewhat less efficient but creates less choice-points.

The default tabling mode for a new tabled predicate is `batched`
and `exec_answers`. To set the tabling mode for all predicates at
once you can use the yap_flag/2 predicate as described next.

 
*/

/** @pred yap_flag(tabling_mode,? _Mode_)
Sets or reads the tabling mode for all tabled predicates. The list of
 _Mode_ options includes:

    + default
Defines that (i) all calls to tabled predicates are evaluated
using the predicate default mode, and that (ii) answers for all
completed calls are obtained by using the predicate default mode.
    + batched
Defines that all calls to tabled predicates are evaluated using
batched scheduling. This option ignores the default tabling mode
of each predicate.
    + local
Defines that all calls to tabled predicates are evaluated using
local scheduling. This option ignores the default tabling mode
of each predicate.
    + exec_answers
Defines that answers for all completed calls are obtained by
executing compiled WAM-like code directly from the trie data
structure. This option ignores the default tabling mode
of each predicate.
    + load_answers
Defines that answers for all completed calls are obtained by
loading them from the trie data structure. This option ignores
the default tabling mode of each predicate.


 
*/

/** @pred abolish_table(+ _P_) 


Removes all the entries from the table space for predicate  _P_ (or
a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The predicate remains as a tabled predicate.

 
*/

/** @pred abolish_all_tables/0 


Removes all the entries from the table space for all tabled
predicates. The predicates remain as tabled predicates.

 
*/

/** @pred show_table(+ _P_) 


Prints table contents (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

 
*/

/** @pred table_statistics(+ _P_) 


Prints table statistics (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

 
*/

/** @pred tabling_statistics/0 


Prints statistics on space used by all tables.



@} */

/** @defgroup Low_Level_Tracing Tracing at Low Level
@ingroup YAPBuiltins
@{

It is possible to follow the flow at abstract machine level if
YAP is compiled with the flag `LOW_LEVEL_TRACER`. Note
that this option is of most interest to implementers, as it quickly generates
an huge amount of information.

Low level tracing can be toggled from an interrupt handler by using the
option `T`. There are also two built-ins that activate and
deactivate low level tracing:

 
*/

/** @pred start_low_level_trace 


Begin display of messages at procedure entry and retry.

    + stop_low_level_trace 


Stop display of messages at procedure entry and retry.


Note that this compile-time option will slow down execution.


@} */

/** @defgroup Low_Level_Profiling Profiling the Abstract Machine

Imp@{
lementors may be interested in detecting on which abstract machine
instructions are executed by a program. The `ANALYST` flag can give
WAM level information. Note that this option slows down execution very
substantially, and is only of interest to developers of the system
internals, or to system debuggers.

 
*/

/** @pred reset_op_counters 


Reinitialize all counters.

 
*/

/** @pred show_op_counters(+ _A_) 


Display the current value for the counters, using label  _A_. The
label must be an atom.

 
*/

/** @pred show_ops_by_group(+ _A_) 


Display the current value for the counters, organized by groups, using
label  _A_. The label must be an atom.




@} */

/** @defgroup Debugging Debugging
@ingroup YAPBuiltins
@{


@} */

/** @defgroup Deb_Preds Debugging Predicates

The@{
 following predicates are available to control the debugging of
programs:

    + debug

Switches the debugger on.

    + debugging 


Outputs status information about the debugger which includes the leash
mode and the existing spy-points, when the debugger is on.

    + nodebug 


Switches the debugger off.

 
*/

/** @pred spy + _P_ 


Sets spy-points on all the predicates represented by
 _P_.  _P_ can either be a single specification or a list of 
specifications. Each one must be of the form  _Name/Arity_ 
or  _Name_. In the last case all predicates with the name 
 _Name_ will be spied. As in C-Prolog, system predicates and 
predicates written in C, cannot be spied.

 
*/

/** @pred nospy + _P_ 


Removes spy-points from all predicates specified by  _P_.
The possible forms for  _P_ are the same as in `spy P`.

 
*/

/** @pred nospyall 


Removes all existing spy-points.

 
*/

/** @pred leash(+ _M_) 


Sets leashing mode to  _M_.
The mode can be specified as:

    + full
prompt on Call, Exit, Redo and Fail
    + tight
prompt on Call, Redo and Fail
    + half
prompt on Call and Redo
    + loose
prompt on Call
    + off
never prompt
    + none
never prompt, same as `off`

The initial leashing mode is `full`.

The user may also specify directly the debugger ports 
where he wants to be prompted. If the argument for leash 
is a number  _N_, each of lower four bits of the number is used to
control prompting at one the ports of the box model. The debugger will 
prompt according to the following conditions:

    + 
if `N/\\ 1 =\\= 0`  prompt on fail 
    + 
if `N/\\ 2 =\\= 0` prompt on redo
    + 
if `N/\\ 4 =\\= 0` prompt on exit
    + 
if `N/\\ 8 =\\= 0` prompt on call

Therefore, `leash(15)` is equivalent to `leash(full)` and
`leash(0)` is equivalent to `leash(off)`.

Another way of using `leash` is to give it a list with the names of
the ports where the debugger should stop. For example,
`leash([call,exit,redo,fail])` is the same as `leash(full)` or
`leash(15)` and `leash([fail])` might be used instead of
`leash(1)`.

 
*/

/** @pred spy_write(+ _Stream_,Term) 


If defined by the user, this predicate will be used to print goals by
the debugger instead of `write/2`.

 
*/

/** @pred trace 


Switches on the debugger and starts tracing.

 
*/

/** @pred notrace 


Ends tracing and exits the debugger. This is the same as
nodebug/0.




@} */

/** @defgroup Deb_Interaction Interacting with the debugger

Deb@{
ugging with YAP is similar to debugging with C-Prolog. Both systems
include a procedural debugger, based on Byrd's four port model. In this
model, execution is seen at the procedure level: each activation of a
procedure is seen as a box with control flowing into and out of that
box.

In the four port model control is caught at four key points: before 
entering the procedure, after exiting the procedure (meaning successful 
evaluation of all queries activated by the procedure), after backtracking but 
before trying new alternative to the procedure and after failing the 
procedure. Each one of these points is named a port:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@group
           *--------------------------------------*
   Call    |                                      |    Exit
---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
           |                                      |
           |  descendant(X,Z) :-                  |
<--------- +     offspring(X,Y), descendant(Y,Z). + <---------
   Fail    |                                      |    Redo
           *--------------------------------------*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    + Call
The call port is activated before initial invocation of
procedure. Afterwards, execution will try to match the goal with the
head of existing clauses for the procedure.
    + Exit
This port is activated if the procedure succeeds.
Control will  now leave the procedure and return to its ancestor.
    + Redo
if the goal, or goals, activated after the call port
fail  then backtracking will eventually return control to this procedure
through  the redo port.
    + Fail
If all clauses for this predicate fail, then the
invocation fails,  and control will try to redo the ancestor of this
invocation.


To start debugging, the user will either call `trace` or spy the
relevant procedures, entering debug mode, and start execution of the
program. When finding the first spy-point, YAP's debugger will take
control and show a message of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* (1)  call:  quicksort([1,2,3],_38) ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The debugger message will be shown while creeping, or at spy-points,
and it includes four or five fields:

    + 
The first three characters are used to point out special states of the
debugger. If the port is exit and the first character is '?', the
current call is non-deterministic, that is, it still has alternatives to
be tried. If the second character is a `\*`, execution is at a
spy-point. If the third character is a `\>`, execution has returned
either from a skip, a fail or a redo command.
    + 
The second field is the activation number, and uniquely identifies the
activation. The number will start from 1 and will be incremented for
each activation found by the debugger.
    + 
In the third field, the debugger shows the active port.
    + 
The fourth field is the goal. The goal is written by
`write_term/3` on the standard error stream, using the options
given by debugger_print_options.


If the active port is leashed, the debugger will prompt the user with a
`?`, and wait for a command. A debugger command is just a
character, followed by a return. By default, only the call and redo
entries are leashed, but the leash/1 predicate can be used in
order to make the debugger stop where needed.

There are several commands available, but the user only needs to 
remember the help command, which is `h`. This command shows all the 
available options, which are:

    + c - creep
this command makes YAP continue execution and stop at the next
leashed port.
    + return - creep
the same as c
    + l - leap
YAP will execute until it meets a port for a spied predicate; this mode
keeps all computation history for debugging purposes, so it is more
expensive than standard execution. Use <tt>k</tt> or <tt>z</tt> for fast execution.
    + k - quasi-leap
similar to leap but faster since the computation history is
not kept; useful when leap becomes too slow.
    + z - zip
same as <tt>k</tt>
    + s - skip
YAP will continue execution without showing any messages until
returning to the current activation. Spy-points will be  ignored in this
mode. Note that this command keeps all debugging history, use <tt>t</tt> for fast execution. This command is meaningless, and therefore illegal, in the fail
and exit ports.
    + t - fast-skip
similar to skip but faster since computation history is not
kept; useful if skip becomes slow.
    + f [ _GoalId_] - fail
If given no argument, forces YAP to fail the goal, skipping the fail
port and backtracking to the parent. 
If <tt>f</tt> receives a goal number as
the argument, the command fails all the way to the goal. If goal  _GoalId_ has completed execution, YAP fails until meeting the first active ancestor.
    + r [ _GoalId_] - retry
This command forces YAP to jump back call to the port. Note that any
side effects of the goal cannot be undone. This command is not available
at the call port.  If <tt>f</tt> receives a goal number as the argument, the
command retries goal  _GoalId_ instead. If goal  _GoalId_ has
completed execution, YAP fails until meeting the first active ancestor.

    + a - abort
execution will be aborted, and the interpreter will return to the
top-level. YAP disactivates debug mode, but spypoints are not removed.
    + n - nodebug
stop debugging and continue execution. The command will not clear active
spy-points.
    + e - exit
leave YAP.
    + h - help
show the debugger commands.
    + ! Query
execute a query. YAP will not show the result of the query.
    + b - break
break active execution and launch a break level. This is  the same as `!break`.
    + + - spy this goal
start spying the active goal. The same as `! spy  G` where  _G_
is the active goal.
    + - - nospy this goal
stop spying the active goal. The same as `! nospy G` where  _G_ is
the active goal.
    + p - print
shows the active goal using print/1
    + d - display
shows the active goal using display/1
    + \<Depth - debugger write depth
sets the maximum write depth, both for composite terms and lists, that
will be used by the debugger. For more
information about `write_depth/2` ( (see Input/Output Control)).
    + \< - full term
resets to the default of ten the debugger's maximum write depth. For
more information about `write_depth/2` ( (see Input/Output Control)).
    + A - alternatives
show the list of backtrack points in the current execution. 
    + g [ _N_] 
show the list of ancestors in the current debugging environment. If it
receives  _N_, show the first  _N_ ancestors.


The debugging information, when fast-skip `quasi-leap` is used, will
be lost.

*/

/** @page Efficiency Efficiency Considerations

We next discuss several issues on trying to make Prolog programs run
fast in YAP. We assume two different programming styles:

    + Execution of <em>deterministic</em> programs often
boils down to a recursive loop of the form:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loop(Env) :-
        do_something(Env,NewEnv),
        loop(NewEnv).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




@} */

/** @defgroup Indexing Indexing

The@{
 indexation mechanism restricts the set of clauses to be tried in a
procedure by using information about the status of the instantiated
arguments of the goal.  These arguments are then used as a key,
selecting a restricted set of a clauses from all the clauses forming the
procedure.

As an example, the two clauses for concatenate:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
concatenate([],L,L).
concatenate([H|T],A,[H|NT]) :- concatenate(T,A,NT).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the first argument for the goal is a list, then only the second clause 
is of interest. If the first argument is the nil atom, the system needs to 
look only for the first clause. The indexation generates instructions that 
test the value of the first argument, and then proceed to a selected clause, 
or group of clauses.

Note that if the first argument was a free variable, then both clauses 
should be tried. In general, indexation will not be useful if the first 
argument is a free variable.

When activating a predicate, a Prolog system needs to store state 
information. This information, stored in a structure known as choice point 
or fail point, is necessary when backtracking to other clauses for the 
predicate. The operations of creating and using a choice point are very 
expensive, both in the terms of space used and time spent.
Creating a choice point is not necessary if there is only a clause for 
the predicate as there are no clauses to backtrack to. With indexation, this 
situation is extended: in the example, if the first argument was the atom 
nil, then only one clause would really be of interest, and it is pointless to 
create a choice point. This feature is even more useful if the first argument 
is a list: without indexation, execution would try the first clause, creating 
a choice point. The clause would fail, the choice point would then be used to 
restore the previous state of the computation and the second clause would 
be tried. The code generated by the indexation mechanism would behave 
much more efficiently: it would test the first argument and see whether it 
is a list, and then proceed directly to the second clause.

An important side effect concerns the use of "cut". In the above 
example, some programmers would use a "cut" in the first clause just to 
inform the system that the predicate is not backtrackable and force the 
removal the choice point just created. As a result, less space is needed but 
with a great loss in expressive power: the "cut" would prevent some uses of 
the procedure, like generating lists through backtracking. Of course, with 
indexation the "cut" becomes useless: the choice point is not even created.

Indexation is also very important for predicates with a large number 
of clauses that are used like tables:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logician(aristoteles,greek).
logician(frege,german).
logician(russel,english).
logician(godel,german).
logician(whitehead,english).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An interpreter like C-Prolog, trying to answer the query:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?- logician(godel,X).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

would blindly follow the standard Prolog strategy, trying first the
first clause, then the second, the third and finally finding the
relevant clause.  Also, as there are some more clauses after the
important one, a choice point has to be created, even if we know the
next clauses will certainly fail. A "cut" would be needed to prevent
some possible uses for the procedure, like generating all logicians.  In
this situation, the indexing mechanism generates instructions that
implement a search table. In this table, the value of the first argument
would be used as a key for fast search of possibly matching clauses. For
the query of the last example, the result of the search would be just
the fourth clause, and again there would be no need for a choice point.

If the first argument is a complex term, indexation will select clauses
just by testing its main functor. However, there is an important
exception: if the first argument of a clause is a list, the algorithm
also uses the list's head if not a variable. For instance, with the
following clauses,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rules([],B,B).
rules([n(N)|T],I,O) :- rules_for_noun(N,I,N), rules(T,N,O).
rules([v(V)|T],I,O) :- rules_for_verb(V,I,N), rules(T,N,O).
rules([q(Q)|T],I,O) :- rules_for_qualifier(Q,I,N), rules(T,N,O).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if the first argument of the goal is a list, its head will be tested, and only 
the clauses matching it will be tried during execution.

Some advice on how to take a good advantage of this mechanism:



    + 
Try to make the first argument an input argument.

    + 
Try to keep together all clauses whose first argument is not a 
variable, that will decrease the number of tests since the other clauses are 
always tried.

    + 
Try to avoid predicates having a lot of clauses with the same key. 
For instance, the procedure:



~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(n(mary),person).
type(n(john), person).
type(n(chair),object).
type(v(eat),active).
type(v(rest),passive).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

becomes more efficient with:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type(n(N),T) :- type_of_noun(N,T).
type(v(V),T) :- type_of_verb(V,T).

type_of_noun(mary,person).
type_of_noun(john,person).
type_of_noun(chair,object).

type_of_verb(eat,active).
type_of_verb(rest,passive).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

/** @page ChYInterface C Language interface to YAP

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages. Last, a new C++ based interface is 
being designed to work with the swig (@url(www.swig.org}) interface compiler.

    + The @ref c-interface  YAP C-interface exports the YAP engine.
    + The @ref swi-c-interface emulates Jan Wielemaker's SWI foreign language interface.
    + The @ref  yap-cplus-interface is desiged to interface with Object-Oriented systems.




@} */

/** @defgroup Loading_Objects Loading Object Files

The@{
 primitive predicate

 
*/

/** @pred load_foreign_files( _Files_, _Libs_, _InitRoutine_)

should be used, from inside YAP, to load object files produced by the C
compiler. The argument  _ObjectFiles_ should be a list of atoms
specifying the object files to load,  _Libs_ is a list (possibly
empty) of libraries to be passed to the unix loader (`ld`) and
InitRoutine is the name of the C routine (to be called after the files
are loaded) to perform the necessary declarations to YAP of the
predicates defined in the files. 

YAP will search for  _ObjectFiles_ in the current directory first. If
it cannot find them it will search for the files using the environment
variable:

    + YAPLIBDIR

if defined, or in the default library.

YAP also supports the SWI-Prolog interface to loading foreign code:

 
*/

/** @pred open_shared_object(+ _File_, - _Handle_)

File is the name of a shared object file (called dynamic load
library in MS-Windows). This file is attached to the current process
and  _Handle_ is unified with a handle to the library. Equivalent to
`open_shared_object(File, [], Handle)`. See also
load_foreign_library/1 and `load_foreign_library/2`.

On errors, an exception `shared_object`( _Action_,
 _Message_) is raised.  _Message_ is the return value from
dlerror().

 
*/

/** @pred open_shared_object(+ _File_, - _Handle_, + _Options_)

As `open_shared_object/2`, but allows for additional flags to
be passed.  _Options_ is a list of atoms. `now` implies the
symbols are 
resolved immediately rather than lazily (default). `global` implies
symbols of the loaded object are visible while loading other shared
objects (by default they are local). Note that these flags may not
be supported by your operating system. Check the documentation of
`dlopen()` or equivalent on your operating system. Unsupported
flags  are silently ignored. 

 
*/

/** @pred close_shared_object(+ _Handle_) 

Detach the shared object identified by  _Handle_. 

 
*/

/** @pred call_shared_object_function(+ _Handle_, + _Function_) 


Call the named function in the loaded shared library. The function
is called without arguments and the return-value is
ignored. In SWI-Prolog, normally this function installs foreign
language predicates using calls to `PL_register_foreign()`.



@} */

/** @defgroup SavebQeERest Saving and Restoring

YAP@{
4 currently does not support `save` and `restore` for object code
loaded with `load_foreign_files/3`. We plan to support save and restore
in future releases of YAP.

*/
