/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		control.yap						 *
* Last rev:     20/08/09						 *
* mods:									 *
* comments:	control predicates available in yap			 *
*									 *
*************************************************************************/

/**
 * @file   control.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:26:35 2015
 *
 * @brief  Control Predicates
 *
 *
*/

:- system_module_( '$_control', [at_halt/1,
        b_getval/2,
        break/0,
        call/2,
        call/3,
        call/4,
        call/5,
        call/6,
        call/7,
        call/8,
        call/9,
        call/10,
        call/11,
        call/12,
        call_cleanup/2,
        call_cleanup/3,
        forall/2,
        garbage_collect/0,
        garbage_collect_atoms/0,
        gc/0,
        grow_heap/1,
        grow_stack/1,
        halt/0,
        halt/1,
        if/3,
        ignore/1,
        nb_getval/2,
        nogc/0,
        notrace/1,
        once/1,
        prolog_current_frame/1,
        prolog_initialization/1,
        setup_call_catcher_cleanup/4,
        setup_call_cleanup/3,
        version/0,
        version/1], ['$run_atom_goal'/1,
        '$set_toplevel_hook'/1]).

:- use_system_module( '$_boot', ['$call'/4,
        '$disable_debugging'/0,
        '$do_live'/0,
        '$enable_debugging'/0,
        '$version'/0]).

:- use_system_module( '$_debug', ['$init_debugger'/0]).


:- use_system_module( '$_utils', ['$getval_exception'/3]).

:- use_system_module( '$coroutining', [freeze_goal/2]).

/**


@defgroup YAPControl
@ingroup Builtins

@{

*/


/** @infixpred  0:P , 0:Q   is iso, meta

The comma represents the Conjunction of goals (and).

The conjunction is a fundamental construct of Prolog. Example:

```
 p(X) :- q(X), r(X).
```

should be read as `p( _X_) if q( _X_) and r( _X_).


*/
','(X,Y) :-
    call((X,Y)).
    
comma(X,Y) :-
    call(X),
    call(Y).

comma(X,Y,Z) :-
    call(X),
    call(Y),
    call(Z).
    
comma(X,Y,Z,A) :-
    call(X),
    call(Y),
    call(Z),
    call(A).
     
comma(X,Y,Z,A,B) :-
    call(X),
    call(Y),
    call(Z),
    call(A),
    call(B).
    
comma(X,Y,Z,A,B,C) :-
    call(X),
    call(Y),
    call(Z),
    call(A),
    call(B),
    call(C).

    /** @infixpred    0:P ; 0:Q  is iso

@brief Disjunction of goals (or).

Example:

```
 p(X) :- q(X); r(X).
```
should be read as "p( _X_) if q( _X_) or r( _X_)".


*/
';'(X,Y) :-
    current_choice_point(CP),
    current_source_module(M),
    (
	X=(A->B)
	      ->
	      (
		  call(A)
	      ->
	      '$call'(B,CP,(X;Y),M)
    ;
    '$call'(Y,CP,(X;Y),M)
	      )
    ; 	X=(A*->B)
	      ->
	      (
		  call(A)
	      *->
	      '$call'(B,CP,(X;Y),M) ;
		  '$call'(Y,CP,(X; Y),M)
		  )
    ;
    '$call'(X,CP,(X; Y),M)
    ;
    '$call'(Y,CP,(X; Y),M)
	      ).




'|'(X,Y) :- ';'(X,Y).

/** @infixpred   0:Condition -> 0:Action  is iso

@short If _Condition__ has a solution, call _Action_;

@long
Read as "if-then-else" or "commit". This operator is similar to the
conditional operator of imperative languages and can be used alone or
with an else part as follows:


```
    +P -> +Q
```

"if P then Q".


```
  +P -> +Q; +R
```

"if P then Q else R".

These two predicates could be defined respectively in Prolog as:

```
 (P -> Q) :- P, !, Q.
```
and

```
 (P -> Q; R) :- P, !, Q.
 (P -> Q; R) :- R.
```
if there were no "cuts" in  _P_,  _Q_ and  _R_.

vNote that the commit operator works by "cutting" any alternative
solutions of  _P_.

Note also that you can use chains of commit operators like:

```
    P -> Q ; R -> S ; T.
```
Note that ->/2 does not affect the scope of cuts in its
arguments.


*/
'->'(X,Y) :-
    (
	'$execute'(X)
    ->
    '$execute'(Y)
    ).



/** @pred  ! is iso


Read as "cut". Cuts any choices taken in the current procedure.
When first found "cut" succeeds as a goal, but if backtracking should
later return to it, the parent goal (the one which matches the head of
the clause containing the "cut", causing the clause activation) will
fail. This is an extra-logical predicate and cannot be explained in
terms of the declarative semantics of Prolog.

example:

```
 member(X,[X|_]).
 member(X,[_|L]) :- member(X,L).
```

With the above definition

```
 ?- member(X,[1,2,3]).
```

will return each element of the list by backtracking. With the following
definition:

```
 member(X,[X|_]) :- !.
 member(X,[_|L]) :- member(X,L).
```

the same query would return only the first element of the
list, since backtracking could not "pass through" the cut.

*/
! :- true.


/** @pred   \+ 0:P   is iso, meta
Negation by failure.

Goal  _P_ is not provable. The execution of this predicate fails if
and only if the goal  _P_ finitely succeeds. It is not a true logical
negation, which is impossible in standard Prolog, but
"negation-by-failure".

This predicate might be defined as:

```
 \+(P) :- P, !, fail.
 \+(_).
```
if  _P_ did not include "cuts".

If _P_ includes cuts, the cuts are defined to be scoped by _P_: they cannot cut over the calling prredicate.

 ~~~~~~~~~~~~
  go(P).

:- \+ P, !, fail.
  \+(_).
 ~~~~~~~~~~~~

*/
\+(G) :-     \+ '$execute'(G).

not(G) :-    \+ '$execute'(G).




/**

@pred repeat is iso

Succeeds repeatedly.

In the next example, `repeat` is used as an efficient way to implement
a loop. The next example reads all terms in a file:
```{.prolog}
 a :- repeat, read(X), write(X), nl, X=end_of_file, !.
```
the loop is effectively terminated by the cut-goal, when the test-goal
`X=end` succeeds. While the test fails, the goals `read(X)`,
`write(X)`, and `nl` are executed repeatedly, because
backtracking is caught by the `repeat` goal.

The built-in repeat/0 could be defined in Prolog by:

```{.prolog}

repeat.
repeat :- repeat.
```

The predicate between/3 can be used to iterate for a pre-defined
number of steps.

*/
 repeat :- '$repeat'.

 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat'.
 '$repeat' :- '$repeat'.

/** @pred  + _P_ is nondet

The same as `call( _P_)`. This feature has been kept to provide
compatibility with C-Prolog. When compiling a goal, YAP
generates a `call( _X_)` whenever a variable  _X_ is found as
a goal.

```{.prolog}
 a(X) :- X.
```
is converted to:

```{.prolog}
 a(X) :- call(X).
```


*/

/** @pred  call( 0:P ) is iso
Meta-call predicate.

If _P_ is instantiated to an atom or a compound term, the goal `call(
_P_)` is executed as if the clause was originally written as _P_
instead as call( _P_ ), except that any "cut" occurring in _P_ only
cuts alternatives in the execution of _P_.

Defined as if: 

~~~
call(G) :- '$execute'(G).
~~~
*/

/** @pred  incore( 0:P )

Alias for call/1

*/

/** @pred  once( 0:G) is iso


Execute the goal  _G_ only once. The predicate is defined by:

```{.prolog}
 once(G) :- call(G), !.
```

Note that cuts inside once/1 can only cut the other goals inside
once/1.


*/
once(!) :- !.
once(G) :-
	'$execute'(G), !.


(:- G) :- '$execute'(G), !.

(?- G) :- '$execute'(G).

'$$!'(CP) :- '$cut_by'(CP).

([]).

/** @pred  forall(: _Cond_,: _Action_)


For all alternative bindings of  _Cond_  _Action_ can be
proven. The example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

```{.prolog}
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
```


*/
/** @pred forall(+ _Cond_,+ _Action_)




For all alternative bindings of  _Cond_  _Action_ can be proven.
The next example verifies that all arithmetic statements in the list
 _L_ are correct. It does not say which is wrong if one proves wrong.

```
?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
                 Result =:= Formula).
```



*/
forall(Cond, Action) :- \+((Cond, \+(Action))).

/** @pred  ignore(: _Goal_)


Calls  _Goal_ as once/1, but succeeds, regardless of whether
`Goal` succeeded or not. Defined as:

```{.prolog}
ignore(Goal) :-
        Goal, !.
ignore(_).
```


*/
ignore(Goal) :- (Goal->true;true).

notrace(G) :-
	 strip_module(G, M, G1),
	 ( current_choice_point(CP),
	   '$debug_stop'( State ),
	   '$call'(G1, CP, G, M),
	   current_choice_point(CP2),
	   (CP == CP2 -> ! ; '$debug_state'( NState ), ( true ; '$debug_restart'(NState), fail ) ),
	   '$debug_restart'( State )
     ;
	'$debug_restart'( State ),
	fail
    ).
/** @infixpred  G *-> H

Call goal  _H_ once per each solution of goal  _G_.

The built-in *->3 is usually called from within a disjunction. It
performs similar to ->/3, with the difference that it will backtrack
over the test goal. Consider the following small data-base:

```{.prolog}
a(1).        b(a).          c(x).
a(2).        b(b).          c(y).
```

Execution of an *->/3 query will proceed as follows:

```{.prolog}
   ?- (a(X)->b(Y);c(Z)).

X = 1,
Y = a ? ;

X = 1,
Y = b ? ;

X = 2,
Y = a ? ;

X = 2,
Y = b ? ;

no
```

The system will backtrack over the two solutions for a/1 and the
two solutions for b/1, generating four solutions.

Cuts are allowed inside the first goal  _G_, but they will only prune
over  _G_.

If you want  _G_ to be deterministic you should use if-then-else, as
it is both more efficient and more portable.

*/
(X *-> Y ) :-
  X, Y.


/** @pred  if(? _G_,? _H_,? _I_)

Call goal  _H_ once per each solution of goal  _H_. If goal
 _H_ has no solutions, call goal  _I_.

The built-in if/3 is similar to `->/3`, with the difference
that it will backtrack over the test goal. Consider the following
small data-base:

```{.prolog}
a(1).        b(a).          c(x).
a(2).        b(b).          c(y).
```

Execution of an if/3 query will proceed as follows:

```{.prolog}
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
```

The system will backtrack over the two solutions for a/1 and the
two solutions for `b/1`, generating four solutions.

Cuts are allowed inside the first goal  _G_, but they will only prune
over  _G_.

If you want  _G_ to be deterministic you should use if-then-else, as
it is both more efficient and more portable.

*/
if(X,Y,Z) :-
    (X *-> Y ; Z).

/** @pred  call(+ _Closure_,...,? _Ai_,...) is iso


Meta-call where  _Closure_ is a closure that is converted into a goal by
appending the  _Ai_ additional arguments. The number of arguments varies
between 0 and 10.


*/

/**
   @pred gated_call(0:Setup, 0:Goal, ?Port, 0:Handler)

   This predicate watches over execution of _Goal_
   - First, it calls `once(Setup)`;
   - Next, it executes `call(Goal)`;
   - if `call(Goal)` succeeds deterministically, it unifies _Port_ with `exit` and if unification succeeds calls _Handler_;
   - if `call(Goal)` succeeds not-deterministically, it unifies _Port_ with `answer` and if unification succeeds calls _Handler_;
   - if execution backtracks to _Goal_, it  unifies _Port_ with `redo` and if unification succeeds calls _Handler_;
    - if execution of _Goal_ fails, it  unifies _Port_ with `fail` and if unification succeeds calls _Handler_;
    - if execution of _Goal_ is pruned by an external goal, it  unifies _Port_ with `!` and if unification succeeds calls _Handler_;
    - if execution of _Goal_ raises an exception _E_, it  unifies _Port_ with `exception(E)` and if unification succeeds calls _Handler_;
    - if  _Goal_ has open alternatives that are discared by an exception  _E_, it  unifies _Port_ with `external_exception(E)` and if unification succeeds calls _Handler_.

    
  
   */
   
gated_call(Setup, Goal, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup), 
    Task0 = bottom( true, Catcher, Cleanup, Tag, true, CP0),
    '$tag_cleanup'(CP0, Task0),
    TaskF = top( true, Catcher, Cleanup, Tag, false, CP0),
    strip_module(Goal,M,G),
    '$execute'( M:G ),
    '$cleanup_on_exit'(CP0, TaskF).


/** @pred call_cleanup(: _Goal_, : _CleanUpGoal_)

This is similar to call_cleanup/1 but with an additional
 _CleanUpGoal_ which gets called after  _Goal_ is finished.

*/
call_cleanup(Goal, Cleanup) :-
	gated_call( true , Goal, Catcher, prolog:cleanup_handler(Catcher,open(_),Cleanup)).

call_cleanup(Goal, Catcher, Cleanup) :-
	gated_call( true , Goal, Catcher , prolog:cleanup_handler(Catcher,open(_),Cleanup)).

/** @pred setup_call_cleanup(: _Setup_,: _Goal_, : _CleanUpGoal_)


Calls `(once(Setup), Goal)`. For each sucessful execution of _Setup_,
calling _Goal_, YAP will call the cleanup handler with `ignore(_Cleanup_)`.
The goal is guaranteed to be
called exactly once.  This will happen after _Goal_ completes, either
through failure, deterministic success, commit, or an exception.
_Setup_ will contain the goals that need to be protected from
asynchronous interrupts such as the ones received from
call_with_time_limit/2 or thread_signal/2.  In most uses, _Setup_
will perform temporary side-effects required by _Goal_ that are
finally undone by _Cleanup_.



*/

setup_call_cleanup(Setup,Goal, Cleanup) :-
	gated_call( Setup , Goal, Catcher , prolog:cleanup_handler(Catcher,open(_),Cleanup)).



setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup) :-
	gated_call( Setup , Goal, Catcher , prolog:cleanup_handler(Catcher,open(_),Cleanup)).


prolog:cleanup_handler(Catcher,_Open,Cleanup) :-
    '$is_catcher'(Catcher),
    (Cleanup->true;true).

'$is_catcher'(exit).
'$is_catcher'(fail).
'$is_catcher'(!).
'$is_catcher'(exception(_)).
'$is_catcher'(external_exception(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   catch/throw implementation

% at each catch point I need to know:
% what is ball;
% where was the previous catch
/**

The goal `catch( _Goal_, _Exception_, _Action_)` tries to
execute goal  _Goal_. If during its execution,  _Goal_ throws an
exception  _E'_ and this exception unifies with  _Exception_, the
exception is considered to be caught and  _Action_ is executed. If
the exception  _E'_ does not unify with  _Exception_, control
again throws the exception.

The top-level of YAP maintains a default exception handler that
is responsible to capture uncaught exceptions.


*/

catch(MG,E,G) :-
    '$':catch(MG,E,G,_,_Done).

'$':catch(MG,_E,_G,Marker,Done) :-
    '$marker'(Marker),
    current_choice_point(CP0),
    '$execute0'(MG),
    Done = true,
    current_choice_point(CP),
    (CP == CP0 -> !;true).

'$':catch(MG,E,G,_,_) :-    
    '$drop_exception'(E0,Info),
    nonvar(E0),
    (
	E = E0
    ->
	(
	    strip_module(G,_,error_handler )
	->
	(
	    E = error(K,U)
	->
	%Info = NewInfo,
	'$extend_info'(Info,U, NewInfo),
	error_handler(error,error(K,NewInfo))
	;
	print_message(warning,error(existence_error(error_handler,E),catch(MG,E,G))),
	fail
	)
	;
	call(G)
	)
    ;
    throw(E0)
    ).
    	
'$rm_user_wrapper'(error(user_defined_error(user_defined_error,EW),_),E0) :-
    !,
    '$rm_user_wrapper'(EW,E0).
'$rm_user_wrapper'(error(user_defined_error,EW),E0) :-
    !,
    '$rm_user_wrapper'(EW,E0).
'$rm_user_wrapper'(E,E).

'$extend_info'(Info0, Hints0, Info) :-
    '$mk_info'(Info0, Info),
    '$mk_hints'(Hints0, Hints),
    '$add_hints'(Hints, Info).


'$mk_info'(Info0, Info) :-
    var(Info0),
    !,
    '$new_exception'(Info).
'$mk_info'(Info0, Info) :-
    is_list(Info0),
    !,
    '$new_exception'(Info),
    '$add_hints'(Info0,Info).
'$mk_info'(Info, Info).

'$mk_hints'(Hints0, []) :-
    var(Hints0),
    !.
'$mk_hints'(Hints, Hints) :-
    is_list(Hints),
    !.
'$mk_hints'(Hints0, Hints) :-
    atom(Hints0),
    !,
    (
	atom_to_term(Hints0,Hints),
	Hints  = [_=_|_]
    ;
    atom_to_string(Hints0,Msg),
    Hints = [errorMsg=Msg]
    ),
    !.
'$mk_hints'(Hints0, Hints) :-
    string(Hints0),
    (
	string_to_term(Hints0,Hints),
	Hints  = [_=_|_]
    ;
    Hints = [errorMsg=Hints0]
    ),
    !.
'$mk_hints'(K=V, [K=V]) :-
    !.
'$mk_hints'(Hint0, [errorMsg=Msg]) :-
    term_to_string(Hint0,Msg) .

'$add_hints'([],_).
'$add_hints'([K=V|KVs], exception(Data)) :-
    '$set_exception'(K,Data, V),
    '$add_hints'(KVs,exception(Data)).

/** @pred  call_with_args(+ _Name_,...,? _Ai_,...)


Meta-call where  _Name_ is the name of the procedure to be called and
the  _Ai_ are the arguments. The number of arguments varies between 0
and 10. New code should use `call/N` for better portability.

If  _Name_ is a complex term, then call_with_args/n behaves as
call/n:

```{.prolog}
call(p(X1,...,Xm), Y1,...,Yn) :- p(X1,...,Xm,Y1,...,Yn).
```


*/

%%% Some "dirty" predicates

% Only efective if yap compiled with -DDEBUG
% this predicate shows the code produced by the compiler
'$show_code' :- '$debug'(0'f). %' just make emacs happy

/** @pred  grow_heap(+ _Size_)
Increase heap size  _Size_ kilobytes.


*/
grow_heap(X) :- '$grow_heap'(X).
/** @pred  grow_stack(+ _Size_)


Increase stack size  _Size_ kilobytes


 */
grow_stack(X) :- '$grow_stack'(X).

/** @pred  gc


The goal `gc` enables garbage collection. The same as
`current_prolog_flag(gc,on)`.


*/
gc :-
	current_prolog_flag(gc,true).
/** @pred  nogc


The goal `nogc` disables garbage collection. The same as
`current_prolog_flag(gc,false)`.


*/
nogc :-
	current_prolog_flag(gc,false).


/** @pred  garbage_collect_atoms


The goal `garbage_collect` forces a garbage collection of the atoms
in the data-base. Currently, only atoms are recovered.


*/
garbage_collect_atoms :-
	'$atom_gc'.

'$force_environment_for_gc'.

'$good_list_of_character_codes'(V) :- var(V), !.
'$good_list_of_character_codes'([]).
'$good_list_of_character_codes'([X|L]) :-
	'$good_character_code'(X),
	'$good_list_of_character_codes'(L).

'$good_character_code'(X) :- var(X), !.
'$good_character_code'(X) :- integer(X), X > -2, X < 256.

/** @pred prolog_initialization( _G_)


Add a goal to be executed on system initialization. This is compatible
with SICStus Prolog's initialization/1.


*/
prolog_initialization(G) :- var(G), !,
	throw_error(instantiation_error,initialization(G)).
prolog_initialization(T) :- must_be_callable(T), !,
	'$assert_init'(T).
prolog_initialization(T) :-
	throw_error(type_error(callable,T) ,initialization(T)).

'$assert_init'(T) :- recordz('$startup_goal',T,_), fail.
'$assert_init'(_).

/** @pred version

Write YAP's boot message.


*/
version :-
	'$version_specs'(Specs),
	print_message(informational, version(Specs)).



/** @pred version(- _Message_)

Add a message to be written when yap boots or after aborting. It is not
possible to remove messages.


*/
version(V) :- var(V),  !,
	throw_r(instantiation_error,version(V)).
version(T) :- atom(T), !, '$assert_version'(T).
version(T) :-
	throw_error(type_error(atom,T),version(T)).

'$assert_version'(T) :- recordz('$version',T,_), fail.
'$assert_version'(_).

'$set_toplevel_hook'(_) :-
	recorded('$toplevel_hooks',_,R),
	erase(R),
	fail.
'$set_toplevel_hook'(H) :-
	recorda('$toplevel_hooks',H,_),
	fail.
'$set_toplevel_hook'(_).


/* This is the break predicate,
	it saves the importante data about current streams and
	debugger state */

'$debug_state'(state(Creep, SPYTarget,SpyOn,Trace, Debug, SPY_GN, GList)) :-
	'$init_debugger',
	nb_getval(creep,Creep),
	nb_getval('$spy_on',SpyOn),
	nb_getval('$spy_target',SPYTarget),
	current_prolog_flag(debug, Debug),
	current_prolog_flag(trace, Trace),
	nb_getval('$spy_gn',SPY_GN),
	b_getval('$spy_glist',GList).


'$debug_stop' :-
	 set_prolog_flag(trace,false),
	set_prolog_flag(debug, false),
	nb_setval('$spy_on',true),
	nb_setval('$spy_target',-1),
	b_setval('$spy_gn',0),
	b_setval('$spy_glist',[]                                              ).

 '$debug_restart'(state(Creep, SPYTarget,SpyOn,Trace, Debug, SPY_GN, GList)) :-
	b_setval('$spy_glist',GList),
	b_setval('$spy_gn',SPY_GN),
	nb_setval('$spy_on',SpyOn),
	nb_setval('$spy_target',SPYTarget),
	nb_setval(creep,Creep),
	set_prolog_flag(debug, Debug),
	set_prolog_flag(trace, Trace),
 	'$enable_debugging'.

/** @pred  break


Suspends the execution of the current goal and creates a new execution
level similar to the top level, displaying the following message:

```{.prolog}
 [ Break (level <number>) ]
```
telling the depth of the break level just entered. To return to the
previous level just type the end-of-file character or call the
end_of_file predicate.  This predicate is especially useful during
debugging.


*/
break :-
        '$debug_state'(DState),
	'$break'( true ),
	current_output(OutStream), current_input(InpStream),
	current_prolog_flag(break_level, BL ),
        NBL is BL+1,
	set_prolog_flag(break_level, NBL ),
	format(user_error, '% Break (level ~w)~n', [NBL]),
	live,
	!,
	set_value('$live','$true'),
        '$debug_restart'(DState),
	set_input(InpStream),
	set_output(OutStream),
	set_prolog_flag(break_level, BL ),
	'$break'( false ).


at_halt(G) :-
	recorda('$halt', G, _),
	fail.
at_halt(_).

/** @pred  halt is iso

Halts Prolog, and exits to the calling application. In YAP,
halt/0 returns the exit code `0`.
*/
halt :-
	print_message(informational, halt),
	fail.
halt :-
    halt(0).

/** @pred  halt(+  _I_) is iso

Halts Prolog, and exits to 1the calling application returning the code
given by the integer  _I_.

*/
halt(_V) :-
	recorded('$halt', G, _),
	catch(once(G), _Error, error_handler),
	fail.
halt(X) :-
	'$sync_mmapped_arrays',
	set_value('$live','$false'),
	'$halt'(X).

prolog_current_frame(Env) :-
	Env is '$env'.

'$run_atom_goal'(GA) :-
	'$current_module'(Module),
	atom_to_term(GA, G, _),
	catch(Module:G, _Error,error_handler),
	!.

'$add_dot_to_atom_goal'([],[0'.]) :- !. %'
'$add_dot_to_atom_goal'([0'.],[0'.]) :- !.
'$add_dot_to_atom_goal'([C|Gs0],[C|Gs]) :-
    '$add_dot_to_atom_goal'(Gs0,Gs).

/**

@pred call_in_module( +M:G )

   This predicate ensures that both deterministic and non-deterministic execution of the goal $G$ takes place in the context of goal _G_?
**/

:- meta_predicate yap_hacks:call_in_module(0).


   
call_in_module(M:G) :-
    gated_call(
	'$module_boundary'(call, M0, M),
	call(G),
	Event,
	'$module_boundary'(Event,M0,M)
    ).



'$module_boundary'(call, M0, M) :-
    current_source_module(M0, M).
'$module_boundary'(answer, M0, _) :-
    current_source_module(_M, M0).
'$module_boundary'(exit, M0, _) :-
    current_source_module(_M, M0).
'$module_boundary'(redo, M0, _M) :-
    current_source_module(_, M0).
'$module_boundary'(fail, M0, _M) :-
    current_source_module(_, M0).
'$module_boundary'(!, _, _).
'$module_boundary'(external_exception(_), _, _).
'$module_boundary'(exception(_), M0, _M) :-
    current_source_module(_, M0).


:-  meta_predicate(call_nth(0,?)).

/**
 * @pred call_nth(G,N)
 *
 * Call _G_ unifying _N_ with the number of answers so far.
 *
 * An example is:
 * ```
 * ?- call_nth(between(1,10,N),N).
 * N=1 ;
 * N=2 ;
 * N=3
 * ```
 * when _N is bound:
 * ```
 * ?- call_nth(between(1,6,X),3).
 * X=3
 * ```
 */
call_nth(Goal_0, Nth) :-
   (
     nonvar(Nth)
       ->
       Nth \== 0,
       \+arg(Nth,+ 1,2), % produces all expected errors
       State = count(0,_), % note the extra argument which remains a variable
       call(Goal_0),
       arg(1, State, C1),
       C2 is C1+1,
       (
       Nth == C2
   ->
 !
   ;
  nb_setarg(1, State, C2),
	 fail
   )
   ;
       State = count(0,_), % note the extra argument which remains a variable
       Goal_0,
       arg(1, State, C1),
       C2 is C1+1,
       nb_setarg(1, State, C2),
       Nth = C2
   ).

 
/**
@}
*/

%% @addtogroup Global_Variables
%% @{

/** @pred  nb_getval(+ _Name_, - _Value_)


The nb_getval/2 predicate is a synonym for b_getval/2,
introduced for compatibility and symmetry. As most scenarios will use
a particular global variable either using non-backtrackable or
backtrackable assignment, using nb_getval/2 can be used to
document that the variable is used non-backtrackable.


*/
/** @pred nb_getval(+ _Name_,- _Value_)


The nb_getval/2 predicate is a synonym for b_getval/2, introduced for
compatibility and symmetry.  As most scenarios will use a particular
global variable either using non-backtrackable or backtrackable
assignment, using nb_getval/2 can be used to document that the
variable is used non-backtrackable.


*/
nb_getval(GlobalVariable, Val) :-
	'__NB_getval__'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, nb_getval(GlobalVariable, Val)) ->
	 nb_getval(GlobalVariable, Val)
	;
	 throw_error(existence_error(variable, GlobalVariable),nb_getval(GlobalVariable, Val))
	).


/** @pred  b_getval(+ _Name_, - _Value_)


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further
instantiate the value of the global variable. If this is undesirable
the normal precautions (double negation or copy_term/2) must be
taken. The b_getval/2 predicate generates errors if  _Name_ is not
an atom or the requested variable does not exist.

Notice that for compatibility with other systems  _Name_ <em>must</em> be already associated with a term: otherwise the system will generate an error.


*/
/** @pred b_getval(+ _Name_,- _Value_)


Get the value associated with the global variable  _Name_ and unify
it with  _Value_. Note that this unification may further instantiate
[the value of the global variable. If this is undesirable the normal
precautions (double negation or copy_term/2) must be taken. The
b_getval/2 predicate generates errors if  _Name_ is not an atom or
the requested variable does not exist.


*/
b_getval(GlobalVariable, Val) :-
	'__NB_getval__'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, b_getval(GlobalVariable, Val)) ->
	 true
	;
	 throw_error(existence_error(variable, GlobalVariable),b_getval(GlobalVariable, Val))
	).

'$getval_exception'(GlobalVariable, _Val, Caller) :-
	user:exception(undefined_global_variable, GlobalVariable, Action),
	!,
	(
	 Action == fail
	->
	 fail
	;
	 Action == retry
	->
	 true
	;
	 Action == error
	->
	 throw_error(existence_error(variable, GlobalVariable),Caller)
	;
	 throw_error(type_error(atom, Action),Caller)
	).


%% @}

