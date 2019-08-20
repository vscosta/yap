 /*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 *************************************************************************/

/**
 * @file pl/os.yap
 */
:- system_module( '$os', [
	       cd/0,
	       cd/1,
	       getcwd/1,
	       ls/0,
	       pwd/0,
	       unix/1,
	       putenv/2,
	       getenv/2,
	       setenv/2
	 ], [] ).
:- use_system_module( '$_errors', ['$do_error'/2]).

/**
@defgroup YAPOS Access to Operating System Functionality
@ingroup builtins

@{

The following built-in predicates allow access to underlying
Operating System functionality. Extra functionality is available fro the
system library.

 */

/** @pred  cd
 *
 * Changes the current directory (on UNIX environments) to the user's home directory.
 *
 * The home directory is obtained from
 * `HOME` environment variable.
*/

cd :-
	cd('~').

/** @pred  cd(+ _D_)


Changes the current directory (on UNIX environments).


*/
cd(F) :-
      absolute_file_name(F, Dir, [file_type(directory),file_errors(fail),access(execute),expand(true)]),
      working_directory(_, Dir).

/** @pred  getcwd(- _D_)


Unify the current directory, represented as an atom, with the argument
 _D_.


*/
getcwd(Dir) :- working_directory(Dir, Dir).

/** @pred  ls


Prints a list of all files in the current directory.


*/
ls :-
	getcwd(X),
	list_directory(X, L),
	'$do_print_files'(L).

'$do_print_files'([]) :-
	nl.
'$do_print_files'([F| Fs]) :-
	'$do_print_file'(F),
	'$do_print_files'(Fs).

'$do_print_file'('.') :- !.
'$do_print_file'('..') :- !.
'$do_print_file'(F) :- atom_concat('.', _, F), !.
'$do_print_file'(F) :-
	write(F), write('  ').

/** @pred  pwd


Prints the current directory.


*/
pwd :-
	getcwd(X),
	write(X), nl.

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
unix(V) :- var(V), !,
	'$do_error'(instantiation_error,unix(V)).
unix(argv(L)) :-
	current_prolog_flag(argv, L).
unix(cd) :- cd('~').
unix(cd(A)) :- cd(A).
unix(environ(X,Y)) :- '$do_environ'(X,Y).
unix(getcwd(X)) :- getcwd(X).
unix(shell(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(shell(V))).
unix(shell(A)) :- atom(A), !, '$shell'(A).
unix(shell(A)) :- string(A), !, '$shell'(A).
unix(shell(V)) :-
	'$do_error'(type_error(atomic,V),unix(shell(V))).
unix(system(V)) :- var(V), !,
	'$do_error'(instantiation_error,unix(system(V))).
unix(system(A)) :- atom(A), !, system(A).
unix(system(A)) :- string(A), !, system(A).
unix(system(V)) :-
	'$do_error'(type_error(atom,V),unix(system(V))).
unix(shell) :- sh.
unix(putenv(X,Y)) :- '$putenv'(X,Y).


'$is_list_of_atoms'(V,_) :- var(V),!.
'$is_list_of_atoms'([],_) :- !.
'$is_list_of_atoms'([H|L],L0) :- !,
	'$check_if_head_may_be_atom'(H,L0),
	'$is_list_of_atoms'(L,L0).
'$is_list_of_atoms'(H,L0) :-
	'$do_error'(type_error(list,H),unix(argv(L0))).

'$check_if_head_may_be_atom'(H,_) :-
	var(H), !.
'$check_if_head_may_be_atom'(H,_) :-
	atom(H), !.
'$check_if_head_may_be_atom'(H,L0) :-
	'$do_error'(type_error(atom,H),unix(argv(L0))).


'$do_environ'(X, Y) :-
	var(X), !,
	'$do_error'(instantiation_error,unix(environ(X,Y))).
'$do_environ'(X, Y) :- atom(X), !,
	'$getenv'(X,Y).
'$do_environ'(X, Y) :-
	'$do_error'(type_error(atom,X),unix(environ(X,Y))).


/** @pred  putenv(+ _E_,+ _S_)


Set environment variable  _E_ to the value  _S_. If the
environment variable  _E_ does not exist, create a new one. Both the
environment variable and the value must be atoms.


*/
putenv(Na,Val) :-
	'$putenv'(Na,Val).

getenv(Na,Val) :-
	'$getenv'(Na,Val).

/** @pred setenv(+ _Name_,+ _Value_)


Set environment variable.   _Name_ and  _Value_ should be
instantiated to atoms or integers.  The environment variable will be
passed to `shell/[0-2]` and can be requested using `getenv/2`.
They also influence expand_file_name/2.


*/
setenv(Na,Val) :-
	'$putenv'(Na,Val).

/**
@}
*/
