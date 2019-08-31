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
* File:		system.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	Operating System Access built-ins			 *
*									 *
*************************************************************************/

/**
 * @file   system.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Nov 18 01:23:45 2015
 *
 *
*/



:- module(operating_system_support,
    [
     datime/1,
     %delete_file/1,
     delete_file/2,
	directory_files/2,
	directory_map/2,
	environ/2,
	exec/3,
     file_exists/2,
	file_property/2,
	host_id/1,
				     host_name/1,
				     kill/1,
				     md5/3,
	pid/1,
				     mktemp/2,
%	make_directory/1,
	popen/3,
     read_link/3,
     rename_file/2,
	shell/0,
	shell/1,
	shell/2,
	system/0,
	system/1,
	system/2,
	mktime/2,
	tmpnam/1,
	tmp_file/2,
    tmpdir/1,
	wait/2,
	working_directory/2
          ]).



/** @defgroup operating_system_support Operating System Functionality
 * @brief Portable Interaction with the OS, be it Unix, Linux, OSX, or Windows.
 *
@ingroup library
@{

YAP  provides a library of system utilities compatible with the
SICStus Prolog system library. This library extends and to some point
complements the functionality of Operating System access routines. The
library includes Unix/Linux and Win32 `C` code. They
are available through the `use_module(library(system))` command.

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

~~~~~
   ?- file_property('Makefile',P).

P = type(regular) ? ;

P = size(2375) ? ;

P = mod_time(990826911) ? ;

no
~~~~~


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
/** @pred mktemp( _Spec_,- _File_)



Direct interface to `mktemp`: given a  _Spec_, that is a file
name with six  _X_ to it, create a file name  _File_. Use
tmpnam/1 instead.


*/
/** @pred mktime(+_Datime_, - _Seconds_)

The `mktime/2` procedure receives a term of the form _datime(+ _Year_,
+ _Month_, + _DayOfTheMonth_, + _Hour_, + _Minute_, + _Second_)_ and
  returns the number of _Seconds_ elapsed since 00:00:00 on January 1,
1970, Coordinated Universal Time (UTC).  The user provides information
on _Year_, _Month_, _DayOfTheMonth_, _Hour_, _Minute_, and
_Second_. The _Hour_ is given on local time. This function uses the
WIN32 `GetLocalTime` function or the Unix `mktime` function.

~~~~~
   ?- mktime(datime(2001,5,28,15,29,46),X).

X = 991081786 ? ;
~~~~~


*/
/** @pred pid(- _Id_)



Unify  _Id_ with the process identifier for the current
process. An interface to the <tt>getpid</tt> function.


*/

/** @pred read_link(+ SymbolicLink, -Link, -NewPath)


Follow a _SymbolicLink_, and obtain the actual _Link_ and the target _newPath_). This predicate uses the
`C` built-in function `readlink` and is not yet operational in WIN32.


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
/** @pred system

Start a new default shell and leave YAP in background until the shell
completes. YAP uses `/bin/sh` in Unix systems and `COMSPEC` in
WIN32.


*/
/** @pred tmp_file(+_Base_, - _File_)

Create a name for a temporary file.  _Base_ is an user provided
identifier for the category of file. The  _TmpName_ is guaranteed to
be unique. If the system halts, it will automatically remove all created
temporary files.


*/
/** @pred tmpnam(- _File_)



Interface with  _tmpnam_: obtain a new, unique file name  _File_.


*/

:- use_module(library(lists), [append/3]).

:- load_foreign_files([sys], [], init_sys).

:- dynamic tmp_file_sequence_counter/1.

% time builtins

/**

 @pred datime(datime(- _Year_, - _Month_, - _DayOfTheMonth_, - _Hour_, - _Minute_, - _Second_)

The datime/1 procedure returns the current date and time, with
information on  _Year_,  _Month_,  _DayOfTheMonth_,
 _Hour_,  _Minute_, and  _Second_. The  _Hour_ is returned
on local time. This function uses the WIN32
`GetLocalTime` function or the Unix `localtime` function.

~~~~~
   ?- datime(X).

X = datime(2001,5,28,15,29,46) ?
~~~~~


*/
datime(X) :-
	datime(X, Error),
	handle_system_internal(Error, off, datime(X)).

mktime(V, A) :- var(V), !,
	throw(error(instantiation_error,mktime(V,A))).
mktime(In,Out) :-
	check_mktime_inp(In, mktime(In,Out)),
	In = datime(Y,Mo,D,H,Mi,S),
	mktime(Y, Mo, D, H, Mi, S, Out, Error),
	handle_system_internal(Error, off, mktime(In,Out)).

check_mktime_inp(V, Inp) :- var(V), !,
	throw(error(instantiation_error,Inp)).
check_mktime_inp(datime(Y,Mo,D,H,Mi,S), Inp) :- !,
	check_int(Y, Inp),
	check_int(Mo, Inp),
	check_int(D, Inp),
	check_int(H, Inp),
	check_int(Mi, Inp),
	check_int(S, Inp).
check_mktime_inp(T, Inp) :-
	throw(error(domain_error(mktime,T),Inp)).

check_int(I, _) :- integer(I), !.
check_int(I, Inp) :- var(I),
	throw(error(instantiation_error,Inp)).
check_int(I, Inp) :-
	throw(error(type_error(integer,I),Inp)).

% file operations
% file operations

/** @pred delete_file(+ _File_,+ _Opts_)

The `delete_file/2` procedure removes file  _File_ according to
options  _Opts_. These options are `directory` if one should
remove directories, `recursive` if one should remove directories
recursively, and `ignore` if errors are not to be reported.

This example is equivalent to using the delete_file/1 predicate:

~~~~~
  ?- delete_file(x, [recursive]).
~~~~~


*/
delete_file(IFile, Opts) :-
	true_file_name(IFile, File),
	process_delete_file_opts(Opts, Dir, Recurse, Ignore, delete_file(File,Opts)),
	delete_file(File, Dir, Recurse, Ignore).

process_delete_file_opts(V, _, _, _, T) :- var(V), !,
	throw(error(instantiation_error,T)).
process_delete_file_opts([], off, off, off, _) :- !.
process_delete_file_opts([V|_], _, _, _, T) :- var(V), !,
	throw(error(instantiation_error,T)).
process_delete_file_opts([directory|Opts], on, Recurse, Ignore, T) :- !,
	process_delete_file_opts(Opts, _, Recurse, Ignore, T).
process_delete_file_opts([recursive|Opts], Dir, on, Ignore, T) :- !,
	process_delete_file_opts(Opts, Dir, _, Ignore, T).
process_delete_file_opts([ignore|Opts], Dir, Recurse, on, T) :- !,
	process_delete_file_opts(Opts, Dir, Recurse, _, T).
process_delete_file_opts(Opts, _, _, _, T) :-
	throw(error(domain_error(delete_file_option,Opts),T)).

delete_file(IFile, Dir, Recurse, Ignore) :-
	true_file_name(IFile, File),
	file_property(File, Type, _, _, _Permissions, _, Ignore),
	delete_file(Type, File, Dir, Recurse, Ignore).

delete_file(N, File, _Dir, _Recurse, Ignore) :- number(N), !, % error.
	handle_system_internal(N, Ignore, delete_file(File)).
delete_file(directory, File, Dir, Recurse, Ignore) :-
	delete_directory(Dir, File, Recurse, Ignore), !.
delete_file(_, File, _Dir, _Recurse, Ignore) :-
	unlink_file(File, Ignore).

unlink_file(IFile, Ignore) :-
	true_file_name(IFile, File),
	unlink(File, N),
	handle_system_internal(N, Ignore, delete_file(File)).

delete_directory(on, File, _Recurse, Ignore) :-
	rm_directory(File, Ignore).
delete_directory(off, File, Recurse, Ignore) :-
	delete_directory(Recurse, File, Ignore).

rm_directory(File, Ignore) :-
	rmdir(File, Error),
	handle_system_internal(Error, Ignore, delete_file(File)).

delete_directory(on, File, Ignore) :-
	directory_files(File, FileList),
	path_separator(D),
	atom_concat(File, D, FileP),
	delete_dirfiles(FileList, FileP, Ignore),
	rmdir(File, Ignore).

delete_dirfiles([], _, _).
delete_dirfiles(['.'|Fs], File, Ignore) :- !,
	delete_dirfiles(Fs, File, Ignore).
delete_dirfiles(['..'|Fs], File, Ignore) :- !,
	delete_dirfiles(Fs, File, Ignore).
delete_dirfiles([F|Fs], File, Ignore) :-
	atom_concat(File,F,TrueF),
	delete_file(TrueF, off, on, Ignore),
	delete_dirfiles(Fs, File, Ignore).

handle_system_internal(Error, _Ignore, _G) :- var(Error), !.
handle_system_internal(Error, off, G) :- atom(Error), !,
	throw(error(system_internal(Error),G)).
handle_system_internal(Error, off, G) :-
	error_message(Error, Message),
	throw(error(system_internal(Message),G)).

handle_system_internal(Error, _Id, _Ignore, _G) :- var(Error), !.
handle_system_internal(Error, _SIG, off, G) :- integer(Error), !,
	error_message(Error, Message),
	throw(error(system_internal(Message),G)).
handle_system_internal(signal, SIG, off, G) :- !,
        throw(error(system_internal(child_signal(SIG)),G)).
handle_system_internal(stopped, SIG, off, G) :-
        throw(error(system_internal(child_stopped(SIG)),G)).

file_property(IFile, type(Type)) :-
	true_file_name(IFile, File),
	file_property(File, Type, _Size, _Date, _Permissions, _LinkName).
file_property(IFile, size(Size)) :-
	true_file_name(IFile, File),
	file_property(File, _Type, Size, _Date, _Permissions, _LinkName).
file_property(IFile, mod_time(Date)) :-
	true_file_name(IFile, File),
	file_property(File, _Type, _Size, Date, _Permissions, _LinkName).
file_property(IFile, mode(Permissions)) :-
	true_file_name(IFile, File),
	file_property(File, _Type, _Size, _Date, Permissions, _LinkName).
file_property(IFile, linkto(LinkName)) :-
	true_file_name(IFile, File),
	file_property(File, _Type, _Size, _Date, _Permissions, LinkName),
	atom(LinkName).

file_property(File, Type, Size, Date, Permissions, LinkName) :-
	file_property(File, Type, Size, Date, Permissions, LinkName, Error),
	handle_system_internal(Error, off, file_property(File)).


/** @pred environ(? _EnvVar_,+ _EnvValue_)


Unify environment variable  _EnvVar_ with its value  _EnvValue_,
if there is one. This predicate is backtrackable in Unix systems, but
not currently in Win32 configurations.

~~~~~
   ?- environ('HOME',V).

V = 'C:\\cygwin\\home\\administrator' ?
~~~~~				      
_EnvVar_ may be bound to an atom, or just be
  unbound. In the latter case environ/2 will enumerate over all
  environment variables.

*/
environ(Na,Val) :- var(Na), !,
	bet(0,I),
	( p_environ(I,S) -> environ_split(S,SNa,SVal) ; !, fail ),
	atom_codes(Na, SNa),
	atom_codes(Val, SVal).
environ(Na,Val) :- atom(Na), !,
	bound_environ(Na, Val).
environ(Na,Val) :-
	throw(error(type_error(atom,Na),environ(Na,Val))).

bound_environ(Na, Val) :- var(Val), !,
	getenv(Na,Val).
bound_environ(Na, Val) :- atom(Val), !,
	putenv(Na,Val).
bound_environ(Na, Val) :-
	throw(error(type_error(atom,Val),environ(Na,Val))).

environ_enum(X,X).
environ_enum(X,X1) :-
	Xi is X+1,
	environ_enum(Xi,X1).

environ_split([61|SVal], [], SVal) :- !.
environ_split([C|S],[C|SNa],SVal) :-
	environ_split(S,SNa,SVal).

/** @pred exec(+ Command, StandardStreams, -PID)
 *
 *
 *
 * Execute command _Command_ with its standard streams connected to the
 * list [_InputStream_, _OutputStream_, _ErrorStream_]. A numeric
 * identifier to the process that executes the command is returned as
 * _PID_. The command is executed by the default shell `bin/sh -c` in
 * Unix.
 *
 * The following example demonstrates the use of exec/3 to send a
 * command and process its output:
 *
 * ~~~~~
  go :-
     exec(ls,[std,pipe(S),null],P),
     repeat,
     get0(S,C),
     (C = -1, close(S) ! ; put(C)).
~~~~~
 *
 * The streams may be one of standard stream, `std`, null stream,
 * `null`, or `pipe(S)`, where  _S_ is a pipe stream. Note
 * that it is up to the user to close the pipe.
 *
 *
*/
exec(Command, [StdIn, StdOut, StdErr], PID) :-
	G = exec(Command, [StdIn, StdOut, StdErr], PID),
	check_command_with_default_shell(Command, TrueCommand, G),
	process_inp_stream_for_exec(StdIn, In, G, [], L1),
	process_out_stream_for_exec(StdOut, Out, G, L1, L2),
	process_err_stream_for_exec(StdErr, Err, G, L2, L3),
	( exec_command(TrueCommand, In, Out, Err, PID, Error) -> true ; true ),
	close_temp_streams(L3),
	handle_system_internal(Error, off, G).

process_inp_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_inp_stream_for_exec(null, null, _, L, L) :- !.
process_inp_stream_for_exec(std, 0, _, L, L) :- !.
process_inp_stream_for_exec(pipe(ForWriting), ForReading, _, L, [ForReading|L]) :- var(ForWriting), !,
	open_pipe_stream(ForReading, ForWriting).
process_inp_stream_for_exec(pipe(Stream), _, _, L, L) :- !,
	stream_property(Stream, input).
process_inp_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, put).


process_out_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_out_stream_for_exec(null, null, _, L, L) :- !.
process_out_stream_for_exec(std, 1, _, L, L) :- !.
process_out_stream_for_exec(pipe(ForReading), ForWriting, _, L, [ForWriting|L]) :- var(ForReading), !,
	open_pipe_stream(ForReading, ForWriting).
process_out_stream_for_exec(pipe(Stream), _, _, L, L) :- !,
	stream_property(Stream, output).
process_out_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, output).

process_err_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_err_stream_for_exec(null, null, _, L, L) :- !.
process_err_stream_for_exec(std, 2, _, L, L) :- !.
process_err_stream_for_exec(pipe(ForReading), ForWriting, _, L, [ForWriting|L]) :- var(ForReading), !,
	open_pipe_stream(ForReading, ForWriting).
process_err_stream_for_exec(pipe(Stream), Stream, _, L, L) :- !,
	stream_property(Stream, output).
process_err_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, output).

close_temp_streams([]).
close_temp_streams([S|Ss]) :-
	close(S),
	close_temp_streams(Ss).

/** @pred popen( +Command, +TYPE, -Stream)

 * Provides the functionaluty of the Unix <tt>popen</tt> function. It
 * opens a process by creating a pipe, forking and invoking _Command_ on
 * the child process. Since a pipe is by definition unidirectional the
 * _Type_ argument may be `read` or `write`, not both. The stream should
 * be closed using close/1, there is no need for a special `pclose`
 * command.
 *
 * The following example demonstrates the use of popen/3 to process the
 * output of a command, note that popen/3 works as a simplified interface
 * to the exec/3 command:
 *
~~~~~
?- popen(ls,read,X),repeat, get0(X,C), (C = -1, ! ; put(C)).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~
 *
 * The implementation of popen/3 relies on exec/3.
 *
*/
popen(Command, read, Stream) :-
	exec(Command, [std,pipe(Stream),std], Stream).
popen(Command, write, Stream) :-
	exec(Command, [pipe(Stream),std,std], Stream).

check_command_with_default_shell(Com, ComF, G) :-
	check_command(Com, G),
	os_command_postprocess(Com, ComF).

%
% make sure that Windows executes the command from $COMSPEC.
%
os_command_postprocess(Com, ComF) :- win, !,
	atom_codes(Com, SC),
	append(" /c ", SC, SC1),
	getenv('COMSPEC', Shell0),
	atom_codes(Shell0, Codes),
	append(Codes, SC1, SCF),
	atom_codes(ComF, SCF).
os_command_postprocess(Com, Com).

check_command(Com, G) :- var(Com), !,
	throw(error(instantiation_error,G)).
check_command(Com, _) :- atom(Com), !.
check_command(Com, G) :-
	throw(error(type_error(atom,Com),G)).

check_mode(Mode, _, G) :- var(Mode), !,
	throw(error(instantiation_error,G)).
check_mode(read, 0, _) :- !.
check_mode(write,1, _) :- !.
check_mode(Mode, G) :-
	throw(error(domain_error(io_mode,Mode),G)).

shell :-
	G = shell,
	get_shell0(FullCommand),
	exec_command(FullCommand, 0, 1, 2, PID, Error),
	handle_system_internal(Error, off, G),
	wait(PID, _Status, Error, Id),
	handle_system_internal(Error, got(FullCommand, Id), off, G).

shell(Command) :-
	G = shell(Command),
	check_command(Command, G),
	get_shell(Shell,Opt),
	do_shell(Shell, Opt, Command, Status, Error),
	Status = 0,
	handle_system_internal(Error, off, G).

shell(Command, Status) :-
	G = shell(Command, Status),
	check_command(Command, G),
	get_shell(Shell,Opt),
	do_shell(Shell, Opt, Command, Status, Error),
	handle_system_internal(Error, off, G).

protect_command([], [0'"]). % "
protect_command([H|L], [H|NL]) :-
	protect_command(L, NL).

get_shell0(Shell) :-
	getenv('SHELL', Shell), !.
get_shell0(Shell) :-
	win, !,
	getenv('COMSPEC', Shell).
get_shell0('/bin/sh').

get_shell(Shell, '-c') :-
	getenv('SHELL', Shell), !.
get_shell(Shell, '/c') :-
	win, !,
	getenv('COMSPEC', Shell).
get_shell('/bin/sh','-c').

/**
  * @pred  system(+ _S_)

Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.
*/

/**
  * @pred  system

Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.
*/
system :-
	default_shell(Command),
	do_system(Command, _Status, Error),
	handle_system_internal(Error, off, system).

default_shell(Shell) :- win, !,
	getenv('COMSPEC', Shell).
default_shell('/bin/sh').


/** @pred system(+ _Command_,- _Res_)

Interface to `system`: execute command  _Command_ and unify
 _Res_ with the result.


n*/
system(Command, Status) :-
	G = system(Command, Status),
	check_command(Command, G),
	do_system(Command, Status, Error),
	Status = 0,
	handle_system_internal(Error, off, G).

wait(PID,STATUS) :- var(PID), !,
 	throw(error(instantiation_error,wait(PID,STATUS))).
wait(PID,STATUS) :- integer(PID), !,
 	plwait(PID, STATUS, Error, _Detail),
 	handle_system_internal(Error, off, wait(PID,STATUS)).
wait(PID,STATUS) :-
 	throw(error(type_error(integer,PID),wait(PID,STATUS))).

%
% host info
%
host_name(X) :-
	host_name(X, Error),
	handle_system_internal(Error, off, host_name(X)).

host_id(X) :-
	host_id(X0, Error),
	handle_system_internal(Error, off, host_id(X)),
	number_codes(X0, S),
	atom_codes(X, S).

pid(X) :-
	pid(X, Error),
	handle_system_internal(Error, off, pid(X)).

kill(X,Y) :-
	integer(X), integer(Y), !,
	kill(X, Y, Error),
	handle_system_internal(Error, off, kill(X,Y)).
kill(X,Y) :- (var(X) ; var(Y)), !,
	throw(error(instantiation_error,kill(X,Y))).
kill(X,Y) :- integer(X), !,
	throw(error(type_error(integer,Y),kill(X,Y))).
kill(X,Y) :-
	throw(error(type_error(integer,X),kill(X,Y))).

mktemp(X,Y) :- var(X), !,
	throw(error(instantiation_error,mktemp(X,Y))).
mktemp(X,Y) :-
	atom(X), !,
	mktemp(X, Y, Error),
	handle_system_internal(Error, off, mktemp(X,Y)).
mktemp(X,Y) :-
	throw(error(type_error(atom,X),mktemp(X,Y))).

tmpnam(X) :-
	tmpnam(X, Error),
	handle_system_internal(Error, off, tmpnam(X)).

%%% Added from Theo, path_seperator is used to replace the c predicate dir_separator which is not OS aware

tmpdir(TmpDir):-
  tmpdir(Dir, Error),
  handle_system_internal(Error, off, tmpdir(Dir)),
  path_separator(D),
  (atom_concat(_, D, Dir) ->
    TmpDir = Dir
  ;
    atom_concat(Dir, D, TmpDir)
  ).

path_separator('\\'):-
  win, !.
path_separator('/').

read_link(P,D,F) :-
	read_link(P, D),
	absolute_file_name(D, [], F).

/** @pred rename_file(+ _OldFile_,+ _NewFile_)


Create file  _OldFile_ to  _NewFile_. This predicate uses the
`C` built-in function `rename`.


*/
rename_file(F0, F) :-
	rename_file(F0, F, Error),
	handle_system_internal(Error, off, rename_file(F0, F)).

/** @pred directory_files(+ _Dir_,+ _List_)


Given a directory  _Dir_,  directory_files/2 procedures a
listing of all files and directories in the directory:

~~~~~
    ?- directory_files('.',L), writeq(L).
['Makefile.~1~','sys.so','Makefile','sys.o',x,..,'.']
~~~~~
The predicates uses the `dirent` family of routines in Unix
environments, and `findfirst` in WIN32 through the system_library buil

*/
directory_files(X,Y) :-
     list_directory(X,Y).

:- meta_predicate directory_map(+,1,-),
	rb_apply(+,+,2,-).

/** @pred directory_map(+ _Dir_, 1:_P_)


Given a directory _Dir_, directory_map/2 visits all files in _Dir_,
and verifies whether `P(F)` holds, where _F_ is the file's absolute
path.

~~~~~
    ?- directory_map('.', process).
~~~~~

The predicates performs a left-recursive traversal. It does not protect against file system errors and it does not check for symbolic links.

*/
directory_map(D, P) :-
        working_directory(_, D),
	list_directory(D,L),
	d_map(L,D, P).

d_map([],_,_).
d_map(['.'|Fs],D, P) :-
    !,
    d_map(Fs,D, P).
d_map(['..'|Fs],D, P) :-
    !,
    d_map(Fs, D, P).
d_map([F|Fs], D, P) :-
    absolute_file_name( F, File, [prefix(D)] ),
    f_map(File, P),
    d_map(Fs, D, P).

f_map(File, P) :-
     catch( file_property( File, type(directory) ), _, fail ),
     directory_map( File, P).
f_map(File, P) :-
     call(P,File).



/** @} */
