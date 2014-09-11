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

/** @defgroup System Calling The Operating System from YAP
@ingroup YAPLibrary
@{

YAP now provides a library of system utilities compatible with the
SICStus Prolog system library. This library extends and to some point
replaces the functionality of Operating System access routines. The
library includes Unix/Linux and Win32 `C` code. They
are available through the `use_module(library(system))` command.



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


/** @pred  environ(+ _E_,- _S_) 





Given an environment variable  _E_ this predicate unifies the second argument  _S_ with its value.

 
*/
/** @pred  system(+ _S_) 


Passes command  _S_ to the Bourne shell (on UNIX environments) or the
current command interpreter in WIN32 environments.

 
*/
/** @pred  working_directory(- _CurDir_,? _NextDir_) 


Fetch the current directory at  _CurDir_. If  _NextDir_ is bound
to an atom, make its value the current working directory.

 
*/
/** @pred delete_file(+ _File_) 


The delete_file/1 procedure removes file  _File_. If
 _File_ is a directory, remove the directory <em>and all its subdirectories</em>.

~~~~~
   ?- delete_file(x).
~~~~~

 
*/
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
/** @pred directory_files(+ _Dir_,+ _List_) 


Given a directory  _Dir_,  directory_files/2 procedures a
listing of all files and directories in the directory:

~~~~~
    ?- directory_files('.',L), writeq(L).
['Makefile.~1~','sys.so','Makefile','sys.o',x,..,'.']
~~~~~
The predicates uses the `dirent` family of routines in Unix
environments, and `findfirst` in WIN32.

 
*/
/** @pred environ(? _EnvVar_,+ _EnvValue_) 


Unify environment variable  _EnvVar_ with its value  _EnvValue_,
if there is one. This predicate is backtrackable in Unix systems, but
not currently in Win32 configurations.

~~~~~
   ?- environ('HOME',X).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~

 
*/
/** @pred exec(+ _Command_, _StandardStreams_,- _PID_) 


Execute command  _Command_ with its standard streams connected to
the list [_InputStream_,  _OutputStream_, _ErrorStream_]. The
process that executes the command is returned as  _PID_. The
command is executed by the default shell `bin/sh -c` in Unix.

The following example demonstrates the use of exec/3 to send a
command and process its output:

~~~~~
exec(ls,[std,pipe(S),null],P),repeat, get0(S,C), (C = -1, close(S) ! ; put(C)).
~~~~~

The streams may be one of standard stream, `std`, null stream,
`null`, or `pipe(S)`, where  _S_ is a pipe stream. Note
that it is up to the user to close the pipe.

 
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
/** @pred make_directory(+ _Dir_) 


Create a directory  _Dir_. The name of the directory must be an atom.

 
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
/** @pred popen(+ _Command_, + _TYPE_, - _Stream_) 


Interface to the <tt>popen</tt> function. It opens a process by creating a
pipe, forking and invoking  _Command_ on the current shell. Since a
pipe is by definition unidirectional the  _Type_ argument may be
`read` or `write`, not both. The stream should be closed
using close/1, there is no need for a special `pclose`
command.

The following example demonstrates the use of popen/3 to process
the output of a command, as exec/3 would do:

~~~~~{.prolog}
   ?- popen(ls,read,X),repeat, get0(X,C), (C = -1, ! ; put(C)).

X = 'C:\\cygwin\\home\\administrator' ?
~~~~~

The WIN32 implementation of popen/3 relies on exec/3.

 
*/
/** @pred rename_file(+ _OldFile_,+ _NewFile_) 


Create file  _OldFile_ to  _NewFile_. This predicate uses the
`C` built-in function `rename`.

 
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
/** @pred tmp_file(+_Base_, - _File_) 

Create a name for a temporary file.  _Base_ is an user provided
identifier for the category of file. The  _TmpName_ is guaranteed to
be unique. If the system halts, it will automatically remove all created
temporary files.

 
*/
/** @pred tmpnam(- _File_) 



Interface with  _tmpnam_: obtain a new, unique file name  _File_.

 
*/
/** @pred working_directory(- _Old_,+ _New_) 



Unify  _Old_ with an absolute path to the current working directory
and change working directory to  _New_.  Use the pattern
`working_directory(CWD, CWD)` to get the current directory.  See
also `absolute_file_name/2` and chdir/1.

 
*/
:- module(operating_system_support, [
	datime/1,
	delete_file/1,
	delete_file/2,
	directory_files/2,
	environ/2,
	exec/3,
	file_exists/1,
	file_exists/2,
	file_property/2,
	host_id/1,
	host_name/1,
	pid/1,
	mktemp/2,
	make_directory/1,
	popen/3,
	rename_file/2,
	shell/0,
	shell/1,
	shell/2,
	sleep/1,
	system/0,
	system/1,
	system/2,
	mktime/2,
	tmpnam/1,
	tmp_file/2,
        tmpdir/1,
	working_directory/2
          ]).

:- use_module(library(lists), [append/3]).

:- if(current_prolog_flag(windows, false)). 
:- reexport(library(unix), [wait/2,
			     kill/2]).
:- endif.

:- load_foreign_files([sys], [], init_sys).

:- dynamic tmp_file_sequence_counter/1.

% time builtins

datime(X) :-
	datime(X, Error),
	handle_system_error(Error, off, datime(X)).
 
mktime(V, A) :- var(V), !,
	throw(error(instantiation_error,mktime(V,A))).
mktime(In,Out) :-
	check_mktime_inp(In, mktime(In,Out)),
	In = datime(Y,Mo,D,H,Mi,S),
	mktime(Y, Mo, D, H, Mi, S, Out, Error),
	handle_system_error(Error, off, mktime(In,Out)).
 
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

delete_file(IFile) :-
	true_file_name(IFile, File),
	delete_file(File, off, on, off).

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
	handle_system_error(N, Ignore, delete_file(File)).
delete_file(directory, File, Dir, Recurse, Ignore) :-
	delete_directory(Dir, File, Recurse, Ignore), !.
delete_file(_, File, _Dir, _Recurse, Ignore) :-
	unlink_file(File, Ignore).

unlink_file(IFile, Ignore) :-
	true_file_name(IFile, File),
	unlink(File, N),
	handle_system_error(N, Ignore, delete_file(File)).

delete_directory(on, File, _Recurse, Ignore) :-
	rm_directory(File, Ignore).
delete_directory(off, File, Recurse, Ignore) :-
	delete_directory(Recurse, File, Ignore).

rm_directory(File, Ignore) :-
	rmdir(File, Error),
	handle_system_error(Error, Ignore, delete_file(File)).

delete_directory(on, File, Ignore) :-
	directory_files(File, FileList, Ignore),
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

directory_files(File, FileList, Ignore) :-
       list_directory(File, FileList, Error),
       handle_system_error(Error, Ignore, directory_files(File, FileList)).

handle_system_error(Error, _Ignore, _G) :- var(Error), !.
handle_system_error(Error, off, G) :- atom(Error), !,
	throw(error(system_error(Error),G)).
handle_system_error(Error, off, G) :-
	error_message(Error, Message),
	throw(error(system_error(Message),G)).

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
	handle_system_error(Error, off, file_property(File)).

file_exists(File) :-
	var(File), !,
	throw(error(instantiation_error,file_exists(File))).
file_exists(File) :-
	\+ atom(File), !,
	throw(error(type_error(atom,File),file_exists(File))).
file_exists(IFile) :-
	true_file_name(IFile, File),
	file_property(File, _Type, _Size, _Date, _Permissions, _, Error),
	var(Error).

file_exists(File, Permissions) :-
	var(File), !,
	throw(error(instantiation_error,file_exists(File, Permissions))).
file_exists(File, Permissions) :-
	\+ atom(File), !,
	throw(error(type_error(atom,File),file_exists(File, Permissions))).
file_exists(IFile, Permissions) :-
	true_file_name(IFile, File),
	file_property(File, _Type, _Size, _Date, FPermissions, _, Error),
	var(Error),
	process_permissions(Permissions, Perms),
	FPermissions /\ Perms =:= Perms.

process_permissions(Number, Number) :- integer(Number).

%
% environment manipulation.
%

environ(Na,Val) :- var(Na), !,
	environ_enum(0,I),
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

%
% process execution
%
exec(Command, [StdIn, StdOut, StdErr], PID) :-
	G = exec(Command, [StdIn, StdOut, StdErr], PID),
	check_command_with_default_shell(Command, TrueCommand, G), 
	process_inp_stream_for_exec(StdIn, In, G, [], L1),
	process_out_stream_for_exec(StdOut, Out, G, L1, L2),
	process_err_stream_for_exec(StdErr, Err, G, L2, L3),
	( exec_command(TrueCommand, In, Out, Err, PID, Error) -> true ; true ),
	close_temp_streams(L3),
	handle_system_error(Error, off, G).

process_inp_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_inp_stream_for_exec(null, null, _, L, L) :- !.
process_inp_stream_for_exec(std, 0, _, L, L) :- !.
process_inp_stream_for_exec(pipe(ForWriting), ForReading, _, L, [ForReading|L]) :- var(ForWriting), !,
	open_pipe_streams(ForReading, ForWriting).
process_inp_stream_for_exec(pipe(Stream), _, _, L, L) :- !,
	stream_property(Stream, output).
process_inp_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, output).


process_out_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_out_stream_for_exec(null, null, _, L, L) :- !.
process_out_stream_for_exec(std, 1, _, L, L) :- !.
process_out_stream_for_exec(pipe(ForReading), ForWriting, _, L, [ForWriting|L]) :- var(ForReading), !,
	open_pipe_streams(ForReading, ForWriting).
process_out_stream_for_exec(pipe(Stream), _, _, L, L) :- !,
	stream_property(Stream, input).
process_out_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, input).

process_err_stream_for_exec(Error, _, G, L, L) :- var(Error), !,
	close_temp_streams(L),
	throw(error(instantiation_error,G)).
process_err_stream_for_exec(null, null, _, L, L) :- !.
process_err_stream_for_exec(std, 2, _, L, L) :- !.
process_err_stream_for_exec(pipe(ForReading), ForWriting, _, L, [ForWriting|L]) :- var(ForReading), !,
	open_pipe_streams(ForReading, ForWriting).
process_err_stream_for_exec(pipe(Stream), Stream, _, L, L) :- !,
	stream_property(Stream, input).
process_err_stream_for_exec(Stream, Stream, _, L, L) :-
	stream_property(Stream, input).

close_temp_streams([]).
close_temp_streams([S|Ss]) :- 
	close(S),
	close_temp_streams(Ss).

popen(Command, Mode, Stream) :-
	open(pipe(Command), Mode, Stream).

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
	handle_system_error(Error, off, G),
	wait(PID, _Status, Error),
	handle_system_error(Error, off, G).

shell(Command) :-
	G = shell(Command),
	check_command(Command, G),
	get_shell(Shell,Opt),
	do_shell(Shell, Opt, Command, Status, Error),
	Status = 0,
	handle_system_error(Error, off, G).

shell(Command, Status) :-
	G = shell(Command, Status),
	check_command(Command, G),
	get_shell(Shell,Opt),
	do_shell(Shell, Opt, Command, Status, Error),
	handle_system_error(Error, off, G).

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
	   
system :-
	default_shell(Command),
	do_system(Command, _Status, Error),
	handle_system_error(Error, off, system).

default_shell(Shell) :- win, !,
	getenv('COMSPEC', Shell).
default_shell('/bin/sh').
	

system(Command, Status) :-
	G = system(Command, Status),
	check_command(Command, G),
	do_system(Command, Status, Error),
	Status = 0,
	handle_system_error(Error, off, G).

%% wait(PID,STATUS) :- var(PID), !,
%% 	throw(error(instantiation_error,wait(PID,STATUS))).
%% wait(PID,STATUS) :- integer(PID), !,
%% 	wait(PID, STATUS, Error),
%% 	handle_system_error(Error, off, wait(PID,STATUS)).
%% wait(PID,STATUS) :-
%% 	throw(error(type_error(integer,PID),wait(PID,STATUS))).

%
% host info
%
host_name(X) :-
	host_name(X, Error),
	handle_system_error(Error, off, host_name(X)).

host_id(X) :-
	host_id(X0, Error),
	handle_system_error(Error, off, host_id(X)),
	number_codes(X0, S),
	atom_codes(X, S).

pid(X) :-
	pid(X, Error),
	handle_system_error(Error, off, pid(X)).

%% kill(X,Y) :-
%% 	integer(X), integer(Y), !,
%% 	kill(X, Y, Error),
%% 	handle_system_error(Error, off, kill(X,Y)).
%% kill(X,Y) :- (var(X) ; var(Y)), !,
%% 	throw(error(instantiation_error,kill(X,Y))).
%% kill(X,Y) :- integer(X), !,
%% 	throw(error(type_error(integer,Y),kill(X,Y))).
%% kill(X,Y) :-
%% 	throw(error(type_error(integer,X),kill(X,Y))).

mktemp(X,Y) :- var(X), !,
	throw(error(instantiation_error,mktemp(X,Y))).
mktemp(X,Y) :-
	atom(X), !,
	mktemp(X, Y, Error),
	handle_system_error(Error, off, mktemp(X,Y)).
mktemp(X,Y) :-
	throw(error(type_error(atom,X),mktemp(X,Y))).

tmpnam(X) :-
	tmpnam(X, Error),
	handle_system_error(Error, off, tmpnam(X)).

%%% Added from Theo, path_seperator is used to replace the c predicate dir_separator which is not OS aware

tmpdir(TmpDir):-
  tmpdir(Dir, Error),
  handle_system_error(Error, off, tmpdir(Dir)),
  path_separator(D),
  (atom_concat(_, D, Dir) ->
    TmpDir = Dir
  ;
    atom_concat(Dir, D, TmpDir)
  ).

path_separator('\\'):-
  win, !.
path_separator('/').
