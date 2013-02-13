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

:- reexport(library(unix), [wait/2,
			     kill/2]).

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
	delete_directory(Dir, File, Recurse, Ignore).
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
