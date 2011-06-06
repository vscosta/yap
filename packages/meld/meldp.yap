:- module(meld_program,
	[
	 run/1
	]).


:- use_module(meldi,
	[
	 push/1,
	 first/2,
	 min/3,
	 max/3
	]).


% built-ins.
:- dynamic root/1, neighbor/2, temperature/2.

trace(A,B) :- !,
	writeln((A:-B)),
	trace.


run([]) :- fail.
