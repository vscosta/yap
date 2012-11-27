
:- use_module(library(python)).

:- initialization(main).

setup_dir :-
	prolog_load_context(directory, Dir),
	atom_concat(Dir,'/pyx/',Base),
	assert(base(Base)).

:- setup_dir.

main :- 
	ex(X),
	flush_output,
	fail.
main.

ex(hello_world) :-
	c := pyx:canvas:canvas,
	:= $c:text(0, 0, 'Hello, world!'),
	:= $c:stroke(path:line(0, 0, 2, 0)),
	:= $c:writePDFfile('hello').

ex(changebar) :-
	g := pyx:graph:graphxy(width=8, x=graph:axis:bar),
	atomic_concat(Source, 'minimal.dat', Data),
	:= $g:plot(graph:data:file(Data, xname=0, y=2), [graph:style:changebar]),
	:= $g:writePDFfile(changebar).


