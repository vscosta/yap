
:- use_module(library(python)).

:- initialization(main).

setup_dir :-
	prolog_load_context(directory, Dir),
	atom_concat(Dir,'pyx/',Base),
	assert(base(Base)).

:- setup_dir.

main :- 
	python_import(pyx),
	ex(X),
	flush_output,
	fail.
main.

ex(hello) :-
	c := canvas:canvas(_),
	:= $c:text(0, 0, 'Hello, world!'),
	:= $c:stroke(path:line(0, 0, 2, 0)),
	:= $c:writePDFfile('hello').

ex(changebar) :-
	g := graph:graphxy(width=8, x=graph:axis:bar(_)),
	base(Source), atomic_concat(Source, 'minimal.dat', Data),
	:= $g:plot(graph:data:file(Data, xname=0, y=2), [graph:style:changebar(_)]),
	:= $g:writePDFfile(changebar).


