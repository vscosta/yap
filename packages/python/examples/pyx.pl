
:- use_module(library(python)).

:- initialization(main).

main :- 
	ex(X),
	flush_output,
	fail.
main.

ex(hello_world) :-
	c = pyx:canvas:canvas,
	:= c:text(0, 0, 'Hello, world!'),
	:= c:stroke(path:line(0, 0, 2, 0)),
	c.writePDFfile('hello.pdf').

