
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

ex(pathitem) :-
	c := canvas:canvas(_),
	rect1 := path:path(path:moveto(0, 0), path:lineto(1, 0),
                  path:moveto(1, 0), path:lineto(1, 1),
                  path:moveto(1, 1), path:lineto(0, 1),
                  path:moveto(0, 1), path:lineto(0, 0)),
        rect2 := path:path(path:moveto(2, 0), path:lineto(3, 0),
                  path:lineto(3, 1), path:lineto(2, 1),
                  path:lineto(2, 0)),
	rect3 := path:path(path:moveto(4, 0), path:lineto(5, 0),
                  path:lineto(5, 1), path:lineto(4, 1),
                  path:closepath(_)),
         := $c:stroke($rect1, [style:linewidth:'THICK']),
         := $c:stroke($rect2, [style:linewidth:'THICK']),
         := $c:stroke($rect3, [style:linewidth:'THICK']),
	 := $c:writePDFfile('pathitem').

ex(changebar) :-
	g := graph:graphxy(width=8, x=graph:axis:bar(_)),
	base(Source), atomic_concat(Source, 'minimal.dat', Data),
	:= $g:plot(graph:data:file(Data, xname=0, y=2), [graph:style:changebar(_)]),
	:= $g:writePDFfile(changebar).


