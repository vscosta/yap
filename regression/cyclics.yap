%, copy_term(X,Y), writeln('....'), writeln(X), writeln(Y).

:- initialization(main).

main :-
	main( cyclic_term(X), X).
main :-
	writeln('-----------------------'),
	fail.
main :-
	main( ground(X), X).
main :-
	writeln('-----------------------'),
	fail.
main :-
	main( writeln(X), X).
main :-
	writeln('-----------------------'),
	fail.
main :-
	main((rational_term_to_tree(X,A,B,[]), numbervars(A+B,1,_),
	      writeln((A->B))), X).
main.

main(G, X) :-
	d(X),
	m(G).

m( G ) :-
	G,
	!,
	writeln(yes),
	end.
m( G ) :-
	writeln(no),
	end.

d(X) :- X = [_A].
d(X) :- X = [a,_A].
d(X) :- X = [X].
d(X) :- X = [_|X].
d(X) :- X = [_,X].
d(X) :- X = [_,x].
d(X) :- X = [_,x(X)].
d(X) :- X= f(X).
d(X) :- X= f(X,X).
d(X) :- X= f(_,X).
d(X) :- X= f(A,A,X).
d(X) :- X= f(A,A,g(A)).
d(X) :- X= f(A,g(X,[A|A]),X).
d(X) :- X= f(X,[X,X]).
d(X) :- X= f(X,[X,g(X)]).
d(X) :- X= f(_,X/[X]).
d(X) :- X= f(_,A/[A]), A= f(X,[X,g(X)]).
d(X) :- X= f(_,A/[A]), A= f(X,[A,g(X)]).
d(X) :- X= f(_,A/[A]), A= f(B,[X,g(A)]), B=[C|B], C=[X].

end :- writeln('....'), fail.

a(no,   no).
a(no,   no).
a(yes, yes).
a(yes,  no).
a(yes,  no).
a( no,  no).
a(yes,  no).
a(yes, yes).
a(yes, yes).
a(yes,  no).
a(yes,  no).
a( no,  no).
a(yes,  no).
a(yes, yes).
a(yes, yes).
a(yes,  no).
a(yes,  no).
