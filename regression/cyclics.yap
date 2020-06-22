%, copy_term(X,Y), writeln('....'), writeln(X), writeln(Y).

:- use_module(library(terms)).

:- initialization(main).

:- op(700, xfx, :=: ).

X :=: X.

main :-
    exec.

test( cyclic_term(X), [X]).
test( ground(X), [X]).
test( (term_variables(X, O), writeln(X=O) ), [X, [], O]).
test( (new_variables_in_term(L,X, O), writeln(X+L=O) ), [X, L, O]).
test( (variables_in_both_terms(L,X, O), writeln(X+L=O) ), [X, L, O]).
test( writeln(X), [X]).
test((rational_term_to_forest(X,A,B,[]),
	   writeln((A->B))), [X, A, B]).
test(( numbervars(A+B,1,_)), [A, B]).
test((rational_term_to_forest(X,A,B,[]), numbervars(A+B,1,_),
	      writeln((A->B))), [X,A,B]).

:- dynamic i/1.
i(0).

id(I) :-
    retract(i(I)),
    I1 is I+1,
    assert(i(I1)).

exec :-
    test( G, [X|Ps] ),
    functors(G, Fs), 
    format('**** ~n',[Fs]),
    d(I, X, GX),
    portray_clause(GX),
    m(I, GX,  G, [X|Ps]),
    fail.
exec.

functors((X,Y),(GX -> GY)) :-
    !,
    functors(X, GX),
    functors(Y, GY).
functors(X, GX) :-
    functor(X, GX, _).

m( I, GX, G, Ps ) :-
   %trace,
    GX,
    G,
    !,
    format( '~d ~a: ~n', [I, yes]), writeln(G).
m( I, GX, G, _Ps ) :-
    GX,
        format( '~d ~a: ', [I, no]), writeln(G).


d(1, X, X = [_A] ).
d(1, X, (  X = [a,_A]) ).
d(1, X, (  X = [X]) ).
d(1, X, (  X = [_|X]) ).
d(1, X, (  X = [_,X]) ).
d(1, X, (  X = [_,x]) ).
d(1, X, (  X = [_,x(X)]) ).
d(1, X, (  X= f(X)) ).
d(1, X, (  X= f(X,X)) ).
d(1, X, (  X= f(_,X)) ).
d(1, X, (  X= f(A,A,X)) ).
d(1, X, (  X= f(A,A,g(A))) ).
d(1, X, (  X= f(A,A,2.3)) ).
d(1, X, (  X= f(A,g(X,[A|A]),X)) ).
d(1, X, (  X= f(X,[X,X])) ).
d(1, X, (  X= f(3.14,[22.3,X])) ).
d(1, X, (  X= f(X,[X,g(X)])) ).
d(1, X, (  X= f(_,X/[X])) ).
d(2, X, (  X= f(_,A/[A]), A= f(_X,[_X,g(_X)])) ).
d(2, X, (  X= f(_,A/[A]), A= f(X,[A,g(X)])) ).
d(2, X,Y, (  X = t(_A,B,_C,D), Y = [B,E]) ) .
d(2, X,Y, (  X = t(_A,_B,_C,_D), Y = [_,_E]) ).
d(2, X,Y, (  X = t(A,_B,C,_D), Y = [ A,C]) ).
d(2, X,Y, (  X = t(A,[X,_D]), Y = [A,_C,_E]) ).
d(2, X,Y, (  X = t(A,[X,C]), Y = [A,C,_E]) ).
d(2, X,Y, ( X = t(A,X,_B,[X,C,_D]), Y = [A,C,_E]) ).
d(4, X, (  X= f(_,A/[A]), A= f(B,[X,g(A)]), B=[C|B], C=[D,f(h(A,_),_),_] ) ).

