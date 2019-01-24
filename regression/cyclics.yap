:- X = [X], copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X = [_|X], copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X= f(X), copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X= f(X,X), copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X= f(_,X), copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X= f(X,[X,X]), copy_term(X,Y), writeln(X), writeln(Y), fail.
:- X= f(X,[X,g(X)]), copy_term(X,Y), writeln(X), writeln(Y), fail.
