                               
:- use_module(library(lists)).


test a(X) returns atom(X) .+
given  user:goal_expansion(a(X), current_atom(X))

test m:a(3,X) returns X =@= 15,
given  user:goal_expansion(a(X,Y), Y is X*5)

test m:a(3,X) returns X =@= 9
given  user:goal_expansion(a(X,Y), m, Y is X*X ))

test  m:a(3,X) returns X =@= 9
given  user:goal_expansion(a(X,Y), m, Y is X*X ), user:goal_expansion(a(X), X is 3*5)
