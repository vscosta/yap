
:- module( dynimport, d/1).

:- use_module( mod1 ).

:- initialization( open('errors.md', write, _Out, [alias(user_error)]), now ).

:- format(user_error, ' Report on dynamic module import.~n~n', []).

d(2).

:- format(user_error, ' + ok if ok~n~n~*c~n', [4,0'~,4,0'~]).

:- initialization( ( mod1(_), writeln(ok) ), now ).

:- format(user_error, '~*c~n', [4,0'~]).

