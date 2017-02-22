

:- style_check(all).

:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).

main :-
    ex(Ex, _),
    sudoku(Ex, _My),
	fail.
main.

sudoku( Ex, Els ) :-
	problem(Ex, Els),
	output(Els).

%
% gecode constraints
%
problem(Ex, Els) :-
	length(Els, 81),
	Els ins 1..9,
	M <== matrix( Els, [dim=[9,9]] ),
	% select rows
	foreach( I in 0..8 , all_different(M[I,_]) ),
	% select cols
	foreach( J in 0..8,  all_different(M[_,J]) ),
	% select squares
	foreach( [I,J] ins 0..2 ,
           all_different(M[I*3+(0..2),J*3+(0..2)]) ),
	ex(Ex, Els),
%	maplist( bound, Els, Exs),
	labeling( [], Els ).


% The gecode interface doesn't support wake-ups on binding constained variables, this is the closest.
%
bound(El, X) :-
	( nonvar(X) -> El #= X ; true ).

%
% output using matrix library
%
output(Els) :-
	M <== matrix( Els, [dim=[9,9]] ),
	foreach( I in 0..2 , output(M, I) ),
	output_line.

output(M, I) :-
	output_line,
	foreach( J in 0..2 , output_row(M, J+I*3) ).

output_row( M, Row ) :-
	L <== M[Row,_],
	format('| ~d ~d ~d | ~d ~d ~d | ~d ~d ~d |~n', L).

output_line :-
	format(' ~|~`-t~24+~n', []).

ex( 1, [
            _,6,_,1,_,4,_,5,_,
            _,_,8,3,_,5,6,_,_,
            2,_,_,_,_,_,_,_,1,
            8,_,_,4,_,7,_,_,6,
            _,_,6,_,_,_,3,_,_,
            7,_,_,9,_,1,_,_,4,
            5,_,_,_,_,_,_,_,2,
            _,_,7,2,_,6,9,_,_,
            _,4,_,5,_,8,_,7,_
            ] ).


ex(2,  [
            _,_,1,_,8,_,6,_,4,
            _,3,7,6,_,_,_,_,_,
            5,_,_,_,_,_,_,_,_,
            _,_,_,_,_,5,_,_,_,
            _,_,6,_,1,_,8,_,_,
            _,_,_,4,_,_,_,_,_,
            _,_,_,_,_,_,_,_,3,
            _,_,_,_,_,7,5,2,_,
            8,_,2,_,9,_,7,_,_
          ] ).
