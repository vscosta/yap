

:- style_check(all).

:- use_module(library(gecode/clpfd)).
:- use_module(library(maplist)).
:- use_module(library(matrix)).

main :-
    problem(Ex, My),
	fail.
main.

sudoku( Ex, Els ) :-
	problem(Ex, Els),
	output(Els).

%
% gecode constraints
%
problem(Ex, Els) :-
    ex(Ex,Info),
     gecode_clpfd:init_gecode(Space, Me),
   length(Els, 81),
    Els ins 1..9,
    M <== matrix[9,9] of Els,
    % select rows
    Z <== zeros[9],
    Q <== zeros[3,3],
    matrix_map( row(M), Z ),
    matrix_map( col(M), Z ),
    matrix_map(sqr(M), Q ), 
    maplist(bind,Els,Info),
    %	maplist( bound, Els, Exs),
    labeling( [], Els ),
 gecode_clpfd:close_gecode(Space, Els, Me).

bind(V,C) :- number(C), !,V #= C.
bind(V,V).

sqr(M,Q,[I,J]) :-
    matrix_foldl(sqrdiff(M,[I,J]),Q,[],Els),
all_different(Els).

sqrdiff(M,[I,J],_,Els,[V|Els],[A,B]) :-
    V <== M[I*3+A, J*3+B].

row(M, Z, [I]) :-
    matrix_foldl(col1(M,I),Z,[],Vs),
    all_different(Vs).

col1(M,I,_Z,Els,[V|Els],[J]) :-
    V <== M[I,J].

col(M, Z, [I]) :-
    matrix_foldl(row1(M,I),Z,[],Vs),
    all_different(Vs).

row1(M,I,_Z,Els,[V|Els],[J]) :-
    V <== M[J,I].

% The gecode interface doesn't support wake-ups on binding constained variables, this is the closest.
%
bound(El, X) :-
	( nonvar(X) -> El #= X ; true ).

%
% output using matrix library
%
output(Els) :-
    L <== Els.list(),
    writeln(L).

/*
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
*/

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
