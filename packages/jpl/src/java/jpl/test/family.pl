% a simple database for Family.java

:- if(current_prolog_flag(dialect, yap)).
sleep(T) :- unix(sleep(T)).
:- endif.

child_of( joe, ralf ).
child_of( mary, joe ).
child_of( steve, joe ).

descendent_of( X, Y ) :-
	child_of( X, Y ).
descendent_of( X, Y ) :-
	child_of( Z, Y ),
	descendent_of( X, Z ).

p( A, B) :-
	(	q( A, B)
	->	write( 'OK'-q(A,B)), nl
	;	write( 'FAIL'-q(A,B)), nl
	).

q( 3, 4).

r( 5, 5).
