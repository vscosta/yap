
:- object(stack_monitor,
	implements(event_handlersp)).


	:- info([
		version is 1.0,
		date is 1998/3/23,
		authors is 'Paulo Moura',
		comment is 'Monitor for brick movements printing an ascii representation of each brick position.']).


	:- uses(loop).
	:- uses(list).


	after(_, move(_, _), _) :-
		findall(
			(Brick, X, Y),
			(instantiates_class(Brick, brick), Brick::position(X, Y)),
			Bricks),
		setof(X, Brick^Y^ (list::member((Brick,X,Y), Bricks)), Xs),
		list::last(Xs, Xmax),
		setof(Y, Brick^X^ (list::member((Brick,X,Y), Bricks)), Ys),
		list::last(Ys, Ymax),
		loop::fordownto(Y, Ymax, 1,
			(write('|'),
			 loop::forto(X, 1, Xmax,
        		(list::member((Brick, X, Y), Bricks) ->
            		write(Brick)
            		;
            		write('.'))),
			 nl)),
		write('-'),
		loop::forto(X, 1, Xmax, write('-')), nl.


:- end_object.
