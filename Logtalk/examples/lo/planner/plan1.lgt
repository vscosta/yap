
:- object(plan(_)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Air-line trip planner.',
		parnames is ['Mode'],
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- public(from/3).

	:- mode(from(+atom, +atom, -list), zero_or_more).

	:- info(from/3,
		[comment is 'Plan a trip from Start to Destination.',
		 argnames is ['Start', 'Destination', 'Plan']]).


	from(Start, Destination, Plan) :-
		from(Start, Destination, [], Plan).


	from(Start, Destination, _, [Step]) :-
		parameter(1, Mode),
		Mode::step(Start, Destination, Step),
		!.

	from(Start, Destination, Locations, [Step| Steps]) :-
		parameter(1, Mode),
		Mode::step(Start, City2, Step),
		not_member(City2, Locations),
		from(City2, Destination, [Start| Locations], Steps).


	not_member(_, []).
	
	not_member(City, [Location| Locations]) :-
		City \= Location,
		not_member(City, Locations).


:- end_object.
