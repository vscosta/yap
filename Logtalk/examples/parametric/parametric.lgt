
/*	This example illustrates how to associate a set of predicates with a 
	compound term.   Parameters can be accessed from within an object by 
	using the execution-context built-in methods this/1 and parameter/2; 
	both alternatives are illustrated below.
*/



/* The first two parametric objects represent time and date values as 
	compound terms using the object's identifiers.
*/


:- object(date(_Year, _Month, _Day)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'Dates as parametric objects.',
		parnames is ['Year', 'Month', 'Day']]).

	:- public(year/1).
	:- mode(year(?integer), one).

	:- public(month/1).
	:- mode(month(?integer), one).

	:- public(day/1).
	:- mode(day(?integer), one).

	:- public(today/0).
	:- mode(today, one).

	:- public(leap_year/0).
	:- mode(leap_year, zero_or_one).

	year(Year) :-
		parameter(1, Year).

	month(Month) :-
		parameter(2, Month).

	day(Day) :-
		parameter(3, Day).

	today :-
		{'$lgt_current_date'(Year, Month, Day)},	% defined in the config files
		parameter(1, Year),
		parameter(2, Month),
		parameter(3, Day).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	year(Year) :-
		this(date(Year, _, _)).

	month(Month) :-
		this(date(_, Month, _)).

	day(Day) :-
		this(date(_, _, Day)).

	today :-
		{'$lgt_current_date'(Year, Month, Day)},	% defined in the config files
		this(date(Year, Month, Day)).

*/

	leap_year :-
		parameter(1, Year),
		(0 =:= mod(Year, 4), 0 =\= mod(Year, 100)
		 ;
		 0 =:= mod(Year, 400)),
		!.

:- end_object.


:- object(time(_Hours, _Mins, _Secs)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'Time as parametric objects.',
		parnames is ['Hours', 'Mins', 'Secs']]).

	:- public(hours/1).
	:- mode(hours(?integer), one).

	:- public(mins/1).
	:- mode(mins(?integer), one).

	:- public(secs/1).
	:- mode(secs(?integer), one).

	:- public(now/0).
	:- mode(now, one).

	hours(Hours) :-
		parameter(1, Hours).

	mins(Mins) :-
		parameter(2, Mins).

	secs(Secs) :-
		parameter(3, Secs).

	now :-
		{'$lgt_current_time'(Hours, Mins, Secs)},	% defined in the config files
		parameter(1, Hours),
		parameter(2, Mins),
		parameter(3, Secs).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	hours(Hours) :-
		this(time(Hours, _, _)).

	mins(Mins) :-
		this(time(_, Mins, _)).

	secs(Secs) :-
		this(time(_, _, Secs)).

	now :-
		{'$lgt_current_time'(Hours, Mins, Secs)},	% defined in the config files
		this(time(Hours, Mins, Secs)).

*/

:- end_object.



/*	The following parametric object illustrates a solution for implementing 
	modifiable object state. The idea is to represent object state by using 
	object parameters, defining "setter" predicates/methods that return the 
	updated object identifier.
*/

:- object(rectangle(_width, _height, _x, _y)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/9/5,
		comment is 'A simple implementation of a geometric rectangle using parametric objects.',
		parnames is ['Width', 'Height', 'X', 'Y']]).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0,
		[comment is 'Initialize rectangle position.']).

	:- public(area/1).
	:- mode(area(-integer), one).
	:- info(area/1,
		[comment is 'Rectangle area.',
		 argnames is ['Area']]).

	:- public(move/3).
	:- mode(move(+integer, +integer, -compound), one).
	:- info(move/3, [
		comment is 'Moves a rectangle to a new position, returning the updated rectangle.',
		argnames is ['X', 'Y', 'NewRectangle']]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Rectangle current position.',
		argnames is ['X', 'Y']]).

	init :-
		parameter(1, 2),	% Width
		parameter(2, 1),	% Height
		parameter(3, 0),	% X
		parameter(4, 0).	% Y

	area(Area) :-
		parameter(1, Width),
		parameter(2, Height),
		Area is Width*Height.

	move(X, Y, rectangle(Width, Height, X, Y)) :-
		parameter(1, Width),
		parameter(2, Height).

	position(X, Y) :-
		parameter(3, X),
		parameter(4, Y).

/*	Alternative predicate definitions using this/1 instead of parameter/2
	(see the User Manual for the pros and cons of both alternatives):

	init :-
		this(rectangle(2, 1, 0, 0)).

	area(Area) :-
		this(rectangle(Width, Height, _, _)),
		Area is Width*Height.

	move(X, Y, rectangle(Width, Height, X, Y)) :-
		this(rectangle(Width, Height, _, _)).

	position(X, Y) :-
		this(rectangle(_, _, X, Y)).

*/

:- end_object.
