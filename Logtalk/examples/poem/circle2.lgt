
/* Circle is a special form of ellipse                                  */
/* Subclasses ('circle' here) must have the same number of arguments    */
/* as their superclass ('ellipse') for the superclass predicates to     */
/* be applicable.  The arguments may be renamed for clarity.            */

:- object(circle(Center, Radius),
	extends(ellipse(Center, Radius, Radius))).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles.',
		parnames is ['Center', 'Radius'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).


	:- public(circumference/1).
	:- mode(circumference(-number), one).


	circumference(Circumference) :-
		pi(Pi),
		parameter(2, Radius),
		Circumference is 2*Pi*Radius.


	pi(3.14196).


:- end_object.
