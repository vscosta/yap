
:- object(joeFilmEnthusiast,
	extends(joePerson)).


	:- public(favActor/1).

	:- public(favFilm/1).

	:- public(favDirector/1).


	favActor('Fred Filistone').


	favFilm('The Wizard of Oz').


	favDirector('Krzystof Kieslowski').


:- end_object.
