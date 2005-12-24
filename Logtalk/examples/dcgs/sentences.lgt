

% Categories allows us to neatly organize the different "kinds"
% of words on this example: determiners, nouns, and verbs


:- category(determiners).

	:- private(determiner//0).	% private category non-terminals become private
								% non-terminals of the objects importing the category
	determiner --> [the].
	determiner --> [a].

:- end_category.


:- category(nouns).

	:- private(noun//0).

	noun --> [boy].
	noun --> [girl].

:- end_category.


:- category(verbs).

	:- private(verb//0).

	verb --> [likes].
	verb --> [hates].

:- end_category.


:- object(sentence,
	implements(parsep),
	imports(determiners, nouns, verbs)).

	parse(List, true) :-
		phrase(sentence, List).
	parse(_, false).

	sentence --> noun_phrase, verb_phrase.

	noun_phrase --> ::determiner, ::noun.	% the ::/1 control construct is used to call grammar
	noun_phrase --> ::noun.					% rules encapsulated on the imported categories

	verb_phrase --> ::verb.
	verb_phrase --> ::verb, noun_phrase.

:- end_object.
