
:- object(sentence,
	implements(parsep),
	imports(determiners, nouns, verbs)).


	parse(List, true) :-
		phrase(sentence, List).

	parse(_, false).


	sentence --> noun_phrase, verb_phrase.

	noun_phrase --> ::determiner, ::noun.
	noun_phrase --> ::noun.

	verb_phrase --> ::verb.
	verb_phrase --> ::verb, noun_phrase.


:- end_object.
