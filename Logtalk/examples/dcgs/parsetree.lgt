
:- object(parsetree,
	implements(parsep)).


	parse(List, Tree) :-
		phrase(sentence(Tree), List).


	sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

	noun_phrase(np(D,NP)) --> determiner(D), noun(NP).
	noun_phrase(NP) --> noun(NP).

	verb_phrase(vp(V)) --> verb(V).
	verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).


	determiner(d(the)) --> [the].
	determiner(d(a)) --> [a].

	noun(n(boy)) --> [boy].
	noun(n(girl)) --> [girl].

	verb(v(likes)) --> [likes].
	verb(v(hates)) --> [hates].


:- end_object.
