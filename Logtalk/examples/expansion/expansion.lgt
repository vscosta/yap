
:- category(number_conversion,
	implements(term_expansionp)).

	term_expansion(0, zero).
	term_expansion(1, one).
	term_expansion(2, two).
	term_expansion(3, three).
	term_expansion(4, four).
	term_expansion(5, five).
	term_expansion(6, six).
	term_expansion(7, seven).
	term_expansion(8, eight).
	term_expansion(9, nine).

:- end_category.


:- category(conversion_test).

	:- public(test/2).

	test(Term, Expansion) :-
		expand_term(Term, Expansion).

:- end_category.


:- object(exp_public,
	imports(public::number_conversion)).

:- end_object.


:- object(desc_public,
	imports(conversion_test),
	extends(exp_public)).

:- end_object.


:- object(exp_protected,
	imports(protected::number_conversion)).

:- end_object.


:- object(desc_protected,
	imports(conversion_test),
	extends(exp_protected)).

:- end_object.


:- object(exp_private,
	imports(private::number_conversion)).

:- end_object.


:- object(desc_private,
	imports(conversion_test),
	extends(exp_private)).

:- end_object.
