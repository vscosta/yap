
:- category(engine).

	:- public(prove/1).

	:- public(if/1).
	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(100, xfx, then).

	prove(true) :-
		!.

	prove([]) :-
		!.
	prove([Cond| Conds]) :-
		!,
		prove(Cond),
		prove(Conds).

	prove(Fact) :-
		::clause(Fact, true).
	prove(Conclusion) :-
		::clause(if Conds then Conclusion, true),
		prove(Conds).

:- end_category.


:- object(rules,
	imports(engine)).

	:- public([weather/1, weekday/1, time/1, goto/1]).
	:- dynamic([weather/1, weekday/1, time/1, goto/1]).

	:- dynamic(if/1).

	:- op(200,  fx, if).
	:- op(100, xfx, then).

	if [weather(sunny), weekday(weekend), time(day)] then goto(beach).
	if [weather(raining), weekday(weekend), time(night)] then goto(cinema).
	if [weekday(workday), time(day)] then goto(work).

	weather(raining).
	weekday(weekend).
	time(night).

:- end_object.
