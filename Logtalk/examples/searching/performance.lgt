
:- object(performance,
	implements(event_handlersp)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Performance monitor for state space searches.']).


	:- uses(event_registry).
	:- uses(before_event_registry).
	:- uses(after_event_registry).
	:- uses(list).
	:- uses(numberlist).
	:- uses(time).


	:- private(transitions/3).
	:- dynamic(transitions/3).
	:- mode(transitions(?state, ?state, ?integer), zero_or_more).

	:- private(solution_length/1).
	:- dynamic(solution_length/1).
	:- mode(solution_length(?integer), zero_or_one).

	:- private(start_time/1).
	:- dynamic(start_time/1).
	:- mode(start_time(-number), zero_or_one).

	:- private(end_time/1).
	:- dynamic(end_time/1).
	:- mode(end_time(-number), zero_or_one).

	:- public(time/1).
	:- mode(time(-number), zero_or_one).

	:- public(transitions/1).
	:- mode(transitions(-number), zero_or_one).

	:- public(branching/3).
	:- mode(branching(-integer, -float, -integer), zero_or_one).

	:- public(report/0).
	:- mode(report, zero_or_one).

	:- public(init/0).
	:- mode(init, one).

	:- public(stop/0).
	:- mode(stop, one).


	report :-
		::solution_length(Length),
		::transitions(Number),
		Ratio is Length / Number,
		::branching(Minimum, Average, Maximum),
		::time(Time),
		write('solution length: '), write(Length), nl,
		write('state transitions: '), write(Number), nl,
		write('ratio solution length / state transitions: '), write(Ratio), nl,
		write('minimum branching degree: '), write(Minimum), nl,
		write('average branching degree: '), write(Average), nl,
		write('maximum branching degree: '), write(Maximum), nl,
		write('time: '), write(Time), nl,
		::retractall(transitions(_, _, _)).		% clean up for next solution


	transitions(Number) :-
		findall(N, ::transitions(_, _, N), List),
		numberlist::sum(List, Number).


	time(Time) :-
		::start_time(Start),
		::end_time(End),
		Time is End - Start.


	branching(Minimum, Average, Maximum) :-
		findall(
			Length, 
			(::transitions(State1, _, _),
			 findall(State2, ::transitions(State1, State2, _), States2),
			 list::length(States2, Length)),
			Lengths),
		list::min(Lengths, Minimum),
		list::max(Lengths, Maximum),
		numberlist::sum(Lengths, Sum),
		list::length(Lengths, Length),
		Average is Sum / Length.


	init :-
		self(Self),
		event_registry::set_monitor(_, solve(_, _, _), _, Self),
		after_event_registry::set_monitor(_, next_state(_, _), _, Self),
		event_registry::set_monitor(_, solve(_, _, _, _), _, Self),
		after_event_registry::set_monitor(_, next_state(_, _, _), _, Self),
		::retractall(transitions(_, _, _)),
		::retractall(start_time(_)),
		::retractall(end_time(_)),
		::retractall(solution_length(_)).


	stop :-
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self).


	before(_, solve(_, _, _), _) :-
		!,
		time::cpu_time(Time),
		::retractall(start_time(_)),
		::asserta(start_time(Time)).

	before(_, solve(_, _, _, _), _) :-
		!,
		time::cpu_time(Time),
		::retractall(start_time(_)),
		::asserta(start_time(Time)).


	after(_, next_state(S1, S2), _) :-
		!,
		(::transitions(S1, S2, N) ->
			N2 is N + 1
			;
			N2 is 1),
		 ::retractall(transitions(S1, S2, _)),
		 ::assertz(transitions(S1, S2, N2)).

	after(_, next_state(S1, S2, _), _) :-
		!,
		(::transitions(S1, S2, N) ->
			N2 is N + 1
			;
			N2 is 1),
		 ::retractall(transitions(S1, S2, _)),
		 ::assertz(transitions(S1, S2, N2)).

	after(_, solve(_, _, Solution), _) :-
		!,
		time::cpu_time(Time),
		::retractall(end_time(_)),
		::asserta(end_time(Time)),
		list::length(Solution, Length),
		::retractall(solution_length(_)),
		::asserta(solution_length(Length)).

	after(_, solve(_, _, Solution, _), _) :-
		!,
		time::cpu_time(Time),
		::retractall(end_time(_)),
		::asserta(end_time(Time)),
		list::length(Solution, Length),
		::retractall(solution_length(_)),
		::asserta(solution_length(Length)).


:- end_object.
