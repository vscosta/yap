
:- object(event_dbg,
	implements(event_dbgp, event_handlersp),
	imports(monitor)).


	:- info([
		version is 1.0,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Debugging facilities similar to those found in most Prolog compilers.']).


	:- initialization(::init).


	:- protected(port_output/4).

	:- mode(port_output(+atom, +object, @callable, +object), one).

	:- info(port_output/4, [
		comment is 'Outputs current port information.',
		argnames is ['Port', 'Object', 'Message', 'Sender']]).


	:- protected(execute_option/1).

	:- mode(execute_option(+atom), one).

	:- info(execute_option/1, [
		comment is 'Executes a user option at a debugger port.',
		argnames is ['Option']]).


	:- protected(query_user/1).

	:- mode(query_user(-atom), one).

	:- info(query_user/1, [
		comment is 'Query a user about an option at a debugger port.',
		argnames is ['Option']]).


	:- private(stream_/2).
	:- dynamic(stream_/2).

	:- mode(stream_(?atom, ?stream), zero_or_more).

	:- info(stream/2, [
		comment is 'Stores the current debugger input and ouput streams.',
		argnames is ['Kind', 'Stream']]).


	stream(Name, Stream) :-
		::stream_(Name, Stream).


	set_stream(Name, Stream) :-
		::retractall(stream_(Name, _)),
		::assertz(stream_(Name, Stream)).


	trace :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self),
		define_events(before, _, _, _, Self),
		define_events(after, _, _, _, Self).


	notrace :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self).


	debugging :-
		::monitor_activated.


	debug :-
		::activate_monitor.


	nodebug :-
		::suspend_monitor.


	port_output(Port, Object, Message, Sender) :-
		::stream(output, Output),
		write(Output, Port),
		write(Output, ':     '),
		writeq(Output, Object),
		write(Output, ' <- '),
		writeq(Output, Message),
		write(Output, ' from '),
		writeq(Output, Sender),
		nl(Output).


	query_user(Option) :-
		::stream(output, Output),
		::stream(input, Input),
		repeat,
			write(Output, '    >> '),
			read(Input, Option),
			nl(Output),
			(valid_option(Option) ->
				true
				;
				::execute_option(h), fail),
		!.


	execute_option(c).

	execute_option(f) :-
		!, fail.

	execute_option(n) :-
		::nodebug.

	execute_option(b) :-
		::stream(output,Output),
		::stream(input, Input),
		repeat,
			write(Output, '     :- '),
			read(Input, Goal),
			writeq(Output, Goal),
			nl(Output),
			(once(Goal) ->
				write(Output, '     answer: '),
				writeq(Output, Goal), nl(Output)
				;
				write(Output, '     no'), nl(Output)),
		Goal = true,
		!.

	execute_option(a) :-
		throw(error(logtalk_execution_aborted)).

	execute_option(h) :-
		::stream(output, Output),
		write(Output, '     Available options are:'), nl(Output),
		write(Output, '       c - creep (go on)'), nl(Output),
		write(Output, '       f - fail (force failure or backtracking)'), nl(Output),
		write(Output, '       n - nodebug (turn off debug)'), nl(Output),
		write(Output, '       b - break (submit queries to the interpreter, type true to terminate)'), nl(Output),
		write(Output, '       a - abort (return to top level interpreter)'), nl(Output),
		write(Output, '       h - help (prints this list of options)'), nl(Output),
		nl(Output).


	valid_option(c).
	valid_option(f).
	valid_option(n).
	valid_option(b).
	valid_option(a).


	before(Object, Message, Sender) :-
		::port_output(call, Object, Message, Sender),
		::query_user(Option),
		::execute_option(Option).


	after(Object, Message, Sender) :-
		::port_output(exit, Object, Message, Sender),
		::query_user(Option),
		::execute_option(Option).


	init :-
		::reset_monitor,
		current_input(Input),
		::set_stream(input, Input),
		current_output(Output),
		::set_stream(output, Output).


:- end_object.
