
:- protocol(event_dbgp).


	:- info([
		version is 1.0,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Debugging protocol similar to those found in most Prolog compilers.']).


	:- public(trace/0).

	:- mode(trace, one).

	:- info(trace/0, [
		comment is 'Starts tracing all message sending events.']).


	:- public(notrace/0).

	:- mode(notrace, one).

	:- info(notrace/0, [
		comment is 'Stops tracing.']).


	:- public(debug/0).

	:- mode(debug, one).

	:- info(debug/0, [
		comment is 'Activates spy points and starts debugging.']).


	:- public(nodebug/0).

	:- mode(nodebug, one).

	:- info(nodebug/0, [
		comment is 'Suspends spy points and stops debugging.']).


	:- public(debugging/0).

	:- mode(debugging, zero_or_one).

	:- info(debugging/0, [
		comment is 'True if the debugger is active.']).


	:- public(init/0).

	:- mode(init, one).

	:- info(init/0, [
		comment is 'Initializes debugger, turns debugging off and resets all spy points and streams.']).


	:- public(stream/2).

	:- mode(stream(?atom, ?stream), zero_or_more).

	:- info(stream/2, [
		comment is 'Current debugger input or ouput streams.',
		argnames is ['Kind', 'Stream']]).


	:- public(set_stream/2).

	:- mode(set_stream(+atom, +stream), one).

	:- info(set_stream/2, [
		comment is 'Sets the debugger input or output stream.',
		argnames is ['Kind', 'Stream']]).


:- end_protocol.
