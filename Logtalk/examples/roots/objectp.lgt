
:- protocol(objectp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default protocol for all objects.']).


	:- public(strict_instance/0).

	:- mode(strict_instance, zero_or_one).

	:- info(strict_instance/0, [
		comment is 'True if the object is strictly an instance.']).


	:- public(print/0).

	:- mode(print, one).

	:- info(print/0, [
		comment is 'Pretty prints an object description.']).


	:- public(nil/0).

	:- mode(nil, zero_or_one).

	:- info(nil/0, [
		comment is 'True if the object represents a void reference.']).


:- end_protocol.
