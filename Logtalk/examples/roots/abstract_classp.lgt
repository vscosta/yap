
:- protocol(abstract_classp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default protocol for all abstract classes.']).


	:- public(metaclass/0).

	:- mode(metaclass, zero_or_one).

	:- info(metaclass/0, [
		comment is 'True if the object is a metaclass.']).


	:- public(abstract_class/0).

	:- mode(abstract_class, zero_or_one).

	:- info(metaclass/0, [
		comment is 'True if the object is an abstract class.']).


:- end_protocol.
