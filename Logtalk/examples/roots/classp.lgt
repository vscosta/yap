
:- protocol(classp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default protocol for all instantiable classes.']).


	:- public(new/1).

	:- mode(new(?object), zero_or_one).

	:- info(new/1, [
		comment is 'Creates a new instance.',
		argnames is ['Instance']]).


	:- public(new/2).

	:- mode(new(?object, +list), zero_or_one).

	:- info(new/2, [
		comment is 'Creates a new instance using a list of initialization options.',
		argnames is ['Instance', 'Options']]).


	:- public(clone/2).

	:- mode(clone(+object, ?object), zero_or_one).

	:- info(clone/2, [
		comment is 'Clones an instance.',
		argnames is ['Instance', 'Clone']]).


	:- public(instance_base_name/1).

	:- mode(instance_base_name(-atom), one).

	:- info(instance_base_name/1, [
		comment is 'Base name to generated new instance names.',
		argnames is ['Name']]).


	:- public(delete/1).

	:- mode(delete(+object), zero_or_one).

	:- info(delete/1, [
		comment is 'Deletes a dynamic instance.',
		argnames is ['Instance']]).


	:- public(delete/2).

	:- mode(delete(+object, +list), zero_or_one).

	:- info(delete/2, [
		comment is 'Deletes a dynamic instance using a list of deleting options.',
		argnames is ['Instance', 'Options']]).


	:- public(delete_all/0).

	:- mode(delete_all, zero_or_one).

	:- info(delete_all/0, [
		comment is 'Deletes all dynamic instances. Fails if some dynamic instance can not be deleted.']).


	:- public(delete_all/1).

	:- mode(delete_all(+list), zero_or_one).

	:- info(delete_all/1, [
		comment is 'Deletes all dynamic instances using a list of deleting options. Fails if some dynamic instance can not be deleted.',
		argnames is ['Options']]).


	:- public(equals/2).

	:- mode(equals(+object, +object), zero_or_one).

	:- info(equals/2, [
		comment is 'The two instances represents the same object for some definition of equality.',
		argnames is ['Instance1', 'Instance2']]).


:- end_protocol.
