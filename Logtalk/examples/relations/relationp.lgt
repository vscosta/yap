
:- protocol(relationp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Relations between objects protocol.']).


	:- private(tuple_/1).
	:- dynamic(tuple_/1).

	:- mode(tuple_(?list), zero_or_more).

	:- info(tuple_/1, [
		comment is 'Stores the relation tuples.',
		argnames is ['Tuple']]).


	:- public(tuple/1).

	:- mode(tuple(?list), zero_or_more).

	:- info(tuple/1, [
		comment is 'Returns a relation tuple.',
		argnames is ['Tuple']]).


	:- public(tuples/1).

	:- mode(tuples(-list), one).

	:- info(tuples/1, [
		comment is 'Returns a list of all relation tuples.',
		argnames is ['Tuples']]).


	:- public(add_tuple/1).

	:- mode(add_tuple(+list), zero_or_one).

	:- info(add_tuple/1, [
		comment is 'Adds a new relation tuple.',
		argnames is ['Tuple']]).


	:- public(remove_tuple/1).

	:- mode(remove_tuple(?list), zero_or_more).

	:- info(remove_tuple/1, [
		comment is 'Removes a matching relation tuple.',
		argnames is ['Tuple']]).


	:- public(remove_all_tuples/0).

	:- mode(remove_all_tuples, one).

	:- info(remove_all_tuples/0, [
		comment is 'Removes all relation tuples.']).


	:- public(number_of_tuples/1).

	:- mode(number_of_tuples(-integer), one).

	:- info(number_of_tuples/1, [
		comment is 'Returns the current number of relation tuples.',
		argnames is ['Number']]).


	:- public(plays_role_in_tuple/3).

	:- mode(plays_role_in_tuple(?object, ?atom, ?list), zero_or_more).

	:- info(plays_role_in_tuple/3, [
		comment is 'List of tuples where an object plays a role.',
		argnames is ['Object', 'Role', 'Tuples']]).


	:- public(plays_roles/2).

	:- mode(plays_roles(?object, -list), zero_or_more).

	:- info(plays_roles/2, [
		comment is 'Returns a list of all roles played by an object in the relation tuples.',
		argnames is ['Object', 'Roles']]).


	:- public(plays_role_n_times/3).

	:- mode(plays_role_n_times(?object, ?atom, -integer), zero_or_more).

	:- info(plays_role_n_times/3, [
		comment is 'Number of times that an object plays a role in the relation tuples.',
		argnames is ['Object', 'Role', 'Times']]).


	:- private(domain_/2).
	:- dynamic(domain_/2).

	:- mode(domain_(?atom, ?object), zero_or_more).

	:- info(domain_/2, [
		comment is 'Table of role domains.',
		argnames is ['Role', 'Domain']]).


	:- public(domains/1).

	:- mode(domains(-list), zero_or_one).

	:- info(domains/1, [
		comment is 'List of domains for all roles.',
		argnames is ['Domains']]).


	:- public(domain/2).

	:- mode(domain(?atom, ?object), zero_or_more).

	:- info(domain/2, [
		comment is 'Role domain.',
		argnames is ['Role', 'Domain']]).


	:- public(set_domains/1).

	:- mode(set_domains(+list), zero_or_one).

	:- info(set_domains/1, [
		comment is 'Set the domains for all roles.',
		argnames is ['Domains']]).


	:- private(descriptor_/1).
	:- dynamic(descriptor_/1).

	:- mode(descriptor_(?list), zero_or_one).

	:- info(descriptor_/1, [
		comment is 'Stores the relation tuple descriptor.',
		argnames is ['Descriptor']]).


	:- public(descriptor/1).

	:- mode(descriptor(?list), zero_or_one).

	:- info(descriptor/1, [
		comment is 'Returns the relation tuple descriptor.',
		argnames is ['Descriptor']]).


	:- public(degree/1).

	:- mode(degree(?integer), zero_or_one).

	:- info(degree/1, [
		comment is 'Descriptor length.',
		argnames is ['Degree']]).


	:- public(set_descriptor/1).

	:- mode(set_descriptor(+list), zero_or_one).

	:- info(set_descriptor/1, [
		comment is 'Sets the relation tuple descriptor.',
		argnames is ['Descriptor']]).


	:- private(key_/1).
	:- dynamic(key_/1).

	:- mode(key_(?list), zero_or_more).

	:- info(key_/1, [
		comment is 'Stores the relation keys.',
		argnames is ['Key']]).


	:- public(key/1).

	:- mode(key(?list), zero_or_more).

	:- info(key/1, [
		comment is 'Returns a relation key.',
		argnames is ['Key']]).


	:- public(keys/1).

	:- mode(keys(-list), one).

	:- info(keys/1, [
		comment is 'Returns a list of all relation keys.',
		argnames is ['Keys']]).


	:- public(set_keys/1).

	:- mode(set_keys(+list), zero_or_one).

	:- info(set_keys/1, [
		comment is 'Sets the relation keys.',
		argnames is ['Keys']]).


	:- private(delete_option_/2).
	:- dynamic(delete_option_/2).

	:- mode(delete_option_(?atom, ?atom), zero_or_more).

	:- info(delete_option_/2, [
		comment is 'Stores role delete options.',
		argnames is ['Role', 'Option']]).


	:- public(delete_options/1).

	:- mode(delete_options(-list), zero_or_one).

	:- info(delete_options/1, [
		comment is 'Returns a list of all role - delete option pairs.',
		argnames is ['Options']]).


	:- public(delete_option/2).

	:- mode(delete_option(?atom, ?atom), zero_or_more).

	:- info(delete_option/2, [
		comment is 'Returns role delete options.',
		argnames is ['Role', 'Option']]).


	:- public(set_delete_options/1).

	:- mode(set_delete_options(+list), zero_or_one).

	:- info(set_delete_options/1, [
		comment is 'Sets the roles delete options.',
		argnames is ['Options']]).


	:- private(cardinality_/3).
	:- dynamic(cardinality_/3).

	:- mode(cardinality_(?atom, ?nonvar, ?nonvar), zero_or_more).

	:- info(cardinality_/3, [
		comment is 'Table of roles minimum and maximum cardinalities.',
		argnames is ['Role', 'Min', 'Max']]).


	:- public(cardinalities/1).

	:- mode(cardinalities(-list), zero_or_one).

	:- info(cardinalities/1, [
		comment is 'List of minimum and maximum cardinalities of all roles.',
		argnames is ['Cardinalities']]).


	:- public(cardinality/3).

	:- mode(cardinality(?atom, ?nonvar, ?nonvar), zero_or_more).

	:- info(cardinality/3, [
		comment is 'Role minimum and maximum cardinalities.',
		argnames is ['Role', 'Min', 'Max']]).


	:- public(set_cardinalities/1).

	:- mode(set_cardinalities(+list), zero_or_one).

	:- info(cardinalities/1, [
		comment is 'Sets the minimum and maximum cardinalities of all roles.',
		argnames is ['Cardinalities']]).


	:- protected(set_monitors/1).
	:- mode(set_monitors(+list), one).

	:- protected(del_all_monitors/0).
	:- mode(del_all_monitors, one).

	:- protected(del_monitors/1).
	:- mode(del_monitors(+list), one).

	:- protected(restore_monitors/0).
	:- mode(restore_monitors, zero_or_one).


:- end_protocol.
