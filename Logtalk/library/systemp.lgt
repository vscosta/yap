
:- protocol(systemp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2002/8/4,
		comment is 'Operating system protocol.']).


	:- public(make_directory/1).

	:- mode(make_directory(+atom), zero_or_one).

	:- info(make_directory/1, [
		comment is 'Make a new directory.',
		argnames is ['Directory']]).


	:- public(delete_directory/1).

	:- mode(delete_directory(+atom), zero_or_one).

	:- info(delete_directory/1, [
		comment is 'Delete a directory.',
		argnames is ['Directory']]).


	:- public(change_directory/1).

	:- mode(change_directory(+atom), zero_or_one).

	:- info(change_directory/1, [
		comment is 'Change working directory.',
		argnames is ['Directory']]).


	:- public(working_directory/1).

	:- mode(working_directory(?atom), zero_or_one).

	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory']]).


	:- public(directory_files/2).

	:- mode(directory_files(+atom, -list), zero_or_one).

	:- info(directory_files/2, [
		comment is 'List of all directory files.',
		argnames is ['Directory', 'Files']]).


	:- public(file_exists/1).

	:- mode(file_exists(+atom), zero_or_one).

	:- info(file_exists/1, [
		comment is 'True if the specified file exists.',
		argnames is ['File']]).


	:- public(file_property/2).

	:- mode(file_property(+atom, ?nonvar), zero_or_more).

	:- info(file_property/2, [
		comment is 'Access to file properties. Fails if the file does not exist or is not user readable.',
		argnames is ['File', 'Property']]).


	:- public(delete_file/1).

	:- mode(delete_file(+atom), zero_or_one).

	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File']]).


	:- public(rename_file/2).

	:- mode(rename_file(+atom, +atom), zero_or_one).

	:- info(rename_file/2, [
		comment is 'Renames a file. Fails if the file does not exist or cannot be renamed.',
		argnames is ['Old', 'New']]).


	:- public(environment/2).

	:- mode(environment(?atom, ?atom), zero_or_more).

	:- info(environment/2, [
		comment is 'Environment variable value.',
		argnames is ['Variable', 'Value']]).


	:- public(date_time/6).

	:- mode(date_time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).

	:- info(date_time/6, [
		comment is 'System date and time.',
		argnames is ['Year', 'Month', 'Day', 'Hour', 'Min', 'Sec']]).


:- end_protocol.
