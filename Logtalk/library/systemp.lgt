
:- protocol(systemp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
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


	:- public(delete_file/1).

	:- mode(delete_file(?atom), zero_or_one).

	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File']]).


	:- public(rename_file/1).

	:- mode(rename_file(?atom), zero_or_one).

	:- info(rename_file/1, [
		comment is 'Renames a file.',
		argnames is ['File']]).


:- end_protocol.
