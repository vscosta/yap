
:- protocol(systemp).

	:- info([
		version is 1.9,
		author is 'Portable Operating-System Interface (POSI) initiative',
		date is 2004/7/27,
		comment is 'Portable operating system access protocol.']).

	:- public(make_directory/1).
	:- mode(make_directory(+atom), one).
	:- info(make_directory/1, [
		comment is 'Makes a new directory.',
		argnames is ['Directory'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for making a new directory' - permission_error(write, 'Directory')]]).

	:- public(delete_directory/1).
	:- mode(delete_directory(+atom), one).
	:- info(delete_directory/1, [
		comment is 'Deletes a directory (and all of its contents).',
		argnames is ['Directory'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for deleting the directory' - permission_error(write, 'Directory'),
			'Directory does not exists' - existence_error(directory, 'Directory')]]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), one).
	:- info(change_directory/1, [
		comment is 'Changes current working directory.',
		argnames is ['Directory'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for accessing the directory' - permission_error(read, 'Directory'),
			'Directory does not exists' - existence_error(directory, 'Directory')]]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory (as an absolute file name).',
		argnames is ['Directory'],
		exceptions is [
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory')]]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists (irrespective of directory permissions).',
		argnames is ['Directory'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory')]]).

	:- public(directory_files/2).
	:- mode(directory_files(+atom, -list), one).
	:- info(directory_files/2, [
		comment is 'List of all directory files (returns an empty list if the directory is empty).',
		argnames is ['Directory', 'Files'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No read permission for the directory' - permission_error(read, 'Directory'),
			'Directory does not exists' - existence_error(directory, 'Directory')]]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), one).
	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'File does not exists' - existence_error(file, 'File'),
			'No write permission to the file' - permission_error(write, 'File')]]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), zero_or_one).
	:- info(rename_file/2, [
		comment is 'Renames a file (or a directory).',
		argnames is ['Old', 'New'],
		exceptions is [
			'Old is not instantiated' - instantiation_error,
			'New is not instantiated' - instantiation_error,
			'Old is neither a variable nor a valid file name' - type_error(file_name, 'Old'),
			'New is neither a variable nor a valid file name' - type_error(file_name, 'New'),
			'File Old does not exists' - existence_error(file, 'Old'),
			'No write permission to the file' - permission_error(write, 'Old')]]).

	:- public(copy_file/2).
	:- mode(copy_file(+atom, +atom), one).
	:- info(copy_file/2, [
		comment is 'Makes a copy of a file.',
		argnames is ['Original', 'Copy'],
		exceptions is [
			'Original is not instantiated' - instantiation_error,
			'Copy is not instantiated' - instantiation_error,
			'Original is neither a variable nor a valid file name' - type_error(file_name, 'Original'),
			'Copy is neither a variable nor a valid file name' - type_error(file_name, 'Copy'),
			'File Original does not exists' - existence_error(file, 'Original'),
			'No read permission to the original file' - permission_error(read, 'Original'),
			'No write permission to the file copy' - permission_error(write, 'Copy')]]).

	:- public(make_symlink/2).
	:- mode(make_symlink(+atom, +atom), one).
	:- info(make_symlink/2, [
		comment is 'Makes a symbolic link.',
		argnames is ['Symlink', 'Target'],
		exceptions is [
			'Symlink is not instantiated' - instantiation_error,
			'Target is not instantiated' - instantiation_error,
			'Symlink is neither a variable nor a valid file name' - type_error(file_name, 'Symlink'),
			'Target is neither a variable nor a valid file name' - type_error(file_name, 'Target'),
			'No permission for creating the symbolic link' - permission_error(write, 'Symlink')]]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists (irrespective of type and file permissions).',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(file_property/2).
	:- mode(file_property(+atom, +compound), zero_or_one).
	:- mode(file_property(+atom, -compound), one_or_more).
	:- info(file_property/2, [
		comment is 'File properties.',
		argnames is ['File', 'Property'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'File does not exists' - existence_error(file, 'File'),
			'No read permission to the file' - permission_error(read, 'File'),
			'Property is neither a variable nor a file property' - type_error(file_property, 'Property')]]).

	:- public(current_environment_variable/1).
	:- mode(current_environment_variable(?atom), zero_or_more).
	:- info(current_environment_variable/1, [
		comment is 'Argument is a currently defined environment variable . Fails if the variable does not exists.',
		argnames is ['Variable'],
		exceptions is [
			'Variable is neither a variable nor an atom' - type_error(atom, 'Variable')]]).

	:- public(delete_environment_variable/1).
	:- mode(delete_environment_variable(+atom), one).
	:- info(delete_environment_variable/1, [
		comment is 'Deletes an environment variable.',
		argnames is ['Variable'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error,
			'Variable is neither a variable nor an atom' - type_error(atom, 'Variable'),
			'Variable is not a currenttly defined environment variable' - existence_error(environment_variable, 'Variable')]]).

	:- public(get_environment_variable/2).
	:- mode(get_environment_variable(+atom, ?atom), zero_or_one).
	:- info(get_environment_variable/2, [
		comment is 'Gets environment variable value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error,
			'Variable is neither a variable nor an atom' - type_error(atom, 'Variable'),
			'Value is neither a variable nor an atom' - type_error(atom, 'Value'),
			'Variable is not a currenttly defined environment variable' - existence_error(environment_variable, 'Variable')]]).

	:- public(set_environment_variable/2).
	:- mode(set_environment_variable(+atom, +atom), one).
	:- info(set_environment_variable/2, [
		comment is 'Sets environment variable value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error,
			'Value is not instantiated' - instantiation_error,
			'Variable is neither a variable nor an atom' - type_error(atom, 'Variable'),
			'Value is neither a variable nor an atom' - type_error(atom, 'Value')]]).

	:- public(time_stamp/1).
	:- mode(time_stamp(-number), one).
	:- info(time_stamp/1, [
		comment is 'Returns a system-dependent time stamp (which can be used for sorting).',
		argnames is ['Time']]).

	:- public(local_time/1).
	:- mode(local_time(?time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer, ?integer)), zero_or_one).
	:- info(local_time/1, [
		comment is 'Local time (respecting time zone and daylight savings settings).',
		argnames is ['time(Year, Month, Day, Hours, Mins, Secs, Microsecs)']]).

	:- public(utc_time/1).
	:- mode(utc_time(?time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer, ?integer)), zero_or_one).
	:- info(utc_time/1, [
		comment is 'Universal Coordinated Time (UTC).',
		argnames is ['time(Year, Month, Day, Hours, Mins, Secs, Microsecs)']]).

	:- public(convert_time/2).
	:- mode(convert_time(+number, ?time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer, ?integer)), zero_or_one).
	:- mode(convert_time(?number, +time(+integer, +integer, +integer, +integer, +integer, +integer, +integer)), zero_or_one).
	:- info(convert_time/2, [
		comment is 'Converts between system-dependent time stamps and calendar local date and time.',
		argnames is ['Time', 'time(Year, Month, Day, Hours, Mins, Secs, Microsecs)'],
		exceptions is [
			'Neither argument is instantiated' - instantiation_error,
			'Time stamp is neither a variable nor a valid time stamp' - type_error(time_stamp, 'Time'),
			'Time structure is neither a variable nor a valid time structure' - type_error(time_structure, 'time(Year, Month, Day, Hours, Mins, Secs, Microsecs)')]]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), one).
	:- info(cpu_time/1, [
		comment is 'System cpu time in seconds.',
		argnames is ['Time']]).

	:- public(host_name/1).
	:- mode(host_name(-atom), one).
	:- info(host_name/1, [
		comment is 'Host name (default is localhost).',
		argnames is ['Name']]).

	:- public(portable_os_file_name/2).
	:- mode(portable_os_file_name(+atom, -atom), one).
	:- mode(portable_os_file_name(-atom, +atom), one).
	:- info(portable_os_file_name/2, [
		comment is 'Converts between portable and operating-system dependent file names.',
		argnames is ['Canonical', 'OS']]).

	:- public(portable_file_name/3).
	:- mode(portable_file_name(+atom, -atom, -atom), one).
	:- mode(portable_file_name(-atom, +atom, -atom), one).
	:- mode(portable_file_name(-atom, -atom, +atom), one).
	:- info(portable_file_name/3, [
		comment is 'Converts between relative, absolute, and URL portable file names.',
		argnames is ['Relative', 'Absolute', 'URL'],
		exceptions is [
			'None of the arguments is instantiated' - instantiation_error,
			'Relative is neither a variable nor a relative file name' - type_error(relative_file_name, 'Relative'),
			'Absolute is neither a variable nor a absolute file name' - type_error(absolute_file_name, 'Absolute'),
			'URL is neither a variable nor a file name URL' - type_error(url_file_name, 'URL')]]).

	:- public(relative_file_name/1).
	:- mode(relative_file_name(+atom), zero_or_one).
	:- info(relative_file_name/1, [
		comment is 'True when the argument is a valid, relative file name. Argument is expanded to a canonical file name before testing.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(absolute_file_name/1).
	:- mode(absolute_file_name(+atom), zero_or_one).
	:- info(absolute_file_name/1, [
		comment is 'True if the argument is a valid, absolute file name. Argument is expanded to a canonical file name before testing.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(url_file_name/1).
	:- mode(url_file_name(+atom), zero_or_one).
	:- info(url_file_name/1, [
		comment is 'True when the argument is a valid, URL file name. Argument is expanded to a canonical file name before testing.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(absolute_file_name/2).
	:- mode(absolute_file_name(+atom, ?atom), zero_or_one).
	:- info(absolute_file_name/2, [
		comment is 'Expands a file name into a canonical absolute file name.',
		argnames is ['File', 'Absolute'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Absolute is neither a variable nor a valid file name' - type_error(file_name, 'Absolute')]]).

	:- public(url_file_name/2).
	:- mode(url_file_name(+atom, ?atom), zero_or_one).
	:- info(url_file_name/2, [
		comment is 'Expands a file name into a canonical URL file name.',
		argnames is ['File', 'URL'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'URL is neither a variable nor a valid file name URL' - type_error(file_name, 'URL')]]).

	:- public(file_name_part/2).
	:- mode(file_name_part(+atom, ?compound), zero_or_more).
	:- info(file_name_part/2, [
		comment is 'File name parts. The file name is expanded to a canonical file name before decomposing in parts.',
		argnames is ['File', 'Part'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'File does not exists' - existence_error(file, 'File'),
			'Part is neither a variable nor a file name part' - type_error(file_name_part, 'Port')]]).

	:- public(file_name_parts/2).
	:- mode(file_name_parts(+atom, -list(compound)), one).
	:- mode(file_name_parts(-atom, +list(compound)), zero_or_one).
	:- info(file_name_parts/2, [
		comment is 'Converts between a file name and its constituent parts (represented as a list of compound terms). The file name (when instantiated) is expanded to a canonical file name before decomposing in parts.',
		argnames is ['File', 'Parts'],
		exceptions is [
			'None of the arguments are instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Parts is neither a variable nor a list' - type_error(list(compound), 'Parts')]]).

:- end_protocol.
