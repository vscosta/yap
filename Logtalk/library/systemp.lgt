
:- protocol(systemp).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2004/6/4,
		comment is 'Operating system access protocol.']).

	:- public(make_directory/1).
	:- mode(make_directory(+atom), zero_or_one).
	:- info(make_directory/1, [
		comment is 'Makes a new directory.',
		argnames is ['Directory'],
		exceptions is [
			'Argument is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for creating a new directory' - permission_error(modify, 'Directory')]]).

	:- public(delete_directory/1).
	:- mode(delete_directory(+atom), zero_or_one).
	:- info(delete_directory/1, [
		comment is 'Deletes a directory.',
		argnames is ['Directory'],
		exceptions is [
			'Argument is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for deleting the directory' - permission_error(modify, 'Directory')]]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), zero_or_one).
	:- info(change_directory/1, [
		comment is 'Changes current working directory.',
		argnames is ['Directory'],
		exceptions is [
			'Argument is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No permission for accessing the directory' - permission_error(access, 'Directory')]]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory'],
		exceptions is [
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory')]]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists.',
		argnames is ['Directory'],
		exceptions is [
			'Argument is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No access permission for the directory' - permission_error(access, 'Directory')]]).

	:- public(directory_files/2).
	:- mode(directory_files(+atom, -list), zero_or_one).
	:- info(directory_files/2, [
		comment is 'List of all directory files. Returns an empty list if the directory is empty.',
		argnames is ['Directory', 'Files'],
		exceptions is [
			'Directory is not instantiated' - instantiation_error,
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory'),
			'No access permission for the directory' - permission_error(access, 'Directory'),
			'Directory does not exists' - existence_error('Directory')]]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(file_modification_time/2).
	:- mode(file_modification_time(+atom, -nonvar), zero_or_one).
	:- info(file_modification_time/2, [
		comment is 'File modification time using a system-dependent time stamp.',
		argnames is ['File', 'Time'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(file_size/2).
	:- mode(file_size(+atom, -integer), zero_or_one).
	:- info(file_size/2, [
		comment is 'File size in bytes.',
		argnames is ['File', 'Size'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(file_type/2).
	:- mode(file_type(+atom, ?atom), zero_or_one).
	:- info(file_type/2, [
		comment is 'File type (regular, directory, symlink, fifo, socket, and unknown).',
		argnames is ['File', 'Type'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Type is neither a variable nor a valid file type' - type_error(file_type, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(file_permission/2).
	:- mode(file_permission(+atom, ?atom), zero_or_more).
	:- info(file_permission/2, [
		comment is 'File permission (read, write, execute, and search).',
		argnames is ['File', 'Permission'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Permission is neither a variable nor a valid file permission' - type_error(file_permission, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), zero_or_one).
	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'No write permission to the file' - permission_error(modify, 'File')]]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), zero_or_one).
	:- info(rename_file/2, [
		comment is 'Renames a file.',
		argnames is ['Old', 'New'],
		exceptions is [
			'Old is not instantiated' - instantiation_error,
			'New is not instantiated' - instantiation_error,
			'Old is neither a variable nor a valid file name' - type_error(file_name, 'Old'),
			'New is neither a variable nor a valid file name' - type_error(file_name, 'New'),
			'File Old does not exists' - existence_error('Old'),
			'No write permission to the file' - permission_error(modify, 'Old')]]).

	:- public(copy_file/2).
	:- mode(copy_file(+atom, +atom), zero_or_one).
	:- info(copy_file/2, [
		comment is 'Makes a copy of a file.',
		argnames is ['Original', 'Copy'],
		exceptions is [
			'Original is not instantiated' - instantiation_error,
			'Copy is not instantiated' - instantiation_error,
			'Original is neither a variable nor a valid file name' - type_error(file_name, 'Original'),
			'Copy is neither a variable nor a valid file name' - type_error(file_name, 'Copy'),
			'File Original does not exists' - existence_error('Original'),
			'No read permission to the file' - permission_error(read, 'Original')]]).

	:- public(symbolic_link/2).
	:- mode(symbolic_link(+atom, -atom), zero_or_one).
	:- info(symbolic_link/2, [
		comment is 'Follows a symbolic link returning the target full path.',
		argnames is ['File', 'Target'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'No access permission to the file' - permission_error(access, 'File')]]).

	:- public(environment_variable/2).
	:- mode(environment_variable(+atom, ?atom), zero_or_one).
	:- info(environment_variable/2, [
		comment is 'Gets environment variable value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error,
			'Variable is neither a variable nor an atom' - type_error(atom, 'Variable'),
			'Value is neither a variable nor an atom' - type_error(atom, 'Value')]]).

	:- public(set_environment_variable/2).
	:- mode(set_environment_variable(+atom, +atom), zero_or_one).
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
		comment is 'Returns the current system-dependent time stamp.',
		argnames is ['Time']]).

	:- public(date_time/7).
	:- mode(date_time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(date_time/7, [
		comment is 'Current calendar date and time.',
		argnames is ['Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs', 'Milisecs']]).

	:- public(convert_time/8).
	:- mode(convert_time(+number, ?integer, ?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(convert_time/8, [
		comment is 'Converts a system-dependent time stamp to calendar local date and time.',
		argnames is ['Time', 'Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs', 'Milisecs'],
		exceptions is [
			'Time is not instantiated' - instantiation_error,
			'Time is neither a variable nor a valid time stamp' - type_error(time_stamp, 'Variable')]]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), zero_or_one).
	:- info(cpu_time/1, [
		comment is 'System cpu time in seconds.',
		argnames is ['Time']]).

	:- public(host_name/1).
	:- mode(host_name(-atom), one).
	:- info(host_name/1, [
		comment is 'Host name.',
		argnames is ['Name']]).

	:- public(canonical_os_path/2).
	:- mode(canonical_os_path(+atom, -atom), one).
	:- mode(canonical_os_path(-atom, +atom), one).
	:- info(canonical_os_path/2, [
		comment is 'Converts between canonical and operating system dependent paths.',
		argnames is ['Canonical', 'OS']]).

	:- public(canonical_path/3).
	:- mode(canonical_path(+atom, -atom, -atom), zero_or_one).
	:- mode(canonical_path(-atom, +atom, -atom), zero_or_one).
	:- mode(canonical_path(-atom, -atom, +atom), zero_or_one).
	:- info(canonical_path/3, [
		comment is 'Converts between relative, absolute, and full canonical paths.',
		argnames is ['Relative', 'Absolute', 'Full'],
		exceptions is [
			'None of the arguments is instantiated' - instantiation_error,
			'Relative is neither a variable nor a relative path' - type_error(relative_file_name, 'Relative'),
			'Absolute is neither a variable nor a absolute path' - type_error(absolute_path, 'Absolute'),
			'Full is neither a variable nor a full path' - type_error(full_file_name, 'Full')]]).

	:- public(relative_file_name/1).
	:- mode(relative_file_name(+atom), zero_or_one).
	:- info(relative_file_name/1, [
		comment is 'True when the argument is a valid, relative file name.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(absolute_file_name/1).
	:- mode(absolute_file_name(+atom), zero_or_one).
	:- info(absolute_file_name/1, [
		comment is 'True if the argument is a valid, absolute file name.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(full_file_name/1).
	:- mode(full_file_name(+atom), zero_or_one).
	:- info(full_file_name/1, [
		comment is 'True when the argument is a valid, full file name.',
		argnames is ['File'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File')]]).

	:- public(absolute_file_name/2).
	:- mode(absolute_file_name(+atom, ?atom), zero_or_one).
	:- info(absolute_file_name/2, [
		comment is 'Expands a file name into an absolute file name.',
		argnames is ['File', 'Absolute'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Absolute is neither a variable nor a valid file name' - type_error(file_name, 'Absolute')]]).

	:- public(full_file_name/2).
	:- mode(full_file_name(+atom, ?atom), zero_or_one).
	:- info(full_file_name/2, [
		comment is 'Expands a file name into a full file name.',
		argnames is ['File', 'Full'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Full is neither a variable nor a valid file name' - type_error(file_name, 'Full')]]).

	:- public(file_name_extension/2).
	:- mode(file_name_extension(+atom, ?atom), zero_or_one).
	:- info(file_name_extension/2, [
		comment is 'Extracts a file name extension (without the dot). Returns the empty atom if no extension exists.',
		argnames is ['File', 'Extension'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Extension is neither a variable nor an atom' - type_error(atom, 'Extension')]]).

	:- public(file_base_name/2).
	:- mode(file_base_name(+atom, ?atom), zero_or_one).
	:- info(file_base_name/2, [
		comment is 'Extracts a file base name.',
		argnames is ['File', 'Base'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Base is neither a variable nor an atom' - type_error(atom, 'Base')]]).

	:- public(file_name_directory/2).
	:- mode(file_name_directory(+atom, ?atom), zero_or_one).
	:- info(file_name_directory/2, [
		comment is 'Extracts a file name directory (ends with the directory path separator).',
		argnames is ['File', 'Directory'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Directory is neither a variable nor a valid file name' - type_error(file_name, 'Directory')]]).

	:- public(file_name_protocol/2).
	:- mode(file_name_protocol(+atom, ?atom), zero_or_one).
	:- info(file_name_protocol/2, [
		comment is 'Extracts a file name access protocol.',
		argnames is ['File', 'Protocol'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Protocol is neither a variable nor a valid file access protocol' - type_error(file_protocol, 'Protocol')]]).

	:- public(file_name_login/3).
	:- mode(file_name_login(+atom, ?atom, ?atom), zero_or_one).
	:- info(file_name_login/3, [
		comment is 'Extracts the username and the password from a file name.',
		argnames is ['File', 'User', 'Password'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'User is neither a variable nor an atom' - type_error(atom, 'User'),
			'Password is neither a variable nor an atom' - type_error(atom, 'Password')]]).

	:- public(file_name_host/3).
	:- mode(file_name_host(+atom, ?atom, ?float), zero_or_one).
	:- info(file_name_host/3, [
		comment is 'Extracts the host name and the host port from a file name.',
		argnames is ['File', 'Host', 'Port'],
		exceptions is [
			'File is not instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Host is neither a variable nor an atom' - type_error(atom, 'Host'),
			'Port is neither a variable nor an integer' - type_error(integer, 'Port')]]).

	:- public(file_name_parts/2).
	:- mode(file_name_parts(+atom, -list), one).
	:- mode(file_name_parts(-atom, +list), zero_or_one).
	:- info(file_name_parts/2, [
		comment is 'Converts between a file name and its constituent parts (represented as a list of compound terms).',
		argnames is ['File', 'Parts'],
		exceptions is [
			'None of the arguments are instantiated' - instantiation_error,
			'File is neither a variable nor a valid file name' - type_error(file_name, 'File'),
			'Parts is neither a variable nor a list' - type_error(list, 'Host')]]).

:- end_protocol.
