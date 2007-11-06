
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.30.7
%
%  Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	os(system('ln -sf $LOGTALKUSER/configs/qu.config $LOGTALKUSER/configs/qu.pl')),
	fcompile('$LOGTALKUSER/configs/qu.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/configs/qu.qo'),
	os(system('ln -sf $LOGTALKHOME/compiler/logtalk.pl $LOGTALKUSER/.logtalk.pl')),
	fcompile('$LOGTALKUSER/.logtalk.pl', [assemble_only(true), object_file('$LOGTALKUSER/.logtalk.qo'), string_table(256)]),
	load('$LOGTALKUSER/.logtalk.qo'),
	fcompile('$LOGTALKUSER/libpaths/libpaths.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/libpaths/libpaths.qo').
