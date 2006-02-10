#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.27.0
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named qplgt for running Logtalk with Qu-Prolog..."
echo

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
fi

if ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

cd "$LOGTALKHOME"
mkdir -p bin
cd configs
cp qu.config qu.ql
echo "fcompile('qu.ql', [assemble_only(true)]), load(qu). \
chdir('../compiler/'), fcompile('logtalk.pl', [assemble_only(true), string_table(256)]), load(logtalk). \
chdir('../libpaths/'), fcompile('libpaths.pl', [assemble_only(true)]), load(libpaths)." | qp -T 16 -s 2048 -d 1024 -h 2048
qc -c qphook.ql
cd ../bin
qc -T 16 -s 2048 -d 1024 -h 2048 -o qplgt ../configs/qphook.qo ../configs/qu.qo ../compiler/logtalk.qo  ../libpaths/libpaths.qo
chmod a+x qplgt
ln -sf $LOGTALKHOME/bin/qplgt $prefix/bin/qplgt
rm ../configs/qu.ql
rm ../configs/qphook.qo
rm ../configs/qu.qo
rm ../compiler/logtalk.qo
rm ../libpaths/libpaths.qo
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
