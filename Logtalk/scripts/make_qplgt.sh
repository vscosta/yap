#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

#	makes a shell script named "qplgt" for running Logtalk with Qu-Prolog 
#	(based on script code contributed by Peter Robinson); this script does 
#	not read at runtime the config and the libpaths files: you will need to 
#	run this script again if you modify these files


echo
echo "Creating a script named qplgt for running Logtalk with Qu-Prolog..."
echo

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "Using Logtalk installation at \"/usr/local/share/logtalk\""
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "Using Logtalk installation at \"/usr/share/logtalk\""
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at \"/opt/local/share/logtalk\""
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "Using Logtalk installation at \"/opt/share/logtalk\""
	else
		echo "Unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	elif ! [ -d "$LOGTALKHOME" ]; then
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
chdir('../libpaths/'), fcompile('libpaths.pl', [assemble_only(true)]), load(libpaths)." | qp -s 3072 -d 1024 -h 2048
qc -c qphook.ql
cd ../bin
qc -s 3072 -d 1024 -h 2048 -o qplgt ../configs/qphook.qo ../configs/qu.qo ../compiler/logtalk.qo  ../libpaths/libpaths.qo
chmod a+x qplgt

mkdir -p $prefix/bin
ln -sf $LOGTALKHOME/bin/qplgt $prefix/bin/qplgt

rm ../configs/qu.ql
rm ../configs/qphook.qo
rm ../configs/qu.qo
rm ../compiler/logtalk.qo
rm ../libpaths/libpaths.qo

echo "Done. A link to the script was been created in $prefix/bin."
echo "The script must be regenerated whenever changes are made to"
echo "either the config file or the libpaths file."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the qplgt script."
echo
