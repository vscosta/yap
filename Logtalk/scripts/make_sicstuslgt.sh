#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.26.2
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named sicstuslgt for running Logtalk with SICStus Prolog..."
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
cd bin
echo ":- compile('\$LOGTALKUSER/configs/sicstus.config')." > logtalk_sicstus.rc
echo ":- compile('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_sicstus.rc
echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_sicstus.rc

echo "#/bin/sh" > sicstuslgt
echo "sicstus -l \$LOGTALKHOME/bin/logtalk_sicstus.rc" >> sicstuslgt
chmod a+x sicstuslgt
ln -sf $LOGTALKHOME/bin/sicstuslgt $prefix/bin/sicstuslgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
