#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.27.1
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named yaplgt for running Logtalk with YAP..."
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
echo ":- reconsult('\$LOGTALKUSER/configs/yap.config')." > logtalk_yap.rc
echo ":- reconsult('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_yap.rc
echo ":- reconsult('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_yap.rc

echo "#/bin/sh" > yaplgt
echo "yap -s 49152 -h 16384 -t 1024 -l \$LOGTALKHOME/bin/logtalk_yap.rc" >> yaplgt
chmod a+x yaplgt
ln -sf $LOGTALKHOME/bin/yaplgt $prefix/bin/yaplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
