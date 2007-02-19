#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.4
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named bplgt for running Logtalk with B-Prolog ..."
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
eval bp -g "\"set_prolog_flag(redefined, off), compile('../compiler/logtalk.pl').\""
mv ../compiler/logtalk.pl.out .

echo ":- set_prolog_flag(redefined, off)." > logtalk_bp.pl
echo ":- compile('\$LOGTALKUSER/configs/b.config')." >> logtalk_bp.pl
echo ":- load('\$LOGTALKUSER/configs/b.config')." >> logtalk_bp.pl
echo ":- load('\$LOGTALKHOME/bin/logtalk.pl')." >> logtalk_bp.pl
echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_bp.pl
echo ":- load('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_bp.pl

echo "#/bin/sh" > bplgt
echo "bp -g  \"consult('\\\$LOGTALKHOME/bin/logtalk_bp.pl'), \\\$bp_top_level\"" >> bplgt
chmod a+x bplgt
ln -sf $LOGTALKHOME/bin/bplgt $prefix/bin/bplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the bplgt script."
echo
