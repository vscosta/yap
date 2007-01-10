#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named gplgt for running Logtalk with GNU Prolog..."
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
echo ":- built_in." > logtalk_gp.pl
cat ../compiler/logtalk.pl >> logtalk_gp.pl
echo "#/bin/sh" > gplgt
echo "gprolog --init-goal \"['\$LOGTALKUSER/configs/gnu.config', '\$LOGTALKHOME/bin/logtalk_gp.pl', '\$LOGTALKUSER/libpaths/libpaths.pl']\"" >> gplgt
chmod a+x gplgt
ln -sf $LOGTALKHOME/bin/gplgt $prefix/bin/gplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the gplgt script."
echo
