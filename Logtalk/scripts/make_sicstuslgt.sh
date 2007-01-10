#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
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
if sicstus -f --goal "halt." 2>&1 | grep "SICStus 4" 2>&1 >/dev/null; then
	echo ":- compile('\$LOGTALKUSER/configs/sicstus4.config')." > logtalk_sicstus.pl
	sed -e '/call_with_args/ s//call/g' ../compiler/logtalk.pl > logtalk_comp_sicstus.pl
	echo ":- compile('\$LOGTALKHOME/bin/logtalk_comp_sicstus.pl')." >> logtalk_sicstus.pl
else
	echo ":- compile('\$LOGTALKUSER/configs/sicstus.config')." > logtalk_sicstus.pl
	echo ":- compile('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_sicstus.pl
fi
echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_sicstus.pl

echo "#/bin/sh" > sicstuslgt
echo "sicstus -l \$LOGTALKHOME/bin/logtalk_sicstus.pl" >> sicstuslgt
chmod a+x sicstuslgt
ln -sf $LOGTALKHOME/bin/sicstuslgt $prefix/bin/sicstuslgt
echo "Done. A link to the script was been created in $prefix/bin."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the sicstuslgt script."
echo
