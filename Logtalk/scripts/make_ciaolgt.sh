#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.1
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named ciaolgt for running Logtalk with CIAO..."
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
echo ":- ensure_loaded('\$LOGTALKUSER/configs/ciao_aux.config')." > logtalk_ciao.pl
echo ":- set_prolog_flag(multi_arity_warnings, off)." >> logtalk_ciao.pl
echo ":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_ciao.pl
echo ":- ensure_loaded('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_ciao.pl
echo ":- op(600, xfy, ::)." >> logtalk_ciao.pl
echo ":- op(600, fy, ::)." >> logtalk_ciao.pl
echo ":- op(600, fy, ^^)." >> logtalk_ciao.pl
echo ":- op(200, fy, +)." >> logtalk_ciao.pl
echo ":- op(200, fy, ?)." >> logtalk_ciao.pl
echo ":- op(200, fy, @)." >> logtalk_ciao.pl
echo ":- op(200, fy, -)." >> logtalk_ciao.pl
echo "#/bin/sh" > ciaolgt
echo "ciaosh -l \$LOGTALKHOME/bin/logtalk_ciao.pl" >> ciaolgt
chmod a+x ciaolgt
ln -sf $LOGTALKHOME/bin/ciaolgt $prefix/bin/ciaolgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "The first call to the script the must be made as root or using"
echo "sudo."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the ciaolgt script."
echo
