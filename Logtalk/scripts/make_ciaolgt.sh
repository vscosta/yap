#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.27.0
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
echo ":- ensure_loaded('\$LOGTALKUSER/configs/ciao_aux.config')." > logtalk_ciao.rc
echo ":- set_prolog_flag(multi_arity_warnings, off)." >> logtalk_ciao.rc
echo ":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_ciao.rc
echo ":- ensure_loaded('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_ciao.rc
echo ":- op(600, xfy, ::)." >> logtalk_ciao.rc
echo ":- op(600, fy, ::)." >> logtalk_ciao.rc
echo ":- op(600, fy, ^^)." >> logtalk_ciao.rc
echo ":- op(200, fy, +)." >> logtalk_ciao.rc
echo ":- op(200, fy, ?)." >> logtalk_ciao.rc
echo ":- op(200, fy, @)." >> logtalk_ciao.rc
echo ":- op(200, fy, -)." >> logtalk_ciao.rc
echo "#/bin/sh" > ciaolgt
echo "ciaosh -l \$LOGTALKHOME/bin/logtalk_ciao.rc" >> ciaolgt
chmod a+x ciaolgt
ln -sf $LOGTALKHOME/bin/ciaolgt $prefix/bin/ciaolgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script. Users must run the"
echo "the cplgtdirs script before using the ciaolgt script."
echo
echo "The first call to script the must be made as root or using sudo."
echo
