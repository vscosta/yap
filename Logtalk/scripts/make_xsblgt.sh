#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.5
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named xsblgt for running Logtalk with XSB..."
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
echo ":- reconsult('~/logtalk/configs/xsb.pl')." > logtalk_xsb.pl
echo ":- reconsult('compiler/logtalk.pl')." >> logtalk_xsb.pl
echo ":- reconsult('~/logtalk/libpaths/libpaths_no_env_var.pl')." >> logtalk_xsb.pl
echo ":- path_sysop(chdir, '~')." >> logtalk_xsb.pl
echo "#/bin/sh" > xsblgt
echo "cd \$LOGTALKHOME" >> xsblgt
echo "xsb -l -e \"reconsult('bin/logtalk_xsb.pl').\" \"\$@\"" >> xsblgt
chmod a+x xsblgt
ln -sf $LOGTALKHOME/bin/xsblgt $prefix/bin/xsblgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "The first call to the script the must be made as root or using"
echo "sudo."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the xsblgt script."
echo
