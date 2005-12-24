#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.26.2
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
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
cp ../compiler/logtalk.pl logtalk.P
sed 's/^..lgt_current_object_.[(]user.*[)]/:- assertz(&)/' logtalk.P > temp1
sed 's/^..lgt_current_object_.[(]debugger.*[)]/:- assertz(&)/' temp1 > temp2
sed 's/^..lgt_dbg_leashing_.[(].*[)]/:- assertz(&)/g' temp2 > logtalk.P
rm temp1
rm temp2
echo ":- reconsult('$LOGTALKUSER/configs/xsb.P')." > logtalkxsb.P
echo ":- reconsult('$LOGTALKHOME/bin/logtalk.P')." >> logtalkxsb.P
echo ":- reconsult('$LOGTALKUSER/libpaths/libpaths.P')." >> logtalkxsb.P
echo "#/bin/sh" > xsblgt
echo "xsb -e \"reconsult('\$LOGTALKHOME/bin/logtalkxsb.P').\"" >> xsblgt
chmod a+x xsblgt
ln -sf $LOGTALKHOME/bin/xsblgt $prefix/bin/xsblgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script. Users must run the"
echo "the cplgtdirs script before using the xsblgt script."
echo
echo "The first call to the script must be made as root or using sudo."
echo
