#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.24.0
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named yaplgt for running Logtalk with YAP..."

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else
	cd "$LOGTALKHOME"
	if [ -z "$1" ]; then
		prefix=/usr/local
	else
		prefix="$1"
	fi
	mkdir -p bin
	cd bin
	echo ":- reconsult('\$LOGTALKUSER/configs/yap.config')." > logtalk_yap.rc
	echo ":- reconsult('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_yap.rc
	echo ":- reconsult('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_yap.rc

	echo "#/bin/sh" > yaplgt
	echo "yap -l \$LOGTALKHOME/bin/logtalk_yap.rc" >> yaplgt
	chmod a+x yaplgt
	ln -sf $LOGTALKHOME/bin/yaplgt $prefix/bin/yaplgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
