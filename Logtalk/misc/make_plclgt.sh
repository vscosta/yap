#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.23.1
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named plclgt for running Logtalk with K-Prolog..."

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else
	cd $LOGTALKHOME
	if [ -z "$1" ]; then
		prefix=/usr/local
	else
		prefix="$1"
	fi
	mkdir -p bin
	cd bin
	echo ":- ensure_loaded('\$LOGTALKUSER/configs/k.config')." > logtalk_plc.rc
	echo ":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_plc.rc
	echo ":- ensure_loaded('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_plc.rc
	echo "#/bin/sh" > plclgt
	echo "\$PLC/plc -h 2048k -l 1024k -g 2048k -e \"(consult('\\\$LOGTALKHOME/bin/logtalk_plc.rc'), '\\\$root').\"" >> plclgt
	chmod a+x plclgt
	ln -sf $LOGTALKHOME/bin/plclgt $prefix/bin/plclgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
