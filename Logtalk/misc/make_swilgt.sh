#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.23.0
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named swilgt for running Logtalk with SWI-Prolog..."

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
	echo ":- system_module." > logtalk_swi.pl
	cat ../compiler/logtalk.pl >> logtalk_swi.pl
	echo ":- consult('\$LOGTALKUSER/configs/swi.config')." > logtalk_swi.rc
	echo ":- consult('\$LOGTALKUSER/configs/swihook.pl')." >> logtalk_swi.rc
	echo ":- consult('\$LOGTALKHOME/bin/logtalk_swi.pl')." >> logtalk_swi.rc
	echo ":- consult('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_swi.rc
	echo "#/bin/sh" > swilgt
	echo "swipl -f \$LOGTALKHOME/bin/logtalk_swi.rc" >> swilgt
	chmod a+x swilgt
	ln -sf $LOGTALKHOME/bin/swilgt $prefix/bin/swilgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
