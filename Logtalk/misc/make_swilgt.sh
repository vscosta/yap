#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.21.6
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
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
	if ! [ -d bin ]
	then
		mkdir bin
	fi
	cd bin
	echo ":- system_module." > logtalkswi.pl
	cat ../compiler/logtalk.pl >> logtalkswi.pl
	echo ":- consult('\$LOGTALKHOME/configs/swi.config')." > logtalkswi.rc
	echo ":- consult('\$LOGTALKHOME/configs/swihook.pl')." >> logtalkswi.rc
	echo ":- consult('\$LOGTALKHOME/bin/logtalkswi.pl')." >> logtalkswi.rc
	echo "#/bin/sh" > swilgt
	echo "swipl -f \$LOGTALKHOME/bin/logtalkswi.rc" >> swilgt
	chmod a+x swilgt
	ln -sf $LOGTALKHOME/bin/swilgt $prefix/bin/swilgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variable LOGTALKHOME in"
	echo "order to use the script."
	echo
fi
