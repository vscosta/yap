#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.3
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named sicstuslgt for running Logtalk with SICStus Prolog..."

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
	echo ":- compile('\$LOGTALKUSER/configs/sicstus.config')." > logtalk_sicstus.rc
	echo ":- compile('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_sicstus.rc
	echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_sicstus.rc

	echo "#/bin/sh" > sicstuslgt
	echo "sicstus -l \$LOGTALKHOME/bin/logtalk_sicstus.rc" >> sicstuslgt
	chmod a+x sicstuslgt
	ln -sf $LOGTALKHOME/bin/sicstuslgt $prefix/bin/sicstuslgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
