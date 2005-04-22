#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.24.0
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named eclipselgt for running Logtalk with ECLiPSe..."

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
	echo ":- pragma(system)." > logtalk_eclipse.pl
	echo ":- pragma(nodebug)." >> logtalk_eclipse.pl
	echo ":- ensure_loaded(library(toplevel))." >> logtalk_eclipse.pl
	echo ":- include('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_eclipse.pl
	echo ":- compile('\$LOGTALKUSER/configs/eclipseiso.config')." > logtalk_eclipse.rc
	echo ":- compile('\$LOGTALKHOME/bin/logtalk_eclipse.pl')." >> logtalk_eclipse.rc
	echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_eclipse.rc
	echo "#/bin/sh" > eclipselgt
	echo "eclipse -b \$LOGTALKHOME/bin/logtalk_eclipse.rc" >> eclipselgt
	chmod a+x eclipselgt
	ln -sf $LOGTALKHOME/bin/eclipselgt $prefix/bin/eclipselgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
