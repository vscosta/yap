#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.4
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named gplgt for running Logtalk with GNU Prolog..."

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
	echo ":- built_in." > logtalk_gp.pl
	cat ../compiler/logtalk.pl >> logtalk_gp.pl
	echo "#/bin/sh" > gplgt
	echo "gprolog --init-goal \"['$LOGTALKUSER/configs/gnu.config', '$LOGTALKHOME/bin/logtalk_gp.pl', '$LOGTALKUSER/libpaths/libpaths.pl']\"" >> gplgt
	chmod a+x gplgt
	ln -sf $LOGTALKHOME/bin/gplgt $prefix/bin/gplgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
