#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.5
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named ciaolgt for running Logtalk with CIAO..."

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
	echo ":- ensure_loaded('\$LOGTALKUSER/configs/ciao_aux.config')." > logtalk_ciao.rc
	echo ":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_ciao.rc
	echo ":- ensure_loaded('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_ciao.rc
	echo ":- op(600, xfy, ::)." >> logtalk_ciao.rc
	echo ":- op(600,  fy, ::)." >> logtalk_ciao.rc
	echo ":- op(600,  fy, ^^)." >> logtalk_ciao.rc
	echo ":- op(200,  fy, +)." >> logtalk_ciao.rc
	echo ":- op(200,  fy, ?)." >> logtalk_ciao.rc
	echo ":- op(200,  fy, @)." >> logtalk_ciao.rc
	echo ":- op(200,  fy, -)." >> logtalk_ciao.rc

	echo "#/bin/sh" > ciaolgt
	echo "ciaosh -l \$LOGTALKHOME/bin/logtalk_ciao.rc" >> ciaolgt
	chmod a+x ciaolgt
	ln -sf $LOGTALKHOME/bin/ciaolgt $prefix/bin/ciaolgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
fi
