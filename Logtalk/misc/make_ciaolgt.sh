#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.20.1
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
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
	if ! [ -d bin ]
	then
		mkdir bin
	fi
	cd bin

	echo ":- ensure_loaded('\$LOGTALKHOME/configs/ciao_aux.config')." > logtalkciao.rc
	echo ":- ensure_loaded('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalkciao.rc
	echo ":- op(600, xfy, ::)." >> logtalkciao.rc
	echo ":- op(600,  fy, ::)." >> logtalkciao.rc
	echo ":- op(600,  fy, ^^)." >> logtalkciao.rc
	echo ":- op(200,  fy, +)." >> logtalkciao.rc
	echo ":- op(200,  fy, ?)." >> logtalkciao.rc
	echo ":- op(200,  fy, @)." >> logtalkciao.rc
	echo ":- op(200,  fy, -)." >> logtalkciao.rc

	echo "#/bin/sh" > ciaolgt
	echo "ciaosh -l \$LOGTALKHOME/bin/logtalkciao.rc" >> ciaolgt
	chmod a+x ciaolgt
	ln -sf $LOGTALKHOME/bin/ciaolgt $prefix/bin/ciaolgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variable LOGTALKHOME in"
	echo "order to use the script."
	echo
fi
