#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.21.3
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named xsblgt for running Logtalk with XSB..."

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
	cp ../configs/xsb.config xsbconfig.P
	cp ../compiler/logtalk.pl logtalk.P
	sed 's/^..lgt_current_object_.[(]user.*[)]/:- assertz(&)/' logtalk.P > temp1
	sed 's/^..lgt_current_object_.[(]debugger.*[)]/:- assertz(&)/' temp1 > temp2
	sed 's/^..lgt_dbg_leashing_.[(].*[)]/:- assertz(&)/g' temp2 > logtalk.P
	rm temp1
	rm temp2
	echo ":- reconsult('$LOGTALKHOME/bin/xsbconfig.P')." > logtalkxsb.P
	echo ":- reconsult('$LOGTALKHOME/bin/logtalk.P')." >> logtalkxsb.P
	echo "#/bin/sh" > xsblgt
	echo "xsb -e \"reconsult('\$LOGTALKHOME/bin/logtalkxsb.P').\"" >> xsblgt
	chmod a+x xsblgt
	ln -sf $LOGTALKHOME/bin/xsblgt $prefix/bin/xsblgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variable LOGTALKHOME in"
	echo "order to use the script."
	echo
fi
