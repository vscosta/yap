#! /bin/sh

echo
echo "Making a script named yaplgt for running Logtalk with YAP..."

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
	echo ":- reconsult('\$LOGTALKHOME/configs/yap.config')." > logtalkyap.rc
	echo ":- reconsult('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalkyap.rc

	echo "#/bin/sh" > yaplgt
	echo "yap -l \$LOGTALKHOME/bin/logtalkyap.rc" >> yaplgt
	chmod a+x yaplgt
	ln -sf $LOGTALKHOME/bin/yaplgt $prefix/bin/yaplgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variable LOGTALKHOME in"
	echo "order to use the script."
	echo
fi
