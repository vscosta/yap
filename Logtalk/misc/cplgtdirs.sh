#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.21.4
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "This script copies the Logtalk library, xml, and examples"
echo "directories to the user home directory (~/logtalk)."
echo

if ! [ $LOGTALKHOME ]
then
	echo "The env variable LOGTALKHOME must be defined first!"
else
	mkdir -p $HOME/logtalk/examples
	mkdir -p $HOME/logtalk/library
	mkdir -p $HOME/logtalk/xml
	cp -RL $LOGTALKHOME/examples $HOME/logtalk/
	cp -RL $LOGTALKHOME/library $HOME/logtalk/
	cp -RL $LOGTALKHOME/xml $HOME/logtalk/
	chmod -R u+w $HOME/logtalk
	echo "Finished copying Logtalk directories."
	echo
fi
