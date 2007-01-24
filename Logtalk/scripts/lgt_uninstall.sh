#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.3
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Uninstalling Logtalk system-level files..."
echo

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
fi

if ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi

cd $LOGTALKHOME/..
rm -rf lgt2293
rm -f logtalk

if [ -d bin ]; then
	cd bin
	rm -f bplgt
	rm -f ciaolgt
	rm -f cplgtdirs
	rm -f eclipselgt
	rm -f gplgt
	rm -f lgt2html
	rm -f lgt2pdf
	rm -f lgt2xml
	rm -f plclgt
	rm -f qplgt
	rm -f sicstuslgt
	rm -f swilgt
	rm -f xsblgt
	rm -f yaplgt
fi

echo "Logtalk system-level uninstall completed. For uninstalling user-level"
echo "Logtalk files simply delete the LOGTALKUSER directories."
echo
