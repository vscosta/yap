#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.23.1
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "This script copies the Logtalk user-modifiable files and directories"
echo "to the user home directory. The location can be set by the environment"
echo "variable \$LOGTALKUSER (defaults to ~/logtalk when the variable is not"
echo "defined)"
echo

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
fi

if ! [ $LOGTALKUSER ]
then
	LOGTALKUSER=$HOME/logtalk
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKUSER pointing to $LOGTALKUSER"
	echo
fi

if [ -d $LOGTALKUSER ]
then
	echo "Error! Logtalk directory already exists!"
	echo "Please rename it and run this script again."
	echo
	exit 1
fi

echo "Copying Logtalk files and directories..."
mkdir -p $LOGTALKUSER/configs
mkdir -p $LOGTALKUSER/examples
mkdir -p $LOGTALKUSER/libpaths
mkdir -p $LOGTALKUSER/library
mkdir -p $LOGTALKUSER/xml
cp -RL $LOGTALKHOME/configs $LOGTALKUSER/
cp -RL $LOGTALKHOME/examples $LOGTALKUSER/
cp -RL $LOGTALKHOME/libpaths $LOGTALKUSER/
cp -RL $LOGTALKHOME/library $LOGTALKUSER/
cp -RL $LOGTALKHOME/xml $LOGTALKUSER/
chmod -R u+w $LOGTALKUSER
ln -sf $LOGTALKHOME/BIBLIOGRAPHY $LOGTALKUSER/BIBLIOGRAPHY
ln -sf $LOGTALKHOME/LICENSE $LOGTALKUSER/LICENSE
ln -sf $LOGTALKHOME/QUICK_START $LOGTALKUSER/QUICK_START
ln -sf $LOGTALKHOME/README $LOGTALKUSER/README
ln -sf $LOGTALKHOME/RELEASE_NOTES $LOGTALKUSER/RELEASE_NOTES
ln -sf $LOGTALKHOME/UPGRADING $LOGTALKUSER/UPGRADING
ln -sf $LOGTALKHOME/manuals $LOGTALKUSER/manuals
echo "Finished copying Logtalk files directories."
echo
echo "You may need to edit the \$LOGTALKUSER/libpaths/libpaths.pl file to match"
echo "your Prolog compiler and operating-system requirements or to add your own"
echo "library paths."
echo
echo "You may want to customize the default Logtalk compiler flags by editing"
echo "the configuration file for your Prolog compiler found in the directory"
echo "\$LOGTALKUSER/configs."
echo
