#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.26.2
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "This script copies the Logtalk user-modifiable files and directories"
echo "to the user home directory. The location can be set by the environment"
echo "variable \$LOGTALKUSER (defaults to ~/logtalk when the variable is not"
echo "defined)"
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

if ! [ "$LOGTALKUSER" ]
then
	LOGTALKUSER=$HOME/logtalk
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKUSER pointing to $LOGTALKUSER"
	echo
fi

if [ -d "$LOGTALKUSER" ]
then
	echo "Error! Logtalk user directory already exists!"
	echo "Please rename it or delete it and run this script again."
	echo
	exit 1
fi

echo "Copying Logtalk files and directories..."
mkdir -p "$LOGTALKUSER"/configs
mkdir -p "$LOGTALKUSER"/contributions
mkdir -p "$LOGTALKUSER"/examples
mkdir -p "$LOGTALKUSER"/libpaths
mkdir -p "$LOGTALKUSER"/library
mkdir -p "$LOGTALKUSER"/xml
cp -RL "$LOGTALKHOME"/configs "$LOGTALKUSER"/
ln -sf xsb.config "$LOGTALKUSER"/configs/xsb.P 
ln -sf xsbcvs.config "$LOGTALKUSER"/configs/xsbcvs.P 
cp -RL "$LOGTALKHOME"/contributions "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/examples "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/libpaths "$LOGTALKUSER"/
sed 's_\$LOGTALKUSER_'$LOGTALKUSER'_' "$LOGTALKUSER"/libpaths/libpaths.pl > "$LOGTALKUSER"/libpaths/libpaths.P
cp -RL "$LOGTALKHOME"/library "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/xml "$LOGTALKUSER"/
chmod -R u+w "$LOGTALKUSER"
rm -f "$LOGTALKUSER"/xml/lgt2*
rm -f "$LOGTALKUSER"/xml/logtalk.dtd
rm -f "$LOGTALKUSER"/xml/logtalk.xsd
ln -sf "$LOGTALKHOME"/BIBLIOGRAPHY "$LOGTALKUSER"/BIBLIOGRAPHY
ln -sf "$LOGTALKHOME"/INSTALL "$LOGTALKUSER"/INSTALL
ln -sf "$LOGTALKHOME"/LICENSE "$LOGTALKUSER"/LICENSE
ln -sf "$LOGTALKHOME"/QUICK_START "$LOGTALKUSER"/QUICK_START
ln -sf "$LOGTALKHOME"/README "$LOGTALKUSER"/README
ln -sf "$LOGTALKHOME"/RELEASE_NOTES "$LOGTALKUSER"/RELEASE_NOTES
ln -sf "$LOGTALKHOME"/UPGRADING "$LOGTALKUSER"/UPGRADING
ln -sf "$LOGTALKHOME"/manuals "$LOGTALKUSER"/manuals
ln -sf "$LOGTALKHOME"/xml/lgt2html.sh "$LOGTALKUSER"/xml/lgt2html
ln -sf "$LOGTALKHOME"/xml/lgt2pdf.sh "$LOGTALKUSER"/xml/lgt2pdf
ln -sf "$LOGTALKHOME"/xml/lgt2xml.sh "$LOGTALKUSER"/xml/lgt2xml
ln -sf "$LOGTALKHOME"/xml/logtalk.dtd "$LOGTALKUSER"/xml/logtalk.dtd
ln -sf "$LOGTALKHOME"/xml/logtalk.xsd "$LOGTALKUSER"/xml/logtalk.xsd
echo "Finished copying Logtalk files and directories."
echo
echo "You may need to edit the \$LOGTALKUSER/libpaths/libpaths.pl file to match"
echo "your Prolog compiler and operating-system requirements or to add your own"
echo "library paths."
echo
echo "You may want to customize the default Logtalk compiler flags by editing"
echo "the configuration file for your Prolog compiler found in the directory"
echo "\$LOGTALKUSER/configs."
echo
