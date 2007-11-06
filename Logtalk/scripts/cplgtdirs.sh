#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.7
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

echo
echo "This script copies the Logtalk user-modifiable files and directories"
echo "to the user home directory. The location can be set by the environment"
echo "variable \$LOGTALKUSER (defaults to \"~/logtalk\" when the variable is not"
echo "defined)"
echo

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "Using Logtalk installation at \"/usr/local/share/logtalk\""
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "Using Logtalk installation at \"/usr/share/logtalk\""
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at \"/opt/local/share/logtalk\""
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "Using Logtalk installation at \"/opt/share/logtalk\""
	else
		echo "Unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKHOME pointing to \"$LOGTALKHOME\"."
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
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
	echo "LOGTALKUSER pointing to \"$LOGTALKUSER\"."
	echo
fi

if [ -d "$LOGTALKUSER" ]
then
	date=`eval date \"+%Y-%m-%d-%H%M%S\"`
	mv $LOGTALKUSER "$LOGTALKUSER backup $date"
	echo "Created a backup of the existing \"$LOGTALKUSER\" directory."
	echo
fi

echo "Copying Logtalk files and directories..."
mkdir -p "$LOGTALKUSER"/configs
mkdir -p "$LOGTALKUSER"/contributions
mkdir -p "$LOGTALKUSER"/examples
mkdir -p "$LOGTALKUSER"/libpaths
mkdir -p "$LOGTALKUSER"/library
mkdir -p "$LOGTALKUSER"/xml
cp -RL "$LOGTALKHOME"/configs "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/contributions "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/examples "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/libpaths "$LOGTALKUSER"/
sed 's_\$LOGTALKUSER_'$LOGTALKUSER'_' "$LOGTALKUSER"/libpaths/libpaths.pl > "$LOGTALKUSER"/libpaths/libpaths_no_env_var.pl
cp -RL "$LOGTALKHOME"/library "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/xml "$LOGTALKUSER"/
chmod -R u+w "$LOGTALKUSER"
rm -f "$LOGTALKUSER"/xml/lgt2*
rm -f "$LOGTALKUSER"/xml/logtalk.dtd
rm -f "$LOGTALKUSER"/xml/logtalk.xsd
ln -sf "$LOGTALKHOME"/BIBLIOGRAPHY.bib "$LOGTALKUSER"/BIBLIOGRAPHY.bib
ln -sf "$LOGTALKHOME"/CUSTOMIZE.txt "$LOGTALKUSER"/CUSTOMIZE.txt
ln -sf "$LOGTALKHOME"/INSTALL.txt "$LOGTALKUSER"/INSTALL.txt
ln -sf "$LOGTALKHOME"/LICENSE.txt "$LOGTALKUSER"/LICENSE.txt
ln -sf "$LOGTALKHOME"/QUICK_START.txt "$LOGTALKUSER"/QUICK_START.txt
ln -sf "$LOGTALKHOME"/README.txt "$LOGTALKUSER"/README.txt
ln -sf "$LOGTALKHOME"/RELEASE_NOTES.txt "$LOGTALKUSER"/RELEASE_NOTES.txt
ln -sf "$LOGTALKHOME"/UPGRADING.txt "$LOGTALKUSER"/UPGRADING.txt
ln -sf "$LOGTALKHOME"/manuals "$LOGTALKUSER"/manuals
ln -sf "$LOGTALKHOME"/wenv "$LOGTALKUSER"/wenv
ln -sf "$LOGTALKHOME"/xml/lgt2html.sh "$LOGTALKUSER"/xml/lgt2html
ln -sf "$LOGTALKHOME"/xml/lgt2pdf.sh "$LOGTALKUSER"/xml/lgt2pdf
ln -sf "$LOGTALKHOME"/xml/lgt2xml.sh "$LOGTALKUSER"/xml/lgt2xml
ln -sf "$LOGTALKHOME"/xml/logtalk.dtd "$LOGTALKUSER"/xml/logtalk.dtd
ln -sf "$LOGTALKHOME"/xml/logtalk.rng "$LOGTALKUSER"/xml/logtalk.rng
ln -sf "$LOGTALKHOME"/xml/logtalk.xsd "$LOGTALKUSER"/xml/logtalk.xsd
echo "Finished copying Logtalk files and directories."
echo
echo "You may need to edit the \"$LOGTALKUSER/libpaths/libpaths.pl\""
echo "file to match your Prolog compiler and operating-system requirements or"
echo "to add your own library paths."
echo
echo "You may want to customize the default Logtalk compiler flags by editing"
echo "the configuration file for your Prolog compiler found in the directory"
echo "\"$LOGTALKUSER/configs\"."
echo "For more information on customizing Logtalk and your working environment,"
echo "consult the \"$LOGTALKUSER/CUSTOMIZE.txt\" file."
echo
