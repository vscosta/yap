#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.25.1
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

echo
echo "Installing Logtalk on $prefix ..."
echo

rm -rf $prefix/lgt2251
rm -f $prefix/logtalk

mkdir $prefix/lgt2251

cd ..
cp -R * $prefix/lgt2251

cd $prefix
chmod -R go-w,a+r lgt2251
chmod a+x lgt2251
chmod a+x lgt2251/scripts/*.sh
chmod a+x lgt2251/xml/*.sh
ln -sf lgt2251 logtalk

mkdir -p bin
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml

echo "Logtalk basic installation completed. See the INSTALL file for details"
echo "on customizing your working environment."
echo
echo "You may want to run some of the Prolog integration scripts, which you"
echo "will find on the same directory as this installer script."
echo
echo "Users should define the environment variable LOGTALKHOME pointing"
echo "to $prefix/logtalk and then run the shell script cplgtdirs"
echo "in order to copy the Logtalk user-modifiable files to their home"
echo "directories."
echo
echo "Links to the cplgtdirs, lgt2pdf, lgt2html, and lgt2xml scripts have"
echo "been created on $prefix/bin;  you may need to add this directory to"
echo "your execution path."
echo
