#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.5
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
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

rm -rf $prefix/lgt2295
rm -f $prefix/logtalk

mkdir $prefix/lgt2295

cd ..
cp -R * $prefix/lgt2295

cd $prefix
chmod -R go-w,a+r lgt2295
chmod a+x lgt2295
chmod a+x lgt2295/scripts/*.sh
chmod a-x lgt2295/scripts/*.js
chmod a+x lgt2295/scripts/linux/*.sh
chmod a+x lgt2295/scripts/macosx/postflight
chmod a+x lgt2295/xml/*.sh
chmod a-x lgt2295/xml/*.js
ln -sf lgt2295 logtalk

mkdir -p bin
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml

echo "Logtalk basic installation completed. See the INSTALL and CUSTOMIZATION"
echo "files for details on customizing your working environment."
echo
echo "You may want to run some of the Prolog integration scripts, which you"
echo "will find on the same directory as this installer script."
echo
echo "Users must define the environment variable LOGTALKHOME pointing to"
echo "$prefix/logtalk and then run the shell script cplgtdirs"
echo "in order to copy the Logtalk user-modifiable files to their home"
echo "directories."
echo
echo "Links to the cplgtdirs, lgt2pdf, lgt2html, and lgt2xml scripts have"
echo "been created on $prefix/bin; you may need to add this directory to"
echo "your execution path."
echo
