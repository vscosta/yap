#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.0
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Installing Logtalk..."

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

rm -rf $prefix/lgt2220
rm -f $prefix/logtalk

mkdir $prefix/lgt2220

cd ..
cp -R * $prefix/lgt2220

cd $prefix
chmod -R go-w,a+r lgt2220
chmod a+x lgt2220
chmod a+x lgt2220/misc/*.sh
chmod a+x lgt2220/xml/*.sh
ln -sf lgt2220 logtalk

mkdir -p bin
cd bin
ln -sf ../lgt2220/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2220/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2220/xml/lgt2html.sh lgt2html

echo "Logtalk installation completed."
echo
echo "Users should define the environment variable LOGTALKHOME pointing"
echo "to $prefix/logtalk and then run the shell script cplgtdirs in"
echo "order to make a local copy of the Logtalk examples, library, and"
echo "xml directories in ~/logtalk."
echo
echo "Links to the cplgtdirs, lgt2pdf, and lgt2html scripts have been"
echo "created on $prefix/bin; you may need to add this directory to"
echo "your execution path."
echo
