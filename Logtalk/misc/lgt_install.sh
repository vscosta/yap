#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.20.1
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

rm -rf $prefix/lgt2201
rm -f $prefix/logtalk

mkdir $prefix/lgt2201

cd ..
cp -R * $prefix/lgt2201

cd $prefix
chmod -R go-w,a+r lgt2201
chmod a+x lgt2201
chmod a+x lgt2201/misc/*.sh
chmod a+x lgt2201/xml/*.sh
ln -sf lgt2201 logtalk

cd bin
ln -sf ../lgt2201/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2201/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2201/xml/lgt2html.sh lgt2html

echo "Logtalk installation completed."
echo
echo "Users should define the environment variable LOGTALKHOME pointing"
echo "to $prefix/logtalk and then run the shell script cplgtdirs in"
echo "order to make a local copy of the Logtalk examples, library, and"
echo "xml directories in ~/logtalk."
echo
