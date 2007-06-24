#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

if [ -z "$1" ]; then
	case $( uname -s ) in
		Darwin	) prefix=/opt/local;;
		*		) prefix=/usr/local;;
	esac
	mkdir -p $prefix
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

echo
echo "Installing Logtalk on $prefix/share ..."
echo

mkdir -p $prefix/share

rm -rf $prefix/share/lgt2302
rm -f $prefix/share/logtalk

mkdir $prefix/share/lgt2302

cd ..
cp -R * $prefix/share/lgt2302

cd $prefix/share/lgt2302
scripts/cleandist.sh
find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755
chmod a+x integration/*.sh
chmod a+x manuals/userman/*.sh
chmod a+x manuals/refman/*.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/debian/postinst
chmod a+x scripts/debian/prerm
chmod a+x scripts/debian/postrm
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macosx/postflight
chmod a+x xml/*.sh
chmod a-x xml/*.js

cd ..
ln -sf lgt2302 logtalk

mkdir -p $prefix/bin
cd $prefix/bin

ln -sf ../share/logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../share/logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../share/logtalk/xml/lgt2html.sh lgt2html
ln -sf ../share/logtalk/xml/lgt2xml.sh lgt2xml

echo "Links to the \"cplgtdirs\", \"lgt2pdf\", \"lgt2html\", and \"lgt2xml\" scripts"
echo "have been created on \"$prefix/bin\"; you may need to add this directory"
echo "to your execution path."
echo

ln -sf ../share/logtalk/integration/bplgt.sh bplgt
ln -sf ../share/logtalk/integration/ciaolgt.sh ciaolgt
ln -sf ../share/logtalk/integration/cxlgt.sh cxlgt
ln -sf ../share/logtalk/integration/eclipselgt.sh eclipselgt
ln -sf ../share/logtalk/integration/gplgt.sh gplgt
ln -sf ../share/logtalk/integration/plclgt.sh plclgt
#ln -sf ../share/logtalk/integration/qplgt.sh qplgt
ln -sf ../share/logtalk/integration/sicstuslgt.sh sicstuslgt
ln -sf ../share/logtalk/integration/swilgt.sh swilgt
ln -sf ../share/logtalk/integration/xsblgt.sh xsblgt
ln -sf ../share/logtalk/integration/yaplgt.sh yaplgt

echo "The following integration scripts are installed for running Logtalk"
echo "with selected back-end Prolog compilers:"
echo
echo "  B-Prolog:       bplgt       (first run must use sudo)"
echo "  CIAO:           ciaolgt     (first run must use sudo)"
echo "  CxProlog:       cxlgt"
echo "  ECLiPSe:        eclipselgt"
echo "  GNU Prolog:     gplgt"
echo "  K-Prolog:       plclgt"
echo "  SICStus Prolog: sicstuslgt"
echo "  SWI-Prolog:     swilgt"
echo "  XSB:            xsblgt      (first run must use sudo)"
echo "  YAP:            yaplgt"
echo
echo "The Prolog integration scripts can be found on \"$prefix/bin\"."
echo "Make sure that the Prolog compilers are properly installed and available"
echo "on your execution path."
echo
echo "Users should ensure that the environment variable LOGTALKHOME is set to"
echo "\"$prefix/share/logtalk\" and then run the \"cplgtdirs\" shell script once"
echo "before running the integration scripts."
echo
echo "If you get an unexpected failure when using one of the Prolog integration"
echo "scripts, consult the \"$prefix/share/logtalk/configs/NOTES.txt\" file"
echo "for compatibility notes."
echo
echo "Logtalk basic installation completed."
echo
