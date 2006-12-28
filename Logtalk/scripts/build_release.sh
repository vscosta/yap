#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.1
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

dir=`PWD`

cvs -d :pserver:anonymous@cvs.logtalk.org:/usr/local/cvsroot checkout logtalk

cd logtalk
scripts/cleandist.sh
chmod a+x manuals/userman/*.sh
chmod a+x manuals/refman/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macosx/postflight
chmod a+x xml/*.sh
chmod a-x xml/*.js

cd ..
cp -R logtalk/manuals man2291
tar -czf man2291.tgz man2291
mv logtalk lgt2291
tar -czf lgt2291.tgz lgt2291

md5="`md5 -q lgt2291.tgz`"
sudo mkdir -p /opt/local/var/db/dports/distfiles/logtalk
sudo cp -f lgt2291.tgz /opt/local/var/db/dports/distfiles/logtalk/lgt2291.tgz
cd /opt/local/var/db/dports/sources/rsync.rsync.darwinports.org_dpupdate_dports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.29.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo sed -e 's/^distname.*/distname lgt2291/' -i '' Portfile
sudo port clean --archive logtalk
sudo port install logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.29.1.pkg $dir
sudo port uninstall logtalk

cd $dir
mkdir manpdf2291
cd man2291/userman
./userman.sh
mv userman.pdf ../../manpdf2291
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2291
cd ../..
tar -czf manpdf2291.tgz manpdf2291
