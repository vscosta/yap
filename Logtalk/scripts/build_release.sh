#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

dir=`PWD`

cvs -d :pserver:anonymous@cvs.logtalk.org:/usr/local/cvsroot checkout logtalk

cd logtalk
scripts/cleandist.sh
chmod a+x manuals/userman/*.sh
chmod a+x manuals/refman/*.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macosx/postflight
chmod a+x xml/*.sh
chmod a-x xml/*.js

cd ..
cp -R logtalk/manuals man2292
tar -czf man2292.tgz man2292
mv logtalk lgt2292
tar -czf lgt2292.tgz lgt2292

md5="`md5 -q lgt2292.tgz`"
sudo mkdir -p /opt/local/var/db/dports/distfiles/logtalk
sudo cp -f lgt2292.tgz /opt/local/var/db/dports/distfiles/logtalk/lgt2292.tgz
cd /opt/local/var/db/dports/sources/rsync.rsync.darwinports.org_dpupdate_dports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.29.2/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo sed -e 's/^distname.*/distname lgt2292/' -i '' Portfile
sudo port clean --archive logtalk
sudo port install logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.29.2.pkg $dir
sudo port uninstall logtalk

cd $dir
mkdir manpdf2292
cd man2292/userman
./userman.sh
mv userman.pdf ../../manpdf2292
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2292
cd ../..
tar -czf manpdf2292.tgz manpdf2292
