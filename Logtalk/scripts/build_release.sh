#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.1
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk logtalk

cd logtalk
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
cp -R logtalk/manuals man2301
tar -czf man2301.tgz man2301
mv logtalk lgt2301
tar -cjf lgt2301.tar.bz2 lgt2301

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt2301/scripts
./install.sh $dir/debian/usr
cp debian/logtalk.doc-base $dir/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $dir/debian/usr/share/menu/logtalk
cp ../*.bib $dir/debian/usr/share/doc/logtalk
cp ../*.txt $dir/debian/usr/share/doc/logtalk
cp debian/copyright $dir/debian/usr/share/doc/logtalk
cp debian/changelog $dir/debian/usr/share/doc/logtalk
cp debian/changelog.Debian $dir/debian/usr/share/doc/logtalk
gzip --best $dir/debian/usr/share/doc/logtalk/*.bib 
gzip --best $dir/debian/usr/share/doc/logtalk/*.txt 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog.Debian
cp debian/control $dir/debian/DEBIAN
cp debian/postinst $dir/debian/DEBIAN
cp debian/prerm $dir/debian/DEBIAN
cp debian/postrm $dir/debian/DEBIAN
cd $dir
dpkg-deb --build debian logtalk_2.30.1-1_all.deb

md5="`md5 -q lgt2301.tar.bz2`"
sudo mkdir -p /opt/local/var/db/dports/distfiles/logtalk
sudo cp -f lgt2301.tar.bz2 /opt/local/var/db/dports/distfiles/logtalk/lgt2301.tar.bz2
cd /opt/local/var/db/dports/sources/rsync.rsync.darwinports.org_dpupdate_dports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.30.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo sed -e 's/^distname.*/distname lgt2301/' -i '' Portfile
sudo sed -e 's/^extract\.suffix.*/extract.suffix .tar.bz2/' -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.30.1.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2301
cd man2301/userman
./userman.sh
mv userman.pdf ../../manpdf2301
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2301
cd ../..
tar -czf manpdf2301.tgz manpdf2301
