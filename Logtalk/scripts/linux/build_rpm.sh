#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.1
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

dir="$PWD"
cd ..

LOGTALKHOME=/usr/local/logtalk ./lgt_uninstall.sh
./lgt_install.sh

cd /usr/local/logtalk
scripts/cleandist.sh

cd ..
tar -czf lgt2291.tgz lgt2291
mv lgt2291.tgz /usr/src/redhat/SOURCES

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd /usr/src/redhat/RPMS/noarch
echo $PWD
ls -l
