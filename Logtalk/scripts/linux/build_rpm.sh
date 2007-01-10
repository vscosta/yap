#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.2
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

dir="$PWD"
cd ..

LOGTALKHOME=/usr/local/logtalk ./lgt_uninstall.sh
./lgt_install.sh

cd /usr/local/logtalk
scripts/cleandist.sh

cd ..
tar -czf lgt2292.tgz lgt2292
mv lgt2292.tgz /usr/src/redhat/SOURCES

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd /usr/src/redhat/RPMS/noarch
echo $PWD
ls -l
