#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.30.1
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

find . -name .svn -print0 | xargs -0 rm -rf
find . -name CVS -print0 | xargs -0 rm -rf
find . -name .cvsignore -print0 | xargs -0 rm -f
find . -name '.#*' -print0 | xargs -0 rm -f
find . -name .DS_Store -print0 | xargs -0 rm -f
find . -name '.gdb*' -print0 | xargs -0 rm -f
