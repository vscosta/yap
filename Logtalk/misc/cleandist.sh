#!/bin/sh

find . -name CVS -print0 | xargs -0 rm -rf
find . -name .cvsignore -print0 | xargs -0 rm -f
find . -name '.#*' -print0 | xargs -0 rm -f
find . -name .DS_Store -print0 | xargs -0 rm -f
find . -name '.gdb*' -print0 | xargs -0 rm -f
