#!/bin/sh

find . -name CVS -print | xargs rm -rf
find . -name .cvsignore -print | xargs rm -f
find . -name '.#*' -print | xargs rm -f
find . -name .DS_Store -print | xargs rm -f
find . -name '.gdb*' -print | xargs rm -f
